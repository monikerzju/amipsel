/** *********************************************************
  * ********************dcache prototype*********************
  * 1-way direct-mapped instruction cache
  * features:
  *    - AXI protocol added
  *    - serialized blocking access
  *    - hit signal delay 1 cycle
  *    - data delay 1 cycle
  *
  * TODO:  [x] output serialized to cater for AXI bandwidth
  *        [x] traits not compatible with icore defination
  *        [x] BRAM interface for data access
  *        [ ] invalidate instructions
  *        [ ] flush
  *        [x] dual-issue for icache
  *        [x] mmio
  *        [x] map from virtual to physical
  *
  * NOTICE: - expect the valid signal early in the cycle, not withstandable with the latency
  *        - provides access for aligned address only
  * FIXME:  [x] skeptical : the valid signal might not trigger the state transfer;
  *                in which case both meta and data will suffer 1-cycle lantency
  *        [x] valid-ready protocol
  *
  * *********************************************************
  * 5/7 changelog
  * assert cpu input not steady, e.g. send valid even when stall
  * add `responsive` singal, asserted when resp.valid and put down when req.valid
  * change all cpu raw input in states apart from s_normal to mux(raw,regEnable(raw,responsive));
  * replace RegNext with RegEnable(_,responsive)
  * *********************************************************
  * bram diff:
  * 1. valid change to RegNext; now stay up to 2 cycle in s_normal instead of 1
  * 2. cancel data delay when bar.resp.valid is received
  * 3. meta output hit, dirty, compatible with current, no diff
  * 4. `responsive` definition
  * *********************************************************
  */
package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import conf._
import icore._
class MetaDataBRAM(nline: Int) extends Module with Config {
  assert(!dcacheMetaZeroLatency)
  // FIXME: io.write-> reg_write
  val io = IO(new MetaIODSimple)
  val blk = Module(new BRAMSyncReadMem(nline, dTagBits + 2))
  blk.io.we := io.update || (io.hit && io.write)
  blk.io.addr := io.index_in
  val v = blk.io.dout(dTagBits)
  val t = blk.io.dout(dTagBits - 1, 0)

  val dirty = Mux(io.update, io.write, true.B)
  val tag = Mux(io.update, io.tags_in, t)

  blk.io.din := Cat(Seq(dirty, true.B, tag))
  io.dirty := blk.io.dout(dTagBits + 1)
  io.hit := RegNext(io.tags_in) === t && v
  io.tag := t
}
class DCacheSimple(real_dcache: Boolean = true)
    extends Module
    with MemAccessType
    with Config {
  val io = IO(new Bundle {
    val cpu = new MemIO(1)
    val bar = new CacheIO(1 << (offsetBits + 3))
  })
  assert(!dcacheMetaZeroLatency)
  val responsive = Wire(Bool())
  val reg_tmp = RegInit(true.B)
  reg_tmp := io.cpu.resp.valid || !io.cpu.req.valid
  responsive := io.cpu.resp.valid || reg_tmp
  def __reg(raw: UInt) = {
    Mux(responsive, raw, RegEnable(raw, responsive))
  }
  val s_normal :: s_evict :: s_refill :: s_uncached :: s_cpu_resp :: Nil = Enum(
    5
  )
  val state = RegInit(s_normal)
  val nline = 1 << dIndexBits
  val data = Module(new DPBRAMSyncReadMem(nline, 1 << (offsetBits + 3)))
  val meta = Module(new MetaDataBRAM(nline));
  val unmaped = __reg(io.cpu.req.bits.addr(31, 29) === "b100".U).asBool
  // 0x80000000-0xa000000
  // translate virtual addr from start
  val tag_raw = Cat(
    Mux(unmaped, 0.U(3.W), __reg(io.cpu.req.bits.addr(31, 29))),
    __reg(io.cpu.req.bits.addr(28, 32 - dTagBits))
  )
  val index_raw =
    __reg(io.cpu.req.bits.addr(len - dTagBits - 1, len - dTagBits - dIndexBits))
  val reg_index = RegNext(index_raw)
  val mmio = if (real_dcache) {
    __reg(io.cpu.req.bits.addr(31, 29) === "b101".U).asBool
  } else {
    true.B
  }
  val line = Wire(Vec(1 << (offsetBits - 2), UInt(len.W)))
  val writeline = Wire(Vec(1 << (offsetBits - 2), UInt(len.W)))
  var i = 0
  for (i <- 0 until 1 << (offsetBits - 2)) {
    line(i) := Mux(
      state === s_normal || state === s_evict,
      data.io.douta(i * len + 31, i * len),
      io.bar.resp.data(i * len + 31, i * len)
    )
  }
  writeline := line
  val reg_tag_refill = RegInit(0.U(dTagBits.W))
  val reg_index_refill = RegInit(0.U(dIndexBits.W))
  val reg_word1 = RegEnable(io.cpu.req.bits.addr(offsetBits - 1, 2), responsive)

  // bram defaults
  {
    data.io.wea := false.B
    data.io.addra := index_raw
    data.io.dina := io.bar.resp.data
    // port b disabled, reserved for future
    // TODO: [ ] issue reads and writes to seperate ports to form pipeline
    data.io.web := false.B
    data.io.dinb := DontCare
    data.io.doutb := DontCare
    data.io.addrb := 0.U
  }

  // meta defaults
  {
    meta.io.tags_in := tag_raw
    meta.io.index_in := index_raw
    meta.io.update := false.B
    meta.io.write := false.B
  }
  // cpu io defaults
  {
    io.cpu.req.ready := io.cpu.resp.valid
    io.cpu.resp.valid := false.B
    io.cpu.resp.bits.respn := 0.U
    io.cpu.resp.bits.rdata(0) := line(reg_word1)
  }

  // bar io defaults
  {
    io.bar.req.mtype := Mux(mmio, __reg(io.cpu.req.bits.mtype), MEM_DWORD.U)
    io.bar.req.valid := false.B
    io.bar.req.wen := false.B
    io.bar.req.addr := Cat(Seq(tag_raw, index_raw, 0.U(offsetBits.W)))
    io.bar.req.data := 0.U
  }

  val mask_raw = Wire(UInt(32.W))
  val reg_mask = RegEnable(mask_raw, responsive)
  val reg_shift = RegEnable(io.cpu.req.bits.addr(1, 0) << 3, responsive)
  mask_raw := "hffffffff".U
  switch(io.cpu.req.bits.mtype) {
    is(MEM_HALF.U) {
      mask_raw := "h0000ffff".U
    }
    is(MEM_BYTE.U) {
      mask_raw := "h000000ff".U
    }
  }
  val reg_wdata = RegEnable(io.cpu.req.bits.wdata, responsive)
  val wd =
    ((reg_mask & reg_wdata) << reg_shift) | ((~(reg_mask << reg_shift)) & line(reg_word1))

  val reg_tag_evict = RegInit(0.U(dTagBits.W))
  val reg_rdata = RegInit(0.U(len.W))

  // val reg_write = RegNext(wen)
  val reg_write = RegInit(false.B)
  reg_write := Mux(
    responsive,
    io.cpu.req.bits.wen && io.cpu.req.valid,
    reg_write
  )

  switch(state) {
    is(s_normal) {
      when(RegNext(!mmio && io.cpu.req.valid)) {
        // dealing with the request from previous cycle

        when(meta.io.hit) {
          when(reg_write) {
            // privious write hit, bubble inserted , delay the valid signal
            reg_write := false.B
            io.cpu.resp.valid := false.B

            data.io.wea := true.B
            writeline(reg_word1) := wd
            data.io.dina := writeline.asUInt
            data.io.addra := reg_index

            meta.io.write := true.B
          }.otherwise{
            io.cpu.resp.valid := RegEnable(io.cpu.req.valid,responsive)
          }
        }.otherwise {
          io.cpu.resp.valid := false.B
          reg_tag_refill := tag_raw
          reg_index_refill := index_raw
          when(meta.io.dirty) {
            state := s_evict
            reg_tag_evict := meta.io.tag

            // for 4 way,
            // to preseve the same temporal feature, 
            // assert data not ready, wait until s_evict to assert valid

            // data.io.addra := Cat(meta.io.evict_way,reg_index)
          }.otherwise {
            io.bar.req.valid := true.B
            state := s_refill
          }
        }
      }.elsewhen(mmio && __reg(io.cpu.req.valid)(0)) {
          state := s_uncached
          io.bar.req.valid := true.B
          io.bar.req.addr := io.cpu.req.bits.addr
          io.bar.req.data := io.cpu.req.bits.wdata
          io.bar.req.wen := io.cpu.req.bits.wen
        }
    }
    is(s_refill) {
      io.bar.req.addr := Cat(
        Seq(reg_tag_refill, reg_index_refill, 0.U(offsetBits.W))
      )
      when(io.bar.resp.valid) {
        state := s_cpu_resp
        meta.io.update := true.B
        meta.io.index_in := reg_index_refill
        meta.io.tags_in := reg_tag_refill
        meta.io.write := reg_write
        data.io.addra := reg_index_refill
        data.io.wea := true.B
        when(!reg_write) {
          reg_rdata := line(reg_word1)
          // dina = resp.data
        }.otherwise {
          writeline(reg_word1) := wd
          data.io.dina := writeline.asUInt
          // reg_write := false.B
          // could be redundent, ought not interfere reg_write manually
        }
      }.otherwise {
        io.bar.req.valid := true.B
      }
    }
    is(s_cpu_resp) {
      // cut down the path from axi to core
      // may use registers as relay to cut path from axi to BRAM data;
      // introduce miss penalty
      state := s_normal
      io.cpu.resp.valid := true.B
      io.cpu.resp.bits.rdata(0) := reg_rdata
      // reg_write := false.B
    }
    is(s_evict) {
      // data.io.addra := Cat(meta.io.evict_way,index)
      // `line` is the output of douta, addra should be controled 
      io.bar.req.data := line.asUInt
      io.bar.req.valid := true.B
      io.bar.req.wen := true.B
      io.bar.req.addr := Cat(
        Seq(reg_tag_evict, reg_index_refill, 0.U(offsetBits.W))
      )
      when(io.bar.resp.valid) {
        state := s_refill
        io.bar.req.addr := Cat(
          Seq(reg_tag_refill, reg_index_refill, 0.U(offsetBits.W))
        )
        io.bar.req.wen := false.B
      }
      // .otherwise {
      //   io.bar.req.valid := true.B
      // }
    }
    is(s_uncached) {
      io.bar.req.addr := __reg(io.cpu.req.bits.addr)
      io.bar.req.data := __reg(io.cpu.req.bits.wdata)
      io.bar.req.wen := reg_write

      when(io.bar.resp.valid) {
        state := s_cpu_resp
        when(!reg_write) {
          reg_rdata := io.bar.resp.data(31, 0)
        }
      }.otherwise {
        io.bar.req.valid := true.B
      }
    }
  }
}

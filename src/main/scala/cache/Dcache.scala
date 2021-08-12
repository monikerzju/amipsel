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
import chisel3.util.experimental.BoringUtils

class MetaIOISimple extends Bundle with Config {
  val index_in = Input(UInt(dIndexBits.W))
  val tags_in = Input(UInt(dTagBits.W))
  val update = Input(Bool())
  val hit = Output(Bool())
  // val aux_index = Input(UInt((dIndexBits).W))
  // val aux_tag = Input(UInt((dTagBits).W))
}

class MetaIODSimple extends MetaIOISimple with Config {
  val write = Input(Bool())
  val tag = Output(UInt(dTagBits.W))
  val dirty = Output(Bool())
}

class MetaDataBRAM(nline: Int, verilator: Boolean = false) extends Module with Config {
  // to support simutanious write and read, implement dirty bit with regs

  // FIXME: io.write-> reg_write
  val io = IO(new MetaIODSimple)
  val blk = if (verilator) Module(new DPSyncReadMem(nline, dTagBits + 2)) else Module(new DPBRAMSyncReadMem(nline, dTagBits + 2))

  val dout = Mux(RegNext(blk.io.web && blk.io.addra === blk.io.addrb),RegNext(blk.io.dinb),blk.io.douta)
  val v = dout(dTagBits)
  val t = dout(dTagBits - 1, 0)
  val reg_update = RegNext(io.update)
  val reg_write = RegNext(io.write)
  val reg_index = RegNext(io.index_in)
  val reg_tag = RegNext(io.tags_in)
  val tag = Mux(reg_update,reg_tag,t)
  
  blk.io.wea := false.B
  blk.io.addra := io.index_in
  blk.io.dina := 0.U
  
  blk.io.web := reg_update || (io.hit && reg_write)
  blk.io.addrb := reg_index
  blk.io.dinb := Cat(reg_write, true.B, tag)

  io.dirty := dout(dTagBits+1)
  io.hit := reg_update || (reg_tag === t && v)
  // always return hit after writing meta
  io.tag := t
}
class DCacheSimple(diffTest: Boolean = true, verilator: Boolean = false)
    extends Module
    with MemAccessType
    with Config {
  val io = IO(new Bundle {
    val cpu = new MemIO(1)
    val bar = new CacheIO(1 << (offsetBits + 3))
  })
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
  val data = if (verilator) Module(new DPSyncReadMem(nline, 1 << (offsetBits + 3))) else Module(new DPBRAMSyncReadMem(nline, 1 << (offsetBits + 3)))
  val meta = Module(new MetaDataBRAM(nline, verilator));
  val unmaped = __reg(io.cpu.req.bits.addr(31, 29) === "b100".U).asBool
  // 0x80000000-0xa000000
  // translate virtual addr from start
  val tag_raw = Cat(
    Mux(unmaped, 0.U(3.W), __reg(io.cpu.req.bits.addr(31, 29))),
    __reg(io.cpu.req.bits.addr(28, 32 - dTagBits))
  )
  val reg_tag = RegNext(tag_raw)
  val index_raw =
    __reg(io.cpu.req.bits.addr(len - dTagBits - 1, len - dTagBits - dIndexBits))
  val reg_index = RegNext(index_raw)
  val mmio = __reg(io.cpu.req.bits.addr(31, 29) === "b101".U).asBool
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
  val reg_tag_refill = RegInit(0.U(dTagBits.W))
  val reg_index_refill = RegInit(0.U(dIndexBits.W))
  val reg_word1 = RegEnable(io.cpu.req.bits.addr(offsetBits - 1, 2), responsive)

  // bram defaults
  {
    data.io.wea := false.B
    data.io.addra := index_raw
    data.io.dina := writeline.asUInt
    // port b disabled, reserved for future
    // TODO: [ ] issue reads and writes to separate ports to form pipeline
    data.io.web := false.B
    data.io.dinb := writeline.asUInt
    data.io.doutb := DontCare
    data.io.addrb := 0.U
  }

  // meta defaults
  {
    meta.io.tags_in := tag_raw
    meta.io.index_in := index_raw
    meta.io.update := false.B
    meta.io.write := __reg(io.cpu.req.bits.wen)
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
  val reg_write = RegInit(false.B)
  reg_write := Mux(
    responsive,
    io.cpu.req.bits.wen && io.cpu.req.valid,
    reg_write
  )
  val reg_wdata = RegEnable(io.cpu.req.bits.wdata, responsive)
  val wd = Wire(UInt(len.W))
  wd := ((reg_mask & reg_wdata) << reg_shift) | ((~(reg_mask << reg_shift)) & line(reg_word1))
  if(withBigCore){
    val swl_mask = Wire(UInt(len.W))
    val swr_mask = Wire(UInt(len.W))
    swl_mask := ~("hffffff00".U << reg_shift)
    swr_mask := "hffffffff".U << reg_shift
    when(RegEnable(io.cpu.req.bits.swlr(0).asBool,responsive)){  // swl
      wd := ((reg_wdata >> (24.U-reg_shift)) & swl_mask) | (line(reg_word1) & ~swl_mask) 
    }.elsewhen(RegEnable(io.cpu.req.bits.swlr(1).asBool,responsive)){  // swr
      wd := ((reg_wdata << reg_shift) & swr_mask) | (line(reg_word1) & ~swr_mask)
    }
  }

  val reg_tag_evict = RegInit(0.U(dTagBits.W))
  val reg_rdata = RegNext(Mux(state === s_uncached, io.bar.resp.data(len-1,0), line(reg_word1)))

  val reg_line = RegInit(0.U((1<<(offsetBits+3)).W))
  val reg_miss = RegInit(false.B)
  val reg_forward = RegInit(false.B)
  reg_miss := false.B
  reg_forward := false.B
  // clears after 1 cycle, only used to convey signal between adjacent cycles 
  writeline := line
  when(reg_write){
    writeline(reg_word1) := wd
  }

  switch(state) {
    is(s_normal) {
      when(RegNext(!mmio && io.cpu.req.valid)) {
        // dealing with the request from previous cycle

        when((meta.io.hit || reg_forward) && !reg_miss) {
          when(reg_write) {

            // reg_write := false.B
            io.cpu.resp.valid := true.B
            
            // use b port for writes, a port for read 
            data.io.web := true.B
            // writeline(reg_word1) := wd
            // data.io.dinb := writeline.asUInt
            data.io.addrb := reg_index

            
            // another request coming in, as resp.valid is asserted;
            // port a set as default, only conflicts when the indexes are same            
            when(index_raw === reg_index){
              // reads xxx from meta and data;
              reg_line := writeline.asUInt
              when(reg_tag === tag_raw){
                // same addr, use forwarding
                reg_forward := true.B
              }.otherwise{
                // miss for next cycle
                reg_miss := true.B
              }
              
            }
          }.otherwise{
            io.cpu.resp.valid := RegEnable(io.cpu.req.valid,responsive)
          }
          when(reg_forward){
            for (i <- 0 until 1 << (offsetBits - 2)) {
              line(i) := reg_line(i *len + 31,i *len)
            }
          }
        }.otherwise {
          when(reg_miss){
            for (i <- 0 until 1 << (offsetBits - 2)) {
              line(i) := reg_line(i *len + 31,i *len)
            }
          }
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

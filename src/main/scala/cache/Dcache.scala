/** *********************************************************
  * ********************dcache prototype*********************
  * 1-way direct-mapped instruction cache
  * features:
  *    - AXI protocol added
  *    - serialized blocking access
  *    - meta ready at the cycle imediately after the request
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
  * NOTICE: - expect the valid signal early in the cycle, not withstandable the latency
  *        - provides access for aligned address only
  * FIXME:  [x] skeptical : the valid signal might not trigger the state transfer;
  *                in which case both meta and data will suffer 1-cycle lantency
  *        [x] valid-ready protocol
  *
  * *********************************************************
  */
package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import conf._
import icore._
class DCacheSimple(real_dcache: Boolean = true)
    extends Module
    with MemAccessType
    with Config {
  val io = IO(new Bundle {
    val cpu = new MemIO(1)
    val bar = new CacheIO(1 << (offsetBits + 3))
  })
  val s_normal :: s_evict :: s_refill :: s_uncached :: Nil = Enum(4)
  val state = RegInit(s_normal)
  val nline = 1 << indexBits
  val data = Module(new DPBRAMSyncReadMem(nline, 1 << (offsetBits + 3)))
  val meta = Module(new MetaDataSimple(nline));
  val unmaped = io.cpu.req.bits.addr(31, 29) === "b100".U
  // 0x80000000-0xa000000
  // translate virtual addr from start
  val tag_raw = Cat(
    Mux(unmaped, 0.U(3.W), io.cpu.req.bits.addr(31, 29)),
    io.cpu.req.bits.addr(28, 32 - tagBits)
  )
  val index_raw =
    io.cpu.req.bits.addr(len - tagBits - 1, len - tagBits - indexBits)
  val index = RegNext(index_raw)
  val mmio = if (real_dcache) {
    io.cpu.req.bits.addr(31, 29) === "b101".U
  } else {
    true.B
  }
  val line = Wire(Vec(1 << (offsetBits - 2), UInt(len.W)))
  val writeline = Wire(Vec(1 << (offsetBits - 2), UInt(len.W)))
  var i = 0
  for (i <- 0 until 1 << (offsetBits - 2)) {
    line(i) := Mux(
      state === s_normal,
      data.io.douta(i * len + 31, i * len),
      io.bar.resp.data(i * len + 31, i * len)
    )
  }
  writeline := line
  val tag_refill = RegInit(0.U(tagBits.W))
  val word1 = RegNext(io.cpu.req.bits.addr(offsetBits - 1, 2))
  val index_refill = RegInit(0.U(indexBits.W))

  data.io.wea := false.B
  data.io.addra := index_raw
  data.io.dina := io.bar.resp.data
  // port b disabled, reserved for future
  // TODO: [ ] issue reads and writes to seperate ports to form pipeline
  data.io.web := false.B
  data.io.dinb := DontCare
  data.io.doutb := DontCare
  data.io.addrb := 0.U

  meta.io.tags_in := tag_raw
  meta.io.index_in := index_raw
  meta.io.update := false.B
  meta.io.write := false.B

  io.cpu.req.ready := io.cpu.resp.valid
  io.cpu.resp.valid := (state =/= s_evict && io.bar.resp.valid) || (state === s_normal && meta.io.hit)
  io.cpu.resp.bits.respn := 0.U
  io.cpu.resp.bits.rdata(0) := line(word1)

  io.bar.req.mtype := Mux(mmio, io.cpu.req.bits.mtype, MEM_DWORD.U)
  io.bar.req.valid := false.B
  io.bar.req.wen := false.B
  io.bar.req.addr := Cat(Seq(tag_raw, index_raw, 0.U(offsetBits.W)))
  io.bar.req.data := 0.U

  val mask_raw = Wire(UInt(32.W))
  val mask_reg = RegEnable(mask_raw, io.cpu.req.valid)
  val shift = RegEnable(io.cpu.req.bits.addr(1, 0) << 3, io.cpu.req.valid)
  mask_raw := "hffffffff".U
  switch(io.cpu.req.bits.mtype) {
    is(MEM_HALF.U) {
      mask_raw := "h0000ffff".U
    }
    is(MEM_BYTE.U) {
      mask_raw := "h000000ff".U
    }
  }
  val wen = io.cpu.req.bits.wen && io.cpu.req.valid
  val wdata = RegEnable(io.cpu.req.bits.wdata, wen)
  val wd =
    ((mask_reg & wdata) << shift) | ((~(mask_reg << shift)) & line(word1))

  val tag_evict_reg = RegInit(0.U(tagBits.W))
  val reg_rdata = RegInit(0.U(len.W))
  val reg_wait = RegInit(false.B)
  val reg_wen = RegNext(wen)
  reg_wait := false.B
  when(reg_wen) {
    writeline(word1) := wd
    data.io.dina := writeline.asUInt
  }

  switch(state) {
    is(s_normal) {
      when(reg_wait) {
        io.cpu.resp.bits.rdata(0) := reg_rdata
      }
      when(reg_wen) {
        data.io.wea := true.B
        data.io.addra := index
        meta.io.write := true.B
      }
      when(!mmio && io.cpu.req.valid) {
        when(meta.io.hit) {
          when(reg_wen) {
            // privious write hit, bubble inserted , delay the valid signal
            reg_wen := false.B
            io.cpu.resp.valid := false.B
          }
        }
          .otherwise {
            tag_refill := tag_raw
            index_refill := index_raw
            when(meta.io.dirty) {
              state := s_evict
              tag_evict_reg := meta.io.tag
              // data not ready, wait until s_evict to assert valid
            }
              .otherwise {
                io.bar.req.valid := true.B
                state := s_refill
              }
          }
      }
        .elsewhen(mmio && io.cpu.req.valid) {
          state := s_uncached
          io.bar.req.valid := true.B
          io.bar.req.addr := io.cpu.req.bits.addr
          io.bar.req.data := io.cpu.req.bits.wdata
          io.bar.req.wen := io.cpu.req.bits.wen
        }
    }
    is(s_refill) {
      io.bar.req.addr := Cat(Seq(tag_refill, index_refill, 0.U(offsetBits.W)))
      when(io.bar.resp.valid) {
        state := s_normal
        io.cpu.resp.valid := true.B
        meta.io.update := true.B
        meta.io.index_in := index_refill
        meta.io.tags_in := tag_refill
        meta.io.write := reg_wen
        data.io.addra := index_refill
        data.io.wea := true.B
        when(!reg_wen) {
          reg_wait := true.B
          reg_rdata := line(word1)
        }.otherwise {
          // already written, but cpu is apt to keep wen high along with valid
          // writing twice won't hurt though
          reg_wen := false.B
        }
      }.otherwise {
        io.bar.req.valid := true.B
      }
    }
    is(s_evict) {
      io.bar.req.wen := true.B
      io.bar.req.data := line.asUInt
      io.bar.req.addr := Cat(
        Seq(tag_evict_reg, index_refill, 0.U(offsetBits.W))
      )
      when(io.bar.resp.valid) {
        state := s_refill
        io.bar.req.addr := Cat(Seq(tag_refill, index_refill, 0.U(offsetBits.W)))
        io.bar.req.wen := false.B
      }.otherwise {
        io.bar.req.valid := true.B
      }
    }
    is(s_uncached) {
      io.bar.req.addr := io.cpu.req.bits.addr
      io.bar.req.data := io.cpu.req.bits.wdata
      io.bar.req.wen := io.cpu.req.bits.wen

      when(io.bar.resp.valid) {
        state := s_normal
        when(!reg_wen) {
          reg_wait := true.B
          reg_rdata := io.bar.resp.data(31, 0)
        }
      }.otherwise {
        io.bar.req.valid := true.B
      }
    }
  }
}

package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import conf._
import icore._

class ICacheMeta(DEPTH: Int, DATA_WIDTH: Int) extends Module with Config {
  val io = IO(new Bundle {
    val we    = Input(Bool())
    val addr  = Input(UInt(log2Ceil(DEPTH).W))
    val din   = Input(UInt(DATA_WIDTH.W))
    val dout  = Output(UInt(DATA_WIDTH.W))
    val dout_next = Output(UInt(DATA_WIDTH.W))
  })

  if (icachePref) {
    val blko = Module(new BRAMSyncReadMem(DEPTH / 2, DATA_WIDTH))
    val blke = Module(new BRAMSyncReadMem(DEPTH / 2, DATA_WIDTH))
    val is_odd = io.addr(0).orR
    val is_odd_last = RegNext(is_odd)
    val addr_truncated = io.addr(log2Ceil(DEPTH) - 1, 1)

    blko.io.we   := io.we && is_odd
    blko.io.addr := addr_truncated
    blko.io.din  := io.din
    blke.io.we   := io.we && !is_odd
    blke.io.addr := addr_truncated + Mux(is_odd, 1.U, 0.U)
    blke.io.din  := io.din

    io.dout := Mux(is_odd_last, blko.io.dout, blke.io.dout)
    io.dout_next := Mux(is_odd_last, blke.io.dout, blko.io.dout)
  } else {
    val blk = Module(new BRAMSyncReadMem(DEPTH, DATA_WIDTH))

    blk.io.we    := io.we
    blk.io.addr  := io.addr
    blk.io.din   := io.din
    io.dout      := blk.io.dout
    io.dout_next := 0.U
  }

}

class ICache extends Module with Config with MemAccessType {
  val io = IO(new Bundle {
    val cpu = new MemIO()
    val bar = new CacheIO(1 << (offsetBits + 3))
  })
  val nline = 1 << indexBits

  val data = Module(new BRAMSyncReadMem(nline, 1 << (offsetBits + 3)))
  val meta = Module(new ICacheMeta(nline, tagBits + 1))
  val tag_req   = io.cpu.req.bits.addr(len - 1, len - tagBits)
  val index_req = io.cpu.req.bits.addr(len - tagBits - 1, len - tagBits - indexBits)

  // Meta Tile
  val req_addr = RegNext(io.cpu.req.bits.addr)
  val word1 = req_addr(offsetBits - 1, 2)
  val word2 = word1 + 1.U
  val hit   = meta.io.dout(tagBits).asBool && meta.io.dout(tagBits - 1, 0) === req_addr(len - 1, len - tagBits)
  meta.io.we   := false.B
  meta.io.addr := index_req
  meta.io.din  := Cat(true.B, tag_req)

  // I$ prefetch
  val prefetch_tag_id = if (icachePref) Reg(UInt((len - offsetBits).W)) else null
  val (prefill_v, prefill_tag_id) = if (icachePref) (RegInit(false.B), Reg(UInt((len - offsetBits).W))) else (null, null)
  val pfbhit = if (icachePref) prefill_v && prefill_tag_id === req_addr(len - 1, len - tagBits - indexBits) else false.B
  val (next_hit, prefetching, prefill_data) = if (icachePref) ((meta.io.dout_next(tagBits).asBool && 
                                              meta.io.dout(tagBits - 1, 0) === req_addr(len - 1, len - tagBits) + 1.U ||
                                              prefill_v && prefill_tag_id === req_addr(len - 1, len - tagBits - indexBits) + 1.U, 
                                              RegInit(false.B), Reg(Vec(1 << (offsetBits - 2), UInt(len.W))))) 
                                              else (true.B, null, null)

  // Data Tile
  val refill_data = Reg(Vec(1 << (offsetBits - 2), UInt(len.W)))
  val line        = Wire(Vec(1 << (offsetBits - 2), UInt(len.W)))
  data.io.addr := index_req
  if (icachePref) {
    data.io.din := Mux(!hit && pfbhit, prefill_data.asUInt, io.bar.resp.data)
  } else {
    data.io.din := io.bar.resp.data
  }
  data.io.we   := false.B
  for (i <- 0 until 1 << (offsetBits - 2)) {
    line(i) := data.io.dout(i * len + 31, i * len)
  }

  // Cache states
  val s_normal :: s_fetch :: s_refill :: Nil = Enum(3)
  val state = RegInit(s_normal)
  val lstate = RegNext(state)
  val nstate = WireDefault(state)
  state := nstate

  io.cpu.req.ready  := io.cpu.resp.valid
  io.cpu.resp.valid := state === s_refill || hit
  io.cpu.resp.bits.respn := (io.cpu.req.bits.mtype === 3.U && io.cpu.req.bits
    .addr(offsetBits - 1, 2) + 1.U =/= 0.U)
  if (icachePref) {
    io.cpu.resp.bits.rdata(0) := Mux(state === s_refill, 
        Mux(lstate === s_normal, prefill_data(word1), refill_data(word1)), line(word1))
    io.cpu.resp.bits.rdata(1) := Mux(state === s_refill, 
        Mux(lstate === s_normal, prefill_data(word2), refill_data(word2)), line(word2))
  } else {
    io.cpu.resp.bits.rdata(0) := Mux(state === s_refill, refill_data(word1), line(word1))
    io.cpu.resp.bits.rdata(1) := Mux(state === s_refill, refill_data(word2), line(word2))
  }

  
  io.bar.req.valid := false.B
  io.bar.req.wen   := false.B
  if (icachePref) {
    io.bar.req.addr := Cat(
      Mux(prefetching, prefetch_tag_id, req_addr(len - 1, offsetBits)),
      Fill(offsetBits, 0.U)
    )
  } else {
    io.bar.req.addr := Cat(
      req_addr(len - 1, offsetBits),
      Fill(offsetBits, 0.U)
    )
  }
  io.bar.req.data  := 0.U
  io.bar.req.mtype := MEM_DWORD.U
  when (state === s_normal) {
    if (icachePref) {
      when (!hit) {
        when (!pfbhit) {  // prefetch buffer
          nstate := s_fetch
          io.bar.req.valid := true.B
        }.otherwise {
          nstate := s_refill
          meta.io.we := true.B
          data.io.we := true.B
        }
      }.elsewhen(!next_hit) {
        nstate := s_fetch // req valid next state
        prefetching := true.B
        prefetch_tag_id := io.cpu.req.bits.addr(len - 1, offsetBits) + 1.U
      }
    } else {
      when (!hit) {
        nstate := s_fetch
        io.bar.req.valid := true.B
      }
    }    
  }.elsewhen (state === s_fetch) {
    io.bar.req.valid := !io.bar.resp.valid
    when (io.bar.resp.valid) {
      if (icachePref) {
        nstate := Mux(prefetching, s_normal, s_refill)
        when (prefetching) {
          prefill_v := true.B
          prefill_tag_id := prefetch_tag_id
          for (i <- 0 until 1 << (offsetBits - 2)) {
            prefill_data(i) := io.bar.resp.data(i * len + 31, i * len)
          }
          prefetching := false.B
        }.otherwise {
          meta.io.we := true.B
          data.io.we := true.B
          for (i <- 0 until 1 << (offsetBits - 2)) {
            refill_data(i) := io.bar.resp.data(i * len + 31, i * len)
          }
        }
      } else {
        nstate := s_refill
        meta.io.we := true.B
        data.io.we := true.B
        for (i <- 0 until 1 << (offsetBits - 2)) {
          refill_data(i) := io.bar.resp.data(i * len + 31, i * len)
        }
      }
    }
  }.otherwise { // s_refill
    nstate := s_normal
  }

}

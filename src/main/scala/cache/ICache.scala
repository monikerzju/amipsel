package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import conf._
import icore._

class ICacheMeta(DEPTH: Int, DATA_WIDTH: Int, verilator: Boolean = false) extends Module with Config {
  val io = IO(new Bundle {
    val we    = Input(Bool())
    val addr  = Input(UInt(log2Ceil(DEPTH).W))
    val din   = Input(UInt(DATA_WIDTH.W))
    val dout  = Output(UInt(DATA_WIDTH.W))
    val dout_next = Output(UInt(DATA_WIDTH.W))
  })

  val blk = if (verilator) Module(new SPSyncReadMem(DEPTH, DATA_WIDTH)) else Module(new BRAMSyncReadMem(DEPTH, DATA_WIDTH))
  blk.io.we    := io.we
  blk.io.addr  := io.addr
  blk.io.din   := io.din
  io.dout      := blk.io.dout
  io.dout_next := 0.U

}

class ICache(verilator: Boolean = false) extends Module with Config with MemAccessType {
  val io = IO(new Bundle {
    val cpu = new MemIO()
    val bar = new CacheIO(1 << (offsetBits + 3))
  })
  val nline = 1 << iIndexBits

  val data = if (verilator) Module(new SPSyncReadMem(nline, 1 << (offsetBits + 3))) else Module(new BRAMSyncReadMem(nline, 1 << (offsetBits + 3)))
  val meta = Module(new ICacheMeta(nline, iTagBits + 1, verilator))
  val tag_req   = io.cpu.req.bits.addr(len - 1, len - iTagBits)
  val index_req = io.cpu.req.bits.addr(len - iTagBits - 1, len - iTagBits - iIndexBits)

  // Meta Tile
  val req_addr = RegNext(io.cpu.req.bits.addr)
  val word1 = req_addr(offsetBits - 1, 2)
  val word2 = word1 + 1.U
  val hit   = if (withBigCore && bigCoreIMMIO) false.B else meta.io.dout(iTagBits).asBool && meta.io.dout(iTagBits - 1, 0) === req_addr(len - 1, len - iTagBits)
  meta.io.we   := false.B
  meta.io.addr := index_req
  meta.io.din  := Cat(true.B, tag_req)

  // Data Tile
  val refill_data = Reg(Vec(1 << (offsetBits - 2), UInt(len.W)))
  val line        = Wire(Vec(1 << (offsetBits - 2), UInt(len.W)))
  data.io.addr := index_req
  data.io.din := io.bar.resp.data
  data.io.we   := false.B
  for (i <- 0 until 1 << (offsetBits - 2)) {
    line(i) := data.io.dout(i * len + 31, i * len)
  }

  // Cache states
  val s_normal :: s_fetch :: s_refill :: Nil = Enum(3)
  val state = RegInit(s_normal)
  val nstate = WireDefault(state)
  state := nstate

  io.cpu.req.ready  := io.cpu.resp.valid
  io.cpu.resp.valid := state === s_refill || hit
  io.cpu.resp.bits.respn := (io.cpu.req.bits.mtype === 3.U && io.cpu.req.bits
    .addr(offsetBits - 1, 2) + 1.U =/= 0.U)
  io.cpu.resp.bits.rdata(0) := Mux(state === s_refill, refill_data(word1), line(word1))
  io.cpu.resp.bits.rdata(1) := Mux(state === s_refill, refill_data(word2), line(word2))

  io.bar.req.valid := false.B
  io.bar.req.wen   := false.B
  io.bar.req.addr := Cat(
    req_addr(len - 1, offsetBits),
    Fill(offsetBits, 0.U)
  )
  io.bar.req.data  := 0.U
  io.bar.req.mtype := MEM_DWORD.U
  when (state === s_normal) {
    when (!hit) {
      nstate := s_fetch
      io.bar.req.valid := true.B
    }  
  }.elsewhen (state === s_fetch) {
    io.bar.req.valid := !io.bar.resp.valid
    when (io.bar.resp.valid) {
      nstate := s_refill
      meta.io.we := true.B
      data.io.we := true.B
      for (i <- 0 until 1 << (offsetBits - 2)) {
        refill_data(i) := io.bar.resp.data(i * len + 31, i * len)
      }
    }
  }.otherwise { // s_refill
    nstate := s_normal
  }

}

/** *********************************************************
  * ********************icache prototype***********************
  * 1-way direct-mapped instruction cache
  * features:
  *    - AXI protocol added
  *    - non-blocking
  *    - meta ready at the cycle imediately after the request
  *    - data delay 1 cycle
  *    - for word access only
  *
  * TODO:   [x] output serialized to cater for AXI bandwidth
  *        [ ] traits not compatible with icore defination
  *        [x] BRAM interface for data access
  *        [ ] invalidate instructions
  *        [ ] flush
  *        [x] dual-issue for icache
  *
  * NOTICE: - expect the valid signal early in the cycle, not withstandable the latency
  *        - provides access for aligned address only
  * FIXME:  [ ] skeptical : the valid signal might not trigger the state transfer;
  *                in which case both meta and data will suffer 1-cycle lantency
  *        [ ] valid-ready protocol
  *        [ ] 双发射字节对齐？若一个miss 一个hit？
  *        [ ] non-blocking 导致 out-of-order?
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

// TODO 2 meta tiles for different lines for prefetch
class ICacheSimple
    extends Module
    with Config
    with MemAccessType {
  val io = IO(new Bundle {
    val cpu = new MemIO()
    val bar = new CacheIO(1 << (offsetBits + 3))
  })
  val nline = 1 << indexBits

  val data = Module(new BRAMSyncReadMem(nline, 1 << (offsetBits + 3)))
  val meta = Module(new BRAMSyncReadMem(nline, tagBits + 1))
  val tag_req   = io.cpu.req.bits.addr(len - 1, len - tagBits)
  val index_req = io.cpu.req.bits.addr(len - tagBits - 1, len - tagBits - indexBits)

  // Data Tile
  val refill_data = Reg(Vec(1 << (offsetBits - 2), UInt(len.W)))
  val line        = Wire(Vec(1 << (offsetBits - 2), UInt(len.W)))
  data.io.addr := index_req
  data.io.din  := io.bar.resp.data
  data.io.we   := false.B
  for (i <- 0 until 1 << (offsetBits - 2)) {
    line(i) := data.io.dout(i * len + 31, i * len)
  }

  // Meta Tile
  val req_addr = RegNext(io.cpu.req.bits.addr)
  val word1 = req_addr(offsetBits - 1, 2)
  val word2 = word1 + 1.U
  val hit   = meta.io.dout(tagBits).asBool && meta.io.dout(tagBits - 1, 0) === req_addr(len - 1, len - tagBits)
  meta.io.we   := false.B
  meta.io.addr := index_req
  meta.io.din  := Cat(true.B, tag_req)

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
  io.bar.req.addr  := Cat(
    req_addr(len - 1, offsetBits),
    Fill(offsetBits, 0.U)
  )
  io.bar.req.data  := 0.U
  io.bar.req.mtype := MEM_DWORD.U
  when (state === s_normal && !hit) {
    nstate := s_fetch
    io.bar.req.valid := true.B
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
  }.otherwise {
    nstate := s_normal
  }

}

package icore

import chisel3._
import chisel3.util._
import conf.Config
import isa._

// PCGen is can be non-project-specific, no with Config
class PCGenIO(va_width: Int = 32) extends Bundle {
  val please_wait = Input(Bool()) // maybe from: A. a full-fifo and not killed; B. icache fetching from memory 
  val redirect = Input(Bool())
  val redirect_pc = Input(UInt(va_width.W))
  val pc_o = Output(UInt(va_width.W))
  override def cloneType = (new PCGenIO(va_width)).asInstanceOf[this.type]
}

class PCGen(va_width: Int = 32, start_va: String = "h80000000", increment: Int = 4) extends Module {
  val io = IO(new PCGenIO(va_width))
  val pc = RegInit(UInt(va_width.W), start_va.U)
  pc := Mux(io.redirect, io.redirect_pc, Mux(io.please_wait, pc, pc + increment.U))
  io.pc_o := pc
}

class Frontend extends Module with Config with MemAccessType with FrontToBack {
  val io = IO(new FrontendIO)

  val pc_gen = Module(new PCGen(len, startAddr, 4 * frontendIssueN))
  val decs = Array.fill(frontendIssueN)(Module(new Dec).io)
  val decode_pc_low = RegInit(UInt(len.W), startAddr.U)
  val decode_instn = RegInit(UInt(frontendIssueN.W), 0.U)
  val decode_reg_line = Array.fill(frontendIssueN)(RegInit(0.U(len.W)))
  val last_wait = RegNext(pc_gen.io.please_wait)

  // Signals define
  val icache_stall_req_a = !io.icache.resp.valid && io.icache.req.valid
  val icache_fetch_half_a = io.icache.resp.valid && !io.icache.resp.bits.respn
  val wtg = last_wait
  val gtw = !last_wait && pc_gen.io.please_wait

  // Replay PC
  val replay = RegNext(icache_stall_req_a)
  val repc = RegInit(startAddr.U(len.W))
  repc := Mux(icache_stall_req_a && replay, repc, pc_gen.io.pc_o)

  // [---------- IF Stage ----------]
  // redirect_pc prio: bmfs.redirect > icache only fetch one
  // Only please wait from icache is atomic and will cause bmfs.please_wait = 1
  val wait_icache = icache_stall_req_a || (io.icache.req.bits.addr =/= pc_gen.io.pc_o)
  pc_gen.io.please_wait := wait_icache || io.fb.fmbs.please_wait
  pc_gen.io.redirect := io.fb.bmfs.redirect_kill || icache_fetch_half_a  // TODO when BPU is added
  pc_gen.io.redirect_pc := Mux(io.fb.bmfs.redirect_kill, io.fb.bmfs.redirect_pc, pc_gen.io.pc_o + 4.U)  // TODO

  io.icache.req.valid := true.B // maybe situation will change by adding BPU
  io.icache.resp.ready := true.B 
  io.icache.req.bits.addr := Mux(replay, repc, pc_gen.io.pc_o)
  io.icache.req.bits.wdata := DontCare
  io.icache.req.bits.wen := false.B
  if (frontendIssueN == 1) {
    io.icache.req.bits.mtype :=  MEM_WORD.U
  } else {
    io.icache.req.bits.mtype :=  MEM_DWORD.U
  }
  io.icache.req.bits.flush := false.B
  io.icache.req.bits.invalidate := false.B

  // [---------- ID Stage ----------]
  // for 1 cycle latency
  for (i <- 0 until frontendIssueN) {
    decode_reg_line(i) := Mux(gtw, io.icache.resp.bits.rdata(i), decode_reg_line(i))
  }
  // for 0 cycle latency
  decode_pc_low := Mux(io.fb.fmbs.please_wait, decode_pc_low, io.icache.req.bits.addr)
  decode_instn := Mux(io.fb.bmfs.redirect_kill, 0.U, Mux(io.fb.fmbs.please_wait, decode_instn, Mux(!wait_icache, Cat(0.U, io.icache.resp.bits.respn) + 1.U, 0.U)))
  // some IO to the fifo backend
  io.fb.fmbs.instn := Mux(io.fb.fmbs.please_wait, 0.U, decode_instn)
  for (i <- 0 until frontendIssueN) {
    decs(i).inst := Mux(wtg, decode_reg_line(i), io.icache.resp.bits.rdata(i))
    decs(i).pc := decode_pc_low + (i.U << 2.U)
    io.fb.fmbs.inst_ops(i) := decs(i).mops.asUInt
  }

}
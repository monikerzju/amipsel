package core

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
}

class PCGen(va_width: Int = 32, start_va: String = "h80000000", increment: Int = 4) extends Module {
  val io = IO(new PCGenIO(va_width))
  val pc = RegInit(UInt(va_width.W), start_va.U)
  when(!io.please_wait) {
    pc := Mux(io.redirect, io.redirect_pc, pc + increment.U)
  }
  io.pc_o := pc
}

class Frontend extends Module with Config with MemAccessType with FrontToBack {
  val io = IO(new FrontendIO)

  val pc_gen = Module(new PCGen(len, startAddr, 4 * frontendIssueN))
  val decs = Array.fill(frontendIssueN)(Module(new Dec).io)
  val decode_pc_low = RegInit(UInt(len.W), startAddr.U)
  val decode_instn = RegInit(UInt(SZ_FB_INSTN.W), 0.U)
  val decode_reg_line = RegInit(Vec(frontendIssueN, 0.U(len.W)))
  val last_wait = RegNext(io.fb.bmfs.please_wait)

  // Signals define
  val icache_stall_req_a = !io.icache.resp.valid && io.icache.req.valid
  val wtg = last_wait && !io.fb.bmfs.please_wait
  val gtw = !last_wait && io.fb.bmfs.please_wait

  // Some of the output signals for backend and icache
  io.fb.bmfs.please_wait := icache_stall_req_a

  // [---------- IF Stage ----------]
  // redirect_pc prio: bmfs.redirect > icache only fetch one
  // Only please wait from icache is atomic and will cause bmfs.please_wait = 1
  pc_gen.io.please_wait := icache_stall_req_a || io.fb.fmbs.please_wait
  pc_gen.io.redirect := io.fb.bmfs.redirect_kill  // TODO when BPU is added
  pc_gen.io.redirect_pc := io.fb.bmfs.redirect_pc  // TODO

  io.icache.req.valid := true.B // maybe situation will change by adding BPU
  io.icache.req.ready := true.B 
  io.icache.req.bits.addr := pc_gen.io.pc_o
  io.icache.req.bits.wdata := DontCare
  io.icache.req.bits.wen := false.B
  if (frontendIssueN == 1) {
    io.icache.req.bits.mtype :=  MEM_WORD
  } else {
    io.icache.req.bits.mtype :=  MEM_DWORD
  }
  io.icache.req.bits.flush := false.B
  io.icache.req.bits.invalidate := false.B

  // [---------- ID Stage ----------]
  // for 1 cycle latency
  for (i <- 0 until frontendIssueN) {
    decode_reg_line(i) := Mux(gtw, io.icache.resp.bits.rdata(i), decode_reg_line(i))
  }
  // for 0 cycle latency
  decode_pc_low := Mux(io.fb.fmbs.please_wait, decode_pc_low, pc_gen.io.pc_o)
  decode_instn := Mux(io.fb.fmbs.please_wait, decode_instn, Mux(io.icache.resp.valid, io.icache.resp.bits.respn + 1.U, 0.U))
  // some IO to the fifo backend
  io.fb.fmbs.instn := decode_instn
  for (i <- 0 until frontendIssueN) {
    decs(i).inst := Mux(wtg, decode_reg_line(i), io.icache.resp.bits.rdata(i))
    io.fb.fmbs.pcs(i) := decode_pc_low + (i.U << 2.U)
    io.fb.fmbs.inst_ops(i) := decs(i).mops
  }

}
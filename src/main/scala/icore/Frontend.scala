package icore

import chisel3._
import chisel3.util._
import conf.Config
import isa._
import fu.BPU
import MicroOpCtrl._
import chisel3.util.experimental.BoringUtils

// PCGen is can be non-project-specific, no with Config
class PCGenIO(va_width: Int = 32) extends Bundle with Config {
  val please_wait = Input(Bool()) // maybe from: A. a full-fifo and not killed; B. icache fetching from memory 
  val redirect = Input(Bool())
  val redirect_pc = Input(UInt(va_width.W))
  val please_narrow = if (frontendIssueN != 1) Input(Bool()) else null
  val predict_taken = Input(Bool())
  val pc_o = Output(UInt(va_width.W))
  val predict_taken_o = Output(Bool())
  val narrow_o = if (frontendIssueN != 1) Output(Bool()) else null
  override def cloneType = (new PCGenIO(va_width)).asInstanceOf[this.type]
}

class PCGen(va_width: Int = 32, start_va: String = "h80000000", increment: Int = 4) extends Module with Config {
  val io = IO(new PCGenIO(va_width))
  val pc = RegInit(UInt(va_width.W), start_va.U)
  val pred = RegInit(false.B)
  pc := Mux(io.redirect, io.redirect_pc, Mux(io.please_wait, pc, pc + increment.U))
  pred := Mux(io.redirect, false.B, Mux(io.please_wait, pred, io.predict_taken))
  io.predict_taken_o := pred
  io.pc_o := pc
  if (frontendIssueN != 1) {
    val narrow = RegInit(false.B)
    narrow := Mux(io.redirect, false.B, Mux(io.please_wait, narrow, io.please_narrow))
    io.narrow_o := narrow
  }
}

class Frontend(diffTestV: Boolean) extends Module with Config with MemAccessType with FrontToBack {
  val io = IO(new FrontendIO)

  // IF
  val pc_gen         = Module(new PCGen(len, startAddr, 4 * frontendIssueN))
  val bpu            = Module(new BPU(depth=4, offset=3, width=len, issueN=frontendIssueN, rasDepth=0, instByte=4))
  val cache_stall    = Wire(Bool())
  val fetch_half     = Wire(Bool())
  val last_req_valid = RegInit(false.B)
  val repc           = Reg(UInt(len.W))
  val repred         = Reg(Bool())
  val renarrow       = if (frontendIssueN != 1) Reg(Bool()) else null
  val stall_f        = Wire(Bool())
  val kill_f         = Wire(Bool())
  val illegal_pc     = pc_gen.io.pc_o(1, 0).orR

  // ID
  val decode_pc_low_reg = RegInit(UInt(len.W), startAddr.U)
  val decode_valid_reg  = RegInit(false.B)
  val decs              = Array.fill(frontendIssueN)(Module(new Dec).io)
  val stall_d           = Wire(Bool())
  val kill_d            = Wire(Bool())
  val decode_pc_predict_taken = RegInit(false.B)  // the first might be branch, the second must be delay slot which is not a branch instruction

  // IF Stage
  stall_f        := stall_d || cache_stall
  kill_f         := kill_d
  fetch_half     := io.icache.resp.bits.respn === 0.U
  cache_stall    := !io.icache.resp.valid && last_req_valid
  last_req_valid := io.icache.req.valid

  pc_gen.io.please_wait := stall_f
  pc_gen.io.redirect    := kill_f || (fetch_half && !cache_stall) || bpu.io.resp.taken_vec(0)
  pc_gen.io.redirect_pc := Mux(kill_f, io.fb.bmfs.redirect_pc, Mux(!bpu.io.resp.taken_vec(0), pc_gen.io.pc_o + 4.U, bpu.io.resp.target_first))
  pc_gen.io.predict_taken := bpu.io.resp.taken_vec(0)
  if (frontendIssueN != 1) {
    pc_gen.io.please_narrow := !bpu.io.resp.taken_vec(0) && bpu.io.resp.taken_vec(1)
  }

  repc := Mux(stall_f, repc, pc_gen.io.pc_o)
  repred := Mux(stall_f, repred, pc_gen.io.predict_taken_o)
  if (frontendIssueN != 1) {
    renarrow := Mux(stall_f, renarrow, pc_gen.io.narrow_o)
  }

  val may_illegal_req_addr = Mux(stall_f, repc, pc_gen.io.pc_o)
  io.icache.req.valid      := true.B
  io.icache.resp.ready     := true.B 
  io.icache.req.bits.addr  := Cat(may_illegal_req_addr(len - 1, 2), Fill(2, 0.U))
  io.icache.req.bits.wdata := DontCare
  io.icache.req.bits.wen   := false.B
  if (frontendIssueN == 1) {
    io.icache.req.bits.mtype := MEM_WORD.U
  } else {
    io.icache.req.bits.mtype := Mux(stall_f, Mux(renarrow, MEM_WORD.U, MEM_DWORD.U), Mux(pc_gen.io.narrow_o, MEM_WORD.U, MEM_DWORD.U))
  }
  io.icache.req.bits.flush := false.B
  io.icache.req.bits.invalidate := false.B

  bpu.io.req.next_line := pc_gen.io.pc_o + 8.U - 4.U  // subtract 4 because of delay slot
  // Here, BHT+BTB only serves the first instruction
  bpu.io.update.dec.pc_br  := decs(0).pc
  bpu.io.update.dec.v      := decs(0).bht_predict_taken && decs(0).mops.next_pc =/= Branch   // TODO currently predict taken but not branch, to add backward jump
  bpu.io.update.exe := io.fb.bmfs.bpu

  // ID Stage
  stall_d := io.fb.fmbs.please_wait
  kill_d  := io.fb.bmfs.redirect_kill

  when (kill_d) {
    decode_valid_reg  := false.B
  }.elsewhen (!stall_d && !cache_stall) {
    decode_pc_low_reg := may_illegal_req_addr
    decode_pc_predict_taken := Mux(stall_f, repred, pc_gen.io.predict_taken_o)
    decode_valid_reg  := io.icache.req.valid
  }

  io.fb.fmbs.instn := Mux(stall_d, 0.U,
    Mux(!cache_stall && decode_valid_reg, Cat(0.U, RegNext(io.icache.resp.bits.respn)) + 1.U, 0.U)
  )
  for (i <- 0 until frontendIssueN) {
    decs(i).inst              := io.icache.resp.bits.rdata(i)
    decs(i).pc                := decode_pc_low_reg + (i.U << 2.U)
    if (i == 0) {
      decs(0).bht_predict_taken := decode_pc_predict_taken
    } else {
      decs(i).bht_predict_taken := false.B
    }
    io.fb.fmbs.inst_ops(i) := decs(i).mops.asUInt
  }

  if (diffTestV) {
    BoringUtils.addSource(cache_stall, "icache_stall")
  }

}
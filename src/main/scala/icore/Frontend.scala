package icore

import chisel3._
import chisel3.util._
import conf.Config
import isa._
import fu._
import MicroOpCtrl._
import chisel3.util.experimental.BoringUtils

// PCGen is can be non-project-specific, no with Config
class PCGenIO(va_width: Int = 32) extends Bundle with Config {
  val please_wait = Input(Bool()) // maybe from: A. a full-fifo and not killed; B. icache fetching from memory 
  val redirect = Input(Bool())
  val redirect_pc = Input(UInt(va_width.W))
  val pc_o = Output(UInt(va_width.W))
  val predict_taken_o = Output(Bool())
  val predict_target_o = Output(UInt(va_width.W))
  val narrow_o = if (frontendIssueN != 1) Output(Bool()) else null
  val bpu_update = new BHTUpdate(va_width)
  override def cloneType = (new PCGenIO(va_width)).asInstanceOf[this.type]
}

class PCGen(va_width: Int = 32, start_va: String = "h80000000", increment: Int = 4) extends Module with Config {
  val io  = IO(new PCGenIO(va_width))
  val pc  = RegInit(UInt(va_width.W), start_va.U)
  val bpu = Module(new BPU(depth=BPUEntryN, offset=BPUOffset, width=len, issueN=frontendIssueN, rasDepth=0, instByte=4))
  val npc = Mux(io.redirect, io.redirect_pc, Mux(io.please_wait, pc, Mux(bpu.io.resp.taken_vec(0), Cat(bpu.io.resp.target_first(len - 1, 2), "b00".U), pc + increment.U)))

  // BPU
  bpu.io.req.next_line := npc
  bpu.io.update        := io.bpu_update

  pc := npc
  io.predict_taken_o  := bpu.io.resp.taken_vec(0)
  io.predict_target_o := bpu.io.resp.target_first
  io.pc_o := pc
  if (frontendIssueN != 1) {
    io.narrow_o := !bpu.io.resp.taken_vec(0) && bpu.io.resp.taken_vec(1)
  }
}

class Frontend(diffTestV: Boolean) extends Module with Config with MemAccessType with FrontToBack {
  val io = IO(new FrontendIO)

  // IF
  val pc_gen         = Module(new PCGen(len, startAddr, 4 * frontendIssueN))
  val cache_stall    = Wire(Bool())
  val fetch_half     = Wire(Bool())
  val last_req_valid = RegInit(false.B)
  val repc           = Reg(UInt(len.W))
  val reptar         = Reg(UInt(len.W))
  val repred         = Reg(Bool())
  val renarrow       = if (frontendIssueN != 1) Reg(Bool()) else null
  val stall_f        = Wire(Bool())
  val kill_f         = Wire(Bool())
  val illegal_pc     = pc_gen.io.pc_o(1, 0).orR
  val dec_kill_redirect_pc = Wire(UInt(len.W))
  val may_illegal_req_addr = Mux(stall_f, repc, pc_gen.io.pc_o)

  // ID
  val decode_pc_low_reg = RegInit(UInt(len.W), startAddr.U)
  val decode_valid_reg  = RegInit(false.B)
  val decs              = Array.fill(frontendIssueN)(Module(new Dec).io)
  val predict_taken_but_not_br = Wire(Bool())
  val dec_kill_d        = Wire(Bool())
  val wfds              = if (predictLastWordInCache) RegInit(false.B) else null
  val wfds_target       = if (predictLastWordInCache) Reg(UInt(len.W)) else null
  val frontend_fire     = Wire(Bool())
  val next_respn        = RegNext(io.icache.resp.bits.respn)
  val fire_number_respn = Cat(0.U, next_respn) + 1.U
  val stall_d           = Wire(Bool())
  val kill_d            = Wire(Bool())
  val decode_pc_predict_target = Reg(UInt(len.W))
  val decode_pc_predict_taken  = Reg(Bool())  // the first might be branch, the second must be delay slot which is not a branch instruction
  def quickCheckBranch(inst: UInt) : Bool = {
    // BXX BXXZAL JAL J
    inst(31, 29) === "b000".U && inst(28, 26) =/= "b000".U
  }

  // IF Stage
  stall_f        := stall_d || cache_stall
  kill_f         := kill_d
  if (frontendIssueN == 1) {
    fetch_half := false.B
  } else {
    fetch_half := io.icache.resp.bits.respn === 0.U
  }
  cache_stall    := !io.icache.resp.valid && last_req_valid
  last_req_valid := io.icache.req.valid

  // for predict taken but not br, detected in ID, if stall_d -> redirect the first pc in dec, else if dec has 1 instruction, redirect to pc + 4, else redirect to pc + 8
  pc_gen.io.please_wait := stall_f
  pc_gen.io.redirect    := kill_f || (fetch_half && !stall_f)
  if (predictLastWordInCache) {
    pc_gen.io.redirect_pc := Mux(kill_f, Mux(io.fb.bmfs.redirect_kill, io.fb.bmfs.redirect_pc, Mux(wfds, wfds_target, dec_kill_redirect_pc)), may_illegal_req_addr + 4.U)
  } else {
    pc_gen.io.redirect_pc := Mux(kill_f, Mux(io.fb.bmfs.redirect_kill, io.fb.bmfs.redirect_pc, dec_kill_redirect_pc), may_illegal_req_addr + 4.U)
  }

  pc_gen.io.bpu_update.dec.pc_br := Cat(decode_pc_low_reg(len - 1, 2), decode_pc_predict_target(1, 0))
  pc_gen.io.bpu_update.dec.v := predict_taken_but_not_br && frontend_fire
  pc_gen.io.bpu_update.exe := io.fb.bmfs.bpu
  dec_kill_redirect_pc  := Mux(stall_d, decode_pc_low_reg, Mux(next_respn.orR, decode_pc_low_reg + 8.U, decode_pc_low_reg + 4.U))

  repc := Mux(stall_f, repc, pc_gen.io.pc_o)
  reptar := Mux(stall_f, reptar, pc_gen.io.predict_target_o)
  repred := Mux(stall_f, repred, pc_gen.io.predict_taken_o)
  if (frontendIssueN != 1) {
    renarrow := Mux(stall_f, renarrow, pc_gen.io.narrow_o)
  }

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

  // ID Stage
  stall_d := io.fb.fmbs.please_wait
  kill_d  := io.fb.bmfs.redirect_kill || dec_kill_d

  when (kill_d) {
    decode_valid_reg  := false.B
  }.elsewhen (!stall_d && !cache_stall) {
    decode_pc_low_reg := may_illegal_req_addr
    decode_pc_predict_target := Mux(stall_f, reptar, pc_gen.io.predict_target_o)
    decode_pc_predict_taken  := Mux(stall_f, repred, pc_gen.io.predict_taken_o)
    decode_valid_reg  := io.icache.req.valid
  }

  frontend_fire := !cache_stall && decode_valid_reg
  if (predictLastWordInCache) {
    io.fb.fmbs.instn := Mux(stall_d, 0.U,
      Mux(frontend_fire, Mux(wfds, 1.U, fire_number_respn), 0.U)
    )
  } else {
    io.fb.fmbs.instn := Mux(stall_d, 0.U,
      Mux(frontend_fire, fire_number_respn, 0.U)
    )
  }

  for (i <- 0 until frontendIssueN) {
    decs(i).inst              := io.icache.resp.bits.rdata(i)
    decs(i).pc                := decode_pc_low_reg + (i.U << 2.U)
    if (i == 0) {
      if (predictLastWordInCache) {
        decs(0).bht_predict_taken := decode_pc_predict_taken
      } else {
        decs(0).bht_predict_taken := Mux(decode_pc_low_reg(offsetBits - 1, 2) + 1.U === 0.U, false.B, decode_pc_predict_taken)
      }
      decs(0).target_pc := decode_pc_predict_target
    } else {
      decs(i).bht_predict_taken := false.B
      decs(i).target_pc := DontCare
    }
    io.fb.fmbs.inst_ops(i) := decs(i).mops.asUInt
  }
  predict_taken_but_not_br := decode_pc_predict_taken && !quickCheckBranch(io.icache.resp.bits.rdata(0))

  if (predictLastWordInCache) {
    wfds := Mux(io.fb.bmfs.redirect_kill, false.B, Mux(!stall_d && frontend_fire && decode_pc_low_reg(offsetBits - 1, 2) + 1.U === 0.U && decode_pc_predict_taken && quickCheckBranch(io.icache.resp.bits.rdata(0)), true.B, Mux(frontend_fire, false.B, wfds)))
    wfds_target := Mux(!wfds, decode_pc_predict_target, wfds_target)
  }

  if (traceBPU) {
    when (predict_taken_but_not_br && frontend_fire) {
      printf("misprediction at %x, not branch\n", decode_pc_low_reg)
    }
  }

  if (predictLastWordInCache) {
    dec_kill_d := (predict_taken_but_not_br || wfds) && frontend_fire
  } else {
    dec_kill_d := predict_taken_but_not_br && frontend_fire
  }

  if (diffTestV) {
    BoringUtils.addSource(cache_stall, "icache_stall")
  }

}
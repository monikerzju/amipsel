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
  val predict_sstat_o = Output(UInt(2.W))
  val predict_taken_o = Output(Bool())
  val predict_target_o = Output(UInt(va_width.W))
  val narrow_o = if (frontendIssueN != 1) Output(Bool()) else null
  val bpu_update = new BHTUpdate(va_width)
  override def cloneType = (new PCGenIO(va_width)).asInstanceOf[this.type]
}

class PCGen(va_width: Int = 32, start_va: String = "h80000000", increment: Int = 4) extends Module with Config {
  val io  = IO(new PCGenIO(va_width))

  val pc  = RegInit(UInt(va_width.W), start_va.U)
  val bpu = Module(new BPU(depth=BPUEntryN, offset=BPUOffset, width=len, issueN=frontendIssueN, instByte=4, cacheDepth=BHTCacheEntryN))

  val legal_target = Cat(bpu.io.resp.target_first(len - 1, 2), "b00".U(2.W))
  val cross_line = pc(offsetBits - 1, 2) === Fill(offsetBits - 2, 1.U)
  val fetch_one_word = io.narrow_o || cross_line
  val last_inst_in_line_predict_taken = RegInit(false.B)
  val last_inst_target = Reg(UInt(va_width.W))
  val reset_last_inst_pred = last_inst_in_line_predict_taken && !io.please_wait

  val npc = Mux(io.redirect, io.redirect_pc, Mux(io.please_wait, pc, Mux(fetch_one_word, Mux(last_inst_in_line_predict_taken, last_inst_target, pc + 4.U), Mux(bpu.io.resp.taken_vec(0), legal_target, pc + increment.U))))

  // last word in cache line predict taken
  when (io.redirect || reset_last_inst_pred) {
    last_inst_in_line_predict_taken := false.B
  }.elsewhen (cross_line && bpu.io.resp.taken_vec(0) && !io.please_wait) {
    last_inst_in_line_predict_taken := true.B
  }
  last_inst_target := Mux(cross_line, legal_target, last_inst_target)

  // BPU
  bpu.io.req.next_line := npc
  bpu.io.update        := io.bpu_update

  pc := npc
  io.predict_taken_o  := bpu.io.resp.taken_vec(0)
  io.predict_target_o := bpu.io.resp.target_first
  io.pc_o := pc
  io.predict_sstat_o := bpu.io.resp.state_second
  if (frontendIssueN != 1) {
    io.narrow_o := !bpu.io.resp.taken_vec(0) && bpu.io.resp.taken_vec(1) || last_inst_in_line_predict_taken
  }
}

class Frontend(diffTestV: Boolean) extends Module with Config with MemAccessType with FrontToBack {
  val io = IO(new FrontendIO)

  val tlb = Module(new TLB)
  tlb.io.virt_addr := 0.U
  tlb.io.din := 0.U.asTypeOf(new TLBEntryIO)
  tlb.io.refType := 0.U(2.W)
  tlb.io.op := 0.U(2.W)
  // IF
  val pc_gen         = Module(new PCGen(len, startAddr, 4 * frontendIssueN))
  val cache_stall    = Wire(Bool())
  val fetch_half     = Wire(Bool())
  val last_req_valid = RegInit(false.B)
  val repc           = Reg(UInt(len.W))
  val reptar         = Reg(UInt(len.W))
  val repstat        = Reg(UInt(2.W))
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
  val frontend_fire     = Wire(Bool())
  val next_respn        = RegNext(io.icache.resp.bits.respn)
  val fire_number_respn = Cat(0.U, next_respn) + 1.U
  val stall_d           = Wire(Bool())
  val kill_d            = Wire(Bool())
  val decode_pc_predict_target = Reg(UInt(len.W))
  val decode_pc_second_state   = Reg(UInt(2.W))
  val decode_pc_predict_taken  = Reg(Bool())  // the first might be branch, the second must be delay slot which is not a branch instruction
  val delayed_early_update = RegInit(false.B)
  def quickCheckBranch(inst: UInt) : Bool = {
    // BXX BXXZAL JAL J
    inst(31, 29) === 0.U && inst(28, 26) =/= 0.U
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
  pc_gen.io.redirect    := kill_f
  pc_gen.io.redirect_pc := Mux(io.fb.bmfs.redirect_kill, io.fb.bmfs.redirect_pc, dec_kill_redirect_pc)

  pc_gen.io.bpu_update.dec.pc_br := RegNext(Cat(decode_pc_low_reg(len - 1, 2), decode_pc_predict_target(1, 0)))
  pc_gen.io.bpu_update.dec.v := delayed_early_update
  pc_gen.io.bpu_update.exe := io.fb.bmfs.bpu
  dec_kill_redirect_pc  := RegNext(Mux(stall_d, decode_pc_low_reg, Mux(next_respn.orR, decode_pc_low_reg + 8.U, decode_pc_low_reg + 4.U)))

  repc := Mux(stall_f, repc, pc_gen.io.pc_o)
  reptar := Mux(stall_f, reptar, pc_gen.io.predict_target_o)
  repstat := Mux(stall_f, repstat, pc_gen.io.predict_sstat_o)
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
    decode_pc_second_state   := Mux(stall_f, repstat, pc_gen.io.predict_sstat_o)
    decode_pc_predict_taken  := Mux(stall_f, repred, pc_gen.io.predict_taken_o)
    decode_valid_reg  := io.icache.req.valid
  }

  frontend_fire := !cache_stall && decode_valid_reg && !delayed_early_update
  io.fb.fmbs.instn := Mux(stall_d, 0.U, Mux(frontend_fire, fire_number_respn, 0.U))

  for (i <- 0 until frontendIssueN) {
    decs(i).inst              := io.icache.resp.bits.rdata(i)
    decs(i).pc                := decode_pc_low_reg + (i.U << 2.U)
    if (i == 0) {
      decs(0).bht_predict_taken := Mux(decode_pc_low_reg(offsetBits - 1, 2) === 0.U && !next_respn, false.B, decode_pc_predict_taken)
      decs(0).target_pc := decode_pc_predict_target
    } else {
      decs(i).bht_predict_taken := false.B
      decs(i).target_pc := Cat(Fill(30, 0.U), decode_pc_second_state)
    }
    io.fb.fmbs.inst_ops(i) := decs(i).mops.asUInt
  }
  predict_taken_but_not_br := decs(0).bht_predict_taken && decs(0).mops.next_pc =/= Branch && decs(0).mops.next_pc =/= Jump && decs(0).mops.next_pc =/= PCReg // TODO !quickCheckBranch(io.icache.resp.bits.rdata(0))

  if (traceBPU) {
    when (delayed_early_update) {
      printf("misprediction at %x, not branch\n", RegNext(Cat(decode_pc_low_reg(len - 1, 2), 0.U(2.W))))
    }
  }

  dec_kill_d := delayed_early_update

  delayed_early_update := Mux(io.fb.bmfs.redirect_kill, false.B, predict_taken_but_not_br && frontend_fire)

  if (diffTestV) {
    BoringUtils.addSource(cache_stall, "icache_stall")
  }

}
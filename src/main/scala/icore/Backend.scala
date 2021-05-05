package icore

import chisel3._
import chisel3.util._
import conf.Config
import fu._
import isa._
import chisel3.experimental.BundleLiterals._
import chisel3.util.experimental.BoringUtils

// TODO Maybe toALU toMDU toLSU toBJU
// but multiplex by ALSU AMU LSBJU
trait InstType {
  val typeLen = 2
  val toBJU = 0
  val toMDU = 1
  val toLSU = 2
  val toALU = 3
}

class StoreInfo extends Bundle with Config {
  val addr = UInt(len.W)
  val data = UInt(len.W)
}

// TODO ex dont care expt mask, move kill expt to wb, dcache sw need to be confirmed at wb
// TODO fwd dont need to care wb or not, valid -> ok, because expt will just cause redirecting
class Backend(diffTestV: Boolean) extends Module with Config with InstType with MemAccessType with CauseExcCode {
  val io = IO(new BackendIO)

  // Global
  val nop = (new Mops).Lit(
    _.illegal -> false.B,
    _.rs1 -> 0.U,
    _.rs2 -> 0.U,
    _.rd -> 0.U,
    _.alu_op -> 1.U,  // addu will never cause except
    _.imm -> 0.U,
    _.alu_mdu_lsu -> toALU.U,
    _.write_dest -> MicroOpCtrl.DXXX,
    _.next_pc -> MicroOpCtrl.PC4,
    _.src_a -> MicroOpCtrl.AXXX,
    _.src_b -> MicroOpCtrl.BXXX,
    _.branch_type -> MicroOpCtrl.BrXXX,
    _.mem_width -> MicroOpCtrl.MemXXX,
    _.write_src -> MicroOpCtrl.WBALU,
    _.pc -> 0.U
  )
  val alu          = Module(new ALU)
  val mdu          = Module(new MDU)

  // Issue
  val issueNum     = Wire(UInt(2.W))
  val issueInsts   = Wire(Vec(backendIssueN, new Mops))
  val issueRss     = Wire(Vec(backendFuN, UInt(len.W)))
  val issueRts     = Wire(Vec(backendFuN, UInt(len.W)))
  val issueQueue   = Module(new FIFO(queueSize, new Mops(), backendIssueN, frontendIssueN))
  val issueArbiter = Module(new IssueArbiter(queueSize))
  val stall_i      = io.dcache.req.valid && !io.dcache.resp.valid || !mdu.io.resp.valid
  val kill_i       = io.fb.bmfs.redirect_kill
  val rsFwdData    = Wire(Vec(backendIssueN, UInt(len.W)))
  val rtFwdData    = Wire(Vec(backendIssueN, UInt(len.W)))
  val rsData       = Wire(Vec(backendIssueN, UInt(len.W)))
  val rtData       = Wire(Vec(backendIssueN, UInt(len.W)))

  // Ex
  val stall_x      = stall_i
  val kill_x       = kill_i
  val exNum        = RegInit(0.U(2.W))
  val exInsts      = RegInit(VecInit(Seq.fill(backendFuN)(nop)))
  val exInstsOrder = if(diffTestV) RegInit(VecInit(Seq.fill(backendFuN)(0.U(2.W)))) else Reg(Vec(backendFuN, UInt(2.W)))
  val exInstsValid = RegInit(VecInit(Seq.fill(backendFuN)(false.B)))
  val exFwdRsData  = Reg(Vec(backendFuN, UInt(len.W)))
  val exFwdRtData  = Reg(Vec(backendFuN, UInt(len.W)))
  val reBranch     = Wire(Bool())
  val jumpPc       = Wire(UInt(len.W))
  val brPC         = exInsts(0).pc + (exInsts(0).imm << 2.U)(len - 1, 0) + 4.U
  val exReBranchPC = Mux(exInsts(0).next_pc === MicroOpCtrl.Branch, brPC, jumpPc)
  val aluValid     = Wire(Bool())
  val mduValid     = Wire(Bool())
  val ldstValid    = Wire(Bool())
  val ldstAddr     = exFwdRsData(2) + exInsts(2).imm 
  val aluWbData    = Wire(UInt(len.W))
  val exIsBrFinal  = exInstsOrder(0) === exNum - 1.U
  val exInterruptd = RegInit(false.B)
  val memMisaligned = MuxLookup(
    exInsts(2).mem_width, false.B,
    Seq(
      MicroOpCtrl.MemHalf  -> ldstAddr(0).asBool,
      MicroOpCtrl.MemHalfU -> ldstAddr(0).asBool,
      MicroOpCtrl.MemWord  -> ldstAddr(1, 0).orR
    )
  )
  val ldMisaligned = Wire(Bool())
  val stMisaligned = Wire(Bool())
  val exInstsTrueValid = Wire(Vec(backendFuN, Bool()))
  val aluExptMask      = (exInstsValid(1) && mdu.io.resp.except && exInstsOrder(1) < exInstsOrder(0) ||
                          exInstsValid(2) && memMisaligned && exInstsOrder(2) < exInstsOrder(0))
  val mduExptMask      = (exInstsValid(0) && alu.io.ovf && exInstsOrder(0) < exInstsOrder(1) ||
                          exInstsValid(2) && memMisaligned && exInstsOrder(2) < exInstsOrder(1))
  val ldstExptMask     = (exInstsValid(0) && alu.io.ovf && exInstsOrder(0) < exInstsOrder(2) ||
                          exInstsValid(1) && mdu.io.resp.except && exInstsOrder(1) < exInstsOrder(2))

  // WB
  val wfds             = RegInit(false.B) // wait for delay slot, this name is from RV's WFI instruction
  val reBranchPC       = RegInit(startAddr.U(len.W))
  val hi               = RegInit(0.U(len.W))
  val lo               = RegInit(0.U(len.W))
  val wbResult         = RegInit(VecInit(Seq.fill(backendFuN)(0.U(len.W))))
  val wbInstsValid     = Reg(Vec(backendFuN, Bool()))
  val wbInstsOrder     = RegNext(exInstsOrder)
  val wbInsts          = RegInit(VecInit(Seq.fill(backendFuN)(nop)))
  val kill_w           = io.fb.bmfs.redirect_kill
  val bubble_w         = stall_x
  val regFile          = Module(new RegFile(nread = 2 * backendIssueN, nwrite = backendIssueN)) // 4 read port, 2 write port
  val cp0              = Module(new CP0)
  val wbData           = Wire(Vec(backendFuN, UInt(len.W)))
  val wbReBranch       = RegInit(false.B)
  val latestBJPC       = RegInit(startAddr.U) // cannot deal with the second is Exception and the first is non-bj
  val wbExcepts        = Wire(Vec(backendFuN, Bool()))
  val wbMisalignedAddr = RegNext(io.dcache.req.bits.addr)
  val wbInterruptd     = RegNext(exInterruptd)

  /**
   *  [---------- IS stage -----------]
   */
  io.fb.fmbs.please_wait := !issueQueue.io.sufficient
  issueQueue.io.flush   := kill_i
  issueQueue.io.deqStep := issueNum
  issueQueue.io.deqReq  := !stall_i && !(issueQueue.io.items.orR && issueNum === 0.U)
  issueQueue.io.enqReq  := io.fb.fmbs.instn =/= 0.U
  issueQueue.io.enqStep := io.fb.fmbs.instn
  for (i <- 0 until backendIssueN) {
    issueInsts(i) := issueQueue.io.dout(i)
  }
  for(i <- 0 until frontendIssueN) {
    issueQueue.io.din(i) := io.fb.fmbs.inst_ops(i).asTypeOf(new Mops)
  }

  val trap_ret_items1 = Mux(issueInsts(1).next_pc(2).andR || issueInsts(1).illegal,  // break syscall eret
    Mux(issueQueue.io.items >= 2.U, 2.U, issueQueue.io.items),
    issueQueue.io.items
  )
  val trap_ret_items0 = Mux(issueQueue.io.items >= 1.U, 1.U, issueQueue.io.items)
  if (backendIssueN == 3) {
    issueArbiter.io.queue_items := Mux(issueInsts(0).next_pc(2).andR || issueInsts(0).illegal, trap_ret_items0,
      Mux(issueInsts(0).next_pc =/= MicroOpCtrl.PC4, 
        Mux(issueQueue.io.items >= 3.U, 2.U, issueQueue.io.items), 
        trap_ret_items1
      )  
    )
  } else {
    issueArbiter.io.queue_items := Mux(issueInsts(0).next_pc(2).andR || issueInsts(0).illegal, trap_ret_items0,
      issueQueue.io.items
    )
  }
  issueArbiter.io.ld_dest_ex  := Fill(32, exInstsValid(2)) & exInsts(2).rd
  issueArbiter.io.mtc0_ex     := exInstsValid(0) & exInsts(0).write_dest === MicroOpCtrl.DCP0
  issueArbiter.io.insts_in    := issueInsts
  issueArbiter.io.rss_in      := rsFwdData
  issueArbiter.io.rts_in      := rtFwdData
  issueNum                    := issueArbiter.io.issue_num
  issueRss                    := issueArbiter.io.rss_out
  issueRts                    := issueArbiter.io.rts_out

  // load to something and mtc0 to mfc0 fwd is canceled, just stall
  // default to regfile
  for(i <- 0 until backendIssueN) {
    rsFwdData(i) := rsData(i)
    rtFwdData(i) := rtData(i)
  }
  // wb to is
  for (j <- 0 until backendFuN) {
    when(wbInstsValid(j) && wbInsts(j).write_dest === MicroOpCtrl.DReg && wbInsts(j).rd =/= 0.U) {
      for(i <- 0 until backendIssueN) {
        when(wbInsts(j).rd === issueInsts(i).rs1) {
          rsFwdData(i) := wbData(j)
        }
        when(wbInsts(j).rd === issueInsts(i).rs2) {
          rtFwdData(i) := wbData(j)
        }
      }
    }    
  }
  // ex to is
  when(exInstsValid(0) && exInsts(0).write_dest === MicroOpCtrl.DReg && exInsts(0).rd =/= 0.U) {
    for(i <- 0 until backendIssueN) {
      when(exInsts(0).rd === issueInsts(i).rs1) {
        rsFwdData(i) := aluWbData
      }
      when(exInsts(0).rd === issueInsts(i).rs2) {
        rtFwdData(i) := aluWbData
      }
    }
  }
  when(exInstsValid(1) && exInsts(1).write_dest === MicroOpCtrl.DReg && exInsts(1).rd =/= 0.U) {
    for(i <- 0 until backendIssueN) {
      when(exInsts(1).rd === issueInsts(i).rs1) {
        rsFwdData(i) := mdu.io.resp.lo
      }
      when(exInsts(1).rd === issueInsts(i).rs2) {
        rtFwdData(i) := mdu.io.resp.lo
      }
    }
  }

  /*
    read port rs_1 rt_1 rs_2 rt_2 rs_3 rt_3 rs_4 rs_4
   */
  for(i <- 0 until backendIssueN) {
    regFile.io.rs_addr_vec(2 * i) := issueInsts(i).rs1
    regFile.io.rs_addr_vec(2 * i + 1) := issueInsts(i).rs2
  }

  // forward all the data here
  // assume I-type Inst replace rt with rd, and update rt = 0
  for(i <- 0 until backendIssueN) {
    rsData(i) := regFile.io.rs_data_vec(2 * i)
    rtData(i) := regFile.io.rs_data_vec(2 * i + 1)
  }

  /**
   *  [---------- EX stage -----------]
   */ 
    /*
    exInsts(0) -> ALU
    exInsts(1) -> MDU
    exInsts(2) -> LSU
   */
  // when call for int, only alu will be valid no matter which fu will be valid normally,
  // so exInstsValid(0) := issueArbiter.io.issue_fu_valid.asUInt.orR
  // exInsts.pc must be the pc with the smallest order
  // so nopPC.pc := issueInsts(0).pc
  exNum := Mux(stall_x, exNum, Mux(kill_x, 0.U, issueNum))
  when (kill_x) {
    exInterruptd := false.B
    for(i <- 0 until backendFuN) {
      exInsts(i) := nop
      exInstsValid(i) := false.B
    }
  }.elsewhen(!stall_x) {
    exInstsOrder := issueArbiter.io.insts_order
    when (cp0.io.except.call_for_int) {
      exInstsValid(0) := issueArbiter.io.issue_fu_valid.asUInt.orR
      val nopPC = WireInit(nop)
      nopPC.pc := issueInsts(0).pc
      exInsts(0) := nopPC
      for (i <- 1 until backendFuN) {
        exInstsValid(i) := false.B
      }
    }.otherwise {
      exInstsValid := issueArbiter.io.issue_fu_valid
      exInsts(0)   := issueArbiter.io.insts_out(0)
    }
    for (i <- 1 until backendFuN) {
      exInsts(i) := issueArbiter.io.insts_out(i)
    }
    exInterruptd := cp0.io.except.call_for_int
  }

  when (!stall_x) {
    for (i <- 0 until backendFuN) {
      exFwdRsData(i) := issueRss(i)
      exFwdRtData(i) := issueRts(i)
    }
  }

  // if waiting for slot, must be the smallsest one
  aluValid   := exInstsValid(0) && !kill_x && (!wfds || exInstsOrder(0) === 0.U)
  mduValid   := exInstsValid(1) && !kill_x && (!wfds || exInstsOrder(1) === 0.U)
  ldstValid  := exInstsValid(2) && !kill_x && (!wfds || exInstsOrder(2) === 0.U)

  // alu execution
  alu.io.a := MuxLookup(exInsts(0).src_a, exFwdRsData(0),
    Seq(
      MicroOpCtrl.AShamt -> exInsts(0).imm(10, 6),
    )
  )
  alu.io.b := MuxLookup(exInsts(0).src_b, exFwdRtData(0),
    Seq(
      MicroOpCtrl.BImm -> exInsts(0).imm
    )
  )
  alu.io.rega  := exFwdRsData(0)
  alu.io.aluOp := exInsts(0).alu_op

  // mdu execution
  mdu.io.req.valid := mduValid
  mdu.io.req.reg1  := exFwdRsData(1)
  mdu.io.req.in1 := MuxLookup(exInsts(1).src_a, exFwdRsData(1),
    Seq(
      MicroOpCtrl.AShamt -> exInsts(1).imm(10, 6),
      MicroOpCtrl.AHi    -> hi,
      MicroOpCtrl.ALo    -> lo
    )
  )
  mdu.io.req.in2 := MuxLookup(exInsts(1).src_b, exFwdRtData(1),
    Seq(
      MicroOpCtrl.BImm -> exInsts(1).imm
    )
  )
  mdu.io.req.op := exInsts(1).alu_op
  hi := Mux(!bubble_w && exInstsTrueValid(1), 
    MuxLookup(exInsts(1).write_dest, hi, Seq(
      MicroOpCtrl.DHi -> mdu.io.resp.lo,
      MicroOpCtrl.DHiLo -> mdu.io.resp.hi
    )),
    hi
  )
  lo := Mux(!bubble_w && (exInsts(1).write_dest === MicroOpCtrl.DLo || exInsts(1).write_dest === MicroOpCtrl.DHiLo) &&
    exInstsTrueValid(1), 
    mdu.io.resp.lo, lo
  )

  // initialize lsu
  ldMisaligned := exInsts(2).write_dest =/= MicroOpCtrl.DMem && memMisaligned
  stMisaligned := exInsts(2).write_dest === MicroOpCtrl.DMem && memMisaligned
  io.dcache.req.bits.flush := false.B
  io.dcache.req.bits.invalidate := false.B
  io.dcache.req.valid := exInstsTrueValid(2) && !memMisaligned
  io.dcache.req.bits.mtype := exInsts(2).mem_width
  io.dcache.req.bits.wen := exInsts(2).write_dest === MicroOpCtrl.DMem
  io.dcache.req.bits.wdata := exFwdRtData(2)
  io.dcache.req.bits.addr := ldstAddr
  io.dcache.resp.ready := true.B

  // jump br
  val isExPCJump = exInsts(0).next_pc === MicroOpCtrl.PCReg || exInsts(0).next_pc === MicroOpCtrl.Jump
  val isExPCBr   = exInsts(0).next_pc === MicroOpCtrl.Branch
  reBranch := false.B
  jumpPc := Mux(
    exInsts(0).next_pc === MicroOpCtrl.Jump, 
    Cat(exInsts(0).pc(31, 28), exInsts(0).imm(25, 0), 0.U(2.W)),
    exFwdRsData(0)
  )
  when(exInstsTrueValid(0)) {
    when (isExPCBr) {
      reBranch := MuxLookup(exInsts(0).branch_type, alu.io.zero === 1.U,
        Seq(
          MicroOpCtrl.BrNE -> (alu.io.zero === 0.U),
          MicroOpCtrl.BrGE -> (alu.io.a.asSInt >= 0.S),
          MicroOpCtrl.BrGT -> (alu.io.a.asSInt > 0.S),
          MicroOpCtrl.BrLE -> (alu.io.a.asSInt <= 0.S),
          MicroOpCtrl.BrLT -> (alu.io.a.asSInt < 0.S)
        )
      )
    }.elsewhen (isExPCJump) {
      reBranch := true.B
    }
  }

  // not valid if exception before the inst
  exInstsTrueValid(0) := aluValid && !aluExptMask
  exInstsTrueValid(1) := mduValid && !mduExptMask
  exInstsTrueValid(2) := ldstValid && !ldstExptMask

  /**
   *  [---------- WB stage -----------]
   */
  wbInsts := exInsts
  aluWbData :=Mux(exInsts(0).write_src === MicroOpCtrl.WBPC || exInsts(0).next_pc =/= MicroOpCtrl.PC4,
    exInsts(0).pc + 8.U,
    MuxLookup(
      exInsts(0).src_a, alu.io.r,
      Seq(
        MicroOpCtrl.AHi  -> hi,
        MicroOpCtrl.ALo  -> lo,
        MicroOpCtrl.ACP0 -> cp0.io.ftc.dout
      )
    )
  )
  wbResult(0) := aluWbData
  wbResult(1) := mdu.io.resp.lo

  // handle load-inst separately
  val delayed_req_byte = RegNext(io.dcache.req.bits.addr(1, 0))
  val dataFromDcache = io.dcache.resp.bits.rdata(0) >> (delayed_req_byte << 3.U)
  val luData = Wire(UInt(len.W))
  luData := dataFromDcache
  switch(wbInsts(2).mem_width) {
    is(MicroOpCtrl.MemByte)  { luData := Cat(Fill(24, dataFromDcache(7)), dataFromDcache(7, 0)) }
    is(MicroOpCtrl.MemByteU) { luData := Cat(Fill(24, 0.U), dataFromDcache(7, 0)) }
    is(MicroOpCtrl.MemHalf)  { luData := Cat(Fill(16, dataFromDcache(15)), dataFromDcache(15, 0)) }
    is(MicroOpCtrl.MemHalfU) { luData := Cat(Fill(16, 0.U), dataFromDcache(15, 0)) }
  }
  wbData(0) := wbResult(0)
  wbData(1) := wbResult(1)
  wbData(2) := luData

  // delay slot and then just jump
  when (!bubble_w) {
    when (exInstsTrueValid(0) && exIsBrFinal && reBranch) {
      wfds := true.B
    }.elsewhen(exInstsValid.asUInt.orR) {
      wfds := false.B
    }
  }

  when (kill_w) {
    for(i <- 0 until backendFuN) {
      wbInstsValid(i) := false.B
    }
    wbReBranch := false.B
  }.elsewhen (bubble_w) {
    for(i <- 0 until backendFuN) {
      wbInstsValid(i) := false.B
    }
  }.otherwise {
    latestBJPC := Mux(exInstsTrueValid(0) && (isExPCBr || isExPCJump), 
      exInsts(0).pc, latestBJPC
    )
    for (i <- 0 until backendFuN) {
      wbInstsValid(i) := exInstsTrueValid(i)
    }
    when (!wfds) {
      reBranchPC := exReBranchPC
      wbReBranch := reBranch
    }
  }

  io.fb.bmfs.redirect_kill := (wbReBranch && !wfds) || cp0.io.except.except_kill
  io.fb.bmfs.redirect_pc   := Mux(cp0.io.except.except_kill, cp0.io.except.except_redirect, reBranchPC)

  // regfile
  if (backendIssueN == 3) {
    for(i <- 0 until backendFuN) {
      regFile.io.wen_vec(i) := wbInstsValid(i) && wbInsts(i).write_dest === MicroOpCtrl.DReg && !wbExcepts(i)
      regFile.io.rd_addr_vec(i) := wbInsts(i).rd
      regFile.io.rd_data_vec(i) := wbData(i)
    }
  } else {
    regFile.io.wen_vec(0) := Mux(
      wbInstsValid(0), wbInsts(0).write_dest === MicroOpCtrl.DReg && !wbExcepts(0), 
      wbInstsValid(1) && wbInsts(1).write_dest === MicroOpCtrl.DReg && !wbExcepts(1),
    )
    regFile.io.rd_addr_vec(0) := Mux(wbInstsValid(0), wbInsts(0).rd, wbInsts(1).rd)
    regFile.io.rd_data_vec(0) := Mux(wbInstsValid(0), wbData(0), wbData(1))
    regFile.io.wen_vec(1) := Mux(
      wbInstsValid(2), wbInsts(2).write_dest === MicroOpCtrl.DReg && !wbExcepts(2), 
      wbInstsValid(1) && wbInsts(1).write_dest === MicroOpCtrl.DReg && !wbExcepts(1),
    )
    regFile.io.rd_addr_vec(1) := Mux(wbInstsValid(2), wbInsts(2).rd, wbInsts(1).rd)
    regFile.io.rd_data_vec(1) := Mux(wbInstsValid(2), wbData(2), wbData(1))
  }

  // cp0
  val wbALUOvfReal  = wbInstsValid(0) && RegNext(alu.io.ovf)
  val wbMDUOvfReal  = wbInstsValid(1) && RegNext(mdu.io.resp.except)
  val wbLdMaReal    = wbInstsValid(2) && RegNext(ldMisaligned)
  val wbStMaReal    = wbInstsValid(2) && RegNext(stMisaligned)
  val wbALUSysReal  = wbInsts(0).next_pc === MicroOpCtrl.Trap && wbInstsValid(0)
  val wbALUBpReal   = wbInsts(0).next_pc === MicroOpCtrl.Break && wbInstsValid(0)
  val illegal       = wbInsts(0).illegal && wbInstsValid(0) // put all in illegal, but inst misaligned is of high prio
  val wbFetchMaReal = wbInsts(0).pc(1, 0).orR && wbInstsValid(0)  // will be in mops.illegal = 1
  val respInt       = wbInterruptd && wbInstsValid(0)
  wbExcepts(0) := wbALUOvfReal
  wbExcepts(1) := wbMDUOvfReal
  wbExcepts(2) := wbLdMaReal || wbStMaReal
  val wb_ev = Wire(Vec(SZ_EXC_CODE, Bool()))
  wb_ev(Interrupt   ) := false.B
  wb_ev(TLBModify   ) := false.B
  wb_ev(TLBLoad     ) := false.B
  wb_ev(TLBStore    ) := false.B
  wb_ev(AddrErrLoad ) := wbLdMaReal || wbFetchMaReal
  wb_ev(AddrErrStore) := wbStMaReal
  wb_ev(Syscall     ) := wbALUSysReal
  wb_ev(Breakpoint  ) := wbALUBpReal
  wb_ev(ReservedInst) := illegal
  wb_ev(Overflow    ) := wbALUOvfReal || wbMDUOvfReal
  wb_ev(Trap        ) := false.B
  wb_ev(Res0        ) := false.B
  wb_ev(Res1        ) := false.B
  wb_ev(Res2        ) := false.B
  cp0.io.except.except_vec    := wb_ev
  cp0.io.except.valid_inst    := wbInstsValid.asUInt.orR
  cp0.io.except.hard_int_vec  := io.interrupt
  cp0.io.except.ret           := wbInsts(0).next_pc === MicroOpCtrl.Ret
  cp0.io.except.epc           := Mux(wbALUSysReal || wbALUBpReal || wbALUOvfReal || illegal || respInt, wbInsts(0).pc, 
                                  Mux(wbMDUOvfReal, wbInsts(1).pc, wbInsts(2).pc)
                                 )
  cp0.io.except.in_delay_slot := cp0.io.except.epc === latestBJPC + 4.U
  cp0.io.except.bad_addr      := Mux(wbFetchMaReal, wbInsts(0).pc,  wbMisalignedAddr)
  cp0.io.except.resp_for_int  := respInt
  cp0.io.ftc.wen              := wbInsts(0).write_dest === MicroOpCtrl.DCP0
  cp0.io.ftc.code             := Mux(cp0.io.ftc.wen, wbInsts(0).rd, exInsts(0).rs1)
  cp0.io.ftc.sel              := 0.U  // TODO Config and Config1, fix it for Linux
  cp0.io.ftc.din              := wbData(0)
  
  // difftest
  if (diffTestV) {
    val instret    = RegInit(0.U(64.W))
    val counter    = RegInit(0.U(64.W))
    val dstall     = RegInit(0.U(64.W))
    val istallw    = WireDefault(false.B)
    val istall     = RegInit(0.U(64.W))
    val common     = RegInit(0.U(64.W))

    dstall  := dstall + Mux(io.dcache.req.valid && !io.dcache.resp.valid, 1.U, 0.U)
    istall  := istall + Mux(istallw, 1.U, 0.U)
    common  := common + Mux(io.dcache.req.valid && !io.dcache.resp.valid && istallw, 1.U, 0.U)
    counter := counter + 1.U
    instret := instret + (wbInstsValid(0) && !wbExcepts(0)).asUInt + (wbInstsValid(1) && !wbExcepts(1)).asUInt + (wbInstsValid(2) && !wbExcepts(2)).asUInt
    when (wbInsts(0).pc === endAddr.U || wbInsts(1).pc === endAddr.U || wbInsts(2).pc === endAddr.U) {
      printf("%d insts, %d cycles, %d d$ stalls, %d i$ stalls, %d common stalls\n", instret + 1.U, counter, dstall, istall, common)
      // TODO BPU mis-prediction, icache miss, dcache miss, mdu stall
    }

    val debug_pc   = Wire(Vec(backendFuN, UInt(len.W)))
    val debug_wen  = Wire(Vec(backendFuN, Bool()))
    val debug_data = Wire(Vec(backendFuN, UInt(len.W)))
    val debug_nreg = Wire(Vec(backendFuN, UInt(5.W)))
    for(i <- 0 until backendFuN) {
      debug_pc(i) := 0.U
      debug_wen(i) := false.B
      debug_data(i) := 0.U
      debug_nreg(i) := 0.U
    }
    for(i <- 0 until backendFuN) {
      when(wbInstsValid(i)) {
        switch(wbInstsOrder(i)) {
          is(0.U) {
            debug_pc(0) := wbInsts(i).pc
            debug_wen(0) := wbInsts(i).write_dest === MicroOpCtrl.DReg && wbInsts(i).rd =/= 0.U && !wbExcepts(i)
            debug_data(0) := MuxLookup(i.U, wbResult(0),
            Seq(0.U -> wbResult(0), 1.U -> wbResult(1), 2.U -> luData))
            debug_nreg(0) := wbInsts(i).rd
          }
          is(1.U) {
            debug_pc(1) := wbInsts(i).pc
            debug_wen(1) := wbInsts(i).write_dest === MicroOpCtrl.DReg && wbInsts(i).rd =/= 0.U && !wbExcepts(i)
            debug_data(1) := MuxLookup(i.U, wbResult(0),
            Seq(0.U -> wbResult(0), 1.U -> wbResult(1), 2.U -> luData))
            debug_nreg(1) := wbInsts(i).rd
          }
          is(2.U) {
            debug_pc(2) := wbInsts(i).pc
            debug_wen(2) := wbInsts(i).write_dest === MicroOpCtrl.DReg && wbInsts(i).rd =/= 0.U && !wbExcepts(i)
            debug_data(2) := MuxLookup(i.U, wbResult(0),
            Seq(0.U -> wbResult(0), 1.U -> wbResult(1), 2.U -> luData))
            debug_nreg(2) := wbInsts(i).rd
          }
        }
      }
    }

    BoringUtils.addSink  (istallw,    "icache_stall")
    BoringUtils.addSource(debug_pc,   "dt_pc"       )
    BoringUtils.addSource(debug_wen,  "dt_wen"      )
    BoringUtils.addSource(debug_data, "dt_data"     )
    BoringUtils.addSource(debug_nreg, "dt_nreg"     )
  }
}
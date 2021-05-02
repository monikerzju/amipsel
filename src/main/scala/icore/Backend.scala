package icore

import chisel3._
import chisel3.util._
import conf.Config
import fu._
import isa._
import chisel3.experimental.BundleLiterals._
import chisel3.util.experimental.BoringUtils

trait InstType {
  val typeLen = 3
  val toALU = 0
  val toMDU = 1
  val toLU = 2
  val toSU = 3
  val toAMU = 4
}

class StoreInfo extends Bundle with Config {
  val addr = UInt(len.W)
  val data = UInt(len.W)
}

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
  val issueInsts   = Wire(Vec(3, new Mops))
  val issueQueue   = Module(new FIFO(queueSize, new Mops(), backendIssueN, frontendIssueN))
  val issueArbiter = Module(new IssueArbiter(queueSize))
  val stall_i      = io.dcache.req.valid && !io.dcache.resp.valid || !mdu.io.resp.valid
  val kill_i       = io.fb.bmfs.redirect_kill
  val isRsFwd      = Wire(Vec(4, Bool()))
  val isRtFwd      = Wire(Vec(4, Bool()))
  val rsFwdIndex   = Wire(Vec(4, UInt(1.W)))
  val rtFwdIndex   = Wire(Vec(4, UInt(1.W)))

  // Ex
  val stall_x      = stall_i
  val kill_x       = kill_i
  val exNum        = RegInit(0.U(2.W))
  val exInsts      = RegInit(VecInit(Seq.fill(4)(nop)))
  val exInstsOrder = if(diffTestV) RegInit(VecInit(Seq.fill(4)(0.U(2.W)))) else Reg(Vec(4, UInt(2.W)))
  val exInstsValid = RegInit(VecInit(Seq.fill(4)(false.B)))
  val fwdRsData    = Wire(Vec(4, UInt(len.W)))
  val fwdRtData    = Wire(Vec(4, UInt(len.W)))
  val exIsRsFwd    = RegInit(VecInit(Seq.fill(4)(false.B)))
  val exIsRtFwd    = RegInit(VecInit(Seq.fill(4)(false.B)))
  val exRsFwdIndex = RegInit(VecInit(Seq.fill(4)(0.U(1.W))))
  val exRtFwdIndex = RegInit(VecInit(Seq.fill(4)(0.U(1.W))))
  val rsData       = Wire(Vec(4, UInt(len.W)))
  val rtData       = Wire(Vec(4, UInt(len.W)))
  val reBranch     = Wire(Bool())
  val jumpPc       = Wire(UInt(len.W))
  val brPC         = exInsts(0).pc + (exInsts(0).imm << 2.U)(len - 1, 0) + 4.U
  val exReBranchPC = Mux(exInsts(0).next_pc === MicroOpCtrl.Branch, brPC, jumpPc)
  val aluValid     = Wire(Bool())
  val mduValid     = Wire(Bool())
  val loadValid    = Wire(Bool())
  val storeValid   = Wire(Bool())
  val loadAddr     = fwdRsData(2) + exInsts(2).imm
  val storeAddr    = fwdRsData(3) + exInsts(3).imm  
  val exIsBrFinal  = exInstsOrder(0) === exNum - 1.U
  val exInterruptd = RegInit(false.B)
  val ldMisaligned = Wire(Bool())
  val stMisaligned = Wire(Bool())
  val exInstsTrueValid = Wire(Vec(4, Bool()))
  val aluExptMask      = (exInstsValid(1) && mdu.io.resp.except && exInstsOrder(1) < exInstsOrder(0) ||
                          exInstsValid(2) && ldMisaligned && exInstsOrder(2) < exInstsOrder(0) ||
                          exInstsValid(3) && stMisaligned && exInstsOrder(3) < exInstsOrder(0))
  val mduExptMask      = (exInstsValid(0) && alu.io.ovf && exInstsOrder(0) < exInstsOrder(1) ||
                          exInstsValid(2) && ldMisaligned && exInstsOrder(2) < exInstsOrder(1) ||
                          exInstsValid(3) && stMisaligned && exInstsOrder(3) < exInstsOrder(1))
  val ldExptMask       = (exInstsValid(0) && alu.io.ovf && exInstsOrder(0) < exInstsOrder(2) ||
                          exInstsValid(1) && mdu.io.resp.except && exInstsOrder(1) < exInstsOrder(2))
  val stExptMask       = (exInstsValid(0) && alu.io.ovf && exInstsOrder(0) < exInstsOrder(3) ||
                          exInstsValid(1) && mdu.io.resp.except && exInstsOrder(1) < exInstsOrder(3))  // ld st cannot be simultaneously issued

  // WB
  val wfds             = RegInit(false.B) // wait for delay slot, this name is from RV's WFI instruction
  val reBranchPC       = RegInit(startAddr.U(len.W))
  val hi               = RegInit(0.U(len.W))
  val lo               = RegInit(0.U(len.W))
  val wbResult         = RegInit(VecInit(Seq.fill(4)(0.U(len.W))))
  val wbInstsValid     = Reg(Vec(4, Bool()))
  val wbInstsOrder     = RegNext(exInstsOrder)
  val wbInsts          = RegInit(VecInit(Seq.fill(4)(nop)))
  val kill_w           = io.fb.bmfs.redirect_kill
  val bubble_w         = stall_x
  val regFile          = Module(new RegFile(nread = 8, nwrite = 3)) // 8 read port, 3 write port
  val cp0              = Module(new CP0)
  val wbData           = Wire(Vec(3, UInt(len.W)))
  val wbReBranch       = RegInit(false.B)
  val latestBJPC       = RegInit(startAddr.U) // cannot deal with the second is Exception and the first is non-bj
  val wbExcepts        = Wire(Vec(4, Bool()))
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
  issueInsts(0)         := issueQueue.io.dout(0)
  issueInsts(1)         := issueQueue.io.dout(1)
  issueInsts(2)         := issueQueue.io.dout(2)
  for(i <- 0 until frontendIssueN) {
    issueQueue.io.din(i) := io.fb.fmbs.inst_ops(i).asTypeOf(new Mops)
  }

  val trap_ret_items1 = Mux(issueInsts(1).next_pc(2).andR || issueInsts(1).illegal,  // break syscall eret
    Mux(issueQueue.io.items >= 2.U, 2.U, issueQueue.io.items),
    issueQueue.io.items
  )
  val trap_ret_items0 = Mux(issueQueue.io.items >= 1.U, 1.U, issueQueue.io.items)
  issueArbiter.io.queue_items := Mux(issueInsts(0).next_pc(2).andR || issueInsts(0).illegal, trap_ret_items0,
    Mux(issueInsts(0).next_pc =/= MicroOpCtrl.PC4, 
      Mux(issueQueue.io.items >= 3.U, 2.U, issueQueue.io.items), 
      trap_ret_items1
    )  
  )
  issueArbiter.io.ld_dest_ex  := Fill(32, exInstsValid(2)) & exInsts(2).rd
  issueArbiter.io.mtc0_ex     := exInstsValid(0) & exInsts(0).write_dest === MicroOpCtrl.DCP0
  issueArbiter.io.insts_in    := issueInsts
  issueNum                    := issueArbiter.io.issue_num

  // load to something fwd is canceled, just stall
  for(i <- 0 until 4) {
    isRsFwd(i) := false.B
    isRtFwd(i) := false.B
    rsFwdIndex(i) := 0.U
    rtFwdIndex(i) := 0.U
  }
  for(i <- 0 until 2) {
    when(exInstsValid(i) && exInsts(i).write_dest === MicroOpCtrl.DReg && exInsts(i).rd =/= 0.U) {
      for(j <- 0 until 4) {
        when(exInsts(i).rd === issueArbiter.io.insts_out(j).rs1) {
          isRsFwd(j) := true.B
          rsFwdIndex(j) := i.U
        }
        when(exInsts(i).rd === issueArbiter.io.insts_out(j).rs2) {
          isRtFwd(j) := true.B
          rtFwdIndex(j) := i.U
        }
      }
    }
  }

  /**
   *  [---------- EX stage -----------]
   */ 
    /*
    exInsts(0) -> ALU
    exInsts(1) -> MDU
    exInsts(2) -> LU
    exInsts(3) -> SU
   */
  // when call for int, only alu will be valid no matter which fu will be valid normally,
  // so exInstsValid(0) := issueArbiter.io.issue_fu_valid.asUInt.orR
  // exInsts.pc must be the pc with the smallest order
  // so nopPC.pc := issueInsts(0).pc
  exNum := Mux(stall_x, exNum, Mux(kill_x, 0.U, issueNum))
  when (kill_x) {
    exInterruptd := false.B
    for(i <- 0 until 4) {
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
      for (i <- 1 until 4) {
        exInstsValid(i) := false.B
      }
    }.otherwise {
      exInstsValid := issueArbiter.io.issue_fu_valid
      exInsts(0)   := issueArbiter.io.insts_out(0)
    }
    for (i <- 1 until 4) {
      exInsts(i) := issueArbiter.io.insts_out(i)
    }
    exInterruptd := cp0.io.except.call_for_int
  }

  for (i <- 0 until 4) {
    exIsRsFwd(i) := isRsFwd(i) && !stall_x
    exIsRtFwd(i) := isRtFwd(i) && !stall_x
  }
  exRsFwdIndex := rsFwdIndex
  exRtFwdIndex := rtFwdIndex

  /*
    read port
    rs_1
    rt_1
    rs_2
    rt_2
    rs_3
    rt_3
    rs_4
    rs_4
   */
  for(i <- 0 until 4) {
    regFile.io.rs_addr_vec(2 * i) := exInsts(i).rs1
    regFile.io.rs_addr_vec(2 * i + 1) := exInsts(i).rs2
  }

  // forward all the data here
  // assume I-type Inst replace rt with rd, and update rt = 0
  for(i <- 0 until 4) {
    rsData(i) := regFile.io.rs_data_vec(2 * i)
    rtData(i) := regFile.io.rs_data_vec(2 * i + 1)
  }

  for(i <- 0 until 4) {
    fwdRsData(i) := Mux(exIsRsFwd(i), wbData(exRsFwdIndex(i)), rsData(i))
    fwdRtData(i) := Mux(exIsRtFwd(i), wbData(exRtFwdIndex(i)), rtData(i))
  }

  // if waiting for slot, must be the smallsest one
  aluValid   := exInstsValid(0) && !kill_x && (!wfds || exInstsOrder(0) === 0.U)
  mduValid   := exInstsValid(1) && !kill_x && (!wfds || exInstsOrder(1) === 0.U)
  loadValid  := exInstsValid(2) && !kill_x && (!wfds || exInstsOrder(2) === 0.U)
  storeValid := exInstsValid(3) && !kill_x && (!wfds || exInstsOrder(3) === 0.U)

  // alu execution
  alu.io.a := MuxLookup(exInsts(0).src_a, fwdRsData(0),
    Seq(
      MicroOpCtrl.AShamt -> exInsts(0).imm(10, 6),
      MicroOpCtrl.AHi    -> hi,
      MicroOpCtrl.ALo    -> lo,
      MicroOpCtrl.ACP0   -> cp0.io.ftc.dout
    )
  )
  alu.io.b := MuxLookup(exInsts(0).src_b, fwdRtData(0),
    Seq(
      MicroOpCtrl.BImm -> exInsts(0).imm
    )
  )
  alu.io.aluOp := exInsts(0).alu_op

  // mdu execution
  mdu.io.req.valid := mduValid
  mdu.io.req.in1 := MuxLookup(exInsts(1).src_a, fwdRsData(1),
    Seq(
      MicroOpCtrl.AShamt -> exInsts(1).imm(10, 6),
      MicroOpCtrl.AHi    -> hi,
      MicroOpCtrl.ALo    -> lo
    )
  )
  mdu.io.req.in2 := MuxLookup(exInsts(1).src_b, fwdRtData(1),
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
  ldMisaligned := MuxLookup(
    exInsts(2).mem_width, false.B,
    Seq(
      MicroOpCtrl.MemHalf  -> loadAddr(0).asBool,
      MicroOpCtrl.MemHalfU -> loadAddr(0).asBool,
      MicroOpCtrl.MemWord  -> loadAddr(1, 0).orR
    )
  )
  stMisaligned := MuxLookup(
    exInsts(3).mem_width, false.B,
    Seq(
      MicroOpCtrl.MemHalf  -> storeAddr(0).asBool,
      MicroOpCtrl.MemWord  -> storeAddr(1, 0).orR
    )
  )
  io.dcache.req.bits.flush := false.B
  io.dcache.req.bits.invalidate := false.B
  io.dcache.req.valid := (loadValid && !ldMisaligned) || (exInstsTrueValid(3) && !stMisaligned)
  io.dcache.req.bits.mtype := Mux(
    loadValid,
    exInsts(2).mem_width,
    exInsts(3).mem_width
  )
  io.dcache.req.bits.wen := storeValid
  io.dcache.req.bits.wdata := fwdRtData(3)
  io.dcache.req.bits.addr := Mux(exInstsValid(2), loadAddr, storeAddr)
  io.dcache.resp.ready := true.B

  // jump br
  val isExPCJump = exInsts(0).next_pc === MicroOpCtrl.PCReg || exInsts(0).next_pc === MicroOpCtrl.Jump
  val isExPCBr   = exInsts(0).next_pc === MicroOpCtrl.Branch
  reBranch := false.B
  jumpPc := Mux(
    exInsts(0).next_pc === MicroOpCtrl.Jump, 
    Cat(exInsts(0).pc(31, 28), exInsts(0).imm(25, 0), 0.U(2.W)),
    fwdRsData(0)
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
  exInstsTrueValid(2) := loadValid && !ldExptMask
  exInstsTrueValid(3) := storeValid && !stExptMask

  /**
   *  [---------- WB stage -----------]
   */
  wbInsts := exInsts

  wbResult(0) := Mux(exInsts(0).write_src === MicroOpCtrl.WBPC,
    exInsts(0).pc + 8.U,
    Mux(exInsts(0).next_pc === MicroOpCtrl.PC4, alu.io.r, exInsts(0).pc + 8.U)
  )
  wbResult(1) := mdu.io.resp.lo

  // handle load-inst separately
  val delayed_req_byte = RegNext(io.dcache.req.bits.addr(1, 0))
  val dataFromDcache = io.dcache.resp.bits.rdata(0) >> (delayed_req_byte << 3.U)
//  val luData = WireDefault(dataFromDcache)
  val luData = Wire(UInt(32.W))
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
    for(i <- 0 until 4) {
      wbInstsValid(i) := false.B
    }
    wbReBranch := false.B
  }.elsewhen (bubble_w) {
    for(i <- 0 until 4) {
      wbInstsValid(i) := false.B
    }
  }.otherwise {
    latestBJPC := Mux(exInstsTrueValid(0) && (isExPCBr || isExPCJump), 
      exInsts(0).pc, latestBJPC
    )
    for (i <- 0 until 4) {
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
  for(i <- 0 until 3) {
    regFile.io.wen_vec(i) := wbInstsValid(i) && wbInsts(i).write_dest === MicroOpCtrl.DReg && !wbExcepts(i)
    regFile.io.rd_addr_vec(i) := wbInsts(i).rd
    regFile.io.rd_data_vec(i) := wbData(i)
  }

  // cp0
  val wbALUOvfReal  = wbInstsValid(0) && RegNext(alu.io.ovf)
  val wbMDUOvfReal  = wbInstsValid(1) && RegNext(mdu.io.resp.except)
  val wbLdMaReal    = wbInstsValid(2) && RegNext(ldMisaligned)
  val wbStMaReal    = wbInstsValid(3) && RegNext(stMisaligned)
  val wbALUSysReal  = wbInsts(0).next_pc === MicroOpCtrl.Trap && wbInstsValid(0)
  val wbALUBpReal   = wbInsts(0).next_pc === MicroOpCtrl.Break && wbInstsValid(0)
  val illegal       = wbInsts(0).illegal && wbInstsValid(0) // put all in illegal, but inst misaligned is of high prio
  val wbFetchMaReal = wbInsts(0).pc(1, 0).orR && wbInstsValid(0)  // will be in mops.illegal = 1
  val respInt       = wbInterruptd && wbInstsValid(0)
  wbExcepts(0) := wbALUOvfReal
  wbExcepts(1) := wbMDUOvfReal
  wbExcepts(2) := wbLdMaReal
  wbExcepts(3) := wbStMaReal
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
                                  Mux(wbMDUOvfReal, wbInsts(1).pc,
                                    Mux(wbLdMaReal, wbInsts(2).pc, wbInsts(3).pc))
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
    val debug_pc   = Wire(Vec(backendIssueN, UInt(len.W)))
    val debug_wen  = Wire(Vec(backendIssueN, Bool()))
    val debug_data = Wire(Vec(backendIssueN, UInt(len.W)))
    val debug_nreg = Wire(Vec(backendIssueN, UInt(5.W)))
    for(i <- 0 until 3) {
      debug_pc(i) := 0.U
      debug_wen(i) := false.B
      debug_data(i) := 0.U
      debug_nreg(i) := 0.U
    }
    for(i <- 0 until 3) {
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

    BoringUtils.addSource(debug_pc,   "dt_pc"    )
    BoringUtils.addSource(debug_wen,  "dt_wen"   )
    BoringUtils.addSource(debug_data, "dt_data"  )
    BoringUtils.addSource(debug_nreg, "dt_nreg"  )
  }
}
package icore

import chisel3._
import chisel3.util._
import conf.Config
import fu._
import isa._
import chisel3.experimental.BundleLiterals._
import chisel3.util.experimental.BoringUtils

// toALU toMDU toLSU toBJU
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

// TODO ALU_LSU ALU_MDU LSU_BJU
class Backend(diffTestV: Boolean, verilator: Boolean) extends Module with Config with InstType with MemAccessType with CauseExcCode with RefType with TLBOpType {
  val io = IO(new BackendIO)

  // Global
  val nop = {
    val tmp = Wire(new Mops)
    tmp.illegal := false.B
    tmp.rs1 := 0.U
    tmp.rs2 := 0.U
    tmp.rd := 0.U
    tmp.alu_op := 1.U  // addu will never cause except
    tmp.imm := 0.U
    tmp.alu_mdu_lsu := toALU.U
    tmp.write_dest := MicroOpCtrl.DXXX
    tmp.next_pc := MicroOpCtrl.PC4
    tmp.src_a := MicroOpCtrl.AXXX
    tmp.src_b := MicroOpCtrl.BXXX
    tmp.branch_type := MicroOpCtrl.BrXXX
    tmp.mem_width := MicroOpCtrl.MemXXX
    tmp.write_src := MicroOpCtrl.WBALU
    tmp.pc := 0.U
    tmp.predict_taken := false.B
    tmp.target_pc := 0.U
    if (withBigCore) {
      tmp.atomic := false.B
      tmp.tlb_exp := 0.U.asTypeOf(new TLBExceptIO)
      tmp.tlb_op := 0.U
      tmp.sel := 0.U
    }
    tmp
  }
  val alu          = Module(new ALU)
  val mdu          = Module(new MDU)

  // Issue
  val dcacheStall  = Wire(Bool())
  val issueNum     = Wire(UInt(2.W))
  val issueInsts   = Wire(Vec(backendIssueN, new Mops))
  val issueRss     = Wire(Vec(backendFuN, UInt(len.W)))
  val issueRts     = Wire(Vec(backendFuN, UInt(len.W)))
  val issueQueue   = Module(new FIFO(queueSize, new Mops(), backendIssueN, frontendIssueN))
  val issueArbiter = Module(new IssueArbiter(queueSize))
  val stall_i      = Wire(Bool())
  val kill_i       = io.fb.bmfs.redirect_kill
  val rsFwdData    = Wire(Vec(backendIssueN, UInt(len.W)))
  val rtFwdData    = Wire(Vec(backendIssueN, UInt(len.W)))
  val rsData       = Wire(Vec(backendIssueN, UInt(len.W)))
  val rtData       = Wire(Vec(backendIssueN, UInt(len.W)))

  // Ex
  val stall_x      = stall_i
  val kill_x       = !dcacheStall && kill_i
  val exNum        = RegInit(0.U(2.W))
  val exInsts      = RegInit(VecInit(Seq.fill(backendFuN)(nop)))
  val exInstsOrder = if(diffTestV) RegInit(VecInit(Seq.fill(backendFuN)(0.U(2.W)))) else Reg(Vec(backendFuN, UInt(2.W)))
  val exInstsValid = RegInit(VecInit(Seq.fill(backendFuN)(false.B)))
  val exFwdRsData  = Reg(Vec(backendFuN, UInt(len.W)))
  val exFwdRtData  = Reg(Vec(backendFuN, UInt(len.W)))
  val isExPCBr     = exInsts(0).next_pc === MicroOpCtrl.Branch
  val isExPCJump   = exInsts(0).next_pc === MicroOpCtrl.PCReg || exInsts(0).next_pc === MicroOpCtrl.Jump
  val reBranch     = Wire(Bool())
  val reBranchBrTaken = Wire(Bool())
  val jumpPc       = Wire(UInt(len.W))
  val brPC         = exInsts(0).pc + (exInsts(0).imm << 2.U)(len - 1, 0) + 4.U
  val exReBranchPC = Mux(isExPCBr, Mux(reBranchBrTaken, brPC, exInsts(0).pc + 8.U), jumpPc)
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
  val exIsTlbAddrFind = if (withBigCore) io.tlbAddrTransl.exp.expType === TLBExceptType.noExp else true.B
  val aluExptMask      = (exInstsValid(1) && mdu.io.resp.except && exInstsOrder(1) < exInstsOrder(0) ||
                          exInstsValid(2) && (memMisaligned || !exIsTlbAddrFind) && exInstsOrder(2) < exInstsOrder(0))
  val mduExptMask      = (exInstsValid(0) && alu.io.ovf && exInstsOrder(0) < exInstsOrder(1) ||
                          exInstsValid(2) && (memMisaligned || !exIsTlbAddrFind) && exInstsOrder(2) < exInstsOrder(1))
  val ldstExptMask     = (exInstsValid(0) && alu.io.ovf && exInstsOrder(0) < exInstsOrder(2) ||
                          exInstsValid(1) && mdu.io.resp.except && exInstsOrder(1) < exInstsOrder(2))
  val exMemRealValid = exInstsTrueValid(2) && !memMisaligned && exIsTlbAddrFind
  val bpuV      = (isExPCBr || isExPCJump) && exInstsValid(0)
  val bpuErrpr  = isExPCBr && exInsts(0).target_pc(len - 1, 2) =/= brPC(len - 1, 2) && reBranchBrTaken || isExPCJump && exInsts(0).target_pc(len - 1, 2) =/= jumpPc(len - 1, 2)
  val bpuPCBr   = exInsts(0).pc
  val bpuTarget = Mux(isExPCBr, brPC, jumpPc)
  val bpuTaken  = reBranchBrTaken || isExPCJump
  val exMoveCondNo = ((exInsts(0).branch_type === MicroOpCtrl.BrEQ).asUInt ^ (exFwdRtData(0) === 0.U).asUInt).andR
  val linkBaseAddrHi = if (withBigCore) Reg(UInt((len - 2).W)) else null
  val linkValid = if (withBigCore) RegInit(false.B) else null
  val exStoreCondSuccess = if (withBigCore) (linkValid && linkBaseAddrHi === ldstAddr(len - 1, 2))  else null  // this is only a wire of data, need other control signals

  // WB
  val wfds             = RegInit(false.B) // wait for delay slot, this name is from RV's WFI instruction
  val reBranchPC       = Reg(UInt(len.W))
  val hi               = RegInit(0.U(len.W))
  val lo               = RegInit(0.U(len.W))
  val wbResult         = Reg(Vec(backendFuN, UInt(len.W)))
  val wbInstsValid     = RegInit(VecInit(Seq.fill(backendFuN)(false.B)))
  val wbInstsOrder     = Reg(Vec(backendFuN, UInt(2.W)))
  val wbInsts          = RegInit(VecInit(Seq.fill(backendFuN)(nop)))
  val kill_w           = !dcacheStall && io.fb.bmfs.redirect_kill
  val bubble_w         = stall_x
  val regFile          = Module(new RegFile(nread = 2 * backendIssueN, nwrite = backendIssueN, verilator = verilator)) // 4 read port, 2 write port
  val cp0              = Module(new CP0(diffTestV))
  val wbData           = Wire(Vec(backendFuN, UInt(len.W)))
  val wbReBranch       = RegInit(false.B)
  val latestBJPC       = RegInit(startAddr.U) // cannot deal with the second is Exception and the first is non-bj
  val wbExcepts        = Wire(Vec(backendFuN, Bool()))
  val wbMisalignedAddr = RegNext(io.dcache.req.bits.addr)
  val wbInterruptd     = RegNext(exInterruptd && !io.fb.bmfs.redirect_kill)
  val wbALUOvf         = RegInit(false.B)
  val wbMDUOvf         = RegInit(false.B)
  val wbLdMa           = RegInit(false.B)
  val wbStMa           = RegInit(false.B)
  val wbBpuV           = RegInit(false.B)
  val wbBpuErrpr       = RegNext(bpuErrpr)
  val wbBpuPCBr        = RegNext(Cat(bpuPCBr(len - 1, 2), exInsts(0).target_pc(1, 0)))
  val wbBpuTarget      = RegNext(bpuTarget)
  val wbBpuTaken       = RegNext(bpuTaken)
  val wbStCondSuccess  = if (withBigCore) RegInit(false.B) else null
  val wbTlbOut         = if (withBigCore) Reg(new TLBEntryIO) else null

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

  def isNextPCMayTrap(npc_op: UInt): Bool = {
    npc_op(2)
  }

  // TODO lr sc, trap, only 1, never 2?
  val trap_ret_items0 = Mux(issueQueue.io.items >= 1.U, 1.U, 0.U)   // break syscall eret tne
  val trapBlockSecondItem = isNextPCMayTrap(issueInsts(0).next_pc) || issueInsts(0).illegal
  val blockSecondItem = if (withBigCore) (trapBlockSecondItem || issueInsts(0).atomic) else trapBlockSecondItem
  if (withBigCore) {  // redirect mtc0 // TODO more redirect, such as cache, sync...
    val redirectFlushBlockSecondItem = issueInsts(0).write_dest === MicroOpCtrl.DCP0
    val redirectFlushBlockSecondItemFinal = if (verilator) (issueInsts(0).src_a === MicroOpCtrl.ACP0 || redirectFlushBlockSecondItem) else redirectFlushBlockSecondItem
    issueArbiter.io.queue_items := Mux(blockSecondItem || redirectFlushBlockSecondItemFinal, trap_ret_items0, issueQueue.io.items)
  } else {
    issueArbiter.io.queue_items := Mux(blockSecondItem, trap_ret_items0, issueQueue.io.items)
  }
  if (withBigCore) {
    issueArbiter.io.mtc0_ex   := exInstsValid(0) && (exInsts(0).write_dest === MicroOpCtrl.DCP0 || exInsts(0).tlb_op === tlbr.U || exInsts(0).tlb_op === tlbp.U)
  } else {
    issueArbiter.io.mtc0_ex   := exInstsValid(0) && exInsts(0).write_dest === MicroOpCtrl.DCP0
  }
  issueArbiter.io.ld_dest_ex  := Fill(len, exInstsValid(2)) & exInsts(2).rd // make sc and load stall one cycle
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
  //TODO: DEL!!!!!!!!!!!!!!!!!!!!!!!!
  // !!!!!!!!!!!!!!!!!!!!!!

  val exCondFwdEnable = if (withBigCore) (exInsts(0).write_dest === MicroOpCtrl.DRegCond && !exMoveCondNo) else false.B
  val movn_info = false
  if(movn_info){
    when(exInsts(0).write_dest === MicroOpCtrl.DRegCond){
      printf("mov/movz, rs, rt = (%x,%x), %d, pc=%x, redirect kill = %d, wfds %d\n",exFwdRsData(0),exFwdRtData(0),exCondFwdEnable,exInsts(0).pc,io.fb.bmfs.redirect_kill,wfds)
    }
  }
  when(exInstsValid(0) && (exInsts(0).write_dest === MicroOpCtrl.DReg || exCondFwdEnable) && exInsts(0).rd =/= 0.U) {
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
      MicroOpCtrl.BImm -> exInsts(0).imm,
      MicroOpCtrl.BZero -> 0.U
    )
  )
  alu.io.rega  := exFwdRsData(0)
  alu.io.aluOp := exInsts(0).alu_op

  // mdu execution
  // one inst only issues one req
  // mul/div may issues req continuously when there is no dcache stall
  mdu.io.req.valid := mduValid && !RegNext(dcacheStall)
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
  val newHiSeqBase = Seq(
    MicroOpCtrl.DHi -> mdu.io.resp.lo,
    MicroOpCtrl.DHiLo -> mdu.io.resp.hi
  )
  val newHiSeqExt = Seq(
    MicroOpCtrl.DHiLoAdd -> (hi + mdu.io.resp.hi)
  )
  val newHiSeqFinal = if (withBigCore) (newHiSeqBase ++ newHiSeqExt) else newHiSeqBase
  hi := Mux(!bubble_w && exInstsTrueValid(1),
    MuxLookup(exInsts(1).write_dest, hi, newHiSeqFinal),
    hi
  )
  val newLoSeqBase = Seq(
    MicroOpCtrl.DLo -> mdu.io.resp.lo,
    MicroOpCtrl.DHiLo -> mdu.io.resp.lo
  )
  val newLoSeqExt = Seq(
    MicroOpCtrl.DHiLoAdd -> (lo + mdu.io.resp.lo)
  )
  val newLoSeqFinal = if (withBigCore) (newLoSeqBase ++ newLoSeqExt) else newLoSeqBase
  lo := Mux(!bubble_w && exInstsTrueValid(1),
    MuxLookup(exInsts(1).write_dest, lo, newLoSeqFinal),
    lo
  )

  if (withBigCore) {
    // tlb addr translate
    io.tlbAddrTransl.virt_addr := ldstAddr
    io.tlbAddrTransl.refType := Mux(exInsts(2).write_dest === MicroOpCtrl.DMem, store.U, load.U)

    // tlb exec
    io.tlbExec.op := exInsts(0).tlb_op
    io.tlbExec.din := cp0.io.ftTlb.exec.dout
  }

  // initialize lsu
  val exLastMemReqValid = RegInit(false.B)
  dcacheStall := exLastMemReqValid && !io.dcache.resp.valid

  ldMisaligned := exInsts(2).write_dest =/= MicroOpCtrl.DMem && memMisaligned
  stMisaligned := exInsts(2).write_dest === MicroOpCtrl.DMem && memMisaligned
  val mmioMask = io.dcache.req.bits.wen && io.dcache.req.bits.addr(28, 0) === "h1faffff0".U
  val dcacheMask = if (simpleNBDCache) mmioMask else false.B
  io.dcache.req.valid := exMemRealValid && !dcacheMask || dcacheStall
  io.dcache.resp.ready := true.B

  if (withBigCore) {
    // load linked - store conditional
    val dcacheNewReqBeforeStoreCondMask = exMemRealValid && !dcacheStall
    when (dcacheNewReqBeforeStoreCondMask && !io.dcache.req.bits.wen && exInsts(2).atomic) {
      // load linked
      linkValid := true.B
      linkBaseAddrHi := ldstAddr(len - 1, 2)
    }.elsewhen (!bubble_w && dcacheNewReqBeforeStoreCondMask && io.dcache.req.bits.wen && (linkBaseAddrHi === ldstAddr(len - 1, 2) || exInsts(2).atomic) || cp0.io.except.except_kill) {
      // cp0 redirect, store on the overlapped address, store conditional succeeds or not
      linkValid := false.B
    }
    val dcache_info = false
    if(dcache_info){
      val reg_last = Reg(UInt(len.W))
      val pc = Mux(dcacheStall, reg_last, exInsts(2).pc)
      reg_last := pc
      when(io.dcache.resp.valid){

        printf("dcache access in  addr %x, wen %d, pc= %x\n",RegNext(io.dcache.req.bits.addr),RegNext(io.dcache.req.bits.wen),reg_last)
        when(RegNext(io.dcache.req.bits.wen)){
          printf("writing %x\n", RegNext(io.dcache.req.bits.wdata))
        }.otherwise{
          printf("reading %x\n", io.dcache.resp.bits.rdata(0))
        }
      }

    }
  }

  val exLastMemReq = RegInit({
    val memReq = Wire(new MemReq)
    memReq.flush := false.B
    memReq.invalidate := false.B
    memReq.mtype := MicroOpCtrl.MemWord
    memReq.wen := false.B
    memReq.wdata := 0.U
    memReq.addr := 0.U
    if(withBigCore){
      memReq.swlr := 0.U
    }
    memReq
  })
  if (withBigCore) {
    io.tlbAddrTransl.virt_addr := ldstAddr
    io.tlbAddrTransl.refType := Mux(exInsts(2).write_dest === MicroOpCtrl.DMem, store.U, load.U)
  }

  val exCurMemReq = {
    val memReq = Wire(new MemReq)
    memReq.flush := false.B
    memReq.invalidate := false.B
    memReq.mtype := exInsts(2).mem_width
    memReq.wdata := exFwdRtData(2)
    if(withBigCore){
      memReq.swlr := Mux(exInsts(2).mem_width === MicroOpCtrl.MemWordL, 1.U, Mux(exInsts(2).mem_width === MicroOpCtrl.MemWordR, 2.U, 0.U))
      memReq.addr := io.tlbAddrTransl.phys_addr
      memReq.wen := exInsts(2).write_dest === MicroOpCtrl.DMem && (!exInsts(2).atomic || exStoreCondSuccess)
    } else {
      memReq.wen := exInsts(2).write_dest === MicroOpCtrl.DMem
      memReq.addr := ldstAddr
    }
    memReq
  }

  when (!dcacheStall) {
    exLastMemReqValid := exMemRealValid && !dcacheMask
    exLastMemReq := exCurMemReq
  }

  // 2 to 1
  io.dcache.req.bits := Mux(dcacheStall, exLastMemReq, exCurMemReq)
  io.dcache.req.bits.flush := false.B
  io.dcache.req.bits.invalidate := false.B

  stall_i := dcacheStall || !mdu.io.resp.valid


  // jump br
  reBranch := false.B
  reBranchBrTaken := false.B
  jumpPc := Mux(
    exInsts(0).next_pc === MicroOpCtrl.Jump,
    Cat(exInsts(0).pc(31, 28), exInsts(0).imm(25, 0), 0.U(2.W)),
    exFwdRsData(0)
  )
  when(exInstsTrueValid(0)) {
    when (isExPCBr) {
      reBranchBrTaken := MuxLookup(exInsts(0).branch_type, alu.io.zero === 1.U, // default beq
        Seq(
          MicroOpCtrl.BrNE -> (alu.io.zero === 0.U),
          MicroOpCtrl.BrGE -> (alu.io.a.asSInt >= 0.S),
          MicroOpCtrl.BrGT -> (alu.io.a.asSInt > 0.S),
          MicroOpCtrl.BrLE -> (alu.io.a.asSInt <= 0.S),
          MicroOpCtrl.BrLT -> (alu.io.a.asSInt < 0.S)
        )
      )
      reBranch := (reBranchBrTaken ^ exInsts(0).predict_taken) || (reBranchBrTaken && (exInsts(0).target_pc(len - 1, 2)  =/= brPC(len - 1, 2) || brPC(1, 0).orR))

      if (traceBPU) {
        when (!stall_x && !kill_x) {
          when (reBranch) {
            printf("misprediction at %x, wrong target %x\n", exInsts(0).pc, exInsts(0).predict_taken && reBranchBrTaken && exInsts(0).target_pc(len - 1, 2) =/= brPC(len - 1, 2))
          }.otherwise {
            printf("hit at %x\n", exInsts(0).pc)
          }
        }
      }

    }.elsewhen (isExPCJump) {
      reBranch := exInsts(0).target_pc(len - 1, 2) =/= jumpPc(len - 1, 2) || !exInsts(0).predict_taken || jumpPc(1, 0).orR

      if (traceBPU) {
        val indirect_cnt = RegInit(0.U(len.W))
        when (!stall_x && !kill_x) {
          when (reBranch) {
            indirect_cnt := indirect_cnt + Mux(exInsts(0).next_pc === MicroOpCtrl.PCReg, 1.U, 0.U)
            printf("jump at %x, indirect %x, target %x, wrong target %x, not taken %x, indirect_count %d\n", exInsts(0).pc, exInsts(0).next_pc === MicroOpCtrl.PCReg, jumpPc, exInsts(0).target_pc(len - 1, 2) =/= jumpPc(len - 1, 2), !exInsts(0).predict_taken, indirect_cnt)
          }.otherwise {
            printf("hit jump at %x\n", exInsts(0).pc)
          }
        }
      }

      if (traceCallRet) {
        when (!stall_x && !kill_x) {
          when (exInsts(0).next_pc === MicroOpCtrl.Jump && exInsts(0).write_dest === MicroOpCtrl.DReg) {
            printf("call at %x, target %x, hit %x\n", exInsts(0).pc, jumpPc, !reBranch)
          }.elsewhen (exInsts(0).next_pc === MicroOpCtrl.PCReg && exInsts(0).write_dest === MicroOpCtrl.DXXX) {
            printf("return at %x, target %x, hit %x\n", exInsts(0).pc, jumpPc, !reBranch)
          }
        }
      }
    }
  }

  // not valid if exception before the inst
  exInstsTrueValid(0) := aluValid && !aluExptMask
  exInstsTrueValid(1) := mduValid && !mduExptMask
  exInstsTrueValid(2) := ldstValid && !ldstExptMask

  /**
   *  [---------- WB stage -----------]
   */
  aluWbData := Mux(exInsts(0).write_src === MicroOpCtrl.WBPC || exInsts(0).next_pc =/= MicroOpCtrl.PC4,
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


  // handle load-inst separately
  val delayed_req_bits = RegNext(io.dcache.req.bits.addr(1, 0) << 3.U)
  val dataFromDcache = io.dcache.resp.bits.rdata(0) >> delayed_req_bits
  val wbLdData = Wire(UInt(len.W))

  // to cope with the situation where load after mult happens,
  // but cache only supplies the load data for only one cycle.
  // Therefore, core should save the data by itself until the mdu stall ends,
  // otherwise, load data will be lost since wbInst is valid when bubble
  val wbLdDataForStall = Reg(UInt(len.W))
  wbLdDataForStall := Mux(!mdu.io.resp.valid && RegNext(mdu.io.resp.valid), wbLdData, wbLdDataForStall)
  val wbLdDataValid = RegNext(mdu.io.resp.valid)

  wbLdData := dataFromDcache
  switch(wbInsts(2).mem_width) {
    is(MicroOpCtrl.MemByte)  { wbLdData := Cat(Fill(24, dataFromDcache(7)), dataFromDcache(7, 0)) }
    is(MicroOpCtrl.MemByteU) { wbLdData := Cat(Fill(24, 0.U), dataFromDcache(7, 0)) }
    is(MicroOpCtrl.MemHalf)  { wbLdData := Cat(Fill(16, dataFromDcache(15)), dataFromDcache(15, 0)) }
    is(MicroOpCtrl.MemHalfU) { wbLdData := Cat(Fill(16, 0.U), dataFromDcache(15, 0)) }
  }
  if(withBigCore){
    def _ex2wb(c: UInt):UInt = {     // remembers the cache request
      val last = Reg(UInt(c.getWidth.W))
      last := Mux(dcacheStall, last, c)
      last
    }
    when(wbInsts(2).atomic && _ex2wb(exInsts(2).write_dest === MicroOpCtrl.DMem).asBool()) { // is sc not load or ll
      wbLdData := Mux(_ex2wb(exStoreCondSuccess).asBool(), 1.U, 0.U)
    }
    val lwl_mask = Wire(UInt(32.W))
    val lwr_mask = Wire(UInt(32.W))
    lwl_mask := "h00ffffff".U >> delayed_req_bits
    lwr_mask := ~("hffffffff".U >> delayed_req_bits)
    val shamt = delayed_req_bits
    val last = _ex2wb(exFwdRtData(2))
    switch(wbInsts(2).mem_width) {
      is(MicroOpCtrl.MemWordL) {
        wbLdData := ((io.dcache.resp.bits.rdata(0)<<(24.U-shamt)) & ~lwl_mask)|(last & lwl_mask)
      }
      is(MicroOpCtrl.MemWordR) {
        wbLdData := ((io.dcache.resp.bits.rdata(0)>>shamt) & ~lwr_mask)|(last & lwr_mask)
      }
    }
  }
  wbData(0) := wbResult(0)
  wbData(1) := wbResult(1)
  wbData(2) := Mux(wbLdDataValid, wbLdData, wbLdDataForStall)

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
    wbBpuV := false.B
  }.elsewhen (!bubble_w) {
    wbBpuV := bpuV
    latestBJPC := Mux(exInstsTrueValid(0) && (isExPCBr || isExPCJump),
      exInsts(0).pc, latestBJPC
    )
    for (i <- 0 until backendFuN) {
      wbInstsValid(i) := exInstsTrueValid(i)
    }
    wbInsts := exInsts
    wbInstsOrder := exInstsOrder
    wbResult(0) := aluWbData
    wbResult(1) := mdu.io.resp.lo

    if (withBigCore) {
      wbInsts(0).next_pc := Mux(alu.io.zero =/= 0.U && exInsts(0).next_pc === MicroOpCtrl.NETrap, MicroOpCtrl.PC4, exInsts(0).next_pc)
      wbInsts(0).write_dest := Mux(exInsts(0).write_dest === MicroOpCtrl.DRegCond,
        Mux(exMoveCondNo,
          MicroOpCtrl.DXXX, MicroOpCtrl.DReg
        ), exInsts(0).write_dest
      )   // MOVN and MOVZ instructions
      wbStCondSuccess := exStoreCondSuccess
      wbInsts(2).write_dest := Mux(exInsts(2).write_dest === MicroOpCtrl.DMem && exInsts(2).atomic, MicroOpCtrl.DReg, exInsts(2).write_dest)
    }
    wbALUOvf := alu.io.ovf
    wbMDUOvf := mdu.io.resp.except
    wbLdMa := ldMisaligned
    wbStMa := stMisaligned

    if (withBigCore) {
      wbInsts(2).tlb_exp := io.tlbAddrTransl.exp // tlb exception
      wbTlbOut := io.tlbExec.dout // tlb exec
    }

    when (!wfds) {
      reBranchPC := exReBranchPC
      wbReBranch := reBranch
    }
  }.otherwise {
    // TODO: do not write regFile until Stall ends
    wbBpuV := false.B
  }

  if (withBigCore) {
    val flushPipeLineRedirect = wbInstsValid(0) && (wbInsts(0).write_dest === MicroOpCtrl.DCP0 || false.B) // TODO add more, such as cache, priority exchange
     // to sync count for qemu
    val flushPipeLineRedirectFinal = if (verilator) flushPipeLineRedirect || (!dcacheStall && wbInstsValid(0) && wbInsts(0).src_a === MicroOpCtrl.ACP0) else flushPipeLineRedirect
    io.fb.bmfs.redirect_kill := (wbReBranch && !wfds) || cp0.io.except.except_kill || flushPipeLineRedirectFinal
    io.fb.bmfs.redirect_pc   := Mux(cp0.io.except.except_kill, cp0.io.except.except_redirect, Mux(wbReBranch && !wfds, reBranchPC, wbInsts(0).pc + 4.U))
    if(movn_info){
      when(RegNext(exCondFwdEnable)){
        printf("wb result: %x, redirect kill: %d, wfds %d\n",wbResult(0),io.fb.bmfs.redirect_kill,wfds)
        printf("flush final: %d, except kill: %d, stall_x %d\n",flushPipeLineRedirectFinal,cp0.io.except.except_kill,stall_x)
      }
    }
  } else {
    io.fb.bmfs.redirect_kill := (wbReBranch && !wfds) || cp0.io.except.except_kill
    io.fb.bmfs.redirect_pc   := Mux(cp0.io.except.except_kill, cp0.io.except.except_redirect, reBranchPC)
  }
  io.fb.bmfs.bpu.v           := wbBpuV
  io.fb.bmfs.bpu.errpr       := wbBpuErrpr
  io.fb.bmfs.bpu.pc_br       := wbBpuPCBr
  io.fb.bmfs.bpu.target      := wbBpuTarget
  io.fb.bmfs.bpu.taken       := wbBpuTaken

  // regfile
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
  regFile.io.rd_data_vec(1) := Mux(wbInstsValid(2), wbData(2), wbData(1))
  regFile.io.rd_addr_vec(1) := Mux(wbInstsValid(2), wbInsts(2).rd, wbInsts(1).rd)

  // cp0
  // TODO: cache stall should also influence the exception
  val wbALUOvfReal  = wbInstsValid(0) && wbALUOvf
  val wbMDUOvfReal  = wbInstsValid(1) && wbMDUOvf
  val wbLdMaReal    = wbInstsValid(2) && wbLdMa
  val wbStMaReal    = wbInstsValid(2) && wbStMa
  val wbALUTrapReal = if (withBigCore) wbInsts(0).next_pc === MicroOpCtrl.NETrap && wbInstsValid(0) else null
  val wbALUSysReal  = wbInsts(0).next_pc === MicroOpCtrl.Trap && wbInstsValid(0)
  val wbALUBpReal   = wbInsts(0).next_pc === MicroOpCtrl.Break && wbInstsValid(0)
  val illegal       = wbInsts(0).illegal && wbInstsValid(0) // put all in illegal, but inst misaligned is of high prio
  val wbFetchMaReal = wbInsts(0).pc(1, 0).orR && wbInstsValid(0)  // will be in mops.illegal = 1
  val wbTlblReal    = if (withBigCore) wbInsts(0).tlb_exp.expType === TLBExceptType.tlbl && wbInstsValid(0) ||
    wbInsts(2).tlb_exp.expType === TLBExceptType.tlbl && wbInstsValid(2) else false.B
  val wbTlbsReal    = if (withBigCore) wbInsts(0).tlb_exp.expType === TLBExceptType.tlbs && wbInstsValid(0) ||
    wbInsts(2).tlb_exp.expType === TLBExceptType.tlbs && wbInstsValid(2) else false.B
  val wbModReal     = if (withBigCore) wbInsts(2).tlb_exp.expType === TLBExceptType.mod && wbInstsValid(2) else false.B
  val wbTlbBadAddr  = if (withBigCore) Mux(wbInstsValid(2), wbInsts(2).tlb_exp.badVaddr, wbInsts(0).tlb_exp.badVaddr) else "hdeadbeef".U
  val respInt       = wbInterruptd && wbInstsValid(0)
  wbExcepts(0) := wbALUOvfReal
  wbExcepts(1) := wbMDUOvfReal
  wbExcepts(2) := wbLdMaReal || wbStMaReal
  val wb_ev = Wire(Vec(SZ_EXC_CODE, Bool()))

  wb_ev(Interrupt   ) := false.B
  wb_ev(TLBModify   ) := wbModReal
  wb_ev(TLBLoad     ) := wbTlblReal
  wb_ev(TLBStore    ) := wbTlbsReal
  wb_ev(AddrErrLoad ) := wbLdMaReal || wbFetchMaReal
  wb_ev(AddrErrStore) := wbStMaReal
  wb_ev(Syscall     ) := wbALUSysReal
  wb_ev(Breakpoint  ) := wbALUBpReal
  wb_ev(ReservedInst) := illegal
  wb_ev(Overflow    ) := wbALUOvfReal || wbMDUOvfReal
  if (withBigCore) {
    wb_ev(Trap        ) := wbALUTrapReal
  } else {
    wb_ev(Trap        ) := false.B
  }
  wb_ev(Res0        ) := false.B
  wb_ev(Res1        ) := false.B
  wb_ev(Res2        ) := false.B
  cp0.io.except.except_vec    := wb_ev
  cp0.io.except.valid_inst    := wbInstsValid.asUInt.orR
  cp0.io.except.hard_int_vec  := io.interrupt
  cp0.io.except.ret           := wbInsts(0).next_pc === MicroOpCtrl.Ret && wbInstsValid(0)
  cp0.io.except.epc           := Mux(wbALUSysReal || wbALUBpReal || wbALUOvfReal || illegal || respInt, wbInsts(0).pc, 
                                  Mux(wbMDUOvfReal, wbInsts(1).pc, wbInsts(2).pc)
                                 )
  cp0.io.except.in_delay_slot := cp0.io.except.epc === latestBJPC + 4.U
  cp0.io.except.bad_addr      := Mux(wbTlblReal || wbTlbsReal || wbModReal, wbTlbBadAddr, Mux(wbFetchMaReal, wbInsts(0).pc, wbMisalignedAddr))
  cp0.io.except.resp_for_int  := respInt
  cp0.io.ftc.wen              := wbInsts(0).write_dest === MicroOpCtrl.DCP0
  cp0.io.ftc.code             := Mux(cp0.io.ftc.wen, wbInsts(0).rd, exInsts(0).rs1)
  cp0.io.ftc.din              := wbData(0)
  if (withBigCore) {
    cp0.io.step               := wbInstsValid(0).asUInt + wbInstsValid(1).asUInt + wbInstsValid(2).asUInt
    cp0.io.ftTlb.exec.op        := wbInsts(0).tlb_op
    cp0.io.ftTlb.exec.din       := wbTlbOut
    cp0.io.ftTlb.vpn            := Mux(wbInstsValid(2), wbInsts(2).tlb_exp.vpn, wbInsts(0).tlb_exp.vpn)
    cp0.io.ftTlb.expVec         := Mux(wbInstsValid(2), wbInsts(2).tlb_exp.expVec, wbInsts(0).tlb_exp.expVec)
    cp0.io.ftc.sel              := exInsts(0).sel
  }
  else {
    cp0.io.ftc.sel              := 0.U
  }

  /**
   *  [---------- DiffTest stage -----------]
   */
   
  if (verilator) {
    // printf("AMIPSEL has commit %x, 1 %x, 2 %x, 3 %x\n", (wbInstsValid(0) || wbInstsValid(1) || wbInstsValid(2)) && !bubble_w, wbInsts(0).pc, wbInsts(1).pc, wbInsts(2).pc)
    BoringUtils.addSource(VecInit((0 to 2).map(i => (wbInstsValid(i) && !bubble_w))), "difftestValids")
    BoringUtils.addSource(VecInit((0 to 2).map(i => Mux(wbInstsValid(i), wbInsts(i).pc, 0.U))), "difftestPCs")
    // printf("valids %x, %x, %x; pcs %x, %x, %x\n", wbInstsValid(0) && !bubble_w, wbInstsValid(1) && !bubble_w, wbInstsValid(2) && !bubble_w, wbInsts(0).pc, wbInsts(1).pc, wbInsts(2).pc)
  }

  if (diffTestV) {
    val instret    = RegInit(0.U(64.W))
    val counter    = RegInit(0.U(64.W))
    val dstall     = RegInit(0.U(64.W))
    val istallw    = WireDefault(false.B)
    val istall     = RegInit(0.U(64.W))
    val mduStall   = RegInit(0.U(64.W))
    val common     = RegInit(0.U(64.W))

    dstall  := dstall + Mux(dcacheStall, 1.U, 0.U)
    istall  := istall + Mux(istallw, 1.U, 0.U)
    common  := common + Mux(dcacheStall && istallw, 1.U, 0.U)
    counter := counter + 1.U
    instret := Mux(bubble_w, instret, instret + (wbInstsValid(0) && !wbExcepts(0)).asUInt + (wbInstsValid(1) && !wbExcepts(1)).asUInt + (wbInstsValid(2) && !wbExcepts(2)).asUInt)
    mduStall := mduStall + (!mdu.io.resp.valid).asUInt()
    when (wbInsts(0).pc === endAddr.U || wbInsts(1).pc === endAddr.U || wbInsts(2).pc === endAddr.U) {
      printf("%d insts, %d cycles, %d d$ stalls, %d i$ stalls, %d mdu stalls, %d common stalls\n", instret + 1.U, counter, dstall, istall, mduStall, common)
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
    // get ordered exception
    val debug_ordered_wbExcepts = Wire(Vec(backendFuN, Bool()))
    for (i <- 0 until backendFuN) {
      debug_ordered_wbExcepts(i) := false.B
    }
    for (i <- 0 until backendFuN) {
      when (wbInstsValid(i)) {
        switch (wbInstsOrder(i)) {
          is(0.U) {
            debug_ordered_wbExcepts(0) := wbExcepts(i)
          }
          is(1.U) {
            debug_ordered_wbExcepts(1) := wbExcepts(i)
          }
          is(2.U) {
            debug_ordered_wbExcepts(2) := wbExcepts(i)
          }
        }
      }
    }
    for(i <- 0 until backendFuN) {
      when(!bubble_w && wbInstsValid(i)) {
        switch(wbInstsOrder(i)) {
          is(0.U) {
            debug_pc(0) := wbInsts(i).pc
            // the inst which has exception should be discarded in the difftest
            debug_wen(0) := wbInsts(i).write_dest === MicroOpCtrl.DReg && wbInsts(i).rd =/= 0.U && !debug_ordered_wbExcepts(0)
            debug_data(0) := MuxLookup(i.U, wbData(0),
            Seq(0.U -> wbData(0), 1.U -> wbData(1), 2.U -> wbData(2)))
            debug_nreg(0) := wbInsts(i).rd
          }
          is(1.U) {
            debug_pc(1) := wbInsts(i).pc
            // consider that previous insts have exception
            debug_wen(1) := wbInsts(i).write_dest === MicroOpCtrl.DReg && wbInsts(i).rd =/= 0.U && !debug_ordered_wbExcepts(0) && !debug_ordered_wbExcepts(1)
            debug_data(1) := MuxLookup(i.U, wbData(0),
            Seq(0.U -> wbData(0), 1.U -> wbData(1), 2.U -> wbData(2)))
            debug_nreg(1) := wbInsts(i).rd
          }
          is(2.U) {
            debug_pc(2) := wbInsts(i).pc
            val tmp = true.B
            debug_wen(2) := wbInsts(i).write_dest === MicroOpCtrl.DReg && wbInsts(i).rd =/= 0.U && !debug_ordered_wbExcepts(0) && !debug_ordered_wbExcepts(1) && tmp
            debug_data(2) := MuxLookup(i.U, wbData(0),
            Seq(0.U -> wbData(0), 1.U -> wbData(1), 2.U -> wbData(2)))
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
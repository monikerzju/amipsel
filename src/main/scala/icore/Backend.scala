package icore

import chisel3._
import chisel3.util._
import conf.Config
import fu._
import isa._
import chisel3.experimental.BundleLiterals._
import chisel3.util.experimental.BoringUtils

// class InstInfo extends Bundle with InstType with AluOpType with MDUOperation {
//   val rs = UInt(5.W)
//   val rt = UInt(5.W)
//   val rd = UInt(5.W)
//   val aluOp = UInt(aluOpWidth.W)
//   val mduOp = UInt(SZ_MDU_OP.W)
//   val imm = UInt(32.W)
//   val fuDest = UInt(typeLen.W)
//   val regWrite = Bool()
//   val pc = UInt(32.W)
//   val pcNext = UInt(32.W)
//   val bType = UInt(32.W)
//   val isBranch = Bool()
// }

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

class Backend extends Module with Config with InstType with MemAccessType {
  val io = IO(new BackendIO)
  val queueSize = 20
  val dcacheStall = WireDefault(false.B)
  dcacheStall := !io.dcache.resp.valid && io.dcache.req.valid
  val issueQueue = Module(new FIFO(queueSize, new Mops(), backendIssueN, frontendIssueN))
  issueQueue.io.enqStep := frontendIssueN.U

  for(i <- 0 until frontendIssueN) {
    // TODO: check
    val mops = io.fb.fmbs.inst_ops(i).asTypeOf(new Mops)
    issueQueue.io.din(i) := mops
  }

  val wbFlush = io.fb.bmfs.redirect_kill
  issueQueue.io.flush := wbFlush
  io.fb.fmbs.please_wait := !issueQueue.io.sufficient

  /**
   *  [---------- IS stage -----------]
   */
  def isDataHazard(inst1: Mops, inst2: Mops): Bool = {
    // TODO: add regWrite or memRead signal
    inst1.rd =/= 0.U && (inst1.rd === inst2.rs1 || inst1.rd === inst2.rs2)
  }

  def isCompatible(inst1: Mops, inst2: Mops): Bool = {
    !isDataHazard(inst1, inst2) &&
      (inst1.alu_mdu_lsu =/= inst2.alu_mdu_lsu ||
      inst1.alu_mdu_lsu === toAMU.U && inst2.alu_mdu_lsu === toAMU.U) //TODO: 4 fu
  }

  def isUglyCompatible(inst1: Mops, inst2: Mops, inst3: Mops): Bool = {
    val isLUorSU = inst3.alu_mdu_lsu === toLU.U || inst3.alu_mdu_lsu === toSU.U
    isCompatible(inst1, inst3) && isCompatible(inst2, inst3) &&
      MuxLookup(
        Cat(inst1.alu_mdu_lsu, inst2.alu_mdu_lsu),
        true.B,
        Seq(
          Cat(toAMU.U, toAMU.U) -> isLUorSU,
          Cat(toAMU.U, toALU.U) -> isLUorSU,
          Cat(toALU.U, toAMU.U) -> isLUorSU,
          Cat(toAMU.U, toMDU.U) -> isLUorSU,
          Cat(toMDU.U, toAMU.U) -> isLUorSU,
          Cat(toALU.U, toMDU.U) -> isLUorSU,
          Cat(toMDU.U, toALU.U) -> isLUorSU
        )
      )
  }


  val exNum = RegInit(0.U(2.W))
  val issueNum = WireDefault(0.U(2.W))
  issueQueue.io.deqStep := issueNum
  issueQueue.io.deqReq := !dcacheStall
  issueQueue.io.enqReq := io.fb.fmbs.instn =/= 0.U
  issueQueue.io.enqStep := io.fb.fmbs.instn
  val issueInsts = Wire(Vec(3, new Mops))
  issueInsts(0) := issueQueue.io.dout(0)
  issueInsts(1) := issueQueue.io.dout(1)
  issueInsts(2) := issueQueue.io.dout(2)

  // decide the true issue num
  val issueValid = WireDefault(VecInit(Seq.fill(4)(false.B)))
  // TODO: replace issueQueue.io.dout with issueInsts
  when(issueQueue.io.items > 0.U) {
    issueValid(0) := true.B
    issueNum := 1.U
    when(issueQueue.io.items > 1.U && isCompatible(issueQueue.io.dout(0), issueQueue.io.dout(1))) {
      // do not have data hazard and structural hazard
      issueValid(1) := true.B
      issueNum := 2.U
      when(issueQueue.io.items > 2.U && isUglyCompatible(issueQueue.io.dout(0), issueQueue.io.dout(1), issueQueue.io.dout(2))) {
        issueValid(2) := true.B
        issueNum := 3.U
      }

      /*
      when(isCompatible(issueQueue.io.dout(0), issueQueue.io.dout(2)) &&
        isCompatible(issueQueue.io.dout(1), issueQueue.io.dout(2)) && issueValid(1)
      ) { // do not have data hazard and structural hazard
        issueValid(2) := true.B
        issueNum := 3.U
      }
       */
    }
  }
  exNum := issueNum

  val exInsts = RegInit(
    VecInit(
      Seq.fill(4)({
        val instInfo = Wire(new Mops)
        instInfo.rs1 := 0.U
        instInfo.rs2 := 0.U
        instInfo.rd := 0.U
        instInfo.alu_op := 0.U
        instInfo.imm := 0.U
        instInfo.alu_mdu_lsu := toALU.U
        instInfo.write_dest := MicroOpCtrl.DXXX
        instInfo.next_pc := MicroOpCtrl.PC4
        instInfo.src_a := MicroOpCtrl.AXXX
        instInfo.src_b := MicroOpCtrl.BXXX
        instInfo.branch_type := MicroOpCtrl.BrXXX
        instInfo.mem_width := MicroOpCtrl.MemXXX
        instInfo.write_src := MicroOpCtrl.WBALU
        instInfo.pc := 0.U
        instInfo
      })
    )
  )
//  val nop = Wire(new Mops)
  val nop = WireInit(
    (new Mops).Lit(
      _.rs1 -> 0.U,
      _.rs2 -> 0.U,
      _.rd -> 0.U,
      _.alu_op -> 0.U,
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
  )

  // judge the inst type and issue inst into specific inst position
  // which associates with corresponding fu
  /*
    exInsts(0) -> ALU
    exInsts(1) -> MDU
    exInsts(2) -> LU
    exInsts(3) -> SU
   */
  val exInstsOrder = if(diffTestV) RegInit(VecInit(Seq.fill(4)(0.U(2.W)))) else Reg(Vec(4, UInt(2.W)))
  val exInstsValid = RegInit(VecInit(Seq.fill(4)(false.B)))
  when(!dcacheStall) {
    for(i <- 0 until 4) {
      exInstsValid(i) := false.B
    }
    for(i <- 0 until backendIssueN) {
      when(issueValid(i)) {
        switch(issueInsts(i).alu_mdu_lsu) {
          // TODO:
          is(toALU.U) {
            exInsts(0) := issueInsts(i)
            exInstsOrder(0) := i.U
            exInstsValid(0) := true.B
          }
          is(toMDU.U) {
            exInsts(1) := issueInsts(i)
            exInstsOrder(1) := i.U
            exInstsValid(1) := true.B
          }
          is(toLU.U) {
            exInsts(2) := issueInsts(i)
            exInstsOrder(2) := i.U
            exInstsValid(2) := true.B
          }
          is(toSU.U) {
            exInsts(3) := issueInsts(i)
            exInstsOrder(3) := i.U
            exInstsValid(3) := true.B
          }
          is(toAMU.U) {
            when(exInstsValid(0)) {
              exInsts(1) := issueInsts(i)
              exInstsOrder(1) := i.U
              exInstsValid(1) := true.B
            } .otherwise {
              exInsts(0) := issueInsts(i)
              exInstsOrder(0) := i.U
              exInstsValid(0) := true.B
            }
          }
        }
      }
    }
    // process toAMU
    when(issueValid(0) && issueValid(1) && issueInsts(0).alu_mdu_lsu === issueInsts(1).alu_mdu_lsu) {
      exInsts(0) := issueInsts(0)
      exInstsOrder(0) := 0.U
      exInstsValid(0) := true.B
      exInsts(1) := issueInsts(1)
      exInstsOrder(1) := 1.U
      exInstsValid(1) := true.B
    } .elsewhen(issueValid(0) && issueValid(2) && issueInsts(0).alu_mdu_lsu === issueInsts(2).alu_mdu_lsu) {
      exInsts(0) := issueInsts(0)
      exInstsOrder(0) := 0.U
      exInstsValid(0) := true.B
      exInsts(1) := issueInsts(2)
      exInstsOrder(1) := 2.U
      exInstsValid(1) := true.B
    } .elsewhen(issueValid(1) && issueValid(2) && issueInsts(1).alu_mdu_lsu === issueInsts(2).alu_mdu_lsu) {
      exInsts(0) := issueInsts(1)
      exInstsOrder(0) := 1.U
      exInstsValid(0) := true.B
      exInsts(1) := issueInsts(2)
      exInstsOrder(1) := 2.U
      exInstsValid(1) := true.B
    }
  }


  when(wbFlush) {
    for(i <- 0 until 4) {
      exInsts(i) := nop
      exInstsValid(i) := false.B
    }
  }



  /**
   *  [---------- EX stage -----------]
   */

  val alu = Module(new ALU)
  val wbNum = RegNext(exNum) // ?
  // TODO: MDU and LSU

  val mdu = Module(new MDU())
  val wbResult = RegInit(VecInit(Seq.fill(4)(0.U(len.W)))) //TODO:
  val wbInstsValid = RegNext(exInstsValid)
  val wbInstsOrder = RegNext(exInstsOrder)

  val wbInsts = RegInit(
    VecInit(
      Seq.fill(4)({
        val instInfo = Wire(new Mops)
        instInfo.rs1 := 0.U
        instInfo.rs2 := 0.U
        instInfo.rd := 0.U
        instInfo.alu_op := 0.U
        instInfo.imm := 0.U
        instInfo.alu_mdu_lsu := toALU.U
        instInfo.write_dest := MicroOpCtrl.DXXX
        instInfo.next_pc := MicroOpCtrl.PC4
        instInfo.src_a := MicroOpCtrl.AXXX
        instInfo.src_b := MicroOpCtrl.BXXX
        instInfo.branch_type := MicroOpCtrl.BrXXX
        instInfo.mem_width := MicroOpCtrl.MemXXX
        instInfo.write_src := MicroOpCtrl.WBALU
        instInfo.pc := 0.U
        instInfo
      })
    )
  )

  val aluSrcA = WireDefault(0.U(2.W))
  val fwdAluSrcAIndex = WireInit(0.U)
  val aluSrcB = WireDefault(0.U(2.W))
  val fwdAluSrcBIndex = WireInit(0.U)
  val regFile = Module(new RegFile(nread = 8, nwrite = 3)) // 8 read port, 3 write port

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

  val rsData = Wire(Vec(4, UInt(32.W)))
  val rtData = Wire(Vec(4, UInt(32.W)))
  for(i <- 0 until 4) {
    rsData(i) := regFile.io.rs_data_vec(2 * i)
    rtData(i) := regFile.io.rs_data_vec(2 * i + 1)
  }

  val reBranch = Wire(Bool())
  val wbReBranch = RegInit(false.B)
  val exIsBrFinal = exInstsOrder(0) === exNum - 1.U
  val wbIsBrFinal = RegNext(exIsBrFinal)
  val exBrSlot = Wire(Vec(4, Bool()))
  // judge delay slot
  exBrSlot(0) := false.B
  for(i <- 1 until 4) {
    // delay slot and branch are issued together
    exBrSlot(i) := !exIsBrFinal && exInstsOrder(i) === exInstsOrder(0) + 1.U
  }
  val wbBrSlot = Wire(Vec(4, Bool()))
  for(i <- 0 until 4) {
    // delay slot and branch are in different pipeline-stages
    wbBrSlot(i) := wbIsBrFinal && exInstsOrder(i) === 0.U
  }

  val resValid = Wire(Vec(4, Bool()))
  for(i <- 0 until 4) {
    resValid(i) := exInstsValid(i) && (!reBranch || exBrSlot(i)) && (!wbReBranch || wbBrSlot(i))
  }
  val wbResValid = RegNext(resValid)
  val wbData = Wire(Vec(3, UInt(32.W)))

  // TODO: load fwd, store fwd, load - store fwd
  // alu execution
  aluSrcA := MuxLookup(exInsts(0).src_a, 0.U,
    Seq(MicroOpCtrl.AReg -> 0.U, MicroOpCtrl.AShamt -> 1.U))
  aluSrcB := MuxLookup(exInsts(0).src_b, 0.U,
    Seq(MicroOpCtrl.BReg -> 0.U, MicroOpCtrl.BImm -> 1.U))
  alu.io.a := MuxLookup(aluSrcA, rsData(0),
    Seq(0.U -> rsData(0), 1.U -> exInsts(0).imm(10, 6), 2.U -> wbData(fwdAluSrcAIndex)))
  alu.io.b := MuxLookup(aluSrcB, rtData(0),
    Seq(0.U -> rtData(0), 1.U -> exInsts(0).imm, 2.U -> wbData(fwdAluSrcBIndex)))
  alu.io.aluOp := exInsts(0).alu_op
  wbResult(0) := alu.io.r
  val aluValid = exInstsValid(0) && (!reBranch || exBrSlot(0)) && (!wbReBranch || wbBrSlot(0))

  // mdu execution
  val mduSrc1 = Wire(UInt(2.W))
  val mduSrc2 = Wire(UInt(2.W))
  val fwdMduSrc1Index = WireInit(0.U)
  val fwdMduSrc2Index = WireInit(0.U)

  mduSrc1 := Mux(exInsts(1).alu_op > 15.U, 0.U, MuxLookup(exInsts(1).src_a, 0.U,
    Seq(MicroOpCtrl.AReg -> 0.U, MicroOpCtrl.AShamt -> 1.U)))
  mduSrc2 := Mux(exInsts(1).alu_op > 15.U, 0.U, MuxLookup(exInsts(1).src_b, 0.U,
    Seq(MicroOpCtrl.BReg -> 0.U, MicroOpCtrl.BImm -> 1.U)))

  mdu.io.req.in1 := MuxLookup(mduSrc1, rsData(1),
    Seq(0.U -> rsData(1), 1.U -> exInsts(1).imm(10, 6), 2.U -> wbData(fwdMduSrc1Index)))
  mdu.io.req.in2 := MuxLookup(mduSrc2, rtData(1),
    Seq(0.U -> rtData(1), 1.U -> exInsts(1).imm, 2.U -> wbData(fwdMduSrc2Index)))
  mdu.io.req.op := exInsts(1).alu_op

  val hi = RegInit(0.U(len.W))
  val lo = RegInit(0.U(len.W))
  val mduValid = exInstsValid(1) && (!reBranch || exBrSlot(1)) && (!wbReBranch || wbBrSlot(1))
  wbResult(1) := mdu.io.resp.lo
  hi := Mux(mduValid, mdu.io.resp.hi, hi)
  lo := Mux(mduValid, mdu.io.resp.lo, lo)

  // initialize lsu
  io.dcache.req.bits.flush := false.B
  io.dcache.req.bits.invalidate := false.B
  // lu execution
  val sqSize = 5
  class StoreInfo extends MemReq {
    val rd = Output(UInt(5.W))
  }
  val storeQueue = Module(new FIFO(sqSize, new StoreInfo, sqSize, 1))
  val loadValid = exInstsValid(2) && (!reBranch || exBrSlot(2)) && (!wbReBranch || wbBrSlot(2))
  val storeValid = Wire(Bool())
//  io.dcache.req.valid := loadValid || storeValid

  val isDataInSQ = Reg(Bool())
  val dataLoadInSQIndex = WireDefault(0.U(log2Ceil(sqSize).W))
  val dataLoadInSQ = RegInit(0.U(32.W))
  for(i <- 0 until sqSize) {
    when(storeQueue.io.dout(i).rd === exInsts(2).rd) {
      isDataInSQ := true.B
      dataLoadInSQIndex := i.U
    }
  }
  dataLoadInSQ := storeQueue.io.dout(dataLoadInSQIndex).wdata

  // su execution
  val canStore = Wire(Bool())
  canStore := !loadValid && storeQueue.io.items > 0.U //TODO: maybe changed
  storeQueue.io.enqStep := 1.U
  storeQueue.io.deqStep := 1.U
  storeQueue.io.enqReq := storeValid
  storeQueue.io.deqReq := canStore
  val storeAddr = Wire(UInt(32.W))
  storeAddr :=  rsData(3) + exInsts(3).imm
  storeQueue.io.din(0) := {
    val storeInfo = Wire(new StoreInfo)
    storeInfo.addr := storeAddr
    storeInfo.wdata := rtData(3)
    storeInfo.wen := (exInsts(3).write_dest === MicroOpCtrl.DMem)
    storeInfo.mtype := exInsts(3).mem_width
    storeInfo.flush := false.B
    storeInfo.invalidate := false.B
    storeInfo.rd := exInsts(3).rd
    storeInfo
  }

//  val storeValid = exInstsValid(3) && (!reBranch || exBrSlot(3)) && (!wbReBranch || wbBrSlot(3))
  storeValid := storeQueue.io.items > 0.U && (!reBranch || exBrSlot(3)) && (!wbReBranch || wbBrSlot(3))
  io.dcache.req.valid := loadValid || storeValid
  io.dcache.req.bits.mtype := Mux(
    loadValid,
    exInsts(2).mem_width,
    Mux(storeQueue.io.items > 0.U, storeQueue.io.dout(0).mtype, exInsts(3).mem_width)
  )
  storeQueue.io.flush := false.B
  io.dcache.req.bits.wen := canStore
  io.dcache.req.bits.wdata := storeQueue.io.dout(0).wdata
  io.dcache.req.bits.addr := Mux( // load first, then store
    loadValid,
    rsData(2) + exInsts(2).imm,
    Mux(storeQueue.io.items > 0.U, storeQueue.io.dout(0).addr, storeAddr)
  )



  // forwarding here?
  // assume I-type Inst replace rt with rd, and update rt = 0
  // TODO: forward mdu and lsu
  for(i <- 0 until 3) {
    when(wbInstsValid(i) && wbInsts(i).write_dest === MicroOpCtrl.DReg && wbInsts(i).rd =/= 0.U) {
      when(wbInsts(i).rd === exInsts(0).rs1) {
        aluSrcA := 2.U
        fwdAluSrcAIndex := i.U
      }
      when(wbInsts(i).rd === exInsts(0).rs2) {
        aluSrcB := 2.U
        fwdAluSrcBIndex := i.U
      }
      when(wbInsts(i).rd === exInsts(1).rs1) {
        mduSrc1 := 2.U
        fwdMduSrc1Index := i.U
      }
      when(wbInsts(i).rd === exInsts(1).rs2) {
        mduSrc2 := 2.U
        fwdMduSrc2Index := i.U
      }
    }
  }



  val pcRedirect = Reg(UInt(len.W))
  // process branch

  reBranch := false.B
  pcRedirect := exInsts(0).pc + (exInsts(0).imm << 2.U)(31, 0) + 4.U
  when(exInsts(0).next_pc === MicroOpCtrl.Branch) {
    switch(exInsts(0).branch_type) {
      is(MicroOpCtrl.BrEQ) { // beq
        when(alu.io.zero === 1.U) {
          reBranch := true.B
        }
      }
      is(MicroOpCtrl.BrNE) { // bne
        when(alu.io.zero === 0.U) {
          reBranch := true.B
        }
      }
    }
  }
  wbReBranch := reBranch




  /**
   *  [---------- WB stage -----------]
   */


  io.fb.bmfs.redirect_pc := pcRedirect
  io.fb.bmfs.redirect_kill := false.B
  // branch, wait for delay slot
  val pcSlot = RegInit(0.U(len.W))
  val waitSlot = RegInit(false.B)


  // dcacheStall?
  when(wbReBranch) {
    when(wbIsBrFinal) {
      pcSlot := pcRedirect
      waitSlot := true.B
      io.fb.bmfs.redirect_kill := false.B
    } .otherwise {
      io.fb.bmfs.redirect_kill := true.B
    }
  }

  when(waitSlot && !dcacheStall) {
    waitSlot := false.B
    io.fb.bmfs.redirect_pc := pcSlot
    io.fb.bmfs.redirect_kill := true.B
  }

  when(!dcacheStall) {
    wbInsts := exInsts
  }

  when(dcacheStall) {
    for(i <- 0 until 4) {
      wbResValid(i) := false.B
    }
  }

  when(wbFlush) {
    for(i <- 0 until 4) {
      wbInsts(i) := nop
      wbInstsValid(i) := false.B
    }
  }


  /*
  for(i <- 0 until 3) {
    regFile.io.wen_vec(i) := Mux(wbInstsValid(i) && wbResValid(i), wbInsts(i).write_dest === MicroOpCtrl.DReg, false.B)
  }
  regFile.io.rd_addr_vec := VecInit(Seq(wbInsts(0).rd, wbInsts(1).rd, wbInsts(2).rd)) // ?
   */


  // handle load-inst separately
  val dataFromDcache = Wire(UInt(32.W))
  dataFromDcache := io.dcache.resp.bits.rdata(0) // connect one port
  io.dcache.resp.ready := true.B
  val dataFromDcacheExtend = Wire(UInt(32.W))
  // load mask
  dataFromDcacheExtend := dataFromDcache
  switch(wbInsts(3).mem_width) {
    is(MicroOpCtrl.MemByte) {
      dataFromDcacheExtend := Cat(Fill(24, dataFromDcache(7)), dataFromDcache(7, 0))
    }
    is(MicroOpCtrl.MemByteU) {
      dataFromDcacheExtend := Cat(Fill(24, 0.U), dataFromDcache(7, 0))
    }
    is(MicroOpCtrl.MemHalf) {
      dataFromDcacheExtend := Cat(Fill(16, dataFromDcache(15)), dataFromDcache(15, 0))
    }
    is(MicroOpCtrl.MemHalfU) {
      dataFromDcacheExtend := Cat(Fill(16, 0.U), dataFromDcache(15, 0))
    }
  }

  val luData = Wire(UInt(len.W))
  luData := Mux(isDataInSQ, dataLoadInSQ, dataFromDcacheExtend)

  wbData(0) := wbResult(0)
  wbData(1) := wbResult(1)
  wbData(2) := luData



  /*
  regFile.io.rd_data_vec(0) := wbResult(0)
  regFile.io.rd_data_vec(1) := wbResult(1)
  regFile.io.rd_data_vec(2) := luData
   */

  for(i <- 0 until 3) {
    regFile.io.wen_vec(i) := false.B
    regFile.io.rd_addr_vec(i) := 0.U
    regFile.io.rd_data_vec(i) := 0.U
  }

  for(i <- 0 until 3) {
    when(wbInstsValid(i)) {
      switch(wbInstsOrder(i)) {
        is(0.U) {
          regFile.io.wen_vec(0) := wbResValid(i) && wbInsts(i).write_dest === MicroOpCtrl.DReg
          regFile.io.rd_addr_vec(0) := wbInsts(i).rd
          regFile.io.rd_data_vec(0) := MuxLookup(i.U, wbResult(0),
            Seq(0.U -> wbResult(0), 1.U -> wbResult(1), 2.U -> luData))
        }
        is(1.U) {
          regFile.io.wen_vec(1) := wbResValid(i) && wbInsts(i).write_dest === MicroOpCtrl.DReg
          regFile.io.rd_addr_vec(1) := wbInsts(i).rd
          regFile.io.rd_data_vec(1) := MuxLookup(i.U, wbResult(0),
            Seq(0.U -> wbResult(0), 1.U -> wbResult(1), 2.U -> luData))
        }
        is(2.U) {
          regFile.io.wen_vec(2) := wbResValid(i) && wbInsts(i).write_dest === MicroOpCtrl.DReg
          regFile.io.rd_addr_vec(2) := wbInsts(i).rd
          regFile.io.rd_data_vec(2) := MuxLookup(i.U, wbResult(0),
            Seq(0.U -> wbResult(0), 1.U -> wbResult(1), 2.U -> luData))
        }
      }
    }
  }

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
            debug_wen(0) := wbResValid(i) && wbInsts(0).write_dest === MicroOpCtrl.DReg && wbInsts(0).rd =/= 0.U
            debug_data(0) := wbResult(i)
            debug_nreg(0) := wbInsts(i).rd
          }
          is(1.U) {
            debug_pc(1) := wbInsts(i).pc
            debug_wen(1) := wbResValid(i) && wbInsts(0).write_dest === MicroOpCtrl.DReg && wbInsts(0).rd =/= 0.U
            debug_data(1) := wbResult(i)
            debug_nreg(1) := wbInsts(i).rd
          }
          is(2.U) {
            debug_pc(2) := wbInsts(i).pc
            debug_wen(2) := wbResValid(i) && wbInsts(0).write_dest === MicroOpCtrl.DReg && wbInsts(0).rd =/= 0.U
            debug_data(2) := wbResult(i)
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
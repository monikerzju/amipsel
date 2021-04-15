package icore

import chisel3._
import chisel3.util._
import conf.Config
import fu._
import isa._
import chisel3.experimental.BundleLiterals._

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
  dcacheStall := !io.dcache.resp.valid
  val issueQueue = Module(new FIFO(queueSize, new Mops(), backendIssueN, frontendIssueN))
  issueQueue.io.enqStep := frontendIssueN.U
  issueQueue.io.enqReq := true.B

  // TODO: connect with frontend
  io.fb.fmbs.please_wait := !issueQueue.io.sufficient

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
    inst1.rd === inst2.rs1 || inst1.rd === inst2.rs2
  }

  def isCompatible(inst1: Mops, inst2: Mops): Bool = {
    !isDataHazard(inst1, inst2) && inst1.alu_mdu_lsu =/= inst2.alu_mdu_lsu //TODO: 4 fu
  }

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

  val exNum = RegInit(0.U(3.W))
  issueQueue.io.deqStep := exNum
  issueQueue.io.deqReq := !dcacheStall
  val issueInsts = Wire(Vec(3, new Mops))
  issueInsts(0) := issueQueue.io.dout(0)
  issueInsts(1) := issueQueue.io.dout(1)
  issueInsts(2) := issueQueue.io.dout(2)

  // decide the true issue num
  val issueValid = WireDefault(VecInit(Seq.fill(4)(false.B)))
  when(!dcacheStall) {
    for(i <- 0 until backendIssueN) {
      when(i.U < issueQueue.io.items) {
        issueValid(0) := true.B
        exNum := 1.U
        switch(i.U) {
          is(1.U) {
            when(isCompatible(issueQueue.io.dout(0), issueQueue.io.dout(1))
            ) { // do not have data hazard and structural hazard
              issueValid(1) := true.B
              exNum := 2.U
            }
          }
          is(2.U) {
            when(isCompatible(issueQueue.io.dout(0), issueQueue.io.dout(2)) &&
              (!isCompatible(issueQueue.io.dout(0), issueQueue.io.dout(1)) ||
                isCompatible(issueQueue.io.dout(1), issueQueue.io.dout(2)))
            ) { // do not have data hazard and structural hazard
              issueValid(2) := true.B
              exNum := 3.U
            }
          }
        }
      }
    }
  }

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
  val exInstsOrder = Reg(Vec(4, UInt(2.W)))
  for(i <- 0 until backendIssueN) {
    when(issueValid(i)) {
      switch(issueInsts(i).alu_mdu_lsu) {
        // TODO:
        is(toALU.U) {
          exInsts(0) := issueInsts(i)
          exInstsOrder(0) := i.U
        }
        is(toMDU.U) {
          exInsts(1) := issueInsts(i)
          exInstsOrder(1) := i.U
        }
        is(toLU.U) {
          exInsts(2) := issueInsts(i)
          exInstsOrder(2) := i.U
        }
        is(toSU.U) {
          exInsts(3) := issueInsts(i)
          exInstsOrder(3) := i.U
        }
      }
    }
  }

  when(wbFlush) {
    for(i <- 0 until backendIssueN) {
      exInsts(i) := nop
    }
  }

  val exInstsValid = RegInit(VecInit(Seq.fill(4)(false.B)))
  exInstsValid := issueValid  // ?

  /**
   *  [---------- EX stage -----------]
   */

  val alu = Module(new ALU)
  val wbNum = RegNext(exNum) // ?
  // TODO: MDU and LSU

  val mdu = Module(new MDU())
  val wbResult = RegInit(VecInit(Seq.fill(4)(0.U(len.W)))) //TODO:

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
  val fwdSrcAIndex = WireInit(0.U)
  val aluSrcB = WireDefault(0.U(2.W))
  val fwdSrcBIndex = WireInit(0.U)
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
  // 0 - mdu 1 - lu 2 - su
//  val brDelay = WireDefault(VecInit(Seq.fill(3)(false.B)))
  // initialize alu
  alu.io.a := 0.U
  alu.io.b := 0.U
  alu.io.aluOp := nop.alu_op

  // initialize mdu
  val hi = RegInit(0.U(len.W))
  val lo = RegInit(0.U(len.W))
  mdu.io.req.in1 := 0.U
  mdu.io.req.in2 := 0.U
  mdu.io.req.op := nop.alu_op
  hi := Mux(exInstsValid(1), mdu.io.resp.hi, hi)
  lo := Mux(exInstsValid(1), mdu.io.resp.lo, lo)

  // initialize lsu
  io.dcache.req.bits.flush := false.B
  io.dcache.req.bits.invalidate := false.B
  io.dcache.req.bits.mtype := MEM_WORD.U


  // alu execution
  alu.io.a := MuxLookup(aluSrcA, rsData(0),
    Seq(0.U -> rsData(0), 1.U -> exInsts(0).pc, 2.U -> wbResult(fwdSrcAIndex)))
  alu.io.b := MuxLookup(aluSrcB, rtData(1),
    Seq(0.U -> rtData(1), 1.U -> exInsts(1).imm, 2.U -> wbResult(fwdSrcBIndex)))
  alu.io.aluOp := exInsts(0).alu_op
  wbResult(0) := alu.io.r
  // TODO: mdu

  // mdu execution
  mdu.io.req.in1 := rsData(1)
  mdu.io.req.in2 := rtData(1)
  mdu.io.req.op := exInsts(1).alu_op

  // lu execution
  val sqSize = 5
  class StoreInfo extends MemReq {
    val rd = Output(UInt(5.W))
  }
  val storeQueue = Module(new FIFO(sqSize, new StoreInfo, sqSize, 1))
  io.dcache.req.valid := !reBranch && (exInstsValid(2) | exInstsValid(3))
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
  canStore := !exInstsValid(2) && storeQueue.io.items > 0.U //TODO: maybe changed
  storeQueue.io.enqStep := 1.U
  storeQueue.io.deqStep := 1.U
  storeQueue.io.enqReq := exInstsValid(3)
  storeQueue.io.deqReq := canStore
  val storeAddr = Wire(UInt(32.W))
  storeAddr :=  exInsts(3).rs1 + Cat(Fill(16, exInsts(3).imm), exInsts(3).imm)
  storeQueue.io.din(0) := {
    val storeInfo = Wire(new StoreInfo)
    storeInfo.addr := storeAddr
    storeInfo.wdata := rtData(3)
    storeInfo.wen := (exInsts(3).write_dest === MicroOpCtrl.DMem)
    storeInfo.mtype := MEM_WORD.U
    storeInfo.flush := false.B
    storeInfo.invalidate := false.B
    storeInfo.rd := exInsts(3).rd
    storeInfo
  }
  /*
  storeQueue.io.din(0) := (new StoreInfo).Lit(
    _.addr -> storeAddr,
    _.wdata -> rtData(3),
    _.wen -> (exInsts(3).write_dest === MicroOpCtrl.DMem),
    _.mtype -> MEM_WORD.U,
    _.flush -> false.B,
    _.invalidate -> false.B,
    _.rd -> exInsts(3).rd
  )

   */
  storeQueue.io.flush := false.B
  io.dcache.req.bits.wen := canStore
  io.dcache.req.bits.wdata := storeQueue.io.dout(0).wdata
  io.dcache.req.bits.addr := Mux( // load first, then store
    exInstsValid(2),
    exInsts(2).rs1 + Cat(Fill(16, exInsts(2).imm), exInsts(2).imm),
    storeQueue.io.dout(0).addr
  )



  // forwarding here?
  // assume I-type Inst replace rt with rd, and update rt = 0
  // TODO: forward mdu and lsu
  for(i <- 0 until 3) {
    when(wbInsts(i).write_dest === MicroOpCtrl.DReg && wbInsts(i).rd =/= 0.U) {
      when(wbInsts(i).rd === exInsts(0).rs1) {
        aluSrcA := 2.U
      }
      when(wbInsts(i).rd === exInsts(0).rs2) {
        aluSrcB := 2.U
      }
    }
  }



  val wbReBranch = RegInit(false.B)
  val pcRedirect = Reg(UInt(len.W))
  // process branch

  reBranch := false.B
  pcRedirect := exInsts(0).pc + Cat(Fill(16, exInsts(0).imm), exInsts(0).imm) + 4.U
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
  /*
  for(i <- 0 until backendIssueN) {
    reBranch := false.B
    when(i.U < exNum) {
      when(exInsts(i).isBranch) {
        wbReBranch := false.B
        switch(exInsts(i).bType) {
          is(0.U) { // beq
            when(alu.io.zero === 1.U) {
              pcRedirect := alu.io.r
              wbReBranch := true.B
              reBranch := true.B
            }
          }
          is(1.U) { // bne
            when(alu.io.zero === 0.U) {
              pcRedirect := alu.io.r
              wbReBranch := true.B
              reBranch := true.B
            }
          }
        }
      }
    }
  }
   */
  /*
  for(i <- 1 until backendIssueN) {
    when(i.U < exNum) {
      brDelay(0) := Mux(exInsts(i).fuDest === toMDU.U(typeLen.W), true.B, false.B)
      brDelay(1) := Mux(exInsts(i).fuDest === toLU.U(typeLen.W), true.B, false.B)
      brDelay(2) := Mux(exInsts(i).fuDest === toSU.U(typeLen.W), true.B, false.B)
    }
  }
   */

  when(wbFlush) {
    for(i <- 0 until 4) {
      wbInsts(i) := nop
    }
  }

  val wbInstsOrder = RegNext(exInstsOrder)
  /**
   *  [---------- WB stage -----------]
   */

  val wbInstsValid = RegNext(exInstsValid)
  io.fb.bmfs.redirect_pc := pcRedirect
  io.fb.bmfs.redirect_kill := false.B
  // branch, wait for delay slot
  val pcSlot = RegInit(0.U(len.W))
  val waitSlot = RegInit(false.B)



  when(wbReBranch) {
    when(wbInstsValid(0) && wbInstsOrder(0) === wbNum - 1.U) {
      pcSlot := pcRedirect
      waitSlot := true.B
      io.fb.bmfs.redirect_kill := false.B
    }
  }

  when(waitSlot) {
    waitSlot := false.B
    io.fb.bmfs.redirect_pc := pcSlot
    io.fb.bmfs.redirect_kill := true.B
  }


  /*
  when(wbReBranch) {
    for(i <- 0 until backendIssueN) {
      when(i.U < wbNum) {
        when(wbInsts(i).isBranch && i.U === wbNum - 1.U) {
          pcSlot := pcRedirect
          waitSlot := true.B
          io.fb.bmfs.redirect_kill := false.B
        }
      }
    }
  }

   */




  when(!dcacheStall) {
    wbInsts := exInsts
  }

  for(i <- 0 until 3) {
    regFile.io.wen_vec(i) := Mux(wbInstsValid(i), wbInsts(i).write_dest === MicroOpCtrl.DReg, false.B)
  }


  regFile.io.rd_addr_vec := VecInit(Seq(wbInsts(0).rd, wbInsts(1).rd, wbInsts(2).rd)) // ?
  // handle load-inst separately
  val dataFromDcache = Wire(UInt(32.W))
  dataFromDcache := io.dcache.resp.bits.rdata(0) // connect one port
  io.dcache.resp.ready := true.B

  regFile.io.rd_data_vec(0) := wbResult(0)
  regFile.io.rd_data_vec(1) := wbResult(1)
  regFile.io.rd_data_vec(2) := Mux(isDataInSQ, dataLoadInSQ, dataFromDcache)

  /*
  for(i <- 0 until backendIssueN) {
    when(i.U < wbNum) {
      when(wbInsts(i).fuDest === toLU.U(typeLen.W)) {
        regFile.io.rd_data_vec(i) := dataFromDcache
      } .otherwise {
        regFile.io.rd_data_vec(i) := wbResult(i)
      }
    } .otherwise {
      regFile.io.rd_data_vec(i) := wbResult(i)
    }
  }
   */
}
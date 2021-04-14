package icore

import chisel3._
import chisel3.util._
import conf.Config
import fu._
import isa._
import chisel3.experimental.BundleLiterals._

class InstInfo extends Bundle with InstType with AluOpType with MDUOperation {
  val rs = UInt(5.W)
  val rt = UInt(5.W)
  val rd = UInt(5.W)
  val aluOp = UInt(aluOpWidth.W)
  val mduOp = UInt(SZ_MDU_OP.W)
  val imm = UInt(32.W)
  val fuDest = UInt(typeLen.W)
  val regWrite = Bool()
  val pc = UInt(32.W)
  val pcNext = UInt(32.W)
  val bType = UInt(32.W)
  val isBranch = Bool()
}

trait InstType {
  val typeLen = 3
  val toALU = 0
  val toMDU = 1
  val toLU = 2
  val toSU = 3
}

class StoreInfo extends Bundle with Config {
  val addr = UInt(len.W)
  val data = UInt(len.W)
}

class Backend extends Module with Config with InstType {
  val io = IO(new BackendIO)
  val queueSize = 20
  val dcacheStall = WireDefault(false.B)
  val issueQueue = Module(new FIFO(queueSize, new InstInfo(), backendIssueN, frontendIssueN))
  issueQueue.io.enqStep := frontendIssueN.U
  issueQueue.io.enqReq := true.B

  // TODO: connect with frontend
  io.fb.fmbs.please_wait := !issueQueue.io.sufficient
  for(i <- 0 until frontendIssueN) {
    // TODO: check all
    val mops = io.fb.fmbs.inst_ops(i).asTypeOf(new Mops)
    issueQueue.io.din(i).rs := mops.rs1
    issueQueue.io.din(i).rt := mops.rs2
    issueQueue.io.din(i).rd := mops.rd
    issueQueue.io.din(i).aluOp := mops.alu_op
    issueQueue.io.din(i).mduOp := mops.alu_op
    issueQueue.io.din(i).imm := mops.imm
    issueQueue.io.din(i).fuDest := mops.alu_mdu_lsu
    issueQueue.io.din(i).regWrite := mops.write_dest === MicroOpCtrl.DXXX
    issueQueue.io.din(i).pc := mops.next_pc
    issueQueue.io.din(i).pcNext := mops.next_pc
    issueQueue.io.din(i).bType := mops.branch_type
    issueQueue.io.din(i).isBranch := mops.branch_type === MicroOpCtrl.BrXXX
  }
  val wbFlush = io.fb.bmfs.redirect_kill
  issueQueue.io.flush := wbFlush
  /**
   *  [---------- IS stage -----------]
   */
  def isDataHazard(inst1: InstInfo, inst2: InstInfo): Bool = {
    inst1.rd === inst2.rs || inst1.rd === inst2.rt
  }

  def isCompatible(inst1: InstInfo, inst2: InstInfo): Bool = {
    !isDataHazard(inst1, inst2) && inst1.fuDest =/= inst2.fuDest
  }
  // TODO: associate the three types with frontend
  /* TODO try with ENUM? */
  // val toALU.U(typeLen.W) :: toMDU.U(typeLen.W) :: toLSU :: Nil = Enum(3)
  // val exInsts = RegInit(VecInit(Seq.fill(backendIssueN)(new InstInfo()))) // ?
  val exInsts = RegInit(
    VecInit(
      Seq.fill(backendIssueN)({
        val instInfo = Wire(new InstInfo)
        instInfo.rs := 0.U
        instInfo.rt := 0.U
        instInfo.rd := 0.U
        instInfo.aluOp := 0.U
        instInfo.mduOp := 0.U
        instInfo.imm := 0.U
        instInfo.fuDest := toALU.U
        instInfo.regWrite := false.B
        instInfo.pc := 0.U
        instInfo.pcNext := 0.U
        instInfo.bType := 0.U
        instInfo.isBranch := false.B
        instInfo
      })
    )
  )
  val exNum = RegInit(0.U(3.W))
  issueQueue.io.deqStep := exNum
  when(!dcacheStall) {
    issueQueue.io.deqReq := true.B
  } .otherwise {
    issueQueue.io.deqReq := false.B
  }
  // decide the true issue num
  when(!dcacheStall) {
    for(i <- 0 until backendIssueN) {
      when(i.U < issueQueue.io.items) {
        exInsts(0) := issueQueue.io.dout(0)
        exNum := 1.U
        switch(i.U) {
          is(1.U) {
            when(isCompatible(issueQueue.io.dout(0), issueQueue.io.dout(1))
            ) { // do not have data hazard and structural hazard
              exInsts(1) := issueQueue.io.dout(1)
              exNum := 2.U
            }
          }
          is(2.U) {
            when(isCompatible(issueQueue.io.dout(0), issueQueue.io.dout(2)) &&
              (!isCompatible(issueQueue.io.dout(0), issueQueue.io.dout(1)) ||
                isCompatible(issueQueue.io.dout(1), issueQueue.io.dout(2)))
            ) { // do not have data hazard and structural hazard
              exInsts(2) := issueQueue.io.dout(2)
              exNum := 3.U
            }
          }
        }
      }
    }
  }

//  val nop = Wire(new InstInfo)
  val nop = WireInit(
    (new InstInfo).Lit(
      _.rs -> 0.U,
      _.rt -> 0.U,
      _.rd -> 0.U,
      _.aluOp -> 0.U,
      _.mduOp -> 0.U,
      _.imm -> 0.U,
      _.fuDest -> toALU.U,
      _.regWrite -> false.B,
      _.pc -> 0.U,
      _.pcNext -> 0.U,
      _.bType -> 0.U,
      _.isBranch -> false.B
    )
  )
  nop.fuDest := toALU.U(typeLen.W)
  nop.regWrite := false.B
  when(wbFlush) {
    for(i <- 0 until backendIssueN) {
      exInsts(i) := nop
    }
  }

  /**
   *  [---------- EX stage -----------]
   */
  val alu = Module(new ALU)
  val wbNum = RegNext(exNum) // ?
  // TODO: MDU and LSU

  val mdu = Module(new MDU())
  val wbResult = RegInit(VecInit(Seq.fill(3)(0.U(len.W))))
//  val wbInsts = RegInit(VecInit(Seq.fill(backendIssueN)(new InstInfo()))) // ?

  val wbInsts = RegInit(
    VecInit(
      Seq.fill(backendIssueN)({
        val instInfo = Wire(new InstInfo)
        instInfo.rs := 0.U
        instInfo.rt := 0.U
        instInfo.rd := 0.U
        instInfo.aluOp := 0.U
        instInfo.mduOp := 0.U
        instInfo.imm := 0.U
        instInfo.fuDest := toALU.U
        instInfo.regWrite := false.B
        instInfo.pc := 0.U
        instInfo.pcNext := 0.U
        instInfo.bType := 0.U
        instInfo.isBranch := false.B
        instInfo
      })
    )
  )
  // wbInsts := RegNext(exInsts) // ?


  val aluSrcA = WireDefault(0.U(2.W))
  val fwdSrcAIndex = WireInit(0.U)
  val aluSrcB = WireDefault(0.U(2.W))
  val fwdSrcBIndex = WireInit(0.U)
  val regFile = Module(new RegFile()) // 6 read port, 3 write port

  /*
    read port
    rs_1
    rt_1
    rs_2
    rt_2
    rs_3
    rt_3
   */
  for(i <- 0 until 3) {
    regFile.io.rs_addr_vec(2 * i) := exInsts(i).rs
    regFile.io.rs_addr_vec(2 * i + 1) := exInsts(i).rt
  }

  val rsData = Wire(Vec(3, UInt(32.W)))
  val rtData = Wire(Vec(3, UInt(32.W)))
  for(i <- 0 until 3) {
    rsData(i) := regFile.io.rs_data_vec(2 * i)
    rtData(i) := regFile.io.rs_data_vec(2 * i + 1)
  }

  val sqSize = 10
//  val storeQueue = Module(new Queue(new StoreInfo, sqSize))

  val reBranch = Wire(Bool())
  // 0 - mdu 1 - lu 2 - su
  val brDelay = WireDefault(VecInit(Seq.fill(3)(false.B)))
  // initialize alu
  alu.io.a := 0.U
  alu.io.b := 0.U
  alu.io.aluOp := nop.aluOp

  // initialize mdu
  mdu.io.req.in1 := 0.U
  mdu.io.req.in2 := 0.U
  mdu.io.req.op := nop.mduOp

  for (i <- 0 until backendIssueN) {
    when(i.U < exNum) {
      switch(exInsts(i).fuDest) {
        is(toALU.U(typeLen.W)) {
          // TODO: mux ?
          alu.io.a := MuxLookup(aluSrcA, rsData(i),
            Seq(0.U -> rsData(i), 1.U -> exInsts(i).pc, 2.U -> wbResult(fwdSrcAIndex)))
          alu.io.b := MuxLookup(aluSrcB, rtData(i),
            Seq(0.U -> rtData(i), 1.U -> exInsts(i).imm, 2.U -> wbResult(fwdSrcBIndex)))
          alu.io.aluOp := exInsts(i).aluOp
          wbResult(i) := alu.io.r
        }
        is(toMDU.U(typeLen.W)) {
          // TODO: deal with branch
          when(!reBranch && !brDelay(0)) {

          }
          mdu.io.req.in1 := rsData(i)
          mdu.io.req.in2 := rtData(i)
          mdu.io.req.op := exInsts(i).mduOp
        }
        is(toLU.U(typeLen.W)) {
          // TODO: connect with dcache
          when(!reBranch && !brDelay(1)) {
            // TODO: deal with branch
          }
        }
        is(toSU.U(typeLen.W)) {
          // TODO
          when(!reBranch && !brDelay(2)) {
            // TODO: deal with branch
          }
          /*
          storeQueue.io.enq.bits.addr := rsData(i) + Cat(Fill(16, exInsts(i).imm(15)),exInsts(i).imm) // sign-extended
          storeQueue.io.enq.bits.data := rtData(i)
          storeQueue.io.enq.ready := true.B
           */
        }
      }
    }
  }
  // forwarding here?
  // assume I-type Inst replace rt with rd, and update rt = 0
  for(i <- 0 until backendIssueN) {
    when(i.U < exNum) {
      for(j <- 0 until backendIssueN) {
        when(j.U < wbNum) {
          when(wbInsts(j).regWrite && wbInsts(j).rd =/= 0.U) {
            when(wbInsts(j).rd === exInsts(i).rs) {
              aluSrcA := 2.U
            }
            when(wbInsts(j).rd === exInsts(i).rt) {
              aluSrcB := 2.U
            }
          }
        }
      }
    }
  }
  val wbReBranch = RegInit(false.B)
  val pcRedirect = Reg(UInt(len.W))
  // process branch
  for(i <- 0 until backendIssueN) {
    reBranch := false.B
    when(i.U < exNum) {
      when(exInsts(i).isBranch) {
        wbReBranch := false.B
        switch(exInsts(i).bType) {
          is(0.U) { // beq
            /*
            when(alu.io.zero === 1.U && alu.io.r =/= exInsts(i).pcNext) {
              pcRedirect := alu.io.r
              wbReBranch := true.B
            } .elsewhen(alu.io.zero === 0.U && exInsts(i).pcNext =/= exInsts(i).pc + 4.U) {
              pcRedirect := exInsts(i).pc + 4.U
              wbReBranch := true.B
            }
             */
            when(alu.io.zero === 1.U) {
              pcRedirect := alu.io.r
              wbReBranch := true.B
              reBranch := true.B

              /* flush insts behind the br_inst
              for (j <- i + 1 until backendIssueN) {
                when(!dcacheStall) {
                  switch(exInsts(j).fuDest) {
                    is(toALU.U(typeLen.W)) {
                      wbInsts(j).rd := 0.U
                    }
                    is(toLU.U(typeLen.W)) {
                      // TODO:
                    }
                    is(toSU.U(typeLen.W)) {
                      // TODO:
                    }
                  }
                }
              }
               */
            }
          }
          is(1.U) { // bne
            /*
            when(alu.io.zero === 0.U && alu.io.r =/= exInsts(i).pcNext) {
              pcRedirect := alu.io.r
              wbReBranch := true.B
            } .elsewhen(alu.io.zero === 1.U && exInsts(i).pcNext =/= exInsts(i).pc + 4.U) {
              pcRedirect := exInsts(i).pc + 4.U
              wbReBranch := true.B
            }
             */
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


  for(i <- 1 until backendIssueN) {
    when(i.U < exNum) {
      brDelay(0) := Mux(exInsts(i).fuDest === toMDU.U(typeLen.W), true.B, false.B)
      brDelay(1) := Mux(exInsts(i).fuDest === toLU.U(typeLen.W), true.B, false.B)
      brDelay(2) := Mux(exInsts(i).fuDest === toSU.U(typeLen.W), true.B, false.B)
    }
  }

  when(wbFlush) {
    for(i <- 0 until backendIssueN) {
      wbInsts(i) := nop
    }
  }

  /**
   *  [---------- WB stage -----------]
   */


  io.fb.bmfs.redirect_pc := pcRedirect
  io.fb.bmfs.redirect_kill := true.B
  // branch, wait for delay slot
  val pcSlot = Reg(UInt(len.W))
  val waitSlot = RegInit(false.B)
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

  when(waitSlot) {
    waitSlot := false.B
    io.fb.bmfs.redirect_pc := pcSlot
    io.fb.bmfs.redirect_kill := true.B
  }

  when(!dcacheStall) {
    wbInsts := exInsts
  }
  for(i <- 0 until backendIssueN) {
    when(i.U < wbNum) {
      regFile.io.wen_vec(i) := wbInsts(i).regWrite
    } .otherwise {
      regFile.io.wen_vec(i) := false.B
    }
  }
  regFile.io.rd_addr_vec := VecInit(Seq(wbInsts(0).rd, wbInsts(1).rd, wbInsts(2).rd)) // ?
  // handle load-inst separately
  val dataFromDcache = 0.U(32.W) // TODO: connect with dcache
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
}
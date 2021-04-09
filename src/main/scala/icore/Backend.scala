package icore

import chisel3._
import chisel3.util._
import conf.Config
import fu._

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
  val toALU = 0.U(typeLen.W)
  val toMDU = 1.U(typeLen.W)
  val toLU = 2.U(typeLen.W)
  val toSU = 3.U(typeLen.W)
}

class StoreInfo extends Bundle with Config {
  val addr = UInt(len.W)
  val data = UInt(len.W)
}

class Backend extends Module with Config with InstType {
  val io = IO(new BackendIO)
  val queueSize = 20
  val dcacheStall = Bool()
  val issueQueue = Module(new FIFO(queueSize, new InstInfo(), backendIssueN, frontendIssueN))
  issueQueue.io.enqStep := frontendIssueN.U
  issueQueue.io.enqReq := true.B

  // TODO: connect with frontend
  issueQueue.io.din := Vec(frontendIssueN, new InstInfo())

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
  // val toALU :: toMDU :: toLSU :: Nil = Enum(3)
  val exInsts = RegInit(VecInit(Seq.fill(backendIssueN)(new InstInfo()))) // ?
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

  /**
   *  [---------- EX stage -----------]
   */
  val alu = Module(new ALU)
  val wbNum = RegNext(exNum) // ?
  // TODO: MDU and LSU

  val mdu = Module(new MDU())
  val wbResult = RegInit(VecInit(Seq.fill(3)(0.U(len.W))))
  val wbInsts = RegInit(VecInit(Seq.fill(backendIssueN)(new InstInfo()))) // ?

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
//  regFile.io.wen_vec := Vec(3, false.B)
  val rsData = Wire(Vec(3, 0.U(32.W)))
  val rtData = Wire(Vec(3, 0.U(32.W)))
  for(i <- 0 until 3) {
    rsData(i) := regFile.io.rs_data_vec(2 * i)
    rtData(i) := regFile.io.rs_data_vec(2 * i + 1)
  }

  val sqSize = 10
  val storeQueue = Module(new Queue(new StoreInfo, sqSize))
  // TODO: aluSrcB mux(rt.Reg, imm)
  for (i <- 0 until backendIssueN) {
    when(i.U < exNum) {
      switch(exInsts(i).fuDest) {
        is(toALU) {
          // TODO: mux ?
          alu.io.a := MuxLookup(aluSrcA, rsData(i),
            Seq(0.U -> rsData(i), 1.U -> exInsts(i).pc, 2.U -> wbResult(fwdSrcAIndex)))
          alu.io.b := MuxLookup(aluSrcB, rtData(i),
            Seq(0.U -> rtData(i), 1.U -> exInsts(i).imm, 2.U -> wbResult(fwdSrcBIndex)))
          alu.io.aluOp := exInsts(i).aluOp
          wbResult(i) := alu.io.r
        }
        is(toMDU) {
          // TODO
          mdu.io.req.in1 := rsData(i)
          mdu.io.req.in2 := rtData(i)
          mdu.io.req.op := exInsts(i).mduOp
        }
        is(toLU) {
          // TODO: connect with dcache
        }
        is(toSU) {
          // TODO
          storeQueue.io.enq.bits.addr := rsData(i) + Cat(Fill(16, exInsts(i).imm(15)),exInsts(i).imm) // sign-extended
          storeQueue.io.enq.bits.data := rtData(i)
          storeQueue.io.enq.ready := true.B
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

  val pcRedirect = Reg(UInt(32.W))
  val reBranch = RegInit(false.B)
  // process branch
  for(i <- 0 until backendIssueN) {
    when(i.U < exNum) {
      when(exInsts(i).isBranch) {
        switch(exInsts(i).bType) {
          is(0.U) { // beq
            when(alu.io.zero === 1.U && alu.io.r =/= exInsts(i).pcNext) {
              pcRedirect := alu.io.r
              reBranch := true.B
            } .elsewhen(alu.io.zero === 0.U && exInsts(i).pcNext =/= exInsts(i).pc + 4.U) {
              pcRedirect := exInsts(i).pc + 4.U
              reBranch := true.B
            }
          }
          is(1.U) { // bne
            when(alu.io.zero === 0.U && alu.io.r =/= exInsts(i).pcNext) {
              pcRedirect := alu.io.r
              reBranch := true.B
            } .elsewhen(alu.io.zero === 1.U && exInsts(i).pcNext =/= exInsts(i).pc + 4.U) {
              pcRedirect := exInsts(i).pc + 4.U
              reBranch := true.B
            }
          }
        }
      }
    }
  }

  /**
   *  [---------- WB stage -----------]
   */

  io.fb.bmfs.redirect_pc := RegNext(pcRedirect)
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
      when(wbInsts(i).fuDest === toLU) {
        regFile.io.rd_data_vec(i) := dataFromDcache
      }
    } .otherwise {
      regFile.io.rd_data_vec(i) := wbResult(i)
    }
  }

}
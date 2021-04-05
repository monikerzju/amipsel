package icore

import chisel3._
import chisel3.util._
import conf.Config
import fu._

class InstInfo extends Bundle with InstType with Config {
  val rs = UInt(32.W)
  val rt = UInt(32.W)
  val rd = UInt(32.W)
  val aluOp = UInt(aluOpWidth.W)
  val imm = UInt(32.W)
  val fuDest = UInt(typeLen.W)
}

trait InstType {
  val typeLen = 3
  val toALU = 0.U(typeLen.W)
  val toMDU = 1.U(typeLen.W)
  val toLU = 2.U(typeLen.W)
  val toSU = 3.U(typeLen.W)
}

class Backend extends Module with Config with InstType {
  val io = IO(new BackendIO)
  val queueSize = 20

  val issueQueue = Module(new FIFO(queueSize, new InstInfo(), backendIssueN, frontendIssueN))
  issueQueue.io.enqStep := frontendIssueN.U
  issueQueue.io.enqReq := true.B
  issueQueue.io.deqReq := true.B

  // TODO: connect with frontend
  issueQueue.io.din := Vec(frontendIssueN, new InstInfo())

  /* [---------- IS stage -----------] */
  def isDataHazard(inst1: InstInfo, inst2: InstInfo): Bool = {
    inst1.rd === inst2.rs || inst1.rd === inst2.rt
  }
  def isCompatible(inst1: InstInfo, inst2: InstInfo): Bool = {
    !isDataHazard(inst1, inst2) && inst1.fuDest =/= inst2.fuDest
  }
  // TODO: associate the three types with frontend
  /**
   * TODO try with ENUM?
   */
  // val toALU :: toMDU :: toLSU :: Nil = Enum(3)
  val exInsts = RegInit(VecInit(Seq.fill(backendIssueN)(new InstInfo()))) // ?
  val exNum = RegInit(0.U(3.W))
  issueQueue.io.deqStep := exNum
  // decide the true issue num
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
  /*
  switch(issueQueue.io.items) {
    is(1.U) {
      exInsts(0) := issueQueue.io.dout(0)
    }
    is(2.U) { // queue only has two items
      exInsts(0) := issueQueue.io.dout(0) // issue first inst
      when(!isDataHazard(issueQueue.io.dout(0), issueQueue.io.dout(1)) &&
        issueQueue.io.dout(0).fuDest =/= issueQueue.io.dout(1).fuDest
          ) { // do not have data hazard and structural hazard
        exInsts(1) := issueQueue.io.dout(1)
      }
    }
    is(3.U) {
      exInsts(0) := issueQueue.io.dout(0)
      when()
    }
  }
   */
  /* [---------- EX stage -----------] */
  val alu = Module(new ALU)
  val wbNum = RegNext(exNum) // ?
  // TODO: MDU and LSU
  //  val mdu = Module(new MDU())
  val wbResult = RegInit(VecInit(Seq.fill(3)(0.U(len.W))))
  val wbInsts = RegInit(VecInit(Seq.fill(backendIssueN)(new InstInfo()))) // ?
  wbInsts := RegNext(exInsts) // ?

  val aluSrcA = WireDefault(true.B)
  val fwdSrcAIndex = WireInit(0.U)
  val aluSrcB = WireDefault(0.U(2.W))
  val fwdSrcBIndex = WireInit(0.U)

  for (i <- 0 until backendIssueN) {
    when(i.U < exNum) {
      switch(exInsts(i).fuDest) {
        is(toALU) {
          // TODO: mux ?
          alu.io.a := Mux(aluSrcA, exInsts(i).rs, wbInsts(fwdSrcAIndex).rd)
          alu.io.b := exInsts(i).rt
          alu.io.b := MuxLookup(aluSrcB, exInsts(i).rt,
            Seq(0.U -> exInsts(i).rt, 1.U -> exInsts(i).imm, 2.U -> wbInsts(fwdSrcBIndex).rd))
          alu.io.aluOp := exInsts(i).aluOp
          wbResult(0) := alu.io.r
        }
        is(toMDU) {
          // TODO
        }
        is(toLU) {
          // TODO
        }
        is(toSU) {
          // TODO
        }
      }
    }
  }
  // forwarding here?
  for(i <- 0 until backendIssueN) {
    when(i.U < exNum) {
      for(j <- 0 until backendIssueN) {
        when(j.U < wbNum) {
          when(wbInsts(j).fuDest === toALU && exInsts(i).fuDest === toALU && wbInsts(j).rd =/= 0.U) {
            when(wbInsts(j).rd === exInsts(i).rs) {
              aluSrcA := false.B
            }
            when(wbInsts(j).rd === exInsts(i).rt) {
              aluSrcB := 2.U
            }
          }
        }
      }
    }
  }
  /* [---------- WB stage -----------] */

}
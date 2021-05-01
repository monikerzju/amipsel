package fu

import chisel3._
import chisel3.util._
import conf._
import isa.{MicroOpCtrl, Mops}
import icore.InstType

class IAIO(private val iq_size: Int) extends Bundle with Config {
  val insts_in = Input(Vec(backendIssueN, new Mops))
  val queue_items = Input(UInt(log2Ceil(iq_size + 1).W))
  val ld_dest_ex = Input(UInt(log2Ceil(len).W))
  val insts_out = Output(Vec(fuN, new Mops))
  val issue_num = Output(UInt(log2Ceil(fuN + 1).W))
  val issue_fu_valid = Output(Vec(fuN, Bool()))
  val insts_order = Output(Vec(fuN, UInt(log2Ceil(backendIssueN + 1).W)))
}

class IssueArbiter(private val iq_size: Int) extends Module with InstType with Config {

  def isWAW(inst1: Mops, inst2: Mops): Bool = {
    inst1.rd =/= 0.U && inst1.rd === inst2.rd
  }

  def isRAW(inst1: Mops, inst2: Mops): Bool = {
    inst1.rd =/= 0.U && (inst1.rd === inst2.rs1 || inst1.rd === inst2.rs2) ||
      inst1.write_dest === MicroOpCtrl.DHiLo && inst2.src_a === MicroOpCtrl.AHi ||
      inst1.write_dest === MicroOpCtrl.DHiLo && inst2.src_a === MicroOpCtrl.ALo ||
      inst1.write_dest === MicroOpCtrl.DHi && inst2.src_a === MicroOpCtrl.AHi ||
      inst1.write_dest === MicroOpCtrl.DLo && inst2.src_a === MicroOpCtrl.ALo
  }

  def isDataHazard(inst1: Mops, inst2: Mops): Bool = {
    isRAW(inst1, inst2) || isWAW(inst1, inst2)
  }

  def isSimpleCompatible(inst1: Mops, dest: UInt): Bool = {
    dest === 0.U || (inst1.rs1 =/= dest && inst1.rs2 =/= dest)
  }

  def isCompatible(inst1: Mops, inst2: Mops): Bool = {
    !isDataHazard(inst1, inst2) &&
      (inst1.alu_mdu_lsu =/= inst2.alu_mdu_lsu &&
        !(inst1.alu_mdu_lsu === toLU.U && inst2.alu_mdu_lsu === toSU.U ||
          inst1.alu_mdu_lsu === toSU.U && inst2.alu_mdu_lsu === toLU.U) ||
          inst1.alu_mdu_lsu === toAMU.U && inst2.alu_mdu_lsu === toAMU.U)
  }

  def isUglyCompatible(inst1: Mops, inst2: Mops, inst3: Mops): Bool = {
    val isLUorSU = inst3.alu_mdu_lsu === toLU.U || inst3.alu_mdu_lsu === toSU.U
    isCompatible(inst1, inst3) && isCompatible(inst2, inst3) &&
      MuxLookup(
        Cat(inst1.alu_mdu_lsu, inst2.alu_mdu_lsu),
        true.B,
        Seq(
          Cat(toAMU.U(typeLen.W), toAMU.U(typeLen.W)) -> isLUorSU,
          Cat(toAMU.U(typeLen.W), toALU.U(typeLen.W)) -> isLUorSU,
          Cat(toALU.U(typeLen.W), toAMU.U(typeLen.W)) -> isLUorSU,
          Cat(toAMU.U(typeLen.W), toMDU.U(typeLen.W)) -> isLUorSU,
          Cat(toMDU.U(typeLen.W), toAMU.U(typeLen.W)) -> isLUorSU,
          Cat(toALU.U(typeLen.W), toMDU.U(typeLen.W)) -> isLUorSU,
          Cat(toMDU.U(typeLen.W), toALU.U(typeLen.W)) -> isLUorSU
        )
      )
  }
  val io = IO(new IAIO(iq_size))
  // decide the true issue num
  val issue_valid = WireDefault(VecInit(Seq.fill(4)(false.B)))
  io.issue_num := 0.U
  when(io.queue_items > 0.U && isSimpleCompatible(io.insts_in(0), io.ld_dest_ex)) {
    issue_valid(0) := true.B
    io.issue_num := 1.U
    when(io.queue_items > 1.U && isCompatible(io.insts_in(0), io.insts_in(1)) && 
      isSimpleCompatible(io.insts_in(1), io.ld_dest_ex)) {
      // do not have data hazard and structural hazard
      issue_valid(1) := true.B
      io.issue_num := 2.U
      when(io.queue_items > 2.U && isUglyCompatible(io.insts_in(0), io.insts_in(1), io.insts_in(2)) && 
        isSimpleCompatible(io.insts_in(2), io.ld_dest_ex)) {
        issue_valid(2) := true.B
        io.issue_num := 3.U
      }
    }
  }

  // judge the inst type and issue inst into specific inst position
  // which associates with corresponding fu
  val alu_occupy = Wire(Bool())
  val mdu_occupy = Wire(Bool())
  alu_occupy := false.B
  mdu_occupy := false.B
  for(i <- 0 until fuN) {
    io.insts_out(i) := 0.U.asTypeOf(new Mops)
    io.insts_order(i) := 0.U
    io.issue_fu_valid(i) := false.B
  }
  for(i <- 0 until backendIssueN) {
    when(issue_valid(i)) {
      switch(io.insts_in(i).alu_mdu_lsu) {
        // TODO:
        is(toALU.U) {
          io.insts_out(0) := io.insts_in(i)
          io.insts_order(0) := i.U
          io.issue_fu_valid(0) := true.B
          alu_occupy := true.B
        }
        is(toMDU.U) {
          io.insts_out(1) := io.insts_in(i)
          io.insts_order(1) := i.U
          io.issue_fu_valid(1) := true.B
          mdu_occupy := true.B
        }
        is(toLU.U) {
          io.insts_out(2) := io.insts_in(i)
          io.insts_order(2) := i.U
          io.issue_fu_valid(2) := true.B
        }
        is(toSU.U) {
          io.insts_out(3) := io.insts_in(i)
          io.insts_order(3) := i.U
          io.issue_fu_valid(3) := true.B
        }
      }
    }

    for(i <- 0 until backendIssueN) {
      when(issue_valid(i) && io.insts_in(i).alu_mdu_lsu === toAMU.U) {
        when(!alu_occupy) {
          io.insts_out(0) := io.insts_in(i)
          io.insts_order(0) := i.U
          io.issue_fu_valid(0) := true.B
        } .elsewhen(!mdu_occupy) {
          io.insts_out(1) := io.insts_in(i)
          io.insts_order(1) := i.U
          io.issue_fu_valid(1) := true.B
        }
      }
    }
  }
  // process toAMU
  when(issue_valid(0) && issue_valid(1) && io.insts_in(0).alu_mdu_lsu === io.insts_in(1).alu_mdu_lsu) {
    io.insts_out(0) := io.insts_in(0)
    io.insts_order(0) := 0.U
    io.issue_fu_valid(0) := true.B
    io.insts_out(1) := io.insts_in(1)
    io.insts_order(1) := 1.U
    io.issue_fu_valid(1) := true.B
  } .elsewhen(issue_valid(0) && issue_valid(2) && io.insts_in(0).alu_mdu_lsu === io.insts_in(2).alu_mdu_lsu) {
    io.insts_out(0) := io.insts_in(0)
    io.insts_order(0) := 0.U
    io.issue_fu_valid(0) := true.B
    io.insts_out(1) := io.insts_in(2)
    io.insts_order(1) := 2.U
    io.issue_fu_valid(1) := true.B
  } .elsewhen(issue_valid(1) && issue_valid(2) && io.insts_in(1).alu_mdu_lsu === io.insts_in(2).alu_mdu_lsu) {
    io.insts_out(0) := io.insts_in(1)
    io.insts_order(0) := 1.U
    io.issue_fu_valid(0) := true.B
    io.insts_out(1) := io.insts_in(2)
    io.insts_order(1) := 2.U
    io.issue_fu_valid(1) := true.B
  }

}

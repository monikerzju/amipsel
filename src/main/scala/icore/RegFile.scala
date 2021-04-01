package icore

import chisel3._
import chisel3.util._
import chisel3.util.experimental._

class RegFileIO(nregs: Int = 32, len: Int = 32) extends Bundle {
  val rs1_addr = Input(UInt(log2Ceil(nregs).W))
  val rs1_data = Output(UInt(len.W))
  val rs2_addr = Input(UInt(log2Ceil(nregs).W))
  val rs2_data = Output(UInt(len.W))
  val wen      = Input(Bool())
  val rd_addr  = Input(UInt(log2Ceil(nregs).W))
  val rd_data  = Input(UInt(len.W))
}

class RegFile(nregs: Int = 32, len: Int = 32) extends Module {
  val io = IO(new RegFileIO(nregs, len))

  val regs = RegInit(VecInit(Seq.fill(nregs)(0.U(len.W))))
  io.rs1_data := Mux(io.rs1_addr.orR, regs(io.rs1_addr), 0.U)
  io.rs2_data := Mux(io.rs2_addr.orR, regs(io.rs2_addr), 0.U)
  when(io.wen & io.rd_addr.orR) {
    regs(io.rd_addr) := io.rd_data
  }
}
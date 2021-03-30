package fu

import chisel3._
import chisel3.util._
import conf.Config

trait aluOpType {
  val aluOpWidth = 5
  val aluAdd = 0.U(aluOpWidth.W)
  val aluSub = 1.U(aluOpWidth.W)
  val aluSlt = 2.U(aluOpWidth.W)
  val aluSltu = 3.U(aluOpWidth.W)
  val aluXor = 4.U(aluOpWidth.W)
  val aluAnd = 5.U(aluOpWidth.W)
  val aluOr = 6.U(aluOpWidth.W)
  val aluNor = 7.U(aluOpWidth.W)
  val aluSll = 8.U(aluOpWidth.W)
  val aluSrl = 9.U(aluOpWidth.W)
  val aluSra = 10.U(aluOpWidth.W)
}
class ALU extends Module with Config with aluOpType {
  val io = IO(new Bundle {
    val a, b = Input(UInt(len.W))
    val aluOp = Input(UInt()) // maybe configured
    val r = Output(UInt(len.W))
    val zero = Output(UInt(len.W))
  })

  val shamt = io.b & "h0000001f".U
  // cascade mux?
  // maybe choose "switch"
  io.r := MuxLookup(
    io.aluOp,
    "hfefefefe".U,
    Seq(
      aluAdd -> (io.a + io.b),
      aluSub -> (io.a - io.b),
      aluSlt -> Mux(io.a.asSInt() < io.b.asSInt(), 1.U, 0.U),
      aluSltu -> Mux(io.a < io.b, 1.U, 0.U),
      aluXor -> (io.a ^ io.b),
      aluAnd -> (io.a & io.b),
      aluOr -> (io.a | io.b),
      aluNor -> ~(io.a | io.b),
      aluSll -> (io.a >> shamt),
      aluSrl -> (io.a << shamt),
      aluSra -> (io.a.asSInt() >> shamt), // should be tested
    )
  )
  // io.zero := Mux(io.r === 0.U(len.W), 1.U, 0.U)
  io.zero := ~io.r.orR() // zero = 1 means r = 0
}
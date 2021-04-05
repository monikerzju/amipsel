package fu

import chisel3._
import chisel3.util._
import conf.Config

trait AluOpType extends Config {
  val aluAdd = 0.U(aluOpWidth.W)
  val aluAddu = 1.U(aluOpWidth.W)
  val aluSub = 2.U(aluOpWidth.W)
  val aluSlt = 3.U(aluOpWidth.W)
  val aluSltu = 4.U(aluOpWidth.W)
  val aluXor = 5.U(aluOpWidth.W)
  val aluAnd = 6.U(aluOpWidth.W)
  val aluOr = 7.U(aluOpWidth.W)
  val aluNor = 8.U(aluOpWidth.W)
  val aluSll = 9.U(aluOpWidth.W)
  val aluSrl = 10.U(aluOpWidth.W)
  val aluSra = 10.U(aluOpWidth.W)
  val aluLui = 11.U(aluOpWidth.W)
}
class ALU extends Module with Config with AluOpType {
  val io = IO(new Bundle {
    val a, b = Input(UInt(len.W))
    val aluOp = Input(UInt(aluOpWidth.W)) // maybe configured
    val r = Output(UInt(len.W))
    val zero = Output(UInt(len.W))
  })

  val shamt = io.b & "h0000001f".U
  // cascade mux?
  // maybe choose "switch"
  io.r := MuxLookup(
    io.aluOp,
    (io.a + io.b),  // do not add useless logic as ZJV :(
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
      aluLui -> ("hdeadbeef".U)  // TODO
    )
  )
  // io.zero := Mux(io.r === 0.U(len.W), 1.U, 0.U)
  io.zero := ~io.r.orR() // zero = 1 means r = 0
}
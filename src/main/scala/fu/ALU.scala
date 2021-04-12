package fu

import chisel3._
import chisel3.util._
import conf.Config

trait AluOpType{
  val aluOpWidth = 5
  val aluAdd = 0
  val aluAddu = 1
  val aluSub = 2
  val aluSlt = 3
  val aluSltu = 4
  val aluXor = 5
  val aluAnd = 6
  val aluOr = 7
  val aluNor = 8
  val aluSll = 9
  val aluSrl = 10
  val aluSra = 10
  val aluLui = 11
}
class ALU extends Module with Config with AluOpType {
  val io = IO(new Bundle {
    val a, b = Input(UInt(len.W))
    val aluOp = Input(UInt(aluOpWidth.W)) // maybe configured
    val r = Output(UInt(len.W))
    val zero = Output(UInt(len.W))
  })

  val shamt = io.b(4, 0)
  // cascade mux?
  // maybe choose "switch"
  io.r := MuxLookup(
    io.aluOp,
    (io.a + io.b),  // do not add useless logic as ZJV :(
    Seq(
      aluAdd.U -> (io.a + io.b),
      aluSub.U -> (io.a - io.b),
      aluSlt.U -> Mux(io.a.asSInt() < io.b.asSInt(), 1.U, 0.U),
      aluSltu.U -> Mux(io.a < io.b, 1.U, 0.U),
      aluXor.U -> (io.a ^ io.b),
      aluAnd.U -> (io.a & io.b),
      aluOr.U -> (io.a | io.b),
      aluNor.U -> ~(io.a | io.b),
      aluSll.U -> (io.a >> shamt),
      aluSrl.U -> (io.a << shamt),
      aluSra.U -> (io.a.asSInt() >> shamt).asUInt(), // should be tested
      aluLui.U -> ("hdeadbeef".U)  // TODO
    )
  )
  // io.zero := Mux(io.r === 0.U(len.W), 1.U, 0.U)
  io.zero := ~io.r.orR() // zero = 1 means r = 0
}
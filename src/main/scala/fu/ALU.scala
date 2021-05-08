package fu

import chisel3._
import chisel3.util._
import conf.Config
import utils._

trait AluOpType{
  val aluOpWidth = 5
  val aluAdd  = 0
  val aluAddu = 1
  val aluSub  = 2
  val aluSubu = 3
  val aluSlt  = 4
  val aluSltu = 5
  val aluXor  = 6
  val aluAnd  = 7
  val aluOr   = 8
  val aluNor  = 9
  val aluSll  = 10
  val aluSrl  = 11
  val aluSra  = 12
  val aluLui  = 13
}

class ALU extends Module with Config with AluOpType {
  val io = IO(new Bundle {
    val a, b  = Input(UInt(len.W))
    val rega  = Input(UInt(len.W))
    val aluOp = Input(UInt(aluOpWidth.W))
    val r     = Output(UInt(len.W))
    val zero  = Output(UInt(len.W))
    val ovf   = Output(Bool())
  })

  val addrResult = io.rega + io.b
  val subrResult = io.rega - io.b
  val addResult  = io.a + io.b
  val subResult  = io.a - io.b
  val shamt = io.a(4, 0)

  if (useLookupBi) {
    io.r := MuxLookupBi(
      io.aluOp,
      addResult,
      Seq(
        aluSub.U  -> subResult,
        aluSubu.U -> subResult,
        aluSlt.U  -> Mux(io.a.asSInt() < io.b.asSInt(), 1.U, 0.U),
        aluSltu.U -> Mux(io.a < io.b, 1.U, 0.U),
        aluXor.U  -> (io.a ^ io.b),
        aluAnd.U  -> (io.a & io.b),
        aluOr.U   -> (io.a | io.b),
        aluNor.U  -> ~(io.a | io.b),
        aluSll.U  -> (io.b << shamt),
        aluSrl.U  -> (io.b >> shamt),
        aluSra.U  -> (io.b.asSInt() >> shamt).asUInt(),
        aluLui.U  -> Cat(io.b(15, 0), Fill(16, 0.U))
      )
    )
  } else {
    io.r := MuxLookup(
      io.aluOp,
      addResult,
      Seq(
        aluSub.U  -> subResult,
        aluSubu.U -> subResult,
        aluSlt.U  -> Mux(io.a.asSInt() < io.b.asSInt(), 1.U, 0.U),
        aluSltu.U -> Mux(io.a < io.b, 1.U, 0.U),
        aluXor.U  -> (io.a ^ io.b),
        aluAnd.U  -> (io.a & io.b),
        aluOr.U   -> (io.a | io.b),
        aluNor.U  -> ~(io.a | io.b),
        aluSll.U  -> (io.b << shamt),
        aluSrl.U  -> (io.b >> shamt),
        aluSra.U  -> (io.b.asSInt() >> shamt).asUInt(),
        aluLui.U  -> Cat(io.b(15, 0), Fill(16, 0.U))
      )
    )    
  }


  io.zero := ~io.r.orR()
  io.ovf  := Mux(io.aluOp === aluAdd.U, io.rega(len - 1) === io.b(len - 1) && io.rega(len - 1) =/= addrResult(len - 1), 
    Mux(io.aluOp === aluSub.U, io.rega(len - 1) =/= io.b(len - 1) && io.rega(len - 1) =/= subrResult(len - 1), false.B)
  )
}
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
  val aluFilt = 14
}

class ALU extends Module with Config with AluOpType {
  val io = IO(new Bundle {
    val valid = Input(Bool())     // TODO
    val rd    = Input(UInt(5.W))  // TODO
    val rs    = Input(UInt(5.W))  // TODO
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

  val fCnt = RegInit(0.U(3.W))
  val fSum = RegInit(0.U(len.W))
  val f1 = RegInit(0.U(len.W))
  val f2 = RegInit(0.U(len.W))
  val f3 = RegInit(0.U(len.W))
  val f4 = RegInit(0.U(len.W))
  val filtResult = Wire(UInt(len.W))
  val filt = io.valid && io.aluOp === aluFilt.U

  when (filt) {
    when (io.rs === 0.U && io.rd === 0.U) {
      fCnt := 0.U
      filtResult := 0.U
      fSum := 0.U
      f1 := 0.U 
      f2 := 0.U 
      f3 := 0.U 
      f4 := 0.U 
    }.otherwise {
      fSum := fSum + io.a
      when (fCnt === 0.U) { // 1 num
        fCnt := 1.U
        f4 := io.a
        filtResult := 0.U
      }.elsewhen (fCnt === 1.U) { // 2 num
        fCnt := 2.U
        when (f4 < io.a) {
          f4 := io.a
          f3 := f4
        }.otherwise {
          f3 := io.a
        }
        filtResult := 0.U
      }.elsewhen (fCnt === 2.U) { // 3 num
        fCnt := 3.U
        when (f4 < io.a) {
          f2 := f3
          f3 := f4
          f4 := io.a
        }.elsewhen (f3 < io.a) {
          f2 := f3
          f3 := io.a
        }.otherwise {
          f2 := io.a
        }
        filtResult := 0.U
      }.elsewhen (fCnt === 3.U) { // 4 num
        fCnt := 4.U
        when (f4 < io.a) {
          f1 := f2
          f2 := f3
          f3 := f4
          f4 := io.a
        }.elsewhen (f3 < io.a) {
          f1 := f2
          f2 := f3
          f3 := io.a
        }.elsewhen (f2 < io.a) {
          f1 := f2
          f2 := io.a
        }.otherwise {
          f1 := io.a
        }
        filtResult := 0.U
      }.otherwise {
        when (io.a > f4) {
          f3 := f4
          f4 := io.a
          filtResult := fSum - f1 - f2 - f4
        }.elsewhen (io.a > f3) {
          f3 := io.a 
          filtResult := fSum - f1 - f2 - f4
        }.elsewhen (io.a < f1) {
          f1 := io.a
          f2 := f1
          filtResult := fSum - f1 - f3 - f4
        }.elsewhen (io.a < f2) {
          f2 := io.a 
          filtResult := fSum - f1 - f3 - f4
        }.otherwise {
          filtResult := fSum + io.a - f1 - f2 - f3 - f4
        }
        // when (io.a > f1 && io.a > f2 && io.a > f3 && io.a > f4) { // > max
        //   when (f1 >= f2 && f1 >= f3 && f1 >= f4) { // f1 max
        //     f1 := io.a
        //     filtResult := fSum - f2 - f3 - f4
        //   }.elsewhen (f2 >= f1 && f2 >= f3 && f2 >= f4) { // f2 max
        //     f2 := io.a
        //     filtResult := fSum - f1 - f3 - f4
        //   }.elsewhen (f3 >= f1 && f3 >= f2 && f3 >= f4) { // f3 max
        //     f3 := io.a
        //     filtResult := fSum - f1 - f2 - f4
        //   }.otherwise { // f4 max
        //     f4 := io.a
        //     filtResult := fSum - f1 - f2 - f3
        //   }
        // }.elsewhen (io.a < f1 && io.a < f2 && io.a < f3 && io.a < f4) { // < min
        //   when (f1 <= f2 && f1 <= f3 && f1 <= f4) { // f1 min
        //     f1 := io.a
        //     filtResult := fSum - f2 - f3 - f4
        //   }.elsewhen (f2 <= f1 && f2 <= f3 && f2 <= f4) { // f2 min
        //     f2 := io.a
        //     filtResult := fSum - f1 - f3 - f4
        //   }.elsewhen (f3 <= f1 && f3 <= f2 && f3 <= f4) { // f3 min
        //     f3 := io.a
        //     filtResult := fSum - f1 - f2 - f4
        //   }.otherwise { // f4 min
        //     f4 := io.a
        //     filtResult := fSum - f1 - f2 - f3
        //   }
        // }.otherwise { // just add
        //   filtResult := fSum + io.a - f1 - f2 - f3 - f4
        // }
      }
    }
  }.otherwise {
    filtResult := 0.U
  }

  val alu_seq_base = Seq(
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
    aluLui.U  -> Cat(io.b(15, 0), Fill(16, 0.U)),
    aluFilt.U -> filtResult
  )
  val alu_seq_final = alu_seq_base
  if (useLookupBi) {
    io.r := MuxLookupBi(
      io.aluOp(aluOpWidth - 2, 0),
      addResult,
      alu_seq_final
    )
  } else {
    io.r := MuxLookup(
      io.aluOp(aluOpWidth - 2, 0),
      addResult,
      alu_seq_final
    )    
  }


  io.zero := ~io.r.orR()
  io.ovf  := Mux(io.aluOp === aluAdd.U, io.rega(len - 1) === io.b(len - 1) && io.rega(len - 1) =/= addrResult(len - 1), 
    Mux(io.aluOp === aluSub.U, io.rega(len - 1) =/= io.b(len - 1) && io.rega(len - 1) =/= subrResult(len - 1), false.B)
  )
}
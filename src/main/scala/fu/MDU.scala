package fu

import chisel3._
import chisel3.util._

trait MDUOperation extends AluOpType {
  val SZ_MDU_OP = aluOpWidth // same with ALU
  val MDU_MUL = 16
  val MDU_DIV = 17
  val MDU_MULU = 18
  val MDU_DIVU = 19
  // Some other operations
}

class MDUReq(width: Int = 32) extends Bundle with MDUOperation {
  val op = Input(Bits(SZ_MDU_OP.W))
  val in1 = Input(Bits(width.W))
  val in2 = Input(Bits(width.W))
}

class MDUResp(width: Int = 32) extends Bundle {
  val hi = Output(Bits(width.W))
  val lo = Output(Bits(width.W))
  val except = Output(Bits(1.W))
}

class MDUIO(width: Int = 32) extends Bundle {
  val req = new MDUReq(width)
  val resp = new MDUResp(width)
//  val kill = Input(Bool())
  override def cloneType = (new MDUIO(width)).asInstanceOf[this.type]
}

class MDU(width: Int = 32) extends Module with MDUOperation {
  val io = IO(new MDUIO(width))

  /*
  val (lo, hi) = MuxLookup(
    io.req.op,
    (0.U(32.W), 0.U(32.W)),
    Seq(
      MDU_MUL -> ((io.req.in1.asSInt() * io.req.in2.asSInt())(0, 31).asUInt(), (io.req.in1.asSInt() * io.req.in2.asSInt())(32, 63).asUInt()),
      MDU_MULU -> ((io.req.in1.asUInt() * io.req.in2.asUInt())(0, 31).asUInt(), (io.req.in1.asUInt() * io.req.in2.asUInt())(32, 63).asUInt()),
      MDU_DIV -> ((io.req.in1.asSInt() / io.req.in2.asSInt()).asUInt(), (io.req.in1.asSInt() % io.req.in2.asSInt()).asUInt()),
      MDU_DIVU -> ((io.req.in1.asUInt() / io.req.in2.asUInt()).asUInt(), (io.req.in1.asUInt() % io.req.in2.asUInt()).asUInt())
    )
  )
   */
  val shamt = io.req.in2(4, 0)
  val lo = MuxLookup(
    io.req.op,
    io.req.in1 + io.req.in2,
    Seq(
      MDU_MUL.U -> (io.req.in1.asSInt() * io.req.in2.asSInt())(31, 0).asUInt(),
      MDU_MULU.U -> (io.req.in1.asUInt() * io.req.in2.asUInt())(31, 0),
      MDU_DIV.U -> (io.req.in1.asSInt() / io.req.in2.asSInt()).asUInt(),
      MDU_DIVU.U -> (io.req.in1.asUInt() / io.req.in2.asUInt()),

      aluAdd.U -> (io.req.in1 + io.req.in2),
      aluSub.U -> (io.req.in1 - io.req.in2),
      aluSlt.U -> Mux(io.req.in1.asSInt() < io.req.in2.asSInt(), 1.U, 0.U),
      aluSltu.U -> Mux(io.req.in1 < io.req.in2, 1.U, 0.U),
      aluXor.U -> (io.req.in1 ^ io.req.in2),
      aluAnd.U -> (io.req.in1 & io.req.in2),
      aluOr.U -> (io.req.in1 | io.req.in2),
      aluNor.U -> ~(io.req.in1 | io.req.in2),
      aluSll.U -> (io.req.in1 >> shamt),
      aluSrl.U -> (io.req.in1 << shamt),
      aluSra.U -> (io.req.in1.asSInt() >> shamt).asUInt(), // should be tested
      aluLui.U -> Cat(io.req.in2(15, 0), Fill(16, 0.U))  // TODO:check imm
    )
  )

  val hi = MuxLookup(
    io.req.op,
    0.U(32.W),
    Seq(
      MDU_MUL.U -> (io.req.in1.asSInt() * io.req.in2.asSInt())(63, 32).asUInt(),
      MDU_MULU.U -> (io.req.in1.asUInt() * io.req.in2.asUInt())(63, 32),
      MDU_DIV.U -> (io.req.in1.asSInt() % io.req.in2.asSInt()).asUInt(),
      MDU_DIVU.U -> (io.req.in1.asUInt() % io.req.in2.asUInt())
    )
  )

  io.resp.lo := lo
  io.resp.hi := hi

  io.resp.except := (io.req.op === MDU_DIV.U || io.req.op === MDU_DIVU.U) && io.req.in2.asUInt === 0.U
}
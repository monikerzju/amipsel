package fu

import chisel3._
import chisel3.util._

trait MDUOperation extends AluOpType {
  val SZ_MDU_OP = aluOpWidth // same with ALU
  val MDU_MUL = 0.U(SZ_MDU_OP.W)
  val MDU_DIV = 1.U(SZ_MDU_OP.W)
  val MDU_MULU = 2.U(SZ_MDU_OP.W)
  val MDU_DIVU = 3.U(SZ_MDU_OP.W)
  // Some other operations
}

class MDUReq(width: Int = 32) extends Bundle with MDUOperation {
  val op = Bits(SZ_MDU_OP.W)
  val in1 = Bits(width.W)
  val in2 = Bits(width.W)
}

class MDUResp(width: Int = 32) extends Bundle {
  val hi = Bits(width.W)
  val lo = Bits(width.W)
  val except = Bits(1.W)
}

class MDUIO(width: Int = 32) extends Bundle {
  val req = Input(new MDUReq(width))
  val resp = Output(new MDUResp(width))
  val kill = Input(Bool())
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

  val lo = MuxLookup(
    io.req.op,
    0.U(32.W),
    Seq(
      MDU_MUL -> (io.req.in1.asSInt() * io.req.in2.asSInt())(31, 0),
      MDU_MULU -> (io.req.in1.asUInt() * io.req.in2.asUInt())(31, 0),
      MDU_DIV -> (io.req.in1.asSInt() / io.req.in2.asSInt()),
      MDU_DIVU -> (io.req.in1.asUInt() / io.req.in2.asUInt())
    )
  )

  val hi = MuxLookup(
    io.req.op,
    0.U(32.W),
    Seq(
      MDU_MUL -> (io.req.in1.asSInt() * io.req.in2.asSInt())(63, 32),
      MDU_MULU -> (io.req.in1.asUInt() * io.req.in2.asUInt())(63, 32),
      MDU_DIV -> (io.req.in1.asSInt() % io.req.in2.asSInt()),
      MDU_DIVU -> (io.req.in1.asUInt() % io.req.in2.asUInt())
    )
  )

  io.resp.lo := lo
  io.resp.hi := hi
}
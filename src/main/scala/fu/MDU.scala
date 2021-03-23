package fu

import chisel3._

object MDU {
  val SZ_MDU_OP = 1
  val MDU_MUL = 0.U(SZ_MDU_OP.W)
  val MDU_DIV = 1.U(SZ_MDU_OP.W)
  // Some other operations
}

class MDUReq(width: Int = 32) extends Bundle {
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
  val req = Flipped(Decoupled(new MDUReq(width)))
  val resp = Decoupled(new MDUResp(width))
  val kill = Input(Bool())
}

class MDU(width: Int = 32) extends Module {
  val io = IO(new MDUIO(width))
}
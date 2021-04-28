package fu

import chisel3._
import chisel3.util._

trait MDUOperation extends AluOpType {
  val SZ_MDU_OP = aluOpWidth
  val MDU_MUL  = 16
  val MDU_DIV  = 17
  val MDU_MULU = 18
  val MDU_DIVU = 19
}

class MDUReq(width: Int = 32) extends Bundle with MDUOperation {
  val op    = Input(Bits(SZ_MDU_OP.W))
  val in1   = Input(Bits(width.W))
  val in2   = Input(Bits(width.W))
}

class MDUResp(width: Int = 32) extends Bundle {
  val hi     = Output(Bits(width.W))
  val lo     = Output(Bits(width.W))
  val except = Output(Bits(1.W))
  val valid  = Output(Bool())
}

class MDUIO(width: Int = 32) extends Bundle {
  val req  = new MDUReq(width)
  val resp = new MDUResp(width)
  // TODO ADD val kill = Input(Bool())
  override def cloneType = (new MDUIO(width)).asInstanceOf[this.type]
}

class MDU(width: Int = 32) extends Module with MDUOperation {
  val io = IO(new MDUIO(width))

  val mul_res = io.req.in1.asSInt * io.req.in2.asSInt
  val mulu_res = io.req.in1.asUInt * io.req.in2.asUInt
  val diving = io.req.op === MDU_DIV.U || io.req.op === MDU_DIVU.U

  val divider = Module(new Div32)
  divider.io.vi   := diving
  divider.io.kill := false.B // TODO ADD io.kill
  divider.io.in1  := io.req.in1
  divider.io.in2  := io.req.in2
  divider.io.sign := io.req.op === MDU_DIV.U

  val shamt = io.req.in1(4, 0)
  val lo = Mux(
    io.req.op(4).andR,
    MuxLookup(
      io.req.op,
      divider.io.div_res,
      Seq(
        MDU_MUL.U  -> mul_res(31, 0).asUInt,
        MDU_MULU.U -> mulu_res(31, 0)
      )
    ),
    MuxLookup(
      io.req.op,
      io.req.in1 + io.req.in2,
      Seq(
        aluSub.U   -> (io.req.in1 - io.req.in2),
        aluSlt.U   -> Mux(io.req.in1.asSInt() < io.req.in2.asSInt(), 1.U, 0.U),
        aluSltu.U  -> Mux(io.req.in1 < io.req.in2, 1.U, 0.U),
        aluXor.U   -> (io.req.in1 ^ io.req.in2),
        aluAnd.U   -> (io.req.in1 & io.req.in2),
        aluOr.U    -> (io.req.in1 | io.req.in2),
        aluNor.U   -> ~(io.req.in1 | io.req.in2),
        aluSll.U   -> (io.req.in2 << shamt),
        aluSrl.U   -> (io.req.in2 >> shamt),
        aluSra.U   -> (io.req.in2.asSInt() >> shamt).asUInt(),
        aluLui.U   -> Cat(io.req.in2(15, 0), Fill(16, 0.U))
      )
    )
  )

  val hi = MuxLookup(
    io.req.op,
    divider.io.rem_res,
    Seq(
      MDU_MULU.U -> mulu_res(2 * width - 1, width),
      MDU_MUL.U  -> mul_res(2 * width - 1, width)
    )
  )

  io.resp.lo := lo
  io.resp.hi := hi

  io.resp.except := diving && io.req.in2.asUInt === 0.U
  io.resp.valid  := divider.io.vo
}

class Div32 extends Module {
  val io = IO(new Bundle {
    val vi      = Input(Bool())
    val sign    = Input(Bool())
    val in1     = Input(UInt(32.W))
    val in2     = Input(UInt(32.W))
    val kill    = Input(Bool())
    val vo      = Output(Bool())
    val div_res = Output(UInt(32.W))
    val rem_res = Output(UInt(32.W))
  })

  // num nonz = 4
  // bit  | 8   | 8    | 8    | 8
  // step | s0  | s1   | s2   | s3   | s0
  // stat | id  | ca   | ca   | ca   | id

  val isz = Wire(Vec(32 / 8 - 1, Bool()))
  val num_nonz = Wire(UInt(log2Ceil(32 / 8 + 1).W))
  val s_idle :: s_calc :: Nil = Enum(2)
  val state  = RegInit(s_idle)
  val nstate = WireDefault(s_idle)
  val step   = RegInit(0.U(log2Ceil(32 / 8).W))
  state := nstate

  val remr  = Reg(UInt((2 * 32).W))
  val remsa = Wire(Vec(8, UInt((2 * 32).W)))
  val rems  = Wire(Vec(8, UInt((2 * 32).W)))

  when (io.kill) {
    nstate := s_idle
  }.otherwise {
    when (state === s_idle) {
      nstate := Mux(io.vi && num_nonz === 1.U, s_calc, s_idle)
      remr := Cat(Fill(32, 0.U), io.in1)
    }.otherwise {
      nstate := Mux(step === num_nonz - 1.U, s_idle, s_calc)
      step := Mux(step === num_nonz - 1.U, 0.U, step + 1.U)
      remr := rems(7)
    }
  }

  for (i <- 32 / 8 - 1 to 1 by -1) {
    isz(i - 1) := !io.in1(8 * i + 7, 8 * i).orR
  }

  num_nonz := Mux(
    isz(2),
    Mux(
      isz(1),
      Mux(isz(0), 1.U, 2.U),
      3.U
    ),
    4.U
  )

  val true_rem = Mux(step === 0.U, io.in1 << 1.U, remr)
  remsa(0) := true_rem - io.in2
  rems(0)  := Mux(remsa(0).asSInt < 0.S, true_rem << 1.U, Cat(true_rem(32 - 2, 0), 1.U))
  for (i <- 1 until 8) {
    remsa(i) := rems(i - 1) - io.in2
    rems(i)  := Mux(remsa(i).asSInt < 0.S, rems(i - 1) << 1.U, Cat(rems(i - 1)(32 - 2, 0), 1.U))
  }

  io.vo := io.vi && step === num_nonz - 1.U
  io.div_res := rems(7)(32 - 1, 0)
  io.rem_res := rems(7)(32 * 2 - 1, 32) >> 1.U
}
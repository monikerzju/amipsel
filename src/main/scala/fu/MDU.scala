package fu

import chisel3._
import chisel3.util._

import conf.Config

trait MDUOperation extends AluOpType {
  val SZ_MDU_OP = aluOpWidth
  val MDU_MUL  = 16
  val MDU_DIV  = 17
  val MDU_MULU = 18
  val MDU_DIVU = 19
}

class MDUReq(width: Int = 32) extends Bundle with MDUOperation {
  val valid = Input(Bool())
  val op    = Input(Bits(SZ_MDU_OP.W))
  val in1   = Input(Bits(width.W))
  val in2   = Input(Bits(width.W))
  val reg1  = Input(Bits(width.W))
}

class MDUResp(width: Int = 32) extends Bundle {
  val hi     = Output(Bits(width.W))
  val lo     = Output(Bits(width.W))
  val except = Output(Bool())
  val valid  = Output(Bool())
}

class MDUIO(width: Int = 32) extends Bundle {
  val req  = new MDUReq(width)
  val resp = new MDUResp(width)
  override def cloneType = (new MDUIO(width)).asInstanceOf[this.type]
}

class MDU(width: Int = 32) extends Module with MDUOperation {
  val io = IO(new MDUIO(width))

  val mul_res = RegNext(io.req.in1.asSInt * io.req.in2.asSInt)
  val mulu_res = RegNext(io.req.in1.asUInt * io.req.in2.asUInt)
  val diving = (io.req.op === MDU_DIV.U || io.req.op === MDU_DIVU.U) && io.req.valid
  val multing = (io.req.op === MDU_MUL.U || io.req.op === MDU_MULU.U) && io.req.valid
  val last_valid = RegInit(false.B)

  val divider = Module(new Div32)
  val sign1   = io.req.in1(width - 1)
  val sign2   = io.req.in2(width - 1)
  divider.io.vi   := diving
  divider.io.in1  := Mux(io.req.op === MDU_DIV.U && sign1.asBool, ((~io.req.in1).asUInt + 1.U), io.req.in1)
  divider.io.in2  := Mux(io.req.op === MDU_DIV.U && sign2.asBool, ((~io.req.in2).asUInt + 1.U), io.req.in2)

  val shamt = io.req.in1(4, 0)
  val addrResult = io.req.reg1 + io.req.in2
  val subrResult = io.req.reg1 - io.req.in2
  val addResult  = io.req.in1 + io.req.in2
  val subResult  = io.req.in1 - io.req.in2
  val lo = Mux(
    io.req.op(4).andR,
    MuxLookup(
      io.req.op,
      divider.io.div_res,
      Seq(
        MDU_DIV.U  -> Mux(sign1 === sign2, divider.io.div_res, ((~divider.io.div_res).asUInt + 1.U)),
        MDU_MUL.U  -> mul_res(31, 0).asUInt,
        MDU_MULU.U -> mulu_res(31, 0)
      )
    ),
    MuxLookup(
      io.req.op,
      addResult,
      Seq(
        aluSub.U   -> subResult,
        aluSubu.U  -> subResult,
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
      MDU_DIV.U  -> Mux(sign1 === 0.U, divider.io.rem_res, ((~divider.io.rem_res).asUInt + 1.U)),
      MDU_MULU.U -> mulu_res(2 * width - 1, width),
      MDU_MUL.U  -> mul_res(2 * width - 1, width)
    )
  )

  io.resp.lo := lo
  io.resp.hi := hi

  io.resp.except := Mux(io.req.op === aluAdd.U, 
    io.req.reg1(width - 1) === io.req.in2(width - 1) && io.req.reg1(width - 1) =/= addrResult(width - 1), 
    Mux(io.req.op === aluSub.U, 
      io.req.reg1(width - 1) =/= io.req.in2(width - 1) && io.req.reg1(width - 1) =/= subrResult(width - 1), false.B)
  )
  last_valid := io.resp.valid
  io.resp.valid  := Mux(diving, divider.io.vo, Mux(multing, !last_valid && RegNext(multing), true.B))
}

class Div32 extends Module with Config {
  val io = IO(new Bundle {
    val vi      = Input(Bool())
    val in1     = Input(UInt(32.W))
    val in2     = Input(UInt(32.W))
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
  val s_idle :: s_calc :: s_fin :: Nil = Enum(3)
  val state  = RegInit(s_idle)
  val nstate = WireDefault(s_idle)
  val step   = RegInit(0.U((log2Ceil(32 / 2) + 1).W))
  state := nstate

  val remr      = Reg(UInt((2 * 32 + 1).W))
  val rems      = Wire(Vec(2, UInt((2 * 32 + 1).W)))

  when (state === s_idle) {
    nstate := Mux(io.vi, s_calc, s_idle)
  }.elsewhen(state === s_calc) {
    nstate := Mux(step === 8.U * num_nonz - 1.U, s_fin, s_calc)
    step := step + 1.U
    remr := rems(0)
  }.otherwise {
    step := 0.U
    nstate := s_idle
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

  val true_rem = Wire(UInt((2 * 32 + 1).W))
  true_rem := Mux(step === 0.U, io.in1 << (1.U + 8.U * (4.U - num_nonz)), remr)
  rems(0)  := Mux(true_rem(63, 32) < io.in2, 
    true_rem << 1.U, 
    Cat((true_rem(63, 32) - io.in2)(30, 0), true_rem(31, 0), 1.U)
  )
  rems(1) := 0.U

  io.vo      := state === s_fin

  io.div_res := RegNext(rems(0)(32 - 1, 0))
  io.rem_res := RegNext(rems(0)(32 * 2, 32) >> 1.U)
}
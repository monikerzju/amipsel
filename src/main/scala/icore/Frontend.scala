package icore

import chisel3._
import chisel3.util._
import conf.Config
import isa._

// PCGen is can be non-project-specific, no with Config
class PCGenIO(va_width: Int = 32) extends Bundle {
  val please_wait = Input(Bool()) // maybe from: A. a full-fifo and not killed; B. icache fetching from memory 
  val redirect = Input(Bool())
  val redirect_pc = Input(UInt(va_width.W))
  val pc_o = Output(UInt(va_width.W))
  override def cloneType = (new PCGenIO(va_width)).asInstanceOf[this.type]
}

class PCGen(va_width: Int = 32, start_va: String = "h80000000", increment: Int = 4) extends Module {
  val io = IO(new PCGenIO(va_width))
  val pc = RegInit(UInt(va_width.W), start_va.U)
  pc := Mux(io.redirect, io.redirect_pc, Mux(io.please_wait, pc, pc + increment.U))
  io.pc_o := pc
}

class Frontend extends Module with Config with MemAccessType with FrontToBack {
  val io = IO(new FrontendIO)

  // IF
  val pc_gen         = Module(new PCGen(len, startAddr, 4 * frontendIssueN))
  val cache_stall    = Wire(Bool())
  val fetch_half     = Wire(Bool())
  val last_req_valid = RegInit(false.B)
  val repc           = RegInit(startAddr.U(len.W))
  val stall_f        = Wire(Bool())
  val kill_f         = Wire(Bool())

  // ID
  val decode_pc_low_reg = RegInit(UInt(len.W), startAddr.U)
  val decode_valid_reg  = RegInit(false.B)
  val decs              = Array.fill(frontendIssueN)(Module(new Dec).io)
  val stall_d           = Wire(Bool())
  val kill_d            = Wire(Bool())

  // IF Stage
  stall_f        := stall_d || cache_stall
  kill_f         := kill_d
  fetch_half     := io.icache.resp.bits.respn === 0.U
  cache_stall    := (if (metaZeroLatency) RegNext(!io.icache.resp.valid) else !io.icache.resp.valid) && last_req_valid
  last_req_valid := io.icache.req.valid

  pc_gen.io.please_wait := stall_f
  pc_gen.io.redirect    := kill_f || (fetch_half && !cache_stall)
  pc_gen.io.redirect_pc := Mux(kill_f, io.fb.bmfs.redirect_pc, pc_gen.io.pc_o + 4.U)

  repc := Mux(stall_f, repc, pc_gen.io.pc_o)

  io.icache.req.valid      := true.B
  io.icache.resp.ready     := true.B 
  io.icache.req.bits.addr  := Mux(stall_f, repc, pc_gen.io.pc_o)
  io.icache.req.bits.wdata := DontCare
  io.icache.req.bits.wen   := false.B
  if (frontendIssueN == 1) {
    io.icache.req.bits.mtype := MEM_WORD.U
  } else {
    io.icache.req.bits.mtype := MEM_DWORD.U
  }
  io.icache.req.bits.flush := false.B
  io.icache.req.bits.invalidate := false.B

  // ID Stage
  stall_d := io.fb.fmbs.please_wait
  kill_d  := io.fb.bmfs.redirect_kill

  when (kill_d) {
    decode_valid_reg  := false.B
  }.elsewhen (!stall_d && !cache_stall) {
    decode_pc_low_reg := io.icache.req.bits.addr
    decode_valid_reg  := io.icache.req.valid
  }

  io.fb.fmbs.instn := Mux(stall_d, 0.U,
    Mux(!cache_stall && decode_valid_reg, Cat(0.U, RegNext(io.icache.resp.bits.respn)) + 1.U, 0.U)
  )
  for (i <- 0 until frontendIssueN) {
    decs(i).inst           := io.icache.resp.bits.rdata(i)
    decs(i).pc             := decode_pc_low_reg + (i.U << 2.U)
    io.fb.fmbs.inst_ops(i) := decs(i).mops.asUInt
  }

}
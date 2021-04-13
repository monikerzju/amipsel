package fu

import chisel3._
import chisel3.util._

class FIFOIO[T <: Data](size: Int, private val gen: T, readN: Int, enqN: Int) extends Bundle {
  val dout = Output(Vec(readN, gen))
  val enqStep = Input(UInt(3.W)) //TODO const should be altered
  val enqReq = Input(Bool())
  val deqStep = Input(UInt(3.W)) //TODO const should be altered
  val deqReq = Input(Bool())
  val din = Input(Vec(enqN, gen)) //TODO const should be altered
  val flush = Input(Bool())
  val sufficient = Output(Bool())
  val items = Output(UInt(log2Ceil(readN).W)) // queue item num
  override def cloneType = (new FIFOIO(size, gen, readN, enqN)).asInstanceOf[this.type]
}

class FIFO[T <: Data](size: Int, gen: T, readN: Int, enqN: Int) extends Module {
  val io = IO(new FIFOIO(size, gen, readN, enqN))
  val mem = Reg(Vec(size, gen))

  def counter(incr: Bool, step: UInt): UInt = {
    val cntReg = RegInit(0.U(log2Ceil(size).W))
    when(incr) {
      cntReg := Mux(cntReg === (size - 1).U, 0.U, cntReg + step)
    }
    cntReg
  }
  val maybe_full = RegInit(false.B) // help judge whether queue is empty or full

  val do_enq = WireInit(false.B)
  val do_deq = WireInit(false.B)
  val enqPtr = counter(do_enq, io.enqStep)
  val deqPtr = counter(do_deq, io.deqStep)
  val ptr_match = enqPtr === deqPtr
  val empty = ptr_match && !maybe_full | io.flush
  val full = ptr_match && maybe_full && !io.flush
  do_enq := io.enqReq && io.sufficient
  do_deq := io.deqReq && !empty
  io.items := (deqPtr + size.U - enqPtr) % size.U
  io.sufficient := !full && (enqN.U <= size.U - io.items)

  when(do_enq) {
    for(i <- 0 until enqN) {
      mem(enqPtr + i.U) := io.din(i)
    }
  }

  when(do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  for(i <- 0 until 3) { //TODO const should be altered
    io.dout(i) := mem(deqPtr + i.U)
  }

  when(io.flush) {
    enqPtr := 0.U
    deqPtr := 0.U
  }
}
package tile

import chisel3._
import chisel3.util._

class CacheReq(bit_cacheline: Int = 128, width: Int = 32) extends Bundle {
  val valid = Input(Bool())
  val wen   = Input(Bool())
  val addr  = Input(UInt(width.W))
  val data  = Input(UInt(bit_cacheline.W))
}

class CacheResp(bit_cacheline: Int = 128) extends Bundle {
  val valid = Output(Bool())
  val data  = Output(UInt(bit_cacheline.W))
}

// From the view of Cache
class CacheIO(bit_cacheline: Int = 128, width: Int = 32) extends Bundle {
  val req = Flipped(new CacheReq(bit_cacheline, width))
  val resp = Flipped(new CacheResp(bit_cacheline))
}

class AChannel extends Bundle {
  val addr  = Output()
}

class RChannel extends Bundle {

}

class WChannel extends Bundle {

}

class BChannel extends Bundle {

}

// From the AXI3 Server's view
class AXI3 extends Bundle {
  val ar = Decoupled(new AChannel)
  val r  = Flipped(Decoupled(new RChannel))
  val aw = Decoupled(new AChannel)
  val w  = Decoupled(new WChannel)
  val b  = Flipped(Decoupled(new BChannel))
}

class AXI3ServerIO(nclient: Int = 2, bit_cacheline: Int = 128) extends Bundle {
  val cache = Vec(nclient, Flipped(new CacheIO(bit_cacheline)))
  val axi3  = new AXI3
}

class AXI3Server(nclient: Int = 2, bit_cacheline: Int = 128, policy: String = "RR") extends Module {
  val io = IO(new AXI3ServerIO(nclient))

  val rbuff = Module(new )  // Queue
  val wptr = RegInit(0.U(log2Ceil(bit_cacheline / 8).W))
  val wbuff = // RegNext

  val rs_idle :: rs_wait_ready :: rs_receive :: rs_finish :: Nil = Enum(4)
  val ws_idle :: ws_wait_ready :: ws_write :: ws_wait_valid :: Nil = Enum(4)

  val rstate = RegInit(rs_idle)
  val wstate = RegInit(ws_idle)
  val next_rstate = WireDefault(rstate)
  val next_wstate = WireDefault(wstate)
  rstate := next_rstate
  wstate := next_wstate

  val rsel = RegInit((nclient - 1).U(log2Ceil(nclient).W))
  val wsel = RegInit((nclient - 1).U(log2Ceil(nclient).W))

  val ren = // orR
  val wen = // orR
  
  for (i <- 0 until nclient) {
    io.cache(rsel) := rstate === rs_finish
    io.cache(wsel) := wstate === ws_wait_valid
  }

  // Selector

  // AXI3 Read Channel
  io.axi3.ar.valid     := false.B
  io.axi3.ar.bits.addr := RegNext(io.cache(rsel).addr)
  io.axi3.r.ready      := false.B
  switch (rstate) {
    is (rs_idle) {
      next_rstate := Mux(ren, rs_wait_ready, rstate)
    }
    is (rs_wait_ready) {
      rbuff.clear()
      io.axi3.ar.valid := true.B
      next_rstate := Mux(io.axi3.ar.ready, rs_receive, rstate)
    }
    is (rs_receive) {
      rbuff.enqueue(io.axi3.r.bits.data)
      io.axi3.r.ready := io.axi3.r.valid
      next_rstate := Mux(io.axi3.r.valid && io.axi3.r.bits.last, rs_finish, rstate)
    }
    is (rs_finish) {
      next_rstate := rs_idle
    }
  }

  // AXI3 Write Channel
  io.axi3.aw.valid     := false.B
  io.axi3.aw.bits.addr := RegNext(io.cache(wsel).addr)
  io.axi3.w.bits.data  := wbuff(wptr)
  io.axi3.w.bits.last  := 
  switch (wstate) {
    is (ws_idle) {
      next_wstate := Mux(wen, ws_wait_ready, wstate)
    }
    is (ws_wait_ready) {
      wptr := 0.U
      io.axi3.aw.valid := true.B
      next_wstate := Mux(io.axi3.aw.ready, ws_write, wstate)
    }
    is (ws_write) {
      io.axi3.w.valid := true.B
      wptr := wptr + Mux(io.axi3.w.ready, 1.U, 0.U)
      next_wstate := Mux(io.axi3.w.bits.last, ws_wait_valid, wstate)
    }
    is (ws_wait_valid) {
      next_wstate := Mux(io.axi3.b.valid, ws_idle, wstate)
    }
  }
}

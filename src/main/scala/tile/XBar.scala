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
  val req   = Flipped(new CacheReq(bit_cacheline, width))
  val resp  = Flipped(new CacheResp(bit_cacheline))
}

class AChannel(id_width: Int = 1, width: Int = 32) extends Bundle {
  val id    = Output(UInt(id_width.W))
  val addr  = Output(UInt(width.W))
  val len   = Output(UInt(4.W))
  val size  = Output(UInt(3.W))
  val burst = Output(UInt(2.W))
  val lock  = Output(UInt(1.W))
  val cache = Output(UInt(4.W))
  val prot  = Output(UInt(3.W))
}

class RChannel(id_width: Int = 1, width: Int = 32) extends Bundle {
  val id    = Output(UInt(id_width.W))
  val data  = Output(UInt(width.W))
  val resp  = Output(UInt(2.W))
  val last  = Output(UInt(1.W))
}

class WChannel(id_width: Int = 1, width: Int = 32) extends Bundle {
  val id    = Output(UInt(id_width.W))
  val data  = Output(UInt(width.W))
  val strb  = Output(UInt(4.W))
  val last  = Output(UInt(1.W))
}

class BChannel(id_width: Int = 1) extends Bundle {
  val id    = Output(UInt(id_width.W))
  val resp  = Output(UInt(2.W))
}

// From the AXI3 Server's view
class AXI3(id_width: Int = 1, width: Int = 32) extends Bundle {
  val ar = Decoupled(new AChannel(id_width, width))
  val r  = Flipped(Decoupled(new RChannel(id_width, width)))
  val aw = Decoupled(new AChannel(id_width, width))
  val w  = Decoupled(new WChannel(id_width, width))
  val b  = Flipped(Decoupled(new BChannel(id_width)))
}

class RBuff(bit_cacheline: Int = 128) {
  val buffer = Reg(Vec(bit_cacheline / 32, UInt(32.W)))
  val rptr   = RegInit(0.U(log2Ceil(bit_cacheline / 32).W))

  def clear(): Unit = {
    for (i <- 0 until bit_cacheline / 32) {
      buffer(i) := 0.U
    }
    rptr := 0.U
  }

  def enqueue(data: UInt): Unit = {
    buffer(rptr) := data
    rptr := rptr + 1.U
  }
  
  def getl(): UInt = {
    buffer.asUInt
  }
}

class AXI3ServerIO(nclient: Int = 2, bit_cacheline: Int = 128, id_width: Int = 1) extends Bundle {
  val cache = Vec(nclient, Flipped(new CacheIO(bit_cacheline)))
  val axi3  = new AXI3(id_width)
}

class AXI3Server(nclient: Int = 2, bit_cacheline: Int = 128, policy: String = "RR") extends Module {
  val io = IO(new AXI3ServerIO(nclient))

  assert(bit_cacheline <= 256)  
  val INCR: String = "b01"

  val rbuff = new RBuff(bit_cacheline)
  val wptr = RegInit(0.U(log2Ceil(bit_cacheline / 8).W))
  val wbuff = RegNext(io.cache(wsel).req.data)

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

  val ren = WireDefault(false.B)
  val wen = WireDefault(false.B)
  
  for (i <- 0 until nclient) {
    io.cache(i.U).resp.valid := Mux(i.U === rsel, rstate === rs_finish, Mux(i.U === wsel, wstate === ws_wait_valid, false.B))
    io.cache(i.U).resp.data  := rbuff.getl()
  }

  // Selector

  // AXI3 Read Channel
  io.axi3.ar.bits.id    := 0.U
  io.axi3.ar.bits.addr  := RegNext(io.cache(rsel).req.addr)
  io.axi3.ar.bits.len   := (bit_cacheline / 32).U
  io.axi3.ar.bits.size  := "b10".U
  io.axi3.ar.bits.burst := INCR.U
  io.axi3.ar.bits.lock  := 0.U
  io.axi3.ar.bits.cache := 0.U
  io.axi3.ar.bits.prot  := 0.U
  io.axi3.ar.valid      := false.B
  io.axi3.r.ready       := false.B
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
      io.axi3.r.ready := true.B
      next_rstate := Mux(io.axi3.r.valid && io.axi3.r.bits.last.orR, rs_finish, rstate)
    }
    is (rs_finish) {
      next_rstate := rs_idle
    }
  }

  // AXI3 Write Channel
  io.axi3.aw.bits.id    := 0.U
  io.axi3.aw.bits.addr  := RegNext(io.cache(wsel).req.addr)
  io.axi3.aw.bits.len   := (bit_cacheline / 32 - 1).U
  io.axi3.aw.bits.size  := "b10".U
  io.axi3.aw.bits.burst := INCR.U
  io.axi3.aw.bits.lock  := 0.U
  io.axi3.aw.bits.cache := 0.U
  io.axi3.aw.bits.prot  := 0.U
  io.axi3.aw.valid      := false.B
  io.axi3.w.bits.id     := 0.U
  io.axi3.w.bits.data   := wbuff << (wptr << 5.U)
  io.axi3.w.bits.strb   := "b1111".U
  io.axi3.w.bits.last   := wptr === (bit_cacheline / 32 - 1).U
  io.axi3.w.valid       := false.B
  io.axi3.b.ready       := true.B
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
      next_wstate := Mux(io.axi3.w.bits.last.orR, ws_wait_valid, wstate)
    }
    is (ws_wait_valid) {
      next_wstate := Mux(io.axi3.b.valid, ws_idle, wstate)
    }
  }
}

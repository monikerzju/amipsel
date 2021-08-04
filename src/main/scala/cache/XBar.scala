package cache

import chisel3._
import chisel3.util._
import icore._

class CacheReq(bit_cacheline: Int = 128, width: Int = 32) extends Bundle with MemAccessType {
  val valid = Input(Bool())
  val wen   = Input(Bool())
  val addr  = Input(UInt(width.W))
  val data  = Input(UInt(bit_cacheline.W))
  val mtype = Input(UInt(SZ_MEM_TYPE.W))  // for mmio only; set to DWORD if not in mmio 
  override def cloneType = (new CacheReq(bit_cacheline, width)).asInstanceOf[this.type]
}

class CacheResp(bit_cacheline: Int = 128) extends Bundle {
  val valid = Output(Bool())
  val data  = Output(UInt(bit_cacheline.W))
  override def cloneType = (new CacheResp(bit_cacheline)).asInstanceOf[this.type]
}

// From the view of Cache
class CacheIO(bit_cacheline: Int = 128, width: Int = 32) extends Bundle {
  val req   = Flipped(new CacheReq(bit_cacheline, width))
  val resp  = Flipped(new CacheResp(bit_cacheline))
  override def cloneType = (new CacheIO(bit_cacheline, width)).asInstanceOf[this.type]
}

class AChannel(id_width: Int = 1, width: Int = 32) extends Bundle {
  val id    = Output(UInt(id_width.W))
  val addr  = Output(UInt(width.W))
  val len   = Output(UInt(4.W))
  val size  = Output(UInt(3.W))
  val burst = Output(UInt(2.W))
  val lock  = Output(UInt(2.W))
  val cache = Output(UInt(4.W))
  val prot  = Output(UInt(3.W))
  override def cloneType = (new AChannel(id_width, width)).asInstanceOf[this.type]
}

class RChannel(id_width: Int = 1, width: Int = 32) extends Bundle {
  val id    = Output(UInt(id_width.W))
  val data  = Output(UInt(width.W))
  val resp  = Output(UInt(2.W))
  val last  = Output(UInt(1.W))
  override def cloneType = (new RChannel(id_width, width)).asInstanceOf[this.type]
}

class WChannel(id_width: Int = 1, width: Int = 32) extends Bundle {
  val id    = Output(UInt(id_width.W))
  val data  = Output(UInt(width.W))
  val strb  = Output(UInt(4.W))
  val last  = Output(UInt(1.W))
  override def cloneType = (new WChannel(id_width, width)).asInstanceOf[this.type]
}

class BChannel(id_width: Int = 1) extends Bundle {
  val id    = Output(UInt(id_width.W))
  val resp  = Output(UInt(2.W))
  override def cloneType = (new BChannel(id_width)).asInstanceOf[this.type]
}

// From the AXI3 Server's view
class AXI3(id_width: Int = 1, width: Int = 32) extends Bundle {
  val ar = Decoupled(new AChannel(id_width, width))
  val r  = Flipped(Decoupled(new RChannel(id_width, width)))
  val aw = Decoupled(new AChannel(id_width, width))
  val w  = Decoupled(new WChannel(id_width, width))
  val b  = Flipped(Decoupled(new BChannel(id_width)))
  override def cloneType = (new AXI3(id_width, width)).asInstanceOf[this.type]
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

class CacheArbiter(nclient: Int  = 2, policy: String = "Seq") extends Module {
  val io = IO(new Bundle {
    val chosen = Output(UInt(log2Ceil(nclient).W))
    val en     = Output(Bool())
    val valid  = Input(Vec(nclient, Bool()))
    val lock   = Input(Bool())
  })

  val chosen = RegInit((nclient - 1).asUInt)
  val active = WireDefault(io.valid(nclient - 1))
  val sel = WireDefault((nclient - 1).asUInt)

  for (i <- nclient-2 to 0 by -1) {
    when (io.valid(i)) {
      sel := i.asUInt
      active := true.B
    }
  }  

  io.chosen := Mux(io.lock, chosen, sel)
  io.en := active
  chosen := io.chosen
}

class AXI3ServerIO(nclient: Int = 2, bit_cacheline: Int = 128, id_width: Int = 1, len: Int = 32) extends Bundle {
  val cache = Vec(nclient, Flipped(new CacheIO(bit_cacheline)))
  val axi3  = new AXI3(id_width, len)
  override def cloneType = (new AXI3ServerIO(nclient, bit_cacheline, id_width, len)).asInstanceOf[this.type]
}

class AXI3Server(nclient: Int = 2, bit_cacheline: Int = 128, id_width: Int = 1, policy: String = "Seq", 
  len: Int = 32, nwrite: Int = 1, writer: Int = 0, trace: Boolean = true) extends Module with MemAccessType {
  val io = IO(new AXI3ServerIO(nclient, bit_cacheline, id_width))

  def mapAddr(addr: UInt): UInt = {
    Cat("b000".U, addr(len - 4, 0))
  }

  if (trace) {
    when (!RegNext(io.cache(0).req.valid) && io.cache(0).req.valid) {
      printf("dcache addr %x, wen %x\n", io.cache(0).req.addr, io.cache(0).req.wen)
    }
    when (!RegNext(io.cache(1).req.valid) && io.cache(1).req.valid) {
      printf("icache addr %x\n", io.cache(1).req.addr)
    }
  }

  assert(bit_cacheline <= 256) 
  assert(policy == "Seq" || policy == "RR")

  val rs_idle :: rs_wait_ready :: rs_receive :: rs_finish :: Nil = Enum(4)
  val ws_idle :: ws_wait_ready :: ws_write :: ws_wait_valid :: ws_finish :: Nil = Enum(5)
  val rstate = RegInit(rs_idle)
  val next_rstate = WireDefault(rstate)
  val wstate = RegInit(ws_idle)
  val next_wstate = WireDefault(wstate)

  val w_arbiter = if (nwrite != 1) Module(new CacheArbiter(nclient, policy)) else null
  val r_arbiter = Module(new CacheArbiter(nclient, policy))
  val cache_wen = RegInit(VecInit(Seq.fill(nclient)(false.B)))
  val cache_valid = RegInit(VecInit(Seq.fill(nclient)(false.B)))
  val addr = Reg(Vec(nclient, UInt(len.W)))

  for (i <- 0 until nclient) {
    addr(i) := mapAddr(io.cache(i).req.addr)
    cache_wen(i) := io.cache(i).req.wen
    cache_valid(i) := io.cache(i).req.valid
    if (nwrite != 1)
      w_arbiter.io.valid(i) := cache_valid(i) && cache_wen(i)
    r_arbiter.io.valid(i) := cache_valid(i) && !cache_wen(i)
  }
  if (nwrite != 1)
    w_arbiter.io.lock := wstate =/= ws_idle
  r_arbiter.io.lock := rstate =/= rs_idle
 
  val INCR: String = "b01"

  val rsel = r_arbiter.io.chosen
  val wsel = if (nwrite != 1) w_arbiter.io.chosen else writer.U
  val rbuff = new RBuff(bit_cacheline)
  val wptr = RegInit(0.U(log2Ceil(bit_cacheline / 8).W))
  val wbuff = RegNext(io.cache(wsel).req.data)

  rstate := next_rstate
  wstate := next_wstate

  val ren = r_arbiter.io.en
  val wen = if (nwrite != 1) w_arbiter.io.en else io.cache(writer).req.valid && io.cache(writer).req.wen
  
  for (i <- 0 until nclient) {
    io.cache(i).resp.valid := Mux(i.U === rsel, rstate === rs_finish, Mux(i.U === wsel, wstate === ws_finish, false.B))
    io.cache(i).resp.data  := rbuff.getl()
  }

  // AXI3 Read Channel
  val rtype = RegEnable(io.cache(rsel).req.mtype, ren)
  io.axi3.ar.bits.id    := 0.U
  io.axi3.ar.bits.addr  := addr(rsel)
  io.axi3.ar.bits.len   := Mux(rtype === MEM_DWORD.U, (bit_cacheline / 32 - 1).U, 0.U)
  io.axi3.ar.bits.size  := MuxLookup(
    rtype, "b10".U,
    Seq(
      MEM_BYTE.U  -> "b00".U,
      MEM_HALF.U  -> "b01".U
    )
  )
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
      when (io.axi3.r.valid) {
        rbuff.enqueue(io.axi3.r.bits.data)
      }
      io.axi3.r.ready := true.B
      next_rstate := Mux(io.axi3.r.valid && io.axi3.r.bits.last.orR, rs_finish, rstate)
    }
    is (rs_finish) {
      next_rstate := rs_idle
    }
  }

  // AXI3 Write Channel
  val wtype = RegEnable(io.cache(wsel).req.mtype, wen)
  io.axi3.aw.bits.id    := 0.U
  io.axi3.aw.bits.addr  := addr(wsel) & "hffff_fffc".U
  io.axi3.aw.bits.len   := Mux(wtype === MEM_DWORD.U, (bit_cacheline / 32 - 1).U, 0.U)
  io.axi3.aw.bits.size  := MuxLookup(
    wtype, "b10".U,
    Seq(
      MEM_BYTE.U  -> "b00".U,
      MEM_HALF.U  -> "b01".U
    )
  )
  io.axi3.aw.bits.burst := INCR.U
  io.axi3.aw.bits.lock  := 0.U
  io.axi3.aw.bits.cache := 0.U
  io.axi3.aw.bits.prot  := 0.U
  io.axi3.aw.valid      := false.B
  io.axi3.w.bits.id     := 0.U
  io.axi3.w.bits.data   := MuxLookup(
    wtype,
    MuxLookup(
      wptr, wbuff(31, 0),
      Seq(
        1.U -> wbuff(32*1+31, 32*1),
        2.U -> wbuff(32*2+31, 32*2),
        3.U -> wbuff(32*3+31, 32*3),
        4.U -> wbuff(32*4+31, 32*4),
        5.U -> wbuff(32*5+31, 32*5),
        6.U -> wbuff(32*6+31, 32*6),
        7.U -> wbuff(32*7+31, 32*7)
      )
    ),
    Seq(
      MEM_BYTE.U  -> (wbuff << (addr(wsel)(1, 0) << 3.U)),
      MEM_HALF.U  -> (wbuff << (addr(wsel)(1, 0) << 3.U))
    )
  )
  io.axi3.w.bits.strb   := MuxLookup(
    wtype, "b1111".U,
    Seq(
      MEM_BYTE.U -> ("b0001".U << addr(wsel)(1, 0)),
      MEM_HALF.U -> ("b0011".U << addr(wsel)(1, 0))
    )
  )
  io.axi3.w.bits.last   := wtype =/= MEM_DWORD.U || wptr === (bit_cacheline / 32 - 1).U
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
      wptr := wptr + Mux(io.axi3.w.ready && wtype === MEM_DWORD.U, 1.U, 0.U)
      next_wstate := Mux(io.axi3.w.bits.last.orR, ws_wait_valid, wstate)
    }
    is (ws_wait_valid) {
      next_wstate := Mux(io.axi3.b.valid, ws_finish, wstate)
    }
    is (ws_finish) {
      next_wstate := ws_idle
    }
  }
}

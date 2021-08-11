// DONE BTBv1

package fu

import chisel3._
import chisel3.util._
import chisel3.experimental._
import cache._

class RASReq() extends Bundle {
  val call = Input(Bool())  // 
}

class RASResp() extends Bundle {

}

class RAS(depth: Int = 8, width: Int = 32) {
  val count = RegInit(0.U(log2Up(depth + 1).W))
  val pos = RegInit(0.U(log2Up(depth).W))
  val stack = Reg(Vec(depth, UInt(width.W)))

  def push(addr: UInt): Unit = {
    when (count < depth.U) { count := count + 1.U }
    val next_pos = Mux(isPow2(depth).asBool() || pos < (depth - 1).U, pos + 1.U, 0.U)
    stack(next_pos) := addr
    pos := next_pos
  }
  def peek: UInt = stack(pos)
  def pop(): Unit = when (!isEmpty) {
    count := count - 1.U
    pos := Mux(isPow2(depth).asBool() || pos > 0.U, pos - 1.U, (depth - 1).U)
  }
  def clear(): Unit = count := 0.U
  def isEmpty: Bool = count === 0.U
}

// First, the delay slot will cause multiple issues.
// So I try BHT[DS-Addr] = Jump-Or-Not in order to get rid of waiting the vilatile delay slot fetching operation.
// Furthermore, BRAM has 1 cycle delay, so I have to predict the next insturction rather than the original one.
// Thanks to the delay slot, the next-instrcution prediction will bring out no penalty even if the first instruction is JMP.
class BHTReq(width: Int = 32) extends Bundle {
  val next_line = Input(UInt(width.W))
  override def cloneType = (new BHTReq(width)).asInstanceOf[this.type]
}

class BHTResp(width: Int = 32, issueN: Int = 2) extends Bundle {
  val taken_vec = Output(Vec(issueN, Bool()))
  val target_first = Output(UInt(width.W))
  val state_second = Output(UInt(2.W))
  override def cloneType = (new BHTResp(width, issueN)).asInstanceOf[this.type]
}

class BHTExeUpdate(width: Int = 32) extends Bundle {
  val v = Input(Bool())
  val errpr = Input(Bool())          // wrong 1, reinforce 0
  val pc_br = Input(UInt(width.W))   // bht[pc_br] will change
  val target = Input(UInt(width.W))
  val taken = Input(Bool())
  override def cloneType = (new BHTExeUpdate(width)).asInstanceOf[this.type]
}

// If predict taken but not a branch instruction, CPU will not jump after the so-called branch slot is fetched.
// So, the pipeline needs flushing
class BHTDecUpdate(width: Int = 32) extends Bundle {
  val v = Input(Bool())
  val pc_br = Input(UInt(width.W))   // bht[pc_br] to not taken, the original state
  override def cloneType = (new BHTDecUpdate(width)).asInstanceOf[this.type]
}

class BHTUpdate(width: Int = 32) extends Bundle {
  val dec = new BHTDecUpdate(width)
  val exe = new BHTExeUpdate(width)
  override def cloneType = (new BHTUpdate(width)).asInstanceOf[this.type]
}

// req.nextline = pc + 8
// resp.taken_vec(0) is for pc, (1) is for pc + 4, this signal also influence how many instructions to decode
class BPUIO(width: Int = 32, issueN: Int = 2) extends Bundle {
  val req = new BHTReq(width)
  val resp = new BHTResp(width, issueN)
  val update = new BHTUpdate(width)
  override def cloneType = (new BPUIO(width, issueN)).asInstanceOf[this.type]
}

class BPU(depth: Int = 256, offset: Int = 3, width: Int = 32, issueN: Int = 2, instByte: Int = 4, delaySlot: Boolean = true, cacheDepth: Int = 4, verilator: Boolean = false) extends Module {
  val io = IO(new BPUIO(width, issueN))
  
  // strongly not taken
  val SN = 0
  val WN = 1
  val WT = 2
  val ST = 3

  def getHashedIndex(addr: UInt) : UInt = { // a good hash function should convert the sparse list to a dense but not conflict one
    if (offset == 32 && (depth == 128 || depth == 256)) {
      val oft = if (depth == 128) 0 else 1
      val higher = Cat(addr(15 + oft) ^ addr(14 + oft) ^ addr(8 + oft), addr(14 + oft) ^ addr(13 + oft) ^ addr(7 + oft), addr(13 + oft) ^ addr(12 + oft) ^ addr(6 + oft), addr(12 + oft) ^ addr(11 + oft) ^ addr(5 + oft), addr(11 + oft) ^ addr(10 + oft) ^ addr(4 + oft), addr(10 + oft) ^ addr(9 + oft) ^ addr(3 + oft))
      if (depth == 128) {
        higher
      } else {
        Cat(higher, addr(10) ^ addr(9) ^ addr(3))
      }
    } else if (delaySlot && offset >= 3) {
      addr(offset + log2Ceil(depth) - 2, offset)
    } else {
      addr(offset + log2Ceil(depth) - 1, offset)
    }
  }

  def getHashedIndexWith2(addr: UInt) : UInt = {
    Cat(getHashedIndex(addr), addr(2))
  }

  val bht_cache_tag  = Reg(Vec(cacheDepth, UInt(log2Ceil(depth).W)))
  val bht_cache_stat = RegInit(VecInit(Seq.fill(cacheDepth)(0.U(2.W))))
  val hit_in_bht_cache = Wire(Bool())
  val bht_cache_result = Wire(UInt(2.W))
  val update_cache_index = if (cacheDepth > 1) RegInit(0.U(log2Ceil(cacheDepth).W)) else 0.U
  val cache_or_update_hit = Reg(Bool())
  val chosen_result = Reg(UInt(2.W))

  // BHT are registers, because it is relatively small and BRAM does not support reset
  val history      = if (verilator) Module(new DPSyncReadMem(depth, 2)) else Module(new DPBRAMSyncReadMem(depth, 2, 1))
  val buffer       = if (verilator) Module(new SPSyncReadMem(depth, width - log2Ceil(instByte))) else Module(new BRAMSyncReadMem(depth, width - log2Ceil(instByte), 1))

  // for BHT
  val waddr      = Mux(!io.update.dec.v, io.update.exe.pc_br, io.update.dec.pc_br)
  val may_update = io.update.exe.v || io.update.dec.v
  val update     = Mux(!io.update.dec.v, Mux(io.update.exe.taken, !io.update.exe.pc_br(1, 0).andR, io.update.exe.pc_br(1, 0).orR), true.B)
  val wdata      = Mux(!io.update.dec.v, Mux(io.update.exe.taken, io.update.exe.pc_br(1, 0) + 1.U, io.update.exe.pc_br(1, 0) - 1.U), io.update.dec.pc_br(1, 0) - 1.U)

  history.io.addra := Mux(update, getHashedIndexWith2(waddr), getHashedIndexWith2(io.req.next_line))
  history.io.addrb := getHashedIndexWith2(io.req.next_line + 4.U)
  history.io.dina  := wdata
  history.io.dinb  := DontCare
  history.io.wea   := update
  history.io.web   := false.B

  val last_update = RegNext(update)
  val bht_first   = Mux(last_update, Mux(cache_or_update_hit, chosen_result, Fill(2, 0.U)), history.io.douta)

  var useBPU = false
  if (useBPU) {
    io.resp.taken_vec(0)  := bht_first(1)
    io.resp.taken_vec(1)  := history.io.doutb(1)
  }
  else {
    io.resp.taken_vec(0)  := false.B
    io.resp.taken_vec(1)  := false.B
  }


  // Notice that the update from ID will only change BHT
  buffer.io.we := io.update.exe.errpr && io.update.exe.v
  buffer.io.addr := getHashedIndexWith2(Mux(io.update.exe.errpr, io.update.exe.pc_br, io.req.next_line))
  buffer.io.din := io.update.exe.target(width - 1, log2Ceil(instByte))
  io.resp.target_first := Cat(buffer.io.dout, bht_first)
  io.resp.state_second := history.io.doutb

  // BHT cache
  hit_in_bht_cache := false.B
  bht_cache_result := bht_cache_stat(0)
  for (i <- 0 until cacheDepth) {
    when (bht_cache_tag(i) === getHashedIndexWith2(io.req.next_line)) {
      hit_in_bht_cache := true.B
      if (i != 0) {
        bht_cache_result := bht_cache_stat(i)
      }
    }
  }
  when(update && hit_in_bht_cache) {
    for (i <- 0 until cacheDepth) {
      bht_cache_stat(i) := 0.U
    }
  }.elsewhen(io.resp.taken_vec(0) && !hit_in_bht_cache) {
    if (cacheDepth > 1) {
      update_cache_index := update_cache_index + 1.U
    }
    bht_cache_tag(update_cache_index) := RegNext(getHashedIndexWith2(io.req.next_line))
    bht_cache_stat(update_cache_index) := bht_first
  }

  // cache or update hit
  val update_query_hit = getHashedIndexWith2(io.req.next_line) === getHashedIndexWith2(waddr)
  cache_or_update_hit := hit_in_bht_cache || update_query_hit
  chosen_result := Mux(update_query_hit, wdata, bht_cache_result)

  assert(instByte == 4)

}

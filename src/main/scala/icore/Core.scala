package icore

import chisel3._
import chisel3.util._
import conf.Config
import isa.MicroOpCtrl._
import fu.CauseExcCode

// for a 2-insts slot (cache/frontbackend), if only 1 is filled, it will go to the lower (least significant) Config.${len} bits

trait FrontToBack extends Config {
  // decode to fifo 0, 1 or 2 instructions' micro-ops
  val FB_INSTN_0 = 0
  val FB_INSTN_1 = 1
  val FB_INSTN_2 = 2
}

// Backend master frontend slave signals
class BMFS extends Bundle with Config {
  // redirect, eg syscall, branch, jump. please_wait means the frontend is busy, eg icache fetching from memory
  val redirect_kill = Output(Bool())
  val redirect_pc = Output(UInt(len.W))
}

// Frontend master backend slave
class FMBS extends Bundle with Config {
  val instn = Output(UInt(log2Ceil(frontendIssueN + 1).W))
  val inst_ops = Output(Vec(frontendIssueN, UInt(SZ_MICRO_OP.W)))
  // fifo is full maybe
  val please_wait = Input(Bool())
}

// From the perspective of frontend, rearend should flip in-out
class FrontBackIO extends Bundle with Config {
  val bmfs = Flipped(new BMFS)
  val fmbs = new FMBS
}

class FrontendIO extends Bundle with Config {
  val fb = new FrontBackIO
  val icache = Flipped(new MemIO())
}

class BackendIO extends Bundle with Config with CauseExcCode {
  val fb = Flipped(new FrontBackIO)
  val dcache = Flipped(new MemIO(1))
  val interrupt = Input(Vec(SZ_HARD_INT, Bool()))
}

trait MemAccessType {
  val SZ_MEM_READ_RESP = 1
  val SZ_MEM_TYPE = 2
  val MEM_BYTE = 0
  val MEM_HALF = 1
  val MEM_WORD = 2
  val MEM_DWORD = 3   // for dual-issue icache only
  val MEM_RESP_1 = 0
  val MEM_RESP_2 = 1  // for response of dual-issue icache or dual-issue dcache
}

class MemReq extends Bundle with Config with MemAccessType {
  val addr = Output(UInt(len.W))
  val wdata = Output(UInt(len.W))
  val wen = Output(Bool())
  val flush = Output(Bool())    // for cache instructions
  val invalidate = Output(Bool())   // for cache instructions
  val mtype = Output(UInt(SZ_MEM_TYPE.W))
}

class MemResp(val issueN:Int = 2) extends Bundle with Config with MemAccessType {
  val rdata = Output(Vec(issueN, UInt(len.W)))    // 64-bit because of MEM_DWORD for dual-issue, 1 cycle latency for BRAM
  val respn = Output(UInt(SZ_MEM_READ_RESP.W))       // if req is MEM_DWORD but cannot be responsed because of icache miss, 0 cycle latency
}

// From the view of cache, req is input and resp is output
class MemIO(val issueN:Int = 2) extends Bundle with Config {
  val req = Flipped(Decoupled(new MemReq))
  val resp = Decoupled(new MemResp(issueN))
}

// Memory or MMIO is judged in icache and dcache, not in core
// From the view of core, MemIO should be in-out flipped
class CoreIO extends Bundle with Config with CauseExcCode {
  val icache = Flipped(new MemIO)
  val dcache = Flipped(new MemIO(1))
  val interrupt = Input(Vec(SZ_HARD_INT, Bool()))
}

class Core extends Module with Config {
  val io = IO(new CoreIO)

  val fe = Module(new Frontend)
  val be = Module(new Backend)

  fe.io.fb <> be.io.fb
  be.io.interrupt := io.interrupt.map((i: Bool) => RegNext(i))

  io.icache <> fe.io.icache
  io.dcache <> be.io.dcache
}
package cache
import conf._
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
trait Cache_Parameters{
    val TagBits=18
    val IndexBits=8
    val OffsetBits=6
    val DataBits=512
    val nBuf=4
    // FIXME: not compatible with current interface
    assert(TagBits+IndexBits+OffsetBits==32)
    val MissTolerance=4
}
class MemResp extends Bundle with Config {
//   val rdata = Output(Vec(1, UInt(len.W)))    // 64-bit because of MEM_DWORD for dual-issue, 1 cycle latency for BRAM
  val rdata = Output(UInt(len.W))    // 64-bit because of MEM_DWORD for dual-issue, 1 cycle latency for BRAM
  val respn = Output(UInt(2.W))  // if req is MEM_DWORD but cannot be responsed because of icache miss, 0 cycle latency
}
class MemReq extends Bundle with Config {
  val addr = Output(UInt(len.W))
  val wdata = Output(UInt(len.W))
  val wen = Output(Bool())
  val flush = Output(Bool())    // for cache instructions
  val invalidate = Output(Bool())   // for cache instructions
  val mtype = Output(UInt(2.W))
}
class MemIO extends Bundle with Config {
  val req = Flipped(Decoupled(new MemReq))
  val resp = Decoupled(new MemResp)
}

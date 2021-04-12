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
trait Cache_Parameters_4Way{
    val TagBits=20
    val IndexBits=6
    val OffsetBits=6
    val DataBits=512
    val nBuf=4
    // FIXME: not compatible with current interface
    assert(TagBits+IndexBits+OffsetBits==32)
    val MissTolerance=4
}
class MemResp extends Bundle with Config {
  val rdata = Output(Vec(2, UInt(len.W)))    // 64-bit because of MEM_DWORD for dual-issue, 1 cycle latency for BRAM
  val respn = Output(UInt(2.W))  // if req is MEM_DWORD but cannot be responsed because of icache miss, 0 cycle latency
}
class MemRespData extends Bundle with Config {
  val rdata=Output(UInt(len.W))
  val respn =Output(Bool())
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
// class AXI4RA extends Bundle with Cache_Parameters with Config{
//     val addr=Output(UInt(len.W))
// }
// class AXI4RD extends Bundle with Cache_Parameters with Config{
//     val data=Output(UInt(len.W))
//     val last=Bool()
// } 
// class AXI4WA extends AXI4RA {}
// class AXI4WD extends AXI4RD {
//     val bready=Bool()
//     val bvalid=Bool()
//     val bresp=Bool()
// }

trait AXI4Parameters {
  // These are all fixed by the AXI4 standard:
  val lenBits = 8
  val sizeBits = 3
  val burstBits = 2
  val lockBits = 1
  val cacheBits = 4
  val protBits = 3
  val qosBits = 4
  val respBits = 2

  // These are not fixed:
  val idBits = 1
  val addrBits = 32
  val dataBits = 32
  val userBits = 1

  def CACHE_RALLOCATE = 8.U(cacheBits.W)
  def CACHE_WALLOCATE = 4.U(cacheBits.W)
  def CACHE_MODIFIABLE = 2.U(cacheBits.W)
  def CACHE_BUFFERABLE = 1.U(cacheBits.W)

  def PROT_PRIVILEDGED = 1.U(protBits.W)
  def PROT_INSECURE = 2.U(protBits.W)
  def PROT_INSTRUCTION = 4.U(protBits.W)

  def BURST_FIXED = 0.U(burstBits.W)
  def BURST_INCR = 1.U(burstBits.W)
  def BURST_WRAP = 2.U(burstBits.W)

  def RESP_OKAY = 0.U(respBits.W)
  def RESP_EXOKAY = 1.U(respBits.W)
  def RESP_SLVERR = 2.U(respBits.W)
  def RESP_DECERR = 3.U(respBits.W)
}
abstract class AXI4BundleBase() extends Bundle

class AXI4BundleA extends AXI4BundleBase with AXI4Parameters {
  val id = Output(UInt(idBits.W))
  val addr = Output(UInt(addrBits.W))
  val len = Output(UInt(lenBits.W)) // number of beats - 1
  val size = Output(UInt(sizeBits.W)) // bytes in beat = 2^size
  val burst = Output(UInt(burstBits.W)) // burst type
  val lock = Output(UInt(lockBits.W)) // lock type
  val cache = Output(UInt(cacheBits.W)) // memory type
  val prot = Output(UInt(protBits.W)) // protection type
  val qos = Output(UInt(qosBits.W)) // 0=no QoS, bigger = higher priority
  //   val region = UInt(width = 4) // optional
  val user = Output(UInt(userBits.W))

  override def toPrintable: Printable =
    p"addr = 0x${Hexadecimal(addr)}, len = ${len}, size = ${size}"
}

class AXI4BundleAW extends AXI4BundleA
class AXI4BundleAR extends AXI4BundleA

class AXI4BundleW extends AXI4BundleBase with AXI4Parameters {
  val data = Output(UInt(dataBits.W))
  val strb = Output(UInt((dataBits / 8).W))
  val last = Output(Bool())
  val user = Output(UInt(userBits.W))

  override def toPrintable: Printable =
    p"data = 0x${Hexadecimal(data)}, strb = 0x${Hexadecimal(strb)}, last = ${last}"
}

class AXI4BundleR extends AXI4BundleBase with AXI4Parameters {
  val id = Output(UInt(idBits.W))
  val data = Output(UInt(dataBits.W))
  val resp = Output(UInt(respBits.W))
  val last = Output(Bool())
  val user = Output(UInt(userBits.W))

  override def toPrintable: Printable =
    p"data = 0x${Hexadecimal(data)}, resp = ${resp}, last = ${last}"
}

class AXI4BundleB extends AXI4BundleBase with AXI4Parameters {
  val id = Output(UInt(idBits.W))
  val resp = Output(UInt(respBits.W))
  val user = Output(UInt(userBits.W))

  override def toPrintable: Printable = p"resp = ${resp}"
}

class AXI4Bundle extends AXI4BundleBase with AXI4Parameters {
  // Decoupled provides ready&valid bit
  val aw = Decoupled(new AXI4BundleAW) // address write
  val w = Decoupled(new AXI4BundleW) // data write
  val b = Flipped(Decoupled(new AXI4BundleB)) // write response
  val ar = Decoupled(new AXI4BundleAR) // address read
  val r = Flipped(Decoupled(new AXI4BundleR)) // data read

  override def toPrintable: Printable =
    p"aw: valid=${aw.valid}, ready=${aw.ready}, ${aw.bits}\nw: valid=${w.valid}, ready=${w.ready}, ${w.bits}\nb: valid=${b.valid}, ready=${b.ready}, ${b.bits}\nar: valid=${ar.valid}, ready=${ar.ready}, ${ar.bits}\nr: valid=${r.valid}, ready=${r.ready}, ${r.bits}\n"
}

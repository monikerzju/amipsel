package cache
import conf._
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import icore._

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

// class AXI3R(id: Int = 0, width: Int = 32) extends Bundle {
//   val id = if (id == 0) Output(UInt(1.W)) else Output(UInt(id.W))
//   val data = Output(UInt(width.W))
//   val resp = Output(UInt(2.W))
//   val last = Output(Bool())
// }

// class AXI3AR(id: Int = 0, width: Int = 32) extends Bundle {
//   val id = if (id == 0) Output(UInt(1.W)) else Output(UInt(id.W))
//   val addr = Output(UInt(width.W))
//   val len = Output(UInt(8.W))
//   val size = Output(UInt(3.W))
//   val burst = Output(UInt(2.W))
//   // DontCare from here to the end
//   val lock = Output(Bool())
//   val cache = Output(UInt(4.W))
//   val prot = Output(UInt(3.W))
//   val qos = Output(UInt(4.W))
// }

// class AXI3W(id: Int = 0, width: Int = 32) extends Bundle {
//   val data = Output(UInt(width.W))
//   val strb = Output(UInt(8.W))
//   val last = Output(Bool())
// }

// class AXI3B(id: Int = 0) extends Bundle {
//   val id = if (id == 0) Output(UInt(1.W)) else Output(UInt(id.W))
//   val resp = Output(UInt(2.W))
// }

// // From CPU's view
// class AXI3IO(id: Int = 0, width: Int = 32) extends Bundle {
//   val ar = Decoupled(new AXI3AR(id, width))
//   val r = Flipped(Decoupled(new AXI3R(id, width)))
//   val aw = Decoupled(new AXI3AR(id, width)) // totally same with ar channel
//   val w = Decoupled(new AXI3W(id, width))
//   val b = Flipped(Decoupled(new AXI3B(id)))
// }

package cache
import conf._
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import icore._

class MetaIOISimple extends Bundle with Config {
  val index_in = Input(UInt(dIndexBits.W))
  val tags_in = Input(UInt(dTagBits.W))
  val update = Input(Bool())
  val hit = Output(Bool())
  // val aux_index = Input(UInt((dIndexBits).W))
  // val aux_tag = Input(UInt((dTagBits).W))
}

class MetaIOI extends MetaIOISimple {
  val invalidate = Input(Bool())
}

class MetaIODSimple extends MetaIOISimple with Config {
  val write = Input(Bool())
  val tag = Output(UInt(dTagBits.W))
  val dirty = Output(Bool())
}

class MetaIOI4Way extends Bundle with Config {
  val index_in = Input(UInt(dIndexBits.W))
  val tags_in = Input(UInt(dTagBits.W))
  val update = Input(Bool())
  val hit = Output(Bool())
  val aux_index = Input(UInt(dIndexBits.W))
  val aux_tag = Input(UInt(dTagBits.W))
  val sub_index = Output(
    UInt(2.W)
  ) // when hit, output the corresponding sub-index for data access
}

class MetaIOD4Way extends MetaIOI4Way with Config {
  val write = Input(Bool())
  val tag = Output(UInt(dTagBits.W))
  val dirty = Output(Bool())
}

class MetaBundleI extends Bundle with Config {
  val tag = UInt(dTagBits.W)
  val valid = Bool()
  val b = Vec(4, Bool())
}

class MetaBundleD extends Bundle with Config {
  val tag = UInt(dTagBits.W)
  val valid = Bool()
  val b = Vec(4, Bool())
  val dirty = Bool()
}

trait CacheParameters_4Way {
  val dTagBits = 20
  val dIndexBits = 7
  val offsetBits = 5
  val DataBits = 256
  val nBuf = 4
  // FIXME: not compatible with current interface
  assert(dTagBits + dIndexBits + offsetBits == 32)
  val MissTolerance = 4
}

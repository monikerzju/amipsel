package cache
import conf._
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import icore._

class MetaIOISimple extends Bundle with CacheParameters {
  val index_in = Input(UInt(IndexBits.W))
  val tags_in = Input(UInt(TagBits.W))
  val update = Input(Bool())
  val hit = Output(Bool())
  val aux_index = Input(UInt((IndexBits).W))
  val aux_tag = Input(UInt((TagBits).W))
}

class MetaIOI extends MetaIOISimple {
  val invalidate = Input(Bool())
}

class MetaIODSimple extends MetaIOISimple {
  val write = Input(Bool())
  val tag = Output(UInt(TagBits.W))
  val dirty = Output(Bool())
}

class MetaIOI4Way extends Bundle with CacheParameters_4Way {
  val index_in = Input(UInt(IndexBits.W))
  val tags_in = Input(UInt(TagBits.W))
  val update = Input(Bool())
  val hit = Output(Bool())
  val aux_index = Input(UInt(IndexBits.W))
  val aux_tag = Input(UInt(TagBits.W))
  val sub_index = Output(
    UInt(2.W)
  ) // when hit, output the corresponding sub-index for data access
}

class MetaIOD4Way extends MetaIOI4Way with CacheParameters_4Way {
  val write = Input(Bool())
  val tag = Output(UInt(TagBits.W))
  val dirty = Output(Bool())
}

class MetaBundleI extends Bundle with CacheParameters_4Way {
  val tag = UInt(TagBits.W)
  val valid = Bool()
  val b = Vec(4, Bool())
}

class MetaBundleD extends Bundle with CacheParameters_4Way {
  val tag = UInt(TagBits.W)
  val valid = Bool()
  val b = Vec(4, Bool())
  val dirty = Bool()
}

trait CacheParameters {
  val TagBits = 18
  val IndexBits = 9
  val OffsetBits = 5
  val DataBits = 256
  val nBuf = 4
  // FIXME: not compatible with current interface
  assert(TagBits + IndexBits + OffsetBits == 32)
  val MissTolerance = 4
}

trait CacheParameters_4Way {
  val TagBits = 20
  val IndexBits = 7
  val OffsetBits = 5
  val DataBits = 256
  val nBuf = 4
  // FIXME: not compatible with current interface
  assert(TagBits + IndexBits + OffsetBits == 32)
  val MissTolerance = 4
}

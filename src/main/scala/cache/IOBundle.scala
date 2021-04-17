package cache
import conf._
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import icore._
class MetaIOISimple extends Bundle with CacheParameters{
    val index_in=Input(UInt(IndexBits.W))
    val tags_in=Input(UInt(TagBits.W))
    val update=Input(Bool())
    val hit=Output(Bool())
    val aux_index=Input(UInt((IndexBits).W))
    val aux_tag=Input(UInt((TagBits).W))
}
class MetaIOI extends MetaIOISimple{
    val invalidate=Input(Bool())
} 
class MetaIODSimple extends MetaIOISimple{
    val write=Input(Bool())
    val tag=Output(UInt(TagBits.W))
    val dirty=Output(Bool())
} 
class MetaIOI4Way extends Bundle with CacheParameters_4Way{
    val index_in=Input(UInt(IndexBits.W))
    val tags_in=Input(UInt(TagBits.W))
    val update=Input(Bool())
    val hit=Output(Bool())
    val aux_index=Input(UInt(IndexBits.W))
    val aux_tag=Input(UInt(TagBits.W))
    val sub_index=Output(UInt(2.W))         // when hit, output the corresponding sub-index for data access
}
class MetaIOD4Way extends MetaIOI4Way with CacheParameters_4Way{
    val write=Input(Bool())
    val tag=Output(UInt(TagBits.W))
    val dirty=Output(Bool())
}
class MetaBundleI extends Bundle with CacheParameters_4Way{
    val tag=UInt(TagBits.W)
    val valid=Bool()
    val b=Vec(4,Bool())
}
class MetaBundleD extends Bundle with CacheParameters_4Way{
// class MetaBundleD extends MetaBundleI{
    val tag=UInt(TagBits.W)
    val valid=Bool()
    val b=Vec(4,Bool())
    val dirty=Bool()
}
trait CacheParameters{
    val TagBits=19
    val IndexBits=8
    val OffsetBits=5
    val DataBits=256
    val nBuf=4
    // FIXME: not compatible with current interface
    assert(TagBits+IndexBits+OffsetBits==32)
    val MissTolerance=4
}
trait CacheParameters_4Way{
    val TagBits=21
    val IndexBits=6
    val OffsetBits=5
    val DataBits=256
    val nBuf=4
    // FIXME: not compatible with current interface
    assert(TagBits+IndexBits+OffsetBits==32)
    val MissTolerance=4
}
// class CacheReq(bit_cacheline: Int = 128, width: Int = 32) extends Bundle {
//   val valid = Input(Bool())
//   val wen   = Input(Bool())
//   val addr  = Input(UInt(width.W))
//   val data  = Input(UInt(bit_cacheline.W))
// }

// class CacheResp(bit_cacheline: Int = 128) extends Bundle {
//   val valid = Output(Bool())
//   val data  = Output(UInt(bit_cacheline.W))
// }

// // From the view of Cache
// class CacheIO(bit_cacheline: Int = 128, width: Int = 32) extends Bundle {
//   val req = Flipped(new CacheReq(bit_cacheline, width))
//   val resp = Flipped(new CacheResp(bit_cacheline))
// }


// class MemResp extends Bundle with Config {
//   val rdata = Output(Vec(2, UInt(len.W)))    // 64-bit because of MEM_DWORD for dual-issue, 1 cycle latency for BRAM
//   val respn = Output(UInt(2.W))  // if req is MEM_DWORD but cannot be responsed because of icache miss, 0 cycle latency
// }
// class MemRespData extends Bundle with Config {
//   val rdata=Output(UInt(len.W))
//   val respn =Output(Bool())
// }
// class MemReq extends Bundle with Config {
//   val addr = Output(UInt(len.W))
//   val wdata = Output(UInt(len.W))
//   val wen = Output(Bool())
//   val flush = Output(Bool())    // for cache instructions
//   val invalidate = Output(Bool())   // for cache instructions
//   val mtype = Output(UInt(2.W))
// }
// class MemIO extends Bundle with Config {
//   val req = Flipped(Decoupled(new MemReq))
//   val resp = Decoupled(new MemResp)
// }
// class BufBundle extends Bundle with Config with Cache_Parameters{
//   val addr=UInt(len.W)
//   val data=Vec((1<<OffsetBits-2),UInt(len.W))
//   val written=Bool()
//   val valid=Bool()
// }

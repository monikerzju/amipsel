package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import conf._
import icore._
class MetaBRAMIOD4Way extends Bundle with Config{
  val index_in = Input(UInt(indexBits.W))
  val tags_in = Input(UInt(tagBits.W))
  val update = Input(Bool())
  val hit = Output(Bool())

  val write = Input(Bool())
  val tag = Output(UInt(tagBits.W))
  val dirty = Output(Bool())
  
  val hit_way = Output(UInt(2.W))
  val evict_way = Output(UInt(2.W))
}
class StructuredMeta(tagBits: Int) extends Bundle{
    val valid = Bool()
    val tag = UInt(tagBits.W)
}
class MetaDataBRAM4Way(nline:Int) extends Module with Config{
    val io=IO(new MetaBRAMIOD4Way)
    val blk = Module(new BRAMSyncReadMem(nline/4, (tagBits +1)*4))
    val dirty_bits = RegInit(VecInit(Seq.fill(nline)(false.B)))
    val lru_bits = RegInit(VecInit(Seq.fill(nline)(
        VecInit(Seq.fill(4)(false.B))
    )))

    val __latest__ = VecInit(Seq.fill(4)(true.B))

    val replace_way = lru_bits.indexWhere({
        c: Vec[Bool] => c.asUInt === 0.U
    }) 
    
    // valid and tags for the whole group of 4
    val vt = VecInit(Seq.fill(4)(new StructuredMeta(tagBits)))
    val w = VecInit(Seq.fill(4)(new StructuredMeta(tagBits)))
    val hit_way = vt.indexWhere({
        c:StructuredMeta => c.tag===io.tags_in && c.valid
    }) 
    
    
    blk.io.we := io.update
    blk.io.addr := io.index_in
    blk.io.din := w
    
    
    var i = 0
    for(i<- 0 until 4){
        vt(i) :=  blk.io.dout(tagBits+(tagBits+1)*i,(tagBits+1)*i).asTypeOf(vt(i))
        w(i) := Mux(
            !RegNext(io.hit) && io.update && replace_way === i.U,
            Cat(true.B,io.tags_in).asTypeOf(w(i)),
            vt(i)
        ) 
    }
        
    io.dirty := dirty_bits(io.index_in<< 2.U+hit_way)
    // FIXME: 
    io.hit := io.update || vt.exists({
        c: StructuredMeta => c.tag===io.tags_in && c.valid
    })
    io.tag := vt(replace_way).tag
    
    when(io.hit){
        // lru policy
        lru_bits(io.index_in << 2.U +hit_way) := __latest__
        for (i<- 0 until 4){
            lru_bits(io.index_in << 2.U + i.U)(hit_way) := false.B
        }
    }

    
    when(io.update || (RegNext(io.write) && io.hit)){
        when(io.update && !(RegNext(io.write))){
            dirty_bits(io.index_in << 2.U+ hit_way) := false.B
        }.otherwise{
            // write allocate, all writes are write hit
            dirty_bits(io.index_in << 2.U + hit_way) := true.B
        }
    }
}

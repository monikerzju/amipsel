package cache
// class MetaIO extends Bundle with CacheParameters{
//     val index_in=Input(UInt(IndexBits.W))
//     val tags_in=Input(UInt(TagBits.W))
//     val update=Input(Bool())
//     val hit=Output(Bool())
// }
// class Meta(nline:Int) extends Module with CacheParameters{
//     val io=IO(new MetaIO)
//     io.index_in:=DontCare
//     io.tags_in:=DontCare
//     io.update:=DontCare


//     val reg_tag=RegNext(io.tags_in)
//     val reg_index=RegNext(io.index_in)
//     // val tags=VecInit(VecInit(nline,UInt(TagBits.W)),0.U)
//     val tags=VecInit(Seq.fill(nline)(0.U(TagBits.W)))
//     val valid=VecInit(Seq.fill(nline)(false.B))
//     io.hit:=(io.tags_in===tags(io.index_in)&&valid(io.index_in))
//     when(io.update){
//         tags(reg_index):=reg_tag
//         valid(reg_index):=true.B
//     }
// }
// trait CacheParameters4Way{
//     val TagBits=18
//     val IndexBits=6
//     val OffsetBits=6
//     val DataBits=512
//     val nBuf=4
//     // FIXME: not compatible with current interface
//     assert(TagBits+IndexBits+OffsetBits==30)
// }
// class Meta4Way(nline:Int) extends Meta with CacheParameters4Way{
//     val io=IO(new MetaIO)
//     io.index_in:=DontCare
//     io.tags_in:=DontCare
//     io.update:=DontCare
//     val reg_tag=RegNext(io.tags_in)
//     val reg_index=RegNext(io.index_in)
//     // val asc_tag=Vec(4,UInt(DataBits.W))
//     // val asc_tag=VecInit(Seq.fill(4)(0.U(DataBits.W)))
    
//     // val asc_valid=Vec(4,Bool())
//     val asc_valid=VecInit(Seq.fill(4)(false.B))
//     val tags=VecInit(Seq.fill(nline>>2)(associated_bank))
//     val valid=VecInit(Seq.fill(nline)(asc_valid))
//     // io.hit:=(io.tags_in===tags(io.index_in))&&valid(io.idx)
//     io.hit:=tags(io.index_in).exists(x->(x.t===io.tags_in && x.valid))
//     when(io.update){
//         tags(reg_index):=reg_tag
//         valid(reg_index):=true.B
//     }

// } 
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
class MetaIOI extends Bundle with CacheParameters{
    val index_in=Input(UInt(IndexBits.W))
    val tags_in=Input(UInt(TagBits.W))
    val update=Input(Bool())
    val invalidate=Input(Bool())
    val hit=Output(Bool())
    val aux_index=Input(UInt((IndexBits).W))
    val aux_tag=Input(UInt((TagBits).W))
}
class MetaIOI4 extends Bundle with CacheParameters_4Way{
    val index_in=Input(UInt(IndexBits.W))
    val tags_in=Input(UInt(TagBits.W))
    val update=Input(Bool())
    val invalidate=Input(Bool())
    val aux_index=Input(UInt(IndexBits.W))
    val aux_tag=Input(UInt(TagBits.W))
    val hit=Output(Bool())
    val sub_index=Output(UInt(2.W))
}
class Meta(nline:Int) extends Module with CacheParameters{
    val io=IO(new MetaIOI)
    val tags=RegInit(VecInit(Seq.fill(nline)(0.U(TagBits.W))))
    val valid=RegInit(VecInit(Seq.fill(nline)(false.B)))
    io.hit:= !(io.aux_index===io.index_in&&io.invalidate)&&(io.tags_in===tags(io.index_in)&&valid(io.index_in))
    // combinational loop
    // io.hit:= (io.tags_in===tags(io.index_in)&&valid(io.index_in))
    when(io.update){
        tags(io.aux_index):=io.aux_tag
        valid(io.aux_index):=true.B
    }.elsewhen(io.invalidate){
        valid(io.aux_index):=false.B
    }
}
class MetaBundleI extends Bundle with CacheParameters{
    val tag=UInt(TagBits.W)
    val valid=Bool()
}
class MetaBundleD extends MetaBundleI{
    val dirty=Bool()
}
class Meta_4Way(nline:Int) extends Module with CacheParameters_4Way{
    val io=IO(new MetaIOI4)
    val groups=Vec(nline/4,Vec(4,new MetaBundleI))
    io.hit:=groups(io.index_in).exists({c:MetaBundleI=>c.tag===io.tags_in && c.valid})
    io.sub_index:=Mux(io.hit,groups(io.index_in).indexWhere({c=>c.tag===io.tags_in && c.valid}),0.U)
    val idx=io.aux_index
    def replacement(a:UInt)={
        val slot=groups(a).indexWhere({c:MetaBundleI=> !c.valid})
        slot
    }
    val sid=replacement(io.aux_index)
    when(io.update){
        groups(idx)(sid).tag:=io.aux_tag
        groups(idx)(sid).valid:=true.B
    }.elsewhen(io.invalidate){
        groups(idx)(sid).valid:=false.B
    }
}
class Meta_Data(nline:Int) extends Module with CacheParameters{

    val io=IO(new Bundle{
        val index_in=Input(UInt(IndexBits.W))
        val tags_in=Input(UInt(TagBits.W))
        val update=Input(Bool())
        val invalidate=Input(Bool())
        val aux_index=Input(UInt(IndexBits.W))
        val aux_tag=Input(UInt(TagBits.W))
        val write_hit=Input(Bool())
        val hit=Output(Bool())
        val dirty=Output(Bool())
        // for dual-port non-blocking access; 
    })
    val reg_tag=RegNext(io.tags_in)
    val reg_index=RegNext(io.index_in)
    val tags= RegInit(VecInit(Seq.fill(nline)(0.U(TagBits.W))))
    val valid=RegInit(VecInit(Seq.fill(nline)(false.B)))
    val dirty=RegInit(VecInit(Seq.fill(nline)(false.B)))
    io.hit:= !(io.aux_index===io.index_in&&io.invalidate)&&(io.tags_in===tags(io.index_in)&&valid(io.index_in))
    io.dirty:=valid(io.tags_in) && dirty(io.tags_in)
    // combinational loop
    // io.hit:= (io.tags_in===tags(io.index_in)&&valid(io.index_in))
    when(io.update){
        // when(dirty(io.aux_index)){}
        tags(io.aux_index):=io.aux_tag
        valid(io.aux_index):=true.B
    }.elsewhen(io.invalidate){
        valid(io.aux_index):=false.B
    }
    .elsewhen(io.write_hit){
        dirty(reg_index):=true.B
    }
}

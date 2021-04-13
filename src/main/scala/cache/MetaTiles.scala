package cache
// class MetaIO extends Bundle with Cache_Parameters{
//     val index_in=Input(UInt(IndexBits.W))
//     val tags_in=Input(UInt(TagBits.W))
//     val update=Input(Bool())
//     val hit=Output(Bool())
// }
// class Meta(nline:Int) extends Module with Cache_Parameters{
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
// trait Cache_Parameters4Way{
//     val TagBits=18
//     val IndexBits=6
//     val OffsetBits=6
//     val DataBits=512
//     val nBuf=4
//     // FIXME: not compatible with current interface
//     assert(TagBits+IndexBits+OffsetBits==30)
// }
// class Meta4Way(nline:Int) extends Meta with Cache_Parameters4Way{
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

class Meta(nline:Int) extends Module with Cache_Parameters{

    val io=IO(new Bundle{
        val index_in=Input(UInt(IndexBits.W))
        val tags_in=Input(UInt(TagBits.W))
        val update=Input(Bool())
        val invalidate=Input(Bool())
        val hit=Output(Bool())
        val aux_index=Input(UInt(IndexBits.W))
        val aux_tag=Input(UInt(TagBits.W))
        // for dual-port non-blocking access; 
    })
    // io.index_in:=DontCare
    // io.tags_in:=DontCare
    // io.update:=DontCare
    val reg_tag=RegNext(io.tags_in)
    val reg_index=RegNext(io.aux_index)
    // val tags=VecInit(VecInit(nline,UInt(TagBits.W)),0.U)
    val tags=VecInit(Seq.fill(nline)(0.U(TagBits.W)))
    val valid=VecInit(Seq.fill(nline)(false.B))
    io.hit:= !(io.aux_index===io.index_in&&io.invalidate)&&(io.tags_in===tags(io.index_in)&&valid(io.index_in))
    // combinational loop
    // io.hit:= (io.tags_in===tags(io.index_in)&&valid(io.index_in))
    when(io.update){
        tags(io.aux_index):=reg_tag
        valid(io.aux_index):=true.B
    }.elsewhen(io.invalidate){
        valid(io.aux_index):=false.B
    }
}

class Meta_Data(nline:Int) extends Module with Cache_Parameters{

    val io=IO(new Bundle{
        val index_in=Input(UInt(IndexBits.W))
        val tags_in=Input(UInt(TagBits.W))
        val update=Input(Bool())
        val invalidate=Input(Bool())
        val aux_index=Input(UInt(IndexBits.W))
        val aux_tag=Input(UInt(TagBits.W))
        val write=Input(Bool())
        val hit=Output(Bool())
        val dirty=Output(Bool())
        // for dual-port non-blocking access; 
    })
    // io.index_in:=DontCare
    // io.tags_in:=DontCare
    // io.update:=DontCare
    val reg_tag=RegNext(io.tags_in)
    val reg_index=RegNext(io.aux_index)
    // val tags=VecInit(VecInit(nline,UInt(TagBits.W)),0.U)
    val tags=VecInit(Seq.fill(nline)(0.U(TagBits.W)))
    val valid=VecInit(Seq.fill(nline)(false.B))
    val dirty=VecInit(Seq.fill(nline)(false.B))
    io.hit:= !(io.aux_index===io.index_in&&io.invalidate)&&(io.tags_in===tags(io.index_in)&&valid(io.index_in))
    // combinational loop
    // io.hit:= (io.tags_in===tags(io.index_in)&&valid(io.index_in))
    when(io.update){
        // when(dirty(io.aux_index)){}
        tags(io.aux_index):=reg_tag
        valid(io.aux_index):=true.B
    }.elsewhen(io.invalidate){
        valid(io.aux_index):=false.B
    }
}

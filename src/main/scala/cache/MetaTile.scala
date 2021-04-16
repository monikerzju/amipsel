package cache
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
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
class Meta(nline:Int) extends Module with CacheParameters{
    val io=IO(new MetaIOI)
    val tags=RegInit(VecInit(Seq.fill(nline)(0.U(TagBits.W))))
    val valid=RegInit(VecInit(Seq.fill(nline)(false.B)))
    io.hit:= !(io.aux_index===io.index_in&&io.invalidate)&&(io.tags_in===tags(io.index_in)&&valid(io.index_in))
    when(io.update){
        tags(io.aux_index):=io.aux_tag
        valid(io.aux_index):=true.B
    }.elsewhen(io.invalidate){
        valid(io.aux_index):=false.B
    }
}
class MetaSimple(nline:Int) extends Module with CacheParameters{
    // auto set valid bit to false when miss
    val io=IO(new MetaIOISimple)
    val tags=RegInit(VecInit(Seq.fill(nline)(0.U(TagBits.W))))
    val valid=RegInit(VecInit(Seq.fill(nline)(false.B)))
    io.hit:= io.tags_in===tags(io.index_in)&&valid(io.index_in)
    when(io.update){
        tags(io.aux_index):=io.aux_tag
        valid(io.aux_index):=true.B
    }
}
class Meta_4Way(nline:Int) extends Module with CacheParameters_4Way{
    val io=IO(new MetaIOI4Way)
    val groups=RegInit(VecInit(Seq.fill(nline/4)(VecInit(Seq.fill(4)(0.U((TagBits+5).W).asTypeOf(new MetaBundleI))))))
    val lastest=VecInit(Seq.fill(4)(true.B))
    val idx=io.aux_index
    val sid=groups(idx).indexWhere({c:MetaBundleI=> c.b.asUInt===0.U})
    io.hit:=groups(io.index_in).exists({c:MetaBundleI=>c.tag===io.tags_in && c.valid})
    io.sub_index:=Mux(
        io.hit,
        groups(io.index_in).indexWhere({c=>c.tag===io.tags_in && c.valid}),
        sid
    )
    when(io.hit){
        var i=0
        for(i<-0 to 3){
            groups(io.index_in)(i).b(io.sub_index):=false.B
        }
        groups(io.index_in)(io.sub_index).b:=lastest
    }
    when(io.update){
        groups(idx)(sid).tag:=io.aux_tag
        groups(idx)(sid).valid:=true.B
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
        val tag=Output(UInt(TagBits.W))
        // for dual-port non-blocking access; 
    })
    io.tag:=tags(io.tags_in)
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
class MetaDataSimple(nline:Int) extends Module with CacheParameters{
    val io=IO(new MetaIODSimple)
    val tags= RegInit(VecInit(Seq.fill(nline)(0.U(TagBits.W))))
    val valid=RegInit(VecInit(Seq.fill(nline)(false.B)))
    val dirty=RegInit(VecInit(Seq.fill(nline)(false.B)))
    io.dirty:=dirty(io.index_in)
    io.hit:= io.tags_in===tags(io.index_in)&&valid(io.index_in)
    io.tag:=tags(io.index_in)
    when(io.write && io.hit){
        dirty(io.index_in):=true.B
    }
    when(io.update){
        tags(io.aux_index):=io.aux_tag
        valid(io.aux_index):=true.B
        dirty(io.aux_index):=false.B
    }
}
class MetaData_4Way(nline:Int) extends Module with CacheParameters_4Way{
    val io=IO(new MetaIOD4Way)
    val groups=RegInit(VecInit(Seq.fill(nline/4)(VecInit(Seq.fill(4)(0.U((TagBits+5).W).asTypeOf(new MetaBundleI))))))
    val dirty=RegInit(VecInit(Seq.fill(nline)(false.B)))
    val lastest=VecInit(Seq.fill(4)(true.B))
    val idx=io.aux_index
    val sid=groups(idx).indexWhere({c:MetaBundleI=> c.b.asUInt===0.U})
    val sub=groups(io.index_in).indexWhere({c:MetaBundleI=> c.b.asUInt===0.U})
    io.hit:=groups(io.index_in).exists({c:MetaBundleI=>c.tag===io.tags_in && c.valid})
    io.sub_index:=Mux(
        io.hit,
        groups(io.index_in).indexWhere({c=>c.tag===io.tags_in && c.valid}),
        sid
    )
    when(io.hit){
        var i=0
        for(i<-0 to 3){
            groups(io.index_in)(i).b(io.sub_index):=false.B
        }
        groups(io.index_in)(io.sub_index).b:=lastest
    }
    when(io.write && io.hit){
        dirty(Cat(io.index_in,io.sub_index)):=true.B
    }
    when(io.update){
        groups(idx)(sid).tag:=io.aux_tag
        groups(idx)(sid).valid:=true.B
    }
    io.tag:=groups(idx)(sid).tag
    io.dirty:=dirty(Cat(io.index_in,sub)) && groups(io.index_in)(sub).valid
}

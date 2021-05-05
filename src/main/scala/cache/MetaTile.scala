package cache
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._

import conf.Config

class Meta_4Way(nline:Int) extends Module with Config{
    val io=IO(new MetaIOI4Way)
    val groups=RegInit(VecInit(Seq.fill(nline/4)(VecInit(Seq.fill(4)(0.U((tagBits+5).W).asTypeOf(new MetaBundleI))))))
    val latest=VecInit(Seq.fill(4)(true.B))
    val idx=io.aux_index
    val sid=groups(idx).indexWhere({c:MetaBundleI=> c.b.asUInt===0.U})
    io.hit:=groups(io.index_in).exists({c:MetaBundleI=>c.tag===io.tags_in && c.valid})
    io.sub_index:=Mux(
        io.hit,
        groups(io.index_in).indexWhere({c=>c.tag===io.tags_in && c.valid}),
        sid
    )
    when(io.update){
        groups(idx)(sid).tag:=io.aux_tag
        groups(idx)(sid).valid:=true.B
        var i=0
        groups(idx)(sid).b:=latest
        for(i<-0 to 3){
            groups(idx)(i).b(sid):=false.B
        }
    }
    .elsewhen(io.hit){
        var i=0
        groups(io.index_in)(io.sub_index).b:=latest
        for(i<-0 to 3){
            groups(io.index_in)(i).b(io.sub_index):=false.B
        }
    }
}
class Meta_Data(nline:Int) extends Module with Config{

    val io=IO(new Bundle{
        val index_in=Input(UInt(indexBits.W))
        val tags_in=Input(UInt(tagBits.W))
        val update=Input(Bool())
        val invalidate=Input(Bool())
        val aux_index=Input(UInt(indexBits.W))
        val aux_tag=Input(UInt(tagBits.W))
        val write_hit=Input(Bool())
        val hit=Output(Bool())
        val dirty=Output(Bool())
        val tag=Output(UInt(tagBits.W))
        // for dual-port non-blocking access; 
    })
    io.tag:=tags(io.tags_in)
    val reg_tag=RegNext(io.tags_in)
    val reg_index=RegNext(io.index_in)
    val tags= RegInit(VecInit(Seq.fill(nline)(0.U(tagBits.W))))
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
class MetaDataSimple(nline:Int) extends Module with Config{
    val io=IO(new MetaIODSimple)
    val tags= RegInit(VecInit(Seq.fill(nline)(0.U(tagBits.W))))
    val valid=RegInit(VecInit(Seq.fill(nline)(false.B)))
    val dirty=RegInit(VecInit(Seq.fill(nline)(false.B)))
    io.dirty:=dirty(io.index_in)
    io.hit:= io.tags_in===tags(io.index_in)&&valid(io.index_in)
    io.tag:=tags(io.index_in)
    when(io.write && io.hit){
        dirty(io.index_in):=true.B
    }
    when(io.update){
        tags(io.index_in):=io.tags_in
        valid(io.index_in):=true.B
        dirty(io.index_in):=io.write
    }
}
class MetaData_4Way(nline:Int) extends Module with Config{
    val io=IO(new MetaIOD4Way)
    val groups=RegInit(VecInit(Seq.fill(nline/4)(VecInit(Seq.fill(4)(0.U((tagBits+5).W).asTypeOf(new MetaBundleI))))))
    val dirty=RegInit(VecInit(Seq.fill(nline)(false.B)))
    val latest=VecInit(Seq.fill(4)(true.B))
    val idx=io.aux_index
    val sid=groups(idx).indexWhere({c:MetaBundleI=> c.b.asUInt===0.U})
    val sub=groups(io.index_in).indexWhere({c:MetaBundleI=> c.b.asUInt===0.U})
    io.hit:=groups(io.index_in).exists({c:MetaBundleI=>c.tag===io.tags_in && c.valid})
    io.sub_index:=Mux(
        io.hit,
        groups(io.index_in).indexWhere({c=>c.tag===io.tags_in && c.valid}),
        sid
    )
    when(io.update){
        groups(idx)(sid).tag:=io.aux_tag
        groups(idx)(sid).valid:=true.B
        dirty(Cat(idx,sid)):=false.B
        groups(idx)(sid).b:=latest
        var i=0
        for(i<-0 to 3){
            groups(idx)(i).b(sid):=false.B
        }
    }
    .elsewhen(io.hit){
        when(io.write){
            dirty(Cat(io.index_in,io.sub_index)):=true.B
        }
        groups(io.index_in)(io.sub_index).b:=latest
        var i=0
        for(i<-0 to 3){
            groups(io.index_in)(i).b(io.sub_index):=false.B
        }
    }
    io.tag:=groups(idx)(sid).tag
    io.dirty:=dirty(Cat(io.index_in,sub)) && groups(io.index_in)(sub).valid
}

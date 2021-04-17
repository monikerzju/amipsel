package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import conf._
import icore._
class ICacheSimple_4Way extends Module with CacheParameters_4Way with Config{
    val io=IO(new Bundle{
        val cpu=new MemIO()
        val bar=new CacheIO(1<<(OffsetBits+3))
    })
    val nline=1<<IndexBits
    val data=Module(new BRAMSyncReadMem(nline*4,1<<(OffsetBits+3)))
    val meta=Module(new Meta_4Way(nline));
    data.io.we:=false.B

    io.bar.req.valid:=false.B
    io.bar.req.wen:=false.B
    io.bar.req.addr:=io.cpu.req.bits.addr
    io.bar.req.data:=0.U
    // TODO: [ ] set the content during the test 
    // TODO: [ ] dual-port BRAM

    
    val tag_raw=io.cpu.req.bits.addr(31,32-TagBits)
    val index_raw=io.cpu.req.bits.addr(31-OffsetBits,32-OffsetBits-IndexBits)
    
    val line=Wire(Vec(1<<(OffsetBits-2),UInt(len.W)))
    val fillline=RegInit(VecInit(Seq.fill(1<<(OffsetBits-2))(0.U(len.W))))
    val index=RegNext(index_raw)
    data.io.addr:=Cat(index_raw,meta.io.sub_index)
    data.io.din:=io.bar.resp.data
    var i=0
    for(i<- 0 until 1<<(OffsetBits-2)){line(i):=data.io.dout(i*len+31,i*len)}

    val tag_refill=RegInit(0.U(TagBits.W))
    val word1=RegNext(io.cpu.req.bits.addr(OffsetBits,2))
    val word2=word1+1.U
    val index_refill=RegInit(0.U(IndexBits.W))
    meta.io.tags_in:=tag_raw
    meta.io.index_in:=index_raw
    meta.io.update:=false.B
    meta.io.aux_index:=index_refill
    meta.io.aux_tag:=tag_refill
    // meta.io.invalidate:=false.B
    val s_normal::s_refill::Nil=Enum(2)
    val state=RegInit(s_normal)
    io.cpu.req.ready:=io.cpu.resp.valid

    io.cpu.resp.valid:=io.bar.resp.valid||(state===s_normal && meta.io.hit)
    val dual_issue=io.cpu.req.bits.mtype===3.U && word2=/=0.U
    // io.cpu.resp.bits.respn:= Cat(dual_issue,!meta.io.hit)
    io.cpu.resp.bits.respn:= dual_issue
    io.cpu.resp.bits.rdata(0):=line(word1)
    io.cpu.resp.bits.rdata(1):=line(word2)
    // val checking= !out_of_service && io.cpu.req.valid
    when(state===s_normal){
        when(io.cpu.req.valid){
            when(meta.io.hit){
                // nothing to be done, data on the way
            }
            .otherwise{
                // out_of_service:=true.B
                state:=s_refill
                io.bar.req.valid:=true.B
                io.bar.req.addr:=io.cpu.req.bits.addr
                tag_refill:=tag_raw
                index_refill:=index_raw
                // meta.io.invalidate:=true.B
                // meta.io.aux_index:=index_raw
            }
        }
    }
    .elsewhen(state===s_refill){
        when(io.bar.resp.valid){
                // out_of_service:=false.B
                state:=s_normal
            for(i<- 0 until 1<<(OffsetBits-2)){line(i):=io.bar.resp.data(i*len+31,i*len)}
            io.cpu.resp.valid:=true.B
            meta.io.update:=true.B
            data.io.addr:=Cat(index_refill,meta.io.sub_index)
            data.io.we:=true.B
            // inform_cpu_data_valid()
        }
    }
}

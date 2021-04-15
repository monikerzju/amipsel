/***********************************************************
*********************icache prototype***********************
1-way direct-mapped instruction cache
features:   
    - AXI protocol added
    - non-blocking
    - meta ready at the cycle imediately after the request
    - data delay 1 cycle
    - for word access only

TODO:   [x] output serialized to cater for AXI bandwidth
        [ ] traits not compatible with icore defination
        [x] BRAM interface for data access
        [ ] invalidate instructions 
        [ ] flush
        [x] dual-issue for icache

NOTICE: - expect the valid signal early in the cycle, not withstandable the latency 
        - provides access for aligned address only
FIXME:  [ ] skeptical : the valid signal might not trigger the state transfer; 
                in which case both meta and data will suffer 1-cycle lantency 
        [ ] valid-ready protocol 
        [ ] 双发射字节对齐？若一个miss 一个hit？
        [ ] non-blocking 导致 out-of-order?

***********************************************************/
package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import conf._
import icore._
class DCacheSimple extends Module with CacheParameters with Config{
    val io=IO(new Bundle{
        val cpu=new MemIO()
        val bar=new CacheIO(1<<(OffsetBits+3))
    })
    val nline=1<<IndexBits
    val data=Module(new BRAMSyncReadMem(nline,1<<(OffsetBits+3)))
    
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
    data.io.addr:=index_raw
    data.io.din:=io.bar.resp.data
    var i=0
    for(i<- 0 until 1<<(OffsetBits-2)){line(i):=data.io.dout(i*len+31,i*len)}

    val meta=Module(new MetaDataSimple(nline));
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
    
    val out_of_service=RegInit(false.B)

    io.cpu.req.ready:=io.cpu.resp.valid

    io.cpu.resp.valid:=io.bar.resp.valid||(!out_of_service && meta.io.hit)
    val dual_issue=io.cpu.req.bits.mtype===3.U && word2=/=0.U
    // io.cpu.resp.bits.respn:= Cat(dual_issue,!meta.io.hit)
    io.cpu.resp.bits.respn:= dual_issue
    io.cpu.resp.bits.rdata(0):=line(word1)
    io.cpu.resp.bits.rdata(1):=line(word2)
    val s_normal::s_evict::s_refill::Nil=Enum(3)
    val state=RegInit(s_normal)
    switch(state){
        is(s_normal){
            when(io.cpu.req.valid){
                when(meta.io.hit){
                    // nothing to be done, data on the way
                }
                .otherwise{
                    tag_refill:=tag_raw
                    index_refill:=index_raw
                    io.bar.req.valid:=true.B
                    when(meta.io.dirty){
                        state:=s_evict
                        io.bar.req.addr:=Cat(Seq(meta.io.tag,index_raw,0.U(OffsetBits.W)))
                        // FIXME: align? 
                        io.bar.req.wen:=true.B
                    }
                    .otherwise{
                        // out_of_service:=true.B
                        state:=s_refill
                    }
                }
            }
        }
        is(s_refill){
            when(io.bar.resp.valid){
                out_of_service:=false.B
                for(i<- 0 until 1<<(OffsetBits-2)){line(i):=io.bar.resp.data(i*len+31,i*len)}
                io.cpu.resp.valid:=true.B
                meta.io.update:=true.B
                data.io.addr:=index_refill
                data.io.we:=true.B
            }
        }
        is(s_evict){
            when(io.bar.resp.valid){
                state:=s_normal
                out_of_service:=true.B
                io.bar.req.valid:=true.B
                io.bar.req.addr:=Cat(Seq(tag_refill,index_refill,0.U(OffsetBits.W)))
                io.bar.req.wen:=false.B
            }
        }
    }
}

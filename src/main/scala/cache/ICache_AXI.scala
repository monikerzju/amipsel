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
class AXI4RA extends Bundle with Cache_Parameters with Config{
    val addr=Output(UInt(len.W))
}
class AXI4RD extends Bundle with Cache_Parameters with Config{
    val data=Output(UInt(len.W))
    val last=Bool()
} 
class AXI4WA extends AXI4RA {}
class AXI4WD extends AXI4RD {
    val bready=Bool()
    val bvalid=Bool()
    val bresp=Bool()
}
class ICacheAXI extends Module with Cache_Parameters with Config{
    val io=IO(new Bundle{
        val in=new Bundle{
            val cpu=Flipped(Decoupled(new MemReq()))
            val AXI=Flipped(Decoupled(new AXI4RD()))
        }
        val out=new Bundle{
            val cpu=Decoupled(new MemResp())
            val AXI=Decoupled(new AXI4RA())
        }
    })
    val nline=1<<IndexBits
    // io.in.cpu.bits:=DontCare
    io.in.AXI.ready:=false.B
    // io.in.AXI.valid:=DontCare
    // io.in.AXI.bits:=DontCare

    // val data=Mem(nline,Vec(1<<(OffsetBits-2),UInt(len.W)))
    val data=Module(new BRAMSyncReadMem(nline,1<<(OffsetBits+3)))
    data.io.web:=DontCare
    data.io.addrb:=DontCare
    data.io.dinb:=DontCare
    data.io.doutb:=DontCare
    data.io.wea:=false.B
    data.io.addra:=0.U
    data.io.dina:=0.U
    val line=VecInit(Seq.fill(1<<(OffsetBits-2))(0.U(len.W)))
    val fillline=VecInit(Seq.fill(1<<(OffsetBits-2))(0.U(len.W)))
    // TODO: [ ] set the content during the test 
    // TODO: [ ] dual-port BRAM

    
    val tag_raw=Wire(UInt(TagBits.W))
    tag_raw:=io.in.cpu.bits.addr(31,32-TagBits)
    val index_raw=Wire(UInt(IndexBits.W))
    index_raw:=io.in.cpu.bits.addr(31-OffsetBits,32-OffsetBits-IndexBits)
    
    val meta=Module(new Meta(nline));
    meta.io.tags_in:=tag_raw
    meta.io.index_in:=index_raw

    val tag=RegNext(tag_raw)
    val index=RegNext(index_raw)
    val word_offset=RegNext(io.in.cpu.bits.addr(OffsetBits,2))
    val word2=word_offset+1.U

    val pipe=VecInit(Seq.fill(MissTolerance)(0.U(len.W)))
    // more like a ring buffer
    val next=RegInit(0.U(log2Floor(MissTolerance).W))
    val ptr=RegInit(0.U(log2Floor(MissTolerance).W))
    val serving=RegInit(0.U(log2Floor(MissTolerance).W))

    val que=RegInit(false.B)
    val que_addr=RegInit(false.B)
    val out_of_service=RegInit(false.B)

    io.out.AXI.valid:=que_addr
    io.out.AXI.bits.addr:=pipe(ptr)

    io.in.cpu.ready:=io.out.cpu.valid

    io.in.AXI.ready:=que

    io.out.cpu.valid:=io.in.cpu.valid && !out_of_service
    val dual_issue=io.in.cpu.bits.mtype===3.U && word2=/=0.U
    io.out.cpu.bits.respn:= Cat(dual_issue,!meta.io.hit)
    val rdata=VecInit(Seq.fill(2)(0.U(len.W)))
    io.out.cpu.bits.rdata:=rdata
    // line:=data.io.douta
    data.io.addra:=index_raw
    // io.out.cpu.bits.rdata:=data(index_raw)(word_offset)

    // TODO: [ ] merge with the bram
    when(!out_of_service && io.in.cpu.valid){
        when(meta.io.hit){
            var i=0
            for(i<- 0 until 1<<(OffsetBits-2)){
                line(i):=data.io.douta(i*len+31,i*len)
            }
            rdata(0):=line(word_offset)
            rdata(1):=line(word2)
        }
        .otherwise{
            pipe(next):=io.in.cpu.bits.addr                
            next:=next+1.U
            when(next===serving && !(io.in.AXI.valid && io.in.AXI.bits.last)){
                out_of_service:=true.B
            }
            que_addr:=true.B

        }
    }
    when(que_addr && io.out.AXI.ready){
        ptr:=ptr+1.U
        when(ptr===next && !(!out_of_service && io.in.cpu.valid && !meta.io.hit)){
            que_addr:=false.B
            // FIXME: [x] concurency
            // can simultiniusly put and get, making it difficult to judge if it is full
        }
        que:=true.B
    }
    def inform_cpu_data_valid()={
        // if single-issue, nothing to be done as the cache is blocked, 
        // miss tolerance=1
        // valid all of a sudden
    }
    val meta_invalidate=RegInit(true.B)
    meta.io.invalidate:=meta_invalidate
    val meta_update=RegInit(false.B)
    meta.io.update:=meta_update
    val meta_aux_tag=RegInit(0.U(TagBits.W))
    meta.io.aux_tag:=meta_aux_tag
    // FIXME: [x] refill process needs to tend to valid bit
    // val index_fill=pipe(serving)(IndexBits+TagBits-1,TagBits)
    // ERROR: if written like above, it causes a combinational loop
    // val index_fill=RegInit(0.U(IndexBits.W))
    val index_fill=RegNext(pipe(serving)(31-TagBits,32-TagBits-IndexBits))
    // FIXME: [ ] possible error here
    // val index_fill=Wire(pipe(serving)(31-TagBits,32-TagBits-IndexBits))
    meta.io.aux_index:=index_fill
    val j=RegInit(0.U((1<<(OffsetBits-2)).W))
    when(que&&io.in.AXI.valid){
        // meta.io.aux_index:=index_fill
        meta_invalidate:=true.B
        // data(index_fill)(j):=io.in.AXI.bits.data
        fillline(j):=io.in.AXI.bits.data
        when(io.in.AXI.bits.last){
            j:=0.U;
            inform_cpu_data_valid()
            serving:=serving+1.U
            out_of_service:=false.B
            meta_update:=true.B
            meta_aux_tag:=pipe(serving)(len-1,len-TagBits)
            when(serving===ptr && !(que_addr && io.out.AXI.ready)){
                que:=false.B
            }
            data.io.wea:=true.B
            data.io.addra:=index_fill
            data.io.dina:=fillline.asUInt
            // data.io.dina:=Cat(line(i) for i<-0 until 1<<(OffsetBits-2))
        }
        .otherwise{
            j:=j+1.U
        }
    }.otherwise{
        meta_invalidate:=false.B
        meta_update:=false.B
    }
}

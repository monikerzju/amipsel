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
        [ ] some registers need to be asserted down when not used  
        [ ] temporal issue
***********************************************************/
package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import conf._
// import icore._
class DCacheAXIDummy extends Module with Cache_Parameters with Config{
    val io=IO(new Bundle{
        val cpu=new MemIO()
        val AXI=new AXI4Bundle()
        // val in=new Bundle{
        //     val cpu=Flipped(Decoupled(new MemReq()))
        //     val AXI=Flipped(Decoupled(new AXI4RD()))
        // }
        // val out=new Bundle{
        //     val cpu=Decoupled(new MemResp())
        //     val AXI=Decoupled(new AXI4RA())
        // }
    })
    val nline=1<<IndexBits
    // io.cpu.req.bits:=DontCare
    io.AXI.r.ready:=false.B
    // io.AXI.r.valid:=DontCare
    // io.AXI.r.bits:=DontCare
    io.AXI.b:=DontCare
    // io.AXI.aw:=DontCare
    io.AXI.aw.bits.burst := 0.U
    io.AXI.aw.bits.id := 0.U
    io.AXI.aw.bits.user := DontCare
    io.AXI.aw.bits.lock := 0.U
    io.AXI.aw.bits.cache := 0.U
    io.AXI.aw.bits.prot := 0.U
    io.AXI.aw.bits.qos := 0.U
    io.AXI.aw.bits.len := 0.U // 1 word
    io.AXI.aw.bits.size := "b010".U

    // io.AXI.w:=DontCare
    io.AXI.w.bits.strb:=DontCare
    io.AXI.w.bits.user:=DontCare
    io.AXI.w.bits.last:=false.B

    io.AXI.ar.bits.burst := 0.U
    io.AXI.ar.bits.id := 0.U
    io.AXI.ar.bits.user := DontCare
    io.AXI.ar.bits.lock := 0.U
    io.AXI.ar.bits.cache := 0.U
    io.AXI.ar.bits.prot := 0.U
    io.AXI.ar.bits.qos := 0.U
    io.AXI.ar.bits.len := 0.U // 1 word
    io.AXI.ar.bits.size := "b010".U
    // io.ar.bits.size := MuxLookup(
    //   io.cpu.req.bits.mtype, "b011".U,
    //   Seq(
    //     memByte -> "b000".U,
    //     memByteU -> "b000".U,
    //     memHalf -> "b001".U,
    //     memHalfU -> "b001".U,
    //     memWord -> "b010".U,
    //     memWordU -> "b010".U
    //   )
    // )
    // val data=Mem(nline,Vec(1<<(OffsetBits-2),UInt(len.W)))
    val data=Module(new BRAMSyncReadMem(nline,1<<(OffsetBits+3)))
    data.io.web:=false.B
    data.io.doutb:=DontCare
    data.io.wea:=false.B
    // TODO: [ ] set the content during the test 
    // TODO: [ ] dual-port BRAM

    
    val tag_raw=Wire(UInt(TagBits.W))
    tag_raw:=io.cpu.req.bits.addr(31,32-TagBits)
    val index_raw=Wire(UInt(IndexBits.W))
    val index=RegNext(index_raw)
    index_raw:=io.cpu.req.bits.addr(31-OffsetBits,32-OffsetBits-IndexBits)
    val line=VecInit(Seq.fill(1<<(OffsetBits-2))(0.U(len.W)))
    val fillline=VecInit(Seq.fill(1<<(OffsetBits-2))(0.U(len.W)))
    val writeline=Wire(Vec(1<<(OffsetBits-2),UInt(len.W)))
    writeline:=line
    data.io.addra:=index_raw
    data.io.addrb:=index
    data.io.dina:=fillline.asUInt
    data.io.dinb:=writeline.asUInt
    val meta=Module(new Meta_Data(nline));
    meta.io.tags_in:=tag_raw
    meta.io.index_in:=index_raw
    val dirty=RegInit(false.B)
    dirty:=meta.io.dirty
    val write_hit=RegInit(false.B)
    meta.io.write_hit:=write_hit

    val tag=RegNext(tag_raw)
    val word_offset=RegNext(io.cpu.req.bits.addr(OffsetBits,2))
    val word2=word_offset+1.U

    val pipe=VecInit(Seq.fill(MissTolerance)(0.U(len.W)))
    // more like a ring buffer
    val next=RegInit(0.U(log2Floor(MissTolerance).W))
    val ptr=RegInit(0.U(log2Floor(MissTolerance).W))
    val serving=RegInit(0.U(log2Floor(MissTolerance).W))

    val que=RegInit(false.B)
    val que_addr=RegInit(false.B)
    val out_of_service=RegInit(false.B)

    io.AXI.ar.valid:=que_addr
    io.AXI.ar.bits.addr:=pipe(ptr)

    io.cpu.req.ready:=io.cpu.resp.valid

    io.AXI.r.ready:=que

    io.cpu.resp.valid:=io.cpu.req.valid && !out_of_service
    val dual_issue=io.cpu.req.bits.mtype===3.U && word2=/=0.U
    io.cpu.resp.bits.respn:= Cat(dual_issue,!meta.io.hit)
    val rdata=VecInit(Seq.fill(2)(0.U(len.W)))
    io.cpu.resp.bits.rdata:=rdata
    // line:=data.io.douta
    // io.cpu.resp.bits.rdata:=data(index_raw)(word_offset)
    val mask="hffffffff".U
    val reg_wen=RegNext(io.cpu.req.bits.wen)
    val wdata=RegNext(io.cpu.req.bits.wdata)
    val wd=(mask & wdata) | (~mask & line(word_offset))
    // TODO: [ ] merge with the bram
    val cpu_transfer=RegInit(false.B)

    val buf=Reg(Vec(nBuf,new BufBundle));
    var i=0
    val buf_hit=buf.exists({p:BufBundle=>p.addr===tag_raw && p.valid})
    val buf_id=buf.indexWhere({p:BufBundle=>p.addr===tag_raw && p.valid})
    for (i<-0 until nBuf) {
        buf(i).addr:=0.U
        var m=0
        for(m <- 0 until 1<<(OffsetBits-2)){
            buf(i).data(m):=0.U
        }
        buf(i).written:=false.B
        buf(i).valid:=false.B
    }
    val put=RegInit(0.U(log2Floor(nBuf).W) ) 
    val get=RegInit(0.U(log2Floor(nBuf).W) ) 
    val que_write=RegInit(false.B)
    val que_wdata=RegInit(false.B)
    val put_next=que_write
    val que_write_back=RegInit(false.B)
    val que_data=RegInit(false.B)
    val out_of_service2=RegInit(false.B)
    io.AXI.aw.valid:=que_write_back
    io.AXI.aw.bits.addr:=buf(get).addr
    val next_get=que_write_back && io.AXI.aw.ready
    when(!out_of_service && io.cpu.req.valid && !out_of_service2){
        when(meta.io.hit){
            var i=0
            for(i<- 0 until 1<<(OffsetBits-2)){
                line(i):=data.io.douta(i*len+31,i*len)
            }
            cpu_transfer:=true.B
        }
        .elsewhen(buf_hit){
            // var i=0
            // for(i<- 0 until 1<<(OffsetBits-2)){
            //     line(i):=buf(buf_id).data(i*len+31,i*len)
            // }
            line:=buf(buf_id).data
            cpu_transfer:=true.B
        }
        .otherwise{
            pipe(next):=io.cpu.req.bits.addr                
            next:=next+1.U
            when(next===serving && !(io.AXI.r.valid && io.AXI.r.bits.last)){
                out_of_service:=true.B
            }
            que_addr:=true.B
            when(dirty){
                que_write:=true.B
                buf(put).addr:=io.cpu.req.bits.addr
                when(put===get && !(next_get)){
                    out_of_service2:=true.B
                }
            }
        }
    }
    when(que_write){
        put:=put+1.U
        var i=0
        for(i <- 0 until 1<<(OffsetBits-2)){
            buf(put).data(i):=data.io.douta(31+i*32,i*32)
        }
        // buf(put).data:=
        buf(put).valid:=true.B        
        buf(put).written:=false.B
        que_write_back:=true.B
    }
    when(next_get){
        // get:=get+1.U
        when(get===put && !(put_next)){
            que_write_back:=false.B
        }
        out_of_service2:=false.B
        que_data:=true.B
    }
    io.AXI.w.valid:=que_data
    val k=RegInit(0.U((OffsetBits-2).W))
    io.AXI.w.bits.data:=buf(get).data(k)
    when(que_data&&io.AXI.w.ready){
        k:=k+1.U
        when(k+1.U===0.U){
            io.AXI.w.bits.last:=true.B
            out_of_service2:=false.B
            get:=get+1.U
            when(get===put && !put_next){
                que_write_back:=false.B
            }
            que_data:=false.B
        }
    }
    when(cpu_transfer){
        rdata(0):=line(word_offset)
        rdata(1):=line(word2)
        when(reg_wen){
            writeline(word_offset):=wd
            data.io.web:=true.B
            write_hit:=true.B
        }
    }
    when(que_addr && io.AXI.ar.ready){
        ptr:=ptr+1.U
        when(ptr===next && !(!out_of_service && io.cpu.req.valid && !meta.io.hit)){
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
    when(que&&io.AXI.r.valid){
        // meta.io.aux_index:=index_fill
        meta_invalidate:=true.B
        // data(index_fill)(j):=io.AXI.r.bits.data
        fillline(j):=io.AXI.r.bits.data
        when(io.AXI.r.bits.last){
            j:=0.U;
            inform_cpu_data_valid()
            serving:=serving+1.U
            out_of_service:=false.B
            meta_update:=true.B
            meta_aux_tag:=pipe(serving)(len-1,len-TagBits)
            when(serving===ptr && !(que_addr && io.AXI.ar.ready)){
                que:=false.B
            }
            data.io.wea:=true.B
            data.io.addra:=index_fill
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

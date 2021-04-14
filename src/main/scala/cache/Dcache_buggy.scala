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
import icore._
class DCacheAXIDummy extends Module with Cache_Parameters with Config{
    val io=IO(new Bundle{
        val cpu=new MemIO()
        val AXI=new AXI4Bundle()
    })
    val nline=1<<IndexBits
    io.AXI.r.ready:=false.B
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
    val data=Module(new BRAMSyncReadMem(nline,1<<(OffsetBits+3)))
    data.io.web:=false.B
    data.io.doutb:=DontCare
    data.io.wea:=false.B
    // TODO: [ ] set the content during the test 
    // TODO: [ ] dual-port BRAM

    
    val tag_raw=io.cpu.req.bits.addr(31,32-TagBits)
    val index_raw=io.cpu.req.bits.addr(31-OffsetBits,32-OffsetBits-IndexBits)

    val line=Wire(Vec(1<<(OffsetBits-2),UInt(len.W)))
    val fillline=RegInit(VecInit(Seq.fill(1<<(OffsetBits-2))(0.U(len.W))))
    val index=RegNext(index_raw)
    val writeline=Wire(Vec(1<<(OffsetBits-2),UInt(len.W)))
    writeline:=line
    data.io.addra:=index_raw
    data.io.addrb:=index
    data.io.dina:=fillline.asUInt
    data.io.dinb:=writeline.asUInt
    var i=0
    for(i<- 0 until 1<<(OffsetBits-2)){line(i):=data.io.douta(i*len+31,i*len)}

    val meta=Module(new Meta_Data(nline));
    meta.io.tags_in:=tag_raw
    meta.io.index_in:=index_raw
    val dirty=RegInit(false.B)
    dirty:=meta.io.dirty
    val write_hit=RegInit(false.B)
    meta.io.write_hit:=write_hit

    val tag=RegNext(tag_raw)
    val word1=RegNext(io.cpu.req.bits.addr(OffsetBits,2))
    val word2=word1+1.U

    val ring_buf=RegInit(VecInit(Seq.fill(MissTolerance)(0.U(len.W))))
    // to store the read addr  
    val ptr_put=RegInit(0.U(log2Floor(MissTolerance).W))
    val ptr_ra=RegInit(0.U(log2Floor(MissTolerance).W))
    val ptr_rd=RegInit(0.U(log2Floor(MissTolerance).W))

    val que_rd=RegInit(false.B)
    val que_ra=RegInit(false.B)
    val out_of_service=RegInit(false.B)
    val out_of_service2=RegInit(false.B)

    io.AXI.ar.valid:=que_ra
    io.AXI.ar.bits.addr:=ring_buf(ptr_ra)

    io.cpu.req.ready:=io.cpu.resp.valid

    io.AXI.r.ready:=que_rd

    io.cpu.resp.valid:=io.cpu.req.valid && !out_of_service
    val dual_issue=io.cpu.req.bits.mtype===3.U && word2=/=0.U
    io.cpu.resp.bits.respn:= Cat(dual_issue,!meta.io.hit)
    io.cpu.resp.bits.rdata(0):=line(word1)
    io.cpu.resp.bits.rdata(1):=line(word2)
    val checking= !out_of_service && io.cpu.req.valid && !out_of_service2
    val inc_ptr_ra=que_ra && io.AXI.ar.ready
    val inc_ptr_put=checking && !meta.io.hit
    val inc_ptr_rd=que_rd && io.AXI.r.valid && io.AXI.r.bits.last
    val mask="hffffffff".U
    val reg_wen=RegNext(io.cpu.req.bits.wen && io.cpu.req.valid)
    val wdata=RegNext(io.cpu.req.bits.wdata)
    val wd=(mask & wdata) | (~mask & line(word1))

    val buf=Reg(Vec(nBuf,new BufBundle));
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
    io.AXI.aw.valid:=que_write_back
    io.AXI.aw.bits.addr:=buf(get).addr
    val next_get=que_write_back && io.AXI.aw.ready
    when(checking){
        when(meta.io.hit){
            // nothing to be done, data on the way
        }
        .elsewhen(buf_hit){
            line:=buf(buf_id).data
        }
        .otherwise{
            ring_buf(ptr_put):=io.cpu.req.bits.addr                
            ptr_put:=ptr_put+1.U
            when(ptr_put+1.U===ptr_rd && !inc_ptr_rd){
                out_of_service:=true.B
            }
            que_ra:=true.B
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
    when(reg_wen){
        writeline(word1):=wd
        data.io.web:=true.B
        write_hit:=true.B
    }
    when(inc_ptr_ra){
        ptr_ra:=ptr_ra+1.U
        when(ptr_ra+1.U===ptr_put && !inc_ptr_put){
            que_ra:=false.B
            // FIXME: [x] concurency
            // can simultiniusly put and get, making it difficult to judge if it is full
        }
        que_rd:=true.B
    }
    def inform_cpu_data_valid()={
        // if single-issue, nothing to be done as the cache is blocked, 
        // miss tolerance=1
        // valid all of a sudden
    }
    meta.io.invalidate:=true.B
    meta.io.update:=false.B
    // FIXME: [x] refill process needs to tend to valid bit
    // val index_refill=ring_buf(ptr_rd)(IndexBits+TagBits-1,TagBits)
    // ERROR: if written like above, it causes a combinational loop
    // val index_refill=RegInit(0.U(IndexBits.W))
    // FIXME: [ ] possible error here
    // val index_refill=Wire(ring_buf(ptr_rd)(31-TagBits,32-TagBits-IndexBits))
    val index_refill=ring_buf(ptr_rd)(31-TagBits,32-TagBits-IndexBits)
    meta.io.aux_index:=index_refill
    meta.io.aux_tag:=0.U
    val que_wea=Reg(Bool())
    que_wea:=false.B
    val j=RegInit(0.U((OffsetBits-3).W))
    when(que_rd&&io.AXI.r.valid){
        meta.io.invalidate:=true.B
        // MEMO: [ ] Mem for unit test: data(index_refill)(j):=io.AXI.r.bits.data
        fillline(j):=io.AXI.r.bits.data
        when(io.AXI.r.bits.last){
            j:=0.U
            inform_cpu_data_valid()
            ptr_rd:=ptr_rd+1.U
            out_of_service:=false.B
            meta.io.update:=true.B
            meta.io.aux_tag:=ring_buf(ptr_rd)(31,32-TagBits)
            when(ptr_rd+1.U===ptr_ra && !inc_ptr_ra){
                que_rd:=false.B
            }
            que_wea:=true.B
        }
        .otherwise{
            j:=j+1.U
        }
    }
    when(que_wea){
        data.io.wea:=true.B
        data.io.addra:=index_refill
    }
}

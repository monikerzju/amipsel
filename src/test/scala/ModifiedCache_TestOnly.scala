package test
import cache._
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import conf._
import icore._
class DCacheSimple_test extends Module with CacheParameters with MemAccessType with Config{
    val io=IO(new Bundle{
        val cpu=new MemIO()
        val bar=new CacheIO(1<<(OffsetBits+3))
        val data=Flipped(new DPBRAMSyncReadMemIO(8*(1<<OffsetBits),1<<IndexBits))
    })
    val nline=1<<IndexBits
    // val data=Module(new DPBRAMSyncReadMem(nline,1<<(OffsetBits+3)))
    val meta=Module(new MetaDataSimple(nline));

    val tag_raw=io.cpu.req.bits.addr(31,32-TagBits)
    val index_raw=io.cpu.req.bits.addr(31-OffsetBits,32-OffsetBits-IndexBits)
    io.bar.req.valid:=false.B
    io.bar.req.wen:=false.B
    io.bar.req.addr:=Cat(Seq(tag_raw,index_raw,0.U(OffsetBits.W)))
    io.bar.req.data:=0.U
    // TODO: [ ] set the content during the test 
    // TODO: [ ] dual-port BRAM
    val line=Wire(Vec(1<<(OffsetBits-2),UInt(len.W)))
    val index=RegNext(index_raw)
    var i=0
    for(i<- 0 until 1<<(OffsetBits-2)){line(i):=io.data.douta(i*len+31,i*len)}
    val tag_refill=RegInit(0.U(TagBits.W))
    val word1=RegNext(io.cpu.req.bits.addr(OffsetBits,2))
    val word2=word1+1.U
    val index_refill=RegInit(0.U(IndexBits.W))
    io.data.wea:=false.B
    io.data.addra:=index_raw
    io.data.dina:=io.bar.resp.data
    meta.io.tags_in:=tag_raw
    meta.io.index_in:=index_raw
    meta.io.update:=false.B
    meta.io.aux_index:=index_refill
    meta.io.aux_tag:=tag_refill
    meta.io.write:=io.cpu.req.bits.wen
    val writeline=Wire(Vec(1<<(OffsetBits-2),UInt(len.W)))
    writeline:=line
    val mask=Wire(UInt(32.W))
    mask:="hffffffff".U
    switch(io.cpu.req.bits.mtype){
        is(MEM_HALF.U){
            mask:="h0000ffff".U
        }
        is(MEM_BYTE.U){
            mask:="h000000ff".U
        }
    }
    val wen=io.cpu.req.bits.wen && io.cpu.req.valid
    val reg_wen=RegNext(wen)
    val wdata=RegEnable(io.cpu.req.bits.wdata,wen)
    val wd=(mask & wdata) | (~mask & line(word1))

    io.cpu.req.ready:=io.cpu.resp.valid
    val reg_addr=RegNext(index_raw)
    io.cpu.resp.valid:=io.bar.resp.valid||meta.io.hit
    val dual_issue=io.cpu.req.bits.mtype===3.U && word2=/=0.U
    // io.cpu.resp.bits.respn:= Cat(dual_issue,!meta.io.hit)
    io.cpu.resp.bits.respn:= dual_issue
    io.cpu.resp.bits.rdata(0):=line(word1)
    io.cpu.resp.bits.rdata(1):=line(word2)
    val s_normal::s_evict::s_refill::Nil=Enum(3)
    val state=RegInit(s_normal)
    val tag_evict_reg=RegInit(0.U(TagBits.W))
    io.data.web:=reg_wen
    // FIXME: [ ] write miss?
    io.data.addrb:=reg_addr
    io.data.dinb:=writeline.asUInt
    when(reg_wen){
        writeline(word1):=wd
    }
    switch(state){
        is(s_normal){
            when(io.cpu.req.valid){
                when(meta.io.hit){
                    // nothing to be done, data on the way
                }
                .otherwise{
                    tag_refill:=tag_raw
                    index_refill:=index_raw
                    when(meta.io.dirty){
                        state:=s_evict
                        tag_evict_reg:=meta.io.tag
                        // NOTE:如果路径过长可以从此处切开并把寄存器移到meta内部
                        // io.bar.req.addr:=Cat(Seq(meta.io.tag,index_raw,0.U(OffsetBits.W)))
                        // FIXME: align? register?
                        // io.bar.req.wen:=true.B
                    }
                    .otherwise{
                        // out_of_service:=true.B
                        io.bar.req.valid:=true.B
                        state:=s_refill
                    }
                }
            }
        }
        is(s_refill){
            io.bar.req.addr:=Cat(Seq(tag_refill,index_refill,0.U(OffsetBits.W)))
            when(io.bar.resp.valid){
                state:=s_normal
                for(i<- 0 until 1<<(OffsetBits-2)){line(i):=io.bar.resp.data(i*len+31,i*len)}
                io.cpu.resp.valid:=true.B
                meta.io.update:=true.B
                io.data.addra:=index_refill
                io.data.wea:=true.B
            }
        }
        is(s_evict){
            io.bar.req.valid:=true.B
            io.bar.req.wen:=true.B
            io.bar.req.data:=line.asUInt
            // FIXME: register?
            io.bar.req.addr:=Cat(Seq(tag_evict_reg,index_refill,0.U(OffsetBits.W)))
            when(io.bar.resp.valid){
                state:=s_refill
                io.bar.req.valid:=true.B
                io.bar.req.addr:=Cat(Seq(tag_refill,index_refill,0.U(OffsetBits.W)))
                io.bar.req.wen:=false.B
            }
        }
    }
}
class ICacheSimple_test extends Module with CacheParameters with Config{
    val io=IO(new Bundle{
        val cpu=new MemIO()
        val bar=new CacheIO(1<<(OffsetBits+3))
        val data=Flipped(new BRAMSyncReadMemIO(8*1<<OffsetBits,1<<IndexBits))
    })
    val nline=1<<IndexBits
    // val data=Module(new BRAMSyncReadMem(nline,1<<(OffsetBits+3)))
    val meta=Module(new MetaSimple(nline));
    io.data.we:=false.B

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
    io.data.addr:=index_raw
    io.data.din:=io.bar.resp.data
    var i=0
    for(i<- 0 until 1<<(OffsetBits-2)){line(i):=io.data.dout(i*len+31,i*len)}

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
            io.data.addr:=index_refill
            io.data.we:=true.B
            // inform_cpu_data_valid()
        }
    }
}
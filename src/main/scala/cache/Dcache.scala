/***********************************************************
*********************dcache prototype***********************
1-way direct-mapped instruction cache
features:   
    - AXI protocol added
    - non-blocking
    - meta ready at the cycle imediately after the request
    - data delay 1 cycle
    - for word access only

TODO:   [x] output serialized to cater for AXI bandwidth
        [x] traits not compatible with icore defination
        [x] BRAM interface for data access
        [ ] invalidate instructions 
        [ ] flush
        [x] dual-issue for icache
        [ ] mmio 
        [ ] map from virtual to physical

NOTICE: - expect the valid signal early in the cycle, not withstandable the latency 
        - provides access for aligned address only
FIXME:  [x] skeptical : the valid signal might not trigger the state transfer; 
                in which case both meta and data will suffer 1-cycle lantency 
        [x] valid-ready protocol 
        [ ] 双发射字节对齐？若一个miss 一个hit？
        [x] non-blocking 导致 out-of-order?

***********************************************************/
package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import conf._
import icore._
class DCacheSimple extends Module with CacheParameters with MemAccessType with Config{
    val io=IO(new Bundle{
        val cpu=new MemIO(1)
        val bar=new CacheIO(1<<(OffsetBits+3))
        // val data=Flipped(new DPBRAMSyncReadMemIO(8*(1<<OffsetBits),1<<IndexBits))
    })
    val nline=1<<IndexBits
    val data=Module(new DPBRAMSyncReadMem(nline,1<<(OffsetBits+3)))
    val meta=Module(new MetaDataSimple(nline));
    val unmaped=io.cpu.req.bits.addr(31,29)==="b100".U
    // 0x80000000-0xa000000
    // translate virtual addr from start
    val tag_raw=Cat(Mux(unmaped,0.U(3.W),io.cpu.req.bits.addr(31,29)),io.cpu.req.bits.addr(28,32-TagBits))
    val index_raw=io.cpu.req.bits.addr(len-TagBits-1,len-TagBits-IndexBits)
    io.bar.req.valid:=false.B
    io.bar.req.wen:=false.B
    io.bar.req.addr:=Cat(Seq(tag_raw,index_raw,0.U(OffsetBits.W)))
    io.bar.req.data:=0.U
    // TODO: [ ] set the content during the test 
    // TODO: [ ] dual-port BRAM
    val line=Wire(Vec(1<<(OffsetBits-2),UInt(len.W)))
    val index=RegNext(index_raw)
    var i=0
    for(i<- 0 until 1<<(OffsetBits-2)){line(i):=data.io.douta(i*len+31,i*len)}
    val tag_refill=RegInit(0.U(TagBits.W))
    val word1=RegNext(io.cpu.req.bits.addr(OffsetBits-1,2))
    val word2=word1+1.U
    val index_refill=RegInit(0.U(IndexBits.W))
    data.io.wea:=false.B
    data.io.addra:=index_raw
    data.io.dina:=io.bar.resp.data
    data.io.doutb:=DontCare
    meta.io.tags_in:=tag_raw
    meta.io.index_in:=index_raw
    meta.io.update:=false.B
    meta.io.aux_index:=index_refill
    meta.io.aux_tag:=tag_refill
    meta.io.write:=io.cpu.req.bits.wen
    val writeline=Wire(Vec(1<<(OffsetBits-2),UInt(len.W)))
    writeline:=line
    val mask=Wire(UInt(32.W))
    val shift=io.cpu.req.bits.addr(1,0)<<3
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
    // val wd=(mask & wdata) | (~mask & line(word1))
    val wd=((mask & wdata)<<shift) | ((~(mask << shift)) & line(word1))

    io.cpu.req.ready:=io.cpu.resp.valid
    val s_normal::s_evict::s_refill::s_uncached::s_wait::Nil=Enum(5)
    val state=RegInit(s_normal)
    io.cpu.resp.valid:=io.bar.resp.valid||(state===s_normal && meta.io.hit)
    // val dual_issue=io.cpu.req.bits.mtype===3.U && word2=/=0.U
    // io.cpu.resp.bits.respn:= Cat(dual_issue,!meta.io.hit)
    io.cpu.resp.bits.respn:= 0.U
    io.cpu.resp.bits.rdata(0):=line(word1)
    // io.cpu.resp.bits.rdata(1):=line(word2)
    val tag_evict_reg=RegInit(0.U(TagBits.W))
    data.io.web:=reg_wen
    // FIXME: [ ] write miss?
    data.io.addrb:=index
    data.io.dinb:=writeline.asUInt
    val mmio=io.cpu.req.bits.addr(31,29)==="b101".U // A000_0000-C000_0000
    io.bar.req.mtype:=Mux(mmio,io.cpu.req.bits.mtype,MEM_DWORD.U)
    val reg_rdata = RegInit(0.U(len.W))
    when(reg_wen){
        writeline(word1):=wd
    }
    switch(state){
        is(s_normal){
            when(!mmio&&io.cpu.req.valid){
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
            .elsewhen(mmio && io.cpu.req.valid){
                // send with raw data right away
                state:=s_uncached
                io.bar.req.valid:=true.B
                io.bar.req.addr:=io.cpu.req.bits.addr
                io.bar.req.data:=io.cpu.req.bits.wdata
                io.bar.req.wen:=io.cpu.req.bits.wen
            }
        }
        is(s_refill){
            io.bar.req.addr:=Cat(Seq(tag_refill,index_refill,0.U(OffsetBits.W)))
            when(io.bar.resp.valid){
                state:=s_normal
                for(i<- 0 until 1<<(OffsetBits-2)){line(i):=io.bar.resp.data(i*len+31,i*len)}
                io.cpu.resp.valid:=true.B
                meta.io.update:=true.B
                data.io.addra:=index_refill
                data.io.wea:=true.B
            }.otherwise {
                io.bar.req.valid:=true.B
            }
        }
        is(s_evict){
            io.bar.req.valid:=true.B
            io.bar.req.wen:=true.B
            io.bar.req.data:=line.asUInt
            // FIXME: [ ] register for line?
            io.bar.req.addr:=Cat(Seq(tag_evict_reg,index_refill,0.U(OffsetBits.W)))
            when(io.bar.resp.valid){
                state:=s_refill
                io.bar.req.addr:=Cat(Seq(tag_refill,index_refill,0.U(OffsetBits.W)))
                io.bar.req.wen:=false.B
            }
        }
        is(s_uncached){
            io.bar.req.valid:=true.B
            io.bar.req.addr:=io.cpu.req.bits.addr
            io.bar.req.data:=io.cpu.req.bits.wdata
            io.bar.req.wen:=io.cpu.req.bits.wen

            when(io.bar.resp.valid){
                when(!reg_wen){
                    state:=s_wait
                    reg_rdata:=io.bar.resp.data(31,0)
                }
                .otherwise{
                    state := s_normal
                }
                // nothing to be done for write uncached 
            }
        }
        is(s_wait){
            io.cpu.resp.bits.rdata(0):=reg_rdata
            state:=s_normal
        }
    }
}

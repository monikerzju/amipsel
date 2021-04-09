/***********************************************************
*********************icache prototype***********************
1-way direct-mapped instruction cache
features:   
    - meta ready at the cycle imediately after the request
    - data delay 1 cycle

TODO:   [ ] output not serialized to cater for AXI bandwidth
        [ ] traits not compatible with icore defination
        [ ] BRAM interface for data access


NOTICE: - expect the valid signal early in the cycle, not withstandable the latency 
        - 
FIXME:  [ ] skeptical : the valid signal might not trigger the state transfer; 
                in which case both meta and data will suffer 1-cycle lantency 
        [ ] valid-ready protocol 
***********************************************************/
package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import icore._
trait Cache_Parameters{
    val TagBits=18
    val IndexBits=8
    val OffsetBits=6
    val DataBits=32
    // FIXME: not compatible with current interface
    assert(TagBits+IndexBits+OffsetBits==32)
}
class AXI4IO_Req extends Bundle {
  val addr = Output(UInt(32.W))
//   val wdata = Output(UInt(len.W))
//   val wen = Output(Bool())
//   val type = Output(UInt(SZ_MEM_TYPE.W))
}
class AXI4IO extends Bundle {
    val req=Flipped(Decoupled(new AXI4IO_Req))
    val resp=Decoupled(new MemResp)
}

class ICache1WayDummy(nline:Int,size:Int) extends Module with Cache_Parameters{
    val io=IO(new Bundle{
        val cpu=new MemIO()
        val AXI=Flipped(new AXI4IO())
    })

    val s_idle::s_check::s_fetch::s_refill::Nil=Enum(4)
    val state=RegInit(s_idle)
    
    val meta=new Module{
        val io=new Bundle{
            val index_in=Input(UInt(IndexBits.W))
            val tags_in=Input(UInt(TagBits.W))
            val hit=Output(Bool())
        }
        
        val tags=Vec(nline,0.U(TagBits.W))
        val valid=Vec(nline,false.B)
        io.hit:=io.tags_in===tags(io.index_in)&&valid(io.index_in)
    }

    val data=Mem(nline,0.U(DataBits.W))
    // TODO: [ ] set the content during the test 
    // TODO: [ ] dual-port BRAM

    val addr=io.cpu.req.bits.addr
    val reg_addr=RegNext(addr)  // cached from cpu output, as the data can put up with a delay of 1 cycle

    val tag=addr(0,TagBits-1)
    val index=addr(32-OffsetBits-IndexBits,31-OffsetBits)
    meta.io.tags_in:=tag
    meta.io.index_in:=index
    
    switch(state){
        is(s_idle){
            when(io.cpu.req.valid){     // FIXME: [ ] expect to trigger at valid posedge, don't know how
                state:=s_check
                io.cpu.req.ready:=false.B
            }
        }
        is(s_check){
            io.cpu.req.ready:=true.B
            io.cpu.resp.bits.respn:= !meta.io.hit
            when(meta.io.hit){          // expected to triggered at clk posedge 
                state:=s_fetch
                // reg_addr:=addr
            }
            .otherwise{
                state:=s_refill
                io.AXI.req.bits.addr:=addr
                io.AXI.req.valid:=true.B
            }
        }
        is(s_fetch){
            state:=s_idle
            io.cpu.resp.bits.rdata:=data(reg_addr)
        }
        is(s_refill){
            when(io.AXI.req.ready){     // might add to miss penalty if triggered at clk posedge; not big deal
                // FIXME: [ ] when to set valid to 0?
                state:=s_idle
                data(reg_addr):=io.AXI.resp.bits.rdata

                meta.tags(index):=tag 
                meta.valid(index):=true.B
                // TODO: [ ] decouple these, required by dual-port access 
            }
        }
    }
}

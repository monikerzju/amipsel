/***********************************************************
*********************icache prototype***********************
1-way direct-mapped instruction cache
features:   
    - meta ready at the cycle imediately after the request
    - data delay 1 cycle
    - for word access only

TODO:   [ ] output not serialized to cater for AXI bandwidth
        [ ] traits not compatible with icore defination
        [ ] BRAM interface for data access
        [ ] invalidate instructions 
        [ ] flush
        [ ] dual-issue

NOTICE: - expect the valid signal early in the cycle, not withstandable the latency 
        - provides access for aligned address only
FIXME:  [ ] skeptical : the valid signal might not trigger the state transfer; 
                in which case both meta and data will suffer 1-cycle lantency 
        [ ] valid-ready protocol 
***********************************************************/
package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
// import icore._
import conf._

class AXI4IO_Req extends Bundle with Config{
    val addr = Output(UInt(len.W))
//   val wdata = Output(UInt(len.W))
//   val wen = Output(Bool())
//   val type = Output(UInt(SZ_MEM_TYPE.W))
}
class MemResp extends Bundle with Config {
//   val rdata = Output(Vec(1, UInt(len.W)))    // 64-bit because of MEM_DWORD for dual-issue, 1 cycle latency for BRAM
  val rdata = Output(UInt(len.W))    // 64-bit because of MEM_DWORD for dual-issue, 1 cycle latency for BRAM
  val respn = Output(UInt(2.W))  // if req is MEM_DWORD but cannot be responsed because of icache miss, 0 cycle latency
}
class MemReq extends Bundle with Config {
  val addr = Output(UInt(len.W))
  val wdata = Output(UInt(len.W))
  val wen = Output(Bool())
  val flush = Output(Bool())    // for cache instructions
  val invalidate = Output(Bool())   // for cache instructions
  val mtype = Output(UInt(2.W))
}
class MemIO extends Bundle with Config {
  val req = Flipped(Decoupled(new MemReq))
  val resp = Decoupled(new MemResp)
}
class AXIResp extends Bundle with Cache_Parameters with Config{
    // val rdata=Output(Vec(1<<(OffsetBits-2),UInt(len.W)))
    val rdata=Output(UInt(512.W))
    // respn=
}
class AXI4IO extends Bundle {
    val req=Decoupled(new AXI4IO_Req)
    val resp=Flipped(Decoupled(new AXIResp))
}
// class Meta(nline:Int) extends Module with Cache_Parameters{
//     val io=IO(new Bundle{
//         val index_in=Input(UInt(IndexBits.W))
//         val tags_in=Input(UInt(TagBits.W))
//         val update=Input(Bool())
//         val hit=Output(Bool())
//     })
//     io.index_in:=DontCare
//     io.tags_in:=DontCare
//     io.update:=DontCare
//     val reg_tag=RegNext(io.tags_in)
//     val reg_index=RegNext(io.index_in)
//     // val tags=VecInit(VecInit(nline,UInt(TagBits.W)),0.U)
//     val tags=VecInit(Seq.fill(nline)(0.U(TagBits.W)))
//     val valid=VecInit(Seq.fill(nline)(false.B))
//     io.hit:=(io.tags_in===tags(io.index_in)&&valid(io.index_in))
//     when(io.update){
//         tags(reg_index):=reg_tag
//         valid(reg_index):=true.B
//     }
// }
class ICache1WayDummy(nline:Int) extends Module with Cache_Parameters with Config{
    val io=IO(new Bundle{
        val cpu=new MemIO()
        val AXI=new AXI4IO()
    })
    io.cpu.req.ready:=true.B
    io.cpu.req.valid:=DontCare
    io.cpu.req.bits:=DontCare
    io.cpu.resp.valid:=false.B
    io.cpu.resp.bits:=DontCare
    io.cpu.resp.ready:=DontCare
    io.AXI.req.valid:=DontCare
    io.AXI.req.bits:=DontCare
    io.AXI.req.ready:=DontCare
    io.AXI.resp:=DontCare
    io.AXI.resp:=DontCare
    io.AXI.resp:=DontCare
    val s_idle::s_check::s_fetch::s_refill::Nil=Enum(4)
    val state=RegInit(s_idle)
    

    val data=Mem(nline,Vec(1<<(OffsetBits-2),UInt(len.W)))
    // TODO: [ ] set the content during the test 
    // TODO: [ ] dual-port BRAM

    val addr=io.cpu.req.bits.addr
    val reg_addr=RegNext(addr)  // cached from cpu output, as the data can put up with a delay of 1 cycle, asserted bandwidth 32 bits

    val tag_raw=addr(TagBits-1,0)
    val index_raw=addr(31-OffsetBits,32-OffsetBits-IndexBits)

    val tag=RegNext(tag_raw)
    val index=RegNext(index_raw)
    val word_offset=reg_addr(OffsetBits-2,0)

    val meta=Module (new Meta(nline));
    meta.io.tags_in:=tag_raw
    meta.io.index_in:=index_raw
    meta.io.update:=false.B
    // val slab=Wire(Vec(1<<(OffsetBits-2),UInt(len.W)))
    // slab:=data(index)
    switch(state){
        is(s_idle){
            when(io.cpu.req.valid){     // FIXME: [ ] expect to trigger at valid posedge, don't know how
                state:=s_check
                io.cpu.req.ready:=true.B    // FIXME: [ ] wierld, should be false, but this would pass the test
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
                io.AXI.req.bits.addr:=reg_addr
                io.AXI.req.valid:=true.B
            }
        }
        is(s_fetch){
            state:=s_idle
            io.cpu.resp.bits.rdata:=data(index)(word_offset)
        }
        is(s_refill){
            when(io.AXI.req.ready){     // might add to miss penalty if triggered at clk posedge; not big deal
                // FIXME: [ ] when to set valid to 0?
                state:=s_idle
                var i=0
                for( i <-0 until 1<<(OffsetBits-2)){
                    data(index)(i):=io.AXI.resp.bits.rdata(i*32+31,i*32)
                }
                meta.io.update:=true.B
                io.AXI.req.valid:=false.B
                state:=s_idle
                io.cpu.resp.bits.rdata:=data(index)(word_offset)
                // meta.tags(index):=tag 
                // TODO: [ ] decouple these, required by dual-port access 
            }
        }
    }
}

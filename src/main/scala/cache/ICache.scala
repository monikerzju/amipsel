package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
// import chisel3.tester._
// import chisel3.tester.RawTester.test
import icore._    //FIXME: many errors with this added
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
    
    val tiles=new Module{
        val io=new Bundle{
            val index_in=Input(UInt(IndexBits.W))
            val tags_in=Input(UInt(TagBits.W))
            // val tags_out=Output(UInt(TagBits))
            val stall=Output(Bool())
            val hit=Output(Bool())
            // val pos=Output(UInt(IndexBits))
        }
        
        val tags=Vec(nline,0.U(TagBits.W))
        val valid=Vec(nline,false.B)
        io.hit:=io.tags_in===tags(io.index_in)
        
    }
    val mem=Mem(nline,0.U(DataBits.W))
    // FIXME: just a prototype, connect it to BRAM
    val addr=io.cpu.req.bits.addr
    val _tag=addr(0,TagBits-1)
    val index=addr(32-OffsetBits-IndexBits,31-OffsetBits)
    val resp_stall=RegEnable(~tiles.io.hit,state===s_check)
    io.cpu.resp.bits.respn:=resp_stall
    switch(state){
        is(s_idle){
            when(io.cpu.req.valid){
                state:=s_check
            }
        }
        is(s_check){
            when(/*clk&&*/tiles.io.hit){    //FIXME: imagined syntax
                state:=s_fetch
            }
            // .elsewhen(/*clk*/){      // toggle all clk, imagined
            .otherwise{
                state:=s_refill
                io.AXI.req.bits.addr:=addr
            }
        }
        is(s_refill){
            when(io.AXI.req.ready){ //FIXME: imagined syntax, replace it with proper Decoupled interface
                state:=s_idle
                tiles.tags(index):=_tag 
                tiles.valid(index):=true.B
                // FIXME: ugly
            }
        }
    }
    val data_out=RegNext(mem(index))
    // FIXME: Dequeue Decoupled
    io.cpu.resp.bits.rdata:=data_out
}

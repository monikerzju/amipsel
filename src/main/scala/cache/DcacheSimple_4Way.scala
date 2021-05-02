// package cache

// import chisel3._
// import chisel3.util._
// import chisel3.experimental._
// import chisel3.experimental.BundleLiterals._
// import conf._
// import icore._
// class DCacheSimple_4Way extends Module with CacheParameters_4Way with MemAccessType with Config{
//     val io=IO(new Bundle{
//         val cpu=new MemIO()
//         val bar=new CacheIO(1<<(offsetBits+3))
//     })
//     val nline=1<<indexBits
//     val data=Module(new DPBRAMSyncReadMem(nline*4,1<<(offsetBits+3)))
//     val meta=Module(new MetaData_4Way(nline));

//     io.bar.req.valid:=false.B
//     io.bar.req.wen:=false.B
//     io.bar.req.addr:=io.cpu.req.bits.addr
//     io.bar.req.data:=0.U
//     // TODO: [ ] set the content during the test 
//     // TODO: [ ] dual-port BRAM

    
//     val tag_raw=io.cpu.req.bits.addr(31,32-tagBits)
//     val index_raw=io.cpu.req.bits.addr(31-offsetBits,32-offsetBits-indexBits)
    
//     val line=Wire(Vec(1<<(offsetBits-2),UInt(len.W)))
//     val fillline=RegInit(VecInit(Seq.fill(1<<(offsetBits-2))(0.U(len.W))))
//     val index=RegNext(index_raw)
//     var i=0
//     for(i<- 0 until 1<<(offsetBits-2)){line(i):=data.io.douta(i*len+31,i*len)}

//     val tag_refill=RegInit(0.U(tagBits.W))
//     val word1=RegNext(io.cpu.req.bits.addr(offsetBits,2))
//     val word2=word1+1.U
//     val index_refill=RegInit(0.U(indexBits.W))
//     data.io.wea:=false.B
//     data.io.addra:=Cat(index_raw,meta.io.sub_index)
//     data.io.dina:=io.bar.resp.data
//     meta.io.tags_in:=tag_raw
//     meta.io.index_in:=index_raw
//     meta.io.update:=false.B
//     meta.io.aux_index:=index_refill
//     meta.io.aux_tag:=tag_refill
//     meta.io.write:=io.cpu.req.bits.wen
//     val writeline=Wire(Vec(1<<(offsetBits-2),UInt(len.W)))
//     writeline:=line
//     val mask=Wire(UInt(32.W))
//     mask:="hffffffff".U
//     switch(io.cpu.req.bits.mtype){
//         is(MEM_HALF.U){
//             mask:="h0000ffff".U
//         }
//         is(MEM_BYTE.U){
//             mask:="h000000ff".U
//         }
//     }
//     val reg_wen=RegNext(io.cpu.req.bits.wen && io.cpu.req.valid)
//     val wdata=RegNext(io.cpu.req.bits.wdata)
//     val wd=(mask & wdata) | (~mask & line(word1))

//     io.cpu.req.ready:=io.cpu.resp.valid
//     val reg_addr=RegNext(Cat(index_raw,meta.io.sub_index))
//     io.cpu.resp.valid:=io.bar.resp.valid||meta.io.hit
//     val dual_issue=io.cpu.req.bits.mtype===3.U && word2=/=0.U
//     // io.cpu.resp.bits.respn:= Cat(dual_issue,!meta.io.hit)
//     io.cpu.resp.bits.respn:= dual_issue
//     io.cpu.resp.bits.rdata(0):=line(word1)
//     io.cpu.resp.bits.rdata(1):=line(word2)
//     val s_normal::s_evict::s_refill::Nil=Enum(3)
//     val state=RegInit(s_normal)

//     data.io.web:=reg_wen
//     data.io.addrb:=reg_addr
//     data.io.dinb:=writeline.asUInt
//     when(reg_wen){
//         writeline(word1):=wd
//     }
//     switch(state){
//         is(s_normal){
//             when(io.cpu.req.valid){
//                 when(meta.io.hit){
//                     // nothing to be done, data on the way
//                 }
//                 .otherwise{
//                     tag_refill:=tag_raw
//                     index_refill:=index_raw
//                     io.bar.req.valid:=true.B
//                     when(meta.io.dirty){
//                         state:=s_evict
//                         io.bar.req.addr:=Cat(Seq(meta.io.tag,index_raw,0.U(offsetBits.W)))
//                         // FIXME: align? 
//                         io.bar.req.wen:=true.B
//                     }
//                     .otherwise{
//                         // out_of_service:=true.B
//                         state:=s_refill
//                     }
//                 }
//             }
//         }
//         is(s_refill){
//             when(io.bar.resp.valid){
//                 for(i<- 0 until 1<<(offsetBits-2)){line(i):=io.bar.resp.data(i*len+31,i*len)}
//                 io.cpu.resp.valid:=true.B
//                 meta.io.update:=true.B
//                 data.io.addra:=Cat(index_refill,meta.io.sub_index)
//                 data.io.wea:=true.B
//             }
//         }
//         is(s_evict){
//             when(io.bar.resp.valid){
//                 state:=s_normal
//                 io.bar.req.valid:=true.B
//                 io.bar.req.addr:=Cat(Seq(tag_refill,index_refill,0.U(offsetBits.W)))
//                 io.bar.req.wen:=false.B
//             }
//         }
//     }
// }

package test
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import chisel3.tester._
import chisel3.tester.RawTester.test
import org.scalatest.FreeSpec
import cache._
import icore._
import conf._

class MetaTester extends FreeSpec with ChiselScalatestTester with CacheParameters{
    "test the direct mapped meta" in{
        test(new MetaSimple(256)){c=>
            c.io.tags_in.poke(0.U)
            c.io.index_in.poke(0.U)
            c.io.update.poke(false.B)
            c.io.aux_index.poke(0.U)
            c.io.aux_tag.poke(0.U)
            c.io.hit.expect(false.B)
            c.clock.step()
            // test simple miss

            c.io.tags_in.poke(0.U)
            c.io.index_in.poke(0.U)
            c.io.update.poke(true.B)
            c.io.aux_index.poke(0.U)
            c.io.aux_tag.poke(0x10.U)
            c.io.hit.expect(false.B)
            c.clock.step()
            // test update

            c.io.tags_in.poke(0x10.U)
            c.io.index_in.poke(0.U)
            c.io.update.poke(false.B)
            c.io.aux_index.poke(0.U)
            c.io.aux_tag.poke(0.U)
            c.io.hit.expect(true.B)
            c.clock.step()
            // test hit
            
            var i=0
            for(i<-0 to 5){
                c.io.tags_in.poke(0x10.U)
                c.io.index_in.poke(i.U)
                c.io.update.poke(true.B)
                c.io.aux_index.poke(0.U)
                c.io.aux_tag.poke(0x20.U)
                c.clock.step()
                c.io.tags_in.poke(0x10.U)
                c.io.index_in.poke(i.U)
                c.io.hit.expect(false.B)
                c.clock.step()
                // test replace
            }
        }
    }
    "test 4-way meta" in{
        test(new Meta_4Way(256)){c=>
            c.io.tags_in.poke(0.U)
            c.io.index_in.poke(0.U)
            c.io.update.poke(false.B)
            c.io.aux_index.poke(0.U)
            c.io.aux_tag.poke(0.U)
            c.io.hit.expect(false.B)
            c.clock.step()
            // test simple miss

            c.io.tags_in.poke(0.U)
            c.io.index_in.poke(0.U)
            c.io.update.poke(true.B)
            c.io.aux_index.poke(0.U)
            c.io.aux_tag.poke(0x10.U)
            c.io.hit.expect(false.B)
            c.clock.step()
            // test update

            c.io.tags_in.poke(0x10.U)
            c.io.index_in.poke(0.U)
            c.io.update.poke(false.B)
            c.io.aux_index.poke(0.U)
            c.io.aux_tag.poke(0.U)
            c.io.hit.expect(true.B)
            c.clock.step()
            // test hit
            
            var i=0
            for(i<-0 to 10){
                // test replace
                if(i>0)c.io.tags_in.poke((i-1).U)
                else c.io.tags_in.poke(0x10.U)
                c.io.update.poke(true.B)
                c.io.aux_tag.poke(i.U)
                println(s"$i , sub_index is ${c.io.sub_index.peek}")                        
                c.io.hit.expect(true.B)
                c.clock.step()
                if(i>3){
                    var j=0
                    for(j<-4 to 0 by -1){
                        c.io.tags_in.poke((i-j).U)
                        c.io.update.poke(false.B)
                        println(s"$j ,${c.io.sub_index.peek},${c.io.hit.peek}")                        
                        if(j!=4)
                            c.io.hit.expect(true.B)
                        else 
                            c.io.hit.expect(false.B)
                        c.clock.step()
                    }
                }
                // if(i>0)c.io.sub_index.expect((i-1).U)
                // else c.io.sub_index.expect(0.U)
            }
        }
    }
    "test 4-way meta(data)" in{
        test(new MetaData_4Way(256)){c=>
            c.io.tags_in.poke(0.U)
            c.io.index_in.poke (0.U)
            c.io.update.poke(false.B)
            c.io.aux_index.poke(0.U)
            c.io.aux_tag.poke(0.U)
            c.io.hit.expect(false.B)
            c.io.dirty.expect(false.B)
            c.clock.step()

            // test simple miss

            c.io.tags_in.poke(0.U)
            c.io.index_in.poke(0.U)
            c.io.update.poke(true.B)
            c.io.aux_index.poke(0.U)
            c.io.aux_tag.poke(0x10.U)
            c.io.hit.expect(false.B)
            c.io.dirty.expect(false.B)
            c.clock.step()
            // test update

            c.io.tags_in.poke(0x10.U)
            c.io.index_in.poke(0.U)
            c.io.update.poke(false.B)
            c.io.write.poke(true.B)
            c.io.aux_index.poke(0.U)
            c.io.aux_tag.poke(0.U)
            c.io.hit.expect(true.B)
            c.io.dirty.expect(false.B)
            c.clock.step()
            // test hit
            c.io.tags_in.poke(0x10.U)
            c.io.write.poke(false.B)
            c.io.update.poke(false.B)
            c.io.aux_index.poke(0.U)
            c.io.aux_tag.poke(0.U)
            c.io.hit.expect(true.B)
            c.io.dirty.expect(false.B)
            c.clock.step()
            // test write 
            
            var i=0
            for(i<-0 to 10){
                // test replace
                if(i>0)c.io.tags_in.poke((i-1).U)
                else c.io.tags_in.poke(0x10.U)
                c.io.update.poke(true.B)
                c.io.write.poke(false.B)
                c.io.aux_tag.poke(i.U)
                println(s"$i , sub_index is ${c.io.sub_index.peek}")                        
                c.io.hit.expect(true.B)
                if(i==3)c.io.dirty.expect(true.B)
                    else c.io.dirty.expect(false.B)
                if(i>4)c.io.tag.expect((i-4).U)
                c.clock.step()
                if(i>3){
                    var j=0
                    for(j<-4 to 0 by -1){
                        c.io.tags_in.poke((i-j).U)
                        c.io.update.poke(false.B)
                        println(s"$j ,${c.io.sub_index.peek},${c.io.hit.peek}")                        
                        if(j!=4)
                            c.io.hit.expect(true.B)
                        else 
                            c.io.hit.expect(false.B)
                        c.clock.step()
                    }
                }
                // if(i>0)c.io.sub_index.expect((i-1).U)
                // else c.io.sub_index.expect(0.U)
            }
        }
    }
}
// WARNING: DON'T run this test unless following the steps below
// update: now can be run without any concern
/****************** steps of usage: ****************************
    1. ucomment line 39 (val data ...) and comment line 42 (val data=...) in Dcache.scala
    2. replace all strings of "data.io" with "io.data" 
***************************************************************/
// WARNING#2: DON'T run this test unless following the steps below
// WARNING#3: DON'T run this test unless following the steps below
class DcacheTester extends FreeSpec with ChiselScalatestTester with CacheParameters{
    "test dcache" in{
        test(new DCacheSimple_test){c=>
            c.io.cpu.req.initSource()
            c.io.cpu.req.setSourceClock(c.clock)
            c.io.cpu.resp.initSink()
            c.io.cpu.resp.setSinkClock(c.clock)
            val input_values=Seq.tabulate(8){i => i.U(32.W)}
            val cpu_request_vector=Seq.tabulate(8){i => 
                // test hit, read
                new MemReq().Lit(_.addr->(i*4).U,_.wdata->0.U,_.wen->false.B,_.flush->false.B,_.invalidate->false.B,_.mtype->0.U)
            }
            var feed="h"
            var j=0
            for (j<-7 to 0 by -1){
                feed+=s"0000${j}000"
            }
            val cpu_request_vector_write=Seq.tabulate(3){i => 
                // test write hit, different access type
                new MemReq().Lit(_.addr->(i*4).U,_.wdata->(i+0x0fffff00+1).U,_.wen->true.B,_.flush->false.B,_.invalidate->false.B,_.mtype->(i%4).U)
            }
            val expected_write=Seq("h2000_00001001".U,"h0000ff02_00001000".U,"h0fffff03_00002000_00001000".U)
            // expected write value; dout from cacheline set to "h2000_00001000"

            val cpu_request_vector_replace=Seq.tabulate(10){i => 
                // test replacement, half write, half read
                new MemReq().Lit(_.addr->((i+1)<<(OffsetBits+IndexBits)).U,_.wdata->"h0fffffff".U,_.wen->(i%2==1).B,_.flush->false.B,_.invalidate->false.B,_.mtype->2.U)
            }
            // val expected_replace=Seq.tabulate(5){i=>

            // }

            // val Cache_request_vector=input_values.map{i => 
            //     new CacheReq().Lit(
            //         _.addr->i,_.valid->true.B,_.wen->false.B,_.data->0.U
            //     )
            // }
            // val Cache_response_vector=Seq.tabulate(10){i => 
            //     new CacheResp().Lit(
            //         _.data->(i+1000).U(256.W),_.valid->true.B
            //     )
            // }
            // val cpu_response_vector=Seq.tabulate(10){i => 
            //     new MemResp().Lit(
            //         _.rdata->(1000+i).U(32.W)
            //     )
            // }
            c.io.cpu.req.valid.poke(true.B)
            c.io.cpu.req.bits.poke(cpu_request_vector(0))
            c.io.bar.req.addr.expect(0.U)
            c.io.bar.req.valid.expect(true.B)
            c.io.bar.req.wen.expect(false.B)
            c.clock.step()
            c.io.bar.resp.data.poke(1000.U)
            c.io.bar.resp.valid.poke(true.B)
            c.io.cpu.resp.bits.rdata(0).expect(1000.U)
            c.io.cpu.resp.bits.rdata(1).expect(0.U)
            c.io.cpu.resp.valid.expect(true.B)
            c.io.data.wea.expect(true.B)
            c.io.data.addra.expect(0.U)
            c.io.data.dina.expect(1000.U)
            c.clock.step()
            c.io.data.douta.poke(1000.U)
            c.io.cpu.resp.bits.rdata(0).expect(1000.U)
            c.io.cpu.resp.bits.rdata(1).expect(0.U)
            // test miss 
            var i=0
            for (i<-0 until 8){
                // test hit
                // FIXME: can't pass if 0 until 8
                c.io.cpu.req.valid.poke(true.B)
                c.io.cpu.req.bits.poke(cpu_request_vector(i))
                c.io.cpu.resp.valid.expect(true.B)
                c.io.data.wea.expect(false.B)
                c.io.data.addra.expect(0.U)
                // c.io.bar.req.addr.expect(0.U)
                c.io.bar.req.valid.expect(false.B)
                // c.io.bar.req.wen.expect(false.B)
                c.clock.step()
                c.io.data.douta.poke(feed.U)
                // c.io.cpu.resp.data(0).expect(1000)
                // c.io.bar.resp.data.poke(1000.U)
                // c.io.bar.resp.valid.poke(true.B)
                println(s"${i},${c.io.cpu.resp.bits.rdata(0).peek},${feed}")
                c.io.cpu.resp.bits.rdata(0).expect(s"h${i}000".U)
                if(i<7)c.io.cpu.resp.bits.rdata(1).expect(s"h${i+1}000".U)
                // c.clock.step()
            }
            
            for(i<-0 to 2){
                // test write hit
                c.io.cpu.req.valid.poke(true.B)
                c.io.cpu.req.bits.poke(cpu_request_vector_write(i))
                c.io.cpu.resp.valid.expect(true.B)
                c.io.data.wea.expect(false.B)
                c.io.data.addra.expect(0.U)
                // c.io.bar.req.addr.expect(0.U)
                c.io.bar.req.valid.expect(false.B)
                // c.io.bar.req.wen.expect(false.B)
                c.clock.step()
                c.io.data.web.expect(true.B)
                c.io.data.douta.poke("h200000001000".U)
                c.io.data.dinb.expect(expected_write(i))
                println(s"written: ${c.io.data.dinb.peek}")

            }

            for(i<-0 until 10){
                // test replace (for directmapped only)
                println(s"${i}")
                c.io.bar.resp.valid.poke(false.B)
                c.io.cpu.req.valid.poke(true.B)
                c.io.cpu.req.bits.poke(cpu_request_vector_replace(i))
                c.io.cpu.resp.valid.expect(false.B)
                c.io.data.wea.expect(false.B)
                if(i%2==0)  // evict needed
                    c.io.bar.req.valid.expect(false.B)
                else {
                    c.io.bar.req.valid.expect(true.B)// not dirty, refill
                    c.io.bar.req.addr.expect(((i+1)<<(OffsetBits+IndexBits)).U)
                    c.io.bar.req.wen.expect(false.B)
                }
                if(i%2==0){
                    // read replace
                    c.clock.step()
                    c.io.data.douta.poke("hdeadbeef".U)
                    c.io.bar.req.addr.expect((i<<(OffsetBits+IndexBits)).U)
                    c.io.bar.req.valid.expect(true.B)
                    c.io.bar.req.wen.expect(true.B)
                    c.io.bar.req.data.expect("hdeadbeef".U)
                    c.clock.step()
                    // c.io.bar.resp.data.poke("h2000_00001000".U)
                    c.io.bar.resp.valid.poke(true.B)
                    // evicted, refill started
                    c.io.bar.req.valid.expect(true.B)
                    c.io.bar.req.wen.expect(false.B)
                    c.io.bar.req.addr.expect(((i+1)<<(OffsetBits+IndexBits)).U)
                    c.clock.step()
                    c.io.bar.resp.data.poke("h2000_00001000".U)
                    c.io.bar.resp.valid.poke(true.B)
                    c.io.cpu.resp.bits.rdata(0).expect("h1000".U)
                    c.io.cpu.resp.bits.rdata(1).expect("h2000".U)  
                    // still in evict state, with bar.resp.valid as que to transfer to s_normal
                }
                else{
                    // write replace
                    c.clock.step()
                    c.io.bar.resp.valid.poke(true.B)
                    c.io.bar.resp.data.poke("h2000_00001000".U)
                    
                    c.clock.step()
                    c.io.data.web.expect(true.B)
                    c.io.data.douta.poke("h2000_00001000".U)
                    c.io.data.dinb.expect("h2000_0fffffff".U)
                    // println(s"written: ${c.io.data.dinb.peek}")
                }
                c.clock.step()                  
            }
        }
    }
}
class ICacheTester extends FreeSpec with ChiselScalatestTester with CacheParameters{
    "test icache" in{
        test(new ICacheSimple_test){c=>
            c.io.cpu.req.initSource()
            c.io.cpu.req.setSourceClock(c.clock)
            c.io.cpu.resp.initSink()
            c.io.cpu.resp.setSinkClock(c.clock)
            val input_values=Seq.tabulate(8){i => i.U(32.W)}
            val cpu_request_vector=Seq.tabulate(8){i => 
                // test hit, read
                new MemReq().Lit(_.addr->(i*4).U,_.wdata->0.U,_.wen->false.B,_.flush->false.B,_.invalidate->false.B,_.mtype->0.U)
            }
            var feed="h"
            var j=0
            for (j<-7 to 0 by -1){
                feed+=s"0000${j}000"
            }

            val cpu_request_vector_replace=Seq.tabulate(10){i => 
                // test replacement, half write, half read
                new MemReq().Lit(_.addr->((i+1)<<(OffsetBits+IndexBits)).U,_.wdata->"h0fffffff".U,_.wen->false.B,_.flush->false.B,_.invalidate->false.B,_.mtype->2.U)
            }
            c.io.cpu.req.valid.poke(true.B)
            c.io.cpu.req.bits.poke(cpu_request_vector(0))
            c.io.bar.req.addr.expect(0.U)
            c.io.bar.req.valid.expect(true.B)
            c.io.bar.req.wen.expect(false.B)
            c.clock.step()
            c.io.bar.resp.data.poke(1000.U)
            c.io.bar.resp.valid.poke(true.B)
            c.io.cpu.resp.bits.rdata(0).expect(1000.U)
            c.io.cpu.resp.bits.rdata(1).expect(0.U)
            c.io.cpu.resp.valid.expect(true.B)
            c.io.data.we.expect(true.B)
            c.io.data.addr.expect(0.U)
            c.io.data.din.expect(1000.U)
            c.clock.step()
            c.io.data.dout.poke(1000.U)
            c.io.cpu.resp.bits.rdata(0).expect(1000.U)
            c.io.cpu.resp.bits.rdata(1).expect(0.U)
            // test miss 
            var i=0
            for (i<-0 until 8){
                // test hit
                c.io.cpu.req.valid.poke(true.B)
                c.io.cpu.req.bits.poke(cpu_request_vector(i))
                c.io.cpu.resp.valid.expect(true.B)
                c.io.data.we.expect(false.B)
                c.io.data.addr.expect(0.U)
                c.io.bar.req.valid.expect(false.B)
                // c.io.bar.req.wen.expect(false.B)
                c.clock.step()
                c.io.data.dout.poke(feed.U)
                println(s"${i},${c.io.cpu.resp.bits.rdata(0).peek},${feed}")
                c.io.cpu.resp.bits.rdata(0).expect(s"h${i}000".U)
                if(i<7)c.io.cpu.resp.bits.rdata(1).expect(s"h${i+1}000".U)
                // c.clock.step()
            }

            for(i<-0 until 10){
                // test replace (for directmapped only)
                println(s"${i}")
                c.io.bar.resp.valid.poke(false.B)
                c.io.cpu.req.valid.poke(true.B)
                c.io.cpu.req.bits.poke(cpu_request_vector_replace(i))
                c.io.cpu.resp.valid.expect(false.B)
                c.io.data.we.expect(false.B)
                c.io.bar.req.valid.expect(true.B)// not dirty, refill
                c.io.bar.req.addr.expect(((i+1)<<(OffsetBits+IndexBits)).U)
                c.io.bar.req.wen.expect(false.B)
                c.clock.step()

                c.io.bar.resp.data.poke("h2000_00001000".U)
                c.io.bar.resp.valid.poke(true.B)
                c.io.cpu.resp.bits.rdata(0).expect("h1000".U)
                c.io.cpu.resp.bits.rdata(1).expect("h2000".U)  
                // still in evict state, with bar.resp.valid as que to transfer to s_normal
                c.clock.step()                  
            }
        }
    }
}
class dev extends FreeSpec with ChiselScalatestTester with CacheParameters{
    "test dcache" in{
        test(new DCacheSimple_test){c=>
            c.io.cpu.req.initSource()
            c.io.cpu.req.setSourceClock(c.clock)
            c.io.cpu.resp.initSink()
            c.io.cpu.resp.setSinkClock(c.clock)
            val input_values=Seq.tabulate(8){i => i.U(32.W)}
            val cpu_request_vector=Seq.tabulate(8){i => 
                // test hit, read
                new MemReq().Lit(_.addr->(i*4).U,_.wdata->0.U,_.wen->false.B,_.flush->false.B,_.invalidate->false.B,_.mtype->0.U)
            }
            var feed="h"
            var j=0
            for (j<-7 to 0 by -1){
                feed+=s"0000${j}000"
            }
            val cpu_request_vector_write=Seq.tabulate(3){i => 
                // test write hit, different access type
                new MemReq().Lit(_.addr->(i).U,_.wdata->(i+0x0fffff00+1).U,_.wen->true.B,_.flush->false.B,_.invalidate->false.B,_.mtype->(2-i).U)
            }
            val expected_write=Seq("h2000_0fffff01".U,"h2000_00ff0200".U,"h2000_00031000".U)
            // expected write value; dout from cacheline set to "h2000_00001000"

            val cpu_request_vector_replace=Seq.tabulate(10){i => 
                // test replacement, half write, half read
                new MemReq().Lit(_.addr->((i+1)<<(OffsetBits+IndexBits)).U,_.wdata->"h0fffffff".U,_.wen->(i%2==1).B,_.flush->false.B,_.invalidate->false.B,_.mtype->2.U)
            }
            // val expected_replace=Seq.tabulate(5){i=>

            // }

            // val Cache_request_vector=input_values.map{i => 
            //     new CacheReq().Lit(
            //         _.addr->i,_.valid->true.B,_.wen->false.B,_.data->0.U
            //     )
            // }
            // val Cache_response_vector=Seq.tabulate(10){i => 
            //     new CacheResp().Lit(
            //         _.data->(i+1000).U(256.W),_.valid->true.B
            //     )
            // }
            // val cpu_response_vector=Seq.tabulate(10){i => 
            //     new MemResp().Lit(
            //         _.rdata->(1000+i).U(32.W)
            //     )
            // }
            c.io.cpu.req.valid.poke(true.B)
            c.io.cpu.req.bits.poke(cpu_request_vector(0))
            c.io.bar.req.addr.expect(0.U)
            c.io.bar.req.valid.expect(true.B)
            c.io.bar.req.wen.expect(false.B)
            c.clock.step()
            c.io.bar.resp.data.poke(1000.U)
            c.io.bar.resp.valid.poke(true.B)
            c.io.cpu.resp.bits.rdata(0).expect(1000.U)
            c.io.cpu.resp.bits.rdata(1).expect(0.U)
            c.io.cpu.resp.valid.expect(true.B)
            c.io.data.wea.expect(true.B)
            c.io.data.addra.expect(0.U)
            c.io.data.dina.expect(1000.U)
            c.clock.step()
            c.io.data.douta.poke(1000.U)
            c.io.cpu.resp.bits.rdata(0).expect(1000.U)
            c.io.cpu.resp.bits.rdata(1).expect(0.U)
            // test miss 
            var i=0
            for (i<-0 until 8){
                // test hit
                // FIXME: can't pass if 0 until 8
                c.io.cpu.req.valid.poke(true.B)
                c.io.cpu.req.bits.poke(cpu_request_vector(i))
                c.io.cpu.resp.valid.expect(true.B)
                c.io.data.wea.expect(false.B)
                c.io.data.addra.expect(0.U)
                // c.io.bar.req.addr.expect(0.U)
                c.io.bar.req.valid.expect(false.B)
                // c.io.bar.req.wen.expect(false.B)
                c.clock.step()
                c.io.data.douta.poke(feed.U)
                // c.io.cpu.resp.data(0).expect(1000)
                // c.io.bar.resp.data.poke(1000.U)
                // c.io.bar.resp.valid.poke(true.B)
                println(s"${i},${c.io.cpu.resp.bits.rdata(0).peek},${feed}")
                c.io.cpu.resp.bits.rdata(0).expect(s"h${i}000".U)
                if(i<7)c.io.cpu.resp.bits.rdata(1).expect(s"h${i+1}000".U)
                // c.clock.step()
            }
            
            for(i<-0 to 2){
                // test write hit
                c.io.cpu.req.valid.poke(true.B)
                c.io.cpu.req.bits.poke(cpu_request_vector_write(i))
                c.io.cpu.resp.valid.expect(true.B)
                c.io.data.wea.expect(false.B)
                c.io.data.addra.expect(0.U)
                // c.io.bar.req.addr.expect(0.U)
                c.io.bar.req.valid.expect(false.B)
                // c.io.bar.req.wen.expect(false.B)
                c.clock.step()
                c.io.data.web.expect(true.B)
                c.io.data.douta.poke("h200000001000".U)
                c.io.data.dinb.expect(expected_write(i))
                println(s"written: ${c.io.data.dinb.peek}")

            }

            for(i<-0 until 10){
                // test replace (for directmapped only)
                println(s"${i}")
                c.io.bar.resp.valid.poke(false.B)
                c.io.cpu.req.valid.poke(true.B)
                c.io.cpu.req.bits.poke(cpu_request_vector_replace(i))
                c.io.cpu.resp.valid.expect(false.B)
                c.io.data.wea.expect(false.B)
                if(i%2==0)  // evict needed
                    c.io.bar.req.valid.expect(false.B)
                else {
                    c.io.bar.req.valid.expect(true.B)// not dirty, refill
                    c.io.bar.req.addr.expect(((i+1)<<(OffsetBits+IndexBits)).U)
                    c.io.bar.req.wen.expect(false.B)
                }
                if(i%2==0){
                    // read replace
                    c.clock.step()
                    c.io.data.douta.poke("hdeadbeef".U)
                    c.io.bar.req.addr.expect((i<<(OffsetBits+IndexBits)).U)
                    c.io.bar.req.valid.expect(true.B)
                    c.io.bar.req.wen.expect(true.B)
                    c.io.bar.req.data.expect("hdeadbeef".U)
                    c.clock.step()
                    // c.io.bar.resp.data.poke("h2000_00001000".U)
                    c.io.bar.resp.valid.poke(true.B)
                    // evicted, refill started
                    c.io.bar.req.valid.expect(true.B)
                    c.io.bar.req.wen.expect(false.B)
                    c.io.bar.req.addr.expect(((i+1)<<(OffsetBits+IndexBits)).U)
                    c.clock.step()
                    c.io.bar.resp.data.poke("h2000_00001000".U)
                    c.io.bar.resp.valid.poke(true.B)
                    c.io.cpu.resp.bits.rdata(0).expect("h1000".U)
                    c.io.cpu.resp.bits.rdata(1).expect("h2000".U)  
                    // still in evict state, with bar.resp.valid as que to transfer to s_normal
                }
                else{
                    // write replace
                    c.clock.step()
                    c.io.bar.resp.valid.poke(true.B)
                    c.io.bar.resp.data.poke("h2000_00001000".U)
                    
                    c.clock.step()
                    c.io.data.web.expect(true.B)
                    c.io.data.douta.poke("h2000_00001000".U)
                    c.io.data.dinb.expect("h2000_0fffffff".U)
                    // println(s"written: ${c.io.data.dinb.peek}")
                }
                c.clock.step()                  
            }
        }
    }
}

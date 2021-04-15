package cache_test
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import chisel3.tester._
import chisel3.tester.RawTester.test
import org.scalatest.FreeSpec
import cache._
// import icore._
class cache_test extends FreeSpec with ChiselScalatestTester with Cache_Parameters{
    "test the interface protocol and simple miss/hit scenario" in{
        // still havn't figure how to construct input for IO(Vec(...))
        test(new ICache1WayDummy(256)){ c =>
            c.io.cpu.req.initSource()
            c.io.cpu.req.setSourceClock(c.clock)
            c.io.AXI.resp.initSource()
            c.io.AXI.resp.setSourceClock(c.clock)
            c.io.AXI.req.initSink()
            c.io.AXI.req.setSinkClock(c.clock)
            c.io.cpu.resp.initSink()
            c.io.cpu.resp.setSinkClock(c.clock)
            val k=10
            val input_values=Seq.tabulate(k){i => (i<<6).U(32.W)}
            val cpu_request_vector=input_values.map{i => 
                new MemReq().Lit(_.addr->i,_.wdata->0.U,_.wen->false.B,_.flush->false.B,_.invalidate->false.B,_.mtype->0.U)
            }
            val AXI_request_vector=input_values.map{i => 
                new AXI4IO_Req().Lit(_.addr->i)
            }
            
            val AXI_response_vector=Seq.tabulate(k){i => 
                new AXIResp().Lit(
                    _.rdata->(1000+i).U(512.W)
                )
            }
            val cpu_response_vector=Seq.tabulate(k){i => 
                new MemResp().Lit(
                    _.rdata->(1000+i).U(32.W)
                )
            }
            // println(cpu_request_vector)
            c.io.cpu.req.enqueueNow(cpu_request_vector(4))

            c.io.AXI.req.expectDequeueNow(AXI_request_vector(4))
            // fork{
            //     c.io.cpu.req.enqueueSeq(cpu_request_vector)     
            // }
            // .fork{
            //     fork{
            //         c.io.AXI.req.expectDequeueSeq(AXI_request_vector)     
            //     }
            //     .fork{
            //         c.io.AXI.resp.enqueueSeq(AXI_response_vector)  
            //     }
            //     .join()
            // }
            // .fork{
            //     c.io.cpu.resp.expectDequeueSeq(cpu_response_vector)     
            // }.join()
            
            fork{
                c.io.cpu.req.enqueueSeq(cpu_request_vector)     
            }.fork{
                c.io.cpu.resp.expectDequeueSeq(cpu_response_vector)
            }.join()
        }
    }
}
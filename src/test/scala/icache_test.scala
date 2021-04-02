import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import chisel3.tester._
import chisel3.tester.RawTester.test
import cache._
import icore._
class cache_test {

// "test the interface protocol and simple miss/hit scenario" in{
    test(new ICache1WayDummy(256)){ c =>
        c.io.cpu.req.initSource()
        c.io.cpu.req.setSourceClock(c.clock)
        c.io.AXI.resp.initSource()
        c.io.AXI.resp.setSourceClock(c.clock)
        c.io.AXI.req.initSink()
        c.io.AXI.req.setSinkClock(c.clock)
        c.io.cpu.resp.initSink()
        c.io.cpu.resp.setSinkClock(c.clock)

        val input_values=Seq.tabulate(100){i => i.U}
        val cpu_request_vector=input_values.map{i => 
            new MemReq().Lit(_.addr->i)
        }
        val AXI_request_vector=input_values.map{i => 
            new AXI4IO_Req().Lit(_.addr->i)
        }
        
        val AXI_response_vector=Seq.tabulate(100){i => 
            new MemResp().Lit(_.rdata->(1000+i).U)
        }
        fork{
            c.io.cpu.req.enqueueSeq(cpu_request_vector)     
        }
        .fork{
            fork{
                c.io.AXI.req.expectDequeueSeq(AXI_request_vector)     
            }
            .fork{
                c.io.AXI.resp.enqueueSeq(AXI_response_vector)  
            }
            .join()
        }
        .fork{
            c.io.cpu.resp.expectDequeueSeq(AXI_response_vector)     
        }.join()
        
        fork{
            c.io.cpu.req.enqueueSeq(cpu_request_vector)     
        }.fork{
            c.io.cpu.resp.expectDequeueSeq(AXI_response_vector)
        }.join()
    }
}
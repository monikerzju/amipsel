package cache

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import chisel3.tester._
import chisel3.tester.RawTester.test
test(new ICache1WayDummy(256)) { c =>
    c.io.cpu.req.initSource()
    c.io.cpu.req.setSourceClock(c.clock)
    c.io.AXI.resp.initSource()
    c.io.AXI.resp.setSourceClock(c.clock)
    c.io.AXI.req.initSink()
    c.io.AXI.req.setSinkClock(c.clock)
    c.io.cpu.resp.initSink()
    c.io.cpu.resp.setSinkClock(c.clock)

    val cpu_request_vector=Seq.tabulate(100){i => i.U}
    val AXI_response_vector=Seq.tabulate(100){i => (i+0x1000).U}
    fork{
        c.io.cpu.req.enqueueSeq(cpu_request_vector)     
    }
    .fork{
        fork{
            c.io.AXI.req.expectDequeueSeq(cpu_request_vector)     
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
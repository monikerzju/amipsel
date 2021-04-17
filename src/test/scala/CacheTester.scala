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
class CacheTester extends FreeSpec with ChiselScalatestTester with CacheParameters{
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
class dev extends FreeSpec with ChiselScalatestTester with CacheParameters{
}
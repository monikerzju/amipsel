package tile
import cache._
import chisel3._
import chisel3.stage._

// tile(Yuan) = core + cache + uncache
// core = datapath(Shen) + control(Shen) + coprocessor(Yuan)
// cache = icache(Shi) + dcache(Shi)

// AXI3 Protocol according to Loongson
// Interrupt(s)
class TileIO extends Bundle {

}

class Tile extends Module {
  val io = IO(new TileIO)

//   val core = Module(new Core)
//   val icache = Module(new ICache)
//   val dcache = Module(new DCache)
//   val xbar = Module(new XBar)
}

// run `sbt "runMain tile.Main"` in terminal
object Main {
  def main(args: Array[String]): Unit = {
    val packageName = this.getClass.getPackage.getName

    (new chisel3.stage.ChiselStage).execute(
      Array("-td", "build/verilog/"+packageName, "-X", "verilog"),
      Seq(ChiselGeneratorAnnotation(() => new  ICacheAXI)))
  }
}
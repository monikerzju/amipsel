package tile

import chisel3._
import chisel3.stage._
import fu._
import icore._
import cache._
import conf.Config
import cache.CacheParameters
import fu.CauseExcCode

// tile(Yuan) = core + cache + uncache
// core = datapath(Shen) + control(Shen) + coprocessor(Yuan)
// cache = icache(Shi) + dcache(Shi)

// AXI3 Protocol according to Loongson
// Interrupt(s)
class TileIO extends Bundle with Config with CauseExcCode {
  val axi3 = new AXI3(1, len)
  val intr = Input(Vec(SZ_HARD_INT, Bool()))
}

class Tile extends Module with Config with CacheParameters {
  val io = IO(new TileIO)

  val core = Module(new Core)
  val icache = Module(new ICacheSimple)

  // TODO FIX DCACHE = ICACHE
  val dcache = Module(new ICacheSimple)
  val xbar = Module(new AXI3Server(2, 1 << (OffsetBits + 3), 1, "Seq", len))

  core.io.interrupt := io.intr
  core.io.icache <> icache.io.cpu
  core.io.dcache <> dcache.io.cpu
  // Serve I$ first
  xbar.io.cache(0) <> icache.io.bar
  xbar.io.cache(1) <> dcache.io.bar

  io.axi3 <> xbar.io.axi3
}

// run `sbt "runMain tile.GenT"` in terminal
object GenT {
  def main(args: Array[String]): Unit = {
    val packageName = this.getClass.getPackage.getName

    (new chisel3.stage.ChiselStage).execute(
      Array("-td", "build/verilog/"+packageName, "-X", "verilog"),
      Seq(ChiselGeneratorAnnotation(() => new Tile)))
  }
}

// run `sbt "runMain tile.GenC"` in terminal
object GenC {
  def main(args: Array[String]): Unit = {
    val packageName = this.getClass.getPackage.getName

    (new chisel3.stage.ChiselStage).execute(
      Array("-td", "build/verilog/"+packageName, "-X", "verilog"),
      Seq(ChiselGeneratorAnnotation(() => new Core)))
  }
}
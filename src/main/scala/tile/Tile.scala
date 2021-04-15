package tile
import cache._
import chisel3._
import chisel3.stage._
import fu._
import icore._
import cache._
import conf.Config
import cache.CacheParameters
import fu.CauseExcCode
import chisel3.util.experimental.BoringUtils

class DiffTestVIO extends Bundle with Config {
  val wb_pc   = Output(Vec(backendIssueN, UInt(len.W)))
  val wb_wen  = Output(Vec(backendIssueN, Bool()))
  val wb_data = Output(Vec(backendIssueN, UInt(len.W)))
  val wb_nreg = Output(Vec(backendIssueN, UInt(5.W)))
}

// AXI3 Protocol according to Loongson
// Interrupt(s)
class TileIO extends Bundle with Config with CauseExcCode {
  val axi3 = new AXI3(1, len)
  val intr = Input(Vec(SZ_HARD_INT, Bool()))
  val debug = if (diffTestV) new DiffTestVIO else null
}

class Tile extends Module with Config with CacheParameters {
  val io = IO(new TileIO)

  if (diffTestV) {
    val debug = WireInit(0.U.asTypeOf(new DiffTestVIO))

    BoringUtils.addSink(debug.wb_pc,   "dt_pc"   )
    BoringUtils.addSink(debug.wb_wen,  "dt_wen"  )
    BoringUtils.addSink(debug.wb_data, "dt_data" )
    BoringUtils.addSink(debug.wb_nreg, "dt_nreg" )

    io.debug := debug
  }

  val core = Module(new Core)
  val icache = Module(new ICacheSimple)

  // TODO FIX DCACHE = ICACHE
  val dcache = Module(new DCacheSimple)
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
      Seq(ChiselGeneratorAnnotation(() => new  Core)))
  }
}
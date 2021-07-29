package tile
import chisel3._
import chisel3.stage._
import fu._
import icore._
import cache._
import conf.Config
import fu.CauseExcCode
import chisel3.util.experimental.BoringUtils

class DiffTestVIO extends Bundle with Config {
  val wb_pc   = Output(Vec(backendFuN, UInt(len.W)))
  val wb_wen  = Output(Vec(backendFuN, Bool()))
  val wb_data = Output(Vec(backendFuN, UInt(len.W)))
  val wb_nreg = Output(Vec(backendFuN, UInt(5.W)))
}

// AXI3 Protocol according to Loongson
// Interrupt(s)
class TileIO(diffTestV: Boolean) extends Bundle with Config with CauseExcCode {
  val axi3 = new AXI3(4, len)
  val intr = Input(Vec(SZ_HARD_INT, Bool()))
  val debug = if (diffTestV) new DiffTestVIO else null
  override def cloneType = (new TileIO(diffTestV)).asInstanceOf[this.type]
}

class Tile(diffTestV: Boolean) extends Module with Config {
  val io = IO(new TileIO(diffTestV))

  if (diffTestV) {
    val debug = WireInit(0.U.asTypeOf(new DiffTestVIO))

    BoringUtils.addSink(debug.wb_pc,   "dt_pc"   )
    BoringUtils.addSink(debug.wb_wen,  "dt_wen"  )
    BoringUtils.addSink(debug.wb_data, "dt_data" )
    BoringUtils.addSink(debug.wb_nreg, "dt_nreg" )

    io.debug := debug
  }

  val core = Module(new Core(diffTestV))
  val icache = Module(new ICache)
  val dcache = Module(new DCacheSimple)
  val xbar = Module(new AXI3Server(2, 1 << (offsetBits + 3), 4, "Seq", len, 1, 0))

  core.io.interrupt := io.intr
  core.io.icache <> icache.io.cpu
  core.io.dcache <> dcache.io.cpu
  
  // Serve D$ first
  xbar.io.cache(0) <> dcache.io.bar
  xbar.io.cache(1) <> icache.io.bar

  io.axi3 <> xbar.io.axi3
}

// Verilator Shell, only for Loooooongson CUP, not configurable for simple
class TileForVerilator extends RawModule {
  override val desiredName = s"mycpu_top"

  val aclk         = IO(Input(Clock()))
  val aresetn      = IO(Input(Bool()))
  val ext_int      = IO(Input(UInt(6.W)))
  // AXI
  val arid         = IO(Output(UInt(4.W)))
  val araddr       = IO(Output(UInt(32.W)))
  val arlen        = IO(Output(UInt(8.W)))
  val arsize       = IO(Output(UInt(3.W)))
  val arburst      = IO(Output(UInt(2.W)))
  val arlock       = IO(Output(UInt(2.W)))
  val arcache      = IO(Output(UInt(4.W)))
  val arprot       = IO(Output(UInt(3.W)))
  val arvalid      = IO(Output(Bool()))
  val arready      = IO(Input(Bool()))
  val rid          = IO(Input(UInt(4.W)))
  val rdata        = IO(Input(UInt(32.W)))
  val rresp        = IO(Input(UInt(2.W)))
  val rlast        = IO(Input(Bool()))
  val rvalid       = IO(Input(Bool()))
  val rready       = IO(Output(Bool()))
  val awid         = IO(Output(UInt(4.W)))
  val awaddr       = IO(Output(UInt(32.W)))
  val awlen        = IO(Output(UInt(8.W)))
  val awsize       = IO(Output(UInt(3.W)))
  val awburst      = IO(Output(UInt(2.W)))
  val awlock       = IO(Output(UInt(2.W)))
  val awcache      = IO(Output(UInt(4.W)))
  val awprot       = IO(Output(UInt(3.W)))
  val awvalid      = IO(Output(Bool()))
  val awready      = IO(Input(Bool()))
  val wid          = IO(Output(UInt(4.W)))
  val wdata        = IO(Output(UInt(32.W)))
  val wstrb        = IO(Output(UInt(4.W)))
  val wlast        = IO(Output(Bool()))
  val wvalid       = IO(Output(Bool()))
  val wready       = IO(Input(Bool()))
  val bid          = IO(Input(UInt(4.W)))
  val bresp        = IO(Input(UInt(2.W)))
  val bvalid       = IO(Input(Bool()))
  val bready       = IO(Output(Bool()))
  // DBG
  val debug_wb_pc       = IO(Output(UInt(32.W)))
  val debug_wb_rf_wen   = IO(Output(UInt(4.W)))
  val debug_wb_rf_wnum  = IO(Output(UInt(5.W)))
  val debug_wb_rf_wdata = IO(Output(UInt(32.W)))

  withClockAndReset(aclk, !aresetn) {
    val tile = Module(new Tile(false))

    arid      <>    tile.io.axi3.ar.bits.id 
    araddr    <>    tile.io.axi3.ar.bits.addr   
    arlen     <>    tile.io.axi3.ar.bits.len  
    arsize    <>    tile.io.axi3.ar.bits.size   
    arburst   <>    tile.io.axi3.ar.bits.burst    
    arlock    <>    tile.io.axi3.ar.bits.lock   
    arcache   <>    tile.io.axi3.ar.bits.cache    
    arprot    <>    tile.io.axi3.ar.bits.prot   
    arvalid   <>    tile.io.axi3.ar.valid    
    arready   <>    tile.io.axi3.ar.ready   

    rid       <>    tile.io.axi3.r.bits.id
    rdata     <>    tile.io.axi3.r.bits.data  
    rresp     <>    tile.io.axi3.r.bits.resp  
    rlast     <>    tile.io.axi3.r.bits.last  
    rvalid    <>    tile.io.axi3.r.valid   
    rready    <>    tile.io.axi3.r.ready 

    awid      <>    tile.io.axi3.aw.bits.id 
    awaddr    <>    tile.io.axi3.aw.bits.addr   
    awlen     <>    tile.io.axi3.aw.bits.len  
    awsize    <>    tile.io.axi3.aw.bits.size   
    awburst   <>    tile.io.axi3.aw.bits.burst    
    awlock    <>    tile.io.axi3.aw.bits.lock   
    awcache   <>    tile.io.axi3.aw.bits.cache    
    awprot    <>    tile.io.axi3.aw.bits.prot   
    awvalid   <>    tile.io.axi3.aw.valid    
    awready   <>    tile.io.axi3.aw.ready    

    wid       <>    tile.io.axi3.w.bits.id
    wdata     <>    tile.io.axi3.w.bits.data  
    wstrb     <>    tile.io.axi3.w.bits.strb  
    wlast     <>    tile.io.axi3.w.bits.last  
    wvalid    <>    tile.io.axi3.w.valid   
    wready    <>    tile.io.axi3.w.ready 

    bid       <>    tile.io.axi3.b.bits.id
    bresp     <>    tile.io.axi3.b.bits.resp  
    bvalid    <>    tile.io.axi3.b.valid   
    bready    <>    tile.io.axi3.b.ready   

    tile.io.intr := ext_int.asBools

    debug_wb_pc       := 0.U
    debug_wb_rf_wen   := 0.U 
    debug_wb_rf_wnum  := 0.U
    debug_wb_rf_wdata := 0.U 
  }
}

// run `sbt "runMain tile.GenT"` in terminal
object GenT {
  def main(args: Array[String]): Unit = {
    val packageName = this.getClass.getPackage.getName
    (new chisel3.stage.ChiselStage).execute(
      Array("-td", "build/verilog/"+packageName, "-X", "verilog"),
      Seq(ChiselGeneratorAnnotation(() => new Tile(args.contains("-diff")))))
  }
}

// run `sbt "runMain tile.GenC"` in terminal
object GenC {
  def main(args: Array[String]): Unit = {
    val packageName = this.getClass.getPackage.getName

    (new chisel3.stage.ChiselStage).execute(
      Array("-td", "build/verilog/"+packageName, "-X", "verilog"),
      Seq(ChiselGeneratorAnnotation(() => new Core(args.contains("-diff")))))
  }
}

object GenTV {
  def main(args: Array[String]): Unit = {
    val packageName = this.getClass.getPackage.getName

    (new chisel3.stage.ChiselStage).execute(
      Array("-td", "build/verilog/"+packageName, "-X", "verilog"),
      Seq(ChiselGeneratorAnnotation(() => new TileForVerilator)))
  }
}

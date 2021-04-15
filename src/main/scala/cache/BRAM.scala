package cache

import chisel3._
import chisel3.util._

// There are 2 types of RAM, true dual port ram and single port ram

// Dual Port BRAM
class DPBRAMWrapperIO(width: Int = 128, depth: Int = 16) extends Bundle {
  val clk = Input(Clock())
  val rst = Input(Reset())
  val wea = Input(Bool())
  val web = Input(Bool())
  val ena = Input(Bool())
  val enb = Input(Bool())
  val addra = Input(UInt(log2Ceil(depth).W))
  val addrb = Input(UInt(log2Ceil(depth).W))
  val dina = Input(UInt(width.W))
  val dinb = Input(UInt(width.W))
  val douta = Output(UInt(width.W))
  val doutb = Output(UInt(width.W))
}

class dual_port_ram(DATA_WIDTH: Int, DEPTH: Int, LATENCY: Int = 1) extends BlackBox(Map("DATA_WIDTH" -> DATA_WIDTH,
                                                                                        "DEPTH" -> DEPTH,
                                                                                        "LATENCY" -> LATENCY)) with HasBlackBoxInline {
  val io = IO(new DPBRAMWrapperIO(DATA_WIDTH, DEPTH))

  setInline("dual_port_ram.v",
  s"""
    module dual_port_ram #(
    	parameter DATA_WIDTH = 32,
    	parameter DEPTH      = 1024,
    	parameter LATENCY    = 1,
    	parameter LATENCY_A  = LATENCY,
    	parameter LATENCY_B  = LATENCY
    ) (
    	input  clk,
    	input  rst,
    	input  wea,
    	input  web,
    	input  ena,
    	input  enb,
    	input  [$$clog2(DEPTH)-1:0] addra,
    	input  [$$clog2(DEPTH)-1:0] addrb,
    	input  [DATA_WIDTH-1:0] dina,
    	input  [DATA_WIDTH-1:0] dinb,
    	output [DATA_WIDTH-1:0] douta,
    	output [DATA_WIDTH-1:0] doutb
    );

    // xpm_memory_tdpram: True Dual Port RAM
    // Xilinx Parameterized Macro, Version 2016.2
    xpm_memory_tdpram #(
    	// Common module parameters
    	.MEMORY_SIZE(DATA_WIDTH * DEPTH),
    	.MEMORY_PRIMITIVE("auto"),
    	.CLOCKING_MODE("common_clock"),
    	.USE_MEM_INIT(0),
    	.WAKEUP_TIME("disable_sleep"),
    	.MESSAGE_CONTROL(0),

    	// Port A module parameters
    	.WRITE_DATA_WIDTH_A(DATA_WIDTH),
    	.READ_DATA_WIDTH_A(DATA_WIDTH),
    	.READ_RESET_VALUE_A("0"),
    	.READ_LATENCY_A(LATENCY_A),
    	.WRITE_MODE_A("read_first"),

    	// Port B module parameters
    	.WRITE_DATA_WIDTH_B(DATA_WIDTH),
    	.READ_DATA_WIDTH_B(DATA_WIDTH),
    	.READ_RESET_VALUE_B("0"),
    	.READ_LATENCY_B(LATENCY_B),
    	.WRITE_MODE_B("read_first")
    ) xpm_mem (
    	// Common module ports
    	.sleep          ( 1'b0  ),

    	// Port A module ports
    	.clka           ( clk   ),
    	.rsta           ( rst   ),
    	.ena            ( ena   ),
    	.regcea         ( 1'b0  ),
    	.wea            ( wea   ),
    	.addra          ( addra ),
    	.dina           ( dina  ),
    	.injectsbiterra ( 1'b0  ), // do not change
    	.injectdbiterra ( 1'b0  ), // do not change
    	.douta          ( douta ),
    	.sbiterra       (       ), // do not change
    	.dbiterra       (       ), // do not change

    	// Port B module ports
    	.clkb           ( clk   ),
    	.rstb           ( rst   ),
    	.enb            ( enb   ),
    	.regceb         ( 1'b0  ),
    	.web            ( web   ),
    	.addrb          ( addrb ),
    	.dinb           ( dinb  ),
    	.injectsbiterrb ( 1'b0  ), // do not change
    	.injectdbiterrb ( 1'b0  ), // do not change
    	.doutb          ( doutb ),
    	.sbiterrb       (       ), // do not change
    	.dbiterrb       (       )  // do not change
    );

    endmodule
   """.stripMargin)
}

class DPBRAMSyncReadMemIO(DATA_WIDTH: Int, DEPTH: Int) extends Bundle {
  val wea   = Input(Bool())
  val web   = Input(Bool())
  val addra = Input(UInt(log2Ceil(DEPTH).W))
  val addrb = Input(UInt(log2Ceil(DEPTH).W))
  val dina  = Input(UInt(DATA_WIDTH.W))
  val dinb  = Input(UInt(DATA_WIDTH.W))
  val douta = Output(UInt(DATA_WIDTH.W))
  val doutb = Output(UInt(DATA_WIDTH.W))
}

class DPBRAMSyncReadMem(DEPTH: Int, DATA_WIDTH: Int, LATENCY: Int = 1) extends Module {
  val dpr = Module(new dual_port_ram(DATA_WIDTH, DEPTH, LATENCY))
  val io = IO(new DPBRAMSyncReadMemIO(DATA_WIDTH, DEPTH))

  dpr.io.clk    := clock
  dpr.io.rst    := reset
  dpr.io.wea    := io.wea
  dpr.io.web    := io.web
  dpr.io.ena    := true.B
  dpr.io.enb    := true.B
  dpr.io.addra  := io.addra
  dpr.io.addrb  := io.addrb
  dpr.io.dina   := io.dina
  dpr.io.dinb   := io.dinb
  io.douta      := dpr.io.douta
  io.doutb      := dpr.io.doutb
}

// Single port
class BRAMWrapperIO(width: Int = 128, depth: Int = 16) extends Bundle {
  val clk = Input(Clock())
  val rst = Input(Reset())
  val we  = Input(Bool())
  val addr = Input(UInt(log2Ceil(depth).W))
  val din = Input(UInt(width.W))
  val dout = Output(UInt(width.W))
  override def cloneType = (new BRAMWrapperIO(width,depth)).asInstanceOf[this.type]
}

class single_port_ram(DATA_WIDTH: Int, DEPTH: Int, LATENCY: Int = 1) extends BlackBox(Map("DATA_WIDTH" -> DATA_WIDTH,
                                                                                        "DEPTH" -> DEPTH,
                                                                                        "LATENCY" -> LATENCY)) with HasBlackBoxInline {
  val io = IO(new BRAMWrapperIO(DATA_WIDTH, DEPTH))

  setInline("single_port_ram.v",
  s"""
    module single_port_ram # (
      parameter DATA_WIDTH = 32,
    	parameter DEPTH      = 1024,
    	parameter LATENCY    = 1
    )(
    	input  clk,
    	input  rst,
    	input  we,
    	input  [$$clog2(DEPTH)-1:0] addr,
    	input  [DATA_WIDTH-1:0]  din,
    	output [DATA_WIDTH-1:0]  dout
    );

    // xpm_memory_spram: Single Port RAM
    // Xilinx Parameterized Macro, Version 2016.2
    xpm_memory_spram #(
    	// Common module parameters
    	.MEMORY_SIZE(DATA_WIDTH * DEPTH),
    	.MEMORY_PRIMITIVE("auto"),
    	.USE_MEM_INIT(0),
    	.WAKEUP_TIME("disable_sleep"),
    	.MESSAGE_CONTROL(0),

    	// Port A module parameters
    	.WRITE_DATA_WIDTH_A(DATA_WIDTH),
    	.READ_DATA_WIDTH_A(DATA_WIDTH),
    	.READ_RESET_VALUE_A("0"),
    	.READ_LATENCY_A(LATENCY),
    	.WRITE_MODE_A("write_first")
    ) xpm_mem (
    	// Common module ports
    	.sleep          ( 1'b0  ),

    	// Port A module ports
    	.clka           ( clk   ),
    	.rsta           ( rst   ),
    	.ena            ( 1'b1  ),
    	.regcea         ( 1'b0  ),
    	.wea            ( we    ),
    	.addra          ( addr  ),
    	.dina           ( din   ),
    	.injectsbiterra ( 1'b0  ), // do not change
    	.injectdbiterra ( 1'b0  ), // do not change
    	.douta          ( dout  ),
    	.sbiterra       (       ), // do not change
    	.dbiterra       (       )  // do not change
    );

    endmodule
   """.stripMargin)
}

class BRAMSyncReadMemIO(DATA_WIDTH: Int, DEPTH: Int) extends Bundle {
  val we    = Input(Bool())
  val addr  = Input(UInt(log2Ceil(DEPTH).W))
  val din   = Input(UInt(DATA_WIDTH.W))
  val dout  = Output(UInt(DATA_WIDTH.W))
  override def cloneType = (new BRAMSyncReadMemIO(DATA_WIDTH, DEPTH)).asInstanceOf[this.type]
}

class BRAMSyncReadMem(DEPTH: Int, DATA_WIDTH: Int, LATENCY: Int = 1) extends Module {
  val spr = Module(new single_port_ram(DATA_WIDTH, DEPTH, LATENCY))
  val io = IO(new BRAMSyncReadMemIO(DATA_WIDTH, DEPTH))

  spr.io.clk    := clock
  spr.io.rst    := reset
  spr.io.we     := io.we
  spr.io.addr   := io.addr
  spr.io.din    := io.din
  io.dout       := spr.io.dout
}

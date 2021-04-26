/*------------------------------------------------------------------------------
--------------------------------------------------------------------------------
Copyright (c) 2016, Loongson Technology Corporation Limited.

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this 
list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, 
this list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

3. Neither the name of Loongson Technology Corporation Limited nor the names of 
its contributors may be used to endorse or promote products derived from this 
software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL LOONGSON TECHNOLOGY CORPORATION LIMITED BE LIABLE
TO ANY PARTY FOR DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------
------------------------------------------------------------------------------*/
`timescale 1ns / 1ps

`define TRACE_REF_FILE "C:/Users/mywindows/Desktop/function_test/function_test.srcs/sim_1/imports/testbench/golden_trace.txt"
`define CONFREG_NUM_REG      soc_lite.u_confreg.num_data
`define CONFREG_OPEN_TRACE   soc_lite.u_confreg.open_trace
`define CONFREG_NUM_MONITOR  soc_lite.u_confreg.num_monitor
`define CONFREG_UART_DISPLAY soc_lite.u_confreg.write_uart_valid
`define CONFREG_UART_DATA    soc_lite.u_confreg.write_uart_data
`define END_PC 32'hbfc00100

module tb_top( );
reg resetn;
reg clk;

//goio
wire [15:0] led;
wire [1 :0] led_rg0;
wire [1 :0] led_rg1;
wire [7 :0] num_csn;
wire [6 :0] num_a_g;
wire [7 :0] switch;
wire [3 :0] btn_key_col;
wire [3 :0] btn_key_row;
wire [1 :0] btn_step;
assign switch      = 8'hff;
assign btn_key_row = 4'd0;
assign btn_step    = 2'd3;

initial
begin
    clk = 1'b0;
    resetn = 1'b0;
    #2000;
    resetn = 1'b1;
end
always #5 clk=~clk;
soc_axi_lite_top #(.SIMULATION(1'b1)) soc_lite
(
       .resetn      (resetn     ), 
       .clk         (clk        ),
    
        //------gpio-------
        .num_csn    (num_csn    ),
        .num_a_g    (num_a_g    ),
        .led        (led        ),
        .led_rg0    (led_rg0    ),
        .led_rg1    (led_rg1    ),
        .switch     (switch     ),
        .btn_key_col(btn_key_col),
        .btn_key_row(btn_key_row),
        .btn_step   (btn_step   )
    );   

//"cpu_clk" means cpu core clk
//"sys_clk" means system clk
//"wb" means write-back stage in pipeline
//"rf" means regfiles in cpu
//"w" in "wen/wnum/wdata" means writing
wire cpu_clk;
wire sys_clk;
wire [31:0] debug_wb_pc_0;
wire debug_wb_rf_wen_0;
wire [4 :0] debug_wb_rf_wnum_0;
wire [31:0] debug_wb_rf_wdata_0;
wire [31:0] debug_wb_pc_1;
wire debug_wb_rf_wen_1;
wire [4 :0] debug_wb_rf_wnum_1;
wire [31:0] debug_wb_rf_wdata_1;
wire [31:0] debug_wb_pc_2;
wire debug_wb_rf_wen_2;
wire [4 :0] debug_wb_rf_wnum_2;
wire [31:0] debug_wb_rf_wdata_2;
assign cpu_clk             = soc_lite.cpu_clk;
assign sys_clk             = soc_lite.sys_clk;
assign debug_wb_pc_0       = soc_lite.debug_wb_pc_0;
assign debug_wb_rf_wen_0   = soc_lite.debug_wb_rf_wen_0;
assign debug_wb_rf_wnum_0  = soc_lite.debug_wb_rf_wnum_0;
assign debug_wb_rf_wdata_0 = soc_lite.debug_wb_rf_wdata_0;
assign debug_wb_pc_1       = soc_lite.debug_wb_pc_1;
assign debug_wb_rf_wen_1   = soc_lite.debug_wb_rf_wen_1;
assign debug_wb_rf_wnum_1  = soc_lite.debug_wb_rf_wnum_1;
assign debug_wb_rf_wdata_1 = soc_lite.debug_wb_rf_wdata_1;
assign debug_wb_pc_2       = soc_lite.debug_wb_pc_2;
assign debug_wb_rf_wen_2   = soc_lite.debug_wb_rf_wen_2;
assign debug_wb_rf_wnum_2  = soc_lite.debug_wb_rf_wnum_2;
assign debug_wb_rf_wdata_2 = soc_lite.debug_wb_rf_wdata_2;

// open the trace file;
integer trace_ref;
initial begin
    trace_ref = $fopen(`TRACE_REF_FILE, "r");
end

//get reference result in falling edge
reg        trace_cmp_flag_0;
reg        trace_cmp_flag_1;
reg        trace_cmp_flag_2;
reg        debug_end;

reg [31:0] ref_wb_pc_0;
reg [4 :0] ref_wb_rf_wnum_0;
reg [31:0] ref_wb_rf_wdata_0;
reg [31:0] ref_wb_pc_1;
reg [4 :0] ref_wb_rf_wnum_1;
reg [31:0] ref_wb_rf_wdata_1;
reg [31:0] ref_wb_pc_2;
reg [4 :0] ref_wb_rf_wnum_2;
reg [31:0] ref_wb_rf_wdata_2;

wire go;
assign go = (debug_wb_rf_wen_0 && debug_wb_rf_wnum_0!=5'd0 || 
debug_wb_rf_wen_1 && debug_wb_rf_wnum_1!=5'd0 || 
debug_wb_rf_wen_2 && debug_wb_rf_wnum_2!=5'd0
);
wire three;
assign three = debug_wb_rf_wen_0 && debug_wb_rf_wen_1 && debug_wb_rf_wen_2;
wire two;
assign two = ((debug_wb_rf_wen_0 && debug_wb_rf_wen_1)||(debug_wb_rf_wen_0 && debug_wb_rf_wen_2)||(debug_wb_rf_wen_1 && debug_wb_rf_wen_2));
wire one;
assign one = debug_wb_rf_wen_0 || debug_wb_rf_wen_1 || debug_wb_rf_wen_2;
wire first, sec, third;
assign first = debug_wb_rf_wen_0;
assign sec = debug_wb_rf_wen_1;
assign third  = debug_wb_rf_wen_2;
wire three_bad;
assign three_bad = (
                three && 
                (
                    ((debug_wb_pc_0!==ref_wb_pc_0) || (debug_wb_rf_wnum_0!==ref_wb_rf_wnum_0)
                     ||(debug_wb_rf_wdata_0!==ref_wb_rf_wdata_0))
                     ||
                     ((debug_wb_pc_1!==ref_wb_pc_1) || (debug_wb_rf_wnum_1!==ref_wb_rf_wnum_1)
                     ||(debug_wb_rf_wdata_1!==ref_wb_rf_wdata_1))
                     ||
                     ((debug_wb_pc_2!==ref_wb_pc_2) || (debug_wb_rf_wnum_2!==ref_wb_rf_wnum_2)
                     ||(debug_wb_rf_wdata_2!==ref_wb_rf_wdata_2))
                )
            );
wire two_bad;
assign two_bad = (
                !three && two &&
                (
                    (first && sec &&
                        (
                            ((debug_wb_pc_0!==ref_wb_pc_0) || (debug_wb_rf_wnum_0!==ref_wb_rf_wnum_0)
                             ||(debug_wb_rf_wdata_0!==ref_wb_rf_wdata_0))
                             ||
                             ((debug_wb_pc_1!==ref_wb_pc_1) || (debug_wb_rf_wnum_1!==ref_wb_rf_wnum_1)
                             ||(debug_wb_rf_wdata_1!==ref_wb_rf_wdata_1))
                        )
                    ) ||
                    (first && third &&
                        (
                             ((debug_wb_pc_0!==ref_wb_pc_0) || (debug_wb_rf_wnum_0!==ref_wb_rf_wnum_0)
                             ||(debug_wb_rf_wdata_0!==ref_wb_rf_wdata_0))
                             ||
                             ((debug_wb_pc_2!==ref_wb_pc_1) || (debug_wb_rf_wnum_2!==ref_wb_rf_wnum_1)
                             ||(debug_wb_rf_wdata_2!==ref_wb_rf_wdata_1))
                        )
                    ) ||
                    (sec && third &&
                        (
                            ((debug_wb_pc_1!==ref_wb_pc_0) || (debug_wb_rf_wnum_1!==ref_wb_rf_wnum_0)
                             ||(debug_wb_rf_wdata_v_1!==ref_wb_rf_wdata_v_0))
                             ||
                             ((debug_wb_pc_2!==ref_wb_pc_1) || (debug_wb_rf_wnum_2!==ref_wb_rf_wnum_1)
                             ||(debug_wb_rf_wdata_2!==ref_wb_rf_wdata_1))
                         )
                    )
                )
            );
wire one_bad;
wire a, b, c;
assign a = (debug_wb_pc_1!==ref_wb_pc_0);
assign b = (debug_wb_rf_wnum_1!==ref_wb_rf_wnum_0);
assign c = (debug_wb_rf_wdata_1!==ref_wb_rf_wdata_0);
assign one_bad = (
                !three && !two && one && 
                (
                    (first && (
                    ((debug_wb_pc_0!==ref_wb_pc_0) || (debug_wb_rf_wnum_0!==ref_wb_rf_wnum_0)
                             ||(debug_wb_rf_wdata_0!==ref_wb_rf_wdata_0))
                    )) ||
                    (sec && (
                    ((debug_wb_pc_1!==ref_wb_pc_0) || (debug_wb_rf_wnum_1!==ref_wb_rf_wnum_0)
                             ||(debug_wb_rf_wdata_1!==ref_wb_rf_wdata_0))
                    )) ||
                    (third && (
                    ((debug_wb_pc_2!==ref_wb_pc_0) || (debug_wb_rf_wnum_2!==ref_wb_rf_wnum_0)
                             ||(debug_wb_rf_wdata_2!==ref_wb_rf_wdata_0))
                    ))
                )
            );

always @(posedge cpu_clk)
begin 
    #1;
    if(|go && !debug_end && `CONFREG_OPEN_TRACE)
    begin
        trace_cmp_flag_0=1'b0;
        trace_cmp_flag_1=1'b0;
        trace_cmp_flag_2=1'b0;
        if (three) begin      // 3 valid
            while (!trace_cmp_flag_0 && !($feof(trace_ref)))
            begin
                $fscanf(trace_ref, "%h %h %h %h", trace_cmp_flag_0,
                        ref_wb_pc_0, ref_wb_rf_wnum_0, ref_wb_rf_wdata_0);
            end
            while (!trace_cmp_flag_1 && !($feof(trace_ref)))
            begin
                $fscanf(trace_ref, "%h %h %h %h", trace_cmp_flag_1,
                        ref_wb_pc_1, ref_wb_rf_wnum_1, ref_wb_rf_wdata_1);
            end
            while (!trace_cmp_flag_2 && !($feof(trace_ref)))
            begin
                $fscanf(trace_ref, "%h %h %h %h", trace_cmp_flag_2,
                        ref_wb_pc_2, ref_wb_rf_wnum_2, ref_wb_rf_wdata_2);
            end
        end
        else if (two)begin      // 2 valid
        while (!trace_cmp_flag_0 && !($feof(trace_ref)))
            begin
                $fscanf(trace_ref, "%h %h %h %h", trace_cmp_flag_0,
                        ref_wb_pc_0, ref_wb_rf_wnum_0, ref_wb_rf_wdata_0);
            end
            while (!trace_cmp_flag_1 && !($feof(trace_ref)))
            begin
                $fscanf(trace_ref, "%h %h %h %h", trace_cmp_flag_1,
                        ref_wb_pc_1, ref_wb_rf_wnum_1, ref_wb_rf_wdata_1);
            end
        end
        else begin
            while (!trace_cmp_flag_0 && !($feof(trace_ref)))
            begin
                $fscanf(trace_ref, "%h %h %h %h", trace_cmp_flag_0,
                        ref_wb_pc_0, ref_wb_rf_wnum_0, ref_wb_rf_wdata_0);
            end
        end
    end
end

//wdata[i*8+7 : i*8] is valid, only wehile wen[i] is valid
wire [31:0] debug_wb_rf_wdata_v_0;
wire [31:0] debug_wb_rf_wdata_v_1;
wire [31:0] debug_wb_rf_wdata_v_2;
wire [31:0] ref_wb_rf_wdata_v_0;
wire [31:0] ref_wb_rf_wdata_v_1;
wire [31:0] ref_wb_rf_wdata_v_2;
assign debug_wb_rf_wdata_v_0 = debug_wb_rf_wdata_0 & {32{debug_wb_rf_wen_0}};
assign debug_wb_rf_wdata_v_1 = debug_wb_rf_wdata_1 & {32{debug_wb_rf_wen_1}};
assign debug_wb_rf_wdata_v_2 = debug_wb_rf_wdata_2 & {32{debug_wb_rf_wen_2}};
assign   ref_wb_rf_wdata_v_0 =   ref_wb_rf_wdata_0 & {32{debug_wb_rf_wen_0}};
assign   ref_wb_rf_wdata_v_1 =   ref_wb_rf_wdata_1 & (first ? {32{debug_wb_rf_wen_1}} : {32{debug_wb_rf_wen_0}});
assign   ref_wb_rf_wdata_v_2 =   ref_wb_rf_wdata_2 & (first ? 
(sec ?
{32{debug_wb_rf_wen_2}} : {32{debug_wb_rf_wen_1}}) :
(
sec ? 
{32{debug_wb_rf_wen_1}} : {32{debug_wb_rf_wen_0}}
)
);


//compare result in rsing edge 
reg debug_wb_err;
always @(posedge cpu_clk)
begin
    #2;
    if(!resetn)
    begin
        debug_wb_err <= 1'b0;
    end
    else if(|go && !debug_end && `CONFREG_OPEN_TRACE)
    begin
        if (debug_wb_rf_wen_0) begin
            $display("    amipsel: PC = 0x%8h, wb_rf_wnum = 0x%2h, wb_rf_wdata = 0x%8h",
                      debug_wb_pc_0, debug_wb_rf_wnum_0, debug_wb_rf_wdata_v_0);
        end
        if (debug_wb_rf_wen_1) begin
            $display("    amipsel: PC = 0x%8h, wb_rf_wnum = 0x%2h, wb_rf_wdata = 0x%8h",
                      debug_wb_pc_1, debug_wb_rf_wnum_1, debug_wb_rf_wdata_v_1);
        end
        if (debug_wb_rf_wen_2) begin
            $display("    amipsel: PC = 0x%8h, wb_rf_wnum = 0x%2h, wb_rf_wdata = 0x%8h",
                      debug_wb_pc_2, debug_wb_rf_wnum_2, debug_wb_rf_wdata_v_2);
        end
        if (
            three_bad
            ||
            two_bad
            ||
            one_bad
        )
        begin
            $display("--------------------------------------------------------------");
            $display("[%t] Error!!!",$time);
            if (first) begin
            $display("    reference: PC = 0x%8h, wb_rf_wnum = 0x%2h, wb_rf_wdata = 0x%8h",
                      ref_wb_pc_0, ref_wb_rf_wnum_0, ref_wb_rf_wdata_v_0);
            $display("    mycpu    : PC = 0x%8h, wb_rf_wnum = 0x%2h, wb_rf_wdata = 0x%8h",
                      debug_wb_pc_0, debug_wb_rf_wnum_0, debug_wb_rf_wdata_0);
            end
            if (sec) begin
                if (first) begin
                    $display("    reference: PC = 0x%8h, wb_rf_wnum = 0x%2h, wb_rf_wdata = 0x%8h",
                          ref_wb_pc_1, ref_wb_rf_wnum_1, ref_wb_rf_wdata_1);
                end
                else begin
                    $display("    reference: PC = 0x%8h, wb_rf_wnum = 0x%2h, wb_rf_wdata = 0x%8h",
                          ref_wb_pc_0, ref_wb_rf_wnum_0, ref_wb_rf_wdata_0);
                end
                $display("    mycpu    : PC = 0x%8h, wb_rf_wnum = 0x%2h, wb_rf_wdata = 0x%8h",
                            debug_wb_pc_1, debug_wb_rf_wnum_1, debug_wb_rf_wdata_1);
            end
            if (third) begin
            if (first && sec) begin
            $display("    reference: PC = 0x%8h, wb_rf_wnum = 0x%2h, wb_rf_wdata = 0x%8h",
                      ref_wb_pc_2, ref_wb_rf_wnum_2, ref_wb_rf_wdata_2);
            end
            if (first || sec) begin
            $display("    reference: PC = 0x%8h, wb_rf_wnum = 0x%2h, wb_rf_wdata = 0x%8h",
                      ref_wb_pc_1, ref_wb_rf_wnum_1, ref_wb_rf_wdata_1);
            end
            else begin
            $display("    reference: PC = 0x%8h, wb_rf_wnum = 0x%2h, wb_rf_wdata = 0x%8h",
                      ref_wb_pc_0, ref_wb_rf_wnum_0, ref_wb_rf_wdata_0);
            end
            $display("    mycpu    : PC = 0x%8h, wb_rf_wnum = 0x%2h, wb_rf_wdata = 0x%8h",
                      debug_wb_pc_2, debug_wb_rf_wnum_2, debug_wb_rf_wdata_2);
            end
            $display("--------------------------------------------------------------");
            debug_wb_err <= 1'b1;
            #40;
            $finish;
        end
    end
end

//monitor numeric display
reg [7:0] err_count;
wire [31:0] confreg_num_reg = `CONFREG_NUM_REG;
reg  [31:0] confreg_num_reg_r;
always @(posedge sys_clk)
begin
    confreg_num_reg_r <= confreg_num_reg;
    if (!resetn)
    begin
        err_count <= 8'd0;
    end
    else if (confreg_num_reg_r != confreg_num_reg && `CONFREG_NUM_MONITOR)
    begin
        if(confreg_num_reg[7:0]!=confreg_num_reg_r[7:0]+1'b1)
        begin
            $display("--------------------------------------------------------------");
            $display("[%t] Error(%d)!!! Occurred in number 8'd%02d Functional Test Point!",$time, err_count, confreg_num_reg[31:24]);
            $display("--------------------------------------------------------------");
            err_count <= err_count + 1'b1;
        end
        else if(confreg_num_reg[31:24]!=confreg_num_reg_r[31:24]+1'b1)
        begin
            $display("--------------------------------------------------------------");
            $display("[%t] Error(%d)!!! Unknown, Functional Test Point numbers are unequal!",$time,err_count);
            $display("--------------------------------------------------------------");
            $display("==============================================================");
            err_count <= err_count + 1'b1;
        end
        else
        begin
            $display("----[%t] Number 8'd%02d Functional Test Point PASS!!!", $time, confreg_num_reg[31:24]);
        end
    end
end

//monitor test
initial
begin
    $timeformat(-9,0," ns",10);
    while(!resetn) #5;
    $display("==============================================================");
    $display("Test begin!");

    #10000;
    while(`CONFREG_NUM_MONITOR)
    begin
        #10000;
        $display ("        [%t] Test is running, debug_wb_pc = 0x%8h",$time, debug_wb_pc_0);
    end
end

//妯℃嫙涓插彛鎵撳嵃
wire uart_display;
wire [7:0] uart_data;
assign uart_display = `CONFREG_UART_DISPLAY;
assign uart_data    = `CONFREG_UART_DATA;

always @(posedge sys_clk)
begin
    if(uart_display)
    begin
        if(uart_data==8'hff)
        begin
            ;//$finish;
        end
        else
        begin
            $write("%c",uart_data);
        end
    end
end

//test end
wire global_err = debug_wb_err || (err_count!=8'd0);
wire test_end = (debug_wb_pc_0==`END_PC || debug_wb_pc_1==`END_PC || debug_wb_pc_2==`END_PC) || (uart_display && uart_data==8'hff);
always @(posedge cpu_clk)
begin
    if (!resetn)
    begin
        debug_end <= 1'b0;
    end
    else if(test_end && !debug_end)
    begin
        debug_end <= 1'b1;
        $display("==============================================================");
        $display("Test end!");
        #40;
        $fclose(trace_ref);
        if (global_err)
        begin
            $display("Fail!!!Total %d errors!",err_count);
        end
        else
        begin
            $display("----PASS!!!");
        end
	    $finish;
	end
end
endmodule

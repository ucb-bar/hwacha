`include "macros.vh"
`include "riscvConst.vh"
`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVMU_Ctrl_ut
(
  input clk,
  input reset,

  input  [`UTMCMD_SZ-1:0] utmcmdq_bits,
  input                   utmcmdq_val,
  output                  utmcmdq_rdy,

  input  [`UTMIMM_SZ-1:0] utmimmq_bits,
  input                   utmimmq_val,
  output                  utmimmq_rdy,

  output [`UTMRESP_SZ-1:0] utmrespq_bits,
  output                 utmrespq_val,
  input                  utmrespq_rdy,

  // vmr queue interface
  input                  utaq_deq_val,
  output                 utaq_deq_rdy,
  input [31:0]           utaq_deq_bits,

  // load data queue interface
  output                 utldq_enq_val,
  input                  utldq_enq_rdy,
  output [64:0]          utldq_enq_bits,
  
  input                  utldq_deq_rdy,
  output                 utldq_wb_done,

  // store data queue interface
  input                  utsdq_deq_val,
  output                 utsdq_deq_rdy,
  input [64:0]           utsdq_deq_bits,

  // D$ interface
  // request
  output [29:0]         dcachereq_addr,
  output [11:0]         dcachereq_tag,
  output [63:0]         dcachereq_data,
  output [7:0]          dcachereq_wmask,
  output [3:0]          dcachereq_op,
  output                dcachereq_val,
  input                 dcachereq_rdy,

  // response
  input [63:0]          dcacheresp_data,
  input [11:0]          dcacheresp_tag,
  input                 dcacheresp_val
);


  wire [`UT_ISCMD_SZ-1:0]  iscmdq_enq_bits, iscmdq_deq_bits;
  wire                        iscmdq_enq_val, iscmdq_enq_rdy, iscmdq_deq_val, iscmdq_deq_rdy;

  wire [`UT_WBCMD_SZ-1:0]  wbcmdq_enq_bits, wbcmdq_deq_bits;
  wire                        wbcmdq_enq_val, wbcmdq_enq_rdy, wbcmdq_deq_val, wbcmdq_deq_rdy;

  wire [`UT_STCMD_SZ-1:0]    stcmdq_enq_bits, stcmdq_deq_bits;
  wire                    stcmdq_enq_val, stcmdq_enq_rdy, stcmdq_deq_val, stcmdq_deq_rdy;

  wire [29:0] lrq_enq_addr_bits, lrq_deq_addr_bits;
  wire [11:0] lrq_enq_tag_bits, lrq_deq_tag_bits;
  wire        lrq_enq_val, lrq_deq_val, lrq_enq_rdy, lrq_deq_rdy;

  wire [7:0]  roq_deq_tag_bits, roq_enq_tag_bits;
  wire        roq_deq_tag_val, roq_deq_tag_rdy;

  wire [63:0] roq_deq_data_bits, roq_enq_data_bits;
  wire        roq_deq_data_val, roq_deq_data_rdy;
  wire        roq_enq_val;

  wire [29:0] srq_enq_addr_bits, srq_deq_addr_bits;
  wire [11:0]  srq_enq_tag_bits,  srq_deq_tag_bits;
  wire [3:0]  srq_enq_op_bits,  srq_deq_op_bits;
  wire [63:0] srq_enq_data_bits, srq_deq_data_bits;
  wire [7:0]  srq_enq_wmask_bits, srq_deq_wmask_bits;
  wire srq_enq_rdy, srq_enq_val, srq_deq_val, srq_deq_rdy;
  wire [2:0] dcacheresp_addr_lsb;

  wire utaq_deq_rdy_is, utaq_deq_rdy_st;
  wire roq_deq_tag_rdy_is, roq_deq_tag_rdy_st;

  wire store_busy, issue_busy;

  assign roq_enq_val         = dcacheresp_val;
  assign roq_enq_tag_bits    = dcacheresp_tag[7:0];
  assign dcacheresp_addr_lsb = dcacheresp_tag[10:8];
  
  assign roq_enq_data_bits = (dcacheresp_addr_lsb == 3'd0) ? dcacheresp_data :
                             (dcacheresp_addr_lsb == 3'd1) ? {8'd0, dcacheresp_data[63:8]} :
                             (dcacheresp_addr_lsb == 3'd2) ? {16'd0, dcacheresp_data[63:16]} :
                             (dcacheresp_addr_lsb == 3'd3) ? {24'd0, dcacheresp_data[63:24]} :
                             (dcacheresp_addr_lsb == 3'd4) ? {32'd0, dcacheresp_data[63:32]} :
                             (dcacheresp_addr_lsb == 3'd5) ? {40'd0, dcacheresp_data[63:40]} :
                             (dcacheresp_addr_lsb == 3'd6) ? {48'd0, dcacheresp_data[63:48]} :
                             (dcacheresp_addr_lsb == 3'd7) ? {56'd0, dcacheresp_data[63:56]} : 64'd0;
                             
  // load issue and store (for AMOs) both access ROQ and UTAQ but not at the same time (I hope!!)
  assign roq_deq_tag_rdy = roq_deq_tag_rdy_is | roq_deq_tag_rdy_st;
  assign utaq_deq_rdy = utaq_deq_rdy_is | utaq_deq_rdy_st;

`ifndef SYNTHESIS
  always @(posedge clk)
  begin
    if (roq_deq_tag_rdy_is & roq_deq_tag_rdy_st)
      $display("ERROR: roq_deq_tag_rdy_is & roq_deq_tag_rdy_st both high!");
    if (utaq_deq_rdy_is & utaq_deq_rdy_st)
      $display("ERROR: utaq_deq_rdy_is & utaq_deq_rdy_st both high!");
  end
`endif

  vuVMU_Ctrl_ut_top ctrl_ut_top
  (
   .clk                   (clk),
   .reset                 (reset),

   .utmcmdq_bits           (utmcmdq_bits),
   .utmcmdq_val            (utmcmdq_val),
   .utmcmdq_rdy            (utmcmdq_rdy),

   .utmimmq_bits           (utmimmq_bits),
   .utmimmq_val            (utmimmq_val),
   .utmimmq_rdy            (utmimmq_rdy),

   .utmrespq_bits          (utmrespq_bits),
   .utmrespq_val           (utmrespq_val),
   .utmrespq_rdy           (utmrespq_rdy),

   .iscmdq_enq_bits           (iscmdq_enq_bits),
   .iscmdq_enq_val            (iscmdq_enq_val),
   .iscmdq_enq_rdy            (iscmdq_enq_rdy),

   .wbcmdq_enq_bits           (wbcmdq_enq_bits),
   .wbcmdq_enq_val            (wbcmdq_enq_val),
   .wbcmdq_enq_rdy            (wbcmdq_enq_rdy),

   .stcmdq_enq_bits           (stcmdq_enq_bits),
   .stcmdq_enq_val            (stcmdq_enq_val),
   .stcmdq_enq_rdy            (stcmdq_enq_rdy),

   .issue_busy            (issue_busy),
   .store_busy            (store_busy)
  );

  `VC_PIPE_QUEUE(`UT_ISCMD_SZ, 4) iscmdq
  (
    .clk                  (clk),
    .reset                (reset),
    .enq_bits             (iscmdq_enq_bits),
    .enq_val              (iscmdq_enq_val),
    .enq_rdy              (iscmdq_enq_rdy),
    .deq_bits             (iscmdq_deq_bits),
    .deq_val              (iscmdq_deq_val),
    .deq_rdy              (iscmdq_deq_rdy)
  );

  vuVMU_Ctrl_ut_issue ctrl_ut_issue
  (
    .clk                  (clk),
    .reset                (reset),

    .iscmdq_deq_bits      (iscmdq_deq_bits),
    .iscmdq_deq_val       (iscmdq_deq_val),
    .iscmdq_deq_rdy       (iscmdq_deq_rdy),

    .utaq_deq_bits        (utaq_deq_bits),
    .utaq_deq_val         (utaq_deq_val),
    .utaq_deq_rdy         (utaq_deq_rdy_is),

    .lrq_enq_addr_bits    (lrq_enq_addr_bits),
    .lrq_enq_tag_bits     (lrq_enq_tag_bits),
    .lrq_enq_val          (lrq_enq_val),
    .lrq_enq_rdy          (lrq_enq_rdy),

    .roq_deq_tag_bits     (roq_deq_tag_bits),
    .roq_deq_tag_val      (roq_deq_tag_val),
    .roq_deq_tag_rdy      (roq_deq_tag_rdy_is),

    .issue_busy           (issue_busy)
  );

  `VC_PIPE_QUEUE(42,4) lrq
  (
    .clk                  (clk),
    .reset                (reset),
    .enq_bits             ({lrq_enq_addr_bits, lrq_enq_tag_bits}),
    .enq_val              (lrq_enq_val),
    .enq_rdy              (lrq_enq_rdy),
    .deq_bits             ({lrq_deq_addr_bits, lrq_deq_tag_bits}),
    .deq_val              (lrq_deq_val),
    .deq_rdy              (lrq_deq_rdy)
  );

  wire [2:0] deq_tag_bits;
`ifdef ASIC_SMALL
  assign roq_deq_tag_bits = {5'd0, deq_tag_bits};
  wire dummy;
  vuVMU_ROQ #(65,8,3) roq
  (
   .clk                   (clk),
   .reset                 (reset),

   .roq_deq_tag_bits      (deq_tag_bits),
   .roq_deq_tag_val       (roq_deq_tag_val),
   .roq_deq_tag_rdy       (roq_deq_tag_rdy),

   .roq_enq_data_bits     ({1'b0, roq_enq_data_bits}),
   .roq_enq_tag_bits      (roq_enq_tag_bits[2:0]),
   .roq_enq_val           (roq_enq_val),

   .roq_deq_data_bits     ({dummy, roq_deq_data_bits}),
   .roq_deq_data_val      (roq_deq_data_val),
   .roq_deq_data_rdy      (roq_deq_data_rdy)
  );
`else
  wire dummy;
  vuVMU_ROQ #(65,256,8) roq
  (
   .clk                   (clk),
   .reset                 (reset),

   .roq_deq_tag_bits      (roq_deq_tag_bits),
   .roq_deq_tag_val       (roq_deq_tag_val),
   .roq_deq_tag_rdy       (roq_deq_tag_rdy),

   .roq_enq_data_bits     ({1'b0, roq_enq_data_bits}),
   .roq_enq_tag_bits      (roq_enq_tag_bits),
   .roq_enq_val           (roq_enq_val),

   .roq_deq_data_bits     ({dummy, roq_deq_data_bits}),
   .roq_deq_data_val      (roq_deq_data_val),
   .roq_deq_data_rdy      (roq_deq_data_rdy)
  );
`endif

  `VC_PIPE_QUEUE(`UT_WBCMD_SZ, 4) wbcmdq
  (
    .clk                  (clk),
    .reset                (reset),
    .enq_bits             (wbcmdq_enq_bits),
    .enq_val              (wbcmdq_enq_val),
    .enq_rdy              (wbcmdq_enq_rdy),
    .deq_bits             (wbcmdq_deq_bits),
    .deq_val              (wbcmdq_deq_val),
    .deq_rdy              (wbcmdq_deq_rdy)
  );

  vuVMU_Ctrl_ut_wb ctrl_ut_wb
  (
    .clk                  (clk),
    .reset                (reset),
    
    .ldq_wb_done          (utldq_wb_done),
    .ldq_deq_rdy          (utldq_deq_rdy),

    .wbcmdq_deq_bits      (wbcmdq_deq_bits),
    .wbcmdq_deq_val       (wbcmdq_deq_val),
    .wbcmdq_deq_rdy       (wbcmdq_deq_rdy),

    .roq_deq_bits         (roq_deq_data_bits),
    .roq_deq_val          (roq_deq_data_val),
    .roq_deq_rdy          (roq_deq_data_rdy),

    .ldq_enq_bits         (utldq_enq_bits),
    .ldq_enq_val          (utldq_enq_val),
    .ldq_enq_rdy          (utldq_enq_rdy)
  );

  `VC_PIPE_QUEUE(`UT_STCMD_SZ, 4) stcmdq
  (
    .clk                  (clk),
    .reset                (reset),
    .enq_bits             (stcmdq_enq_bits),
    .enq_val              (stcmdq_enq_val),
    .enq_rdy              (stcmdq_enq_rdy),
    .deq_bits             (stcmdq_deq_bits),
    .deq_val              (stcmdq_deq_val),
    .deq_rdy              (stcmdq_deq_rdy)
  );

  vuVMU_Ctrl_ut_store ctrl_ut_store
  (
    .clk                  (clk),
    .reset                (reset),

    .stcmdq_deq_bits      (stcmdq_deq_bits),
    .stcmdq_deq_val       (stcmdq_deq_val),
    .stcmdq_deq_rdy       (stcmdq_deq_rdy),

    .sdq_deq_bits         (utsdq_deq_bits),
    .sdq_deq_val          (utsdq_deq_val),
    .sdq_deq_rdy          (utsdq_deq_rdy),

    .utaq_deq_bits        (utaq_deq_bits),
    .utaq_deq_val         (utaq_deq_val),
    .utaq_deq_rdy         (utaq_deq_rdy_st),

    .roq_deq_tag_bits     (roq_deq_tag_bits),
    .roq_deq_tag_val      (roq_deq_tag_val),
    .roq_deq_tag_rdy      (roq_deq_tag_rdy_st),

    .srq_enq_addr_bits    (srq_enq_addr_bits),
    .srq_enq_tag_bits     (srq_enq_tag_bits),
    .srq_enq_op_bits      (srq_enq_op_bits),
    .srq_enq_data_bits    (srq_enq_data_bits),
    .srq_enq_wmask_bits   (srq_enq_wmask_bits),
    .srq_enq_val          (srq_enq_val),
    .srq_enq_rdy          (srq_enq_rdy),

    .store_busy           (store_busy)
  );

  // 4 bits for opcode, 12 bits for tag, 30 bits for address, 8 bits for write mask, 64 bits of data
  `VC_PIPE_QUEUE(4+12+30+8+64,2) srq
  (
    .clk                  (clk),
    .reset                (reset),
    .enq_bits             ({srq_enq_op_bits, srq_enq_tag_bits, srq_enq_addr_bits, srq_enq_wmask_bits, srq_enq_data_bits}),
    .enq_val              (srq_enq_val),
    .enq_rdy              (srq_enq_rdy),
    .deq_bits             ({srq_deq_op_bits, srq_deq_tag_bits, srq_deq_addr_bits, srq_deq_wmask_bits, srq_deq_data_bits}),
    .deq_val              (srq_deq_val),
    .deq_rdy              (srq_deq_rdy)
  );

  // stores are given priority over loads
  assign dcachereq_val   = lrq_deq_val | srq_deq_val;
  assign lrq_deq_rdy     = srq_deq_val ? 1'b0 : dcachereq_rdy;
  assign srq_deq_rdy     = dcachereq_rdy;
  assign dcachereq_data  = srq_deq_data_bits;
  assign dcachereq_wmask = srq_deq_wmask_bits;
  assign dcachereq_addr  = srq_deq_val ? srq_deq_addr_bits : lrq_deq_addr_bits;
  assign dcachereq_tag   = srq_deq_val ? srq_deq_tag_bits : lrq_deq_tag_bits;
  assign dcachereq_op    = srq_deq_val ? srq_deq_op_bits : 4'b0000;

// `ifndef SYNTHESIS
//   disasm_cmd utmcmd(utmcmdq_cmd);
// `endif

endmodule

`include "macros.vh"
`include "riscvConst.vh"
`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVMU_Ctrl_vec
(
  input clk,
  input reset,

  input  [`VMCMD_SZ-1:0] vmcmdq_bits,
  input                  vmcmdq_val,
  output                 vmcmdq_rdy,

  input  [`VMIMM_SZ-1:0] vmimmq_bits,
  input                  vmimmq_val,
  output                 vmimmq_rdy,

  input  [`VMSTRIDE_SZ-1:0] vmstrideq_bits,
  input                  vmstrideq_val,
  output                 vmstrideq_rdy,

  output     [`VMRESP_SZ-1:0] vmrespq_bits,
  output                 vmrespq_val,
  input                  vmrespq_rdy,

  // load data queue interface
  output                 vldq_enq_val,
  input                  vldq_enq_rdy,
  output [64:0]          vldq_enq_bits,
  
  input                  vldq_deq_rdy,
  output                 vldq_wb_done,

  // store data queue interface
  input                  vsdq_deq_val,
  output                 vsdq_deq_rdy,
  input [64:0]           vsdq_deq_bits,

  // D$ interface
  // request
  output [27:0]         dcachereq_addr,
  output [11:0]         dcachereq_tag,
  output [127:0]        dcachereq_data,
  output [15:0]         dcachereq_wmask,
  output [3:0]          dcachereq_op,
  output                dcachereq_val,
  input                 dcachereq_rdy,

  // response
  input [127:0]         dcacheresp_data,
  input [11:0]          dcacheresp_tag,
  input                 dcacheresp_val

);

  wire [`VM_ISCMD_SZ-1:0]  iscmdq_enq_bits, iscmdq_deq_bits;
  wire                        iscmdq_enq_val, iscmdq_enq_rdy, iscmdq_deq_val, iscmdq_deq_rdy;

  wire [`VM_WBCMD_SZ-1:0]  wbcmdq_enq_bits, wbcmdq_deq_bits;
  wire                        wbcmdq_enq_val, wbcmdq_enq_rdy, wbcmdq_deq_val, wbcmdq_deq_rdy;

  wire [`VM_STCMD_SZ-1:0]    stcmdq_enq_bits,  stcmdq_deq_bits;
  wire                    stcmdq_enq_val, stcmdq_enq_rdy, stcmdq_deq_val, stcmdq_deq_rdy;

  wire [35:0] lrq_enq_bits, lrq_deq_bits;
  wire        lrq_enq_val, lrq_deq_val, lrq_enq_rdy, lrq_deq_rdy;

  wire [7:0]  roq_deq_tag_bits, roq_enq_tag_bits;
  wire        roq_deq_tag_val, roq_deq_tag_rdy;

  wire [127:0] roq_deq_data_bits, roq_enq_data_bits;
  wire         roq_deq_data_val, roq_deq_data_rdy;
  wire         roq_enq_val;

  wire [27:0] srq_enq_addr_bits;
  wire [127:0] srq_enq_data_bits;
  wire [15:0] srq_enq_wmask_bits;
  wire srq_enq_rdy, srq_enq_val, srq_deq_val, srq_deq_rdy;
  wire [171:0] srq_deq_bits;

  wire store_busy;

  assign roq_enq_val       = dcacheresp_val & ~dcacheresp_tag[11];
  assign roq_enq_data_bits = dcacheresp_data;
  assign roq_enq_tag_bits  = dcacheresp_tag[7:0];

  vuVMU_Ctrl_vec_top ctrl_vec_top
  (
   .clk                   (clk),
   .reset                 (reset),

   .vmcmdq_bits           (vmcmdq_bits),
   .vmcmdq_val            (vmcmdq_val),
   .vmcmdq_rdy            (vmcmdq_rdy),

   .vmimmq_bits           (vmimmq_bits),
   .vmimmq_val            (vmimmq_val),
   .vmimmq_rdy            (vmimmq_rdy),

   .vmstrideq_bits        (vmstrideq_bits),
   .vmstrideq_val         (vmstrideq_val),
   .vmstrideq_rdy         (vmstrideq_rdy),

   .vmrespq_bits          (vmrespq_bits),
   .vmrespq_val           (vmrespq_val),
   .vmrespq_rdy           (vmrespq_rdy),

   .iscmdq_bits           (iscmdq_enq_bits),
   .iscmdq_val            (iscmdq_enq_val),
   .iscmdq_rdy            (iscmdq_enq_rdy),

   .wbcmdq_bits           (wbcmdq_enq_bits),
   .wbcmdq_val            (wbcmdq_enq_val),
   .wbcmdq_rdy            (wbcmdq_enq_rdy),

   .stcmdq_bits           (stcmdq_enq_bits),
   .stcmdq_val            (stcmdq_enq_val),
   .stcmdq_rdy            (stcmdq_enq_rdy),

   .store_busy            (store_busy)
  );

  `VC_PIPE_QUEUE(`VM_ISCMD_SZ, 4) iscmdq
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

  `VC_PIPE_QUEUE(`VM_WBCMD_SZ, 4) wbcmdq
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

  vuVMU_Ctrl_vec_load_issue ctrl_vec_load_issue
  (
    .clk                  (clk),
    .reset                (reset),

    .iscmdq_deq_bits      (iscmdq_deq_bits),
    .iscmdq_deq_val       (iscmdq_deq_val),
    .iscmdq_deq_rdy       (iscmdq_deq_rdy),

    .lrq_enq_bits         (lrq_enq_bits),
    .lrq_enq_val          (lrq_enq_val),
    .lrq_enq_rdy          (lrq_enq_rdy),

    .roq_deq_tag_bits     (roq_deq_tag_bits),
    .roq_deq_tag_val      (roq_deq_tag_val),
    .roq_deq_tag_rdy      (roq_deq_tag_rdy)
  );

  vuVMU_Ctrl_vec_load_wb ctrl_vec_load_wb
  (
    .clk                  (clk),
    .reset                (reset),
    
    .vec_done             (vldq_wb_done),
    .ldq_deq              (vldq_deq_rdy),

    .wbcmdq_deq_bits      (wbcmdq_deq_bits),
    .wbcmdq_deq_val       (wbcmdq_deq_val),
    .wbcmdq_deq_rdy       (wbcmdq_deq_rdy),

    .roq_deq_bits         (roq_deq_data_bits),
    .roq_deq_val          (roq_deq_data_val),
    .roq_deq_rdy          (roq_deq_data_rdy),

    .ldq_enq_bits         (vldq_enq_bits),
    .ldq_enq_val          (vldq_enq_val),
    .ldq_enq_rdy          (vldq_enq_rdy)
  );

  `VC_PIPE_QUEUE(36,4) lrq
  (
    .clk                  (clk),
    .reset                (reset),
    .enq_bits             (lrq_enq_bits),
    .enq_val              (lrq_enq_val),
    .enq_rdy              (lrq_enq_rdy),
    .deq_bits             (lrq_deq_bits),
    .deq_val              (lrq_deq_val),
    .deq_rdy              (lrq_deq_rdy)
  );

  wire [2:0] deq_tag_bits;
`ifdef ASIC_SMALL
  assign roq_deq_tag_bits = {5'd0, deq_tag_bits};
  wire [1:0] dummy;
  vuVMU_ROQ #(130,8,3) roq
  (
   .clk                   (clk),
   .reset                 (reset),

   .roq_deq_tag_bits      (deq_tag_bits),
   .roq_deq_tag_val       (roq_deq_tag_val),
   .roq_deq_tag_rdy       (roq_deq_tag_rdy),

   .roq_enq_data_bits     ({2'b00, roq_enq_data_bits}),
   .roq_enq_tag_bits      (roq_enq_tag_bits[2:0]),
   .roq_enq_val           (roq_enq_val),

   .roq_deq_data_bits     ({dummy, roq_deq_data_bits}),
   .roq_deq_data_val      (roq_deq_data_val),
   .roq_deq_data_rdy      (roq_deq_data_rdy)
  );
`else
  wire [1:0] dummy;
  vuVMU_ROQ #(130,256,8) roq
  (
   .clk                   (clk),
   .reset                 (reset),

   .roq_deq_tag_bits      (roq_deq_tag_bits),
   .roq_deq_tag_val       (roq_deq_tag_val),
   .roq_deq_tag_rdy       (roq_deq_tag_rdy),

   .roq_enq_data_bits     ({2'b00, roq_enq_data_bits}),
   .roq_enq_tag_bits      (roq_enq_tag_bits),
   .roq_enq_val           (roq_enq_val),

   .roq_deq_data_bits     ({dummy, roq_deq_data_bits}),
   .roq_deq_data_val      (roq_deq_data_val),
   .roq_deq_data_rdy      (roq_deq_data_rdy)
  );
`endif

  // Store control
  `VC_PIPE_QUEUE(`VM_STCMD_SZ, 4) stcmdq
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

  vuVMU_Ctrl_vec_store ctrl_vec_store
  (
    .clk                  (clk),
    .reset                (reset),

    .stcmdq_deq_bits      (stcmdq_deq_bits),
    .stcmdq_deq_val       (stcmdq_deq_val),
    .stcmdq_deq_rdy       (stcmdq_deq_rdy),

    .sdq_deq_bits         (vsdq_deq_bits),
    .sdq_deq_val          (vsdq_deq_val),
    .sdq_deq_rdy          (vsdq_deq_rdy),

    .srq_enq_addr_bits    (srq_enq_addr_bits),
    .srq_enq_data_bits    (srq_enq_data_bits),
    .srq_enq_wmask_bits   (srq_enq_wmask_bits),
    .srq_enq_val          (srq_enq_val),
    .srq_enq_rdy          (srq_enq_rdy),

    .store_busy           (store_busy)
  );

  // 128 bits data + 28 bits address + 16 bits write mask
  `VC_PIPE_QUEUE(128+28+16,2) srq
  (
    .clk                  (clk),
    .reset                (reset),
    .enq_bits             ({srq_enq_addr_bits, srq_enq_wmask_bits, srq_enq_data_bits}),
    .enq_val              (srq_enq_val),
    .enq_rdy              (srq_enq_rdy),
    .deq_bits             (srq_deq_bits),
    .deq_val              (srq_deq_val),
    .deq_rdy              (srq_deq_rdy)
  );

  // stores are given priority over loads
  assign dcachereq_val   = lrq_deq_val | srq_deq_val;
  assign lrq_deq_rdy     = srq_deq_val ? 1'b0 : dcachereq_rdy;
  assign srq_deq_rdy     = dcachereq_rdy;
  assign dcachereq_data  = srq_deq_bits[127:0];
  assign dcachereq_wmask = srq_deq_bits[143:128];
  assign dcachereq_addr  = srq_deq_val ? srq_deq_bits[171:144] : lrq_deq_bits[35:8];
  assign dcachereq_tag   = srq_deq_val ? 12'h800 : {4'd0, lrq_deq_bits[7:0]};
  assign dcachereq_op    = srq_deq_val ? 4'b0001 : 4'b0000;

endmodule

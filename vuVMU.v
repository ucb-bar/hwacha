`include "macros.vh"
`include "riscvConst.vh"
`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVMU
(
  input clk,
  input reset,

  input  [`VMCMD_SZ-1:0] vmu_vcmdq_bits,
  input                  vmu_vcmdq_val,
  output                 vmu_vcmdq_rdy,

  input  [`VMIMM_SZ-1:0] vmu_vbaseq_bits,
  input                  vmu_vbaseq_val,
  output                 vmu_vbaseq_rdy,

  input  [`VMSTRIDE_SZ-1:0] vmu_vstrideq_bits,
  input                     vmu_vstrideq_val,
  output                    vmu_vstrideq_rdy,

  output [`VMRESP_SZ-1:0] vmu_vackq_bits,
  output                  vmu_vackq_val,
  input                   vmu_vackq_rdy,

  input  [`UTMCMD_SZ-1:0] vmu_utcmdq_bits,
  input                   vmu_utcmdq_val,
  output                  vmu_utcmdq_rdy,

  input  [`UTMIMM_SZ-1:0] vmu_utimmq_bits,
  input                   vmu_utimmq_val,
  output                  vmu_utimmq_rdy,

  output [`UTMRESP_SZ-1:0] vmu_utackq_bits,
  output                   vmu_utackq_val,
  input                    vmu_utackq_rdy,

  output [64:0] lane_vldq_deq_bits,
  output        lane_vldq_deq_val,
  input         lane_vldq_deq_rdy,

  input [64:0]  lane_vsdq_enq_bits,
  input         lane_vsdq_enq_val,
  output        lane_vsdq_enq_rdy,

  input [31:0]  lane_utaq_enq_bits,
  input         lane_utaq_enq_val,
  output        lane_utaq_enq_rdy,

  output [64:0] lane_utldq_deq_bits,
  output        lane_utldq_deq_val,
  input         lane_utldq_deq_rdy,

  input [64:0]  lane_utsdq_enq_bits,
  input         lane_utsdq_enq_val,
  output        lane_utsdq_enq_rdy,

  output [29:0]           dmem_req_ut_addr,
  output [3:0]            dmem_req_ut_op,
  output [63:0]           dmem_req_ut_data,
  output [7:0]            dmem_req_ut_wmask,
  output [11:0]           dmem_req_ut_tag,
  output                  dmem_req_ut_val,
  input                   dmem_req_ut_rdy,

  input                   dmem_resp_ut_val,
  input [11:0]            dmem_resp_ut_tag,
  input [63:0]            dmem_resp_ut_data,

  output [27:0]          dmem_req_vec_addr,
  output [3:0]           dmem_req_vec_op,
  output [127:0]         dmem_req_vec_data,
  output [15:0]          dmem_req_vec_wmask,
  output [11:0]          dmem_req_vec_tag,
  output                 dmem_req_vec_val,
  input                  dmem_req_vec_rdy,

  input                  dmem_resp_vec_val,
  input  [11:0]          dmem_resp_vec_tag,
  input  [127:0]         dmem_resp_vec_data
);

`define VMU_QUEUE_ENTRIES   16
`define VMU_QUEUE_LEVEL     8

  wire     vldq_wb_done;
  wire     vldq_count_rdy;
  wire     vldq_enq_val;
  wire     vldq_enq_rdy;
  wire [64:0] vldq_enq_bits;

  wire     vsdq_count_rdy;
  wire     vsdq_deq_val;
  wire     vsdq_deq_rdy;
  wire [64:0] vsdq_deq_bits;

  wire     utaq_count_rdy;
  wire     utaq_deq_val;
  wire     utaq_deq_rdy;
  wire [31:0] utaq_deq_bits;

  wire     utldq_wb_done;
  wire     utldq_count_rdy;
  wire     utldq_enq_val;
  wire     utldq_enq_rdy;
  wire [64:0] utldq_enq_bits;

  wire     utsdq_count_rdy;
  wire     utsdq_deq_val;
  wire     utsdq_deq_rdy;
  wire [64:0] utsdq_deq_bits;

  vuVMU_Ctrl_vec ctrl_vec
  (
    .clk                (clk),
    .reset              (reset),

    .vmcmdq_bits        (vmu_vcmdq_bits),
    .vmcmdq_val         (vmu_vcmdq_val),
    .vmcmdq_rdy         (vmu_vcmdq_rdy),

    .vmimmq_bits        (vmu_vbaseq_bits),
    .vmimmq_val         (vmu_vbaseq_val),
    .vmimmq_rdy         (vmu_vbaseq_rdy),

    .vmstrideq_bits     (vmu_vstrideq_bits),
    .vmstrideq_val      (vmu_vstrideq_val),
    .vmstrideq_rdy      (vmu_vstrideq_rdy),

    .vmrespq_bits       (vmu_vackq_bits),
    .vmrespq_val        (vmu_vackq_val),
    .vmrespq_rdy        (vmu_vackq_rdy),

    .vldq_enq_val       (vldq_enq_val),
    .vldq_enq_rdy       (vldq_enq_rdy),
    .vldq_enq_bits      (vldq_enq_bits),
    
    .vldq_deq_rdy       (lane_vldq_deq_rdy),
    .vldq_wb_done       (vldq_wb_done),

    .vsdq_deq_val       (vsdq_deq_val),
    .vsdq_deq_rdy       (vsdq_deq_rdy),
    .vsdq_deq_bits      (vsdq_deq_bits),

    .dcachereq_val      (dmem_req_vec_val),
    .dcachereq_rdy      (dmem_req_vec_rdy),
    .dcachereq_addr     (dmem_req_vec_addr),
    .dcachereq_op       (dmem_req_vec_op),
    .dcachereq_wmask    (dmem_req_vec_wmask),
    .dcachereq_data     (dmem_req_vec_data),
    .dcachereq_tag      (dmem_req_vec_tag),

    .dcacheresp_val     (dmem_resp_vec_val),
    .dcacheresp_tag     (dmem_resp_vec_tag),
    .dcacheresp_data    (dmem_resp_vec_data)
  );

  vuVMU_Ctrl_ut  ctrl_ut
  (
    .clk                (clk),
    .reset              (reset),

    .utmcmdq_bits       (vmu_utcmdq_bits),
    .utmcmdq_val        (vmu_utcmdq_val),
    .utmcmdq_rdy        (vmu_utcmdq_rdy),

    .utmimmq_bits       (vmu_utimmq_bits),
    .utmimmq_val        (vmu_utimmq_val),
    .utmimmq_rdy        (vmu_utimmq_rdy),

    .utmrespq_bits      (vmu_utackq_bits),
    .utmrespq_val       (vmu_utackq_val),
    .utmrespq_rdy       (vmu_utackq_rdy),

    .utaq_deq_val       (utaq_deq_val),
    .utaq_deq_rdy       (utaq_deq_rdy),
    .utaq_deq_bits      (utaq_deq_bits),

    .utldq_enq_val      (utldq_enq_val),
    .utldq_enq_rdy      (utldq_enq_rdy),
    .utldq_enq_bits     (utldq_enq_bits),
    
    .utldq_deq_rdy      (lane_utldq_deq_rdy),
    .utldq_wb_done      (utldq_wb_done),

    .utsdq_deq_val      (utsdq_deq_val),
    .utsdq_deq_rdy      (utsdq_deq_rdy),
    .utsdq_deq_bits     (utsdq_deq_bits),

    .dcachereq_val      (dmem_req_ut_val),
    .dcachereq_rdy      (dmem_req_ut_rdy),
    .dcachereq_addr     (dmem_req_ut_addr),
    .dcachereq_op       (dmem_req_ut_op),
    .dcachereq_wmask    (dmem_req_ut_wmask),
    .dcachereq_data     (dmem_req_ut_data),
    .dcachereq_tag      (dmem_req_ut_tag),

    .dcacheresp_val     (dmem_resp_ut_val),
    .dcacheresp_tag     (dmem_resp_ut_tag),
    .dcacheresp_data    (dmem_resp_ut_data)
  );

  // queues
  // vldq queue and capacity counter
  vuVMU_QueueCount#(0, `VMU_QUEUE_LEVEL+1, `VMU_QUEUE_ENTRIES) vldq_count
  (
    .clk(clk),
    .reset(reset),
    .enq(vldq_enq_val & vldq_enq_rdy),
    .deq(lane_vldq_deq_rdy),
    .ready(vldq_count_rdy)
  );
  
  assign lane_vldq_deq_val = vldq_count_rdy | vldq_wb_done;
    
  `VC_SIMPLE_QUEUE(65, `VMU_QUEUE_ENTRIES) vldq
  (
    .clk(clk),
    .reset(reset),
    .enq_bits(vldq_enq_bits),
    .enq_val(vldq_enq_val),
    .enq_rdy(vldq_enq_rdy),
    .deq_bits(lane_vldq_deq_bits),
    .deq_val(),
    .deq_rdy(lane_vldq_deq_rdy) // & vldq_deq_val8) 
  );
  
  // vsdq queue and capacity counter
  vuVMU_QueueCount#(`VMU_QUEUE_ENTRIES, `VMU_QUEUE_LEVEL+1, `VMU_QUEUE_ENTRIES) vsdq_count
  (
    .clk(clk),
    .reset(reset),
    .enq(vsdq_deq_val & vsdq_deq_rdy),
    .deq(lane_vsdq_enq_val),
    .ready(vsdq_count_rdy)
  );
  
  assign lane_vsdq_enq_rdy = vsdq_count_rdy;
  
  `VC_SIMPLE_QUEUE(65, `VMU_QUEUE_ENTRIES) vsdq
  (
    .clk(clk),
    .reset(reset),
    .enq_bits(lane_vsdq_enq_bits),
    .enq_val(lane_vsdq_enq_val),
    .enq_rdy(),
    .deq_bits(vsdq_deq_bits),
    .deq_val(vsdq_deq_val),
    .deq_rdy(vsdq_deq_rdy)
  );

  // utaq queue and capacity counter
  
  vuVMU_QueueCount#(`VMU_QUEUE_ENTRIES, `VMU_QUEUE_LEVEL+1, `VMU_QUEUE_ENTRIES) utaq_count
  (
    .clk(clk),
    .reset(reset),
    .enq(utaq_deq_val & utaq_deq_rdy),
    .deq(lane_utaq_enq_val),
    .ready(utaq_count_rdy)
  );
  
   assign lane_utaq_enq_rdy = utaq_count_rdy;
  
  `VC_SIMPLE_QUEUE(32, `VMU_QUEUE_ENTRIES) utaq
  (
    .clk(clk),
    .reset(reset),
    .enq_bits(lane_utaq_enq_bits),
    .enq_val(lane_utaq_enq_val),
    .enq_rdy(),
    .deq_bits(utaq_deq_bits),
    .deq_val(utaq_deq_val),
    .deq_rdy(utaq_deq_rdy)
  );

  // utldq queue and capacity counter
  
  vuVMU_QueueCount#(0, `VMU_QUEUE_LEVEL+1, `VMU_QUEUE_ENTRIES) utldq_count
  (
    .clk(clk),
    .reset(reset),
    .enq(utldq_enq_val & utldq_enq_rdy),
    .deq(lane_utldq_deq_rdy),
    .ready(utldq_count_rdy)
  );
  
  assign lane_utldq_deq_val = utldq_count_rdy | utldq_wb_done;
  
  `VC_SIMPLE_QUEUE(65, `VMU_QUEUE_ENTRIES) utldq
  (
    .clk(clk),
    .reset(reset),
    .enq_bits(utldq_enq_bits),
    .enq_val(utldq_enq_val),
    .enq_rdy(utldq_enq_rdy),
    .deq_bits(lane_utldq_deq_bits),
    .deq_val(),
    .deq_rdy(lane_utldq_deq_rdy)
  );

  // utsdq queue and capacity counter
  
  vuVMU_QueueCount#(`VMU_QUEUE_ENTRIES, `VMU_QUEUE_LEVEL+1, `VMU_QUEUE_ENTRIES) utsdq_count
  (
    .clk(clk),
    .reset(reset),
    .enq(utsdq_deq_val & utsdq_deq_rdy),
    .deq(lane_utsdq_enq_val),
    .ready(utsdq_count_rdy)
  );
    
  assign lane_utsdq_enq_rdy = utsdq_count_rdy;
  
  `VC_SIMPLE_QUEUE(65, `VMU_QUEUE_ENTRIES) utsdq
  (
    .clk(clk),
    .reset(reset),
    .enq_bits(lane_utsdq_enq_bits),
    .enq_val(lane_utsdq_enq_val),
    .enq_rdy(),
    .deq_bits(utsdq_deq_bits),
    .deq_val(utsdq_deq_val),
    .deq_rdy(utsdq_deq_rdy)
  );

endmodule

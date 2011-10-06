`include "macros.vh"
`include "riscvConst.vh"
`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vu
(
  input clk,
  input reset,

  output illegal,

  input  [`VCMD_SZ-1:0]   vec_cmdq_bits,
  input                   vec_cmdq_val,
  output                  vec_cmdq_rdy,

  input  [`VIMM_SZ-1:0]   vec_ximm1q_bits,
  input                   vec_ximm1q_val,
  output                  vec_ximm1q_rdy,

  input  [`VSTRIDE_SZ-1:0] vec_ximm2q_bits,
  input                    vec_ximm2q_val,
  output                   vec_ximm2q_rdy,

  output [`VRESP_SZ-1:0]  vec_ackq_bits,
  output                  vec_ackq_val,
  input                   vec_ackq_rdy,

  input               cp_imul_val,
  output              cp_imul_rdy,
  input  `DEF_VAU0_FN cp_imul_fn,
  input  `DEF_XLEN    cp_imul_in0,
  input  `DEF_XLEN    cp_imul_in1,
  output `DEF_XLEN    cp_imul_out,

  input               cp_fma_val,
  output              cp_fma_rdy,
  input  `DEF_VAU1_FN cp_fma_fn,
  input  `DEF_FLEN    cp_fma_in0,
  input  `DEF_FLEN    cp_fma_in1,
  input  `DEF_FLEN    cp_fma_in2,
  output `DEF_FLEN    cp_fma_out,
  output `DEF_EXC     cp_fma_exc,

  output [31:0]           imem_req_addr,
  output                  imem_req_val,
  input                   imem_req_rdy,
  input  [31:0]           imem_resp_data,
  input                   imem_resp_val,

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

  output [27:0]           dmem_req_vec_addr,
  output [3:0]            dmem_req_vec_op,
  output [127:0]          dmem_req_vec_data,
  output [15:0]           dmem_req_vec_wmask,
  output [11:0]           dmem_req_vec_tag,
  output                  dmem_req_vec_val,
  input                   dmem_req_vec_rdy,

  input                   dmem_resp_vec_val,
  input [11:0]            dmem_resp_vec_tag,
  input [127:0]           dmem_resp_vec_data
);

  wire [`XCMD_SZ-1:0] vxu_cmdq_enq_bits, vxu_cmdq_deq_bits;
  wire                vxu_cmdq_enq_val, vxu_cmdq_deq_val;
  wire                vxu_cmdq_enq_rdy, vxu_cmdq_deq_rdy;

  wire [`XIMM_SZ-1:0] vxu_immq_enq_bits, vxu_immq_deq_bits;
  wire                vxu_immq_enq_val, vxu_immq_deq_val;
  wire                vxu_immq_enq_rdy, vxu_immq_deq_rdy;

  wire [`XRESP_SZ-1:0] vxu_ackq_enq_bits, vxu_ackq_deq_bits;
  wire                 vxu_ackq_enq_val, vxu_ackq_deq_val;
  wire                 vxu_ackq_enq_rdy, vxu_ackq_deq_rdy;

  wire [`VMCMD_SZ-1:0] vmu_vcmdq_enq_bits, vmu_vcmdq_deq_bits;
  wire                 vmu_vcmdq_enq_val, vmu_vcmdq_deq_val;
  wire                 vmu_vcmdq_enq_rdy, vmu_vcmdq_deq_rdy;

  wire [`VMIMM_SZ-1:0] vmu_vbaseq_enq_bits, vmu_vbaseq_deq_bits;
  wire                 vmu_vbaseq_enq_val, vmu_vbaseq_deq_val;
  wire                 vmu_vbaseq_enq_rdy, vmu_vbaseq_deq_rdy;

  wire [`VMSTRIDE_SZ-1:0] vmu_vstrideq_enq_bits, vmu_vstrideq_deq_bits;
  wire                    vmu_vstrideq_enq_val, vmu_vstrideq_deq_val;
  wire                    vmu_vstrideq_enq_rdy, vmu_vstrideq_deq_rdy;

  wire [`VMRESP_SZ-1:0] vmu_vackq_enq_bits, vmu_vackq_deq_bits;
  wire                  vmu_vackq_enq_val, vmu_vackq_deq_val;
  wire                  vmu_vackq_enq_rdy, vmu_vackq_deq_rdy;

  wire [`UTMCMD_SZ-1:0] vmu_utcmdq_enq_bits, vmu_utcmdq_deq_bits;
  wire                  vmu_utcmdq_enq_val, vmu_utcmdq_deq_val;
  wire                  vmu_utcmdq_enq_rdy, vmu_utcmdq_deq_rdy;

  wire [`UTMIMM_SZ-1:0] vmu_utimmq_enq_bits, vmu_utimmq_deq_bits;
  wire                  vmu_utimmq_enq_val, vmu_utimmq_deq_val;
  wire                  vmu_utimmq_enq_rdy, vmu_utimmq_deq_rdy;

  wire [`UTMRESP_SZ-1:0] vmu_utackq_enq_bits, vmu_utackq_deq_bits;
  wire                   vmu_utackq_enq_val, vmu_utackq_deq_val;
  wire                   vmu_utackq_enq_rdy, vmu_utackq_deq_rdy;

  wire [64:0] lane_vldq_bits;
  wire        lane_vldq_val;
  wire        lane_vldq_val8;
  wire        lane_vldq_rdy;

  wire [64:0] lane_vsdq_bits;
  wire        lane_vsdq_val;
  wire        lane_vsdq_rdy8;

  wire [31:0] lane_utaq_bits;
  wire        lane_utaq_val;
  wire        lane_utaq_rdy8;

  wire [64:0] lane_utldq_bits;
  wire        lane_utldq_val8;
  wire        lane_utldq_rdy;

  wire [64:0] lane_utsdq_bits;
  wire        lane_utsdq_val;
  wire        lane_utsdq_rdy8;

  vuVCU vcu
  (
    .clk    (clk),
    .reset  (reset),

    .vec_cmdq_bits(vec_cmdq_bits),
    .vec_cmdq_val(vec_cmdq_val),
    .vec_cmdq_rdy(vec_cmdq_rdy),

    .vec_ximm1q_bits(vec_ximm1q_bits),
    .vec_ximm1q_val(vec_ximm1q_val),
    .vec_ximm1q_rdy(vec_ximm1q_rdy),

    .vec_ximm2q_bits(vec_ximm2q_bits),
    .vec_ximm2q_val(vec_ximm2q_val),
    .vec_ximm2q_rdy(vec_ximm2q_rdy),

    .vxu_cmdq_bits(vxu_cmdq_enq_bits),
    .vxu_cmdq_val(vxu_cmdq_enq_val),
    .vxu_cmdq_rdy(vxu_cmdq_enq_rdy),

    .vxu_immq_bits(vxu_immq_enq_bits),
    .vxu_immq_val(vxu_immq_enq_val),
    .vxu_immq_rdy(vxu_immq_enq_rdy),

    .vmu_vcmdq_bits(vmu_vcmdq_enq_bits),
    .vmu_vcmdq_val(vmu_vcmdq_enq_val),
    .vmu_vcmdq_rdy(vmu_vcmdq_enq_rdy),

    .vmu_vbaseq_bits(vmu_vbaseq_enq_bits),
    .vmu_vbaseq_val(vmu_vbaseq_enq_val),
    .vmu_vbaseq_rdy(vmu_vbaseq_enq_rdy),

    .vmu_vstrideq_bits(vmu_vstrideq_enq_bits),
    .vmu_vstrideq_val(vmu_vstrideq_enq_val),
    .vmu_vstrideq_rdy(vmu_vstrideq_enq_rdy),

    .vec_ackq_bits(vec_ackq_bits),
    .vec_ackq_val(vec_ackq_val),
    .vec_ackq_rdy(vec_ackq_rdy),

    .vxu_ackq_bits(vxu_ackq_deq_bits),
    .vxu_ackq_val(vxu_ackq_deq_val),
    .vxu_ackq_rdy(vxu_ackq_deq_rdy),

    .vmu_vackq_bits(vmu_vackq_deq_bits),
    .vmu_vackq_val(vmu_vackq_deq_val),
    .vmu_vackq_rdy(vmu_vackq_deq_rdy)
  );

  `VC_SIMPLE_QUEUE(`XCMD_SZ, 8) vxu_cmdq
  (
    .clk      (clk),
    .reset    (reset),
    .enq_bits (vxu_cmdq_enq_bits),
    .enq_val  (vxu_cmdq_enq_val),
    .enq_rdy  (vxu_cmdq_enq_rdy),
    .deq_bits (vxu_cmdq_deq_bits),
    .deq_val  (vxu_cmdq_deq_val),
    .deq_rdy  (vxu_cmdq_deq_rdy)
  );

  `VC_SIMPLE_QUEUE(`XIMM_SZ, 4) vxu_immq
  (
    .clk      (clk),
    .reset    (reset),
    .enq_bits (vxu_immq_enq_bits),
    .enq_val  (vxu_immq_enq_val),
    .enq_rdy  (vxu_immq_enq_rdy),
    .deq_bits (vxu_immq_deq_bits),
    .deq_val  (vxu_immq_deq_val),
    .deq_rdy  (vxu_immq_deq_rdy)
  );

  assign vxu_ackq_deq_bits = vxu_ackq_enq_bits;
  assign vxu_ackq_deq_val = vxu_ackq_enq_val;
  assign vxu_ackq_enq_rdy = vxu_ackq_deq_rdy;

  vuVXU vxu
  (
    .clk    (clk),
    .reset  (reset),

    .illegal(illegal),

    .vxu_cmdq_bits(vxu_cmdq_deq_bits),
    .vxu_cmdq_val(vxu_cmdq_deq_val),
    .vxu_cmdq_rdy(vxu_cmdq_deq_rdy),

    .vxu_immq_bits(vxu_immq_deq_bits),
    .vxu_immq_val(vxu_immq_deq_val),
    .vxu_immq_rdy(vxu_immq_deq_rdy),

    .vxu_ackq_bits(vxu_ackq_enq_bits),
    .vxu_ackq_val(vxu_ackq_enq_val),
    .vxu_ackq_rdy(vxu_ackq_enq_rdy),

    .vmu_utcmdq_bits(vmu_utcmdq_enq_bits),
    .vmu_utcmdq_val(vmu_utcmdq_enq_val),
    .vmu_utcmdq_rdy(vmu_utcmdq_enq_rdy),

    .vmu_utimmq_bits(vmu_utimmq_enq_bits),
    .vmu_utimmq_val(vmu_utimmq_enq_val),
    .vmu_utimmq_rdy(vmu_utimmq_enq_rdy),

    .vmu_utackq_bits(vmu_utackq_deq_bits),
    .vmu_utackq_val(vmu_utackq_deq_val),
    .vmu_utackq_rdy(vmu_utackq_deq_rdy),

    .cp_imul_val(cp_imul_val),
    .cp_imul_rdy(cp_imul_rdy),
    .cp_imul_fn(cp_imul_fn),
    .cp_imul_in0(cp_imul_in0),
    .cp_imul_in1(cp_imul_in1),
    .cp_imul_out(cp_imul_out),

    .cp_fma_val(cp_fma_val),
    .cp_fma_rdy(cp_fma_rdy),
    .cp_fma_fn(cp_fma_fn),
    .cp_fma_in0(cp_fma_in0),
    .cp_fma_in1(cp_fma_in1),
    .cp_fma_in2(cp_fma_in2),
    .cp_fma_out(cp_fma_out),
    .cp_fma_exc(cp_fma_exc),

    .imem_req_addr(imem_req_addr),
    .imem_req_val(imem_req_val),
    .imem_req_rdy(imem_req_rdy),
    .imem_resp_data(imem_resp_data),
    .imem_resp_val(imem_resp_val),

    .lane_vldq_bits(lane_vldq_bits),
    .lane_vldq_val8(lane_vldq_val8),
    .lane_vldq_rdy(lane_vldq_rdy),

    .lane_vsdq_bits(lane_vsdq_bits),
    .lane_vsdq_val(lane_vsdq_val),
    .lane_vsdq_rdy8(lane_vsdq_rdy8),

    .lane_utaq_bits(lane_utaq_bits),
    .lane_utaq_val(lane_utaq_val),
    .lane_utaq_rdy8(lane_utaq_rdy8),

    .lane_utldq_bits(lane_utldq_bits),
    .lane_utldq_val8(lane_utldq_val8),
    .lane_utldq_rdy(lane_utldq_rdy),

    .lane_utsdq_bits(lane_utsdq_bits),
    .lane_utsdq_val(lane_utsdq_val),
    .lane_utsdq_rdy8(lane_utsdq_rdy8)
  );

  `VC_SIMPLE_QUEUE(`VMCMD_SZ, 4) vmu_vcmdq
  (
    .clk      (clk),
    .reset    (reset),
    .enq_bits (vmu_vcmdq_enq_bits),
    .enq_val  (vmu_vcmdq_enq_val),
    .enq_rdy  (vmu_vcmdq_enq_rdy),
    .deq_bits (vmu_vcmdq_deq_bits),
    .deq_val  (vmu_vcmdq_deq_val),
    .deq_rdy  (vmu_vcmdq_deq_rdy)
  );

  `VC_SIMPLE_QUEUE(`VMIMM_SZ, 4) vmu_vbaseq
  (
    .clk      (clk),
    .reset    (reset),
    .enq_bits (vmu_vbaseq_enq_bits),
    .enq_val  (vmu_vbaseq_enq_val),
    .enq_rdy  (vmu_vbaseq_enq_rdy),
    .deq_bits (vmu_vbaseq_deq_bits),
    .deq_val  (vmu_vbaseq_deq_val),
    .deq_rdy  (vmu_vbaseq_deq_rdy)
  );

  `VC_SIMPLE_QUEUE(`VMSTRIDE_SZ, 2) vmu_vstrideq
  (
    .clk      (clk),
    .reset    (reset),
    .enq_bits (vmu_vstrideq_enq_bits),
    .enq_val  (vmu_vstrideq_enq_val),
    .enq_rdy  (vmu_vstrideq_enq_rdy),
    .deq_bits (vmu_vstrideq_deq_bits),
    .deq_val  (vmu_vstrideq_deq_val),
    .deq_rdy  (vmu_vstrideq_deq_rdy)
  );

  assign vmu_vackq_deq_bits = vmu_vackq_enq_bits;
  assign vmu_vackq_deq_val = vmu_vackq_enq_val;
  assign vmu_vackq_enq_rdy = vmu_vackq_deq_rdy;

  `VC_SIMPLE_QUEUE(`UTMCMD_SZ, 4) vmu_utcmdq
  (
    .clk      (clk),
    .reset    (reset),
    .enq_bits (vmu_utcmdq_enq_bits),
    .enq_val  (vmu_utcmdq_enq_val),
    .enq_rdy  (vmu_utcmdq_enq_rdy),
    .deq_bits (vmu_utcmdq_deq_bits),
    .deq_val  (vmu_utcmdq_deq_val),
    .deq_rdy  (vmu_utcmdq_deq_rdy)
  );

  `VC_SIMPLE_QUEUE(`UTMIMM_SZ, 4) vmu_utimmq
  (
    .clk      (clk),
    .reset    (reset),
    .enq_bits (vmu_utimmq_enq_bits),
    .enq_val  (vmu_utimmq_enq_val),
    .enq_rdy  (vmu_utimmq_enq_rdy),
    .deq_bits (vmu_utimmq_deq_bits),
    .deq_val  (vmu_utimmq_deq_val),
    .deq_rdy  (vmu_utimmq_deq_rdy)
  );

  assign vmu_utackq_deq_bits = vmu_utackq_enq_bits;
  assign vmu_utackq_deq_val = vmu_utackq_enq_val;
  assign vmu_utackq_enq_rdy = vmu_utackq_deq_rdy;

  vuVMU vmu
  (
    .clk(clk),
    .reset(reset),

    .vmu_vcmdq_bits(vmu_vcmdq_deq_bits),
    .vmu_vcmdq_val(vmu_vcmdq_deq_val),
    .vmu_vcmdq_rdy(vmu_vcmdq_deq_rdy),

    .vmu_vbaseq_bits(vmu_vbaseq_deq_bits),
    .vmu_vbaseq_val(vmu_vbaseq_deq_val),
    .vmu_vbaseq_rdy(vmu_vbaseq_deq_rdy),

    .vmu_vstrideq_bits(vmu_vstrideq_deq_bits),
    .vmu_vstrideq_val(vmu_vstrideq_deq_val),
    .vmu_vstrideq_rdy(vmu_vstrideq_deq_rdy),

    .vmu_vackq_bits(vmu_vackq_enq_bits),
    .vmu_vackq_val(vmu_vackq_enq_val),
    .vmu_vackq_rdy(vmu_vackq_enq_rdy),

    .vmu_utcmdq_bits(vmu_utcmdq_deq_bits),
    .vmu_utcmdq_val(vmu_utcmdq_deq_val),
    .vmu_utcmdq_rdy(vmu_utcmdq_deq_rdy),

    .vmu_utimmq_bits(vmu_utimmq_deq_bits),
    .vmu_utimmq_val(vmu_utimmq_deq_val),
    .vmu_utimmq_rdy(vmu_utimmq_deq_rdy),

    .vmu_utackq_bits(vmu_utackq_enq_bits),
    .vmu_utackq_val(vmu_utackq_enq_val),
    .vmu_utackq_rdy(vmu_utackq_enq_rdy),

    .lane_vldq_deq_bits(lane_vldq_bits),
    .lane_vldq_deq_val(lane_vldq_val8),
    .lane_vldq_deq_rdy(lane_vldq_rdy),

    .lane_vsdq_enq_bits(lane_vsdq_bits),
    .lane_vsdq_enq_val(lane_vsdq_val),
    .lane_vsdq_enq_rdy(lane_vsdq_rdy8),

    .lane_utaq_enq_bits(lane_utaq_bits),
    .lane_utaq_enq_val(lane_utaq_val),
    .lane_utaq_enq_rdy(lane_utaq_rdy8),

    .lane_utldq_deq_bits(lane_utldq_bits),
    .lane_utldq_deq_val(lane_utldq_val8),
    .lane_utldq_deq_rdy(lane_utldq_rdy),

    .lane_utsdq_enq_bits(lane_utsdq_bits),
    .lane_utsdq_enq_val(lane_utsdq_val),
    .lane_utsdq_enq_rdy(lane_utsdq_rdy8),

    .dmem_req_vec_addr(dmem_req_vec_addr),
    .dmem_req_vec_op(dmem_req_vec_op),
    .dmem_req_vec_data(dmem_req_vec_data),
    .dmem_req_vec_wmask(dmem_req_vec_wmask),
    .dmem_req_vec_tag(dmem_req_vec_tag),
    .dmem_req_vec_val(dmem_req_vec_val),
    .dmem_req_vec_rdy(dmem_req_vec_rdy),
    .dmem_resp_vec_val(dmem_resp_vec_val),

    .dmem_req_ut_addr(dmem_req_ut_addr),
    .dmem_req_ut_op(dmem_req_ut_op),
    .dmem_req_ut_data(dmem_req_ut_data),
    .dmem_req_ut_wmask(dmem_req_ut_wmask),
    .dmem_req_ut_tag(dmem_req_ut_tag),
    .dmem_req_ut_val(dmem_req_ut_val),
    .dmem_req_ut_rdy(dmem_req_ut_rdy),
    .dmem_resp_ut_val(dmem_resp_ut_val),

    .dmem_resp_ut_tag(dmem_resp_ut_tag),
    .dmem_resp_ut_data(dmem_resp_ut_data),
    .dmem_resp_vec_tag(dmem_resp_vec_tag),
    .dmem_resp_vec_data(dmem_resp_vec_data)
  );

endmodule

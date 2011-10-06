`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Issue
(
  input clk,
  input reset,

  output illegal,

  output `DEF_ADDR imem_req_addr,
  output           imem_req_val,
  input            imem_req_rdy,
  input  `DEF_INST imem_resp_data,
  input            imem_resp_val,

  input  `DEF_VXU_CMDQ vxu_cmdq_bits,
  input                vxu_cmdq_val,
  output               vxu_cmdq_rdy,

  input  `DEF_VXU_IMMQ vxu_immq_bits,
  input                vxu_immq_val,
  output               vxu_immq_rdy,

  output `DEF_VMU_UTCMDQ vmu_utcmdq_bits,
  output                 vmu_utcmdq_val,
  input                  vmu_utcmdq_rdy,

  output `DEF_VMU_UTIMMQ vmu_utimmq_bits,
  output                 vmu_utimmq_val,
  input                  vmu_utimmq_rdy,

  output `DEF_BANK   bactive,
  output `DEF_BCNT   bcnt,
  output `DEF_VLEN   vlen,
  output `DEF_REGLEN stride,

  output issue_tvec_val_viu,
  output issue_tvec_val_vlu,
  output issue_tvec_val_vsu,

  input  issue_tvec_rdy,

  output issue_tvec_dhazard_vt,
  output issue_tvec_dhazard_vd,

  output issue_tvec_shazard_vlu,
  output issue_tvec_shazard_vsu,

  output issue_tvec_bhazard_r1w1,
  output issue_tvec_bhazard_vlu,
  output issue_tvec_bhazard_vsu,

  output `DEF_VIU_FN  issue_tvec_fn_viu,

  output              issue_tvec_vt_zero,
  output              issue_tvec_vd_zero,
  output `DEF_REGLEN  issue_tvec_vt,
  output `DEF_REGLEN  issue_tvec_vd,
  output `DEF_DATA    issue_tvec_imm,

  output issue_vt_val_viu,
  output issue_vt_val_vau0,
  output issue_vt_val_vau1,
  output issue_vt_val_vau2,
  output issue_vt_val_vgslu,
  output issue_vt_val_vglu,
  output issue_vt_val_vgsu,

  input  issue_vt_rdy,

  output issue_vt_dhazard_vs,
  output issue_vt_dhazard_vt,
  output issue_vt_dhazard_vr,
  output issue_vt_dhazard_vd,

  output issue_vt_shazard_vau0,
  output issue_vt_shazard_vau1,
  output issue_vt_shazard_vau2,
  output issue_vt_shazard_vgu,
  output issue_vt_shazard_vlu,
  output issue_vt_shazard_vsu,

  output issue_vt_bhazard_r1w1,
  output issue_vt_bhazard_r2w1,
  output issue_vt_bhazard_r3w1,
  output issue_vt_bhazard_vgslu,
  output issue_vt_bhazard_vglu,
  output issue_vt_bhazard_vgsu,

  output `DEF_VIU_FN  issue_vt_fn_viu,
  output `DEF_VAU0_FN issue_vt_fn_vau0,
  output `DEF_VAU1_FN issue_vt_fn_vau1,
  output `DEF_VAU2_FN issue_vt_fn_vau2,

  output              issue_vt_vs_zero,
  output              issue_vt_vt_zero,
  output              issue_vt_vr_zero,
  output              issue_vt_vd_zero,
  output `DEF_REGLEN  issue_vt_vs,
  output `DEF_REGLEN  issue_vt_vt,
  output `DEF_REGLEN  issue_vt_vr,
  output `DEF_REGLEN  issue_vt_vd,
  output `DEF_DATA    issue_vt_imm
);

  wire `DEF_REGCNT nxregs;

  wire             vt_active;
  wire             vt_fire;
  wire             vt_stop;
  wire `DEF_ADDR   vt_pc;

  wire `DEF_VMU_UTCMDQ tvec_vmu_utcmdq_bits;
  wire                 tvec_vmu_utcmdq_val;

  wire `DEF_VMU_UTCMDQ vt_vmu_utcmdq_bits;
  wire                 vt_vmu_utcmdq_val;

  wire `DEF_REGLEN issue_tvec_vs_decoded;
  wire `DEF_REGLEN issue_tvec_vt_decoded;
  wire `DEF_REGLEN issue_tvec_vr_decoded;
  wire `DEF_REGLEN issue_tvec_vd_decoded;
  wire             issue_tvec_vd_val;

  wire `DEF_REGLEN  issue_vt_vs_decoded;
  wire `DEF_REGLEN  issue_vt_vt_decoded;
  wire `DEF_REGLEN  issue_vt_vr_decoded;
  wire `DEF_REGLEN  issue_vt_vd_decoded;
  wire              issue_vt_vd_val;

  vuVXU_Issue_TVEC tvec
  (
    .clk(clk),
    .reset(reset),

    .vlen(vlen),
    .stride(stride),
    .nxregs(nxregs),
    .bactive(bactive),
    .bcnt(bcnt),

    .vt_active(vt_active),
    .vt_fire(vt_fire),
    .vt_stop(vt_stop),
    .vt_pc(vt_pc),

    .vxu_cmdq_bits(vxu_cmdq_bits),
    .vxu_cmdq_val(vxu_cmdq_val),
    .vxu_cmdq_rdy(vxu_cmdq_rdy),

    .vxu_immq_bits(vxu_immq_bits),
    .vxu_immq_val(vxu_immq_val),
    .vxu_immq_rdy(vxu_immq_rdy),

    .vmu_utcmdq_bits(tvec_vmu_utcmdq_bits),
    .vmu_utcmdq_val(tvec_vmu_utcmdq_val),
    .vmu_utcmdq_rdy(vmu_utcmdq_rdy),

    .issue_val_viu(issue_tvec_val_viu),
    .issue_val_vlu(issue_tvec_val_vlu),
    .issue_val_vsu(issue_tvec_val_vsu),

    .issue_rdy(issue_tvec_rdy),

    .issue_dhazard_vt(issue_tvec_dhazard_vt),
    .issue_dhazard_vd(issue_tvec_dhazard_vd),

    .issue_shazard_vlu(issue_tvec_shazard_vlu),
    .issue_shazard_vsu(issue_tvec_shazard_vsu),

    .issue_bhazard_r1w1(issue_tvec_bhazard_r1w1),
    .issue_bhazard_vlu(issue_tvec_bhazard_vlu),
    .issue_bhazard_vsu(issue_tvec_bhazard_vsu),

    .issue_fn_viu(issue_tvec_fn_viu),

    .issue_vt_decoded(issue_tvec_vt_decoded),
    .issue_vd_decoded(issue_tvec_vd_decoded),
    .issue_vd_val(issue_tvec_vd_val),
    .issue_imm(issue_tvec_imm)
  );

  assign issue_tvec_vt_zero = (issue_tvec_vt_decoded == 6'd0);
  assign issue_tvec_vd_zero = (issue_tvec_vd_decoded == 6'd0) & issue_tvec_vd_val;
  assign issue_tvec_vt = issue_tvec_vt_decoded[5] ? nxregs + {1'd0,issue_tvec_vt_decoded[4:0]} - 1'b1 : {1'd0,issue_tvec_vt_decoded[4:0]} - 1'b1;
  assign issue_tvec_vd = issue_tvec_vd_decoded[5] ? nxregs + {1'd0,issue_tvec_vd_decoded[4:0]} - 1'b1 : {1'd0,issue_tvec_vd_decoded[4:0]} - 1'b1;

  vuVXU_Issue_VT vt
  (
    .clk(clk),
    .reset(reset),

    .illegal(illegal),

    .imem_req_addr(imem_req_addr),
    .imem_req_val(imem_req_val),
    .imem_req_rdy(imem_req_rdy),
    .imem_resp_data(imem_resp_data),
    .imem_resp_val(imem_resp_val),

    .vmu_utcmdq_bits(vt_vmu_utcmdq_bits),
    .vmu_utcmdq_val(vt_vmu_utcmdq_val),
    .vmu_utcmdq_rdy(vmu_utcmdq_rdy),

    .vmu_utimmq_bits(vmu_utimmq_bits),
    .vmu_utimmq_val(vmu_utimmq_val),
    .vmu_utimmq_rdy(vmu_utimmq_rdy),

    .vt_active(vt_active),
    .vt_fire(vt_fire),
    .vt_stop(vt_stop),
    .vt_pc(vt_pc),

    .issue_val_viu(issue_vt_val_viu),
    .issue_val_vau0(issue_vt_val_vau0),
    .issue_val_vau1(issue_vt_val_vau1),
    .issue_val_vau2(issue_vt_val_vau2),
    .issue_val_vgslu(issue_vt_val_vgslu),
    .issue_val_vglu(issue_vt_val_vglu),
    .issue_val_vgsu(issue_vt_val_vgsu),

    .issue_rdy(issue_vt_rdy),

    .issue_dhazard_vs(issue_vt_dhazard_vs),
    .issue_dhazard_vt(issue_vt_dhazard_vt),
    .issue_dhazard_vr(issue_vt_dhazard_vr),
    .issue_dhazard_vd(issue_vt_dhazard_vd),

    .issue_shazard_vau0(issue_vt_shazard_vau0),
    .issue_shazard_vau1(issue_vt_shazard_vau1),
    .issue_shazard_vau2(issue_vt_shazard_vau2),
    .issue_shazard_vgu(issue_vt_shazard_vgu),
    .issue_shazard_vlu(issue_vt_shazard_vlu),
    .issue_shazard_vsu(issue_vt_shazard_vsu),

    .issue_bhazard_r1w1(issue_vt_bhazard_r1w1),
    .issue_bhazard_r2w1(issue_vt_bhazard_r2w1),
    .issue_bhazard_r3w1(issue_vt_bhazard_r3w1),
    .issue_bhazard_vgslu(issue_vt_bhazard_vgslu),
    .issue_bhazard_vglu(issue_vt_bhazard_vglu),
    .issue_bhazard_vgsu(issue_vt_bhazard_vgsu),

    .issue_fn_viu(issue_vt_fn_viu),
    .issue_fn_vau0(issue_vt_fn_vau0),
    .issue_fn_vau1(issue_vt_fn_vau1),
    .issue_fn_vau2(issue_vt_fn_vau2),

    .issue_vs_decoded(issue_vt_vs_decoded),
    .issue_vt_decoded(issue_vt_vt_decoded),
    .issue_vr_decoded(issue_vt_vr_decoded),
    .issue_vd_decoded(issue_vt_vd_decoded),
    .issue_vd_val(issue_vt_vd_val),
    .issue_imm(issue_vt_imm)
  );

  assign issue_vt_vs_zero = (issue_vt_vs_decoded == 6'd0);
  assign issue_vt_vt_zero = (issue_vt_vt_decoded == 6'd0);
  assign issue_vt_vr_zero = (issue_vt_vr_decoded == 6'd0);
  assign issue_vt_vd_zero = (issue_vt_vd_decoded == 6'd0) & issue_vt_vd_val;
  assign issue_vt_vs = issue_vt_vs_decoded[5] ? nxregs + {1'd0,issue_vt_vs_decoded[4:0]} - 1'b1 : {1'd0,issue_vt_vs_decoded[4:0]} - 1'b1;
  assign issue_vt_vt = issue_vt_vt_decoded[5] ? nxregs + {1'd0,issue_vt_vt_decoded[4:0]} - 1'b1 : {1'd0,issue_vt_vt_decoded[4:0]} - 1'b1;
  assign issue_vt_vr = issue_vt_vr_decoded[5] ? nxregs + {1'd0,issue_vt_vr_decoded[4:0]} - 1'b1 : {1'd0,issue_vt_vr_decoded[4:0]} - 1'b1;
  assign issue_vt_vd = issue_vt_vd_decoded[5] ? nxregs + {1'd0,issue_vt_vd_decoded[4:0]} - 1'b1 : {1'd0,issue_vt_vd_decoded[4:0]} - 1'b1;

  assign vmu_utcmdq_bits = tvec_vmu_utcmdq_val ? tvec_vmu_utcmdq_bits : {vt_vmu_utcmdq_bits[`UTMCMD_CMDCODE], vlen};
  assign vmu_utcmdq_val = tvec_vmu_utcmdq_val | vt_vmu_utcmdq_val;

endmodule

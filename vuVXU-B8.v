`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU
(
  input clk,
  input reset,

  output illegal,

  input `DEF_VXU_CMDQ vxu_cmdq_bits,
  input               vxu_cmdq_val,
  output              vxu_cmdq_rdy,

  input `DEF_VXU_IMMQ vxu_immq_bits,
  input               vxu_immq_val,
  output              vxu_immq_rdy,

  output `DEF_VXU_ACKQ vxu_ackq_bits,
  output               vxu_ackq_val,
  input                vxu_ackq_rdy,

  output `DEF_VMU_UTCMDQ vmu_utcmdq_bits,
  output                 vmu_utcmdq_val,
  input                  vmu_utcmdq_rdy,

  output `DEF_VMU_UTIMMQ vmu_utimmq_bits,
  output                 vmu_utimmq_val,
  input                  vmu_utimmq_rdy,

  input `DEF_VMU_UTACKQ vmu_utackq_bits,
  input                 vmu_utackq_val,
  output                vmu_utackq_rdy,

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

  output `DEF_ADDR imem_req_addr,
  output           imem_req_val,
  input            imem_req_rdy,
  input  `DEF_INST imem_resp_data,
  input            imem_resp_val,

  input  `DEF_DATA lane_vldq_bits,
  input            lane_vldq_val8,
  output           lane_vldq_rdy,

  output `DEF_DATA lane_vsdq_bits,
  output           lane_vsdq_val,
  input            lane_vsdq_rdy8,

  output `DEF_ADDR lane_utaq_bits,
  output           lane_utaq_val,
  input            lane_utaq_rdy8,

  input  `DEF_DATA lane_utldq_bits,
  input            lane_utldq_val8,
  output           lane_utldq_rdy,

  output `DEF_DATA lane_utsdq_bits,
  output           lane_utsdq_val,
  input            lane_utsdq_rdy8
);

  wire `DEF_VLEN   vlen;
  wire `DEF_REGLEN stride;
  wire `DEF_BANK   bactive;
  wire `DEF_BCNT   bcnt;

  wire issue_tvec_val_viu;
  wire issue_tvec_val_vlu;
  wire issue_tvec_val_vsu;

  wire issue_tvec_rdy;

  wire issue_tvec_dhazard_vt;
  wire issue_tvec_dhazard_vd;

  wire issue_tvec_shazard_vlu;
  wire issue_tvec_shazard_vsu;

  wire issue_tvec_bhazard_r1w1;
  wire issue_tvec_bhazard_vlu;
  wire issue_tvec_bhazard_vsu;

  wire `DEF_VIU_FN  issue_tvec_fn_viu;

  wire              issue_tvec_vt_zero;
  wire              issue_tvec_vd_zero;
  wire `DEF_REGLEN  issue_tvec_vt;
  wire `DEF_REGLEN  issue_tvec_vd;
  wire `DEF_DATA    issue_tvec_imm;

  wire issue_vt_val_viu;
  wire issue_vt_val_vau0;
  wire issue_vt_val_vau1;
  wire issue_vt_val_vau2;
  wire issue_vt_val_vgslu;
  wire issue_vt_val_vglu;
  wire issue_vt_val_vgsu;

  wire issue_vt_rdy;

  wire issue_vt_dhazard_vs;
  wire issue_vt_dhazard_vt;
  wire issue_vt_dhazard_vr;
  wire issue_vt_dhazard_vd;

  wire issue_vt_shazard_vau0;
  wire issue_vt_shazard_vau1;
  wire issue_vt_shazard_vau2;
  wire issue_vt_shazard_vgu;
  wire issue_vt_shazard_vlu;
  wire issue_vt_shazard_vsu;

  wire issue_vt_bhazard_r1w1;
  wire issue_vt_bhazard_r2w1;
  wire issue_vt_bhazard_r3w1;
  wire issue_vt_bhazard_vgslu;
  wire issue_vt_bhazard_vglu;
  wire issue_vt_bhazard_vgsu;

  wire `DEF_VIU_FN  issue_vt_fn_viu;
  wire `DEF_VAU0_FN issue_vt_fn_vau0;
  wire `DEF_VAU1_FN issue_vt_fn_vau1;
  wire `DEF_VAU2_FN issue_vt_fn_vau2;

  wire              issue_vt_vs_zero;
  wire              issue_vt_vt_zero;
  wire              issue_vt_vr_zero;
  wire              issue_vt_vd_zero;
  wire `DEF_REGLEN  issue_vt_vs;
  wire `DEF_REGLEN  issue_vt_vt;
  wire `DEF_REGLEN  issue_vt_vr;
  wire `DEF_REGLEN  issue_vt_vd;
  wire `DEF_DATA    issue_vt_imm;

  wire fire_viu;
  wire fire_vau0;
  wire fire_vau1;
  wire fire_vau2;
  wire fire_vgslu;
  wire fire_vglu;
  wire fire_vgsu;
  wire fire_vlu;
  wire fire_vsu;

  wire `DEF_VIU_FN  fire_fn_viu;
  wire `DEF_VAU0_FN fire_fn_vau0;
  wire `DEF_VAU1_FN fire_fn_vau1;
  wire `DEF_VAU2_FN fire_fn_vau2;

  wire              fire_vs_zero;
  wire              fire_vt_zero;
  wire              fire_vr_zero;
  wire `DEF_REGLEN  fire_vs;
  wire `DEF_REGLEN  fire_vt;
  wire `DEF_REGLEN  fire_vr;
  wire `DEF_REGLEN  fire_vd;
  wire `DEF_DATA    fire_imm;

  wire `DEF_STALL seq_stall;
  wire            seq_last;

  wire seq_viu;
  wire seq_vau0;
  wire seq_vau1;
  wire seq_vau2;
  wire seq_vldq;
  wire seq_vsdq;
  wire seq_utaq;
  wire seq_utldq;
  wire seq_utsdq;

  wire `DEF_VIU_FN  seq_fn_viu;
  wire `DEF_VAU0_FN seq_fn_vau0;
  wire `DEF_VAU1_FN seq_fn_vau1;
  wire `DEF_VAU2_FN seq_fn_vau2;
  wire `DEF_BVLEN   seq_cnt;
  wire `DEF_VLEN    seq_utidx;
  wire              seq_vs_zero;
  wire              seq_vt_zero;
  wire              seq_vr_zero;
  wire `DEF_BREGLEN seq_vs;
  wire `DEF_BREGLEN seq_vt;
  wire `DEF_BREGLEN seq_vr;
  wire `DEF_BREGLEN seq_vd;
  wire `DEF_DATA    seq_imm;

  wire              expand_ren;
  wire              expand_rlast;
  wire `DEF_BVLEN   expand_rcnt;
  wire `DEF_BREGLEN expand_raddr;
  wire `DEF_BOPL    expand_roplen;
  wire `DEF_BRPORT  expand_rblen;

  wire              expand_wen;
  wire              expand_wlast;
  wire `DEF_BVLEN   expand_wcnt;
  wire `DEF_BREGLEN expand_waddr;
  wire `DEF_BWPORT  expand_wsel;

  wire              expand_viu;
  wire `DEF_VIU_FN  expand_viu_fn;
  wire `DEF_VLEN    expand_viu_utidx;
  wire `DEF_DATA    expand_viu_imm;
  wire              expand_vau0;
  wire `DEF_VAU0_FN expand_vau0_fn;
  wire              expand_vau1;
  wire `DEF_VAU1_FN expand_vau1_fn;
  wire              expand_vau2;
  wire `DEF_VAU2_FN expand_vau2_fn;
  wire              expand_vldq;
  wire              expand_vsdq;
  wire              expand_utaq;
  wire              expand_utldq;
  wire              expand_utsdq;

  wire lane_rlast;
  wire lane_wlast;

  vuVXU_Issue issue
  (
    .clk(clk),
    .reset(reset),

    .illegal(illegal),

    .imem_req_addr(imem_req_addr),
    .imem_req_val(imem_req_val),
    .imem_req_rdy(imem_req_rdy),
    .imem_resp_data(imem_resp_data),
    .imem_resp_val(imem_resp_val),

    .vxu_cmdq_bits(vxu_cmdq_bits),
    .vxu_cmdq_val(vxu_cmdq_val),
    .vxu_cmdq_rdy(vxu_cmdq_rdy),

    .vxu_immq_bits(vxu_immq_bits),
    .vxu_immq_val(vxu_immq_val),
    .vxu_immq_rdy(vxu_immq_rdy),

    .vmu_utcmdq_bits(vmu_utcmdq_bits),
    .vmu_utcmdq_val(vmu_utcmdq_val),
    .vmu_utcmdq_rdy(vmu_utcmdq_rdy),

    .vmu_utimmq_bits(vmu_utimmq_bits),
    .vmu_utimmq_val(vmu_utimmq_val),
    .vmu_utimmq_rdy(vmu_utimmq_rdy),

    .vlen(vlen),
    .stride(stride),
    .bactive(bactive),
    .bcnt(bcnt),

    .issue_tvec_val_viu(issue_tvec_val_viu),
    .issue_tvec_val_vlu(issue_tvec_val_vlu),
    .issue_tvec_val_vsu(issue_tvec_val_vsu),

    .issue_tvec_rdy(issue_tvec_rdy),

    .issue_tvec_dhazard_vt(issue_tvec_dhazard_vt),
    .issue_tvec_dhazard_vd(issue_tvec_dhazard_vd),

    .issue_tvec_shazard_vlu(issue_tvec_shazard_vlu),
    .issue_tvec_shazard_vsu(issue_tvec_shazard_vsu),

    .issue_tvec_bhazard_r1w1(issue_tvec_bhazard_r1w1),
    .issue_tvec_bhazard_vlu(issue_tvec_bhazard_vlu),
    .issue_tvec_bhazard_vsu(issue_tvec_bhazard_vsu),

    .issue_tvec_fn_viu(issue_tvec_fn_viu),

    .issue_tvec_vt_zero(issue_tvec_vt_zero),
    .issue_tvec_vd_zero(issue_tvec_vd_zero),
    .issue_tvec_vt(issue_tvec_vt),
    .issue_tvec_vd(issue_tvec_vd),
    .issue_tvec_imm(issue_tvec_imm),

    .issue_vt_val_viu(issue_vt_val_viu),
    .issue_vt_val_vau0(issue_vt_val_vau0),
    .issue_vt_val_vau1(issue_vt_val_vau1),
    .issue_vt_val_vau2(issue_vt_val_vau2),
    .issue_vt_val_vgslu(issue_vt_val_vgslu),
    .issue_vt_val_vglu(issue_vt_val_vglu),
    .issue_vt_val_vgsu(issue_vt_val_vgsu),

    .issue_vt_rdy(issue_vt_rdy),

    .issue_vt_dhazard_vs(issue_vt_dhazard_vs),
    .issue_vt_dhazard_vt(issue_vt_dhazard_vt),
    .issue_vt_dhazard_vr(issue_vt_dhazard_vr),
    .issue_vt_dhazard_vd(issue_vt_dhazard_vd),

    .issue_vt_shazard_vau0(issue_vt_shazard_vau0),
    .issue_vt_shazard_vau1(issue_vt_shazard_vau1),
    .issue_vt_shazard_vau2(issue_vt_shazard_vau2),
    .issue_vt_shazard_vgu(issue_vt_shazard_vgu),
    .issue_vt_shazard_vlu(issue_vt_shazard_vlu),
    .issue_vt_shazard_vsu(issue_vt_shazard_vsu),

    .issue_vt_bhazard_r1w1(issue_vt_bhazard_r1w1),
    .issue_vt_bhazard_r2w1(issue_vt_bhazard_r2w1),
    .issue_vt_bhazard_r3w1(issue_vt_bhazard_r3w1),
    .issue_vt_bhazard_vgslu(issue_vt_bhazard_vgslu),
    .issue_vt_bhazard_vglu(issue_vt_bhazard_vglu),
    .issue_vt_bhazard_vgsu(issue_vt_bhazard_vgsu),

    .issue_vt_fn_viu(issue_vt_fn_viu),
    .issue_vt_fn_vau0(issue_vt_fn_vau0),
    .issue_vt_fn_vau1(issue_vt_fn_vau1),
    .issue_vt_fn_vau2(issue_vt_fn_vau2),

    .issue_vt_vs_zero(issue_vt_vs_zero),
    .issue_vt_vt_zero(issue_vt_vt_zero),
    .issue_vt_vr_zero(issue_vt_vr_zero),
    .issue_vt_vd_zero(issue_vt_vd_zero),
    .issue_vt_vs(issue_vt_vs),
    .issue_vt_vt(issue_vt_vt),
    .issue_vt_vr(issue_vt_vr),
    .issue_vt_vd(issue_vt_vd),
    .issue_vt_imm(issue_vt_imm)
  );

  vuVXU_Banked8_Fire b8fire
  (
    .clk(clk),
    .reset(reset),

    .issue_tvec_val_viu(issue_tvec_val_viu),
    .issue_tvec_val_vlu(issue_tvec_val_vlu),
    .issue_tvec_val_vsu(issue_tvec_val_vsu),

    .issue_tvec_rdy(issue_tvec_rdy),

    .issue_tvec_dhazard_vt(issue_tvec_dhazard_vt),
    .issue_tvec_dhazard_vd(issue_tvec_dhazard_vd),

    .issue_tvec_shazard_vlu(issue_tvec_shazard_vlu),
    .issue_tvec_shazard_vsu(issue_tvec_shazard_vsu),

    .issue_tvec_bhazard_r1w1(issue_tvec_bhazard_r1w1),
    .issue_tvec_bhazard_vlu(issue_tvec_bhazard_vlu),
    .issue_tvec_bhazard_vsu(issue_tvec_bhazard_vsu),

    .issue_tvec_fn_viu(issue_tvec_fn_viu),

    .issue_tvec_vt_zero(issue_tvec_vt_zero),
    .issue_tvec_vd_zero(issue_tvec_vd_zero),
    .issue_tvec_vt(issue_tvec_vt),
    .issue_tvec_vd(issue_tvec_vd),
    .issue_tvec_imm(issue_tvec_imm),

    .issue_vt_val_viu(issue_vt_val_viu),
    .issue_vt_val_vau0(issue_vt_val_vau0),
    .issue_vt_val_vau1(issue_vt_val_vau1),
    .issue_vt_val_vau2(issue_vt_val_vau2),
    .issue_vt_val_vgslu(issue_vt_val_vgslu),
    .issue_vt_val_vglu(issue_vt_val_vglu),
    .issue_vt_val_vgsu(issue_vt_val_vgsu),

    .issue_vt_rdy(issue_vt_rdy),

    .issue_vt_dhazard_vs(issue_vt_dhazard_vs),
    .issue_vt_dhazard_vt(issue_vt_dhazard_vt),
    .issue_vt_dhazard_vr(issue_vt_dhazard_vr),
    .issue_vt_dhazard_vd(issue_vt_dhazard_vd),

    .issue_vt_shazard_vau0(issue_vt_shazard_vau0),
    .issue_vt_shazard_vau1(issue_vt_shazard_vau1),
    .issue_vt_shazard_vau2(issue_vt_shazard_vau2),
    .issue_vt_shazard_vgu(issue_vt_shazard_vgu),
    .issue_vt_shazard_vlu(issue_vt_shazard_vlu),
    .issue_vt_shazard_vsu(issue_vt_shazard_vsu),

    .issue_vt_bhazard_r1w1(issue_vt_bhazard_r1w1),
    .issue_vt_bhazard_r2w1(issue_vt_bhazard_r2w1),
    .issue_vt_bhazard_r3w1(issue_vt_bhazard_r3w1),
    .issue_vt_bhazard_vgslu(issue_vt_bhazard_vgslu),
    .issue_vt_bhazard_vglu(issue_vt_bhazard_vglu),
    .issue_vt_bhazard_vgsu(issue_vt_bhazard_vgsu),

    .issue_vt_fn_viu(issue_vt_fn_viu),
    .issue_vt_fn_vau0(issue_vt_fn_vau0),
    .issue_vt_fn_vau1(issue_vt_fn_vau1),
    .issue_vt_fn_vau2(issue_vt_fn_vau2),

    .issue_vt_vs_zero(issue_vt_vs_zero),
    .issue_vt_vt_zero(issue_vt_vt_zero),
    .issue_vt_vr_zero(issue_vt_vr_zero),
    .issue_vt_vd_zero(issue_vt_vd_zero),
    .issue_vt_vs(issue_vt_vs),
    .issue_vt_vt(issue_vt_vt),
    .issue_vt_vr(issue_vt_vr),
    .issue_vt_vd(issue_vt_vd),
    .issue_vt_imm(issue_vt_imm),

    .fire_viu(fire_viu),
    .fire_vau0(fire_vau0),
    .fire_vau1(fire_vau1),
    .fire_vau2(fire_vau2),
    .fire_vgslu(fire_vgslu),
    .fire_vglu(fire_vglu),
    .fire_vgsu(fire_vgsu),
    .fire_vlu(fire_vlu),
    .fire_vsu(fire_vsu),

    .fire_fn_viu(fire_fn_viu),
    .fire_fn_vau0(fire_fn_vau0),
    .fire_fn_vau1(fire_fn_vau1),
    .fire_fn_vau2(fire_fn_vau2),

    .fire_vs_zero(fire_vs_zero),
    .fire_vt_zero(fire_vt_zero),
    .fire_vr_zero(fire_vr_zero),
    .fire_vs(fire_vs),
    .fire_vt(fire_vt),
    .fire_vr(fire_vr),
    .fire_vd(fire_vd),
    .fire_imm(fire_imm)
  );

  vuVXU_Banked8_Hazard b8hazard
  (
    .clk(clk),
    .reset(reset),

    .bcnt(bcnt),

    .issue_tvec_val_viu(issue_tvec_val_viu),
    .issue_tvec_val_vlu(issue_tvec_val_vlu),
    .issue_tvec_val_vsu(issue_tvec_val_vsu),

    .issue_tvec_rdy(issue_tvec_rdy),

    .issue_tvec_dhazard_vt(issue_tvec_dhazard_vt),
    .issue_tvec_dhazard_vd(issue_tvec_dhazard_vd),

    .issue_tvec_shazard_vlu(issue_tvec_shazard_vlu),
    .issue_tvec_shazard_vsu(issue_tvec_shazard_vsu),

    .issue_tvec_bhazard_r1w1(issue_tvec_bhazard_r1w1),
    .issue_tvec_bhazard_vlu(issue_tvec_bhazard_vlu),
    .issue_tvec_bhazard_vsu(issue_tvec_bhazard_vsu),

    .issue_tvec_fn_viu(issue_tvec_fn_viu),

    .issue_tvec_vt_zero(issue_tvec_vt_zero),
    .issue_tvec_vd_zero(issue_tvec_vd_zero),
    .issue_tvec_vt(issue_tvec_vt),
    .issue_tvec_vd(issue_tvec_vd),

    .issue_vt_val_viu(issue_vt_val_viu),
    .issue_vt_val_vau0(issue_vt_val_vau0),
    .issue_vt_val_vau1(issue_vt_val_vau1),
    .issue_vt_val_vau2(issue_vt_val_vau2),
    .issue_vt_val_vgslu(issue_vt_val_vgslu),
    .issue_vt_val_vglu(issue_vt_val_vglu),
    .issue_vt_val_vgsu(issue_vt_val_vgsu),

    .issue_vt_rdy(issue_vt_rdy),

    .issue_vt_dhazard_vs(issue_vt_dhazard_vs),
    .issue_vt_dhazard_vt(issue_vt_dhazard_vt),
    .issue_vt_dhazard_vr(issue_vt_dhazard_vr),
    .issue_vt_dhazard_vd(issue_vt_dhazard_vd),

    .issue_vt_shazard_vau0(issue_vt_shazard_vau0),
    .issue_vt_shazard_vau1(issue_vt_shazard_vau1),
    .issue_vt_shazard_vau2(issue_vt_shazard_vau2),
    .issue_vt_shazard_vgu(issue_vt_shazard_vgu),
    .issue_vt_shazard_vlu(issue_vt_shazard_vlu),
    .issue_vt_shazard_vsu(issue_vt_shazard_vsu),

    .issue_vt_bhazard_r1w1(issue_vt_bhazard_r1w1),
    .issue_vt_bhazard_r2w1(issue_vt_bhazard_r2w1),
    .issue_vt_bhazard_r3w1(issue_vt_bhazard_r3w1),
    .issue_vt_bhazard_vgslu(issue_vt_bhazard_vgslu),
    .issue_vt_bhazard_vglu(issue_vt_bhazard_vglu),
    .issue_vt_bhazard_vgsu(issue_vt_bhazard_vgsu),

    .issue_vt_fn_viu(issue_vt_fn_viu),
    .issue_vt_fn_vau0(issue_vt_fn_vau0),
    .issue_vt_fn_vau1(issue_vt_fn_vau1),
    .issue_vt_fn_vau2(issue_vt_fn_vau2),

    .issue_vt_vs_zero(issue_vt_vs_zero),
    .issue_vt_vt_zero(issue_vt_vt_zero),
    .issue_vt_vr_zero(issue_vt_vr_zero),
    .issue_vt_vd_zero(issue_vt_vd_zero),
    .issue_vt_vs(issue_vt_vs),
    .issue_vt_vt(issue_vt_vt),
    .issue_vt_vr(issue_vt_vr),
    .issue_vt_vd(issue_vt_vd),

    .fire_viu(fire_viu),
    .fire_vau0(fire_vau0),
    .fire_vau1(fire_vau1),
    .fire_vau2(fire_vau2),
    .fire_vgslu(fire_vgslu),
    .fire_vglu(fire_vglu),
    .fire_vgsu(fire_vgsu),
    .fire_vlu(fire_vlu),
    .fire_vsu(fire_vsu),

    .fire_fn_viu(fire_fn_viu),
    .fire_fn_vau1(fire_fn_vau1),

    .fire_vd(fire_vd),

    .seq_stall(seq_stall),
    .seq_last(seq_last),

    .expand_ren(expand_ren),
    .expand_wen(expand_wen),

    .lane_rlast(lane_rlast),
    .lane_wlast(lane_wlast)
  );

  vuVXU_Banked8_Seq b8seq
  (
    .clk(clk),
    .reset(reset),

    .vlen(vlen),
    .stride(stride),
    .bcnt(bcnt),

    .vldq_qstall(~lane_vldq_val8),
    .vsdq_qstall(~lane_vsdq_rdy8),
    .utaq_qstall(~lane_utaq_rdy8),
    .utldq_qstall(~lane_utldq_val8),
    .utsdq_qstall(~lane_utsdq_rdy8),

    .fire_viu(fire_viu),
    .fire_vau0(fire_vau0),
    .fire_vau1(fire_vau1),
    .fire_vau2(fire_vau2),
    .fire_vgslu(fire_vgslu),
    .fire_vglu(fire_vglu),
    .fire_vgsu(fire_vgsu),
    .fire_vlu(fire_vlu),
    .fire_vsu(fire_vsu),

    .fire_fn_viu(fire_fn_viu),
    .fire_fn_vau0(fire_fn_vau0),
    .fire_fn_vau1(fire_fn_vau1),
    .fire_fn_vau2(fire_fn_vau2),

    .fire_vs_zero(fire_vs_zero),
    .fire_vt_zero(fire_vt_zero),
    .fire_vr_zero(fire_vr_zero),
    .fire_vs(fire_vs),
    .fire_vt(fire_vt),
    .fire_vr(fire_vr),
    .fire_vd(fire_vd),
    .fire_imm(fire_imm),

    .seq_stall(seq_stall),
    .seq_last(seq_last),

    .seq_viu(seq_viu),
    .seq_vau0(seq_vau0),
    .seq_vau1(seq_vau1),
    .seq_vau2(seq_vau2),
    .seq_vldq(seq_vldq),
    .seq_vsdq(seq_vsdq),
    .seq_utaq(seq_utaq),
    .seq_utldq(seq_utldq),
    .seq_utsdq(seq_utsdq),

    .seq_fn_viu(seq_fn_viu),
    .seq_fn_vau0(seq_fn_vau0),
    .seq_fn_vau1(seq_fn_vau1),
    .seq_fn_vau2(seq_fn_vau2),

    .seq_cnt(seq_cnt),
    .seq_utidx(seq_utidx),
    .seq_vs_zero(seq_vs_zero),
    .seq_vt_zero(seq_vt_zero),
    .seq_vr_zero(seq_vr_zero),
    .seq_vs(seq_vs),
    .seq_vt(seq_vt),
    .seq_vr(seq_vr),
    .seq_vd(seq_vd),
    .seq_imm(seq_imm)
  );

  vuVXU_Banked8_Expand b8expand
  (
    .clk(clk),
    .reset(reset),

    .seq_last(seq_last),

    .seq_viu(seq_viu),
    .seq_vau0(seq_vau0),
    .seq_vau1(seq_vau1),
    .seq_vau2(seq_vau2),
    .seq_vldq(seq_vldq),
    .seq_vsdq(seq_vsdq),
    .seq_utaq(seq_utaq),
    .seq_utldq(seq_utldq),
    .seq_utsdq(seq_utsdq),

    .seq_fn_viu(seq_fn_viu),
    .seq_fn_vau0(seq_fn_vau0),
    .seq_fn_vau1(seq_fn_vau1),
    .seq_fn_vau2(seq_fn_vau2),

    .seq_cnt(seq_cnt),
    .seq_utidx(seq_utidx),
    .seq_vs_zero(seq_vs_zero),
    .seq_vt_zero(seq_vt_zero),
    .seq_vr_zero(seq_vr_zero),
    .seq_vs(seq_vs),
    .seq_vt(seq_vt),
    .seq_vr(seq_vr),
    .seq_vd(seq_vd),
    .seq_imm(seq_imm),

    .expand_ren(expand_ren),
    .expand_rlast(expand_rlast),
    .expand_rcnt(expand_rcnt),
    .expand_raddr(expand_raddr),
    .expand_roplen(expand_roplen),
    .expand_rblen(expand_rblen),

    .expand_wen(expand_wen),
    .expand_wlast(expand_wlast),
    .expand_wcnt(expand_wcnt),
    .expand_waddr(expand_waddr),
    .expand_wsel(expand_wsel),

    .expand_viu(expand_viu),
    .expand_viu_fn(expand_viu_fn),
    .expand_viu_utidx(expand_viu_utidx),
    .expand_viu_imm(expand_viu_imm),

    .expand_vau0(expand_vau0),
    .expand_vau0_fn(expand_vau0_fn),
    .expand_vau1(expand_vau1),
    .expand_vau1_fn(expand_vau1_fn),
    .expand_vau2(expand_vau2),
    .expand_vau2_fn(expand_vau2_fn),
    .expand_vldq(expand_vldq),
    .expand_vsdq(expand_vsdq),
    .expand_utaq(expand_utaq),
    .expand_utldq(expand_utldq),
    .expand_utsdq(expand_utsdq)
  );

  vuVXU_Banked8_Lane b8lane
  (
    .clk(clk),
    .reset(reset),

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

    .bactive(bactive),

    .expand_ren(expand_ren),
    .expand_rlast(expand_rlast),
    .expand_rcnt(expand_rcnt),
    .expand_raddr(expand_raddr),
    .expand_roplen(expand_roplen),
    .expand_rblen(expand_rblen),

    .expand_wen(expand_wen),
    .expand_wlast(expand_wlast),
    .expand_wcnt(expand_wcnt),
    .expand_waddr(expand_waddr),
    .expand_wsel(expand_wsel),

    .expand_viu(expand_viu),
    .expand_viu_fn(expand_viu_fn),
    .expand_viu_utidx(expand_viu_utidx),
    .expand_viu_imm(expand_viu_imm),

    .expand_vau0(expand_vau0),
    .expand_vau0_fn(expand_vau0_fn),
    .expand_vau1(expand_vau1),
    .expand_vau1_fn(expand_vau1_fn),
    .expand_vau2(expand_vau2),
    .expand_vau2_fn(expand_vau2_fn),
    .expand_vldq(expand_vldq),
    .expand_vsdq(expand_vsdq),
    .expand_utaq(expand_utaq),
    .expand_utldq(expand_utldq),
    .expand_utsdq(expand_utsdq),

    .lane_rlast(lane_rlast),
    .lane_wlast(lane_wlast),

    .vldq_rdy(lane_vldq_rdy),
    .vldq_bits(lane_vldq_bits),
    .vsdq_val(lane_vsdq_val),
    .vsdq_bits(lane_vsdq_bits),
    .utaq_val(lane_utaq_val),
    .utaq_bits(lane_utaq_bits),
    .utldq_rdy(lane_utldq_rdy),
    .utldq_bits(lane_utldq_bits),
    .utsdq_val(lane_utsdq_val),
    .utsdq_bits(lane_utsdq_bits)
  );

  // responses
  assign vxu_ackq_bits = vmu_utackq_bits;
  assign vxu_ackq_val = vmu_utackq_val;
  assign vmu_utackq_rdy = vxu_ackq_rdy;

endmodule

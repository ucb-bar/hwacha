`include "vuVXU-B8-Config.vh"

module vuVXU_Banked8_Fire
(
  input clk,
  input reset,

  input issue_tvec_val_viu,
  input issue_tvec_val_vlu,
  input issue_tvec_val_vsu,

  input issue_tvec_rdy,

  input issue_tvec_dhazard_vt,
  input issue_tvec_dhazard_vd,

  input issue_tvec_shazard_vlu,
  input issue_tvec_shazard_vsu,

  input issue_tvec_bhazard_r1w1,
  input issue_tvec_bhazard_vlu,
  input issue_tvec_bhazard_vsu,

  input `DEF_VIU_FN  issue_tvec_fn_viu,

  input              issue_tvec_vt_zero,
  input              issue_tvec_vd_zero,
  input `DEF_REGLEN  issue_tvec_vt,
  input `DEF_REGLEN  issue_tvec_vd,
  input `DEF_DATA    issue_tvec_imm,

  input issue_vt_val_viu,
  input issue_vt_val_vau0,
  input issue_vt_val_vau1,
  input issue_vt_val_vau2,
  input issue_vt_val_vgslu,
  input issue_vt_val_vglu,
  input issue_vt_val_vgsu,

  input issue_vt_rdy,

  input issue_vt_dhazard_vs,
  input issue_vt_dhazard_vt,
  input issue_vt_dhazard_vr,
  input issue_vt_dhazard_vd,

  input issue_vt_shazard_vau0,
  input issue_vt_shazard_vau1,
  input issue_vt_shazard_vau2,
  input issue_vt_shazard_vgu,
  input issue_vt_shazard_vlu,
  input issue_vt_shazard_vsu,

  input issue_vt_bhazard_r1w1,
  input issue_vt_bhazard_r2w1,
  input issue_vt_bhazard_r3w1,
  input issue_vt_bhazard_vgslu,
  input issue_vt_bhazard_vglu,
  input issue_vt_bhazard_vgsu,

  input `DEF_VIU_FN  issue_vt_fn_viu,
  input `DEF_VAU0_FN issue_vt_fn_vau0,
  input `DEF_VAU1_FN issue_vt_fn_vau1,
  input `DEF_VAU2_FN issue_vt_fn_vau2,

  input              issue_vt_vs_zero,
  input              issue_vt_vt_zero,
  input              issue_vt_vr_zero,
  input              issue_vt_vd_zero,
  input `DEF_REGLEN  issue_vt_vs,
  input `DEF_REGLEN  issue_vt_vt,
  input `DEF_REGLEN  issue_vt_vr,
  input `DEF_REGLEN  issue_vt_vd,
  input `DEF_DATA    issue_vt_imm,

  output fire_viu,
  output fire_vau0,
  output fire_vau1,
  output fire_vau2,
  output fire_vgslu,
  output fire_vglu,
  output fire_vgsu,
  output fire_vlu,
  output fire_vsu,

  output `DEF_VIU_FN  fire_fn_viu,
  output `DEF_VAU0_FN fire_fn_vau0,
  output `DEF_VAU1_FN fire_fn_vau1,
  output `DEF_VAU2_FN fire_fn_vau2,

  output              fire_vs_zero,
  output              fire_vt_zero,
  output              fire_vr_zero,
  output `DEF_REGLEN  fire_vs,
  output `DEF_REGLEN  fire_vt,
  output `DEF_REGLEN  fire_vr,
  output `DEF_REGLEN  fire_vd,
  output `DEF_DATA    fire_imm
);

  wire fire_tvec_viu = ~issue_tvec_vd_zero & issue_tvec_val_viu & issue_tvec_rdy;
  wire fire_tvec_vlu = ~issue_tvec_vd_zero & issue_tvec_val_vlu & issue_tvec_rdy;
  wire fire_tvec_vsu = ~issue_tvec_vd_zero & issue_tvec_val_vsu & issue_tvec_rdy;
  wire fire_tvec = fire_tvec_viu | fire_tvec_vlu | fire_tvec_vsu;

  wire fire_vt_viu = ~issue_vt_vd_zero & issue_vt_val_viu & issue_vt_rdy;
  wire fire_vt_vau0 = ~issue_vt_vd_zero & issue_vt_val_vau0 & issue_vt_rdy;
  wire fire_vt_vau1 = ~issue_vt_vd_zero & issue_vt_val_vau1 & issue_vt_rdy;
  wire fire_vt_vau2 = ~issue_vt_vd_zero & issue_vt_val_vau2 & issue_vt_rdy;
  wire fire_vt_vgslu = ~issue_vt_vd_zero & issue_vt_val_vgslu & issue_vt_rdy;
  wire fire_vt_vglu = ~issue_vt_vd_zero & issue_vt_val_vglu & issue_vt_rdy;
  wire fire_vt_vgsu = ~issue_vt_vd_zero & issue_vt_val_vgsu & issue_vt_rdy;

  assign fire_viu = fire_tvec_viu | fire_vt_viu;
  assign fire_vau0 = fire_vt_vau0;
  assign fire_vau1 = fire_vt_vau1;
  assign fire_vau2 = fire_vt_vau2;
  assign fire_vgslu = fire_vt_vgslu;
  assign fire_vglu = fire_vt_vglu;
  assign fire_vgsu = fire_vt_vgsu;
  assign fire_vlu = fire_tvec_vlu;
  assign fire_vsu = fire_tvec_vsu;

  assign fire_fn_viu = fire_tvec ? issue_tvec_fn_viu : issue_vt_fn_viu;
  assign fire_fn_vau0 = issue_vt_fn_vau0;
  assign fire_fn_vau1 = issue_vt_fn_vau1;
  assign fire_fn_vau2 = issue_vt_fn_vau2;

  assign fire_vs_zero = issue_vt_vs_zero;
  assign fire_vt_zero = fire_tvec ? issue_tvec_vt_zero : issue_vt_vt_zero;
  assign fire_vr_zero = issue_vt_vr_zero;
  assign fire_vs = issue_vt_vs;
  assign fire_vt = fire_tvec ? issue_tvec_vt : issue_vt_vt;
  assign fire_vr = issue_vt_vr;
  assign fire_vd = fire_tvec ? issue_tvec_vd : issue_vt_vd;
  assign fire_imm = fire_tvec ? issue_tvec_imm : issue_vt_imm;

endmodule

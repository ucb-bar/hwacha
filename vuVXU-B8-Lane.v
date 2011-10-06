`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Banked8_Lane
(
  input clk,
  input reset,

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

  input `DEF_BANK bactive,

  input               expand_ren,
  input               expand_rlast,
  input  `DEF_BVLEN   expand_rcnt,
  input  `DEF_BREGLEN expand_raddr,
  input  `DEF_BOPL    expand_roplen,
  input  `DEF_BRPORT  expand_rblen,

  input               expand_wen,
  input               expand_wlast,
  input  `DEF_BVLEN   expand_wcnt,
  input  `DEF_BREGLEN expand_waddr,
  input  `DEF_BWPORT  expand_wsel,

  input               expand_viu,
  input  `DEF_VIU_FN  expand_viu_fn,
  input  `DEF_VLEN    expand_viu_utidx,
  input  `DEF_DATA    expand_viu_imm,

  input               expand_vau0,
  input  `DEF_VAU0_FN expand_vau0_fn,
  input               expand_vau1,
  input  `DEF_VAU1_FN expand_vau1_fn,
  input               expand_vau2,
  input  `DEF_VAU2_FN expand_vau2_fn,
  input               expand_vldq,
  input               expand_vsdq,
  input               expand_utaq,
  input               expand_utldq,
  input               expand_utsdq,

  output lane_rlast,
  output lane_wlast,

  output           vldq_rdy,
  input  `DEF_DATA vldq_bits,
  output           vsdq_val,
  output `DEF_DATA vsdq_bits,
  output           utaq_val,
  output `DEF_ADDR utaq_bits,
  output           utldq_rdy,
  input  `DEF_DATA utldq_bits,
  output           utsdq_val,
  output `DEF_DATA utsdq_bits
);

  wire              conn_ren [0:8];
  wire              conn_rlast [0:8];
  wire `DEF_BVLEN   conn_rcnt [0:8];
  wire `DEF_BREGLEN conn_raddr [0:8];
  wire `DEF_BOPL    conn_roplen [0:8];
  wire `DEF_BRPORT  conn_rblen [0:8];

  wire              conn_wen [0:8];
  wire              conn_wlast [0:8];
  wire `DEF_BVLEN   conn_wcnt [0:8];
  wire `DEF_BREGLEN conn_waddr [0:8];
  wire `DEF_BWPORT  conn_wsel [0:8];

  wire              conn_viu_val [0:8];
  wire `DEF_VIU_FN  conn_viu_fn [0:8];
  wire `DEF_VLEN    conn_viu_utidx [0:8];
  wire `DEF_DATA    conn_viu_imm [0:8];

  wire `DEF_BRPORT rblen [0:7];
  wire `DEF_DATA   rdata [0:7];
  wire `DEF_DATA   ropl0 [0:7];
  wire `DEF_DATA   ropl1 [0:7];

  wire `DEF_DATA rbl0;
  wire `DEF_DATA rbl1;
  wire `DEF_DATA rbl2;
  wire `DEF_DATA rbl3;
  wire `DEF_DATA rbl4;
  wire `DEF_DATA rbl5;
  wire `DEF_DATA rbl6;
  wire `DEF_DATA rbl7;

  wire `DEF_DATA wbl0;
  wire `DEF_DATA wbl1;
  wire `DEF_DATA wbl2;
  wire `DEF_DATA wbl3;

  assign conn_ren[0] = expand_ren;
  assign conn_rlast[0] = expand_rlast;
  assign conn_rcnt[0] = expand_rcnt;
  assign conn_raddr[0] = expand_raddr;
  assign conn_roplen[0] = expand_roplen;
  assign conn_rblen[0] = expand_rblen;

  assign conn_wen[0] = expand_wen;
  assign conn_wlast[0] = expand_wlast;
  assign conn_wcnt[0] = expand_wcnt;
  assign conn_waddr[0] = expand_waddr;
  assign conn_wsel[0] = expand_wsel;

  assign conn_viu_val[0] = expand_viu;
  assign conn_viu_fn[0] = expand_viu_fn;
  assign conn_viu_utidx[0] = expand_viu_utidx;
  assign conn_viu_imm[0] = expand_viu_imm;

  assign lane_rlast = conn_rlast[8];
  assign lane_wlast = conn_wlast[8];

  genvar i;

  generate
    for (i=0; i<`SZ_BANK; i=i+1)
    begin: banks
      vuVXU_Banked8_Bank bank
      (
        .clk(clk),
        .reset(reset),

        .active(bactive[i]),

        .in_ren(conn_ren[i]),
        .in_rlast(conn_rlast[i]),
        .in_rcnt(conn_rcnt[i]),
        .in_raddr(conn_raddr[i]),
        .in_roplen(conn_roplen[i]),
        .in_rblen(conn_rblen[i]),

        .in_wen(conn_wen[i]),
        .in_wlast(conn_wlast[i]),
        .in_wcnt(conn_wcnt[i]),
        .in_waddr(conn_waddr[i]),
        .in_wsel(conn_wsel[i]),

        .in_viu_val(conn_viu_val[i]),
        .in_viu_fn(conn_viu_fn[i]),
        .in_viu_utidx(conn_viu_utidx[i]),
        .in_viu_imm(conn_viu_imm[i]),

        .out_ren(conn_ren[i+1]),
        .out_rlast(conn_rlast[i+1]),
        .out_rcnt(conn_rcnt[i+1]),
        .out_raddr(conn_raddr[i+1]),
        .out_roplen(conn_roplen[i+1]),
        .out_rblen(conn_rblen[i+1]),

        .out_wen(conn_wen[i+1]),
        .out_wlast(conn_wlast[i+1]),
        .out_wcnt(conn_wcnt[i+1]),
        .out_waddr(conn_waddr[i+1]),
        .out_wsel(conn_wsel[i+1]),

        .out_viu_val(conn_viu_val[i+1]),
        .out_viu_fn(conn_viu_fn[i+1]),
        .out_viu_utidx(conn_viu_utidx[i+1]),
        .out_viu_imm(conn_viu_imm[i+1]),

        .rblen(rblen[i]),
        .rdata(rdata[i]),
        .ropl0(ropl0[i]),
        .ropl1(ropl1[i]),

        .wbl0(wbl0),
        .wbl1(wbl1),
        .wbl2(wbl2),
        .wbl3(wbl3)
      );
    end
  endgenerate

  vuVXU_Banked8_Lane_Xbar xbar
  (
    .rblen(rblen),
    .rdata(rdata),
    .ropl0(ropl0),
    .ropl1(ropl1),

    .rbl0(rbl0),
    .rbl1(rbl1),
    .rbl2(rbl2),
    .rbl3(rbl3),
    .rbl4(rbl4),
    .rbl5(rbl5),
    .rbl6(rbl6),
    .rbl7(rbl7)
  );

  wire              vau0_val;
  wire `DEF_VAU0_FN vau0_fn;
  wire              vau1_val;
  wire `DEF_VAU1_FN vau1_fn;
  wire              vau2_val;
  wire `DEF_VAU2_FN vau2_fn;

  vuVXU_Banked8_Lane_LFU lfu
  (
    .clk(clk),
    .reset(reset),

    .expand_rcnt(expand_rcnt),
    .expand_wcnt(expand_wcnt),

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

    .vau0_val(vau0_val),
    .vau0_fn(vau0_fn),
    .vau1_val(vau1_val),
    .vau1_fn(vau1_fn),
    .vau2_val(vau2_val),
    .vau2_fn(vau2_fn),
    .vldq_rdy(vldq_rdy),
    .vsdq_val(vsdq_val),
    .utaq_val(utaq_val),
    .utldq_rdy(utldq_rdy),
    .utsdq_val(utsdq_val)
  );

  wire `DEF_VAU0_FN imul_fn = vau0_val ? vau0_fn : cp_imul_fn;
  wire `DEF_DATA imul_in0 = vau0_val ? rbl0 : {1'b0, cp_imul_in0};
  wire `DEF_DATA imul_in1 = vau0_val ? rbl1 : {1'b0, cp_imul_in1};
  wire `DEF_DATA imul_out;

  assign cp_imul_rdy = ~vau0_val;
  assign cp_imul_out = imul_out[`SZ_XLEN-1:0];
  assign wbl0 = imul_out;

  vuVXU_Banked8_FU_imul imul
  (
    .clk(clk),
    .reset(reset),
    .val(vau0_val | cp_imul_val),
    .fn(imul_fn),
    .in0(imul_in0),
    .in1(imul_in1),
    .out(imul_out)
  );

  wire `DEF_VAU1_FN fma_fn = vau1_val ? vau1_fn : cp_fma_fn;
  wire `DEF_DATA fma_in0 = vau1_val ? rbl2 : cp_fma_in0;
  wire `DEF_DATA fma_in1 = vau1_val ? rbl3 : cp_fma_in1;
  wire `DEF_DATA fma_in2 = vau1_val ? rbl4 : cp_fma_in2;
  wire `DEF_EXC  fma_exc;
  wire `DEF_DATA fma_out;

  assign cp_fma_rdy = ~vau1_val;
  assign cp_fma_out = fma_out;
  assign cp_fma_exc = fma_exc;
  assign wbl1 = fma_out;

  vuVXU_Banked8_FU_fma fma
  (
    .clk(clk),
    .reset(reset),
    .val(vau1_val | cp_fma_val),
    .fn(fma_fn),
    .in0(fma_in0),
    .in1(fma_in1),
    .in2(fma_in2),
    .out(fma_out),
    .exc(fma_exc)
  );

  vuVXU_Banked8_FU_conv conv
  (
    .clk(clk),
    .reset(reset),
    .val(vau2_val),
    .fn(vau2_fn),
    .in(rbl5),
    .exc(),
    .out(wbl2)
  );

  assign utaq_bits = rbl6[`SZ_ADDR-1:0];
  assign vsdq_bits = rbl7;
  assign utsdq_bits = rbl7;

  assign wbl3 = utldq_rdy ? utldq_bits : vldq_bits;

endmodule

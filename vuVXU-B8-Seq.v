`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Banked8_Seq
(
  input clk,
  input reset,

  input `DEF_BCNT   bcnt,
  input `DEF_VLEN   vlen,
  input `DEF_REGLEN stride,

  input vldq_qstall,
  input vsdq_qstall,
  input utaq_qstall,
  input utldq_qstall,
  input utsdq_qstall,

  input fire_viu,
  input fire_vau0,
  input fire_vau1,
  input fire_vau2,
  input fire_vgslu,
  input fire_vglu,
  input fire_vgsu,
  input fire_vlu,
  input fire_vsu,

  input  `DEF_VIU_FN  fire_fn_viu,
  input  `DEF_VAU0_FN fire_fn_vau0,
  input  `DEF_VAU1_FN fire_fn_vau1,
  input  `DEF_VAU2_FN fire_fn_vau2,

  input               fire_vs_zero,
  input               fire_vt_zero,
  input               fire_vr_zero,
  input  `DEF_REGLEN  fire_vs,
  input  `DEF_REGLEN  fire_vt,
  input  `DEF_REGLEN  fire_vr,
  input  `DEF_REGLEN  fire_vd,
  input  `DEF_DATA    fire_imm,

  output `DEF_STALL seq_stall,
  output            seq_last,

  output seq_viu,
  output seq_vau0,
  output seq_vau1,
  output seq_vau2,
  output seq_vldq,
  output seq_vsdq,
  output seq_utaq,
  output seq_utldq,
  output seq_utsdq,

  output `DEF_VIU_FN  seq_fn_viu,
  output `DEF_VAU0_FN seq_fn_vau0,
  output `DEF_VAU1_FN seq_fn_vau1,
  output `DEF_VAU2_FN seq_fn_vau2,

  output `DEF_BVLEN   seq_cnt,
  output `DEF_VLEN    seq_utidx,
  output              seq_vs_zero,
  output              seq_vt_zero,
  output              seq_vr_zero,
  output `DEF_BREGLEN seq_vs,
  output `DEF_BREGLEN seq_vt,
  output `DEF_BREGLEN seq_vr,
  output `DEF_BREGLEN seq_vd,
  output `DEF_DATA    seq_imm
);

  wire `DEF_BCNT bcntm1 = bcnt - 1'b1;

  wire `DEF_BPTR next_ptr1;
  wire `DEF_BPTR next_ptr2;
  wire `DEF_BPTR next_ptr3;
  reg `DEF_BPTR reg_ptr;

  always @(posedge clk)
  begin
    if (reset)
    begin
      reg_ptr <= `SZ_LGBANK'd0;
    end
    else
    begin
      reg_ptr <= next_ptr1;
    end
  end

  wire `DEF_BPTR1 next_ptr1_add = reg_ptr + `SZ_LGBANK1'd1;
  wire `DEF_BPTR1 next_ptr2_add = reg_ptr + `SZ_LGBANK1'd2;
  wire `DEF_BPTR1 next_ptr3_add = reg_ptr + `SZ_LGBANK1'd3;

  wire `DEF_BPTR1 next_ptr1_add_bcnt = next_ptr1_add - bcnt;
  wire `DEF_BPTR1 next_ptr2_add_bcnt = next_ptr2_add - bcnt;
  wire `DEF_BPTR1 next_ptr3_add_bcnt = next_ptr3_add - bcnt;

  assign next_ptr1
    = next_ptr1_add < bcnt ? next_ptr1_add[`SZ_LGBANK-1:0]
    : next_ptr1_add_bcnt[`SZ_LGBANK-1:0];

  assign next_ptr2
    = next_ptr2_add < bcnt ? next_ptr2_add[`SZ_LGBANK-1:0]
    : next_ptr2_add_bcnt[`SZ_LGBANK-1:0];

  assign next_ptr3
    = next_ptr3_add < bcnt ? next_ptr3_add[`SZ_LGBANK-1:0]
    : next_ptr3_add_bcnt[`SZ_LGBANK-1:0];

  reg `DEF_BANK    next_val;
  reg `DEF_BANK    next_last;
  reg `DEF_BANK    next_viu;
  reg `DEF_BANK    next_vau0;
  reg `DEF_BANK    next_vau1;
  reg `DEF_BANK    next_vau2;
  reg `DEF_BANK    next_vldq;
  reg `DEF_BANK    next_vsdq;
  reg `DEF_BANK    next_utaq;
  reg `DEF_BANK    next_utldq;
  reg `DEF_BANK    next_utsdq;

  reg `DEF_VIU_FN  next_fn_viu [0:7];
  reg `DEF_VAU0_FN next_fn_vau0 [0:7];
  reg `DEF_VAU1_FN next_fn_vau1 [0:7];
  reg `DEF_VAU2_FN next_fn_vau2 [0:7];
  reg `DEF_VLEN    next_vlen [0:7];
  reg `DEF_VLEN    next_utidx [0:7];
  reg `DEF_REGLEN  next_stride [0:7];
  reg `DEF_BANK    next_vs_zero;
  reg `DEF_BANK    next_vt_zero;
  reg `DEF_BANK    next_vr_zero;
  reg `DEF_BANK    next_vd_zero;
  reg `DEF_BREGLEN next_vs [0:7];
  reg `DEF_BREGLEN next_vt [0:7];
  reg `DEF_BREGLEN next_vr [0:7];
  reg `DEF_BREGLEN next_vd [0:7];
  reg `DEF_DATA    next_imm [0:7];

  reg `DEF_BANK    array_val;
  reg `DEF_BANK    array_last;
  reg `DEF_BANK    array_viu;
  reg `DEF_BANK    array_vau0;
  reg `DEF_BANK    array_vau1;
  reg `DEF_BANK    array_vau2;
  reg `DEF_BANK    array_vldq;
  reg `DEF_BANK    array_vsdq;
  reg `DEF_BANK    array_utaq;
  reg `DEF_BANK    array_utldq;
  reg `DEF_BANK    array_utsdq;

  reg `DEF_VIU_FN  array_fn_viu [0:7];
  reg `DEF_VAU0_FN array_fn_vau0 [0:7];
  reg `DEF_VAU1_FN array_fn_vau1 [0:7];
  reg `DEF_VAU2_FN array_fn_vau2 [0:7];
  reg `DEF_VLEN    array_vlen [0:7];
  reg `DEF_VLEN    array_utidx [0:7];
  reg `DEF_REGLEN  array_stride [0:7];
  reg `DEF_BANK    array_vs_zero;
  reg `DEF_BANK    array_vt_zero;
  reg `DEF_BANK    array_vr_zero;
  reg `DEF_BANK    array_vd_zero;
  reg `DEF_BREGLEN array_vs [0:7];
  reg `DEF_BREGLEN array_vt [0:7];
  reg `DEF_BREGLEN array_vr [0:7];
  reg `DEF_BREGLEN array_vd [0:7];
  reg `DEF_DATA    array_imm [0:7];

  integer i;

  always @(posedge clk)
  begin
    if (reset)
    begin
      array_val <= `SZ_BANK'd0;
      array_last <= `SZ_BANK'd0;
      array_viu <= `SZ_BANK'd0;
      array_vau0 <= `SZ_BANK'd0;
      array_vau1 <= `SZ_BANK'd0;
      array_vau2 <= `SZ_BANK'd0;
      array_vldq <= `SZ_BANK'd0;
      array_vsdq <= `SZ_BANK'd0;
      array_utaq <= `SZ_BANK'd0;
      array_utldq <= `SZ_BANK'd0;
      array_utsdq <= `SZ_BANK'd0;
    end
    else
    begin
      array_val <= next_val;
      array_last <= next_last;
      array_viu <= next_viu;
      array_vau0 <= next_vau0;
      array_vau1 <= next_vau1;
      array_vau2 <= next_vau2;
      array_vldq <= next_vldq;
      array_vsdq <= next_vsdq;
      array_utaq <= next_utaq;
      array_utldq <= next_utldq;
      array_utsdq <= next_utsdq;
    end

    for (i=0; i<`SZ_BANK; i=i+1)
    begin
      array_fn_viu[i] <= next_fn_viu[i];
      array_fn_vau0[i] <= next_fn_vau0[i];
      array_fn_vau1[i] <= next_fn_vau1[i];
      array_fn_vau2[i] <= next_fn_vau2[i];
      array_vlen[i] <= next_vlen[i];
      array_utidx[i] <= next_utidx[i];
      array_stride[i] <= next_stride[i];
      array_vs_zero[i] <= next_vs_zero[i];
      array_vt_zero[i] <= next_vt_zero[i];
      array_vr_zero[i] <= next_vr_zero[i];
      array_vd_zero[i] <= next_vd_zero[i];
      array_vs[i] <= next_vs[i];
      array_vt[i] <= next_vt[i];
      array_vr[i] <= next_vr[i];
      array_vd[i] <= next_vd[i];
      array_imm[i] <= next_imm[i];
    end
  end

  wire last = vlen < bcnt;

  always @(*)
  begin
    next_val = array_val;
    next_last = array_last;
    next_viu = array_viu;
    next_vau0 = array_vau0;
    next_vau1 = array_vau1;
    next_vau2 = array_vau2;
    next_vldq = array_vldq;
    next_vsdq = array_vsdq;
    next_utaq = array_utaq;
    next_utldq = array_utldq;
    next_utsdq = array_utsdq;

    for (i=0; i<`SZ_BANK; i=i+1)
    begin
      next_fn_viu[i] = array_fn_viu[i];
      next_fn_vau0[i] = array_fn_vau0[i];
      next_fn_vau1[i] = array_fn_vau1[i];
      next_fn_vau2[i] = array_fn_vau2[i];
      next_vlen[i] = array_vlen[i];
      next_utidx[i] = array_utidx[i];
      next_stride[i] = array_stride[i];
      next_vs_zero[i] = array_vs_zero[i];
      next_vt_zero[i] = array_vt_zero[i];
      next_vr_zero[i] = array_vr_zero[i];
      next_vd_zero[i] = array_vd_zero[i];
      next_vs[i] = array_vs[i];
      next_vt[i] = array_vt[i];
      next_vr[i] = array_vr[i];
      next_vd[i] = array_vd[i];
      next_imm[i] = array_imm[i];
    end

    if (fire_viu)
    begin
      next_val[next_ptr1] = 1'b1;
      next_last[next_ptr1] = last;
      next_viu[next_ptr1] = 1'b1;
      next_fn_viu[next_ptr1] = fire_fn_viu;
      next_vlen[next_ptr1] = vlen;
      next_utidx[next_ptr1] = `SZ_VLEN'd0;
      next_stride[next_ptr1] = stride;
      next_vs_zero[next_ptr1] = fire_vs_zero;
      next_vt_zero[next_ptr1] = fire_vt_zero;
      next_vs[next_ptr1] = {2'd0,fire_vs};
      next_vt[next_ptr1] = {2'd0,fire_vt};
      next_vd[next_ptr1] = {2'd0,fire_vd};
      next_imm[next_ptr1] = fire_imm;
    end

    else if (fire_vau0)
    begin
      next_val[next_ptr1] = 1'b1;
      next_last[next_ptr1] = last;
      next_vau0[next_ptr1] = 1'b1;
      next_fn_vau0[next_ptr1] = fire_fn_vau0;
      next_vlen[next_ptr1] = vlen;
      next_stride[next_ptr1] = stride;
      next_vs_zero[next_ptr1] = fire_vs_zero;
      next_vt_zero[next_ptr1] = fire_vt_zero;
      next_vs[next_ptr1] = {2'd0,fire_vs};
      next_vt[next_ptr1] = {2'd0,fire_vt};
      next_vd[next_ptr1] = {2'd0,fire_vd};
    end

    else if (fire_vau1)
    begin
      next_val[next_ptr1] = 1'b1;
      next_last[next_ptr1] = last;
      next_vau1[next_ptr1] = 1'b1;
      next_fn_vau1[next_ptr1] = fire_fn_vau1;
      next_vlen[next_ptr1] = vlen;
      next_stride[next_ptr1] = stride;
      next_vs_zero[next_ptr1] = fire_vs_zero;
      next_vt_zero[next_ptr1] = fire_vt_zero;
      next_vr_zero[next_ptr1] = fire_vr_zero;
      next_vs[next_ptr1] = {2'd0,fire_vs};
      next_vt[next_ptr1] = {2'd0,fire_vt};
      next_vr[next_ptr1] = {2'd0,fire_vr};
      next_vd[next_ptr1] = {2'd0,fire_vd};
    end

    else if (fire_vau2)
    begin
      next_val[next_ptr1] = 1'b1;
      next_last[next_ptr1] = last;
      next_vau2[next_ptr1] = 1'b1;
      next_fn_vau2[next_ptr1] = fire_fn_vau2;
      next_vlen[next_ptr1] = vlen;
      next_stride[next_ptr1] = stride;
      next_vs_zero[next_ptr1] = fire_vs_zero;
      next_vs[next_ptr1] = {2'd0,fire_vs};
      next_vd[next_ptr1] = {2'd0,fire_vd};
    end

    else if (fire_vgslu)
    begin
      next_val[next_ptr1] = 1'b1;
      next_last[next_ptr1] = last;
      next_utaq[next_ptr1] = 1'b1;
      next_vlen[next_ptr1] = vlen;
      next_stride[next_ptr1] = stride;
      next_vs_zero[next_ptr1] = fire_vs_zero;
      next_vs[next_ptr1] = {2'd0,fire_vs};

      next_val[next_ptr2] = 1'b1;
      next_last[next_ptr2] = last;
      next_utsdq[next_ptr2] = 1'b1;
      next_vlen[next_ptr2] = vlen;
      next_stride[next_ptr2] = stride;
      next_vt_zero[next_ptr2] = fire_vt_zero;
      next_vt[next_ptr2] = {2'd0,fire_vt};

      next_val[next_ptr3] = 1'b1;
      next_last[next_ptr3] = last;
      next_utldq[next_ptr3] = 1'b1;
      next_vlen[next_ptr3] = vlen;
      next_stride[next_ptr3] = stride;
      next_vd[next_ptr3] = {2'd0,fire_vd};
    end

    else if (fire_vglu)
    begin
      next_val[next_ptr1] = 1'b1;
      next_last[next_ptr1] = last;
      next_utaq[next_ptr1] = 1'b1;
      next_vlen[next_ptr1] = vlen;
      next_stride[next_ptr1] = stride;
      next_vs_zero[next_ptr1] = fire_vs_zero;
      next_vs[next_ptr1] = {2'd0,fire_vs};

      next_val[next_ptr2] = 1'b1;
      next_last[next_ptr2] = last;
      next_utldq[next_ptr2] = 1'b1;
      next_vlen[next_ptr2] = vlen;
      next_stride[next_ptr2] = stride;
      next_vd[next_ptr2] = {2'd0,fire_vd};
    end

    else if (fire_vgsu)
    begin
      next_val[next_ptr1] = 1'b1;
      next_last[next_ptr1] = last;
      next_utaq[next_ptr1] = 1'b1;
      next_vlen[next_ptr1] = vlen;
      next_stride[next_ptr1] = stride;
      next_vs_zero[next_ptr1] = fire_vs_zero;
      next_vs[next_ptr1] = {2'd0,fire_vs};

      next_val[next_ptr2] = 1'b1;
      next_last[next_ptr2] = last;
      next_utsdq[next_ptr2] = 1'b1;
      next_vlen[next_ptr2] = vlen;
      next_stride[next_ptr2] = stride;
      next_vt_zero[next_ptr2] = fire_vt_zero;
      next_vt[next_ptr2] = {2'd0,fire_vt};
    end

    else if (fire_vlu)
    begin
      next_val[next_ptr1] = 1'b1;
      next_last[next_ptr1] = last;
      next_vldq[next_ptr1] = 1'b1;
      next_vlen[next_ptr1] = vlen;
      next_stride[next_ptr1] = stride;
      next_vd[next_ptr1] = {2'd0,fire_vd};
    end

    else if (fire_vsu)
    begin
      next_val[next_ptr1] = 1'b1;
      next_last[next_ptr1] = last;
      next_vsdq[next_ptr1] = 1'b1;
      next_vlen[next_ptr1] = vlen;
      next_stride[next_ptr1] = stride;
      next_vt_zero[next_ptr1] = fire_vt_zero;
      next_vt[next_ptr1] = {2'd0,fire_vt};
    end

    if (seq_viu || seq_vau0 || seq_vau1 || seq_vau2 || seq_vldq || seq_vsdq || seq_utaq || seq_utldq || seq_utsdq)
    begin
      next_vlen[reg_ptr] = array_vlen[reg_ptr] - seq_cnt - 1'b1;
      next_utidx[reg_ptr] = array_utidx[reg_ptr] + bcnt;
      next_vs[reg_ptr] = array_vs[reg_ptr] + array_stride[reg_ptr];
      next_vt[reg_ptr] = array_vt[reg_ptr] + array_stride[reg_ptr];
      next_vr[reg_ptr] = array_vr[reg_ptr] + array_stride[reg_ptr];
      next_vd[reg_ptr] = array_vd[reg_ptr] + array_stride[reg_ptr];

      if (array_last[reg_ptr])
      begin
        next_val[reg_ptr] = 1'b0;
        next_last[reg_ptr] = 1'b0;
        next_viu[reg_ptr] = 1'b0;
        next_vau0[reg_ptr] = 1'b0;
        next_vau1[reg_ptr] = 1'b0;
        next_vau2[reg_ptr] = 1'b0;
        next_vldq[reg_ptr] = 1'b0;
        next_vsdq[reg_ptr] = 1'b0;
        next_utaq[reg_ptr] = 1'b0;
        next_utldq[reg_ptr] = 1'b0;
        next_utsdq[reg_ptr] = 1'b0;
      end
      else
      if (next_vlen[reg_ptr] < bcnt)
      begin
        next_last[reg_ptr] = 1'b1;
      end
    end
  end

  reg `DEF_BANK next_dep_vldq;
  reg `DEF_BANK next_dep_vsdq;
  reg `DEF_BANK next_dep_utaq;
  reg `DEF_BANK next_dep_utldq;
  reg `DEF_BANK next_dep_utsdq;

  reg `DEF_BANK array_dep_vldq;
  reg `DEF_BANK array_dep_vsdq;
  reg `DEF_BANK array_dep_utaq;
  reg `DEF_BANK array_dep_utldq;
  reg `DEF_BANK array_dep_utsdq;

  always @(posedge clk)
  begin
    if (reset)
    begin
      array_dep_vldq <= `SZ_BANK'd0;
      array_dep_vsdq <= `SZ_BANK'd0;
      array_dep_utaq <= `SZ_BANK'd0;
      array_dep_utldq <= `SZ_BANK'd0;
      array_dep_utsdq <= `SZ_BANK'd0;
    end
    else
    begin
      array_dep_vldq <= next_dep_vldq;
      array_dep_vsdq <= next_dep_vsdq;
      array_dep_utaq <= next_dep_utaq;
      array_dep_utldq <= next_dep_utldq;
      array_dep_utsdq <= next_dep_utsdq;
    end
  end

  always @(*)
  begin
    next_dep_vldq = array_dep_vldq;
    next_dep_vsdq = array_dep_vsdq;
    next_dep_utaq = array_dep_utaq;
    next_dep_utldq = array_dep_utldq;
    next_dep_utsdq = array_dep_utsdq;

    if (fire_viu || fire_vau0 || fire_vau1 || fire_vau2)
    begin
      next_dep_vldq[next_ptr1] = 1'b1;
      next_dep_vsdq[next_ptr1] = 1'b1;
      next_dep_utaq[next_ptr1] = 1'b1;
      next_dep_utldq[next_ptr1] = 1'b1;
      next_dep_utsdq[next_ptr1] = 1'b1;
    end

    else if (fire_vgslu)
    begin
      next_dep_utaq = `SZ_BANK'd0;
      next_dep_vldq[next_ptr1] = 1'b1;
      next_dep_vsdq[next_ptr1] = 1'b1;
      next_dep_utaq[next_ptr1] = 1'b0;
      next_dep_utldq[next_ptr1] = 1'b1;
      next_dep_utsdq[next_ptr1] = 1'b1;

      next_dep_utsdq = `SZ_BANK'd0;
      next_dep_vldq[next_ptr2] = 1'b1;
      next_dep_vsdq[next_ptr2] = 1'b1;
      next_dep_utaq[next_ptr2] = 1'b1;
      next_dep_utldq[next_ptr2] = 1'b1;
      next_dep_utsdq[next_ptr2] = 1'b0;

      next_dep_utldq = `SZ_BANK'd0;
      next_dep_vldq[next_ptr3] = 1'b1;
      next_dep_vsdq[next_ptr3] = 1'b1;
      next_dep_utaq[next_ptr3] = 1'b1;
      next_dep_utldq[next_ptr3] = 1'b0;
      next_dep_utsdq[next_ptr3] = 1'b1;
    end

    else if (fire_vglu)
    begin
      next_dep_utaq = `SZ_BANK'd0;
      next_dep_vldq[next_ptr1] = 1'b1;
      next_dep_vsdq[next_ptr1] = 1'b1;
      next_dep_utaq[next_ptr1] = 1'b0;
      next_dep_utldq[next_ptr1] = 1'b1;
      next_dep_utsdq[next_ptr1] = 1'b1;

      next_dep_utldq = `SZ_BANK'd0;
      next_dep_vldq[next_ptr2] = 1'b1;
      next_dep_vsdq[next_ptr2] = 1'b1;
      next_dep_utaq[next_ptr2] = 1'b1;
      next_dep_utldq[next_ptr2] = 1'b0;
      next_dep_utsdq[next_ptr2] = 1'b1;
    end

    else if (fire_vgsu)
    begin
      next_dep_utaq = `SZ_BANK'd0;
      next_dep_vldq[next_ptr1] = 1'b1;
      next_dep_vsdq[next_ptr1] = 1'b1;
      next_dep_utaq[next_ptr1] = 1'b0;
      next_dep_utldq[next_ptr1] = 1'b1;
      next_dep_utsdq[next_ptr1] = 1'b1;

      next_dep_utsdq = `SZ_BANK'd0;
      next_dep_vldq[next_ptr2] = 1'b1;
      next_dep_vsdq[next_ptr2] = 1'b1;
      next_dep_utaq[next_ptr2] = 1'b1;
      next_dep_utldq[next_ptr2] = 1'b1;
      next_dep_utsdq[next_ptr2] = 1'b0;
    end

    else if (fire_vlu)
    begin
      next_dep_vldq = `SZ_BANK'd0;
      next_dep_vldq[next_ptr1] = 1'b0;
      next_dep_vsdq[next_ptr1] = 1'b1;
      next_dep_utaq[next_ptr1] = 1'b1;
      next_dep_utldq[next_ptr1] = 1'b1;
      next_dep_utsdq[next_ptr1] = 1'b1;
    end

    else if (fire_vsu)
    begin
      next_dep_vsdq = `SZ_BANK'd0;
      next_dep_vldq[next_ptr1] = 1'b1;
      next_dep_vsdq[next_ptr1] = 1'b0;
      next_dep_utaq[next_ptr1] = 1'b1;
      next_dep_utldq[next_ptr1] = 1'b1;
      next_dep_utsdq[next_ptr1] = 1'b1;
    end
  end

  wire current_val = array_val[reg_ptr];
  wire current_vldq_val = current_val & array_vldq[reg_ptr];
  wire current_vsdq_val = current_val & array_vsdq[reg_ptr];
  wire current_utaq_val = current_val & array_utaq[reg_ptr];
  wire current_utldq_val = current_val & array_utldq[reg_ptr];
  wire current_utsdq_val = current_val & array_utsdq[reg_ptr];

  reg reg_vldq_stall;
  reg reg_vsdq_stall;
  reg reg_utaq_stall;
  reg reg_utldq_stall;
  reg reg_utsdq_stall;

  always @(posedge clk)
  begin
    if (reset)
    begin
      reg_vldq_stall <= 1'b0;
      reg_vsdq_stall <= 1'b0;
      reg_utaq_stall <= 1'b0;
      reg_utldq_stall <= 1'b0;
      reg_utsdq_stall <= 1'b0;
    end
    else
    begin
      if (current_vldq_val) reg_vldq_stall <= vldq_qstall;
      if (current_vsdq_val) reg_vsdq_stall <= vsdq_qstall;
      if (current_utaq_val) reg_utaq_stall <= utaq_qstall;
      if (current_utldq_val) reg_utldq_stall <= utldq_qstall;
      if (current_utsdq_val) reg_utsdq_stall <= utsdq_qstall;
    end
  end

  wire stall
    = array_dep_vldq[reg_ptr] & reg_vldq_stall
    | array_dep_vsdq[reg_ptr] & reg_vsdq_stall
    | array_dep_utaq[reg_ptr] & reg_utaq_stall
    | array_dep_utldq[reg_ptr] & reg_utldq_stall
    | array_dep_utsdq[reg_ptr] & reg_utsdq_stall
    | current_vldq_val & vldq_qstall
    | current_vsdq_val & vsdq_qstall
    | current_utaq_val & utaq_qstall
    | current_utldq_val & utldq_qstall
    | current_utsdq_val & utsdq_qstall;

  assign seq_stall = {reg_vldq_stall,reg_vsdq_stall,reg_utaq_stall,reg_utldq_stall,reg_utsdq_stall};

  assign seq_last = ~stall & current_val & array_last[reg_ptr];

  assign seq_viu = ~stall & current_val & array_viu[reg_ptr];
  assign seq_vau0 = ~stall & current_val & array_vau0[reg_ptr];
  assign seq_vau1 = ~stall & current_val & array_vau1[reg_ptr];
  assign seq_vau2 = ~stall & current_val & array_vau2[reg_ptr];
  assign seq_vldq = ~stall & current_vldq_val;
  assign seq_vsdq = ~stall & current_vsdq_val;
  assign seq_utaq = ~stall & current_utaq_val;
  assign seq_utldq = ~stall & current_utldq_val;
  assign seq_utsdq = ~stall & current_utsdq_val;

  assign seq_fn_viu = array_fn_viu[reg_ptr];
  assign seq_fn_vau0 = array_fn_vau0[reg_ptr];
  assign seq_fn_vau1 = array_fn_vau1[reg_ptr];
  assign seq_fn_vau2 = array_fn_vau2[reg_ptr];

  assign seq_cnt = array_vlen[reg_ptr] < bcntm1 ? array_vlen[reg_ptr][`SZ_LGBANK-1:0] : bcntm1[`SZ_LGBANK-1:0];
  assign seq_utidx = array_utidx[reg_ptr];
  assign seq_vs_zero = array_vs_zero[reg_ptr];
  assign seq_vt_zero = array_vt_zero[reg_ptr];
  assign seq_vr_zero = array_vr_zero[reg_ptr];
  assign seq_vs = array_vs[reg_ptr];
  assign seq_vt = array_vt[reg_ptr];
  assign seq_vr = array_vr[reg_ptr];
  assign seq_vd = array_vd[reg_ptr];
  assign seq_imm = array_imm[reg_ptr];

endmodule

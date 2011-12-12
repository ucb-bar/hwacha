`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Banked8_Expand
(
  input clk,
  input reset,

  input seq_last,

  input seq_viu,
  input seq_vau0,
  input seq_vau1,
  input seq_vau2,
  input seq_vldq,
  input seq_vsdq,
  input seq_utaq,
  input seq_utldq,
  input seq_utsdq,

  input  `DEF_VIU_FN  seq_fn_viu,
  input  `DEF_VAU0_FN seq_fn_vau0,
  input  `DEF_VAU1_FN seq_fn_vau1,
  input  `DEF_VAU2_FN seq_fn_vau2,

  input  `DEF_BVLEN   seq_cnt,
  input  `DEF_VLEN    seq_utidx,
  input               seq_vs_zero,
  input               seq_vt_zero,
  input               seq_vr_zero,
  input  `DEF_BREGLEN seq_vs,
  input  `DEF_BREGLEN seq_vt,
  input  `DEF_BREGLEN seq_vr,
  input  `DEF_BREGLEN seq_vd,
  input  `DEF_DATA    seq_imm,

  output              expand_ren,
  output              expand_rlast,
  output `DEF_BVLEN   expand_rcnt,
  output `DEF_BREGLEN expand_raddr,
  output `DEF_BOPL    expand_roplen,
  output `DEF_BRPORT  expand_rblen,

  output              expand_wen,
  output              expand_wlast,
  output `DEF_BVLEN   expand_wcnt,
  output `DEF_BREGLEN expand_waddr,
  output `DEF_BWPORT  expand_wsel,

  output              expand_viu,
  output `DEF_VIU_FN  expand_viu_fn,
  output `DEF_VLEN    expand_viu_utidx,
  output `DEF_DATA    expand_viu_imm,
  output              expand_vau0,
  output `DEF_VAU0_FN expand_vau0_fn,
  output              expand_vau1,
  output `DEF_VAU1_FN expand_vau1_fn,
  output              expand_vau2,
  output `DEF_VAU2_FN expand_vau2_fn,
  output              expand_vldq,
  output              expand_vsdq,
  output              expand_utaq,
  output              expand_utldq,
  output              expand_utsdq
);

  integer i;

  reg              next_ren [0:`SHIFT_BUF_READ-1];
  reg              next_rlast [0:`SHIFT_BUF_READ-1];
  reg `DEF_BVLEN   next_rcnt [0:`SHIFT_BUF_READ-1];
  reg `DEF_BREGLEN next_raddr [0:`SHIFT_BUF_READ-1];
  reg `DEF_BOPL    next_roplen [0:`SHIFT_BUF_READ-1];
  reg `DEF_BRPORT  next_rblen [0:`SHIFT_BUF_READ-1];

  reg              reg_ren [0:`SHIFT_BUF_READ-1];
  reg              reg_rlast [0:`SHIFT_BUF_READ-1];
  reg `DEF_BVLEN   reg_rcnt [0:`SHIFT_BUF_READ-1];
  reg `DEF_BREGLEN reg_raddr [0:`SHIFT_BUF_READ-1];
  reg `DEF_BOPL    reg_roplen [0:`SHIFT_BUF_READ-1];
  reg `DEF_BRPORT  reg_rblen [0:`SHIFT_BUF_READ-1];

  always @(posedge clk)
  begin
    if (reset)
    begin
      for (i=0; i<`SHIFT_BUF_READ; i=i+1)
      begin
        reg_ren[i] <= 1'b0;
      end
    end
    else
    begin
      for (i=0; i<`SHIFT_BUF_READ; i=i+1)
      begin
        reg_ren[i] <= next_ren[i];
        reg_rlast[i] <= next_rlast[i];
        reg_rcnt[i] <= next_rcnt[i];
        reg_raddr[i] <= next_raddr[i];
        reg_roplen[i] <= next_roplen[i];
        reg_rblen[i] <= next_rblen[i];
      end
    end
  end

  always @(*)
  begin
    for (i=0; i<`SHIFT_BUF_READ-1; i=i+1)
    begin
      next_ren[i] = reg_ren[i+1];
      next_rlast[i] = reg_rlast[i+1];
      next_rcnt[i] = reg_rcnt[i+1];
      next_raddr[i] = reg_raddr[i+1];
      next_roplen[i] = reg_roplen[i+1];
      next_rblen[i] = reg_rblen[i+1];
    end

    next_ren[`SHIFT_BUF_READ-1] = 1'b0;
    next_rlast[`SHIFT_BUF_READ-1] = 1'b0;
    next_rcnt[`SHIFT_BUF_READ-1] = 3'd0;
    next_raddr[`SHIFT_BUF_READ-1] = 8'd0;
    next_roplen[`SHIFT_BUF_READ-1] = 2'd0;
    next_rblen[`SHIFT_BUF_READ-1] = 8'd0;

    if (seq_viu)
    begin
      next_ren[0] = 1'b1;
      next_rlast[0] = seq_last;
      next_rcnt[0] = seq_cnt;
      next_raddr[0] = seq_vs;
      next_roplen[0] = 2'b01;
      next_rblen[0] = 8'b00_0_000_00;

      if (seq_fn_viu[`RG_VIU_T] == {`ML,`MR})
      begin
        next_ren[1] = 1'b1;
        next_rlast[1] = seq_last;
        next_rcnt[1] = seq_cnt;
        next_raddr[1] = seq_vt;
        next_roplen[1] = 2'b00;
        next_rblen[1] = 8'b00_0_000_00;
      end
      if (seq_fn_viu[`RG_VIU_T] == {`M0,`MR})
      begin
        next_raddr[0] = seq_vt;
      end
    end
    else if (seq_vau0)
    begin
      next_ren[0] = 1'b1;
      next_rlast[0] = seq_last;
      next_rcnt[0] = seq_cnt;
      next_raddr[0] = seq_vs;
      next_roplen[0] = 2'b01;
      next_rblen[0] = 8'b00_0_000_00;

      next_ren[1] = 1'b1;
      next_rlast[1] = seq_last;
      next_rcnt[1] = seq_cnt;
      next_raddr[1] = seq_vt;
      next_roplen[1] = 2'b00;
      next_rblen[1] = 8'b00_0_000_11;

      if (seq_vs_zero) next_rblen[1][0] = 1'b0;
      if (seq_vt_zero) next_rblen[1][1] = 1'b0;
    end
    else if (seq_vau1)
    begin
      if (seq_fn_vau1[2])
      begin
        next_ren[0] = 1'b1;
        next_rlast[0] = seq_last;
        next_rcnt[0] = seq_cnt;
        next_raddr[0] = seq_vs;
        next_roplen[0] = 2'b10;
        next_rblen[0] = 8'b00_0_000_00;

        next_ren[1] = 1'b1;
        next_rlast[1] = seq_last;
        next_rcnt[1] = seq_cnt;
        next_raddr[1] = seq_vt;
        next_roplen[1] = 2'b01;
        next_rblen[1] = 8'b00_0_000_00;

        next_ren[2] = 1'b1;
        next_rlast[2] = seq_last;
        next_rcnt[2] = seq_cnt;
        next_raddr[2] = seq_vr;
        next_roplen[2] = 2'b00;
        next_rblen[2] = 8'b00_0_111_00;

        if (seq_vs_zero) next_rblen[2][2] = 1'b0;
        if (seq_vt_zero) next_rblen[2][3] = 1'b0;
        if (seq_vr_zero) next_rblen[2][4] = 1'b0;
      end
      else
      begin
        next_ren[0] = 1'b1;
        next_rlast[0] = seq_last;
        next_rcnt[0] = seq_cnt;
        next_raddr[0] = seq_vs;
        next_roplen[0] = 2'b10;
        next_rblen[0] = 8'b00_0_000_00;

        next_ren[1] = 1'b1;
        next_rlast[1] = seq_last;
        next_rcnt[1] = seq_cnt;
        next_raddr[1] = seq_vt;
        next_roplen[1] = 2'b00;
        next_rblen[1] = 8'b00_0_101_00;

        if (seq_vs_zero) next_rblen[2][2] = 1'b0;
        if (seq_vt_zero) next_rblen[2][4] = 1'b0;
      end
    end
    else if (seq_vau2)
    begin
      next_ren[0] = 1'b1;
      next_rlast[0] = seq_last;
      next_rcnt[0] = seq_cnt;
      next_raddr[0] = seq_vs;
      next_roplen[0] = 2'b00;
      next_rblen[0] = 8'b00_1_000_00;

      if (seq_vs_zero) next_rblen[0][5] = 1'b0;
    end
    else if (seq_utaq)
    begin
      next_ren[0] = 1'b1;
      next_rlast[0] = seq_last;
      next_rcnt[0] = seq_cnt;
      next_raddr[0] = seq_vs;
      next_roplen[0] = 2'b00;
      next_rblen[0] = 8'b01_0_000_00;

      if (seq_vs_zero) next_rblen[0][6] = 1'b0;
    end
    else if (seq_vsdq || seq_utsdq)
    begin
      next_ren[0] = 1'b1;
      next_rlast[0] = seq_last;
      next_rcnt[0] = seq_cnt;
      next_raddr[0] = seq_vt;
      next_roplen[0] = 2'b00;
      next_rblen[0] = 8'b10_0_000_00;

      if (seq_vt_zero) next_rblen[0][7] = 1'b0;
    end
  end

  reg              next_wen [0:`SHIFT_BUF_WRITE-1];
  reg              next_wlast [0:`SHIFT_BUF_WRITE-1];
  reg `DEF_BVLEN   next_wcnt [0:`SHIFT_BUF_WRITE-1];
  reg `DEF_BREGLEN next_waddr [0:`SHIFT_BUF_WRITE-1];
  reg `DEF_BWPORT  next_wsel [0:`SHIFT_BUF_WRITE-1];

  reg              reg_wen [0:`SHIFT_BUF_WRITE-1];
  reg              reg_wlast [0:`SHIFT_BUF_WRITE-1];
  reg `DEF_BVLEN   reg_wcnt [0:`SHIFT_BUF_WRITE-1];
  reg `DEF_BREGLEN reg_waddr [0:`SHIFT_BUF_WRITE-1];
  reg `DEF_BWPORT  reg_wsel [0:`SHIFT_BUF_WRITE-1];

  always @(posedge clk)
  begin
    if (reset)
    begin
      for (i=0; i<`SHIFT_BUF_WRITE; i=i+1)
      begin
        reg_wen[i] <= 1'b0;
      end
    end
    else
    begin
      for (i=0; i<`SHIFT_BUF_WRITE; i=i+1)
      begin
        reg_wen[i] <= next_wen[i];
        reg_wlast[i] <= next_wlast[i];
        reg_wcnt[i] <= next_wcnt[i];
        reg_waddr[i] <= next_waddr[i];
        reg_wsel[i] <= next_wsel[i];
      end
    end
  end

  wire `DEF_BPTR1 viu_wptr
    = (seq_fn_viu[`RG_VIU_T] == {`ML,`MR}) ? `INT_STAGES + `SZ_LGBANK'd2
    : `INT_STAGES + `SZ_LGBANK'd1;

  wire `DEF_BPTR1 vau0_wptr
    = `IMUL_STAGES + `SZ_LGBANK'd2;

  wire `DEF_BPTR1 vau1_wptr
    = (seq_fn_vau1[2]) ? `FMA_STAGES + `SZ_LGBANK'd3
    : `FMA_STAGES + `SZ_LGBANK'd2;

  wire `DEF_BPTR1 vau2_wptr
    = `FCONV_STAGES + `SZ_LGBANK'd1;

  always @(*)
  begin
    for (i=0; i<`SHIFT_BUF_WRITE-1; i=i+1)
    begin
      next_wen[i] = reg_wen[i+1];
      next_wlast[i] = reg_wlast[i+1];
      next_wcnt[i] = reg_wcnt[i+1];
      next_waddr[i] = reg_waddr[i+1];
      next_wsel[i] = reg_wsel[i+1];
    end

    next_wen[`SHIFT_BUF_WRITE-1] = 1'b0;
    next_wlast[`SHIFT_BUF_WRITE-1] = 1'b0;
    next_wcnt[`SHIFT_BUF_WRITE-1] = 3'd0;
    next_waddr[`SHIFT_BUF_WRITE-1] = 8'd0;
    next_wsel[`SHIFT_BUF_WRITE-1] = 3'd0;

    if (seq_viu)
    begin
      next_wen[viu_wptr] = 1'b1;
      next_wlast[viu_wptr] = seq_last;
      next_wcnt[viu_wptr] = seq_cnt;
      next_waddr[viu_wptr] = seq_vd;
      next_wsel[viu_wptr] = 3'd4;
    end
    else if (seq_vau0)
    begin
      next_wen[vau0_wptr] = 1'b1;
      next_wlast[vau0_wptr] = seq_last;
      next_wcnt[vau0_wptr] = seq_cnt;
      next_waddr[vau0_wptr] = seq_vd;
      next_wsel[vau0_wptr] = 3'd0;
    end
    else if (seq_vau1)
    begin
      next_wen[vau1_wptr] = 1'b1;
      next_wlast[vau1_wptr] = seq_last;
      next_wcnt[vau1_wptr] = seq_cnt;
      next_waddr[vau1_wptr] = seq_vd;
      next_wsel[vau1_wptr] = 3'd1;
    end
    else if (seq_vau2)
    begin
      next_wen[vau2_wptr] = 1'b1;
      next_wlast[vau2_wptr] = seq_last;
      next_wcnt[vau2_wptr] = seq_cnt;
      next_waddr[vau2_wptr] = seq_vd;
      next_wsel[vau2_wptr] = 3'd2;
    end
    else if (seq_vldq || seq_utldq)
    begin
      next_wen[0] = 1'b1;
      next_wlast[0] = seq_last;
      next_wcnt[0] = seq_cnt;
      next_waddr[0] = seq_vd;
      next_wsel[0] = 3'd3;
    end
  end

  reg              next_viu [0:`SHIFT_BUF_READ-1];
  reg `DEF_VIU_FN  next_viu_fn [0:`SHIFT_BUF_READ-1];
  reg `DEF_VLEN    next_viu_utidx [0:`SHIFT_BUF_READ-1];
  reg `DEF_DATA    next_viu_imm [0:`SHIFT_BUF_READ-1];
  reg              next_vau0 [0:`SHIFT_BUF_READ-1];
  reg `DEF_VAU0_FN next_vau0_fn [0:`SHIFT_BUF_READ-1];
  reg              next_vau1 [0:`SHIFT_BUF_READ-1];
  reg `DEF_VAU1_FN next_vau1_fn [0:`SHIFT_BUF_READ-1];
  reg              next_vau2 [0:`SHIFT_BUF_READ-1];
  reg `DEF_VAU2_FN next_vau2_fn [0:`SHIFT_BUF_READ-1];
  reg              next_vldq [0:`SHIFT_BUF_READ-1];
  reg              next_vsdq [0:`SHIFT_BUF_READ-1];
  reg              next_utaq [0:`SHIFT_BUF_READ-1];
  reg              next_utldq [0:`SHIFT_BUF_READ-1];
  reg              next_utsdq [0:`SHIFT_BUF_READ-1];

  reg              reg_viu [0:`SHIFT_BUF_READ-1];
  reg `DEF_VIU_FN  reg_viu_fn [0:`SHIFT_BUF_READ-1];
  reg `DEF_VLEN    reg_viu_utidx [0:`SHIFT_BUF_READ-1];
  reg `DEF_DATA    reg_viu_imm [0:`SHIFT_BUF_READ-1];
  reg              reg_vau0 [0:`SHIFT_BUF_READ-1];
  reg `DEF_VAU0_FN reg_vau0_fn [0:`SHIFT_BUF_READ-1];
  reg              reg_vau1 [0:`SHIFT_BUF_READ-1];
  reg `DEF_VAU1_FN reg_vau1_fn [0:`SHIFT_BUF_READ-1];
  reg              reg_vau2 [0:`SHIFT_BUF_READ-1];
  reg `DEF_VAU2_FN reg_vau2_fn [0:`SHIFT_BUF_READ-1];
  reg              reg_vldq [0:`SHIFT_BUF_READ-1];
  reg              reg_vsdq [0:`SHIFT_BUF_READ-1];
  reg              reg_utaq [0:`SHIFT_BUF_READ-1];
  reg              reg_utldq [0:`SHIFT_BUF_READ-1];
  reg              reg_utsdq [0:`SHIFT_BUF_READ-1];

  always @(posedge clk)
  begin
    if (reset)
    begin
      for (i=0; i<`SHIFT_BUF_READ; i=i+1)
      begin
        reg_viu[i] <= 1'b0;
        reg_vau0[i] <= 1'b0;
        reg_vau1[i] <= 1'b0;
        reg_vau2[i] <= 1'b0;
        reg_vldq[i] <= 1'b0;
        reg_vsdq[i] <= 1'b0;
        reg_utaq[i] <= 1'b0;
        reg_utldq[i] <= 1'b0;
        reg_utsdq[i] <= 1'b0;
      end
    end
    else
    begin
      for (i=0; i<`SHIFT_BUF_READ; i=i+1)
      begin
        reg_viu[i] <= next_viu[i];
        reg_viu_fn[i] <= next_viu_fn[i];
        reg_viu_utidx[i] <= next_viu_utidx[i];
        reg_viu_imm[i] <= next_viu_imm[i];
        reg_vau0[i] <= next_vau0[i];
        reg_vau0_fn[i] <= next_vau0_fn[i];
        reg_vau1[i] <= next_vau1[i];
        reg_vau1_fn[i] <= next_vau1_fn[i];
        reg_vau2[i] <= next_vau2[i];
        reg_vau2_fn[i] <= next_vau2_fn[i];
        reg_vldq[i] <= next_vldq[i];
        reg_vsdq[i] <= next_vsdq[i];
        reg_utaq[i] <= next_utaq[i];
        reg_utldq[i] <= next_utldq[i];
        reg_utsdq[i] <= next_utsdq[i];
      end
    end
  end

  always @(*)
  begin
    for (i=0; i<`SHIFT_BUF_READ-1; i=i+1)
    begin
      next_viu[i] = reg_viu[i+1];
      next_viu_fn[i] = reg_viu_fn[i+1];
      next_viu_utidx[i] = reg_viu_utidx[i+1];
      next_viu_imm[i] = reg_viu_imm[i+1];
      next_vau0[i] = reg_vau0[i+1];
      next_vau0_fn[i] = reg_vau0_fn[i+1];
      next_vau1[i] = reg_vau1[i+1];
      next_vau1_fn[i] = reg_vau1_fn[i+1];
      next_vau2[i] = reg_vau2[i+1];
      next_vau2_fn[i] = reg_vau2_fn[i+1];
      next_vldq[i] = reg_vldq[i+1];
      next_vsdq[i] = reg_vsdq[i+1];
      next_utaq[i] = reg_utaq[i+1];
      next_utldq[i] = reg_utldq[i+1];
      next_utsdq[i] = reg_utsdq[i+1];
    end

    next_viu[`SHIFT_BUF_READ-1] = 1'b0;
    next_viu_fn[`SHIFT_BUF_READ-1] = `SZ_VIU_FN'd0;
    next_viu_utidx[`SHIFT_BUF_READ-1] = `SZ_VLEN'd0;
    next_viu_imm[`SHIFT_BUF_READ-1] = `SZ_DATA'd0;
    next_vau0[`SHIFT_BUF_READ-1] = 1'b0;
    next_vau0_fn[`SHIFT_BUF_READ-1] = `SZ_VAU0_FN'd0;
    next_vau1[`SHIFT_BUF_READ-1] = 1'b0;
    next_vau1_fn[`SHIFT_BUF_READ-1] = `SZ_VAU1_FN'd0;
    next_vau2[`SHIFT_BUF_READ-1] = 1'b0;
    next_vau2_fn[`SHIFT_BUF_READ-1] = `SZ_VAU2_FN'd0;
    next_vldq[`SHIFT_BUF_READ-1] = 1'b0;
    next_vsdq[`SHIFT_BUF_READ-1] = 1'b0;
    next_utaq[`SHIFT_BUF_READ-1] = 1'b0;
    next_utldq[`SHIFT_BUF_READ-1] = 1'b0;
    next_utsdq[`SHIFT_BUF_READ-1] = 1'b0;

    if (seq_viu)
    begin
      if (seq_fn_viu[`RG_VIU_T] == {`ML,`MR})
      begin
        next_viu[1] = 1'b1;
        next_viu_fn[1] = seq_fn_viu;

        if (seq_vs_zero) next_viu_fn[1][`RG_VIU_T0] = `M0;
        if (seq_vt_zero) next_viu_fn[1][`RG_VIU_T1] = `M0;
      end
      else
      begin
        next_viu[0] = 1'b1;
        next_viu_fn[0] = seq_fn_viu;
        next_viu_utidx[0] = seq_utidx;
        next_viu_imm[0] = seq_imm;

        if (seq_vs_zero) next_viu_fn[0][`RG_VIU_T0] = `M0;
      end
    end
    else if (seq_vau0)
    begin
      next_vau0[1] = 1'b1;
      next_vau0_fn[1] = seq_fn_vau0;
    end
    else if (seq_vau1)
    begin
      if (seq_fn_vau1[2])
      begin
        next_vau1[2] = 1'b1;
        next_vau1_fn[2] = seq_fn_vau1;
      end
      else
      begin
        next_vau1[1] = 1'b1;
        next_vau1_fn[1] = seq_fn_vau1;
      end
    end
    else if (seq_vau2)
    begin
      next_vau2[0] = 1'b1;
      next_vau2_fn[0] = seq_fn_vau2;
    end
    else if (seq_vldq)
    begin
      next_vldq[0] = 1'b1;
    end
    else if (seq_vsdq)
    begin
      next_vsdq[0] = 1'b1;
    end
    else if (seq_utaq)
    begin
      next_utaq[0] = 1'b1;
    end
    else if (seq_utldq)
    begin
      next_utldq[0] = 1'b1;
    end
    else if (seq_utsdq)
    begin
      next_utsdq[0] = 1'b1;
    end
  end

  assign expand_ren = reg_ren[0];
  assign expand_rlast = reg_rlast[0];
  assign expand_rcnt = reg_rcnt[0];
  assign expand_raddr = reg_raddr[0];
  assign expand_roplen = reg_roplen[0];
  assign expand_rblen = reg_rblen[0];

  assign expand_wen = reg_wen[0];
  assign expand_wlast = reg_wlast[0];
  assign expand_wcnt = reg_wcnt[0];
  assign expand_waddr = reg_waddr[0];
  assign expand_wsel = reg_wsel[0];

  assign expand_viu = reg_viu[0];
  assign expand_viu_fn = reg_viu_fn[0];
  assign expand_viu_utidx = reg_viu_utidx[0];
  assign expand_viu_imm = reg_viu_imm[0];
  assign expand_vau0 = reg_vau0[0];
  assign expand_vau0_fn = reg_vau0_fn[0];
  assign expand_vau1 = reg_vau1[0];
  assign expand_vau1_fn = reg_vau1_fn[0];
  assign expand_vau2 = reg_vau2[0];
  assign expand_vau2_fn = reg_vau2_fn[0];
  assign expand_vldq = reg_vldq[0];
  assign expand_vsdq = reg_vsdq[0];
  assign expand_utaq = reg_utaq[0];
  assign expand_utldq = reg_utldq[0];
  assign expand_utsdq = reg_utsdq[0];

endmodule

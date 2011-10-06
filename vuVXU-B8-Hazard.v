`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Banked8_Hazard
(
  input clk,
  input reset,

  input `DEF_BCNT bcnt,

  input issue_tvec_val_viu,
  input issue_tvec_val_vlu,
  input issue_tvec_val_vsu,

  output issue_tvec_rdy,

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

  input issue_vt_val_viu,
  input issue_vt_val_vau0,
  input issue_vt_val_vau1,
  input issue_vt_val_vau2,
  input issue_vt_val_vgslu,
  input issue_vt_val_vglu,
  input issue_vt_val_vgsu,

  output issue_vt_rdy,

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

  input fire_viu,
  input fire_vau0,
  input fire_vau1,
  input fire_vau2,
  input fire_vgslu,
  input fire_vglu,
  input fire_vgsu,
  input fire_vlu,
  input fire_vsu,

  input `DEF_VIU_FN  fire_fn_viu,
  input `DEF_VAU1_FN fire_fn_vau1,

  input `DEF_REGLEN  fire_vd,

  input `DEF_STALL seq_stall,
  input            seq_last,

  input expand_ren,
  input expand_wen,

  input lane_rlast,
  input lane_wlast
);

  wire `DEF_BPTR next_ptr1;
  wire `DEF_BPTR next_ptr2;
  wire `DEF_BPTR next_ptr3;
  wire `DEF_BPTR next_ptr4;
  reg  `DEF_BPTR reg_ptr;

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
  wire `DEF_BPTR1 next_ptr4_add = reg_ptr + `SZ_LGBANK1'd4;

  wire `DEF_BPTR1 next_ptr1_add_bcnt = next_ptr1_add - bcnt;
  wire `DEF_BPTR1 next_ptr2_add_bcnt = next_ptr2_add - bcnt;
  wire `DEF_BPTR1 next_ptr3_add_bcnt = next_ptr3_add - bcnt;
  wire `DEF_BPTR1 next_ptr4_add_bcnt = next_ptr4_add - bcnt;

  wire `DEF_BPTR1 next_ptr4_add_bcnt2 = next_ptr4_add_bcnt - bcnt;

  assign next_ptr1
    = next_ptr1_add < bcnt ? next_ptr1_add[`SZ_LGBANK-1:0]
    : next_ptr1_add_bcnt[`SZ_LGBANK-1:0];

  assign next_ptr2
    = next_ptr2_add < bcnt ? next_ptr2_add[`SZ_LGBANK-1:0]
    : next_ptr2_add_bcnt[`SZ_LGBANK-1:0];

  assign next_ptr3
    = next_ptr3_add < bcnt ? next_ptr3_add[`SZ_LGBANK-1:0]
    : next_ptr3_add_bcnt[`SZ_LGBANK-1:0];

  assign next_ptr4
    = next_ptr4_add < bcnt ? next_ptr4_add[`SZ_LGBANK-1:0]
    : next_ptr4_add_bcnt < bcnt ? next_ptr4_add_bcnt[`SZ_LGBANK-1:0]
    : next_ptr4_add_bcnt2[`SZ_LGBANK-1:0];

  reg `DEF_BANK next_rport_val;
  reg `DEF_BANK next_rport_vau0;
  reg `DEF_BANK next_rport_vau1;
  reg `DEF_BANK next_rport_vau2;
  reg `DEF_BANK next_rport_vsu;
  reg `DEF_BANK next_rport_vgu;

  reg `DEF_BANK array_rport_val;
  reg `DEF_BANK array_rport_vau0;
  reg `DEF_BANK array_rport_vau1;
  reg `DEF_BANK array_rport_vau2;
  reg `DEF_BANK array_rport_vsu;
  reg `DEF_BANK array_rport_vgu;

  always @(posedge clk)
  begin
    if (reset)
    begin
      array_rport_val <= `SZ_BANK'd0;
      array_rport_vau0 <= `SZ_BANK'd0;
      array_rport_vau1 <= `SZ_BANK'd0;
      array_rport_vau2 <= `SZ_BANK'd0;
      array_rport_vsu <= `SZ_BANK'd0;
      array_rport_vgu <= `SZ_BANK'd0;
    end
    else
    begin
      array_rport_val <= next_rport_val;
      array_rport_vau0 <= next_rport_vau0;
      array_rport_vau1 <= next_rport_vau1;
      array_rport_vau2 <= next_rport_vau2;
      array_rport_vsu <= next_rport_vsu;
      array_rport_vgu <= next_rport_vgu;
    end
  end

  always @(*)
  begin
    next_rport_val = array_rport_val;
    next_rport_vau0 = array_rport_vau0;
    next_rport_vau1 = array_rport_vau1;
    next_rport_vau2 = array_rport_vau2;
    next_rport_vsu = array_rport_vsu;
    next_rport_vgu = array_rport_vgu;

    if (fire_viu)
    begin
      next_rport_val[next_ptr2] = 1'b1;

      if (fire_fn_viu[`RG_VIU_T] == {`ML,`MR})
      begin
        next_rport_val[next_ptr3] = 1'b1;
      end
    end
    else if (fire_vau0)
    begin
      next_rport_val[next_ptr2] = 1'b1;
      next_rport_vau0[next_ptr2] = 1'b1;

      next_rport_val[next_ptr3] = 1'b1;
      next_rport_vau0[next_ptr3] = 1'b1;
    end
    else if (fire_vau1)
    begin
      next_rport_val[next_ptr2] = 1'b1;
      next_rport_vau1[next_ptr2] = 1'b1;

      next_rport_val[next_ptr3] = 1'b1;
      next_rport_vau1[next_ptr3] = 1'b1;

      if (fire_fn_vau1[2])
      begin
        next_rport_val[next_ptr4] = 1'b1;
        next_rport_vau1[next_ptr4] = 1'b1;
      end
    end
    else if (fire_vau2)
    begin
      next_rport_val[next_ptr2] = 1'b1;
      next_rport_vau2[next_ptr2] = 1'b1;
    end
    else if (fire_vgslu || fire_vgsu)
    begin
      next_rport_val[next_ptr2] = 1'b1;
      next_rport_vgu[next_ptr2] = 1'b1;

      next_rport_val[next_ptr3] = 1'b1;
      next_rport_vsu[next_ptr3] = 1'b1;
    end
    else if (fire_vglu)
    begin
      next_rport_val[next_ptr2] = 1'b1;
      next_rport_vgu[next_ptr2] = 1'b1;
    end
    else if (fire_vsu)
    begin
      next_rport_val[next_ptr2] = 1'b1;
      next_rport_vsu[next_ptr2] = 1'b1;
    end

    if (lane_rlast)
    begin
      next_rport_val[reg_ptr] = 1'b0;
      next_rport_vau0[reg_ptr] = 1'b0;
      next_rport_vau1[reg_ptr] = 1'b0;
      next_rport_vau2[reg_ptr] = 1'b0;
      next_rport_vsu[reg_ptr] = 1'b0;
      next_rport_vgu[reg_ptr] = 1'b0;
    end
  end

  // I had to change this structure to the following in order to cut the
  // critical path.  I'm precomputing all possible write pointers and then
  // muxing them in later.

  // this delay comes from seq/expand

  `define DELAY `SZ_LGBANK'd2

  // tvec wptr calculation

  wire `DEF_BPTR1 tvec_viu_incr
    = `INT_STAGES + `SZ_LGBANK'd1 + `DELAY; // assuming 1 read port

  wire `DEF_BPTR tvec_viu_wptr1;

  vuVXU_Pointer tvec_viuwptr (reg_ptr, tvec_viu_incr, bcnt, tvec_viu_wptr1);

  wire `DEF_BPTR1 tvec_viu_wptr1_add = tvec_viu_wptr1 + `SZ_LGBANK1'd1;
  wire `DEF_BPTR1 tvec_viu_wptr1_add_sub = tvec_viu_wptr1_add - bcnt;

  wire `DEF_BPTR tvec_viu_wptr2
    = tvec_viu_wptr1_add < bcnt ? tvec_viu_wptr1_add[`SZ_LGBANK-1:0]
    : tvec_viu_wptr1_add_sub[`SZ_LGBANK-1:0];

  wire `DEF_BPTR tvec_viu_wptr
    = issue_tvec_val_viu && (issue_tvec_fn_viu[`RG_VIU_T] == {`ML,`MR}) ? tvec_viu_wptr2
    : issue_tvec_val_viu ? tvec_viu_wptr1
    : `SZ_LGBANK'bx;

  // vt wptr calculation

  wire `DEF_BPTR1 vt_viu_incr
    = `INT_STAGES + `SZ_LGBANK'd1 + `DELAY; // assuming 1 read port

  wire `DEF_BPTR1 vt_vau0_incr
    = `IMUL_STAGES + `SZ_LGBANK'd2 + `DELAY;

  wire `DEF_BPTR1 vt_vau1_incr
    = `FMA_STAGES + `SZ_LGBANK'd2 + `DELAY; // assuming 2 read ports

  wire `DEF_BPTR1 vt_vau2_incr
    = `FCONV_STAGES + `SZ_LGBANK'd1 + `DELAY;

  wire `DEF_BPTR vt_viu_wptr1;
  wire `DEF_BPTR vt_vau0_wptr;
  wire `DEF_BPTR vt_vau1_wptr2;
  wire `DEF_BPTR vt_vau2_wptr;

  vuVXU_Pointer vt_viuwptr  (reg_ptr, vt_viu_incr,  bcnt, vt_viu_wptr1);
  vuVXU_Pointer vt_vau0wptr (reg_ptr, vt_vau0_incr, bcnt, vt_vau0_wptr);
  vuVXU_Pointer vt_vau1wptr (reg_ptr, vt_vau1_incr, bcnt, vt_vau1_wptr2);
  vuVXU_Pointer vt_vau2wptr (reg_ptr, vt_vau2_incr, bcnt, vt_vau2_wptr);

  wire `DEF_BPTR1 vt_viu_wptr1_add = vt_viu_wptr1 + `SZ_LGBANK1'd1;
  wire `DEF_BPTR1 vt_viu_wptr1_add_sub = vt_viu_wptr1_add - bcnt;

  wire `DEF_BPTR1 vt_vau1_wptr2_add = vt_vau1_wptr2 + `SZ_LGBANK1'd1;
  wire `DEF_BPTR1 vt_vau1_wptr2_add_sub = vt_vau1_wptr2_add - bcnt;

  wire `DEF_BPTR vt_viu_wptr2
    = vt_viu_wptr1_add < bcnt ? vt_viu_wptr1_add[`SZ_LGBANK-1:0]
    : vt_viu_wptr1_add_sub[`SZ_LGBANK-1:0];

  wire `DEF_BPTR vt_vau1_wptr3
    = vt_vau1_wptr2_add < bcnt ? vt_vau1_wptr2_add[`SZ_LGBANK-1:0]
    : vt_vau1_wptr2_add_sub[`SZ_LGBANK-1:0];

  wire `DEF_BPTR vt_viu_wptr
    = issue_vt_val_viu && (issue_vt_fn_viu[`RG_VIU_T] == {`ML,`MR}) ? vt_viu_wptr2
    : issue_vt_val_viu ? vt_viu_wptr1
    : `SZ_LGBANK'bx;

  wire `DEF_BPTR vt_vau1_wptr
    = issue_vt_val_vau1 && issue_vt_fn_vau1[2] ? vt_vau1_wptr3
    : issue_vt_val_vau1 ? vt_vau1_wptr2
    : `SZ_LGBANK'bx;

  wire `DEF_BPTR vt_wptr
    = issue_vt_val_viu ? vt_viu_wptr
    : issue_vt_val_vau0 ? vt_vau0_wptr
    : issue_vt_val_vau1 ? vt_vau1_wptr
    : issue_vt_val_vau2 ? vt_vau2_wptr
    : `SZ_LGBANK'bx;

  // for the fire port
  // we can look at the issue port because we're firing at the same cycle

  wire `DEF_BPTR viu_wptr = issue_tvec_val_viu ? tvec_viu_wptr : vt_viu_wptr;
  wire `DEF_BPTR vau0_wptr = vt_vau0_wptr;
  wire `DEF_BPTR vau1_wptr = vt_vau1_wptr;
  wire `DEF_BPTR vau2_wptr = vt_vau2_wptr;

  reg `DEF_BANK   next_wport_val;
  reg `DEF_BANK   next_wport_head;
  reg `DEF_BANK   next_wport_vau0;
  reg `DEF_BANK   next_wport_vau1;
  reg `DEF_BANK   next_wport_vau2;
  reg `DEF_BANK   next_wport_vlu;
  reg `DEF_REGLEN next_wport_vd [0:7];

  reg `DEF_BANK   array_wport_val;
  reg `DEF_BANK   array_wport_head;
  reg `DEF_BANK   array_wport_vau0;
  reg `DEF_BANK   array_wport_vau1;
  reg `DEF_BANK   array_wport_vau2;
  reg `DEF_BANK   array_wport_vlu;
  reg `DEF_REGLEN array_wport_vd [0:7];

  integer i;

  always @(posedge clk)
  begin
    if (reset)
    begin
      array_wport_val <= `SZ_BANK'd0;
      array_wport_head <= `SZ_BANK'd0;
      array_wport_vau0 <= `SZ_BANK'd0;
      array_wport_vau1 <= `SZ_BANK'd0;
      array_wport_vau2 <= `SZ_BANK'd0;
      array_wport_vlu <= `SZ_BANK'd0;
    end
    else
    begin
      array_wport_val <= next_wport_val;
      array_wport_head <= next_wport_head;
      array_wport_vau0 <= next_wport_vau0;
      array_wport_vau1 <= next_wport_vau1;
      array_wport_vau2 <= next_wport_vau2;
      array_wport_vlu <= next_wport_vlu;

      for (i=0; i<`SZ_BANK; i=i+1)
      begin
        array_wport_vd[i] <= next_wport_vd[i];
      end
    end
  end

  always @(*)
  begin
    next_wport_val = array_wport_val;
    next_wport_head = array_wport_head;
    next_wport_vau0 = array_wport_vau0;
    next_wport_vau1 = array_wport_vau1;
    next_wport_vau2 = array_wport_vau2;
    next_wport_vlu = array_wport_vlu;

    for (i=0; i<`SZ_BANK; i=i+1)
    begin
      next_wport_vd[i] = array_wport_vd[i];
    end
    
    if (fire_viu)
    begin
      next_wport_val[viu_wptr] = 1'b1;
      next_wport_head[viu_wptr] = 1'b1;
      next_wport_vd[viu_wptr] = fire_vd;
    end
    else if (fire_vau0)
    begin
      next_wport_val[vau0_wptr] = 1'b1;
      next_wport_head[vau0_wptr] = 1'b1;
      next_wport_vau0[vau0_wptr] = 1'b1;
      next_wport_vd[vau0_wptr] = fire_vd;
    end
    else if (fire_vau1)
    begin
      next_wport_val[vau1_wptr] = 1'b1;
      next_wport_head[vau1_wptr] = 1'b1;
      next_wport_vau1[vau1_wptr] = 1'b1;
      next_wport_vd[vau1_wptr] = fire_vd;
    end
    else if (fire_vau2)
    begin
      next_wport_val[vau2_wptr] = 1'b1;
      next_wport_head[vau2_wptr] = 1'b1;
      next_wport_vau2[vau2_wptr] = 1'b1;
      next_wport_vd[vau2_wptr] = fire_vd;
    end
    else if (fire_vgslu)
    begin
      next_wport_val[next_ptr4] = 1'b1;
      next_wport_head[next_ptr4] = 1'b1;
      next_wport_vlu[next_ptr4] = 1'b1;
      next_wport_vd[next_ptr4] = fire_vd;
    end
    else if (fire_vglu)
    begin
      next_wport_val[next_ptr3] = 1'b1;
      next_wport_head[next_ptr3] = 1'b1;
      next_wport_vlu[next_ptr3] = 1'b1;
      next_wport_vd[next_ptr3] = fire_vd;
    end
    else if (fire_vlu)
    begin
      next_wport_val[next_ptr2] = 1'b1;
      next_wport_head[next_ptr2] = 1'b1;
      next_wport_vlu[next_ptr2] = 1'b1;
      next_wport_vd[next_ptr2] = fire_vd;
    end

    if (expand_wen)
    begin
      next_wport_head[reg_ptr] = 1'b0;
    end

    if (lane_wlast)
    begin
      next_wport_val[reg_ptr] = 1'b0;
      next_wport_vau0[reg_ptr] = 1'b0;
      next_wport_vau1[reg_ptr] = 1'b0;
      next_wport_vau2[reg_ptr] = 1'b0;
      next_wport_vlu[reg_ptr] = 1'b0;
    end
  end

  reg `DEF_BANK next_sport_val;
  reg `DEF_BANK array_seqslot_val;

  always @(posedge clk)
  begin
    if (reset)
      array_seqslot_val <= `SZ_BANK'd0;
    else
      array_seqslot_val <= next_sport_val;
  end

  always @(*)
  begin
    next_sport_val = array_seqslot_val;

    if (fire_viu || fire_vau0 || fire_vau1 || fire_vau2)
    begin
      next_sport_val[next_ptr1] = 1'b1;
    end
    else if (fire_vgslu)
    begin
      next_sport_val[next_ptr1] = 1'b1;
      next_sport_val[next_ptr2] = 1'b1;
      next_sport_val[next_ptr3] = 1'b1;
    end
    else if (fire_vglu || fire_vgsu)
    begin
      next_sport_val[next_ptr1] = 1'b1;
      next_sport_val[next_ptr2] = 1'b1;
    end
    else if (fire_vlu || fire_vsu)
    begin
      next_sport_val[next_ptr1] = 1'b1;
    end

    if (seq_last)
    begin
      next_sport_val[reg_ptr] = 1'b0;
    end
  end

  // hazard check logic for tvec/vt
  wire shazard_vau0 = (|(array_rport_val & array_rport_vau0)) | (|(array_wport_val & array_wport_vau0));
  wire shazard_vau1 = (|(array_rport_val & array_rport_vau1)) | (|(array_wport_val & array_wport_vau1));
  wire shazard_vau2 = (|(array_rport_val & array_rport_vau2)) | (|(array_wport_val & array_wport_vau2));
  wire shazard_vgu = (|(array_rport_val & array_rport_vgu));
  wire shazard_vlu = (|(array_wport_val & array_wport_vlu));
  wire shazard_vsu = (|(array_rport_val & array_rport_vsu));

  wire seqhazard_1slot = array_seqslot_val[next_ptr1];
  wire seqhazard_2slot = array_seqslot_val[next_ptr1] | array_seqslot_val[next_ptr2];
  wire seqhazard_3slot = array_seqslot_val[next_ptr1] | array_seqslot_val[next_ptr2] | array_seqslot_val[next_ptr3];

  // hazard check logic for tvec
  wire [7:0] tvec_comp_vt =
    {
      issue_tvec_vt == array_wport_vd[7],
      issue_tvec_vt == array_wport_vd[6],
      issue_tvec_vt == array_wport_vd[5],
      issue_tvec_vt == array_wport_vd[4],
      issue_tvec_vt == array_wport_vd[3],
      issue_tvec_vt == array_wport_vd[2],
      issue_tvec_vt == array_wport_vd[1],
      issue_tvec_vt == array_wport_vd[0]
    };

  wire [7:0] tvec_comp_vd =
    {
      issue_tvec_vd == array_wport_vd[7],
      issue_tvec_vd == array_wport_vd[6],
      issue_tvec_vd == array_wport_vd[5],
      issue_tvec_vd == array_wport_vd[4],
      issue_tvec_vd == array_wport_vd[3],
      issue_tvec_vd == array_wport_vd[2],
      issue_tvec_vd == array_wport_vd[1],
      issue_tvec_vd == array_wport_vd[0]
    };

  wire tvec_dhazard_vt = (|(array_wport_val & array_wport_head & tvec_comp_vt));
  wire tvec_dhazard_vd = (|(array_wport_val & array_wport_head & tvec_comp_vd));

  wire tvec_bhazard_r1w1  = array_rport_val[next_ptr2] | array_wport_val[tvec_viu_wptr];
  wire tvec_bhazard_vlu   = array_wport_val[next_ptr2];
  wire tvec_bhazard_vsu   = array_rport_val[next_ptr2];

  wire [2:0] tvec_stall =
    {
      issue_tvec_val_viu & (|seq_stall),
      issue_tvec_val_vlu & (seq_stall[`RG_VSDQ] | seq_stall[`RG_UTAQ] | seq_stall[`RG_UTLDQ] | seq_stall[`RG_UTSDQ]),
      issue_tvec_val_vsu & (seq_stall[`RG_VLDQ] | seq_stall[`RG_UTAQ] | seq_stall[`RG_UTLDQ] | seq_stall[`RG_UTSDQ])
    };

  wire [1:0] tvec_dhazard =
    {
      ~issue_tvec_vt_zero & tvec_dhazard_vt & issue_tvec_dhazard_vt,
      tvec_dhazard_vd & issue_tvec_dhazard_vd
    };

  wire [1:0] tvec_shazard =
    {
      shazard_vlu & issue_tvec_shazard_vlu,
      shazard_vsu & issue_tvec_shazard_vsu
    };

  wire [2:0] tvec_seqhazard =
    {
      issue_tvec_val_viu & seqhazard_1slot,
      issue_tvec_val_vlu & seqhazard_1slot,
      issue_tvec_val_vsu & seqhazard_1slot
    };

  wire [2:0] tvec_bhazard =
    {
      tvec_bhazard_r1w1 & issue_tvec_bhazard_r1w1,
      tvec_bhazard_vlu & issue_tvec_bhazard_vlu,
      tvec_bhazard_vsu & issue_tvec_bhazard_vsu
    };

  assign issue_tvec_rdy = issue_tvec_vd_zero | ~(|tvec_stall) & ~(|tvec_dhazard) & ~(|tvec_shazard) & ~(|tvec_seqhazard) & ~(|tvec_bhazard);

  // hazard check logic for vt
  wire [7:0] vt_comp_vs =
    {
      issue_vt_vs == array_wport_vd[7],
      issue_vt_vs == array_wport_vd[6],
      issue_vt_vs == array_wport_vd[5],
      issue_vt_vs == array_wport_vd[4],
      issue_vt_vs == array_wport_vd[3],
      issue_vt_vs == array_wport_vd[2],
      issue_vt_vs == array_wport_vd[1],
      issue_vt_vs == array_wport_vd[0]
    };

  wire [7:0] vt_comp_vt =
    {
      issue_vt_vt == array_wport_vd[7],
      issue_vt_vt == array_wport_vd[6],
      issue_vt_vt == array_wport_vd[5],
      issue_vt_vt == array_wport_vd[4],
      issue_vt_vt == array_wport_vd[3],
      issue_vt_vt == array_wport_vd[2],
      issue_vt_vt == array_wport_vd[1],
      issue_vt_vt == array_wport_vd[0]
    };

  wire [7:0] vt_comp_vr =
    {
      issue_vt_vr == array_wport_vd[7],
      issue_vt_vr == array_wport_vd[6],
      issue_vt_vr == array_wport_vd[5],
      issue_vt_vr == array_wport_vd[4],
      issue_vt_vr == array_wport_vd[3],
      issue_vt_vr == array_wport_vd[2],
      issue_vt_vr == array_wport_vd[1],
      issue_vt_vr == array_wport_vd[0]
    };

  wire [7:0] vt_comp_vd =
    {
      issue_vt_vd == array_wport_vd[7],
      issue_vt_vd == array_wport_vd[6],
      issue_vt_vd == array_wport_vd[5],
      issue_vt_vd == array_wport_vd[4],
      issue_vt_vd == array_wport_vd[3],
      issue_vt_vd == array_wport_vd[2],
      issue_vt_vd == array_wport_vd[1],
      issue_vt_vd == array_wport_vd[0]
    };

  wire vt_dhazard_vs = (|(array_wport_val & array_wport_head & vt_comp_vs));
  wire vt_dhazard_vt = (|(array_wport_val & array_wport_head & vt_comp_vt));
  wire vt_dhazard_vr = (|(array_wport_val & array_wport_head & vt_comp_vr));
  wire vt_dhazard_vd = (|(array_wport_val & array_wport_head & vt_comp_vd));

  wire vt_bhazard_r1w1  = array_rport_val[next_ptr2] | array_wport_val[vt_wptr];
  wire vt_bhazard_r2w1  = array_rport_val[next_ptr2] | array_rport_val[next_ptr3] | array_wport_val[vt_wptr];
  wire vt_bhazard_r3w1  = array_rport_val[next_ptr2] | array_rport_val[next_ptr3] | array_rport_val[next_ptr4] | array_wport_val[vt_wptr];
  wire vt_bhazard_vgslu = array_rport_val[next_ptr2] | array_rport_val[next_ptr3] | array_wport_val[next_ptr4];
  wire vt_bhazard_vglu  = array_rport_val[next_ptr2] | array_wport_val[next_ptr3];
  wire vt_bhazard_vgsu  = array_rport_val[next_ptr2] | array_rport_val[next_ptr3];

  wire [6:0] vt_stall =
    {
      issue_vt_val_viu & (|seq_stall),
      issue_vt_val_vau0 & (|seq_stall),
      issue_vt_val_vau1 & (|seq_stall),
      issue_vt_val_vau2 & (|seq_stall),
      issue_vt_val_vgslu & (seq_stall[`RG_VLDQ] | seq_stall[`RG_VSDQ]),
      issue_vt_val_vglu & (seq_stall[`RG_VLDQ] | seq_stall[`RG_VSDQ] | seq_stall[`RG_UTSDQ]),
      issue_vt_val_vgsu & (seq_stall[`RG_VLDQ] | seq_stall[`RG_VSDQ] | seq_stall[`RG_UTLDQ])
    };

  wire [3:0] vt_dhazard =
    {
      ~issue_vt_vs_zero & vt_dhazard_vs & issue_vt_dhazard_vs,
      ~issue_vt_vt_zero & vt_dhazard_vt & issue_vt_dhazard_vt,
      ~issue_vt_vr_zero & vt_dhazard_vr & issue_vt_dhazard_vr,
      vt_dhazard_vd & issue_vt_dhazard_vd
    };

  wire [5:0] vt_shazard =
    {
      shazard_vau0 & issue_vt_shazard_vau0,
      shazard_vau1 & issue_vt_shazard_vau1,
      shazard_vau2 & issue_vt_shazard_vau2,
      shazard_vgu & issue_vt_shazard_vgu,
      shazard_vlu & issue_vt_shazard_vlu,
      shazard_vsu & issue_vt_shazard_vsu
    };

  wire [6:0] vt_seqhazard =
    {
      issue_vt_val_viu & seqhazard_1slot,
      issue_vt_val_vau0 & seqhazard_1slot,
      issue_vt_val_vau1 & seqhazard_1slot,
      issue_vt_val_vau2 & seqhazard_1slot,
      issue_vt_val_vgslu & seqhazard_3slot,
      issue_vt_val_vglu & seqhazard_2slot,
      issue_vt_val_vgsu & seqhazard_2slot
    };

  wire [5:0] vt_bhazard =
    {
      vt_bhazard_r1w1 & issue_vt_bhazard_r1w1,
      vt_bhazard_r2w1 & issue_vt_bhazard_r2w1,
      vt_bhazard_r3w1 & issue_vt_bhazard_r3w1,
      vt_bhazard_vgslu & issue_vt_bhazard_vgslu,
      vt_bhazard_vglu & issue_vt_bhazard_vglu,
      vt_bhazard_vgsu & issue_vt_bhazard_vgsu
    };

  assign issue_vt_rdy = issue_vt_vd_zero | ~(|vt_stall) & ~(|vt_dhazard) & ~(|vt_shazard) & ~(|vt_seqhazard) & ~(|vt_bhazard);

`ifndef SYNTHESIS
  // invariant check

  wire [3:0] sum_rport_vau0
    = array_rport_vau0[0] + array_rport_vau0[1] + array_rport_vau0[2] + array_rport_vau0[3]
    + array_rport_vau0[4] + array_rport_vau0[5] + array_rport_vau0[6] + array_rport_vau0[7];

  wire [3:0] sum_rport_vau1
    = array_rport_vau1[0] + array_rport_vau1[1] + array_rport_vau1[2] + array_rport_vau1[3]
    + array_rport_vau1[4] + array_rport_vau1[5] + array_rport_vau1[6] + array_rport_vau1[7];

  wire [3:0] sum_rport_vau2
    = array_rport_vau2[0] + array_rport_vau2[1] + array_rport_vau2[2] + array_rport_vau2[3]
    + array_rport_vau2[4] + array_rport_vau2[5] + array_rport_vau2[6] + array_rport_vau2[7];

  wire [3:0] sum_rport_vsu
    = array_rport_vsu[0] + array_rport_vsu[1] + array_rport_vsu[2] + array_rport_vsu[3]
    + array_rport_vsu[4] + array_rport_vsu[5] + array_rport_vsu[6] + array_rport_vsu[7];

  wire [3:0] sum_rport_vgu
    = array_rport_vgu[0] + array_rport_vgu[1] + array_rport_vgu[2] + array_rport_vgu[3]
    + array_rport_vgu[4] + array_rport_vgu[5] + array_rport_vgu[6] + array_rport_vgu[7];

  wire [3:0] sum_wport_vau0
    = array_wport_vau0[0] + array_wport_vau0[1] + array_wport_vau0[2] + array_wport_vau0[3]
    + array_wport_vau0[4] + array_wport_vau0[5] + array_wport_vau0[6] + array_wport_vau0[7];

  wire [3:0] sum_wport_vau1
    = array_wport_vau1[0] + array_wport_vau1[1] + array_wport_vau1[2] + array_wport_vau1[3]
    + array_wport_vau1[4] + array_wport_vau1[5] + array_wport_vau1[6] + array_wport_vau1[7];

  wire [3:0] sum_wport_vau2
    = array_wport_vau2[0] + array_wport_vau2[1] + array_wport_vau2[2] + array_wport_vau2[3]
    + array_wport_vau2[4] + array_wport_vau2[5] + array_wport_vau2[6] + array_wport_vau2[7];

  wire [3:0] sum_wport_vlu
    = array_wport_vlu[0] + array_wport_vlu[1] + array_wport_vlu[2] + array_wport_vlu[3]
    + array_wport_vlu[4] + array_wport_vlu[5] + array_wport_vlu[6] + array_wport_vlu[7];

  always @(posedge clk)
  begin
    // check for bank conflicts
    if (fire_viu)
    begin
      if ((issue_tvec_bhazard_r1w1 || issue_vt_bhazard_r1w1) && (array_rport_val[next_ptr2] || array_wport_val[viu_wptr]))
        $display("%t: viu r1w1 bank conflict!", $time);
      if (issue_vt_bhazard_r2w1 && (array_rport_val[next_ptr2] || array_rport_val[next_ptr3] || array_wport_val[viu_wptr]))
        $display("%t: viu r2w1 bank conflict!", $time);
    end
    if (fire_vau0)
    begin
      if (issue_vt_bhazard_r2w1 && (array_rport_val[next_ptr2] || array_rport_val[next_ptr3] || array_wport_val[vau0_wptr]))
        $display("%t: vau0 r2w1 bank conflict!", $time);
    end
    if (fire_vau1)
    begin
      if (issue_vt_bhazard_r2w1 && (array_rport_val[next_ptr2] || array_rport_val[next_ptr3] || array_wport_val[vau1_wptr]))
        $display("%t: vau1 r2w1 bank conflict!", $time);
      if (issue_vt_bhazard_r3w1 && (array_rport_val[next_ptr2] || array_rport_val[next_ptr3] || array_rport_val[next_ptr4] || array_wport_val[vau1_wptr]))
        $display("%t: vau1 r3w1 bank conflict!", $time);
    end
    if (fire_vau2)
    begin
      if (issue_vt_bhazard_r1w1 && (array_rport_val[next_ptr2] || array_wport_val[vau2_wptr]))
        $display("%t: vau2 r1w1 bank conflict!", $time);
    end
    if (fire_vgslu)
    begin
      if (array_rport_val[next_ptr2] || array_rport_val[next_ptr3] || array_wport_val[next_ptr4])
        $display("%t: vgslu bank conflict!", $time);
    end
    if (fire_vglu)
    begin
      if (array_rport_val[next_ptr2] || array_wport_val[next_ptr3])
        $display("%t: vglu bank conflict!", $time);
    end
    if (fire_vgsu)
    begin
      if (array_rport_val[next_ptr2] || array_rport_val[next_ptr3])
        $display("%t: vgsu bank conflict!", $time);
    end
    if (fire_vlu)
    begin
      if (array_wport_val[next_ptr2])
        $display("%t: vlu bank conflict!", $time);
    end
    if (fire_vsu)
    begin
      if (array_rport_val[next_ptr2])
        $display("%t: vsu bank conflict!", $time);
    end

    // check vau0 structural invariant
    if (sum_rport_vau0 > 4'd2 || sum_wport_vau0 > 4'd1)
      $display("%t: structural hazard on vau0, rport %d wport %d!", $time, sum_rport_vau0, sum_wport_vau0);

    // check vau1 structural invariant
    if (sum_rport_vau1 > 4'd3 || sum_wport_vau1 > 4'd1)
      $display("%t: structural hazard on vau1, rport %d wport %d!", $time, sum_rport_vau1, sum_wport_vau1);

    // check vau2 structural invariant
    if (sum_rport_vau2 > 4'd1 || sum_wport_vau2 > 4'd1)
      $display("%t: structural hazard on vau2, rport %d wport %d!", $time, sum_rport_vau2, sum_wport_vau2);

    // check vgu structural invariant
    if (sum_rport_vgu > 4'd1)
      $display("%t: structural hazard on vgu, rport %d!", $time, sum_rport_vgu);

    // check vlu structural invariant
    if (sum_wport_vlu > 4'd1)
      $display("%t: structural hazard on vlu, wport %d!", $time, sum_wport_vlu);

    // check vsu structural invariant
    if (sum_rport_vsu > 4'd1)
      $display("%t: structural hazard on vsu, rport %d!", $time, sum_rport_vsu);

    // check ren
    if (expand_ren && !array_rport_val[reg_ptr])
      $display("%t: empty rport on bank %d!", $time, reg_ptr);

    // check wen
    if (expand_wen && !array_wport_val[reg_ptr])
      $display("%t: empty wport on bank %d!", $time, reg_ptr);

    // check ren & rlast
    if (lane_rlast && !array_rport_val[reg_ptr])
      $display("%t: empty last rport on bank %d!", $time, reg_ptr);

    // check wen & wlast
    if (lane_wlast && !array_wport_val[reg_ptr])
      $display("%t: empty last wport on bank %d!", $time, reg_ptr);

  end
`endif

endmodule

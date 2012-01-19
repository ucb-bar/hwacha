package riscvVector
{

import Chisel._;
import Node._;
import vuVXU-B8-Config.scala;

class SequencerIO extends Bundle {
  val last = Bool(OUTPUT);

  val viu   = Bool(OUTPUT);
  val vau0  = Bool(OUTPUT);
  val vau1  = Bool(OUTPUT);
  val vau2  = Bool(OUTPUT);
  val vldq  = Bool(OUTPUT);
  val vsdq  = Bool(OUTPUT);
  val utaq  = Bool(OUTPUT);
  val utldq = Bool(OUTPUT);
  val utsdq = Bool(OUTPUT);

  val fn_viu = Bits(DEF_VIU_FN , OUTPUT);
  val fn_vau0 = Bits(DEF_VAU0_FN, OUTPUT);
  val fn_vau1 = Bits(DEF_VAU1_FN, OUTPUT);
  val fn_vau2 = Bits(DEF_VAU2_FN, OUTPUT);

  val cnt     = Bits(DEF_BVLEN, OUTPUT);
  val utidx   = Bits(DEF_VLEN, OUTPUT);
  val vs_zero = Bool(OUTPUT);
  val vt_zero = Bool(OUTPUT);
  val vr_zero = Bool(OUTPUT);
  val vs      = Bits(DEF_BREGLEN, OUTPUT);
  val vt      = Bits(DEF_BREGLEN, OUTPUT);
  val vr      = Bits(DEF_BREGLEN, OUTPUT);
  val vd      = Bits(DEF_BREGLEN, OUTPUT);
  val imm     = Bits(DEF_DATA, OUTPUT);
}

class vuVXU_Banked8_ExpandIO extends Bundle
{
  val seq = new SequencerIO().flip();
  val expand = new ExpanderIO();
}

class vuVXU_Banked8_Expand extends Component 
{
  val io = new vuVXU_B8_ExpandIO();

  val next_ren    = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } };
  val next_rlast  = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } };
  val next_rcnt   = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_BVLEN)} };
  val next_raddr  = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_BREGLEN)} };
  val next_roplen = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_BOPL)} };
  val next_rblen  = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_BRPORT)} };

  val reg_ren    = GenArray(SHIFT_BUF_READ){ Reg(resetVal=Bool(false))};
  val reg_rlast  = GenArray(SHIFT_BUF_READ){ Reg(){ Bool() } };
  val reg_rcnt   = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_BVLEN)} };
  val reg_raddr  = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_BREGLEN)} };
  val reg_roplen = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_BOPL)} };
  val reg_rblen  = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_BRPORT)} };

  for (i <- 0 until SHIFT_BUF_READ){
    reg_ren(i)    <== next_ren(i);
    reg_rlast(i)  <== next_rlast(i);
    reg_rcnt(i)   <== next_rcnt(i);
    reg_raddr(i)  <== next_raddr(i);
    reg_roplen(i) <== next_roplen(i);
    reg_rblen(i)  <== next_rblen(i);
  }

  otherwise 
  {
    for (i <- 0 until SHIFT_BUF_READ-1)
      {
	next_ren(i)    <== reg_ren(i+1);
	next_rlast(i)  <== reg_rlast(i+1);
	next_rcnt(i)   <== reg_rcnt(i+1);
	next_raddr(i)  <== reg_raddr(i+1);
	next_roplen(i) <== reg_roplen(i+1);
	next_rblen(i)  <== reg_rblen(i+1);
      }

    next_ren(SHIFT_BUF_READ-1)    <== Bool(false);
    next_rlast(SHIFT_BUF_READ-1)  <== Bool(false);
    next_rcnt(SHIFT_BUF_READ-1)   <== Bits("d0", 3);;
    next_raddr(SHIFT_BUF_READ-1)  <== Bits("d0", 8);;
    next_roplen(SHIFT_BUF_READ-1) <== Bits("d0", 2);;
    next_rblen(SHIFT_BUF_READ-1)  <== Bits("d0", 8);;
  }

  when(io.seq.viu) {
    next_ren(0)    <== Bool(true);
    next_rlast(0)  <== io.seq.last;
    next_rcnt(0)   <== io.seq.cnt;
    next_raddr(0)  <== io.seq.vs;
    next_roplen(0) <== Bits("b01", 2);
    next_rblen(0)  <== Bits("b00_0_000_00", 8);

    when (io.seq.fn_viu(RG_VIU_T) == Cat(ML,MR)){
      next_ren(1) = Bits("b1", 1);
      next_rlast(1) = io.seq.last;
      next_rcnt(1) = io.seq.cnt;
      next_raddr(1) = io.seq.vt;
      next_roplen(1) = Bits("b00", 2);
      next_rblen(1) = Bits("b00_0_000_00", 8);
    }
    
    when(io.seq.fn_viu(RG_VIU_T) == (M0,MR)){
      next_raddr(0) = io.seq.vt;
    }
    when(io.seq.vau0) {
      next_ren(0) = Bits("b1", 1);
      next_rlast(0) = io.seq.last;
      next_rcnt(0) = io.seq.cnt;
      next_raddr(0) = io.seq.vs;
      next_roplen(0) = Bits("b01", 2);
      next_rblen(0) = Bits("b00_0_000_00", 8);

      next_ren(1) = Bits("b1", 1);
      next_rlast(1) = io.seq.last;
      next_rcnt(1) = io.seq.cnt;
      next_raddr(1) = io.seq.vt;
      next_roplen(1) = Bits("b00", 2);
      next_rblen(1) = Bits("b00_0_000_11", 8);

      if (io.seq.vs_zero) next_rblen[1][0] = Bits("b0", 1);
      if (io.seq.vt_zero) next_rblen[1][1] = Bits("b0", 1);
    end
    else if (io.seq.vau1)
    begin
      if (io.seq.fn_vau1[2])
      begin
        next_ren[0] = Bits("b1", 1);
        next_rlast[0] = io.seq.last;
        next_rcnt[0] = io.seq.cnt;
        next_raddr[0] = io.seq.vs;
        next_roplen[0] = Bits("b10", 2);
        next_rblen[0] = Bits("b00_0_000_00", 8);

        next_ren[1] = Bits("b1", 1);
        next_rlast[1] = io.seq.last;
        next_rcnt[1] = io.seq.cnt;
        next_raddr[1] = io.seq.vt;
        next_roplen[1] = Bits("b01", 2);
        next_rblen[1] = Bits("b00_0_000_00", 8);

        next_ren[2] = Bits("b1", 1);
        next_rlast[2] = io.seq.last;
        next_rcnt[2] = io.seq.cnt;
        next_raddr[2] = io.seq.vr;
        next_roplen[2] = Bits("b00", 2);
        next_rblen[2] = Bits("b00_0_111_00", 8);

        if (io.seq.vs_zero) next_rblen[2][2] = Bits("b0", 1);
        if (io.seq.vt_zero) next_rblen[2][3] = Bits("b0", 1);
        if (io.seq.vr_zero) next_rblen[2][4] = Bits("b0", 1);
      end
      else
      begin
        next_ren[0] = Bits("b1", 1);
        next_rlast[0] = io.seq.last;
        next_rcnt[0] = io.seq.cnt;
        next_raddr[0] = io.seq.vs;
        next_roplen[0] = Bits("b10", 2);
        next_rblen[0] = Bits("b00_0_000_00", 8);

        next_ren[1] = Bits("b1", 1);
        next_rlast[1] = io.seq.last;
        next_rcnt[1] = io.seq.cnt;
        next_raddr[1] = io.seq.vt;
        next_roplen[1] = Bits("b00", 2);
        next_rblen[1] = Bits("b00_0_101_00", 8);

        if (io.seq.vs_zero) next_rblen[2][2] = Bits("b0", 1);
        if (io.seq.vt_zero) next_rblen[2][4] = Bits("b0", 1);
      end
    end
    else if (io.seq.vau2)
    begin
      next_ren[0] = Bits("b1", 1);
      next_rlast[0] = io.seq.last;
      next_rcnt[0] = io.seq.cnt;
      next_raddr[0] = io.seq.vs;
      next_roplen[0] = Bits("b00", 2);
      next_rblen[0] = Bits("b00_1_000_00", 8);

      if (io.seq.vs_zero) next_rblen[0][5] = Bits("b0", 1);
    end
    else if (io.seq.utaq)
    begin
      next_ren[0] = Bits("b1", 1);
      next_rlast[0] = io.seq.last;
      next_rcnt[0] = io.seq.cnt;
      next_raddr[0] = io.seq.vs;
      next_roplen[0] = Bits("b00", 2);
      next_rblen[0] = Bits("b01_0_000_00", 8);

      if (io.seq.vs_zero) next_rblen[0][6] = Bits("b0", 1);
    end
    else if (io.seq.vsdq || io.seq.utsdq)
    begin
      next_ren[0] = Bits("b1", 1);
      next_rlast[0] = io.seq.last;
      next_rcnt[0] = io.seq.cnt;
      next_raddr[0] = io.seq.vt;
      next_roplen[0] = Bits("b00", 2);
      next_rblen[0] = Bits("b10_0_000_00", 8);

      if (io.seq.vt_zero) next_rblen[0][7] = Bits("b0", 1);
    end
  end

  val next_wen   = GenArray(SHIFT_BUF_WRITE){ Wire(){ Bool() } };
  val next_wlast = GenArray(SHIFT_BUF_WRITE){ Wire(){ Bool() } };
  val next_wcnt  = GenArray(SHIFT_BUF_WRITE){ Wire(){Bits(width=DEF_BVLEN)} };
  val next_waddr = GenArray(SHIFT_BUF_WRITE){ Wire(){Bits(width=DEF_BREGLEN)} };
  val next_wsel  = GenArray(SHIFT_BUF_WRITE){ Wire(){Bits(width=DEF_BWPORT)} };

  val reg_wen   = GenArray(SHIFT_BUF_WRITE){ Reg(){ Bool() } };
  val reg_wlast = GenArray(SHIFT_BUF_WRITE){ Reg(){ Bool() } };
  val reg_wcnt  = GenArray(SHIFT_BUF_WRITE){ Reg(){Bits(width=DEF_BVLEN)} };
  val reg_waddr = GenArray(SHIFT_BUF_WRITE){ Reg(){Bits(width=DEF_BREGLEN)} };
  val reg_wsel  = GenArray(SHIFT_BUF_WRITE){ Reg(){Bits(width=DEF_BWPORT)} };

  always @(posedge clk)
  begin
    if (reset)
    begin
      for (i=0; i<SHIFT_BUF_WRITE; i=i+1)
      begin
        reg_wen(i) <= Bits("b0", 1);
      end
    end
    else
    begin
      for (i=0; i<SHIFT_BUF_WRITE; i=i+1)
      begin
        reg_wen(i) <= next_wen(i);
        reg_wlast(i) <= next_wlast(i);
        reg_wcnt(i) <= next_wcnt(i);
        reg_waddr(i) <= next_waddr(i);
        reg_wsel(i) <= next_wsel(i);
      end
    end
  end

  wire DEF_BPTR1 viu_wptr
    = (io.seq.fn_viu[RG_VIU_T] == {ML,MR}) ? INT_STAGES + SZ_LGBANK'd2
    : INT_STAGES + SZ_LGBANK'd1;

  wire DEF_BPTR1 vau0_wptr
    = IMUL_STAGES + SZ_LGBANK'd2;

  wire DEF_BPTR1 vau1_wptr
    = (io.seq.fn_vau1[2]) ? FMA_STAGES + SZ_LGBANK'd3
    : FMA_STAGES + SZ_LGBANK'd2;

  wire DEF_BPTR1 vau2_wptr
    = FCONV_STAGES + SZ_LGBANK'd1;

  always @(*)
  begin
    for (i=0; i<SHIFT_BUF_WRITE-1; i=i+1)
    begin
      next_wen(i) = reg_wen[i+1];
      next_wlast(i) = reg_wlast[i+1];
      next_wcnt(i) = reg_wcnt[i+1];
      next_waddr(i) = reg_waddr[i+1];
      next_wsel(i) = reg_wsel[i+1];
    end

    next_wen[SHIFT_BUF_WRITE-1] = Bits("b0", 1);
    next_wlast[SHIFT_BUF_WRITE-1] = Bits("b0", 1);
    next_wcnt[SHIFT_BUF_WRITE-1] = Bits("d0", 3);
    next_waddr[SHIFT_BUF_WRITE-1] = Bits("d0", 8);
    next_wsel[SHIFT_BUF_WRITE-1] = Bits("d0", 3);

    if (io.seq.viu)
    begin
      next_wen[viu_wptr] = Bits("b1", 1);
      next_wlast[viu_wptr] = io.seq.last;
      next_wcnt[viu_wptr] = io.seq.cnt;
      next_waddr[viu_wptr] = io.seq.vd;
      next_wsel[viu_wptr] = Bits("d4", 3);
    end
    else if (io.seq.vau0)
    begin
      next_wen[vau0_wptr] = Bits("b1", 1);
      next_wlast[vau0_wptr] = io.seq.last;
      next_wcnt[vau0_wptr] = io.seq.cnt;
      next_waddr[vau0_wptr] = io.seq.vd;
      next_wsel[vau0_wptr] = Bits("d0", 3);
    end
    else if (io.seq.vau1)
    begin
      next_wen[vau1_wptr] = Bits("b1", 1);
      next_wlast[vau1_wptr] = io.seq.last;
      next_wcnt[vau1_wptr] = io.seq.cnt;
      next_waddr[vau1_wptr] = io.seq.vd;
      next_wsel[vau1_wptr] = Bits("d1", 3);
    end
    else if (io.seq.vau2)
    begin
      next_wen[vau2_wptr] = Bits("b1", 1);
      next_wlast[vau2_wptr] = io.seq.last;
      next_wcnt[vau2_wptr] = io.seq.cnt;
      next_waddr[vau2_wptr] = io.seq.vd;
      next_wsel[vau2_wptr] = Bits("d2", 3);
    end
    else if (io.seq.vldq || io.seq.utldq)
    begin
      next_wen[0] = Bits("b1", 1);
      next_wlast[0] = io.seq.last;
      next_wcnt[0] = io.seq.cnt;
      next_waddr[0] = io.seq.vd;
      next_wsel[0] = Bits("d3", 3);
    end
  end

  val next_viu       = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } };
  val next_viu_fn    = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_VIU_FN)} };
  val next_viu_utidx = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_VLEN)} };
  val next_viu_imm   = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_DATA)} };
  val next_vau0      = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } };
  val next_vau0_fn   = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_VAU0_FN)} };
  val next_vau1      = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } };
  val next_vau1_fn   = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_VAU1_FN)} };
  val next_vau2      = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } };
  val next_vau2_fn   = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_VAU2_FN)} };
  val next_vldq      = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } };
  val next_vsdq      = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } };
  val next_utaq      = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } };
  val next_utldq     = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } };
  val next_utsdq     = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } };

  val reg_viu       = GenArray(SHIFT_BUF_READ){ Reg(){ Bool() } };
  val reg_viu_fn    = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_VIU_FN)} };
  val reg_viu_utidx = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_VLEN)} };
  val reg_viu_imm   = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_DATA)} };
  val reg_vau0      = GenArray(SHIFT_BUF_READ){ Reg(){ Bool() } };
  val reg_vau0_fn   = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_VAU0_FN)} };
  val reg_vau1      = GenArray(SHIFT_BUF_READ){ Reg(){ Bool() } };
  val reg_vau1_fn   = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_VAU1_FN)} };
  val reg_vau2      = GenArray(SHIFT_BUF_READ){ Reg(){ Bool() } };
  val reg_vau2_fn   = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_VAU2_FN)} };
  val reg_vldq      = GenArray(SHIFT_BUF_READ){ Reg(){ Bool() } };
  val reg_vsdq      = GenArray(SHIFT_BUF_READ){ Reg(){ Bool() } };
  val reg_utaq      = GenArray(SHIFT_BUF_READ){ Reg(){ Bool() } };
  val reg_utldq     = GenArray(SHIFT_BUF_READ){ Reg(){ Bool() } };
  val reg_utsdq     = GenArray(SHIFT_BUF_READ){ Reg(){ Bool() } };

  always @(posedge clk)
  begin
    if (reset)
    begin
      for (i=0; i<SHIFT_BUF_READ; i=i+1)
      begin
        reg_viu(i) <= Bits("b0", 1);
        reg_vau0(i) <= Bits("b0", 1);
        reg_vau1(i) <= Bits("b0", 1);
        reg_vau2(i) <= Bits("b0", 1);
        reg_vldq(i) <= Bits("b0", 1);
        reg_vsdq(i) <= Bits("b0", 1);
        reg_utaq(i) <= Bits("b0", 1);
        reg_utldq(i) <= Bits("b0", 1);
        reg_utsdq(i) <= Bits("b0", 1);
      end
    end
    else
    begin
      for (i=0; i<SHIFT_BUF_READ; i=i+1)
      begin
        reg_viu(i) <= next_viu(i);
        reg_viu_fn(i) <= next_viu_fn(i);
        reg_viu_utidx(i) <= next_viu_utidx(i);
        reg_viu_imm(i) <= next_viu_imm(i);
        reg_vau0(i) <= next_vau0(i);
        reg_vau0_fn(i) <= next_vau0_fn(i);
        reg_vau1(i) <= next_vau1(i);
        reg_vau1_fn(i) <= next_vau1_fn(i);
        reg_vau2(i) <= next_vau2(i);
        reg_vau2_fn(i) <= next_vau2_fn(i);
        reg_vldq(i) <= next_vldq(i);
        reg_vsdq(i) <= next_vsdq(i);
        reg_utaq(i) <= next_utaq(i);
        reg_utldq(i) <= next_utldq(i);
        reg_utsdq(i) <= next_utsdq(i);
      end
    end
  end

  always @(*)
  begin
    for (i=0; i<SHIFT_BUF_READ-1; i=i+1)
    begin
      next_viu(i) = reg_viu[i+1];
      next_viu_fn(i) = reg_viu_fn[i+1];
      next_viu_utidx(i) = reg_viu_utidx[i+1];
      next_viu_imm(i) = reg_viu_imm[i+1];
      next_vau0(i) = reg_vau0[i+1];
      next_vau0_fn(i) = reg_vau0_fn[i+1];
      next_vau1(i) = reg_vau1[i+1];
      next_vau1_fn(i) = reg_vau1_fn[i+1];
      next_vau2(i) = reg_vau2[i+1];
      next_vau2_fn(i) = reg_vau2_fn[i+1];
      next_vldq(i) = reg_vldq[i+1];
      next_vsdq(i) = reg_vsdq[i+1];
      next_utaq(i) = reg_utaq[i+1];
      next_utldq(i) = reg_utldq[i+1];
      next_utsdq(i) = reg_utsdq[i+1];
    end

    next_viu[SHIFT_BUF_READ-1] = Bits("b0", 1);
    next_viu_fn[SHIFT_BUF_READ-1] = SZ_VIU_FN'd0;
    next_viu_utidx[SHIFT_BUF_READ-1] = SZ_VLEN'd0;
    next_viu_imm[SHIFT_BUF_READ-1] = SZ_DATA'd0;
    next_vau0[SHIFT_BUF_READ-1] = Bits("b0", 1);
    next_vau0_fn[SHIFT_BUF_READ-1] = SZ_VAU0_FN'd0;
    next_vau1[SHIFT_BUF_READ-1] = Bits("b0", 1);
    next_vau1_fn[SHIFT_BUF_READ-1] = SZ_VAU1_FN'd0;
    next_vau2[SHIFT_BUF_READ-1] = Bits("b0", 1);
    next_vau2_fn[SHIFT_BUF_READ-1] = SZ_VAU2_FN'd0;
    next_vldq[SHIFT_BUF_READ-1] = Bits("b0", 1);
    next_vsdq[SHIFT_BUF_READ-1] = Bits("b0", 1);
    next_utaq[SHIFT_BUF_READ-1] = Bits("b0", 1);
    next_utldq[SHIFT_BUF_READ-1] = Bits("b0", 1);
    next_utsdq[SHIFT_BUF_READ-1] = Bits("b0", 1);

    if (io.seq.viu)
    begin
      if (io.seq.fn_viu[RG_VIU_T] == {ML,MR})
      begin
        next_viu[1] = Bits("b1", 1);
        next_viu_fn[1] = io.seq.fn_viu;

        if (io.seq.vs_zero) next_viu_fn[1][RG_VIU_T0] = M0;
        if (io.seq.vt_zero) next_viu_fn[1][RG_VIU_T1] = M0;
      end
      else
      begin
        next_viu[0] = Bits("b1", 1);
        next_viu_fn[0] = io.seq.fn_viu;
        next_viu_utidx[0] = io.seq.utidx;
        next_viu_imm[0] = io.seq.imm;

        if (io.seq.vs_zero) next_viu_fn[0][RG_VIU_T0] = M0;
      end
    end
    else if (io.seq.vau0)
    begin
      next_vau0[1] = Bits("b1", 1);
      next_vau0_fn[1] = io.seq.fn_vau0;
    end
    else if (io.seq.vau1)
    begin
      if (io.seq.fn_vau1[2])
      begin
        next_vau1[2] = Bits("b1", 1);
        next_vau1_fn[2] = io.seq.fn_vau1;
      end
      else
      begin
        next_vau1[1] = Bits("b1", 1);
        next_vau1_fn[1] = io.seq.fn_vau1;
      end
    end
    else if (io.seq.vau2)
    begin
      next_vau2[0] = Bits("b1", 1);
      next_vau2_fn[0] = io.seq.fn_vau2;
    end
    else if (io.seq.vldq)
    begin
      next_vldq[0] = Bits("b1", 1);
    end
    else if (io.seq.vsdq)
    begin
      next_vsdq[0] = Bits("b1", 1);
    end
    else if (io.seq.utaq)
    begin
      next_utaq[0] = Bits("b1", 1);
    end
    else if (io.seq.utldq)
    begin
      next_utldq[0] = Bits("b1", 1);
    end
    else if (io.seq.utsdq)
    begin
      next_utsdq[0] = Bits("b1", 1);
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
}

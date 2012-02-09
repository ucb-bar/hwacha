package riscvVector

import Chisel._;
import Node._;
import Config._;

class vuVXU_Banked8_Seq extends Component {

  val io = new io_vxu_seq();

  val bcntm1 = io.issue_to_seq.bcnt - UFix(1);

  val next_ptr1 = Wire(){ UFix(width=DEF_BPTR) };
  val next_ptr2 = Wire(){ UFix(width=DEF_BPTR) };
  val next_ptr3 = Wire(){ UFix(width=DEF_BPTR) };

  val reg_ptr = Reg(next_ptr1, resetVal = UFix(0, SZ_LGBANK));

  val next_ptr1_add = reg_ptr + UFix(1, SZ_LGBANK1);
  val next_ptr2_add = reg_ptr + UFix(2, SZ_LGBANK1);
  val next_ptr3_add = reg_ptr + UFix(3, SZ_LGBANK1);

  val next_ptr1_add_bcnt = next_ptr1_add - io.issue_to_seq.bcnt;
  val next_ptr2_add_bcnt = next_ptr2_add - io.issue_to_seq.bcnt;
  val next_ptr3_add_bcnt = next_ptr3_add - io.issue_to_seq.bcnt;

  next_ptr1 :=
    Mux(next_ptr1_add < io.issue_to_seq.bcnt, next_ptr1_add(SZ_LGBANK-1,0),
	next_ptr1_add_bcnt(SZ_LGBANK-1,0));

  next_ptr2 :=
    Mux(next_ptr2_add < io.issue_to_seq.bcnt, next_ptr2_add(SZ_LGBANK-1,0),
	next_ptr2_add_bcnt(SZ_LGBANK-1,0));

  next_ptr3 :=
    Mux(next_ptr3_add < io.issue_to_seq.bcnt, next_ptr3_add(SZ_LGBANK-1,0),
	next_ptr3_add_bcnt(SZ_LGBANK-1,0));

  val next_val   = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_last  = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_viu   = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_vau0  = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_vau1  = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_vau2  = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_vlaq  = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_vldq  = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_vsdq  = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_utaq  = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_utldq = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_utsdq = GenArray(DEF_BANK){ Wire(){ Bool() } };

  val next_fn_viu  = GenArray(8){ Wire(){Bits(width=DEF_VIU_FN)} };
  val next_fn_vau0 = GenArray(8){ Wire(){Bits(width=DEF_VAU0_FN)} };
  val next_fn_vau1 = GenArray(8){ Wire(){Bits(width=DEF_VAU1_FN)} };
  val next_fn_vau2 = GenArray(8){ Wire(){Bits(width=DEF_VAU2_FN)} };
  val next_vlen    = GenArray(8){ Wire(){Bits(width=DEF_VLEN)} };
  val next_utidx   = GenArray(8){ Wire(){Bits(width=DEF_VLEN)} };
  val next_stride  = GenArray(8){ Wire(){Bits(width=DEF_REGLEN)} };
  val next_vs_zero = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_vt_zero = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_vr_zero = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_vd_zero = GenArray(DEF_BANK){ Wire(){ Bool() } };
  val next_vs  = GenArray(8){ Wire(){Bits(width=DEF_BREGLEN)} };
  val next_vt  = GenArray(8){ Wire(){Bits(width=DEF_BREGLEN)} };
  val next_vr  = GenArray(8){ Wire(){Bits(width=DEF_BREGLEN)} };
  val next_vd  = GenArray(8){ Wire(){Bits(width=DEF_BREGLEN)} };
  val next_imm = GenArray(8){ Wire(){Bits(width=DEF_DATA)} };
  val next_imm2 = GenArray(8){ Wire(){Bits(width=DEF_VXU_IMM2Q)} };

  val array_val   = Reg(resetVal = Bits(0, SZ_BANK));
  val array_last  = Reg(resetVal = Bits(0, SZ_BANK));
  val array_viu   = Reg(resetVal = Bits(0, SZ_BANK));
  val array_vau0  = Reg(resetVal = Bits(0, SZ_BANK));
  val array_vau1  = Reg(resetVal = Bits(0, SZ_BANK));
  val array_vau2  = Reg(resetVal = Bits(0, SZ_BANK));
  val array_vlaq  = Reg(resetVal = Bits(0, SZ_BANK));
  val array_vldq  = Reg(resetVal = Bits(0, SZ_BANK));
  val array_vsdq  = Reg(resetVal = Bits(0, SZ_BANK));
  val array_utaq  = Reg(resetVal = Bits(0, SZ_BANK));
  val array_utldq = Reg(resetVal = Bits(0, SZ_BANK));
  val array_utsdq = Reg(resetVal = Bits(0, SZ_BANK));

  val array_fn_viu  = GenArray(8){ Reg(){Bits(width=DEF_VIU_FN)} };
  val array_fn_vau0 = GenArray(8){ Reg(){Bits(width=DEF_VAU0_FN)} };
  val array_fn_vau1 = GenArray(8){ Reg(){Bits(width=DEF_VAU1_FN)} };
  val array_fn_vau2 = GenArray(8){ Reg(){Bits(width=DEF_VAU2_FN)} };
  val array_vlen    = GenArray(8){ Reg(){Bits(width=DEF_VLEN)} };
  val array_utidx   = GenArray(8){ Reg(){Bits(width=DEF_VLEN)} };
  val array_stride  = GenArray(8){ Reg(){Bits(width=DEF_REGLEN)} };
  val array_vs_zero = GenArray(DEF_BANK){ Reg(){ Bool() } };
  val array_vt_zero = GenArray(DEF_BANK){ Reg(){ Bool() } };
  val array_vr_zero = GenArray(DEF_BANK){ Reg(){ Bool() } };
  val array_vd_zero = GenArray(DEF_BANK){ Reg(){ Bool() } };
  val array_vs  = GenArray(8){ Reg(){Bits(width=DEF_BREGLEN)} };
  val array_vt  = GenArray(8){ Reg(){Bits(width=DEF_BREGLEN)} };
  val array_vr  = GenArray(8){ Reg(){Bits(width=DEF_BREGLEN)} };
  val array_vd  = GenArray(8){ Reg(){Bits(width=DEF_BREGLEN)} };
  val array_imm = GenArray(8){ Reg(){Bits(width=DEF_DATA)} };
  val array_imm2 = GenArray(8){ Reg(){Bits(width=DEF_VXU_IMM2Q)} };

  array_val   := next_val.flatten();
  array_last  := next_last.flatten();
  array_viu   := next_viu.flatten();
  array_vau0  := next_vau0.flatten();
  array_vau1  := next_vau1.flatten();
  array_vau2  := next_vau2.flatten();
  array_vlaq  := next_vlaq.flatten();
  array_vldq  := next_vldq.flatten();
  array_vsdq  := next_vsdq.flatten();
  array_utaq  := next_utaq.flatten();
  array_utldq := next_utldq.flatten();
  array_utsdq := next_utsdq.flatten();

  array_fn_viu  := next_fn_viu;
  array_fn_vau0 := next_fn_vau0;
  array_fn_vau1 := next_fn_vau1;
  array_fn_vau2 := next_fn_vau2;
  array_vlen    := next_vlen;
  array_utidx   := next_utidx;
  array_stride  := next_stride;
  array_vs_zero := next_vs_zero;
  array_vt_zero := next_vt_zero;
  array_vr_zero := next_vr_zero;
  array_vd_zero := next_vd_zero;
  array_vs      := next_vs;
  array_vt      := next_vt;
  array_vr      := next_vr;
  array_vd      := next_vd;
  array_imm     := next_imm;

  val last = io.issue_to_seq.vlen < io.issue_to_seq.bcnt;

  next_val   <== array_val;
  next_last  <== array_last;
  next_viu   <== array_viu;
  next_vau0  <== array_vau0;
  next_vau1  <== array_vau1;
  next_vau2  <== array_vau2;
  next_vlaq  <== array_vlaq;
  next_vldq  <== array_vldq;
  next_vsdq  <== array_vsdq;
  next_utaq  <== array_utaq;
  next_utldq <== array_utldq;
  next_utsdq <== array_utsdq;

  
  next_fn_viu  <== array_fn_viu;
  next_fn_vau0 <== array_fn_vau0;
  next_fn_vau1 <== array_fn_vau1;
  next_fn_vau2 <== array_fn_vau2;
  next_vlen    <== array_vlen;
  next_utidx   <== array_utidx;
  next_stride  <== array_stride;
  next_vs_zero <== array_vs_zero;
  next_vt_zero <== array_vt_zero;
  next_vr_zero <== array_vr_zero;
  next_vd_zero <== array_vd_zero;
  next_vs      <== array_vs;
  next_vt      <== array_vt;
  next_vr      <== array_vr;
  next_vd      <== array_vd;
  next_imm     <== array_imm;
  
  when(io.fire.viu)
  {
      next_val.write(next_ptr1, Bool(true));
      next_last.write(next_ptr1, last);
      next_viu.write(next_ptr1, Bool(true));
      next_fn_viu.write(next_ptr1, io.fire_fn.viu);
      next_vlen.write(next_ptr1, io.issue_to_seq.vlen);
      next_utidx.write(next_ptr1, Bits(0, SZ_VLEN));
      next_stride.write(next_ptr1, io.issue_to_seq.stride);
      next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero);
      next_vt_zero.write(next_ptr1, io.fire_regid_imm.vt_zero);
      next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs));
      next_vt.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vt));
      next_vd.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vd));
      next_imm.write(next_ptr1, io.fire_regid_imm.imm);
  }
  when (io.fire.vau0)
  {
    next_val.write(next_ptr1, Bool(true));
    next_last.write(next_ptr1, last);
    next_vau0.write(next_ptr1, Bool(true));
    next_fn_vau0.write(next_ptr1, io.fire_fn.vau0);
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen);
    next_stride.write(next_ptr1, io.issue_to_seq.stride);
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero);
    next_vt_zero.write(next_ptr1, io.fire_regid_imm.vt_zero);
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs));
    next_vt.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vt));
    next_vd.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vd));
  }

  when (io.fire.vau1)
  {
    next_val.write(next_ptr1, Bool(true));
    next_last.write(next_ptr1, last);
    next_vau1.write(next_ptr1, Bool(true));
    next_fn_vau1.write(next_ptr1, io.fire_fn.vau1);
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen);
    next_stride.write(next_ptr1, io.issue_to_seq.stride);
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero);
    next_vt_zero.write(next_ptr1, io.fire_regid_imm.vt_zero);
    next_vr_zero.write(next_ptr1, io.fire_regid_imm.vr_zero);
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs));
    next_vt.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vt));
    next_vr.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vr));
    next_vd.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vd));
  }

  when (io.fire.vau2)
  {
    next_val.write(next_ptr1, Bool(true));
    next_last.write(next_ptr1, last);
    next_vau2.write(next_ptr1, Bool(true));
    next_fn_vau2.write(next_ptr1, io.fire_fn.vau2);
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen);
    next_stride.write(next_ptr1, io.issue_to_seq.stride);
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero);
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs));
    next_vd.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vd));
  }

  when (io.fire.vgslu)
  {
    next_val.write(next_ptr1, Bool(true));
    next_last.write(next_ptr1, last);
    next_utaq.write(next_ptr1, Bool(true));
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen);
    next_stride.write(next_ptr1, io.issue_to_seq.stride);
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero);
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs));

    next_val.write(next_ptr2, Bool(true));
    next_last.write(next_ptr2, last);
    next_utsdq.write(next_ptr2, Bool(true));
    next_vlen.write(next_ptr2, io.issue_to_seq.vlen);
    next_stride.write(next_ptr2, io.issue_to_seq.stride);
    next_vt_zero.write(next_ptr2, io.fire_regid_imm.vt_zero);
    next_vt.write(next_ptr2, Cat(Bits("d0",2),io.fire_regid_imm.vt));

    next_val.write(next_ptr3, Bool(true));
    next_last.write(next_ptr3, last);
    next_utldq.write(next_ptr3, Bool(true));
    next_vlen.write(next_ptr3, io.issue_to_seq.vlen);
    next_stride.write(next_ptr3, io.issue_to_seq.stride);
    next_vd.write(next_ptr3, Cat(Bits("d0",2),io.fire_regid_imm.vd));
  }

  when(io.fire.vglu)
  {
    next_val.write(next_ptr1, Bool(true));
    next_last.write(next_ptr1, last);
    next_utaq.write(next_ptr1, Bool(true));
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen);
    next_stride.write(next_ptr1, io.issue_to_seq.stride);
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero);
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs));

    next_val.write(next_ptr2, Bool(true));
    next_last.write(next_ptr2, last);
    next_utldq.write(next_ptr2, Bool(true));
    next_vlen.write(next_ptr2, io.issue_to_seq.vlen);
    next_stride.write(next_ptr2, io.issue_to_seq.stride);
    next_vd.write(next_ptr2, Cat(Bits("d0",2),io.fire_regid_imm.vd));
  }

  when (io.fire.vgsu)
  {
    next_val.write(next_ptr1, Bool(true));
    next_last.write(next_ptr1, last);
    next_utaq.write(next_ptr1, Bool(true));
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen);
    next_stride.write(next_ptr1, io.issue_to_seq.stride);
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero);
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs));

    next_val.write(next_ptr2, Bool(true));
    next_last.write(next_ptr2, last);
    next_utsdq.write(next_ptr2, Bool(true));
    next_vlen.write(next_ptr2, io.issue_to_seq.vlen);
    next_stride.write(next_ptr2, io.issue_to_seq.stride);
    next_vt_zero.write(next_ptr2, io.fire_regid_imm.vt_zero);
    next_vt.write(next_ptr2, Cat(Bits("d0",2),io.fire_regid_imm.vt));
  }

  when (io.fire.vlu)
  {
    // new
    next_val.write(next_ptr1, Bool(true));
    next_last.write(next_ptr1, last);
    next_vlaq.write(next_ptr1, Bool(true));
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen);
    next_stride.write(next_ptr1, io.issue_to_seq.stride);
    next_imm.write(next_ptr1, io.fire_regid_imm.imm);
    next_imm2.write(next_ptr1, io.fire_regid_imm.imm2);
    // new
    
    // new
    next_val.write(next_ptr2, Bool(true));
    next_last.write(next_ptr2, last);
    next_vldq.write(next_ptr2, Bool(true));
    next_vlen.write(next_ptr2, io.issue_to_seq.vlen);
    next_stride.write(next_ptr2, io.issue_to_seq.stride);
    next_vd.write(next_ptr2, Cat(Bits("d0",2),io.fire_regid_imm.vd));
    // new
  }

  when (io.fire.vsu)
  {
    next_val.write(next_ptr1, Bool(true));
    next_last.write(next_ptr1, last);
    next_vsdq.write(next_ptr1, Bool(true));
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen);
    next_stride.write(next_ptr1, io.issue_to_seq.stride);
    next_vt_zero.write(next_ptr1, io.fire_regid_imm.vt_zero);
    next_vt.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vt));
  }

  when (io.seq.viu || io.seq.vau0 || io.seq.vau1 || io.seq.vau2 || io.seq.vlaq || io.seq.vldq || io.seq.vsdq || io.seq.utaq || io.seq.utldq || io.seq.utsdq) // new
  {
    next_vlen.write(reg_ptr, array_vlen.read(reg_ptr) - io.seq_regid_imm.cnt - Bits("b1",1));
    next_utidx.write(reg_ptr, array_utidx.read(reg_ptr) + io.issue_to_seq.bcnt);
    next_vs.write(reg_ptr, array_vs.read(reg_ptr) + array_stride.read(reg_ptr));
    next_vt.write(reg_ptr, array_vt.read(reg_ptr) + array_stride.read(reg_ptr));
    next_vr.write(reg_ptr, array_vr.read(reg_ptr) + array_stride.read(reg_ptr));
    next_vd.write(reg_ptr, array_vd.read(reg_ptr) + array_stride.read(reg_ptr));

    when (array_last(reg_ptr).toBool)
    {
      next_val.write(reg_ptr, Bool(false));
      next_last.write(reg_ptr, Bool(false));
      next_viu.write(reg_ptr, Bool(false));
      next_vau0.write(reg_ptr, Bool(false));
      next_vau1.write(reg_ptr, Bool(false));
      next_vau2.write(reg_ptr, Bool(false));
      next_vlaq.write(reg_ptr, Bool(false)); // new
      next_vldq.write(reg_ptr, Bool(false));
      next_vsdq.write(reg_ptr, Bool(false));
      next_utaq.write(reg_ptr, Bool(false));
      next_utldq.write(reg_ptr, Bool(false));
      next_utsdq.write(reg_ptr, Bool(false));
    }
    when (!array_last(reg_ptr).toBool)
    {
      when (next_vlen.read(reg_ptr) < io.issue_to_seq.bcnt)
      {
        next_last.write(reg_ptr, Bool(true));
      }
    }

  }

  when(io.seq.vlaq)
  {
    next_imm.write(reg_ptr, array_imm.read(reg_ptr) + array_imm2.read(reg_ptr) << 3);
  }

  val next_dep_vlaq = GenArray(SZ_BANK){ Wire(){ Bool() } };
  val next_dep_vldq = GenArray(SZ_BANK){ Wire(){ Bool() } };
  val next_dep_vsdq = GenArray(SZ_BANK){ Wire(){ Bool() } };
  val next_dep_utaq = GenArray(SZ_BANK){ Wire(){ Bool() } };
  val next_dep_utldq = GenArray(SZ_BANK){ Wire(){ Bool() } };
  val next_dep_utsdq = GenArray(SZ_BANK){ Wire(){ Bool() } };

  val array_dep_vlaq = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) };
  val array_dep_vldq = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) };
  val array_dep_vsdq = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) };
  val array_dep_utaq = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) };
  val array_dep_utldq = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) };
  val array_dep_utsdq = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) };

  array_dep_vlaq  := next_dep_vlaq;
  array_dep_vldq  := next_dep_vldq;
  array_dep_vsdq  := next_dep_vsdq;
  array_dep_utaq  := next_dep_utaq;
  array_dep_utldq := next_dep_utldq;
  array_dep_utsdq := next_dep_utsdq;

  next_dep_vlaq <== array_dep_vlaq;
  next_dep_vldq <== array_dep_vldq;
  next_dep_vsdq <== array_dep_vsdq;
  next_dep_utaq <== array_dep_utaq;
  next_dep_utldq <== array_dep_utldq;
  next_dep_utsdq <== array_dep_utsdq;
  
  when (io.fire.viu || io.fire.vau0 || io.fire.vau1 || io.fire.vau2)
  {
    next_dep_vlaq.write(next_ptr1, Bool(true));
    next_dep_vldq.write(next_ptr1, Bool(true));
    next_dep_vsdq.write(next_ptr1, Bool(true));
    next_dep_utaq.write(next_ptr1, Bool(true));
    next_dep_utldq.write(next_ptr1, Bool(true));
    next_dep_utsdq.write(next_ptr1, Bool(true));
  }

  when (io.fire.vgslu)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_utaq(i) <== Bool(false);
    next_dep_vlaq.write(next_ptr1, Bool(true));
    next_dep_vldq.write(next_ptr1, Bool(true));
    next_dep_vsdq.write(next_ptr1, Bool(true));
    next_dep_utaq.write(next_ptr1, Bool(false));
    next_dep_utldq.write(next_ptr1, Bool(true));
    next_dep_utsdq.write(next_ptr1, Bool(true));

    for(i <- 0 until SZ_BANK)
      next_dep_utsdq(i) <== Bool(false);
    next_dep_vlaq.write(next_ptr2, Bool(true));
    next_dep_vldq.write(next_ptr2, Bool(true));
    next_dep_vsdq.write(next_ptr2, Bool(true));
    next_dep_utaq.write(next_ptr2, Bool(true));
    next_dep_utldq.write(next_ptr2, Bool(true));
    next_dep_utsdq.write(next_ptr2, Bool(false));

    for(i <- 0 until SZ_BANK)
      next_dep_utldq(i) <== Bool(false);
    next_dep_vlaq.write(next_ptr3, Bool(true));
    next_dep_vldq.write(next_ptr3, Bool(true));
    next_dep_vsdq.write(next_ptr3, Bool(true));
    next_dep_utaq.write(next_ptr3, Bool(true));
    next_dep_utldq.write(next_ptr3, Bool(false));
    next_dep_utsdq.write(next_ptr3, Bool(true));
  }

  when (io.fire.vglu)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_utaq(i) <== Bool(false);
    next_dep_vlaq.write(next_ptr1, Bool(true));    
    next_dep_vldq.write(next_ptr1, Bool(true));
    next_dep_vsdq.write(next_ptr1, Bool(true));
    next_dep_utaq.write(next_ptr1, Bool(false));
    next_dep_utldq.write(next_ptr1, Bool(true));
    next_dep_utsdq.write(next_ptr1, Bool(true));

    for(i <- 0 until SZ_BANK)
      next_dep_utldq(i) <== Bool(false);
    next_dep_vlaq.write(next_ptr2, Bool(true));    
    next_dep_vldq.write(next_ptr2, Bool(true));
    next_dep_vsdq.write(next_ptr2, Bool(true));
    next_dep_utaq.write(next_ptr2, Bool(true));
    next_dep_utldq.write(next_ptr2, Bool(false));
    next_dep_utsdq.write(next_ptr2, Bool(true));
  }

  when (io.fire.vgsu)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_utaq(i) <== Bool(false);
    next_dep_vlaq.write(next_ptr1, Bool(true));
    next_dep_vldq.write(next_ptr1, Bool(true));
    next_dep_vsdq.write(next_ptr1, Bool(true));
    next_dep_utaq.write(next_ptr1, Bool(false));
    next_dep_utldq.write(next_ptr1, Bool(true));
    next_dep_utsdq.write(next_ptr1, Bool(true));

    for(i <- 0 until SZ_BANK)
      next_dep_utsdq(i) <== Bool(false);
    next_dep_vlaq.write(next_ptr2, Bool(true));
    next_dep_vldq.write(next_ptr2, Bool(true));
    next_dep_vsdq.write(next_ptr2, Bool(true));
    next_dep_utaq.write(next_ptr2, Bool(true));
    next_dep_utldq.write(next_ptr2, Bool(true));
    next_dep_utsdq.write(next_ptr2, Bool(false));
  }

  when (io.fire.vlu)
  {
    //new
    for(i <- 0 until SZ_BANK)
      next_dep_vlaq(i) <== Bool(false);
    next_dep_vlaq.write(next_ptr1, Bool(false));
    next_dep_vldq.write(next_ptr1, Bool(true));
    next_dep_vsdq.write(next_ptr1, Bool(true));
    next_dep_utaq.write(next_ptr1, Bool(true));
    next_dep_utldq.write(next_ptr1, Bool(true));
    next_dep_utsdq.write(next_ptr1, Bool(true));    

    for(i <- 0 until SZ_BANK)
      next_dep_vldq(i) <== Bool(false);
    next_dep_vlaq.write(next_ptr2, Bool(true));
    next_dep_vldq.write(next_ptr2, Bool(false));
    next_dep_vsdq.write(next_ptr2, Bool(true));
    next_dep_utaq.write(next_ptr2, Bool(true));
    next_dep_utldq.write(next_ptr2, Bool(true));
    next_dep_utsdq.write(next_ptr2, Bool(true));
  }

  when (io.fire.vsu)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_vsdq(i) <== Bool(false);
    next_dep_vlaq.write(next_ptr1, Bool(true));
    next_dep_vldq.write(next_ptr1, Bool(true));
    next_dep_vsdq.write(next_ptr1, Bool(false));
    next_dep_utaq.write(next_ptr1, Bool(true));
    next_dep_utldq.write(next_ptr1, Bool(true));
    next_dep_utsdq.write(next_ptr1, Bool(true));
  }

  val current_val = array_val(reg_ptr);
  val current_vlaq_val = current_val & array_vlaq(reg_ptr); // new 
  val current_vldq_val = current_val & array_vldq(reg_ptr);
  val current_vsdq_val = current_val & array_vsdq(reg_ptr);
  val current_utaq_val = current_val & array_utaq(reg_ptr);
  val current_utldq_val = current_val & array_utldq(reg_ptr);
  val current_utsdq_val = current_val & array_utsdq(reg_ptr);

  val reg_vldq_stall = Reg(resetVal = Bool(false));
  val reg_vsdq_stall = Reg(resetVal = Bool(false));
  val reg_utaq_stall = Reg(resetVal = Bool(false));
  val reg_utldq_stall = Reg(resetVal = Bool(false));
  val reg_utsdq_stall = Reg(resetVal = Bool(false));

  when (current_vldq_val.toBool) { reg_vldq_stall <== io.qstall.vldq };
  when (current_vsdq_val.toBool) { reg_vsdq_stall <== io.qstall.vsdq };
  when (current_utaq_val.toBool) { reg_utaq_stall <== io.qstall.utaq };
  when (current_utldq_val.toBool) { reg_utldq_stall <== io.qstall.utldq };
  when (current_utsdq_val.toBool) { reg_utsdq_stall <== io.qstall.utsdq };

  val stall = 
    array_dep_vldq.read(reg_ptr) & reg_vldq_stall |
    array_dep_vsdq.read(reg_ptr) & reg_vsdq_stall |
    array_dep_utaq.read(reg_ptr) & reg_utaq_stall |
    array_dep_utldq.read(reg_ptr) & reg_utldq_stall |
    array_dep_utsdq.read(reg_ptr) & reg_utsdq_stall |
    current_vlaq_val & io.qstall.vlaq | // new
    current_vldq_val & io.qstall.vldq | 
    current_vsdq_val & io.qstall.vsdq |
    current_utaq_val & io.qstall.utaq |
    current_utldq_val & io.qstall.utldq |
    current_utsdq_val & io.qstall.utsdq;

  io.seq_to_hazard.stall := Cat(reg_vldq_stall,reg_vsdq_stall,reg_utaq_stall,reg_utldq_stall,reg_utsdq_stall);

  io.seq_to_hazard.last := ~stall & current_val & array_last(reg_ptr);
  io.seq_to_expand.last := ~stall & current_val & array_last(reg_ptr);

  io.seq.viu := ~stall & current_val & array_viu(reg_ptr);
  io.seq.vau0 := ~stall & current_val & array_vau0(reg_ptr);
  io.seq.vau1 := ~stall & current_val & array_vau1(reg_ptr);
  io.seq.vau2 := ~stall & current_val & array_vau2(reg_ptr);
  io.seq.vlaq := ~stall & current_vlaq_val;  // new
  io.seq.vldq := ~stall & current_vldq_val;
  io.seq.vsdq := ~stall & current_vsdq_val;
  io.seq.utaq := ~stall & current_utaq_val;
  io.seq.utldq := ~stall & current_utldq_val;
  io.seq.utsdq := ~stall & current_utsdq_val;

  io.seq_fn.viu := array_fn_viu.read(reg_ptr);
  io.seq_fn.vau0 := array_fn_vau0.read(reg_ptr);
  io.seq_fn.vau1 := array_fn_vau1.read(reg_ptr);
  io.seq_fn.vau2 := array_fn_vau2.read(reg_ptr);

  io.seq_regid_imm.cnt := Mux(array_vlen.read(reg_ptr) < bcntm1, 
			     array_vlen.read(reg_ptr)(SZ_LGBANK-1,0), 
			     bcntm1(SZ_LGBANK-1,0));
  io.seq_regid_imm.utidx := array_utidx.read(reg_ptr);
  io.seq_regid_imm.vs_zero := array_vs_zero.read(reg_ptr);
  io.seq_regid_imm.vt_zero := array_vt_zero.read(reg_ptr);
  io.seq_regid_imm.vr_zero := array_vr_zero.read(reg_ptr);
  io.seq_regid_imm.vs := array_vs.read(reg_ptr);
  io.seq_regid_imm.vt := array_vt.read(reg_ptr);
  io.seq_regid_imm.vr := array_vr.read(reg_ptr);
  io.seq_regid_imm.vd := array_vd.read(reg_ptr);
  io.seq_regid_imm.imm := array_imm.read(reg_ptr);
  io.seq_regid_imm.imm2 := array_imm2.read(reg_ptr);
}

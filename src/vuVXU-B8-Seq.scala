package hwacha

import Chisel._
import Node._
import Config._
import Interface._

class vuVXU_Banked8_Seq extends Component
{
  val io = new io_vxu_seq()

  val bcntm1 = io.issue_to_seq.bcnt - UFix(1)

  val next_ptr1 = Wire(){ UFix(width=DEF_BPTR) }
  val next_ptr2 = Wire(){ UFix(width=DEF_BPTR) }
  val next_ptr3 = Wire(){ UFix(width=DEF_BPTR) }

  val reg_ptr = Reg(next_ptr1, resetVal = UFix(0, SZ_LGBANK))

  val next_ptr1_add = reg_ptr + UFix(1, SZ_LGBANK1)
  val next_ptr2_add = reg_ptr + UFix(2, SZ_LGBANK1)
  val next_ptr3_add = reg_ptr + UFix(3, SZ_LGBANK1)

  val next_ptr1_add_bcnt = next_ptr1_add - io.issue_to_seq.bcnt
  val next_ptr2_add_bcnt = next_ptr2_add - io.issue_to_seq.bcnt
  val next_ptr3_add_bcnt = next_ptr3_add - io.issue_to_seq.bcnt

  next_ptr1 :=
    Mux(next_ptr1_add < io.issue_to_seq.bcnt, next_ptr1_add(SZ_LGBANK-1,0),
        next_ptr1_add_bcnt(SZ_LGBANK-1,0))

  next_ptr2 :=
    Mux(next_ptr2_add < io.issue_to_seq.bcnt, next_ptr2_add(SZ_LGBANK-1,0),
        next_ptr2_add_bcnt(SZ_LGBANK-1,0))

  next_ptr3 :=
    Mux(next_ptr3_add < io.issue_to_seq.bcnt, next_ptr3_add(SZ_LGBANK-1,0),
        next_ptr3_add_bcnt(SZ_LGBANK-1,0))

  val next_val = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_last = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_viu = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_vau0 = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_vau1 = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_vau2 = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_vaq = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_vldq = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_vsdq = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_utmemop = GenArray(DEF_BANK){ Wire(){ Bool() } }

  val next_fn_viu = GenArray(8){ Wire(){Bits(width=DEF_VIU_FN)} }
  val next_fn_vau0 = GenArray(8){ Wire(){Bits(width=DEF_VAU0_FN)} }
  val next_fn_vau1 = GenArray(8){ Wire(){Bits(width=DEF_VAU1_FN)} }
  val next_fn_vau2 = GenArray(8){ Wire(){Bits(width=DEF_VAU2_FN)} }
  val next_vlen = GenArray(8){ Wire(){Bits(width=DEF_VLEN)} }
  val next_utidx = GenArray(8){ Wire(){Bits(width=DEF_VLEN)} }
  val next_stride = GenArray(8){ Wire(){Bits(width=DEF_REGLEN)} }
  val next_vs_zero = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_vt_zero = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_vr_zero = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_vd_zero = GenArray(DEF_BANK){ Wire(){ Bool() } }
  val next_vs = GenArray(8){ Wire(){Bits(width=DEF_BREGLEN)} }
  val next_vt = GenArray(8){ Wire(){Bits(width=DEF_BREGLEN)} }
  val next_vr = GenArray(8){ Wire(){Bits(width=DEF_BREGLEN)} }
  val next_vd = GenArray(8){ Wire(){Bits(width=DEF_BREGLEN)} }
  val next_mem_cmd = GenArray(8){ Wire(){Bits(width=4)} }
  val next_mem_typ = GenArray(8){ Wire(){Bits(width=3)} }
  val next_mem_typ_float = GenArray(8){ Wire(){Bits(width=1)} }
  val next_imm = GenArray(8){ Wire(){Bits(width=DEF_DATA)} }
  val next_imm2 = GenArray(8){ Wire(){Bits(width=DEF_VXU_IMM2Q)} }
  val next_irb_imm1_rtag = GenArray(DEF_BANK){ Wire(){ Bits(width=IRB_IMM1_SZ) } }
  val next_irb_cnt_rtag = GenArray(DEF_BANK){ Wire(){ Bits(width=IRB_CNT_SZ) } }
  val next_irb_cnt = GenArray(DEF_BANK){ Wire(){ Bits(width=DEF_VLEN) } }
  val next_irb_pc_next = GenArray(DEF_BANK){ Wire(){ Bits(width=SZ_ADDR) } }
  val next_irb_update_imm1 = GenArray(DEF_BANK){ Wire(){ Bool() } }

  val array_val = Reg(resetVal = Bits(0, SZ_BANK))
  val array_last = Reg(resetVal = Bits(0, SZ_BANK))
  val array_viu = Reg(resetVal = Bits(0, SZ_BANK))
  val array_vau0 = Reg(resetVal = Bits(0, SZ_BANK))
  val array_vau1 = Reg(resetVal = Bits(0, SZ_BANK))
  val array_vau2 = Reg(resetVal = Bits(0, SZ_BANK))
  val array_vaq = Reg(resetVal = Bits(0, SZ_BANK))
  val array_vldq = Reg(resetVal = Bits(0, SZ_BANK))
  val array_vsdq = Reg(resetVal = Bits(0, SZ_BANK))
  val array_utmemop = Reg(resetVal = Bits(0, SZ_BANK))

  val array_fn_viu = GenArray(8){ Reg(){Bits(width=DEF_VIU_FN)} }
  val array_fn_vau0 = GenArray(8){ Reg(){Bits(width=DEF_VAU0_FN)} }
  val array_fn_vau1 = GenArray(8){ Reg(){Bits(width=DEF_VAU1_FN)} }
  val array_fn_vau2 = GenArray(8){ Reg(){Bits(width=DEF_VAU2_FN)} }
  val array_vlen = GenArray(8){ Reg(){Bits(width=DEF_VLEN)} }
  val array_utidx = GenArray(8){ Reg(){Bits(width=DEF_VLEN)} }
  val array_stride = GenArray(8){ Reg(){Bits(width=DEF_REGLEN)} }
  val array_vs_zero = GenArray(DEF_BANK){ Reg(){ Bool() } }
  val array_vt_zero = GenArray(DEF_BANK){ Reg(){ Bool() } }
  val array_vr_zero = GenArray(DEF_BANK){ Reg(){ Bool() } }
  val array_vd_zero = GenArray(DEF_BANK){ Reg(){ Bool() } }
  val array_vs = GenArray(8){ Reg(){Bits(width=DEF_BREGLEN)} }
  val array_vt = GenArray(8){ Reg(){Bits(width=DEF_BREGLEN)} }
  val array_vr = GenArray(8){ Reg(){Bits(width=DEF_BREGLEN)} }
  val array_vd = GenArray(8){ Reg(){Bits(width=DEF_BREGLEN)} }
  val array_mem_cmd = GenArray(8){ Reg(){Bits(width=4)} }
  val array_mem_typ = GenArray(8){ Reg(){Bits(width=3)} }
  val array_mem_typ_float = GenArray(8){ Reg(){Bits(width=1)} }
  val array_imm = GenArray(8){ Reg(){Bits(width=DEF_DATA)} }
  val array_imm2 = GenArray(8){ Reg(){Bits(width=DEF_VXU_IMM2Q)} }
  val array_irb_imm1_rtag = GenArray(DEF_BANK){ Reg(){ Bits(width=IRB_IMM1_SZ) } }
  val array_irb_cnt_rtag = GenArray(DEF_BANK){ Reg(){ Bits(width=IRB_CNT_SZ) } }
  val array_irb_cnt = GenArray(DEF_BANK){ Reg(){ Bits(width=DEF_VLEN) } }
  val array_irb_pc_next = GenArray(DEF_BANK){ Reg(){ Bits(width=SZ_ADDR) } }
  val array_irb_update_imm1 = GenArray(DEF_BANK){ Reg(){ Bool() } }

  array_val := next_val.flatten()
  array_last := next_last.flatten()
  array_viu := next_viu.flatten()
  array_vau0 := next_vau0.flatten()
  array_vau1 := next_vau1.flatten()
  array_vau2 := next_vau2.flatten()
  array_vaq := next_vaq.flatten()
  array_vldq := next_vldq.flatten()
  array_vsdq := next_vsdq.flatten()
  array_utmemop := next_utmemop.flatten()

  array_fn_viu := next_fn_viu
  array_fn_vau0 := next_fn_vau0
  array_fn_vau1 := next_fn_vau1
  array_fn_vau2 := next_fn_vau2
  array_vlen := next_vlen
  array_utidx := next_utidx
  array_stride := next_stride
  array_vs_zero := next_vs_zero
  array_vt_zero := next_vt_zero
  array_vr_zero := next_vr_zero
  array_vd_zero := next_vd_zero
  array_vs := next_vs
  array_vt := next_vt
  array_vr := next_vr
  array_vd := next_vd
  array_mem_cmd := next_mem_cmd
  array_mem_typ := next_mem_typ
  array_mem_typ_float := next_mem_typ_float
  array_imm := next_imm
  array_imm2 := next_imm2
  array_irb_imm1_rtag := next_irb_imm1_rtag
  array_irb_cnt_rtag := next_irb_cnt_rtag
  array_irb_cnt := next_irb_cnt
  array_irb_pc_next := next_irb_pc_next
  array_irb_update_imm1 := next_irb_update_imm1

  val last = io.issue_to_seq.vlen < io.issue_to_seq.bcnt

  next_val := array_val
  next_last := array_last
  next_viu := array_viu
  next_vau0 := array_vau0
  next_vau1 := array_vau1
  next_vau2 := array_vau2
  next_vaq := array_vaq
  next_vldq := array_vldq
  next_vsdq := array_vsdq
  next_utmemop := array_utmemop

  next_fn_viu := array_fn_viu
  next_fn_vau0 := array_fn_vau0
  next_fn_vau1 := array_fn_vau1
  next_fn_vau2 := array_fn_vau2
  next_vlen := array_vlen
  next_utidx := array_utidx
  next_stride := array_stride
  next_vs_zero := array_vs_zero
  next_vt_zero := array_vt_zero
  next_vr_zero := array_vr_zero
  next_vd_zero := array_vd_zero
  next_vs := array_vs
  next_vt := array_vt
  next_vr := array_vr
  next_vd := array_vd
  next_mem_cmd := array_mem_cmd
  next_mem_typ := array_mem_typ
  next_mem_typ_float := array_mem_typ_float
  next_imm := array_imm
  next_imm2 := array_imm2
  next_irb_imm1_rtag := array_irb_imm1_rtag
  next_irb_cnt_rtag := array_irb_cnt_rtag
  array_irb_cnt := next_irb_cnt
  next_irb_pc_next := array_irb_pc_next
  next_irb_update_imm1 := array_irb_update_imm1

  when(io.fire.viu)
  {
    next_val.write(next_ptr1, Bool(true))
    next_last.write(next_ptr1, last)
    next_viu.write(next_ptr1, Bool(true))
    next_fn_viu.write(next_ptr1, io.fire_fn.viu)
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen)
    next_utidx.write(next_ptr1, Bits(0, SZ_VLEN))
    next_stride.write(next_ptr1, io.issue_to_seq.stride)
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero)
    next_vt_zero.write(next_ptr1, io.fire_regid_imm.vt_zero)
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs))
    next_vt.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vt))
    next_vd.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vd))
    next_imm.write(next_ptr1, io.fire_regid_imm.imm)
    next_irb_imm1_rtag.write(next_ptr1, io.fire_regid_imm.irb.imm1_rtag)
    next_irb_cnt_rtag.write(next_ptr1, io.fire_regid_imm.irb.cnt_rtag)
    next_irb_cnt.write(next_ptr1, Bits(0))
    next_irb_pc_next.write(next_ptr1, io.fire_regid_imm.irb.pc_next)
    next_irb_update_imm1.write(next_ptr1, io.fire_regid_imm.irb.update_imm1)
  }

  when (io.fire.vau0)
  {
    next_val.write(next_ptr1, Bool(true))
    next_last.write(next_ptr1, last)
    next_vau0.write(next_ptr1, Bool(true))
    next_fn_vau0.write(next_ptr1, io.fire_fn.vau0)
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen)
    next_stride.write(next_ptr1, io.issue_to_seq.stride)
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero)
    next_vt_zero.write(next_ptr1, io.fire_regid_imm.vt_zero)
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs))
    next_vt.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vt))
    next_vd.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vd))
    next_irb_imm1_rtag.write(next_ptr1, io.fire_regid_imm.irb.imm1_rtag)
    next_irb_cnt_rtag.write(next_ptr1, io.fire_regid_imm.irb.cnt_rtag)
    next_irb_cnt.write(next_ptr1, Bits(0))
    next_irb_pc_next.write(next_ptr1, io.fire_regid_imm.irb.pc_next)
    next_irb_update_imm1.write(next_ptr1, io.fire_regid_imm.irb.update_imm1)
  }

  when (io.fire.vau1)
  {
    next_val.write(next_ptr1, Bool(true))
    next_last.write(next_ptr1, last)
    next_vau1.write(next_ptr1, Bool(true))
    next_fn_vau1.write(next_ptr1, io.fire_fn.vau1)
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen)
    next_stride.write(next_ptr1, io.issue_to_seq.stride)
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero)
    next_vt_zero.write(next_ptr1, io.fire_regid_imm.vt_zero)
    next_vr_zero.write(next_ptr1, io.fire_regid_imm.vr_zero)
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs))
    next_vt.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vt))
    next_vr.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vr))
    next_vd.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vd))
    next_irb_imm1_rtag.write(next_ptr1, io.fire_regid_imm.irb.imm1_rtag)
    next_irb_cnt_rtag.write(next_ptr1, io.fire_regid_imm.irb.cnt_rtag)
    next_irb_cnt.write(next_ptr1, Bits(0))
    next_irb_pc_next.write(next_ptr1, io.fire_regid_imm.irb.pc_next)
    next_irb_update_imm1.write(next_ptr1, io.fire_regid_imm.irb.update_imm1)
  }

  when (io.fire.vau2)
  {
    next_val.write(next_ptr1, Bool(true))
    next_last.write(next_ptr1, last)
    next_vau2.write(next_ptr1, Bool(true))
    next_fn_vau2.write(next_ptr1, io.fire_fn.vau2)
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen)
    next_stride.write(next_ptr1, io.issue_to_seq.stride)
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero)
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs))
    next_vd.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vd))
    next_irb_imm1_rtag.write(next_ptr1, io.fire_regid_imm.irb.imm1_rtag)
    next_irb_cnt_rtag.write(next_ptr1, io.fire_regid_imm.irb.cnt_rtag)
    next_irb_cnt.write(next_ptr1, Bits(0))
    next_irb_pc_next.write(next_ptr1, io.fire_regid_imm.irb.pc_next)
    next_irb_update_imm1.write(next_ptr1, io.fire_regid_imm.irb.update_imm1)
  }

  when (io.fire.amo)
  {
    next_val.write(next_ptr1, Bool(true))
    next_last.write(next_ptr1, last)
    next_vaq.write(next_ptr1, Bool(true))
    next_utmemop.write(next_ptr1, Bool(true))
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen)
    next_stride.write(next_ptr1, io.issue_to_seq.stride)
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero)
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs))
    next_mem_cmd.write(next_ptr1, io.fire_regid_imm.mem.cmd)
    next_mem_typ.write(next_ptr1, io.fire_regid_imm.mem.typ)
    next_mem_typ_float.write(next_ptr1, io.fire_regid_imm.mem.typ_float)

    next_val.write(next_ptr2, Bool(true))
    next_last.write(next_ptr2, last)
    next_vsdq.write(next_ptr2, Bool(true))
    next_vlen.write(next_ptr2, io.issue_to_seq.vlen)
    next_stride.write(next_ptr2, io.issue_to_seq.stride)
    next_vt_zero.write(next_ptr2, io.fire_regid_imm.vt_zero)
    next_vt.write(next_ptr2, Cat(Bits("d0",2),io.fire_regid_imm.vt))

    next_val.write(next_ptr3, Bool(true))
    next_last.write(next_ptr3, last)
    next_vldq.write(next_ptr3, Bool(true))
    next_vlen.write(next_ptr3, io.issue_to_seq.vlen)
    next_stride.write(next_ptr3, io.issue_to_seq.stride)
    next_vd.write(next_ptr3, Cat(Bits("d0",2),io.fire_regid_imm.vd))
    next_irb_imm1_rtag.write(next_ptr3, io.fire_regid_imm.irb.imm1_rtag)
    next_irb_cnt_rtag.write(next_ptr3, io.fire_regid_imm.irb.cnt_rtag)
    next_irb_cnt.write(next_ptr1, Bits(0))
    next_irb_pc_next.write(next_ptr3, io.fire_regid_imm.irb.pc_next)
    next_irb_update_imm1.write(next_ptr3, io.fire_regid_imm.irb.update_imm1)
  }

  when(io.fire.utld)
  {
    next_val.write(next_ptr1, Bool(true))
    next_last.write(next_ptr1, last)
    next_vaq.write(next_ptr1, Bool(true))
    next_utmemop.write(next_ptr1, Bool(true))
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen)
    next_stride.write(next_ptr1, io.issue_to_seq.stride)
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero)
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs))
    next_mem_cmd.write(next_ptr1, io.fire_regid_imm.mem.cmd)
    next_mem_typ.write(next_ptr1, io.fire_regid_imm.mem.typ)
    next_mem_typ_float.write(next_ptr1, io.fire_regid_imm.mem.typ_float)
    next_imm.write(next_ptr1, io.fire_regid_imm.imm)

    next_val.write(next_ptr2, Bool(true))
    next_last.write(next_ptr2, last)
    next_vldq.write(next_ptr2, Bool(true))
    next_vlen.write(next_ptr2, io.issue_to_seq.vlen)
    next_stride.write(next_ptr2, io.issue_to_seq.stride)
    next_vd.write(next_ptr2, Cat(Bits("d0",2),io.fire_regid_imm.vd))
    next_irb_imm1_rtag.write(next_ptr2, io.fire_regid_imm.irb.imm1_rtag)
    next_irb_cnt_rtag.write(next_ptr2, io.fire_regid_imm.irb.cnt_rtag)
    next_irb_cnt.write(next_ptr1, Bits(0))
    next_irb_pc_next.write(next_ptr2, io.fire_regid_imm.irb.pc_next)
    next_irb_update_imm1.write(next_ptr2, io.fire_regid_imm.irb.update_imm1)
  }

  when (io.fire.utst)
  {
    next_val.write(next_ptr1, Bool(true))
    next_last.write(next_ptr1, last)
    next_vaq.write(next_ptr1, Bool(true))
    next_utmemop.write(next_ptr1, Bool(true))
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen)
    next_stride.write(next_ptr1, io.issue_to_seq.stride)
    next_vs_zero.write(next_ptr1, io.fire_regid_imm.vs_zero)
    next_vs.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vs))
    next_mem_cmd.write(next_ptr1, io.fire_regid_imm.mem.cmd)
    next_mem_typ.write(next_ptr1, io.fire_regid_imm.mem.typ)
    next_mem_typ_float.write(next_ptr1, io.fire_regid_imm.mem.typ_float)
    next_imm.write(next_ptr1, io.fire_regid_imm.imm)

    next_val.write(next_ptr2, Bool(true))
    next_last.write(next_ptr2, last)
    next_vsdq.write(next_ptr2, Bool(true))
    next_vlen.write(next_ptr2, io.issue_to_seq.vlen)
    next_stride.write(next_ptr2, io.issue_to_seq.stride)
    next_vt_zero.write(next_ptr2, io.fire_regid_imm.vt_zero)
    next_vt.write(next_ptr2, Cat(Bits("d0",2),io.fire_regid_imm.vt))
    next_irb_imm1_rtag.write(next_ptr2, io.fire_regid_imm.irb.imm1_rtag)
    next_irb_cnt_rtag.write(next_ptr2, io.fire_regid_imm.irb.cnt_rtag)
    next_irb_cnt.write(next_ptr1, Bits(0))
    next_irb_pc_next.write(next_ptr2, io.fire_regid_imm.irb.pc_next)
    next_irb_update_imm1.write(next_ptr2, io.fire_regid_imm.irb.update_imm1)
  }

  when (io.fire.vld)
  {
    next_val.write(next_ptr1, Bool(true))
    next_last.write(next_ptr1, last)
    next_vaq.write(next_ptr1, Bool(true))
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen)
    next_stride.write(next_ptr1, io.issue_to_seq.stride)
    next_mem_cmd.write(next_ptr1, io.fire_regid_imm.mem.cmd)
    next_mem_typ.write(next_ptr1, io.fire_regid_imm.mem.typ)
    next_mem_typ_float.write(next_ptr1, io.fire_regid_imm.mem.typ_float)
    next_imm.write(next_ptr1, Cat(Bits(0,1), io.fire_regid_imm.imm(63,0)))
    next_imm2.write(next_ptr1, io.fire_regid_imm.imm2)

    next_val.write(next_ptr2, Bool(true))
    next_last.write(next_ptr2, last)
    next_vldq.write(next_ptr2, Bool(true))
    next_vlen.write(next_ptr2, io.issue_to_seq.vlen)
    next_stride.write(next_ptr2, io.issue_to_seq.stride)
    next_vd.write(next_ptr2, Cat(Bits("d0",2),io.fire_regid_imm.vd))
    next_imm.write(next_ptr2, Cat(Bits(0,1), io.fire_regid_imm.imm(63,0)))
    next_imm2.write(next_ptr2, io.fire_regid_imm.imm2)
    next_irb_imm1_rtag.write(next_ptr2, io.fire_regid_imm.irb.imm1_rtag)
    next_irb_cnt_rtag.write(next_ptr2, io.fire_regid_imm.irb.cnt_rtag)
    next_irb_cnt.write(next_ptr1, Bits(0))
    next_irb_pc_next.write(next_ptr2, io.fire_regid_imm.irb.pc_next)
    next_irb_update_imm1.write(next_ptr2, io.fire_regid_imm.irb.update_imm1)
  }

  when (io.fire.vst)
  {
    next_val.write(next_ptr1, Bool(true))
    next_last.write(next_ptr1, last)
    next_vaq.write(next_ptr1, Bool(true))
    next_vlen.write(next_ptr1, io.issue_to_seq.vlen)
    next_stride.write(next_ptr1, io.issue_to_seq.stride)
    next_mem_cmd.write(next_ptr1, io.fire_regid_imm.mem.cmd)
    next_mem_typ.write(next_ptr1, io.fire_regid_imm.mem.typ)
    next_mem_typ_float.write(next_ptr1, io.fire_regid_imm.mem.typ_float)
    next_imm.write(next_ptr1, Cat(Bits(0,1), io.fire_regid_imm.imm(63,0)))
    next_imm2.write(next_ptr1, io.fire_regid_imm.imm2)

    next_vsdq.write(next_ptr1, Bool(true))
    next_vt_zero.write(next_ptr1, io.fire_regid_imm.vt_zero)
    next_vt.write(next_ptr1, Cat(Bits("d0",2),io.fire_regid_imm.vt))

    next_irb_imm1_rtag.write(next_ptr1, io.fire_regid_imm.irb.imm1_rtag)
    next_irb_cnt_rtag.write(next_ptr1, io.fire_regid_imm.irb.cnt_rtag)
    next_irb_cnt.write(next_ptr1, Bits(0))
    next_irb_pc_next.write(next_ptr1, io.fire_regid_imm.irb.pc_next)
    next_irb_update_imm1.write(next_ptr1, io.fire_regid_imm.irb.update_imm1)
  }

  when (io.seq.viu || io.seq.vau0 || io.seq.vau1 || io.seq.vau2 || io.seq.vaq || io.seq.vldq || io.seq.vsdq)
  {
    next_vlen.write(reg_ptr, array_vlen.read(reg_ptr) - io.seq_regid_imm.cnt - Bits("b1",1))
    next_utidx.write(reg_ptr, array_utidx.read(reg_ptr) + io.issue_to_seq.bcnt)
    next_vs.write(reg_ptr, array_vs.read(reg_ptr) + array_stride.read(reg_ptr))
    next_vt.write(reg_ptr, array_vt.read(reg_ptr) + array_stride.read(reg_ptr))
    next_vr.write(reg_ptr, array_vr.read(reg_ptr) + array_stride.read(reg_ptr))
    next_vd.write(reg_ptr, array_vd.read(reg_ptr) + array_stride.read(reg_ptr))

    when (array_last(reg_ptr).toBool)
    {
      next_val.write(reg_ptr, Bool(false))
      next_last.write(reg_ptr, Bool(false))
      next_viu.write(reg_ptr, Bool(false))
      next_vau0.write(reg_ptr, Bool(false))
      next_vau1.write(reg_ptr, Bool(false))
      next_vau2.write(reg_ptr, Bool(false))
      next_vaq.write(reg_ptr, Bool(false))
      next_vldq.write(reg_ptr, Bool(false))
      next_vsdq.write(reg_ptr, Bool(false))
      next_utmemop.write(reg_ptr, Bool(false))
    }
    when (!array_last(reg_ptr).toBool)
    {
      when (next_vlen.read(reg_ptr) < io.issue_to_seq.bcnt)
      {
        next_last.write(reg_ptr, Bool(true))
      }
    }

  }

  when(io.seq.vaq || io.seq.vldq)
  {
    next_imm.write(reg_ptr, array_imm.read(reg_ptr) + (array_imm2.read(reg_ptr) << UFix(3)))
  }

  val next_dep_vaq = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_dep_vldq = GenArray(SZ_BANK){ Wire(){ Bool() } }
  val next_dep_vsdq = GenArray(SZ_BANK){ Wire(){ Bool() } }

  val array_dep_vaq = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_dep_vldq = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_dep_vsdq = GenArray(SZ_BANK){ Reg(resetVal=Bool(false)) }

  array_dep_vaq := next_dep_vaq
  array_dep_vldq := next_dep_vldq
  array_dep_vsdq := next_dep_vsdq

  next_dep_vaq := array_dep_vaq
  next_dep_vldq := array_dep_vldq
  next_dep_vsdq := array_dep_vsdq

  when (io.fire.viu || io.fire.vau0 || io.fire.vau1 || io.fire.vau2)
  {
    next_dep_vaq.write(next_ptr1, Bool(true))
    next_dep_vldq.write(next_ptr1, Bool(true))
    next_dep_vsdq.write(next_ptr1, Bool(true))
  }

  when (io.fire.amo)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_vaq(i) := Bool(false)
    next_dep_vaq.write(next_ptr1, Bool(false))
    next_dep_vldq.write(next_ptr1, Bool(true))
    next_dep_vsdq.write(next_ptr1, Bool(true))

    for(i <- 0 until SZ_BANK)
      next_dep_vsdq(i) := Bool(false)
    next_dep_vaq.write(next_ptr2, Bool(false))
    next_dep_vldq.write(next_ptr2, Bool(true))
    next_dep_vsdq.write(next_ptr2, Bool(false))

    for(i <- 0 until SZ_BANK)
      next_dep_vldq(i) := Bool(false)
    next_dep_vaq.write(next_ptr3, Bool(false))
    next_dep_vldq.write(next_ptr3, Bool(false))
    next_dep_vsdq.write(next_ptr3, Bool(false))
  }

  when (io.fire.utld)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_vaq(i) := Bool(false)
    next_dep_vaq.write(next_ptr1, Bool(false))
    next_dep_vldq.write(next_ptr1, Bool(true))
    next_dep_vsdq.write(next_ptr1, Bool(true))

    for(i <- 0 until SZ_BANK)
      next_dep_vldq(i) := Bool(false)
    next_dep_vaq.write(next_ptr2, Bool(false))
    next_dep_vldq.write(next_ptr2, Bool(false))
    next_dep_vsdq.write(next_ptr2, Bool(true))
  }

  when (io.fire.utst)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_vaq(i) := Bool(false)
    next_dep_vaq.write(next_ptr1, Bool(false))
    next_dep_vldq.write(next_ptr1, Bool(true))
    next_dep_vsdq.write(next_ptr1, Bool(true))

    for(i <- 0 until SZ_BANK)
      next_dep_vsdq(i) := Bool(false)
    next_dep_vaq.write(next_ptr2, Bool(false))
    next_dep_vldq.write(next_ptr2, Bool(true))
    next_dep_vsdq.write(next_ptr2, Bool(false))
  }

  when (io.fire.vld)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_vaq(i) := Bool(false)
    next_dep_vaq.write(next_ptr1, Bool(false))
    next_dep_vldq.write(next_ptr1, Bool(true))
    next_dep_vsdq.write(next_ptr1, Bool(true))

    for(i <- 0 until SZ_BANK)
      next_dep_vldq(i) := Bool(false)
    next_dep_vaq.write(next_ptr2, Bool(false))
    next_dep_vldq.write(next_ptr2, Bool(false))
    next_dep_vsdq.write(next_ptr2, Bool(true))
  }

  when (io.fire.vst)
  {
    for(i <- 0 until SZ_BANK){
      next_dep_vsdq(i) := Bool(false)
      next_dep_vaq(i) := Bool(false)
    }
    next_dep_vaq.write(next_ptr1, Bool(false))
    next_dep_vldq.write(next_ptr1, Bool(true))
    next_dep_vsdq.write(next_ptr1, Bool(false))
  }

  val current_val = array_val(reg_ptr)
  val current_vaq_val = current_val & array_vaq(reg_ptr)
  val current_vldq_val = current_val & array_vldq(reg_ptr)
  val current_vsdq_val = current_val & array_vsdq(reg_ptr)

  val reg_vaq_stall = Reg(resetVal = Bool(false))
  val reg_vldq_stall = Reg(resetVal = Bool(false))
  val reg_vsdq_stall = Reg(resetVal = Bool(false))

  when (current_vaq_val.toBool) { reg_vaq_stall := io.qstall.vaq }
  when (current_vldq_val.toBool) { reg_vldq_stall := io.qstall.vldq }
  when (current_vsdq_val.toBool) { reg_vsdq_stall := (io.qstall.vaq || io.qstall.vsdq) }

  val stall =
    array_dep_vaq.read(reg_ptr) & reg_vaq_stall |
    array_dep_vldq.read(reg_ptr) & reg_vldq_stall |
    array_dep_vsdq.read(reg_ptr) & reg_vsdq_stall |
    current_vaq_val & io.qstall.vaq |
    current_vldq_val & io.qstall.vldq |
    current_vsdq_val & (io.qstall.vaq | io.qstall.vsdq)

  val reg_stall =
    current_vaq_val & reg_vaq_stall |
    current_vldq_val & reg_vldq_stall |
    current_vsdq_val & reg_vsdq_stall

  io.seq_to_hazard.stall := Cat(reg_vaq_stall, reg_vldq_stall, reg_vsdq_stall)

  io.seq_to_hazard.last := ~stall & current_val & array_last(reg_ptr)
  io.seq_to_expand.last := ~stall & current_val & array_last(reg_ptr)

  io.seq.viu := ~stall & current_val & array_viu(reg_ptr)
  io.seq.vau0 := ~stall & current_val & array_vau0(reg_ptr)
  io.seq.vau1 := ~stall & current_val & array_vau1(reg_ptr)
  io.seq.vau2 := ~stall & current_val & array_vau2(reg_ptr)
  io.seq.vaq := ~stall & current_vaq_val
  io.seq.vldq := ~stall & current_vldq_val
  io.seq.vsdq := ~stall & current_vsdq_val

  io.seq_fn.viu := array_fn_viu.read(reg_ptr)
  io.seq_fn.vau0 := array_fn_vau0.read(reg_ptr)
  io.seq_fn.vau1 := array_fn_vau1.read(reg_ptr)
  io.seq_fn.vau2 := array_fn_vau2.read(reg_ptr)

  io.seq_regid_imm.cnt :=
    Mux(array_vlen.read(reg_ptr) < bcntm1,  array_vlen.read(reg_ptr)(SZ_LGBANK-1,0),
        bcntm1(SZ_LGBANK-1,0))
  io.seq_regid_imm.utidx := array_utidx.read(reg_ptr)
  io.seq_regid_imm.vs_zero := array_vs_zero.read(reg_ptr)
  io.seq_regid_imm.vt_zero := array_vt_zero.read(reg_ptr)
  io.seq_regid_imm.vr_zero := array_vr_zero.read(reg_ptr)
  io.seq_regid_imm.vs := array_vs.read(reg_ptr)
  io.seq_regid_imm.vt := array_vt.read(reg_ptr)
  io.seq_regid_imm.vr := array_vr.read(reg_ptr)
  io.seq_regid_imm.vd := array_vd.read(reg_ptr)
  io.seq_regid_imm.qcnt := Mux(reg_stall, io.seq_regid_imm.cnt + UFix(1, 5), io.seq_regid_imm.cnt + UFix(2, 5))
  io.seq_regid_imm.mem.cmd := array_mem_cmd.read(reg_ptr)
  io.seq_regid_imm.mem.typ := array_mem_typ.read(reg_ptr)
  io.seq_regid_imm.mem.typ_float := array_mem_typ_float.read(reg_ptr)
  io.seq_regid_imm.imm := array_imm.read(reg_ptr)
  io.seq_regid_imm.imm2 := array_imm2.read(reg_ptr)
  io.seq_regid_imm.utmemop := array_utmemop(reg_ptr)

  // irb
  io.seq_to_irb.update_imm1.valid := Bool(false)
  io.seq_to_irb.update_cnt.valid := Bool(false)
  io.seq_to_irb.last := Bool(false)
  
  when (io.seq.viu || io.seq.vau0 || io.seq.vau1 || io.seq.vau2 || io.seq.vldq || io.seq.vsdq)
  {
    next_irb_cnt.write(reg_ptr, io.seq_to_irb.update_cnt.bits.data)

    io.seq_to_irb.update_imm1.valid := array_irb_update_imm1.read(reg_ptr)
    io.seq_to_irb.update_cnt.valid := Bool(true)

    when(array_last(reg_ptr))
    {
      io.seq_to_irb.last := Bool(true)
    }
  }

  io.seq_to_irb.update_imm1.bits.addr := array_irb_imm1_rtag.read(reg_ptr).toUFix
  io.seq_to_irb.update_imm1.bits.data := 
    Mux(io.seq.vldq || io.seq.vsdq, array_imm.read(reg_ptr), 
	array_irb_pc_next.read(reg_ptr))

  io.seq_to_irb.update_cnt.bits.addr := array_irb_cnt_rtag.read(reg_ptr).toUFix
  io.seq_to_irb.update_cnt.bits.data := array_irb_cnt.read(reg_ptr) + io.seq_regid_imm.cnt

}

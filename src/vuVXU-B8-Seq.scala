package hwacha

import Chisel._
import Node._
import Constants._

class io_vxu_seq_fu extends Bundle
{
  val viu = Bool()
  val vau0 = Bool()
  val vau1 = Bool()
  val vau2 = Bool()
  val vaq = Bool()
  val vldq = Bool()
  val vsdq = Bool()
}

class io_vxu_seq_fn extends Bundle
{
  val viu = Bits(width = SZ_VIU_FN)
  val vau0 = Bits(width = SZ_VAU0_FN)
  val vau1 = Bits(width = SZ_VAU1_FN)
  val vau2 = Bits(width = SZ_VAU2_FN)
}

class io_vxu_seq_regid_imm extends Bundle
{
  val cnt = Bits(width = SZ_BVLEN)
  val utidx = Bits(width = SZ_VLEN)
  val vs_zero = Bool()
  val vt_zero = Bool()
  val vr_zero = Bool()
  val vs = Bits(width = SZ_BREGLEN)
  val vt = Bits(width = SZ_BREGLEN)
  val vr = Bits(width = SZ_BREGLEN)
  val vd = Bits(width = SZ_BREGLEN)
  val vm = Bits(width = SZ_BMASK)
  val mem = new io_vxu_mem_cmd()
  val imm = Bits(width = SZ_DATA)
  val imm2 = Bits(width = SZ_XIMM2)
  val utmemop = Bool()
  val aiw = new io_vxu_aiw_bundle()
  val mask = Bits(width=SZ_BANK)
}

class io_vxu_seq_to_hazard extends Bundle
{
  val stall = Bool()
  val last = Bool()
}

class io_vxu_seq_to_expand extends Bundle
{
  val last = Bool()
}

class io_seq_to_aiw extends Bundle
{
  val last = Bool(OUTPUT)
  val update_imm1 = new ioPipe()( new io_aiwUpdateReq(SZ_VIMM, 3) )
  val update_cnt = new ioPipe()( new io_aiwUpdateReq(SZ_VLEN, 3) )
  val update_numCnt = new io_update_num_cnt()
}

class io_vxu_seq extends Bundle
{
  val issue_to_seq = new io_vxu_issue_to_seq().asInput
  val seq_to_hazard = new io_vxu_seq_to_hazard().asOutput
  val seq_to_expand = new io_vxu_seq_to_expand().asOutput

  val qcntp1 = UFix(SZ_QCNT, OUTPUT)
  val qcntp2 = UFix(SZ_QCNT, OUTPUT)
  val qstall = new io_qstall().asInput

  val fire = new io_vxu_issue_fire().asInput
  val fire_fn = new io_vxu_issue_fn().asInput
  val fire_regid_imm = new io_vxu_issue_regid_imm().asInput

  val seq = new io_vxu_seq_fu().asOutput
  val seq_fn = new io_vxu_seq_fn().asOutput
  val seq_regid_imm = new io_vxu_seq_regid_imm().asOutput

  val seq_to_aiw = new io_seq_to_aiw()

  val flush = Bool(INPUT)
  val xcpt_to_seq = new io_xcpt_handler_to_seq().flip()
}

class vuVXU_Banked8_Seq extends Component
{
  val io = new io_vxu_seq()

  val bcntm1 = io.issue_to_seq.bcnt - UFix(1)

  val next_ptr1 = Wire(){ UFix(width=SZ_BPTR) }
  val next_ptr2 = Wire(){ UFix(width=SZ_BPTR) }
  val next_ptr3 = Wire(){ UFix(width=SZ_BPTR) }

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

  val next_val = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_stall = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_last = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_viu = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_vau0 = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_vau1 = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_vau2 = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_vaq = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_vldq = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_vsdq = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_utmemop = Vec(SZ_BANK){ Wire(){ Bool() } }

  val next_fn_viu = Vec(8){ Wire(){Bits(width=SZ_VIU_FN)} }
  val next_fn_vau0 = Vec(8){ Wire(){Bits(width=SZ_VAU0_FN)} }
  val next_fn_vau1 = Vec(8){ Wire(){Bits(width=SZ_VAU1_FN)} }
  val next_fn_vau2 = Vec(8){ Wire(){Bits(width=SZ_VAU2_FN)} }
  val next_vlen = Vec(8){ Wire(){Bits(width=SZ_VLEN)} }
  val next_utidx = Vec(8){ Wire(){Bits(width=SZ_VLEN)} }
  val next_stride = Vec(8){ Wire(){Bits(width=SZ_REGLEN)} }
  val next_vs_zero = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_vt_zero = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_vr_zero = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_vd_zero = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_vs = Vec(8){ Wire(){Bits(width=SZ_BREGLEN)} }
  val next_vt = Vec(8){ Wire(){Bits(width=SZ_BREGLEN)} }
  val next_vr = Vec(8){ Wire(){Bits(width=SZ_BREGLEN)} }
  val next_vd = Vec(8){ Wire(){Bits(width=SZ_BREGLEN)} }
  val next_vm = Vec(8){ Wire(){Bits(width=SZ_BMASK)} }
  val next_mem = Vec(8){ Wire(){ new io_vxu_mem_cmd() } }

  val next_imm = Vec(8){ Wire(){Bits(width=SZ_DATA)} }
  val next_imm2 = Vec(8){ Wire(){Bits(width=SZ_XIMM2)} }
  val next_aiw_imm1_rtag = Vec(SZ_BANK){ Wire(){ Bits(width=SZ_AIW_IMM1) } }
  val next_aiw_cnt_rtag = Vec(SZ_BANK){ Wire(){ Bits(width=SZ_AIW_CNT) } }
  val next_aiw_numCnt_rtag = Vec(SZ_BANK){ Wire(){ Bits(width=SZ_AIW_NUMCNT) } }
  val next_aiw_cnt = Vec(SZ_BANK){ Wire(){ Bits(width=SZ_VLEN) } }
  val next_aiw_pc_next = Vec(SZ_BANK){ Wire(){ Bits(width=SZ_ADDR) } }
  val next_aiw_update_imm1 = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_aiw_update_numCnt = Vec(SZ_BANK){ Wire(){ Bool() } }

  val next_mask = Vec(SZ_BANK){ Wire(){ Bits(width=WIDTH_PVFB) } }

  val array_val = Reg(resetVal = Bits(0, SZ_BANK))
  val array_stall = Reg(resetVal = Bits(0, SZ_BANK))
  val array_last = Reg(resetVal = Bits(0, SZ_BANK))
  val array_viu = Reg(resetVal = Bits(0, SZ_BANK))
  val array_vau0 = Reg(resetVal = Bits(0, SZ_BANK))
  val array_vau1 = Reg(resetVal = Bits(0, SZ_BANK))
  val array_vau2 = Reg(resetVal = Bits(0, SZ_BANK))
  val array_vaq = Reg(resetVal = Bits(0, SZ_BANK))
  val array_vldq = Reg(resetVal = Bits(0, SZ_BANK))
  val array_vsdq = Reg(resetVal = Bits(0, SZ_BANK))
  val array_utmemop = Reg(resetVal = Bits(0, SZ_BANK))

  val array_fn_viu = Vec(8){ Reg(){Bits(width=SZ_VIU_FN)} }
  val array_fn_vau0 = Vec(8){ Reg(){Bits(width=SZ_VAU0_FN)} }
  val array_fn_vau1 = Vec(8){ Reg(){Bits(width=SZ_VAU1_FN)} }
  val array_fn_vau2 = Vec(8){ Reg(){Bits(width=SZ_VAU2_FN)} }
  val array_vlen = Vec(8){ Reg(){Bits(width=SZ_VLEN)} }
  val array_utidx = Vec(8){ Reg(){Bits(width=SZ_VLEN)} }
  val array_stride = Vec(8){ Reg(){Bits(width=SZ_REGLEN)} }
  val array_vs_zero = Vec(SZ_BANK){ Reg(){ Bool() } }
  val array_vt_zero = Vec(SZ_BANK){ Reg(){ Bool() } }
  val array_vr_zero = Vec(SZ_BANK){ Reg(){ Bool() } }
  val array_vd_zero = Vec(SZ_BANK){ Reg(){ Bool() } }
  val array_vs = Vec(8){ Reg(){Bits(width=SZ_BREGLEN)} }
  val array_vt = Vec(8){ Reg(){Bits(width=SZ_BREGLEN)} }
  val array_vr = Vec(8){ Reg(){Bits(width=SZ_BREGLEN)} }
  val array_vd = Vec(8){ Reg(){Bits(width=SZ_BREGLEN)} }
  val array_vm = Vec(8){ Reg(){Bits(width=SZ_BMASK)} }
  val array_mem = Vec(8){ Reg(){ new io_vxu_mem_cmd() } }

  val array_imm = Vec(8){ Reg(){Bits(width=SZ_DATA)} }
  val array_imm2 = Vec(8){ Reg(){Bits(width=SZ_XIMM2)} }
  val array_aiw_imm1_rtag = Vec(SZ_BANK){ Reg(){ Bits(width=SZ_AIW_IMM1) } }
  val array_aiw_cnt_rtag = Vec(SZ_BANK){ Reg(){ Bits(width=SZ_AIW_CNT) } }
  val array_aiw_numCnt_rtag = Vec(SZ_BANK){ Reg(){ Bits(width=SZ_AIW_NUMCNT) } }
  val array_aiw_cnt = Vec(SZ_BANK){ Reg(){ Bits(width=SZ_VLEN) } }
  val array_aiw_pc_next = Vec(SZ_BANK){ Reg(){ Bits(width=SZ_ADDR) } }
  val array_aiw_update_imm1 = Vec(SZ_BANK){ Reg(resetVal = Bool(false)) }
  val array_aiw_update_numCnt = Vec(SZ_BANK){ Reg(resetVal = Bool(false)) }

  val array_mask = Vec(SZ_BANK){ Reg(){ Bits(width=WIDTH_PVFB) } }

  array_val := next_val.toBits
  array_stall := next_stall.toBits
  array_last := next_last.toBits
  array_viu := next_viu.toBits
  array_vau0 := next_vau0.toBits
  array_vau1 := next_vau1.toBits
  array_vau2 := next_vau2.toBits
  array_vaq := next_vaq.toBits
  array_vldq := next_vldq.toBits
  array_vsdq := next_vsdq.toBits
  array_utmemop := next_utmemop.toBits

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
  array_vm := next_vm
  array_mem := next_mem

  array_imm := next_imm
  array_imm2 := next_imm2
  array_aiw_imm1_rtag := next_aiw_imm1_rtag
  array_aiw_cnt_rtag := next_aiw_cnt_rtag
  array_aiw_numCnt_rtag := next_aiw_numCnt_rtag
  array_aiw_cnt := next_aiw_cnt
  array_aiw_pc_next := next_aiw_pc_next
  array_aiw_update_imm1 := next_aiw_update_imm1
  array_aiw_update_numCnt := next_aiw_update_numCnt

  array_mask := next_mask

  val last = io.issue_to_seq.vlen < io.issue_to_seq.bcnt

  next_val := array_val
  next_stall := array_stall
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
  next_vm := array_vm
  next_mem := array_mem

  next_imm := array_imm
  next_imm2 := array_imm2
  next_aiw_imm1_rtag := array_aiw_imm1_rtag
  next_aiw_cnt_rtag := array_aiw_cnt_rtag
  next_aiw_numCnt_rtag := array_aiw_numCnt_rtag  
  next_aiw_cnt := array_aiw_cnt
  next_aiw_pc_next := array_aiw_pc_next
  next_aiw_update_imm1 := array_aiw_update_imm1
  next_aiw_update_numCnt := array_aiw_update_numCnt

  next_mask := array_mask

  when (io.fire.viu)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_viu(next_ptr1) := Bool(true)
    next_fn_viu(next_ptr1) := io.fire_fn.viu
    next_vlen(next_ptr1) := io.issue_to_seq.vlen
    next_utidx(next_ptr1) := io.fire_regid_imm.utidx
    next_stride(next_ptr1) := io.issue_to_seq.stride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vt_zero(next_ptr1) := io.fire_regid_imm.vt_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_vt(next_ptr1) := io.fire_regid_imm.vt
    next_vd(next_ptr1) := io.fire_regid_imm.vd
    next_vm(next_ptr1) := io.fire_regid_imm.vm
    next_imm(next_ptr1) := io.fire_regid_imm.imm

    next_aiw_imm1_rtag(next_ptr1) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr1) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr1) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr1) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr1) := Bool(true)

    next_mask(next_ptr1) := io.fire_regid_imm.mask
  }

  when (io.fire.vau0)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vau0(next_ptr1) := Bool(true)
    next_fn_vau0(next_ptr1) := io.fire_fn.vau0
    next_vlen(next_ptr1) := io.issue_to_seq.vlen
    next_stride(next_ptr1) := io.issue_to_seq.stride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vt_zero(next_ptr1) := io.fire_regid_imm.vt_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_vt(next_ptr1) := io.fire_regid_imm.vt
    next_vd(next_ptr1) := io.fire_regid_imm.vd

    next_aiw_imm1_rtag(next_ptr1) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr1) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr1) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr1) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr1) := Bool(true)

    next_mask(next_ptr1) := io.fire_regid_imm.mask
  }

  when (io.fire.vau1)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vau1(next_ptr1) := Bool(true)
    next_fn_vau1(next_ptr1) := io.fire_fn.vau1
    next_vlen(next_ptr1) := io.issue_to_seq.vlen
    next_stride(next_ptr1) := io.issue_to_seq.stride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vt_zero(next_ptr1) := io.fire_regid_imm.vt_zero
    next_vr_zero(next_ptr1) := io.fire_regid_imm.vr_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_vt(next_ptr1) := io.fire_regid_imm.vt
    next_vr(next_ptr1) := io.fire_regid_imm.vr
    next_vd(next_ptr1) := io.fire_regid_imm.vd

    next_aiw_imm1_rtag(next_ptr1) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr1) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr1) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr1) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr1) := Bool(true)

    next_mask(next_ptr1) := io.fire_regid_imm.mask
  }

  when (io.fire.vau2)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vau2(next_ptr1) := Bool(true)
    next_fn_vau2(next_ptr1) := io.fire_fn.vau2
    next_vlen(next_ptr1) := io.issue_to_seq.vlen
    next_stride(next_ptr1) := io.issue_to_seq.stride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_vd(next_ptr1) := io.fire_regid_imm.vd

    next_aiw_imm1_rtag(next_ptr1) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr1) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr1) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr1) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr1) := Bool(true)

    next_mask(next_ptr1) := io.fire_regid_imm.mask
  }

  when (io.fire.amo)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vaq(next_ptr1) := Bool(true)
    next_utmemop(next_ptr1) := Bool(true)
    next_vlen(next_ptr1) := io.issue_to_seq.vlen
    next_stride(next_ptr1) := io.issue_to_seq.stride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_mem(next_ptr1) := io.fire_regid_imm.mem
    // should always write 0, amo's don't take immediates
    next_imm(next_ptr1) := Bits(0)

    next_mask(next_ptr1) := io.fire_regid_imm.mask

    next_val(next_ptr2) := Bool(true)
    next_last(next_ptr2) := last
    next_vsdq(next_ptr2) := Bool(true)
    next_utmemop(next_ptr2) := Bool(true)
    next_vlen(next_ptr2) := io.issue_to_seq.vlen
    next_stride(next_ptr2) := io.issue_to_seq.stride
    next_vt_zero(next_ptr2) := io.fire_regid_imm.vt_zero
    next_vt(next_ptr2) := io.fire_regid_imm.vt
    next_mem(next_ptr2) := io.fire_regid_imm.mem

    next_mask(next_ptr2) := io.fire_regid_imm.mask

    next_val(next_ptr3) := Bool(true)
    next_last(next_ptr3) := last
    next_vldq(next_ptr3) := Bool(true)
    next_utmemop(next_ptr3) := Bool(true)
    next_vlen(next_ptr3) := io.issue_to_seq.vlen
    next_stride(next_ptr3) := io.issue_to_seq.stride
    next_vd(next_ptr3) := io.fire_regid_imm.vd

    next_aiw_imm1_rtag(next_ptr3) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr3) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr3) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr3) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr3) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr3) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr3) := Bool(true)

    next_mask(next_ptr3) := io.fire_regid_imm.mask
  }

  when (io.fire.utld)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vaq(next_ptr1) := Bool(true)
    next_utmemop(next_ptr1) := Bool(true)
    next_vlen(next_ptr1) := io.issue_to_seq.vlen
    next_stride(next_ptr1) := io.issue_to_seq.stride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_mem(next_ptr1) := io.fire_regid_imm.mem
    next_imm(next_ptr1) := io.fire_regid_imm.imm

    next_mask(next_ptr1) := io.fire_regid_imm.mask

    next_val(next_ptr2) := Bool(true)
    next_last(next_ptr2) := last
    next_vldq(next_ptr2) := Bool(true)
    next_utmemop(next_ptr2) := Bool(true)
    next_vlen(next_ptr2) := io.issue_to_seq.vlen
    next_stride(next_ptr2) := io.issue_to_seq.stride
    next_vd(next_ptr2) := io.fire_regid_imm.vd

    next_aiw_imm1_rtag(next_ptr2) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr2) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr2) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr2) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr2) := Bool(true)

    next_mask(next_ptr2) := io.fire_regid_imm.mask
  }

  when (io.fire.utst)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vaq(next_ptr1) := Bool(true)
    next_utmemop(next_ptr1) := Bool(true)
    next_vlen(next_ptr1) := io.issue_to_seq.vlen
    next_stride(next_ptr1) := io.issue_to_seq.stride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_mem(next_ptr1) := io.fire_regid_imm.mem
    next_imm(next_ptr1) := io.fire_regid_imm.imm

    next_mask(next_ptr1) := io.fire_regid_imm.mask

    next_val(next_ptr2) := Bool(true)
    next_last(next_ptr2) := last
    next_vsdq(next_ptr2) := Bool(true)
    next_utmemop(next_ptr2) := Bool(true)
    next_vlen(next_ptr2) := io.issue_to_seq.vlen
    next_stride(next_ptr2) := io.issue_to_seq.stride
    next_vt_zero(next_ptr2) := io.fire_regid_imm.vt_zero
    next_vt(next_ptr2) := io.fire_regid_imm.vt
    next_mem(next_ptr2) := io.fire_regid_imm.mem  

    next_aiw_imm1_rtag(next_ptr2) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr2) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr2) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr2) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr2) := Bool(true)

    next_mask(next_ptr2) := io.fire_regid_imm.mask
  }

  when (io.fire.vld)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vaq(next_ptr1) := Bool(true)
    next_vlen(next_ptr1) := io.issue_to_seq.vlen
    next_stride(next_ptr1) := io.issue_to_seq.stride
    next_mem(next_ptr1) := io.fire_regid_imm.mem
    next_imm(next_ptr1) := Cat(Bits(0,1), io.fire_regid_imm.imm(63,0))
    next_imm2(next_ptr1) := io.fire_regid_imm.imm2

    next_mask(next_ptr1) := io.fire_regid_imm.mask

    next_val(next_ptr2) := Bool(true)
    next_last(next_ptr2) := last
    next_vldq(next_ptr2) := Bool(true)
    next_vlen(next_ptr2) := io.issue_to_seq.vlen
    next_stride(next_ptr2) := io.issue_to_seq.stride
    next_vd(next_ptr2) := io.fire_regid_imm.vd
    next_imm(next_ptr2) := Cat(Bits(0,1), io.fire_regid_imm.imm(63,0))
    next_imm2(next_ptr2) := io.fire_regid_imm.imm2

    next_aiw_imm1_rtag(next_ptr2) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr2) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr2) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr2) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr2) := Bool(true)

    next_mask(next_ptr2) := io.fire_regid_imm.mask
  }

  when (io.fire.vst)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vaq(next_ptr1) := Bool(true)
    next_vlen(next_ptr1) := io.issue_to_seq.vlen
    next_stride(next_ptr1) := io.issue_to_seq.stride
    next_mem(next_ptr1) := io.fire_regid_imm.mem 
    next_imm(next_ptr1) := Cat(Bits(0,1), io.fire_regid_imm.imm(63,0))
    next_imm2(next_ptr1) := io.fire_regid_imm.imm2

    next_mask(next_ptr1) := io.fire_regid_imm.mask

    next_val(next_ptr2) := Bool(true)
    next_last(next_ptr2) := last
    next_vsdq(next_ptr2) := Bool(true)
    next_vlen(next_ptr2) := io.issue_to_seq.vlen
    next_stride(next_ptr2) := io.issue_to_seq.stride
    next_vt_zero(next_ptr2) := io.fire_regid_imm.vt_zero
    next_vt(next_ptr2) := io.fire_regid_imm.vt
    next_mem(next_ptr2) := io.fire_regid_imm.mem
    next_imm(next_ptr2) := Cat(Bits(0,1), io.fire_regid_imm.imm(63,0))
    next_imm2(next_ptr2) := io.fire_regid_imm.imm2

    next_aiw_imm1_rtag(next_ptr2) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr2) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr2) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr2) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr2) := Bool(true)

    next_mask(next_ptr2) := io.fire_regid_imm.mask
  }

  val next_vlen_update = Mux(array_vlen(reg_ptr) < bcntm1, array_vlen(reg_ptr)(SZ_LGBANK-1,0), bcntm1(SZ_LGBANK-1,0))

  when (io.seq.viu || io.seq.vau0 || io.seq.vau1 || io.seq.vau2 || io.seq.vaq || io.seq.vldq || io.seq.vsdq)
  {
    next_vlen(reg_ptr) := array_vlen(reg_ptr) - next_vlen_update - UFix(1)
    next_utidx(reg_ptr) := array_utidx(reg_ptr) + io.issue_to_seq.bcnt
    next_vs(reg_ptr) := array_vs(reg_ptr) + array_stride(reg_ptr)
    next_vt(reg_ptr) := array_vt(reg_ptr) + array_stride(reg_ptr)
    next_vr(reg_ptr) := array_vr(reg_ptr) + array_stride(reg_ptr)
    next_vd(reg_ptr) := array_vd(reg_ptr) + array_stride(reg_ptr)
    next_vm(reg_ptr) := array_vm(reg_ptr) + UFix(1)
    next_mask(reg_ptr) := array_mask(reg_ptr) >> io.issue_to_seq.bcnt

    when (array_last(reg_ptr))
    {
      next_val(reg_ptr) := Bool(false)
      next_last(reg_ptr) := Bool(false)
      next_viu(reg_ptr) := Bool(false)
      next_vau0(reg_ptr) := Bool(false)
      next_vau1(reg_ptr) := Bool(false)
      next_vau2(reg_ptr) := Bool(false)
      next_vaq(reg_ptr) := Bool(false)
      next_vldq(reg_ptr) := Bool(false)
      next_vsdq(reg_ptr) := Bool(false)
      next_utmemop(reg_ptr) := Bool(false)
      next_aiw_update_imm1(reg_ptr) := Bool(false)
      next_aiw_update_numCnt(reg_ptr) := Bool(false)
    }
    .otherwise
    {
      when (next_vlen(reg_ptr) < io.issue_to_seq.bcnt)
      {
        next_last(reg_ptr) :=  Bool(true)
      }
    }

  }

  val mem_base_plus_stride = array_imm(reg_ptr) + (array_imm2(reg_ptr) << UFix(3))

  when ((io.seq.vaq || io.seq.vldq || io.seq.vsdq) && !io.seq_regid_imm.utmemop)
  {
    next_imm(reg_ptr) := mem_base_plus_stride
  }

  val next_dep_vaq = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_dep_vldq = Vec(SZ_BANK){ Wire(){ Bool() } }
  val next_dep_vsdq = Vec(SZ_BANK){ Wire(){ Bool() } }

  val array_dep_vaq = Vec(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_dep_vldq = Vec(SZ_BANK){ Reg(resetVal=Bool(false)) }
  val array_dep_vsdq = Vec(SZ_BANK){ Reg(resetVal=Bool(false)) }

  array_dep_vaq := next_dep_vaq
  array_dep_vldq := next_dep_vldq
  array_dep_vsdq := next_dep_vsdq

  next_dep_vaq := array_dep_vaq
  next_dep_vldq := array_dep_vldq
  next_dep_vsdq := array_dep_vsdq

  when (io.fire.viu || io.fire.vau0 || io.fire.vau1 || io.fire.vau2)
  {
    next_dep_vaq(next_ptr1) := Bool(true)
    next_dep_vldq(next_ptr1) := Bool(true)
    next_dep_vsdq(next_ptr1) := Bool(true)
  }

  when (io.fire.amo)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_vaq(i) := Bool(false)
    next_dep_vaq(next_ptr1) := Bool(false)
    next_dep_vldq(next_ptr1) := Bool(true)
    next_dep_vsdq(next_ptr1) := Bool(true)

    for(i <- 0 until SZ_BANK)
      next_dep_vsdq(i) := Bool(false)
    next_dep_vaq(next_ptr2) := Bool(false)
    next_dep_vldq(next_ptr2) := Bool(true)
    next_dep_vsdq(next_ptr2) := Bool(false)

    for(i <- 0 until SZ_BANK)
      next_dep_vldq(i) := Bool(false)
    next_dep_vaq(next_ptr3) := Bool(false)
    next_dep_vldq(next_ptr3) := Bool(false)
    next_dep_vsdq(next_ptr3) := Bool(false)
  }

  when (io.fire.utld)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_vaq(i) := Bool(false)
    next_dep_vaq(next_ptr1) := Bool(false)
    next_dep_vldq(next_ptr1) := Bool(true)
    next_dep_vsdq(next_ptr1) := Bool(true)

    for(i <- 0 until SZ_BANK)
      next_dep_vldq(i) := Bool(false)
    next_dep_vaq(next_ptr2) := Bool(false)
    next_dep_vldq(next_ptr2) := Bool(false)
    next_dep_vsdq(next_ptr2) := Bool(true)
  }

  when (io.fire.utst)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_vaq(i) := Bool(false)
    next_dep_vaq(next_ptr1) := Bool(false)
    next_dep_vldq(next_ptr1) := Bool(true)
    next_dep_vsdq(next_ptr1) := Bool(true)

    for(i <- 0 until SZ_BANK)
      next_dep_vsdq(i) := Bool(false)
    next_dep_vaq(next_ptr2) := Bool(false)
    next_dep_vldq(next_ptr2) := Bool(true)
    next_dep_vsdq(next_ptr2) := Bool(false)
  }

  when (io.fire.vld)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_vaq(i) := Bool(false)
    next_dep_vaq(next_ptr1) := Bool(false)
    next_dep_vldq(next_ptr1) := Bool(true)
    next_dep_vsdq(next_ptr1) := Bool(true)

    for(i <- 0 until SZ_BANK)
      next_dep_vldq(i) := Bool(false)
    next_dep_vaq(next_ptr2) := Bool(false)
    next_dep_vldq(next_ptr2) := Bool(false)
    next_dep_vsdq(next_ptr2) := Bool(true)
  }

  when (io.fire.vst)
  {
    for(i <- 0 until SZ_BANK)
      next_dep_vaq(i) := Bool(false)
    next_dep_vaq(next_ptr1) := Bool(false)
    next_dep_vldq(next_ptr1) := Bool(true)
    next_dep_vsdq(next_ptr1) := Bool(true)
    
    for(i <- 0 until SZ_BANK)
      next_dep_vsdq(i) := Bool(false)
    next_dep_vaq(next_ptr2) := Bool(false)
    next_dep_vldq(next_ptr2) := Bool(true)
    next_dep_vsdq(next_ptr2) := Bool(false)
  }

  val bcnt_mask = MuxLookup(
    io.issue_to_seq.bcnt, Bits(0,SZ_BANK), Array(
      Bits(1) -> Bits("b0000_0001",8),
      Bits(2) -> Bits("b0000_0011",8),
      Bits(3) -> Bits("b0000_0111",8),
      Bits(4) -> Bits("b0000_1111",8),
      Bits(5) -> Bits("b0001_1111",8),
      Bits(6) -> Bits("b0011_1111",8),
      Bits(7) -> Bits("b0111_1111",8),
      Bits(8) -> Bits("b1111_1111",8)
    ))
  val mask = array_mask(reg_ptr) & bcnt_mask

  var pop_count = Bits(0,SZ_BPTR)
  for(i <- 0 until SZ_BANK) pop_count = pop_count + mask(i)
  val skip = !mask.orR()

  val current_val = array_val(reg_ptr)
  val current_vaq_val = current_val & array_vaq(reg_ptr)
  val current_vldq_val = current_val & array_vldq(reg_ptr)
  val current_vsdq_val = current_val & array_vsdq(reg_ptr)

  val reg_vaq_stall = Reg(resetVal = Bool(false))
  val reg_vldq_stall = Reg(resetVal = Bool(false))
  val reg_vsdq_stall = Reg(resetVal = Bool(false))

  when (current_vaq_val) { reg_vaq_stall := io.qstall.vaq }
  when (current_vldq_val) { reg_vldq_stall := io.qstall.vldq }
  when (current_vsdq_val) { reg_vsdq_stall := io.qstall.vsdq }

  val masked_xcpt_stall = (!current_vldq_val && !current_vsdq_val) && io.xcpt_to_seq.stall

  val stall =
    masked_xcpt_stall |
    array_dep_vaq(reg_ptr) & reg_vaq_stall |
    array_dep_vldq(reg_ptr) & reg_vldq_stall |
    array_dep_vsdq(reg_ptr) & reg_vsdq_stall |
    current_vaq_val & io.qstall.vaq |
    current_vldq_val & io.qstall.vldq |
    current_vsdq_val & io.qstall.vsdq

  val reg_stall =
    current_vaq_val & reg_vaq_stall |
    current_vldq_val & reg_vldq_stall |
    current_vsdq_val & reg_vsdq_stall

  next_stall(reg_ptr) := stall

  io.seq_to_hazard.stall := (array_stall & array_val).orR

  io.seq_to_hazard.last := ~stall & current_val & array_last(reg_ptr)
  io.seq_to_expand.last := ~stall & current_val & array_last(reg_ptr)

  io.seq.viu := ~stall & current_val & array_viu(reg_ptr)
  io.seq.vau0 := ~stall & current_val & array_vau0(reg_ptr)
  io.seq.vau1 := ~stall & current_val & array_vau1(reg_ptr)
  io.seq.vau2 := ~stall & current_val & array_vau2(reg_ptr)
  io.seq.vaq := ~stall & current_vaq_val
  io.seq.vldq := ~stall & current_vldq_val
  io.seq.vsdq := ~stall & current_vsdq_val

  io.seq_fn.viu := array_fn_viu(reg_ptr)
  io.seq_fn.vau0 := array_fn_vau0(reg_ptr)
  io.seq_fn.vau1 := array_fn_vau1(reg_ptr)
  io.seq_fn.vau2 := array_fn_vau2(reg_ptr)

  io.seq_regid_imm.cnt := pop_count(SZ_LGBANK-1,0)
  io.seq_regid_imm.utidx := array_utidx(reg_ptr)
  io.seq_regid_imm.vs_zero := array_vs_zero(reg_ptr)
  io.seq_regid_imm.vt_zero := array_vt_zero(reg_ptr)
  io.seq_regid_imm.vr_zero := array_vr_zero(reg_ptr)
  io.seq_regid_imm.vs := array_vs(reg_ptr)
  io.seq_regid_imm.vt := array_vt(reg_ptr)
  io.seq_regid_imm.vr := array_vr(reg_ptr)
  io.seq_regid_imm.vd := array_vd(reg_ptr)
  io.seq_regid_imm.vm := array_vm(reg_ptr)
  io.seq_regid_imm.mem := array_mem(reg_ptr)
  io.seq_regid_imm.imm := array_imm(reg_ptr)
  io.seq_regid_imm.imm2 := array_imm2(reg_ptr)
  io.seq_regid_imm.utmemop := array_utmemop(reg_ptr)
  io.seq_regid_imm.mask := mask

  // looking for one cycle ahead
  io.qcntp1 := Mux(reg_stall, io.seq_regid_imm.cnt + UFix(1, SZ_QCNT), io.seq_regid_imm.cnt + UFix(2, SZ_QCNT))
  // looking for two cycles ahead
  io.qcntp2 := Mux(reg_stall, io.seq_regid_imm.cnt + UFix(1, SZ_QCNT), io.seq_regid_imm.cnt + UFix(3, SZ_QCNT))

  // aiw
  io.seq_to_aiw.update_imm1.valid := Bool(false)
  io.seq_to_aiw.update_cnt.valid := Bool(false)
  io.seq_to_aiw.last := Bool(false)
  io.seq_to_aiw.update_numCnt.valid := Bool(false)

  val seq_alu = io.seq.viu || io.seq.vau0 || io.seq.vau1 || io.seq.vau2
  val seq_mem = io.seq.vldq || io.seq.vsdq
  val seq_vmem = seq_mem && !array_utmemop(reg_ptr)
  val seq_utmem = seq_mem && array_utmemop(reg_ptr)
  
  when (seq_alu || seq_mem)
  {
    io.seq_to_aiw.update_cnt.valid := array_aiw_update_numCnt(reg_ptr)
    next_aiw_cnt(reg_ptr) := io.seq_to_aiw.update_cnt.bits.data

    when (array_last(reg_ptr))
    {
      io.seq_to_aiw.last := array_aiw_update_numCnt(reg_ptr)
      io.seq_to_aiw.update_numCnt.valid := array_aiw_update_numCnt(reg_ptr)
    }
  }

  when (seq_alu || seq_utmem)
  {
    when (array_last(reg_ptr))
    {
      io.seq_to_aiw.update_imm1.valid := array_aiw_update_imm1(reg_ptr)
    }
  } 
  .elsewhen (seq_vmem)
  {
    io.seq_to_aiw.update_imm1.valid := array_aiw_update_imm1(reg_ptr)
  }

  io.seq_to_aiw.update_imm1.bits.addr := array_aiw_imm1_rtag(reg_ptr).toUFix
  io.seq_to_aiw.update_imm1.bits.data := 
    Mux(io.seq.vldq || io.seq.vsdq, mem_base_plus_stride,
        array_aiw_pc_next(reg_ptr))

  io.seq_to_aiw.update_cnt.bits.addr := array_aiw_cnt_rtag(reg_ptr).toUFix
  io.seq_to_aiw.update_cnt.bits.data := array_aiw_cnt(reg_ptr) + next_vlen_update + UFix(1)

  io.seq_to_aiw.update_numCnt.bits := array_aiw_numCnt_rtag(reg_ptr)

  when (io.flush)
  {
    for(i <- 0 until SZ_BANK)
    {
      next_val(i) := Bool(false)
      next_stall(i) := Bool(false)
      next_last(i) := Bool(false)
      next_viu(i) := Bool(false)
      next_vau0(i) := Bool(false)
      next_vau1(i) := Bool(false)
      next_vau2(i) := Bool(false)
      next_vaq(i) := Bool(false)
      next_vldq(i) := Bool(false)
      next_vsdq(i) := Bool(false)
      next_utmemop(i) := Bool(false)
      next_aiw_update_imm1(i) := Bool(false)
      next_aiw_update_numCnt(i) := Bool(false)
    }

    reg_vaq_stall := Bool(false)
    reg_vldq_stall := Bool(false)
    reg_vsdq_stall := Bool(false)
  }
}

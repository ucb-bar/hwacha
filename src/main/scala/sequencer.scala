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
  val vlen = Bits(width = SZ_VLEN)
  val cnt = Bits(width = SZ_BVLEN)
  val tcnt = Bits(width = SZ_BVLEN) // turbo
  val utidx = Bits(width = SZ_VLEN)
  val vs_zero = Bool()
  val vt_zero = Bool()
  val vr_zero = Bool()
  val vs = Bits(width = SZ_BREGLEN)
  val vt = Bits(width = SZ_BREGLEN)
  val vr = Bits(width = SZ_BREGLEN)
  val vd = Bits(width = SZ_BREGLEN)
  val rtype = Bits(width = 4)
  val vm = Bits(width = SZ_BMASK)
  val mem = new io_vxu_mem_cmd()
  val imm = Bits(width = SZ_DATA)
  val imm2 = Bits(width = SZ_XIMM2)
  val utmemop = Bool()
  val aiw = new io_vxu_aiw_bundle()
  val mask = Bits(width=SZ_BANK)
  val pvfb_tag = Bits(width=SZ_PVFB_TAG)
  val pop_count = UInt(width=SZ_LGBANK1)
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
  val update_imm1 = Valid(new io_aiwUpdateReq(SZ_VIMM, 3) )
  val update_cnt = Valid(new io_aiwUpdateReq(SZ_VLEN, 3) )
  val update_numCnt = new io_update_num_cnt()
}

class Sequencer(resetSignal: Bool = null) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val issue_to_seq = new io_vxu_issue_to_seq().asInput
    val seq_to_hazard = new io_vxu_seq_to_hazard().asOutput
    val seq_to_expand = new io_vxu_seq_to_expand().asOutput

    val qcntp1 = UInt(OUTPUT, SZ_QCNT)
    val qcntp2 = UInt(OUTPUT, SZ_QCNT)
    val qstall = new io_qstall().asInput

    val fire = new io_vxu_issue_fire().asInput
    val fire_fn = new io_vxu_issue_fn().asInput
    val fire_regid_imm = new io_vxu_issue_regid_imm().asInput

    val seq = new io_vxu_seq_fu().asOutput
    val seq_fn = new io_vxu_seq_fn().asOutput
    val seq_regid_imm = new io_vxu_seq_regid_imm().asOutput

    val seq_to_aiw = new io_seq_to_aiw()

    val xcpt_to_seq = new io_xcpt_handler_to_seq().flip()

    val prec = Bits(INPUT, SZ_PREC)
  }

  val bcntm1 = io.issue_to_seq.bcnt - UInt(1)

  val next_ptr1 = UInt(width=SZ_BPTR)
  val next_ptr2 = UInt(width=SZ_BPTR)
  val next_ptr3 = UInt(width=SZ_BPTR)

  val reg_ptr = Reg(next = next_ptr1, init = UInt(0, SZ_LGBANK))

  val next_ptr1_add = reg_ptr + UInt(1, SZ_LGBANK1)
  val next_ptr2_add = reg_ptr + UInt(2, SZ_LGBANK1)
  val next_ptr3_add = reg_ptr + UInt(3, SZ_LGBANK1)

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

  val next_val = Vec.fill(SZ_BANK){Bool()}
  val next_stall = Vec.fill(SZ_BANK){Bool()}
  val next_last = Vec.fill(SZ_BANK){Bool()}
  val next_viu = Vec.fill(SZ_BANK){Bool()}
  val next_vau0 = Vec.fill(SZ_BANK){Bool()}
  val next_vau1 = Vec.fill(SZ_BANK){Bool()}
  val next_vau2 = Vec.fill(SZ_BANK){Bool()}
  val next_vaq = Vec.fill(SZ_BANK){Bool()}
  val next_vldq = Vec.fill(SZ_BANK){Bool()}
  val next_vsdq = Vec.fill(SZ_BANK){Bool()}
  val next_utmemop = Vec.fill(SZ_BANK){Bool()}

  val next_fn_viu = Vec.fill(8){Bits(width=SZ_VIU_FN)}
  val next_fn_vau0 = Vec.fill(8){Bits(width=SZ_VAU0_FN)}
  val next_fn_vau1 = Vec.fill(8){Bits(width=SZ_VAU1_FN)}
  val next_fn_vau2 = Vec.fill(8){Bits(width=SZ_VAU2_FN)}
  val next_vlen = Vec.fill(8){Bits(width=SZ_VLEN)}
  val next_utidx = Vec.fill(8){Bits(width=SZ_VLEN)}
  val next_xstride = Vec.fill(8){Bits(width=SZ_REGLEN)}
  val next_fstride = Vec.fill(8){Bits(width=SZ_REGLEN)}
  val next_vs_zero = Vec.fill(SZ_BANK){Bool()}
  val next_vt_zero = Vec.fill(SZ_BANK){Bool()}
  val next_vr_zero = Vec.fill(SZ_BANK){Bool()}
  val next_vd_zero = Vec.fill(SZ_BANK){Bool()}
  val next_vs = Vec.fill(8){Bits(width=SZ_BREGLEN)}
  val next_vt = Vec.fill(8){Bits(width=SZ_BREGLEN)}
  val next_vr = Vec.fill(8){Bits(width=SZ_BREGLEN)}
  val next_vd = Vec.fill(8){Bits(width=SZ_BREGLEN)}
  val next_rtype = Vec.fill(8){Bits(width=4)}
  val next_vm = Vec.fill(8){Bits(width=SZ_BMASK)}
  val next_mem = Vec.fill(8){new io_vxu_mem_cmd()}

  val next_imm = Vec.fill(8){Bits(width=SZ_DATA)}
  val next_imm2 = Vec.fill(8){Bits(width=SZ_XIMM2)}
  val next_aiw_imm1_rtag = Vec.fill(SZ_BANK){Bits(width=SZ_AIW_IMM1)}
  val next_aiw_cnt_rtag = Vec.fill(SZ_BANK){Bits(width=SZ_AIW_CNT)}
  val next_aiw_numCnt_rtag = Vec.fill(SZ_BANK){Bits(width=SZ_AIW_NUMCNT)}
  val next_aiw_cnt = Vec.fill(SZ_BANK){Bits(width=SZ_VLEN)}
  val next_aiw_pc_next = Vec.fill(SZ_BANK){Bits(width=SZ_ADDR)}
  val next_aiw_update_imm1 = Vec.fill(SZ_BANK){Bool()}
  val next_aiw_update_numCnt = Vec.fill(SZ_BANK){Bool()}

  val next_pvfb_tag = Vec.fill(SZ_BANK){Bits(width=SZ_PVFB_TAG)}
  val next_active_mask = Vec.fill(SZ_BANK){Bool()}
  val next_mask = Vec.fill(SZ_BANK){Bits(width=WIDTH_PVFB)}

  val array_val = Reg(init=Bits(0, SZ_BANK))
  val array_stall = Reg(init=Bits(0, SZ_BANK))
  val array_last = Reg(init=Bits(0, SZ_BANK))
  val array_viu = Reg(init=Bits(0, SZ_BANK))
  val array_vau0 = Reg(init=Bits(0, SZ_BANK))
  val array_vau1 = Reg(init=Bits(0, SZ_BANK))
  val array_vau2 = Reg(init=Bits(0, SZ_BANK))
  val array_vaq = Reg(init=Bits(0, SZ_BANK))
  val array_vldq = Reg(init=Bits(0, SZ_BANK))
  val array_vsdq = Reg(init=Bits(0, SZ_BANK))
  val array_utmemop = Reg(init=Bits(0, SZ_BANK))

  val array_fn_viu = Vec.fill(8){Reg(Bits(width=SZ_VIU_FN))}
  val array_fn_vau0 = Vec.fill(8){Reg(Bits(width=SZ_VAU0_FN))}
  val array_fn_vau1 = Vec.fill(8){Reg(Bits(width=SZ_VAU1_FN))}
  val array_fn_vau2 = Vec.fill(8){Reg(Bits(width=SZ_VAU2_FN))}
  val array_vlen = Vec.fill(8){Reg(Bits(width=SZ_VLEN), init = UInt(0, SZ_VLEN))}
  val array_utidx = Vec.fill(8){Reg(Bits(width=SZ_VLEN))}
  val array_xstride = Vec.fill(8){Reg(Bits(width=SZ_REGLEN))}
  val array_fstride = Vec.fill(8){Reg(Bits(width=SZ_REGLEN))}
  val array_vs_zero = Vec.fill(SZ_BANK){Reg(Bool())}
  val array_vt_zero = Vec.fill(SZ_BANK){Reg(Bool())}
  val array_vr_zero = Vec.fill(SZ_BANK){Reg(Bool())}
  val array_vd_zero = Vec.fill(SZ_BANK){Reg(Bool())}
  val array_vs = Vec.fill(8){Reg(Bits(width=SZ_BREGLEN))}
  val array_vt = Vec.fill(8){Reg(Bits(width=SZ_BREGLEN))}
  val array_vr = Vec.fill(8){Reg(Bits(width=SZ_BREGLEN))}
  val array_vd = Vec.fill(8){Reg(Bits(width=SZ_BREGLEN))}
  val array_vm = Vec.fill(8){Reg(Bits(width=SZ_BMASK))}
  val array_rtype = Vec.fill(8){Reg(Bits(width=4), init = Bits(0, 4))}
  val array_mem = Vec.fill(8){Reg(new io_vxu_mem_cmd())}

  val array_imm = Vec.fill(8){Reg(Bits(width=SZ_DATA))}
  val array_imm2 = Vec.fill(8){Reg(Bits(width=SZ_XIMM2))}
  val array_aiw_imm1_rtag = Vec.fill(SZ_BANK){Reg(Bits(width=SZ_AIW_IMM1))}
  val array_aiw_cnt_rtag = Vec.fill(SZ_BANK){Reg(Bits(width=SZ_AIW_CNT))}
  val array_aiw_numCnt_rtag = Vec.fill(SZ_BANK){Reg(Bits(width=SZ_AIW_NUMCNT))}
  val array_aiw_cnt = Vec.fill(SZ_BANK){Reg(Bits(width=SZ_VLEN))}
  val array_aiw_pc_next = Vec.fill(SZ_BANK){Reg(Bits(width=SZ_ADDR))}
  val array_aiw_update_imm1 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_aiw_update_numCnt = Vec.fill(SZ_BANK){Reg(init=Bool(false))}

  val array_pvfb_tag = Vec.fill(SZ_BANK){Reg(Bits(width=SZ_PVFB_TAG))}
  val array_active_mask = Vec.fill(SZ_BANK){Reg(init=Bool(false) )}
  val array_mask = Vec.fill(SZ_BANK){Reg(Bits(width=WIDTH_PVFB))}

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
  array_xstride := next_xstride
  array_fstride := next_fstride
  array_vs_zero := next_vs_zero
  array_vt_zero := next_vt_zero
  array_vr_zero := next_vr_zero
  array_vd_zero := next_vd_zero
  array_vs := next_vs
  array_vt := next_vt
  array_vr := next_vr
  array_vd := next_vd
  array_vm := next_vm
  array_rtype := next_rtype
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

  array_pvfb_tag := next_pvfb_tag
  array_active_mask := next_active_mask
  array_mask := next_mask

  val last = io.fire_regid_imm.vlen < io.issue_to_seq.bcnt

  val turbo_last = MuxLookup(io.prec, io.fire_regid_imm.vlen < io.issue_to_seq.bcnt, Array(
    PREC_DOUBLE -> (io.fire_regid_imm.vlen < io.issue_to_seq.bcnt),
    PREC_SINGLE -> ((io.fire_regid_imm.vlen >> UInt(1)) < io.issue_to_seq.bcnt),
    PREC_HALF -> ((io.fire_regid_imm.vlen >> UInt(2)) < io.issue_to_seq.bcnt)
  ))

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
  next_xstride := array_xstride
  next_fstride := array_fstride
  next_vs_zero := array_vs_zero
  next_vt_zero := array_vt_zero
  next_vr_zero := array_vr_zero
  next_vd_zero := array_vd_zero
  next_vs := array_vs
  next_vt := array_vt
  next_vr := array_vr
  next_vd := array_vd
  next_rtype := array_rtype
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

  next_pvfb_tag := array_pvfb_tag
  next_active_mask := array_active_mask
  next_mask := array_mask

  when (io.fire.viu)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_viu(next_ptr1) := Bool(true)
    next_fn_viu(next_ptr1) := io.fire_fn.viu
    next_vlen(next_ptr1) := io.fire_regid_imm.vlen
    next_utidx(next_ptr1) := io.fire_regid_imm.utidx
    next_xstride(next_ptr1) := io.issue_to_seq.xstride
    next_fstride(next_ptr1) := io.issue_to_seq.fstride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vt_zero(next_ptr1) := io.fire_regid_imm.vt_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_vt(next_ptr1) := io.fire_regid_imm.vt
    next_vd(next_ptr1) := io.fire_regid_imm.vd
    next_rtype(next_ptr1) := io.fire_regid_imm.rtype
    next_vm(next_ptr1) := io.fire_regid_imm.vm
    next_imm(next_ptr1) := io.fire_regid_imm.imm

    next_aiw_imm1_rtag(next_ptr1) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr1) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr1) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr1) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr1) := Bool(true)

    next_pvfb_tag(next_ptr1) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr1) := io.fire_regid_imm.active_mask
    next_mask(next_ptr1) := io.fire_regid_imm.mask
  }

  when (io.fire.vau0)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vau0(next_ptr1) := Bool(true)
    next_fn_vau0(next_ptr1) := io.fire_fn.vau0
    next_vlen(next_ptr1) := io.fire_regid_imm.vlen
    next_xstride(next_ptr1) := io.issue_to_seq.xstride
    next_fstride(next_ptr1) := io.issue_to_seq.fstride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vt_zero(next_ptr1) := io.fire_regid_imm.vt_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_vt(next_ptr1) := io.fire_regid_imm.vt
    next_vd(next_ptr1) := io.fire_regid_imm.vd
    next_rtype(next_ptr1) := io.fire_regid_imm.rtype

    next_aiw_imm1_rtag(next_ptr1) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr1) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr1) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr1) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr1) := Bool(true)

    next_pvfb_tag(next_ptr1) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr1) := io.fire_regid_imm.active_mask
    next_mask(next_ptr1) := io.fire_regid_imm.mask
  }

  when (io.fire.vau1)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := turbo_last
    next_vau1(next_ptr1) := Bool(true)
    next_fn_vau1(next_ptr1) := io.fire_fn.vau1
    next_vlen(next_ptr1) := io.fire_regid_imm.vlen
    next_xstride(next_ptr1) := io.issue_to_seq.xstride
    next_fstride(next_ptr1) := io.issue_to_seq.fstride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vt_zero(next_ptr1) := io.fire_regid_imm.vt_zero
    next_vr_zero(next_ptr1) := io.fire_regid_imm.vr_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_vt(next_ptr1) := io.fire_regid_imm.vt
    next_vr(next_ptr1) := io.fire_regid_imm.vr
    next_vd(next_ptr1) := io.fire_regid_imm.vd
    next_rtype(next_ptr1) := io.fire_regid_imm.rtype

    next_aiw_imm1_rtag(next_ptr1) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr1) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr1) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr1) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr1) := Bool(true)

    next_pvfb_tag(next_ptr1) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr1) := io.fire_regid_imm.active_mask
    next_mask(next_ptr1) := io.fire_regid_imm.mask
  }

  when (io.fire.vau2)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vau2(next_ptr1) := Bool(true)
    next_fn_vau2(next_ptr1) := io.fire_fn.vau2
    next_vlen(next_ptr1) := io.fire_regid_imm.vlen
    next_xstride(next_ptr1) := io.issue_to_seq.xstride
    next_fstride(next_ptr1) := io.issue_to_seq.fstride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_vd(next_ptr1) := io.fire_regid_imm.vd
    next_rtype(next_ptr1) := io.fire_regid_imm.rtype

    next_aiw_imm1_rtag(next_ptr1) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr1) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr1) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr1) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr1) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr1) := Bool(true)

    next_pvfb_tag(next_ptr1) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr1) := io.fire_regid_imm.active_mask
    next_mask(next_ptr1) := io.fire_regid_imm.mask
  }

  when (io.fire.amo)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vaq(next_ptr1) := Bool(true)
    next_utmemop(next_ptr1) := Bool(true)
    next_vlen(next_ptr1) := io.fire_regid_imm.vlen
    next_xstride(next_ptr1) := io.issue_to_seq.xstride
    next_fstride(next_ptr1) := io.issue_to_seq.fstride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_rtype(next_ptr1) := io.fire_regid_imm.rtype
    next_mem(next_ptr1) := io.fire_regid_imm.mem
    // should always write 0, amo's don't take immediates
    next_imm(next_ptr1) := Bits(0)

    next_pvfb_tag(next_ptr1) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr1) := io.fire_regid_imm.active_mask
    next_mask(next_ptr1) := io.fire_regid_imm.mask

    next_val(next_ptr2) := Bool(true)
    next_last(next_ptr2) := last
    next_vsdq(next_ptr2) := Bool(true)
    next_utmemop(next_ptr2) := Bool(true)
    next_vlen(next_ptr2) := io.fire_regid_imm.vlen
    next_xstride(next_ptr2) := io.issue_to_seq.xstride
    next_fstride(next_ptr2) := io.issue_to_seq.fstride
    next_vt_zero(next_ptr2) := io.fire_regid_imm.vt_zero
    next_vt(next_ptr2) := io.fire_regid_imm.vt
    next_rtype(next_ptr2) := io.fire_regid_imm.rtype
    next_mem(next_ptr2) := io.fire_regid_imm.mem

    next_pvfb_tag(next_ptr2) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr2) := io.fire_regid_imm.active_mask
    next_mask(next_ptr2) := io.fire_regid_imm.mask

    next_val(next_ptr3) := Bool(true)
    next_last(next_ptr3) := last
    next_vldq(next_ptr3) := Bool(true)
    next_utmemop(next_ptr3) := Bool(true)
    next_vlen(next_ptr3) := io.fire_regid_imm.vlen
    next_xstride(next_ptr3) := io.issue_to_seq.xstride
    next_fstride(next_ptr3) := io.issue_to_seq.fstride
    next_vd(next_ptr3) := io.fire_regid_imm.vd
    next_rtype(next_ptr3) := io.fire_regid_imm.rtype

    next_aiw_imm1_rtag(next_ptr3) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr3) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr3) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr3) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr3) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr3) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr3) := Bool(true)

    next_pvfb_tag(next_ptr3) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr3) := io.fire_regid_imm.active_mask
    next_mask(next_ptr3) := io.fire_regid_imm.mask
  }

  when (io.fire.utld)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vaq(next_ptr1) := Bool(true)
    next_utmemop(next_ptr1) := Bool(true)
    next_vlen(next_ptr1) := io.fire_regid_imm.vlen
    next_xstride(next_ptr1) := io.issue_to_seq.xstride
    next_fstride(next_ptr1) := io.issue_to_seq.fstride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_rtype(next_ptr1) := io.fire_regid_imm.rtype
    next_mem(next_ptr1) := io.fire_regid_imm.mem
    next_imm(next_ptr1) := io.fire_regid_imm.imm

    next_pvfb_tag(next_ptr1) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr1) := io.fire_regid_imm.active_mask
    next_mask(next_ptr1) := io.fire_regid_imm.mask

    next_val(next_ptr2) := Bool(true)
    next_last(next_ptr2) := turbo_last
    next_vldq(next_ptr2) := Bool(true)
    next_utmemop(next_ptr2) := Bool(true)
    next_vlen(next_ptr2) := io.fire_regid_imm.vlen
    next_xstride(next_ptr2) := io.issue_to_seq.xstride
    next_fstride(next_ptr2) := io.issue_to_seq.fstride
    next_vd(next_ptr2) := io.fire_regid_imm.vd
    next_rtype(next_ptr2) := io.fire_regid_imm.rtype

    next_aiw_imm1_rtag(next_ptr2) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr2) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr2) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr2) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr2) := Bool(true)

    next_pvfb_tag(next_ptr2) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr2) := io.fire_regid_imm.active_mask
    next_mask(next_ptr2) := io.fire_regid_imm.mask
  }

  when (io.fire.utst)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vaq(next_ptr1) := Bool(true)
    next_utmemop(next_ptr1) := Bool(true)
    next_vlen(next_ptr1) := io.fire_regid_imm.vlen
    next_xstride(next_ptr1) := io.issue_to_seq.xstride
    next_fstride(next_ptr1) := io.issue_to_seq.fstride
    next_vs_zero(next_ptr1) := io.fire_regid_imm.vs_zero
    next_vs(next_ptr1) := io.fire_regid_imm.vs
    next_rtype(next_ptr1) := io.fire_regid_imm.rtype
    next_mem(next_ptr1) := io.fire_regid_imm.mem
    next_imm(next_ptr1) := io.fire_regid_imm.imm

    next_pvfb_tag(next_ptr1) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr1) := io.fire_regid_imm.active_mask
    next_mask(next_ptr1) := io.fire_regid_imm.mask

    next_val(next_ptr2) := Bool(true)
    next_last(next_ptr2) := turbo_last
    next_vsdq(next_ptr2) := Bool(true)
    next_utmemop(next_ptr2) := Bool(true)
    next_vlen(next_ptr2) := io.fire_regid_imm.vlen
    next_xstride(next_ptr2) := io.issue_to_seq.xstride
    next_fstride(next_ptr2) := io.issue_to_seq.fstride
    next_vt_zero(next_ptr2) := io.fire_regid_imm.vt_zero
    next_vt(next_ptr2) := io.fire_regid_imm.vt
    next_rtype(next_ptr2) := io.fire_regid_imm.rtype
    next_mem(next_ptr2) := io.fire_regid_imm.mem  

    next_aiw_imm1_rtag(next_ptr2) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr2) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr2) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr2) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr2) := Bool(true)

    next_pvfb_tag(next_ptr2) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr2) := io.fire_regid_imm.active_mask
    next_mask(next_ptr2) := io.fire_regid_imm.mask
  }

  when (io.fire.vld)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vaq(next_ptr1) := Bool(true)
    next_vlen(next_ptr1) := io.fire_regid_imm.vlen
    next_xstride(next_ptr1) := io.issue_to_seq.xstride
    next_fstride(next_ptr1) := io.issue_to_seq.fstride
    next_mem(next_ptr1) := io.fire_regid_imm.mem
    next_imm(next_ptr1) := Cat(Bits(0,1), io.fire_regid_imm.imm(63,0))
    next_imm2(next_ptr1) := io.fire_regid_imm.imm2

    next_pvfb_tag(next_ptr1) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr1) := io.fire_regid_imm.active_mask
    next_mask(next_ptr1) := io.fire_regid_imm.mask

    next_val(next_ptr2) := Bool(true)
    next_last(next_ptr2) := last
    next_vldq(next_ptr2) := Bool(true)
    next_vlen(next_ptr2) := io.fire_regid_imm.vlen
    next_xstride(next_ptr2) := io.issue_to_seq.xstride
    next_fstride(next_ptr2) := io.issue_to_seq.fstride
    next_vd(next_ptr2) := io.fire_regid_imm.vd
    next_rtype(next_ptr2) := io.fire_regid_imm.rtype
    next_imm(next_ptr2) := Cat(Bits(0,1), io.fire_regid_imm.imm(63,0))
    next_imm2(next_ptr2) := io.fire_regid_imm.imm2

    next_aiw_imm1_rtag(next_ptr2) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr2) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr2) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr2) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr2) := Bool(true)

    next_pvfb_tag(next_ptr2) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr2) := io.fire_regid_imm.active_mask
    next_mask(next_ptr2) := io.fire_regid_imm.mask
  }

  when (io.fire.vst)
  {
    next_val(next_ptr1) := Bool(true)
    next_last(next_ptr1) := last
    next_vaq(next_ptr1) := Bool(true)
    next_vlen(next_ptr1) := io.fire_regid_imm.vlen
    next_xstride(next_ptr1) := io.issue_to_seq.xstride
    next_fstride(next_ptr1) := io.issue_to_seq.fstride
    next_mem(next_ptr1) := io.fire_regid_imm.mem 
    next_imm(next_ptr1) := Cat(Bits(0,1), io.fire_regid_imm.imm(63,0))
    next_imm2(next_ptr1) := io.fire_regid_imm.imm2

    next_pvfb_tag(next_ptr1) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr1) := io.fire_regid_imm.active_mask
    next_mask(next_ptr1) := io.fire_regid_imm.mask

    next_val(next_ptr2) := Bool(true)
    next_last(next_ptr2) := last
    next_vsdq(next_ptr2) := Bool(true)
    next_vlen(next_ptr2) := io.fire_regid_imm.vlen
    next_xstride(next_ptr2) := io.issue_to_seq.xstride
    next_fstride(next_ptr2) := io.issue_to_seq.fstride
    next_vt_zero(next_ptr2) := io.fire_regid_imm.vt_zero
    next_vt(next_ptr2) := io.fire_regid_imm.vt
    next_mem(next_ptr2) := io.fire_regid_imm.mem
    next_imm(next_ptr2) := Cat(Bits(0,1), io.fire_regid_imm.imm(63,0))
    next_imm2(next_ptr2) := io.fire_regid_imm.imm2
    next_rtype(next_ptr2) := io.fire_regid_imm.rtype

    next_aiw_imm1_rtag(next_ptr2) := io.fire_regid_imm.aiw.imm1_rtag
    next_aiw_cnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.cnt_rtag
    next_aiw_numCnt_rtag(next_ptr2) := io.fire_regid_imm.aiw.numCnt_rtag
    next_aiw_cnt(next_ptr2) := io.fire_regid_imm.cnt
    next_aiw_pc_next(next_ptr2) := io.fire_regid_imm.aiw.pc_next
    next_aiw_update_imm1(next_ptr2) := io.fire_regid_imm.aiw.update_imm1
    next_aiw_update_numCnt(next_ptr2) := Bool(true)

    next_pvfb_tag(next_ptr2) := io.fire_regid_imm.pvfb_tag
    next_active_mask(next_ptr2) := io.fire_regid_imm.active_mask
    next_mask(next_ptr2) := io.fire_regid_imm.mask
  }

  val next_vlen_update = UInt(width = SZ_LGBANK + 2)

  next_vlen_update := Mux(array_vlen(reg_ptr) < bcntm1, array_vlen(reg_ptr)(SZ_LGBANK-1,0), bcntm1(SZ_LGBANK-1,0))

  // increase throughput when certain operations (FMA, VLDQ, VSDQ) are used by
  // allowing up to 32 elements to proceed in half precision
  // allowing up to 16 elements to proceed in single precision
  val turbo_capable = io.seq.vau1 || io.seq.vldq || io.seq.vsdq

  when (turbo_capable)
  {
    when (io.prec === PREC_SINGLE)
    {
      val bcntx2p1 = (bcntm1(SZ_LGBANK-1,0) << UInt(1)) + UInt(1)
      next_vlen_update := Mux(array_vlen(reg_ptr) < bcntx2p1, array_vlen(reg_ptr), bcntx2p1)
    }
    .elsewhen (io.prec === PREC_HALF)
    {
      val bcntx4p3 = (bcntm1(SZ_LGBANK-1,0) << UInt(2)) + UInt(3)
      next_vlen_update := Mux(array_vlen(reg_ptr) < bcntx4p3, array_vlen(reg_ptr), bcntx4p3)
    }
  }

  when (io.seq.viu || io.seq.vau0 || io.seq.vau1 || io.seq.vau2 || io.seq.vaq || io.seq.vldq || io.seq.vsdq)
  {
    next_vlen(reg_ptr) := array_vlen(reg_ptr) - next_vlen_update - UInt(1) // WHY?!
    next_utidx(reg_ptr) := array_utidx(reg_ptr) + io.issue_to_seq.bcnt
    // rtype: vs vt vr vd
    next_vs(reg_ptr) := array_vs(reg_ptr) + Mux(array_rtype(reg_ptr)(3), array_fstride(reg_ptr), array_xstride(reg_ptr))
    next_vt(reg_ptr) := array_vt(reg_ptr) + Mux(array_rtype(reg_ptr)(2), array_fstride(reg_ptr), array_xstride(reg_ptr))
    next_vr(reg_ptr) := array_vr(reg_ptr) + Mux(array_rtype(reg_ptr)(1), array_fstride(reg_ptr), array_xstride(reg_ptr))
    next_vd(reg_ptr) := array_vd(reg_ptr) + Mux(array_rtype(reg_ptr)(0), array_fstride(reg_ptr), array_xstride(reg_ptr))
    next_vm(reg_ptr) := array_vm(reg_ptr) + UInt(1)
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

      next_active_mask(reg_ptr) := Bool(false)
    }
    .otherwise
    {
      when (next_vlen(reg_ptr) < io.issue_to_seq.bcnt)
      {
        next_last(reg_ptr) :=  Bool(true)
      }
      .elsewhen (turbo_capable && io.prec === PREC_SINGLE &&
                ((next_vlen(reg_ptr) >> UInt(1)) < io.issue_to_seq.bcnt)) // do NOT ceil
      {
        next_last(reg_ptr) :=  Bool(true)
      }
      .elsewhen (turbo_capable && io.prec === PREC_HALF &&
                ((next_vlen(reg_ptr) >> UInt(2)) < io.issue_to_seq.bcnt))
      {
        next_last(reg_ptr) :=  Bool(true)
      }
    }

  }

  val mem_base_plus_stride = array_imm(reg_ptr) + (array_imm2(reg_ptr) << UInt(3))

  when ((io.seq.vaq || io.seq.vldq || io.seq.vsdq) && !io.seq_regid_imm.utmemop)
  {
    next_imm(reg_ptr) := mem_base_plus_stride
  }

  val next_dep_vaq = Vec.fill(SZ_BANK){Bool()}
  val next_dep_vldq = Vec.fill(SZ_BANK){Bool()}
  val next_dep_vsdq = Vec.fill(SZ_BANK){Bool()}

  val array_dep_vaq = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_dep_vldq = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_dep_vsdq = Vec.fill(SZ_BANK){Reg(init=Bool(false))}

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

  val mask_sel = Mux(array_last(reg_ptr), array_vlen(reg_ptr) + UInt(1), io.issue_to_seq.bcnt)
  val bcnt_mask = MuxLookup(
    mask_sel, Bits(0,SZ_BANK), Array(
      Bits(1) -> Bits("b0000_0001",8),
      Bits(2) -> Bits("b0000_0011",8),
      Bits(3) -> Bits("b0000_0111",8),
      Bits(4) -> Bits("b0000_1111",8),
      Bits(5) -> Bits("b0001_1111",8),
      Bits(6) -> Bits("b0011_1111",8),
      Bits(7) -> Bits("b0111_1111",8),
      Bits(8) -> Bits("b1111_1111",8)
    ))
  val mask = (array_mask(reg_ptr) & bcnt_mask) | (Fill(SZ_BANK, ~array_active_mask(reg_ptr)) & bcnt_mask)

  val current_val = array_val(reg_ptr)
  val current_vaq_val = current_val & array_vaq(reg_ptr)
  val current_vldq_val = current_val & array_vldq(reg_ptr)
  val current_vsdq_val = current_val & array_vsdq(reg_ptr)

  val pop_count = if (HAVE_PVFB) 
    PopCount(mask)(SZ_LGBANK1-1,0)
  else
    // if v[sl]dq, use tcnt
    Mux(current_vldq_val || current_vsdq_val,
      io.seq_regid_imm.tcnt + UInt(1, SZ_LGBANK1),
      io.seq_regid_imm.cnt + UInt(1, SZ_LGBANK1))

  io.seq_regid_imm.pop_count := pop_count

  val reg_vaq_stall = Reg(init=Bool(false))
  val reg_vldq_stall = Reg(init=Bool(false))
  val reg_vsdq_stall = Reg(init=Bool(false))

  val masked_vaq_stall = array_dep_vaq(reg_ptr) & reg_vaq_stall
  val masked_vldq_stall = array_dep_vldq(reg_ptr) & reg_vldq_stall
  val masked_vsdq_stall = array_dep_vsdq(reg_ptr) & reg_vsdq_stall

  when (current_vaq_val & !masked_vldq_stall & !masked_vsdq_stall) { reg_vaq_stall := io.qstall.vaq }
  when (current_vldq_val & !masked_vaq_stall & !masked_vsdq_stall) { reg_vldq_stall := io.qstall.vldq }
  when (current_vsdq_val & !masked_vldq_stall & !masked_vsdq_stall) { reg_vsdq_stall := io.qstall.vsdq }

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

  io.seq_regid_imm.cnt := 
    Mux(array_vlen(reg_ptr) < bcntm1, array_vlen(reg_ptr)(SZ_LGBANK-1,0),
        bcntm1(SZ_LGBANK-1,0))

  io.seq_regid_imm.tcnt := io.seq_regid_imm.cnt

  def div2ceil(x: UInt) = (x + UInt(1)) >> UInt(1)
  def div4ceil(x: UInt) = (x + UInt(3)) >> UInt(2)

  // set turbo count
  // d[24]cm1 = divided by {2,4}, ceiling, minus 1
  when (io.prec === PREC_SINGLE) {
    val vlen_d2cm1 = div2ceil(array_vlen(reg_ptr)) - UInt(1)
    io.seq_regid_imm.tcnt := Mux(vlen_d2cm1 < bcntm1, vlen_d2cm1, bcntm1)
  } .elsewhen (io.prec === PREC_HALF) {
    val vlen_d4cm1 = div4ceil(array_vlen(reg_ptr)) - UInt(1)
    io.seq_regid_imm.tcnt := Mux(vlen_d4cm1 < bcntm1, vlen_d4cm1, bcntm1)
  }

  io.seq_regid_imm.utidx := array_utidx(reg_ptr)
  io.seq_regid_imm.vs_zero := array_vs_zero(reg_ptr)
  io.seq_regid_imm.vt_zero := array_vt_zero(reg_ptr)
  io.seq_regid_imm.vr_zero := array_vr_zero(reg_ptr)
  io.seq_regid_imm.vs := array_vs(reg_ptr)
  io.seq_regid_imm.vt := array_vt(reg_ptr)
  io.seq_regid_imm.vr := array_vr(reg_ptr)
  io.seq_regid_imm.vd := array_vd(reg_ptr)
  io.seq_regid_imm.rtype := array_rtype(reg_ptr)
  io.seq_regid_imm.vm := array_vm(reg_ptr)
  io.seq_regid_imm.mem := array_mem(reg_ptr)
  io.seq_regid_imm.imm := array_imm(reg_ptr)
  io.seq_regid_imm.imm2 := array_imm2(reg_ptr)
  io.seq_regid_imm.utmemop := array_utmemop(reg_ptr)
  io.seq_regid_imm.pvfb_tag := array_pvfb_tag(reg_ptr)
  if(HAVE_PVFB) io.seq_regid_imm.mask := mask

  // looking for one cycle ahead
  io.qcntp1 := Mux(reg_stall, pop_count, pop_count + UInt(1, SZ_QCNT))
  // looking for two cycles ahead
  io.qcntp2 := Mux(reg_stall, pop_count, pop_count + UInt(2, SZ_QCNT))

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

  io.seq_to_aiw.update_imm1.bits.addr := array_aiw_imm1_rtag(reg_ptr).toUInt
  io.seq_to_aiw.update_imm1.bits.data := 
    Mux(io.seq.vldq || io.seq.vsdq, mem_base_plus_stride,
        array_aiw_pc_next(reg_ptr))

  io.seq_to_aiw.update_cnt.bits.addr := array_aiw_cnt_rtag(reg_ptr).toUInt
  io.seq_to_aiw.update_cnt.bits.data := array_aiw_cnt(reg_ptr) + next_vlen_update + UInt(1)

  io.seq_to_aiw.update_numCnt.bits := array_aiw_numCnt_rtag(reg_ptr)
}

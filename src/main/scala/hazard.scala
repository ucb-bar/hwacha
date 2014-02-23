package hwacha

import Chisel._
import Node._
import Constants._

class Hazard(resetSignal: Bool = null)(implicit conf: HwachaConfiguration) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip

    val issue_to_hazard = new io_vxu_issue_to_hazard().asInput
    val seq_to_hazard = new io_vxu_seq_to_hazard().asInput
    val expand_to_hazard = new io_vxu_expand_to_hazard().asInput
    val lane_to_hazard = new io_lane_to_hazard().asInput

    val tvec_valid = new io_vxu_issue_fire().asInput
    val tvec_ready = Bool(OUTPUT)
    val tvec_dhazard = new io_vxu_issue_reg().asInput
    val tvec_shazard = new io_vxu_issue_fu().asInput
    val tvec_bhazard = new io_vxu_issue_op().asInput
    val tvec_fn = new io_vxu_issue_fn().asInput
    val tvec_regid_imm = new io_vxu_issue_regid_imm().asInput

    val vt_valid = new io_vxu_issue_fire().asInput
    val vt_ready = Bool(OUTPUT)
    val vt_dhazard = new io_vxu_issue_reg().asInput
    val vt_shazard = new io_vxu_issue_fu().asInput
    val vt_bhazard = new io_vxu_issue_op().asInput
    val vt_fn = new io_vxu_issue_fn().asInput
    val vt_regid_imm = new io_vxu_issue_regid_imm().asInput

    val fire = new io_vxu_issue_fire().asInput
    val fire_fn = new io_vxu_issue_fn().asInput
    val fire_regid_imm = new io_vxu_issue_regid_imm().asInput
    
    val pending_memop = Bool(OUTPUT)
  }

  val ptr = Reg(init = UInt(0, SZ_BPTR))
  val ptr1 = PtrIncr(ptr, 1, io.cfg.bcnt)
  val ptr2 = PtrIncr(ptr, 2, io.cfg.bcnt)
  val ptr3 = PtrIncr(ptr, 3, io.cfg.bcnt)
  val ptr4 = PtrIncr(ptr, 4, io.cfg.bcnt)
  val ptr5 = PtrIncr(ptr, 5, io.cfg.bcnt)
  ptr := ptr1

  val array_rport_val = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_rport_vau0 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_rport_vau1 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_rport_vau2 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_rport_vsu = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_rport_vgu = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  
  when (io.fire.viu) {
    array_rport_val(ptr2) := Bool(true)

    when (io.fire_fn.viu.rtype()) {
      array_rport_val(ptr3) := Bool(true)
    }
  }
  when (io.fire.vau0) {
    array_rport_val(ptr2) := Bool(true)
    array_rport_vau0(ptr2) := Bool(true)

    array_rport_val(ptr3) := Bool(true)
    array_rport_vau0(ptr3) := Bool(true)
  }
  when (io.fire.vau1) {
    array_rport_val(ptr2) := Bool(true)
    array_rport_vau1(ptr2) := Bool(true)

    array_rport_val(ptr3) := Bool(true)
    array_rport_vau1(ptr3) := Bool(true)

    when (io.fire_fn.vau1.fma()) {
      array_rport_val(ptr4) := Bool(true)
      array_rport_vau1(ptr4) := Bool(true)
    }
  }
  when (io.fire.vau2) {
    array_rport_val(ptr2) := Bool(true)
    array_rport_vau2(ptr2) := Bool(true)
  }
  when (io.fire.amo || io.fire.utst) {
    array_rport_val(ptr2) := Bool(true)
    array_rport_vgu(ptr2) := Bool(true)

    array_rport_val(ptr3) := Bool(true)
    array_rport_vsu(ptr3) := Bool(true)
  }
  when (io.fire.utld) {
    array_rport_val(ptr2) := Bool(true)
    array_rport_vgu(ptr2) := Bool(true)
  }
  when (io.fire.vld) {
    array_rport_val(ptr2) := Bool(true)
    array_rport_vgu(ptr2) := Bool(true)
  }
  when (io.fire.vst) {
    array_rport_val(ptr2) := Bool(true)
    array_rport_vgu(ptr2) := Bool(true)
    
    array_rport_val(ptr3) := Bool(true)
    array_rport_vsu(ptr3) := Bool(true)
  }

  when (io.lane_to_hazard.rlast) {
    array_rport_val(ptr) := Bool(false)
    array_rport_vau0(ptr) := Bool(false)
    array_rport_vau1(ptr) := Bool(false)
    array_rport_vau2(ptr) := Bool(false)
    array_rport_vsu(ptr) := Bool(false)
    array_rport_vgu(ptr) := Bool(false)
  }

  // I had to change this structure to the following in order to cut the
  // critical path.  I'm precomputing all possible write pointers and then
  // muxing them in later.

  // tvec wptr calculation

  val tvec_viu_incr = UInt(conf.int_stages + 1 + conf.delay_seq_exp, conf.ptr_incr_sz)

  val tvec_viu_wptr1 = PtrIncr(ptr, tvec_viu_incr, io.cfg.bcnt)
  val tvec_viu_wptr2 = PtrIncr(tvec_viu_wptr1, 1, io.cfg.bcnt)
  val tvec_viu_wptr = Mux(io.tvec_fn.viu.rtype(), tvec_viu_wptr2, tvec_viu_wptr1)

  // vt wptr calculation

  val vt_viu_incr = UInt(conf.int_stages + 1 + conf.delay_seq_exp, conf.ptr_incr_sz)
  val vt_vau0_incr = UInt(conf.imul_stages + 2 + conf.delay_seq_exp, conf.ptr_incr_sz)
  val vt_vau1_incr = UInt(conf.fma_stages + 2 + conf.delay_seq_exp, conf.ptr_incr_sz)
  val vt_vau2_incr = UInt(conf.fconv_stages + 1 + conf.delay_seq_exp, conf.ptr_incr_sz)

  val vt_viu_wptr1 = PtrIncr(ptr, vt_viu_incr, io.cfg.bcnt)
  val vt_viu_wptr2 = PtrIncr(vt_viu_wptr1, 1, io.cfg.bcnt)
  val vt_viu_wptr = Mux(io.vt_fn.viu.rtype(), vt_viu_wptr2, vt_viu_wptr1)
  val vt_vau0_wptr = PtrIncr(ptr, vt_vau0_incr, io.cfg.bcnt)
  val vt_vau1_wptr2 = PtrIncr(ptr, vt_vau1_incr, io.cfg.bcnt)
  val vt_vau1_wptr3 = PtrIncr(vt_vau1_wptr2, 1, io.cfg.bcnt)
  val vt_vau1_wptr = Mux(io.vt_fn.vau1.fma(), vt_vau1_wptr3, vt_vau1_wptr2)
  val vt_vau2_wptr = PtrIncr(ptr, vt_vau2_incr, io.cfg.bcnt)

  val vt_wptr = MuxCase(
    Bits(0,SZ_LGBANK), Array(
      io.vt_valid.viu -> vt_viu_wptr,
      io.vt_valid.vau0 -> vt_vau0_wptr,
      io.vt_valid.vau1 -> vt_vau1_wptr,
      io.vt_valid.vau2 -> vt_vau2_wptr
    ))

  // for the fire port
  // we can look at the issue port because we're firing at the same cycle

  val viu_wptr = Mux(io.tvec_valid.viu, tvec_viu_wptr, vt_viu_wptr)
  val vau0_wptr = vt_vau0_wptr
  val vau1_wptr = vt_vau1_wptr
  val vau2_wptr = vt_vau2_wptr

  val array_wport_val = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_wport_vau0 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_wport_vau1 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_wport_vau2 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_wport_vlu = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val array_wport_vd = Vec.fill(SZ_BANK){Reg(Bits(width = SZ_BREGLEN))}
  val array_wport_stride = Vec.fill(SZ_BANK){Reg(Bits(width = SZ_REGLEN))}

  when (io.fire.viu) {
    array_wport_val(viu_wptr) := Bool(true)
    array_wport_vd(viu_wptr) := io.fire_regid_imm.vd
    array_wport_stride(viu_wptr) := io.issue_to_hazard.stride
  }
  when (io.fire.vau0) {
    array_wport_val(vau0_wptr) := Bool(true)
    array_wport_vau0(vau0_wptr) := Bool(true)
    array_wport_vd(vau0_wptr) := io.fire_regid_imm.vd
    array_wport_stride(vau0_wptr) := io.issue_to_hazard.stride
  }
  when (io.fire.vau1) {
    array_wport_val(vau1_wptr) := Bool(true)
    array_wport_vau1(vau1_wptr) := Bool(true)
    array_wport_vd(vau1_wptr) := io.fire_regid_imm.vd
    array_wport_stride(vau1_wptr) := io.issue_to_hazard.stride
  }
  when (io.fire.vau2) {
    array_wport_val(vau2_wptr) := Bool(true)
    array_wport_vau2(vau2_wptr) := Bool(true)
    array_wport_vd(vau2_wptr) := io.fire_regid_imm.vd
    array_wport_stride(vau2_wptr) := io.issue_to_hazard.stride
  }
  when (io.fire.amo) {
    array_wport_val(ptr5) := Bool(true)
    array_wport_vlu(ptr5) := Bool(true)
    array_wport_vd(ptr5) := io.fire_regid_imm.vd
    array_wport_stride(ptr5) := io.issue_to_hazard.stride
  }
  when (io.fire.utld) {
    array_wport_val(ptr4) := Bool(true)
    array_wport_vlu(ptr4) := Bool(true)
    array_wport_vd(ptr4) := io.fire_regid_imm.vd
    array_wport_stride(ptr4) := io.issue_to_hazard.stride
  }
  when (io.fire.vld) {
    array_wport_val(ptr4) := Bool(true)
    array_wport_vlu(ptr4) := Bool(true)
    array_wport_vd(ptr4) := io.fire_regid_imm.vd
    array_wport_stride(ptr4) := io.issue_to_hazard.stride
  }

  when (io.expand_to_hazard.wen) {
    array_wport_vd(ptr) := array_wport_vd(ptr) + array_wport_stride(ptr)
  }

  when (io.lane_to_hazard.wlast) {
    array_wport_val(ptr) := Bool(false)
    array_wport_vau0(ptr) := Bool(false)
    array_wport_vau1(ptr) := Bool(false)
    array_wport_vau2(ptr) := Bool(false)
    array_wport_vlu(ptr) := Bool(false)
  }

  val array_sport_val = Vec.fill(SZ_BANK){Reg(init=Bool(false))}


  when (io.fire.viu || io.fire.vau0 || io.fire.vau1 || io.fire.vau2) {
    array_sport_val(ptr1) := Bool(true)
  }
  when (io.fire.amo) {
    array_sport_val(ptr1) := Bool(true)
    array_sport_val(ptr2) := Bool(true)
    array_sport_val(ptr3) := Bool(true)
  }
  when (io.fire.utld || io.fire.utst || io.fire.vld || io.fire.vst) {
    array_sport_val(ptr1) := Bool(true)
    array_sport_val(ptr2) := Bool(true)
  }
  
  when (io.seq_to_hazard.last) {
    array_sport_val(ptr) := Bool(false)
  }

  // hazard check logic for tvec/vt
  val shazard_vau0 = (array_rport_val.toBits & array_rport_vau0.toBits).orR | (array_wport_val.toBits & array_wport_vau0.toBits).orR
  val shazard_vau1 = (array_rport_val.toBits & array_rport_vau1.toBits).orR | (array_wport_val.toBits & array_wport_vau1.toBits).orR
  val shazard_vau2 = (array_rport_val.toBits & array_rport_vau2.toBits).orR | (array_wport_val.toBits & array_wport_vau2.toBits).orR
  val shazard_vgu = (array_rport_val.toBits & array_rport_vgu.toBits).orR
  val shazard_vlu = (array_wport_val.toBits & array_wport_vlu.toBits).orR
  val shazard_vsu = (array_rport_val.toBits & array_rport_vsu.toBits).orR

  val seqhazard_1slot = array_sport_val(ptr1)
  val seqhazard_2slot = array_sport_val(ptr1) | array_sport_val(ptr2)
  val seqhazard_3slot = array_sport_val(ptr1) | array_sport_val(ptr2) | array_sport_val(ptr3)

  // checking any pending memory ops for fences
  io.pending_memop := array_rport_vsu.toBits.orR || array_rport_vgu.toBits.orR || array_wport_vlu.toBits.orR

  // hazard check logic for tvec
  val tvec_dhazard_vt = array_wport_val.zip(array_wport_vd).map{ case (valid, vd) => valid && io.tvec_regid_imm.vt === vd }.reduce(_||_)
  val tvec_dhazard_vd = array_wport_val.zip(array_wport_vd).map{ case (valid, vd) => valid && io.tvec_regid_imm.vd === vd }.reduce(_||_)

  val tvec_bhazard_r1w1 = array_rport_val(ptr2) | array_wport_val(tvec_viu_wptr)
  val tvec_bhazard_vld = array_rport_val(ptr2) | array_wport_val(ptr4)
  val tvec_bhazard_vst = array_rport_val(ptr2) | array_rport_val(ptr3) // not sure about this 

  val tvec_dhazard = List(
      ~io.tvec_regid_imm.vt_zero & tvec_dhazard_vt & io.tvec_dhazard.vt & io.tvec_regid_imm.vt_active,
      tvec_dhazard_vd & io.tvec_dhazard.vd & io.tvec_regid_imm.vd_active
    ).reduce(_||_)

  val tvec_shazard = List(
      shazard_vgu & io.tvec_shazard.vgu,
      shazard_vlu & io.tvec_shazard.vlu,
      shazard_vsu & io.tvec_shazard.vsu
    ).reduce(_||_)

  val tvec_seqhazard = List(
      io.tvec_valid.viu & seqhazard_1slot,
      io.tvec_valid.vld & seqhazard_2slot,
      io.tvec_valid.vst & seqhazard_2slot
    ).reduce(_||_)

  val tvec_bhazard = List(
      tvec_bhazard_r1w1 & io.tvec_bhazard.r1w1,
      tvec_bhazard_vld & io.tvec_bhazard.vld,
      tvec_bhazard_vst & io.tvec_bhazard.vst
    ).reduce(_||_)

  io.tvec_ready := io.tvec_regid_imm.vd_zero || !io.seq_to_hazard.stall && !tvec_dhazard && !tvec_shazard && !tvec_seqhazard && !tvec_bhazard

  // hazard check logic for vt
  val vt_dhazard_vs = array_wport_val.zip(array_wport_vd).map{ case (valid,vd) => valid && io.vt_regid_imm.vs === vd }.reduce(_||_)
  val vt_dhazard_vt = array_wport_val.zip(array_wport_vd).map{ case (valid,vd) => valid && io.vt_regid_imm.vt === vd }.reduce(_||_)
  val vt_dhazard_vr = array_wport_val.zip(array_wport_vd).map{ case (valid,vd) => valid && io.vt_regid_imm.vr === vd }.reduce(_||_)
  val vt_dhazard_vd = array_wport_val.zip(array_wport_vd).map{ case (valid,vd) => valid && io.vt_regid_imm.vd === vd }.reduce(_||_)

  val vt_bhazard_r1w1 = array_rport_val(ptr2) | array_wport_val(vt_wptr)
  val vt_bhazard_r2w1 = array_rport_val(ptr2) | array_rport_val(ptr3) | array_wport_val(vt_wptr)
  val vt_bhazard_r3w1 = array_rport_val(ptr2) | array_rport_val(ptr3) | array_rport_val(ptr4) | array_wport_val(vt_wptr)
  val vt_bhazard_amo = array_rport_val(ptr2) | array_rport_val(ptr3) | array_wport_val(ptr5)
  val vt_bhazard_utld = array_rport_val(ptr2) | array_wport_val(ptr4)
  val vt_bhazard_utst = array_rport_val(ptr2) | array_rport_val(ptr3)

  val vt_dhazard = List(
      ~io.vt_regid_imm.vs_zero & vt_dhazard_vs & io.vt_dhazard.vs & io.vt_regid_imm.vs_active,
      ~io.vt_regid_imm.vt_zero & vt_dhazard_vt & io.vt_dhazard.vt & io.vt_regid_imm.vt_active,
      ~io.vt_regid_imm.vr_zero & vt_dhazard_vr & io.vt_dhazard.vr & io.vt_regid_imm.vr_active,
      vt_dhazard_vd & io.vt_dhazard.vd & io.vt_regid_imm.vd_active
    ).reduce(_||_)

  val vt_shazard = List(
      shazard_vau0 & io.vt_shazard.vau0,
      shazard_vau1 & io.vt_shazard.vau1,
      shazard_vau2 & io.vt_shazard.vau2,
      shazard_vgu & io.vt_shazard.vgu,
      shazard_vlu & io.vt_shazard.vlu,
      shazard_vsu & io.vt_shazard.vsu
    ).reduce(_||_)

  val vt_seqhazard = List(
      io.vt_valid.viu & seqhazard_1slot,
      io.vt_valid.vau0 & seqhazard_1slot,
      io.vt_valid.vau1 & seqhazard_1slot,
      io.vt_valid.vau2 & seqhazard_1slot,
      io.vt_valid.amo & seqhazard_3slot,
      io.vt_valid.utld & seqhazard_2slot,
      io.vt_valid.utst & seqhazard_2slot
    ).reduce(_||_)

  val vt_bhazard = List(
      vt_bhazard_r1w1 & io.vt_bhazard.r1w1,
      vt_bhazard_r2w1 & io.vt_bhazard.r2w1,
      vt_bhazard_r3w1 & io.vt_bhazard.r3w1,
      vt_bhazard_amo & io.vt_bhazard.amo,
      vt_bhazard_utld & io.vt_bhazard.utld,
      vt_bhazard_utst & io.vt_bhazard.utst
    ).reduce(_||_)

  io.vt_ready := io.vt_regid_imm.vd_zero || !io.seq_to_hazard.stall && !vt_dhazard && !vt_shazard && !vt_seqhazard && !vt_bhazard
}

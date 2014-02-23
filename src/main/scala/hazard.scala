package hwacha

import Chisel._
import Node._
import Constants._

class Hazard(resetSignal: Bool = null)(implicit conf: HwachaConfiguration) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip

    val seq_to_hazard = new io_vxu_seq_to_hazard().asInput
    val expand_to_hazard = new io_vxu_expand_to_hazard().asInput
    val lane_to_hazard = new io_lane_to_hazard().asInput

    val tvec = new Bundle {
      val active = Bool(INPUT)
      val ready = Bool(OUTPUT)
      val op = new IssueOpIO().flip
    }

    val vt = new Bundle {
      val ready = Bool(OUTPUT)
      val op = new IssueOpIO().flip
    }

    val issueop = new IssueOpIO
    
    val pending_memop = Bool(OUTPUT)
  }

  val ptr = Reg(init = UInt(0, SZ_BPTR))
  val ptr1 = PtrIncr(ptr, 1, io.cfg.bcnt)
  val ptr2 = PtrIncr(ptr, 2, io.cfg.bcnt)
  val ptr3 = PtrIncr(ptr, 3, io.cfg.bcnt)
  val ptr4 = PtrIncr(ptr, 4, io.cfg.bcnt)
  val ptr5 = PtrIncr(ptr, 5, io.cfg.bcnt)
  ptr := ptr1

  val viu_incr = UInt(conf.int_stages + 1 + conf.delay_seq_exp, conf.ptr_incr_sz)
  val vau0_incr = UInt(conf.imul_stages + 2 + conf.delay_seq_exp, conf.ptr_incr_sz)
  val vau1_incr = UInt(conf.fma_stages + 2 + conf.delay_seq_exp, conf.ptr_incr_sz)
  val vau2_incr = UInt(conf.fconv_stages + 1 + conf.delay_seq_exp, conf.ptr_incr_sz)

  val viu_wptr1 = PtrIncr(ptr, viu_incr, io.cfg.bcnt)
  val viu_wptr2 = PtrIncr(viu_wptr1, 1, io.cfg.bcnt)
  val vau0_wptr = PtrIncr(ptr, vau0_incr, io.cfg.bcnt)
  val vau1_wptr2 = PtrIncr(ptr, vau1_incr, io.cfg.bcnt)
  val vau1_wptr3 = PtrIncr(vau1_wptr2, 1, io.cfg.bcnt)
  val vau2_wptr = PtrIncr(ptr, vau2_incr, io.cfg.bcnt)

  val viu_wptr = Mux(io.issueop.bits.fn.viu.rtype(), viu_wptr2, viu_wptr1)
  val vau1_wptr = Mux(io.issueop.bits.fn.vau1.fma(), vau1_wptr3, vau1_wptr2)

  val tbl_rport_val = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_rport_vau0 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_rport_vau1 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_rport_vau2 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_rport_vsu = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_rport_vgu = Vec.fill(SZ_BANK){Reg(init=Bool(false))}

  val tbl_wport_val = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_wport_vau0 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_wport_vau1 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_wport_vau2 = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_wport_vlu = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_wport_vd = Vec.fill(SZ_BANK){Reg(new RegInfo)}

  val tbl_sport_val = Vec.fill(SZ_BANK){Reg(init=Bool(false))}

  when (io.issueop.valid) {

    when (io.issueop.bits.active.viu) {
      tbl_rport_val(ptr2) := Bool(true)

      when (io.issueop.bits.fn.viu.rtype()) {
        tbl_rport_val(ptr3) := Bool(true)
      }

      tbl_wport_val(viu_wptr) := Bool(true)
      tbl_wport_vd(viu_wptr) := io.issueop.bits.reg.vd

      tbl_sport_val(ptr1) := Bool(true)
    }

    when (io.issueop.bits.active.vau0) {
      tbl_rport_val(ptr2) := Bool(true)
      tbl_rport_vau0(ptr2) := Bool(true)

      tbl_rport_val(ptr3) := Bool(true)
      tbl_rport_vau0(ptr3) := Bool(true)

      tbl_wport_val(vau0_wptr) := Bool(true)
      tbl_wport_vau0(vau0_wptr) := Bool(true)
      tbl_wport_vd(vau0_wptr) := io.issueop.bits.reg.vd

      tbl_sport_val(ptr1) := Bool(true)
    }

    when (io.issueop.bits.active.vau1) {
      tbl_rport_val(ptr2) := Bool(true)
      tbl_rport_vau1(ptr2) := Bool(true)

      tbl_rport_val(ptr3) := Bool(true)
      tbl_rport_vau1(ptr3) := Bool(true)

      when (io.issueop.bits.fn.vau1.fma()) {
        tbl_rport_val(ptr4) := Bool(true)
        tbl_rport_vau1(ptr4) := Bool(true)
      }

      tbl_wport_val(vau1_wptr) := Bool(true)
      tbl_wport_vau1(vau1_wptr) := Bool(true)
      tbl_wport_vd(vau1_wptr) := io.issueop.bits.reg.vd

      tbl_sport_val(ptr1) := Bool(true)
    }

    when (io.issueop.bits.active.vau2) {
      tbl_rport_val(ptr2) := Bool(true)
      tbl_rport_vau2(ptr2) := Bool(true)

      tbl_wport_val(vau2_wptr) := Bool(true)
      tbl_wport_vau2(vau2_wptr) := Bool(true)
      tbl_wport_vd(vau2_wptr) := io.issueop.bits.reg.vd

      tbl_sport_val(ptr1) := Bool(true)
    }

    when (io.issueop.bits.active.amo) {
      tbl_rport_val(ptr2) := Bool(true)
      tbl_rport_vgu(ptr2) := Bool(true)

      tbl_rport_val(ptr3) := Bool(true)
      tbl_rport_vsu(ptr3) := Bool(true)

      tbl_wport_val(ptr5) := Bool(true)
      tbl_wport_vlu(ptr5) := Bool(true)
      tbl_wport_vd(ptr5) := io.issueop.bits.reg.vd

      tbl_sport_val(ptr1) := Bool(true)
      tbl_sport_val(ptr2) := Bool(true)
      tbl_sport_val(ptr3) := Bool(true)
    }

    when (io.issueop.bits.active.utst || io.issueop.bits.active.vst) {
      tbl_rport_val(ptr2) := Bool(true)
      tbl_rport_vgu(ptr2) := Bool(true)

      tbl_rport_val(ptr3) := Bool(true)
      tbl_rport_vsu(ptr3) := Bool(true)

      tbl_sport_val(ptr1) := Bool(true)
      tbl_sport_val(ptr2) := Bool(true)
    }

    when (io.issueop.bits.active.utld || io.issueop.bits.active.vld) {
      tbl_rport_val(ptr2) := Bool(true)
      tbl_rport_vgu(ptr2) := Bool(true)

      tbl_wport_val(ptr4) := Bool(true)
      tbl_wport_vlu(ptr4) := Bool(true)
      tbl_wport_vd(ptr4) := io.issueop.bits.reg.vd

      tbl_sport_val(ptr1) := Bool(true)
      tbl_sport_val(ptr2) := Bool(true)
    }
  }

  when (io.seq_to_hazard.last) {
    tbl_sport_val(ptr) := Bool(false)
  }

  when (io.expand_to_hazard.wen) {
    tbl_wport_vd(ptr).id := tbl_wport_vd(ptr).id + Mux(tbl_wport_vd(ptr).float, io.cfg.fstride, io.cfg.xstride)
  }

  when (io.lane_to_hazard.rlast) {
    tbl_rport_val(ptr) := Bool(false)
    tbl_rport_vau0(ptr) := Bool(false)
    tbl_rport_vau1(ptr) := Bool(false)
    tbl_rport_vau2(ptr) := Bool(false)
    tbl_rport_vsu(ptr) := Bool(false)
    tbl_rport_vgu(ptr) := Bool(false)
  }

  when (io.lane_to_hazard.wlast) {
    tbl_wport_val(ptr) := Bool(false)
    tbl_wport_vau0(ptr) := Bool(false)
    tbl_wport_vau1(ptr) := Bool(false)
    tbl_wport_vau2(ptr) := Bool(false)
    tbl_wport_vlu(ptr) := Bool(false)
  }

  // hazard check logic for tvec/vt
  def zipReduce(valid: Vec[Bool], p: Vec[Bool]) = valid.zip(p).map(b => b._1 && b._2).reduce(_||_)
  val shazard_vau0 = zipReduce(tbl_rport_val, tbl_rport_vau0) || zipReduce(tbl_wport_val, tbl_wport_vau0)
  val shazard_vau1 = zipReduce(tbl_rport_val, tbl_rport_vau1) || zipReduce(tbl_wport_val, tbl_wport_vau1)
  val shazard_vau2 = zipReduce(tbl_rport_val, tbl_rport_vau2) || zipReduce(tbl_wport_val, tbl_wport_vau2)
  val shazard_vgu = zipReduce(tbl_rport_val, tbl_rport_vgu)
  val shazard_vlu = zipReduce(tbl_wport_val, tbl_wport_vlu)
  val shazard_vsu = zipReduce(tbl_rport_val, tbl_rport_vsu)

  val seqhazard_1slot = tbl_sport_val(ptr1)
  val seqhazard_2slot = tbl_sport_val(ptr1) | tbl_sport_val(ptr2)
  val seqhazard_3slot = tbl_sport_val(ptr1) | tbl_sport_val(ptr2) | tbl_sport_val(ptr3)

  val bhazard_amo = tbl_rport_val(ptr2) | tbl_rport_val(ptr3) | tbl_wport_val(ptr5)
  val bhazard_utld = tbl_rport_val(ptr2) | tbl_wport_val(ptr4)
  val bhazard_utst = tbl_rport_val(ptr2) | tbl_rport_val(ptr3)
  val bhazard_vld = tbl_rport_val(ptr2) | tbl_wport_val(ptr4)
  val bhazard_vst = tbl_rport_val(ptr2) | tbl_rport_val(ptr3)

  def check_hazards(op: IssueOpIO) = {
    val dhazard_vs = tbl_wport_val.zip(tbl_wport_vd).map{ case (valid,vd) => valid && op.bits.reg.vs.id === vd.id }.reduce(_||_)
    val dhazard_vt = tbl_wport_val.zip(tbl_wport_vd).map{ case (valid,vd) => valid && op.bits.reg.vt.id === vd.id }.reduce(_||_)
    val dhazard_vr = tbl_wport_val.zip(tbl_wport_vd).map{ case (valid,vd) => valid && op.bits.reg.vr.id === vd.id }.reduce(_||_)
    val dhazard_vd = tbl_wport_val.zip(tbl_wport_vd).map{ case (valid,vd) => valid && op.bits.reg.vd.id === vd.id }.reduce(_||_)
    val dhazard = List(
        op.bits.reg.vs.active && dhazard_vs && !op.bits.reg.vs.zero,
        op.bits.reg.vt.active && dhazard_vt && !op.bits.reg.vt.zero,
        op.bits.reg.vr.active && dhazard_vr && !op.bits.reg.vr.zero,
        op.bits.reg.vd.active && dhazard_vd
      ).reduce(_||_)

    val memop = List(op.bits.active.amo, op.bits.active.utld, op.bits.active.utst, op.bits.active.vld, op.bits.active.vst).reduce(_||_)

    val shazard = List(
      shazard_vau0 && op.bits.active.vau0,
      shazard_vau1 && op.bits.active.vau1,
      shazard_vau2 && op.bits.active.vau2,
      shazard_vgu && memop,
      shazard_vlu && List(op.bits.active.amo, op.bits.active.utld, op.bits.active.vld).reduce(_||_),
      shazard_vsu && List(op.bits.active.amo, op.bits.active.utst, op.bits.active.vst).reduce(_||_)
    ).reduce(_||_)

    val seqhazard = List(
      seqhazard_1slot && op.bits.active.viu,
      seqhazard_1slot && op.bits.active.vau0,
      seqhazard_1slot && op.bits.active.vau1,
      seqhazard_1slot && op.bits.active.vau2,
      seqhazard_3slot && op.bits.active.amo,
      seqhazard_2slot && op.bits.active.utld,
      seqhazard_2slot && op.bits.active.utst,
      seqhazard_2slot && op.bits.active.vld,
      seqhazard_2slot && op.bits.active.vst
    ).reduce(_||_)

    val wptr = MuxCase(
      UInt(0), Array(
        op.bits.active.viu -> Mux(op.bits.fn.viu.rtype(), viu_wptr2, viu_wptr1),
        op.bits.active.vau0 -> vau0_wptr,
        op.bits.active.vau1 -> Mux(op.bits.fn.vau1.fma(), vau1_wptr3, vau1_wptr2),
        op.bits.active.vau2 -> vau2_wptr
      ))

    val rports = PopCount(List(op.bits.reg.vs.active, op.bits.reg.vt.active, op.bits.reg.vr.active))

    val bhazard_r1w1 = tbl_rport_val(ptr2) | tbl_wport_val(wptr)
    val bhazard_r2w1 = tbl_rport_val(ptr2) | tbl_rport_val(ptr3) | tbl_wport_val(wptr)
    val bhazard_r3w1 = tbl_rport_val(ptr2) | tbl_rport_val(ptr3) | tbl_rport_val(ptr4) | tbl_wport_val(wptr)
    val bhazard = List(
      bhazard_r1w1 && !memop && op.bits.reg.vd.active && (rports === UInt(0) || rports === UInt(1)),
      bhazard_r2w1 && !memop && op.bits.reg.vd.active && rports === UInt(2),
      bhazard_r3w1 && !memop && op.bits.reg.vd.active && rports === UInt(3),
      bhazard_amo && op.bits.active.amo,
      bhazard_utld && op.bits.active.utld,
      bhazard_utst && op.bits.active.utst,
      bhazard_vld && op.bits.active.vld,
      bhazard_vst && op.bits.active.vst
    ).reduce(_||_)

    op.bits.reg.vd.zero || !io.seq_to_hazard.stall && !dhazard && !shazard && !seqhazard && !bhazard
  }

  io.tvec.ready := check_hazards(io.tvec.op)
  io.vt.ready := check_hazards(io.vt.op)

  val fire_tvec = io.tvec.op.valid && !io.tvec.op.bits.reg.vd.zero && io.tvec.ready
  val fire_vt = io.vt.op.valid && !io.vt.op.bits.reg.vd.zero && io.vt.ready

  io.issueop.valid := Mux(io.tvec.active, fire_tvec, fire_vt)
  io.issueop.bits := Mux(io.tvec.active, io.tvec.op.bits, io.vt.op.bits)

  io.pending_memop := List(tbl_rport_vgu, tbl_rport_vsu, tbl_wport_vlu).map(vb => vb.reduce(_||_)).reduce(_||_)
}

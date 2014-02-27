package hwacha

import Chisel._
import Node._
import Constants._

class SequencerToHazardIO extends Bundle
{
  val stall = Bool(OUTPUT)
  val last = Bool(OUTPUT)
  val active = new VFU().asOutput
  val cnt = Bits(OUTPUT, SZ_BCNT)
}

class ExpanderToHazardIO extends Bundle
{
  val wen = Bool(OUTPUT)
  val rlast = Bool(OUTPUT)
  val wlast = Bool(OUTPUT)
}

class Hazard(resetSignal: Bool = null)(implicit conf: HwachaConfiguration) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip

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
    
    val seq_to_hazard = new SequencerToHazardIO().flip
    val expand_to_hazard = new ExpanderToHazardIO().flip
    val pending_memop = Bool(OUTPUT)
  }

  val ptr = Reg(init = UInt(0, SZ_BPTR))
  val ptr1 = PtrIncr(ptr, 1, io.cfg.bcnt)
  val ptr2 = PtrIncr(ptr, 2, io.cfg.bcnt)
  val ptr3 = PtrIncr(ptr, 3, io.cfg.bcnt)
  val ptr4 = PtrIncr(ptr, 4, io.cfg.bcnt)
  val ptr5 = PtrIncr(ptr, 5, io.cfg.bcnt)
  ptr := ptr1

  val viu_incr = UInt(conf.int_stages + conf.delay_seq_exp, conf.ptr_incr_sz)
  val vau0_incr = UInt(conf.imul_stages + 3 + conf.delay_seq_exp, conf.ptr_incr_sz)
  val vau1_incr = UInt(conf.fma_stages + 3 + conf.delay_seq_exp, conf.ptr_incr_sz)
  val vau2_incr = UInt(conf.fconv_stages + 2 + conf.delay_seq_exp, conf.ptr_incr_sz)

  val viu_wptr0 = PtrIncr(ptr, viu_incr, io.cfg.bcnt)
  val viu_wptr1 = PtrIncr(viu_wptr0, 1, io.cfg.bcnt)
  val viu_wptr2 = PtrIncr(viu_wptr0, 2, io.cfg.bcnt)
  val vau0_wptr = PtrIncr(ptr, vau0_incr, io.cfg.bcnt)
  val vau1_wptr2 = PtrIncr(ptr, vau1_incr, io.cfg.bcnt)
  val vau1_wptr3 = PtrIncr(vau1_wptr2, 1, io.cfg.bcnt)
  val vau2_wptr = PtrIncr(ptr, vau2_incr, io.cfg.bcnt)

  val viu_wptr = io.issueop.bits.fn.viu.wptr_sel(viu_wptr0, viu_wptr1, viu_wptr2)
  val vau1_wptr = io.issueop.bits.fn.vau1.wptr_sel(vau1_wptr2, vau1_wptr3)

  class RegHazardTblInfo extends RegHazardInfo
  {
    val float = Bool()
    val utidx = UInt(width = SZ_VLEN)

    override def clone = new RegHazardTblInfo().asInstanceOf[this.type]
  }

  val tbl_rport = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_wport = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_vd = Vec.fill(SZ_BANK){Reg(new RegHazardTblInfo)}
  val tbl_seqslot = Vec.fill(SZ_BANK){Reg(init=Bool(false))}
  val tbl_vfu = Reg(init=new VFU().fromBits(Bits(0)))

  when (io.issueop.valid) {

    when (io.issueop.bits.active.viu) {
      when (io.issueop.bits.fn.viu.rs1()) { tbl_rport(ptr2) := Bool(true) }
      when (io.issueop.bits.fn.viu.rs2()) { tbl_rport(ptr3) := Bool(true) }
      tbl_wport(viu_wptr) := Bool(true)
      tbl_vd(viu_wptr).active := Bool(true)
      tbl_vd(viu_wptr).float := io.issueop.bits.reg.vd.float
      tbl_vd(viu_wptr).base := io.issueop.bits.regcheck.vd.base
      tbl_vd(viu_wptr).utidx := io.issueop.bits.utidx
      tbl_seqslot(ptr1) := Bool(true)
      tbl_vfu.viu := Bool(false) // just to make chisel happy
    }

    when (io.issueop.bits.active.vau0) {
      tbl_rport(ptr2) := Bool(true)
      tbl_rport(ptr3) := Bool(true)
      tbl_wport(vau0_wptr) := Bool(true)
      tbl_vd(vau0_wptr).active := Bool(true)
      tbl_vd(vau0_wptr).float := io.issueop.bits.reg.vd.float
      tbl_vd(vau0_wptr).base := io.issueop.bits.regcheck.vd.base
      tbl_vd(vau0_wptr).utidx := io.issueop.bits.utidx
      tbl_seqslot(ptr1) := Bool(true)
      tbl_vfu.vau0 := Bool(true)
    }

    when (io.issueop.bits.active.vau1) {
      tbl_rport(ptr2) := Bool(true)
      tbl_rport(ptr3) := Bool(true)
      when (io.issueop.bits.fn.vau1.r4type()) { tbl_rport(ptr4) := Bool(true) }
      tbl_wport(vau1_wptr) := Bool(true)
      tbl_vd(vau1_wptr).active := Bool(true)
      tbl_vd(vau1_wptr).float := io.issueop.bits.reg.vd.float
      tbl_vd(vau1_wptr).base := io.issueop.bits.regcheck.vd.base
      tbl_vd(vau1_wptr).utidx := io.issueop.bits.utidx
      tbl_seqslot(ptr1) := Bool(true)
      when (io.issueop.bits.sel.vau1) { tbl_vfu.vau1t := Bool(true) }
      .otherwise { tbl_vfu.vau1f := Bool(true) }
    }

    when (io.issueop.bits.active.vau2) {
      tbl_rport(ptr2) := Bool(true)
      tbl_wport(vau2_wptr) := Bool(true)
      tbl_vd(vau2_wptr).active := Bool(true)
      tbl_vd(vau2_wptr).float := io.issueop.bits.reg.vd.float
      tbl_vd(vau2_wptr).base := io.issueop.bits.regcheck.vd.base
      tbl_vd(vau2_wptr).utidx := io.issueop.bits.utidx
      tbl_seqslot(ptr1) := Bool(true)
      tbl_vfu.vau2 := Bool(true)
    }

    when (io.issueop.bits.active.amo) {
      tbl_rport(ptr2) := Bool(true)
      tbl_rport(ptr3) := Bool(true)
      tbl_wport(ptr5) := Bool(true)
      tbl_vd(ptr5).active := Bool(true)
      tbl_vd(ptr5).float := io.issueop.bits.reg.vd.float
      tbl_vd(ptr5).base := io.issueop.bits.regcheck.vd.base
      tbl_vd(ptr5).utidx := io.issueop.bits.utidx
      tbl_seqslot(ptr1) := Bool(true)
      tbl_seqslot(ptr2) := Bool(true)
      tbl_seqslot(ptr3) := Bool(true)
      tbl_vfu.vgu := Bool(true)
      tbl_vfu.vcu := Bool(true)
      tbl_vfu.vsu := Bool(true)
      tbl_vfu.vlu := Bool(true)
    }

    when (io.issueop.bits.active.utst || io.issueop.bits.active.vst) {
      tbl_rport(ptr2) := Bool(true)
      tbl_rport(ptr3) := Bool(true)
      tbl_seqslot(ptr1) := Bool(true)
      tbl_seqslot(ptr2) := Bool(true)
      tbl_vfu.vgu := Bool(true)
      tbl_vfu.vcu := Bool(true)
      tbl_vfu.vsu := Bool(true)
    }

    when (io.issueop.bits.active.utld || io.issueop.bits.active.vld) {
      tbl_rport(ptr2) := Bool(true)
      // ptr3 for vcu, but don't allocate a rport for it
      tbl_wport(ptr5) := Bool(true)
      tbl_vd(ptr5).active := Bool(true)
      tbl_vd(ptr5).float := io.issueop.bits.reg.vd.float
      tbl_vd(ptr5).base := io.issueop.bits.regcheck.vd.base
      tbl_vd(ptr5).utidx := io.issueop.bits.utidx
      tbl_seqslot(ptr1) := Bool(true)
      tbl_seqslot(ptr2) := Bool(true)
      tbl_seqslot(ptr3) := Bool(true)
      tbl_vfu.vgu := Bool(true)
      tbl_vfu.vcu := Bool(true)
      tbl_vfu.vlu := Bool(true)
    }
  }

  when (io.expand_to_hazard.rlast) {
    tbl_rport(ptr) := Bool(false)
  }

  when (io.expand_to_hazard.wlast) {
    tbl_wport(ptr) := Bool(false)
  }

  when (io.expand_to_hazard.wen) {
    tbl_vd(ptr).active := Bool(false)
  }

  def clear_shazards(active: VFU) = {
    when (active.vau0) { tbl_vfu.vau0 := Bool(false) }
    when (active.vau1t) { tbl_vfu.vau1t := Bool(false) }
    when (active.vau1f) { tbl_vfu.vau1f := Bool(false) }
    when (active.vau2) { tbl_vfu.vau2 := Bool(false) }
    when (active.vgu) { tbl_vfu.vgu := Bool(false) }
    when (active.vcu) { tbl_vfu.vcu := Bool(false) }
    when (active.vlu) { tbl_vfu.vlu := Bool(false) }
    when (active.vsu) { tbl_vfu.vsu := Bool(false) }
  }

  val n = conf.nbanks-1
  val last = Vec.fill(n){Reg(init=Bool(false))}
  val clear_vfu = Vec.fill(n){Reg(new VFU().fromBits(Bits(0)))}
  (0 until n).reverse.foreach(i => ({
    val step_en = if (i==n-1) Bool(false) else last(i+1)
    last(i) := step_en
    if (i==n-1) {
      clear_vfu(i) := clear_vfu(i).fromBits(Bits(0))
    } else {
      when (step_en) {
        clear_vfu(i) := clear_vfu(i+1)
      }
    }
  }))

  when (io.seq_to_hazard.last) {
    tbl_seqslot(ptr) := Bool(false)

    when (io.seq_to_hazard.cnt === UInt(1)) {
      clear_shazards(io.seq_to_hazard.active)
    }
    .otherwise {
      val cnt = io.seq_to_hazard.cnt - UInt(2)
      last(cnt) := Bool(true)
      when (last(cnt)) {
        clear_vfu(cnt) := new VFU().fromBits(clear_vfu(cnt).toBits | io.seq_to_hazard.active.toBits)
      }
      .otherwise {
        clear_vfu(cnt) := io.seq_to_hazard.active
      }
    }
  }

  when (last(0)) {
    clear_shazards(clear_vfu(0))
  }

  // hazard check logic for tvec/vt
  val seqhazard_1slot = tbl_seqslot(ptr1)
  val seqhazard_2slot = tbl_seqslot(ptr1) | tbl_seqslot(ptr2)
  val seqhazard_3slot = tbl_seqslot(ptr1) | tbl_seqslot(ptr2) | tbl_seqslot(ptr3)

  val bhazard_amo = tbl_rport(ptr2) | tbl_rport(ptr3) | tbl_wport(ptr5)
  val bhazard_utld = tbl_rport(ptr2) | tbl_wport(ptr5)
  val bhazard_utst = tbl_rport(ptr2) | tbl_rport(ptr3)
  val bhazard_vld = tbl_rport(ptr2) | tbl_wport(ptr5)
  val bhazard_vst = tbl_rport(ptr2) | tbl_rport(ptr3)

  def check_hazards(op: IssueOpIO) = {
    def check_dhazard(tbl: RegHazardTblInfo, rinfo: RegInfo, rhzinfo: RegHazardInfo) =
      tbl.active && tbl.float === rinfo.float && tbl.base === rhzinfo.base && tbl.utidx === op.bits.utidx

    val dhazard_vs = tbl_wport.zip(tbl_vd).map{ case (valid,vd) => valid && check_dhazard(vd, op.bits.reg.vs, op.bits.regcheck.vs) }.reduce(_||_)
    val dhazard_vt = tbl_wport.zip(tbl_vd).map{ case (valid,vd) => valid && check_dhazard(vd, op.bits.reg.vt, op.bits.regcheck.vt) }.reduce(_||_)
    val dhazard_vr = tbl_wport.zip(tbl_vd).map{ case (valid,vd) => valid && check_dhazard(vd, op.bits.reg.vr, op.bits.regcheck.vr) }.reduce(_||_)
    val dhazard_vd = tbl_wport.zip(tbl_vd).map{ case (valid,vd) => valid && check_dhazard(vd, op.bits.reg.vd, op.bits.regcheck.vd) }.reduce(_||_)
    val dhazard = List(
        op.bits.regcheck.vs.active && dhazard_vs && !op.bits.reg.vs.zero,
        op.bits.regcheck.vt.active && dhazard_vt && !op.bits.reg.vt.zero,
        op.bits.regcheck.vr.active && dhazard_vr && !op.bits.reg.vr.zero,
        op.bits.regcheck.vd.active && dhazard_vd && !op.bits.reg.vd.zero
      ).reduce(_||_)

    val memop = List(op.bits.active.amo, op.bits.active.utld, op.bits.active.utst, op.bits.active.vld, op.bits.active.vst).reduce(_||_)

    val shazard = List(
      tbl_vfu.vau0 && op.bits.active.vau0,
      tbl_vfu.vau1t && tbl_vfu.vau1f && op.bits.active.vau1, // shazard when both vau1t and vau1f are used
      tbl_vfu.vau2 && op.bits.active.vau2,
      tbl_vfu.vgu && memop,
      tbl_vfu.vcu && memop,
      tbl_vfu.vlu && List(op.bits.active.amo, op.bits.active.utld, op.bits.active.vld).reduce(_||_),
      tbl_vfu.vsu && List(op.bits.active.amo, op.bits.active.utst, op.bits.active.vst).reduce(_||_)
    ).reduce(_||_)

    val seqhazard = List(
      seqhazard_1slot && op.bits.active.viu,
      seqhazard_1slot && op.bits.active.vau0,
      seqhazard_1slot && op.bits.active.vau1,
      seqhazard_1slot && op.bits.active.vau2,
      seqhazard_3slot && op.bits.active.amo,
      seqhazard_3slot && op.bits.active.utld,
      seqhazard_2slot && op.bits.active.utst,
      seqhazard_3slot && op.bits.active.vld,
      seqhazard_2slot && op.bits.active.vst
    ).reduce(_||_)

    val wptr = MuxCase(
      UInt(0), Array(
        op.bits.active.viu -> op.bits.fn.viu.wptr_sel(viu_wptr0, viu_wptr1, viu_wptr2),
        op.bits.active.vau0 -> vau0_wptr,
        op.bits.active.vau1 -> op.bits.fn.vau1.wptr_sel(vau1_wptr2, vau1_wptr3),
        op.bits.active.vau2 -> vau2_wptr
      ))

    val rports = PopCount(List(op.bits.regcheck.vs.active, op.bits.regcheck.vt.active, op.bits.regcheck.vr.active))

    val bhazard_r0w1 = tbl_wport(wptr)
    val bhazard_r1w1 = tbl_rport(ptr2) || bhazard_r0w1
    val bhazard_r2w1 = tbl_rport(ptr3) || bhazard_r1w1
    val bhazard_r3w1 = tbl_rport(ptr4) || bhazard_r2w1
    val bhazard = List(
      bhazard_r0w1 && !memop && op.bits.regcheck.vd.active && rports === UInt(0),
      bhazard_r1w1 && !memop && op.bits.regcheck.vd.active && rports === UInt(1),
      bhazard_r2w1 && !memop && op.bits.regcheck.vd.active && rports === UInt(2),
      bhazard_r3w1 && !memop && op.bits.regcheck.vd.active && rports === UInt(3),
      bhazard_amo && op.bits.active.amo,
      bhazard_utld && op.bits.active.utld,
      bhazard_utst && op.bits.active.utst,
      bhazard_vld && op.bits.active.vld,
      bhazard_vst && op.bits.active.vst
    ).reduce(_||_)

    !io.seq_to_hazard.stall && !dhazard && !shazard && !seqhazard && !bhazard
  }

  io.tvec.ready := check_hazards(io.tvec.op)
  io.vt.ready := check_hazards(io.vt.op)

  val fire_tvec = io.tvec.op.valid && io.tvec.ready
  val fire_vt = io.vt.op.valid && io.vt.ready

  io.issueop.valid := Mux(io.tvec.active, fire_tvec, fire_vt)
  io.issueop.bits := Mux(io.tvec.active, io.tvec.op.bits, io.vt.op.bits)
  io.issueop.bits.sel.vau1 := !tbl_vfu.vau1t // select vau1t when not in use

  io.pending_memop := tbl_vfu.vsu || tbl_vfu.vlu
}

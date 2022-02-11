package hwacha

import Chisel._
import freechips.rocketchip.config._
import DataGating._

class VDUTag(implicit p: Parameters) extends VXUBundle()(p)
  with BankPred with BankPack {
  val bank = UInt(width = bBanks)
  val selff = Bool() // select ff if true
  val addr = UInt(width = math.max(log2Up(nSRAM), log2Up(nFF)))
  val fusel = Bits(width = 1) // because we have 2 units idiv/fdiv
}

class ReduceResultIO(implicit p: Parameters) extends VXUBundle()(p) {
  val pred = Decoupled(new RPredResult)
  val first = Decoupled(new RFirstResult)
}

class VDU(implicit p: Parameters) extends VXUModule()(p) {
  val io = new DCCIssueIO {
    val cfg = new HwachaConfigIO().flip
    val ack = new DCCAckIO
    val pla = new CounterLookAheadIO().flip // lpq entry
    val qla = Vec(nVDUOperands, new CounterLookAheadIO).flip // lrq entries
    val ila = new CounterLookAheadIO().flip // idiv output entries
    val fla = new CounterLookAheadIO().flip // fdiv output entries
    val lpq = new LPQIO().flip
    val lrqs = Vec(nVDUOperands, new LRQIO).flip
    val bwqs = Vec(nBanks, new BWQIO)
    val red = new ReduceResultIO
  }

  val ctrl = Module(new VDUCtrl)
  ctrl.suggestName("ctrlInst")

  ctrl.io.op <> io.op
  ctrl.io.ila <> io.ila
  ctrl.io.fla <> io.fla
  ctrl.io.cfg <> io.cfg

  val lpq = Module(new Queue(new LPQEntry, nBanks+2))
  lpq.suggestName("lpqInst")
  lpq.io.enq <> io.lpq
  ctrl.io.lpq <> lpq.io.deq

  val pcntr = Module(new LookAheadCounter(nBanks+2, nBanks+2))
  pcntr.suggestName("pcntrInst")
  pcntr.io.inc.cnt := UInt(1)
  pcntr.io.inc.update := lpq.io.deq.fire
  pcntr.io.dec <> io.pla

  for (i <- 0 until nVDUOperands) {
    val lrq = Module(new Queue(new LRQEntry, nBanks+2))
    lrq.suggestName("lrqInst")
    lrq.io.enq <> io.lrqs(i)
    ctrl.io.lrqs.q(i) <> lrq.io.deq

    val cntr = Module(new LookAheadCounter(nBanks+2, nBanks+2))
    cntr.suggestName("cntrInst")
    cntr.io.inc.cnt := UInt(1)
    cntr.io.inc.update := ctrl.io.lrqs.update(i)
    cntr.io.dec <> io.qla(i)
  }

  for (i <- 0 until nSlices) {
    val idiv = Module(new IDivSlice)
    idiv.suggestName("idivInst")
    val fdiv = Module(new FDivSlice)
    fdiv.suggestName("fdivInst")
    idiv.io <> ctrl.io.idiv.fus(i)
    fdiv.io <> ctrl.io.fdiv.fus(i)
  }

  val rpred = Module(new RPredLane)
  rpred.suggestName("rpredInst")
  rpred.io <> ctrl.io.rpred.fu
  io.red.pred.bits.cond := rpred.io.result.bits.cond
  io.red.pred.valid := ctrl.io.rpred.result.valid
  ctrl.io.rpred.result.ready := io.red.pred.ready

  val rfirst = Module(new RFirstLane)
  rfirst.suggestName("rfirstInst")
  rfirst.io <> ctrl.io.rfirst.fu
  io.red.first.bits <> rfirst.io.result.bits
  io.red.first.valid := ctrl.io.rfirst.result.valid
  ctrl.io.rfirst.result.ready := io.red.first.ready

  io.ack.vidu <> ctrl.io.idiv.ack
  io.ack.vfdu <> ctrl.io.fdiv.ack
  io.bwqs <> ctrl.io.bwqs
}

class VDUCtrl(implicit p: Parameters) extends VXUModule()(p) with PackLogic {
  val io = new DCCIssueIO {
    val cfg = new HwachaConfigIO().flip
    val ila = new CounterLookAheadIO().flip
    val fla = new CounterLookAheadIO().flip
    val lpq = new LPQIO().flip
    val lrqs = new Bundle {
      val q = Vec(nVDUOperands, new LRQIO).flip
      val update = Vec(nVDUOperands, Bool(OUTPUT))
    }

    val idiv = new Bundle {
      val fus = Vec(nSlices, new IDivIO)
      val ack = Valid(new VIDUAck)
    }
    val fdiv = new Bundle {
      val fus = Vec(nSlices, new FDivIO)
      val ack = Valid(new VFDUAck)
    }
    val rpred = new Bundle {
      val fu = new RPredIO
      val result = Decoupled(new RPredResult)
    }
    val rfirst = new Bundle {
      val fu = new RFirstIO
      val result = Decoupled(new RFirstResult)
    }

    val bwqs = Vec(nBanks, new BWQIO)
  }

  val opq = Module(new Queue(new DCCOp, nDCCOpQ))
  opq.suggestName("opqInst")
  opq.io.enq <> io.op

  val s_idle :: s_busy :: s_wait :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)
  val op = Reg(new DCCOp)

  val slice_idx = Reg(UInt(width = bfLStrip - bSlices))
  val strip_idx = Reg(UInt(width = bVLen - bStrip))
  val slice_idx_next = slice_idx + UInt(1)
  val lstrip = io.cfg.lstrip >> UInt(bSlices)
  val bank = slice_idx(bBanks-1, 0)

  val pack = Wire(new PackInfo)
  val (vd_update, vd_stride) = if (confprec) {
    pack.prec := op.vd.prec
    pack.idx := slice_idx >> UInt(bBanks)
    confprec_step(pack.prec, slice_idx_next >> UInt(bBanks), io.cfg)
  } else {
    pack.prec := PREC_D
    pack.idx := UInt(0)
    (Bool(true), io.cfg.vstride.d)
  }

  val fire = Wire(Bool())
  val fire_div = Wire(Bool())
  val fire_first = Wire(Bool())
  val fire_reduce = Wire(Bool())
  val ecnt = Mux(op.vlen > UInt(nSlices), UInt(nSlices), op.vlen(bSlices, 0))
  val vlen_next = op.vlen - ecnt
  val pred = Vec((0 until nSlices).map(UInt(_) < ecnt)).asUInt
  val idiv_active = op.active.vidiv

  opq.io.deq.ready := Bool(false)
  io.rpred.result.valid := Bool(false)
  io.rfirst.result.valid := Bool(false)

  switch (state) {
    is (s_idle) {
      opq.io.deq.ready := Bool(true)
      when (opq.io.deq.valid) {
        state := s_busy
        op := opq.io.deq.bits
        slice_idx := UInt(0)
        strip_idx := UInt(0)
      }
    }
    is (s_busy) {
      when (fire) {
        op.vlen := vlen_next
        when (vlen_next === UInt(0)) {
          state := Mux(fire_reduce, s_wait, s_idle)
        }
      }
      when (fire_div) {
        slice_idx := slice_idx_next
        when (vd_update && (bank === UInt(nBanks-1))) {
          op.vd.id := op.vd.id + vd_stride
        }
      }
      when (fire_first) {
        slice_idx := slice_idx_next
        when (slice_idx_next === lstrip) {
          slice_idx := UInt(0)
          strip_idx := strip_idx + UInt(1)
        }
      }
    }
    is (s_wait) {
      io.rpred.result.valid := op.active.vrpred
      io.rfirst.result.valid := op.active.vrfirst
      when (io.rpred.result.fire || io.rfirst.result.fire) {
        state := s_idle
      }
    }
  }

  val tagq = Module(new Queue(new VDUTag, nDecoupledUnitWBQueue))
  tagq.suggestName("tagqInst")

  val active_entry = io.lpq.bits.active()
  val mask_lrq0_valid = !active_entry || io.lrqs.q(0).valid
  val mask_lrq1_valid = !active_entry || io.lrqs.q(1).valid
  val deq_fdiv_lrq1 = op.fn.vfdu().op_is(FD_DIV)
  val mask_fdiv_lrq1_valid = !deq_fdiv_lrq1 || mask_lrq1_valid
  val enq_idivs_req = (0 until nSlices).map { i => pred(i) && io.lpq.bits.pred(i) }
  val enq_fdivs_req = (0 until nSlices).map { i => pred(i) && io.lpq.bits.pred(i) }
  val mask_idivs_req_ready = io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    !enq_idivs_req(i) || idiv.req.ready }
  val mask_fdivs_req_ready = io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    !enq_fdivs_req(i) || fdiv.req.ready }

  def fire_idiv(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      state === s_busy, op.active.vidiv,
      io.lpq.valid, mask_lrq0_valid, mask_lrq1_valid,
      tagq.io.enq.ready) ++ mask_idivs_req_ready
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  def fire_fdiv(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      state === s_busy, op.active.vfdiv,
      io.lpq.valid, mask_lrq0_valid, mask_fdiv_lrq1_valid,
      tagq.io.enq.ready) ++ mask_fdivs_req_ready
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  def fire_rpred(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      state === s_busy, op.active.vrpred,
      io.lpq.valid, io.rpred.fu.req.ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  def fire_rfirst(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      state === s_busy, op.active.vrfirst,
      io.lpq.valid, mask_lrq0_valid, io.rfirst.fu.req.ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  fire_div := fire_idiv(null) || fire_fdiv(null)
  fire_first := fire_rfirst(null)
  fire_reduce := fire_rpred(null) || fire_first
  fire := fire_div || fire_reduce

  io.lpq.ready :=
    fire_idiv(io.lpq.valid) || fire_fdiv(io.lpq.valid) ||
    fire_rpred(io.lpq.valid) || fire_rfirst(io.lpq.valid)
  io.lrqs.q(0).ready :=
    fire_idiv(mask_lrq0_valid, active_entry) || fire_fdiv(mask_lrq0_valid, active_entry) ||
    fire_rfirst(mask_lrq0_valid, active_entry)
  io.lrqs.q(1).ready :=
    fire_idiv(mask_lrq1_valid, active_entry) ||
    fire_fdiv(mask_fdiv_lrq1_valid, deq_fdiv_lrq1, active_entry)
  io.lrqs.update(0) := fire_idiv(null) || fire_fdiv(null) || fire_rfirst(null)
  io.lrqs.update(1) := fire_idiv(null) || fire_fdiv(null, deq_fdiv_lrq1)

  tagq.io.enq.valid := fire_idiv(tagq.io.enq.ready) || fire_fdiv(tagq.io.enq.ready)
  tagq.io.enq.bits.pred := pred & io.lpq.bits.pred
  tagq.io.enq.bits.bank := bank
  tagq.io.enq.bits.selff := Bool(false) // FIXME
  tagq.io.enq.bits.addr := op.vd.id
  tagq.io.enq.bits.fusel := op.active.vidiv
  tagq.io.enq.bits.pack := pack

  io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    idiv.req.valid := fire_idiv(mask_idivs_req_ready(i), enq_idivs_req(i)) }
  io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    idiv.req.bits.fn := op.fn.vidu()
    idiv.req.bits.in0 := unpack_slice(io.lrqs.q(0).bits.data, i)
    idiv.req.bits.in1 := unpack_slice(io.lrqs.q(1).bits.data, i)
  }

  io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    fdiv.req.valid := fire_fdiv(mask_fdivs_req_ready(i), enq_fdivs_req(i)) }
  io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    fdiv.req.bits.fn := op.fn.vfdu()
    fdiv.req.bits.in0 := unpack_slice(io.lrqs.q(0).bits.data, i)
    fdiv.req.bits.in1 := unpack_slice(io.lrqs.q(1).bits.data, i)
  }

  io.rpred.fu.op.valid := opq.io.deq.fire && opq.io.deq.bits.active.vrpred
  io.rpred.fu.op.bits := opq.io.deq.bits.fn.vrpu()
  io.rpred.fu.req.valid := fire_rpred(io.rpred.fu.req.ready)
  io.rpred.fu.req.bits.active := pred
  io.rpred.fu.req.bits.pred := io.lpq.bits.pred

  io.rfirst.fu.op.valid := opq.io.deq.fire && opq.io.deq.bits.active.vrfirst
  io.rfirst.fu.op.bits := opq.io.deq.bits.fn.vrfu()
  io.rfirst.fu.req.valid := fire_rfirst(io.rfirst.fu.req.ready)
  io.rfirst.fu.req.bits.active := pred
  io.rfirst.fu.req.bits.pred := io.lpq.bits.pred
  io.rfirst.fu.req.bits.lsidx := strip_idx
  io.rfirst.fu.req.bits.in := Vec((0 until nSlices) map { unpack_slice(io.lrqs.q(0).bits.data, _) })

  val deq_idivs_resp = (0 until nSlices).map {  tagq.io.deq.bits.fusel.asBool && tagq.io.deq.bits.pred(_) }
  val deq_fdivs_resp = (0 until nSlices).map { !tagq.io.deq.bits.fusel.asBool && tagq.io.deq.bits.pred(_) }
  val mask_idivs_resp_valid = io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    !deq_idivs_resp(i) || idiv.resp.valid }
  val mask_fdivs_resp_valid = io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    !deq_fdivs_resp(i) || fdiv.resp.valid }

  val enq_bwqs = (0 until nBanks).map { tagq.io.deq.bits.active() && tagq.io.deq.bits.bank === UInt(_) }
  val mask_bwqs_ready = io.bwqs.zipWithIndex.map { case (bwq, i) => !enq_bwqs(i) || bwq.ready }

  def fire_bwq(exclude: Bool, include: Bool*) = {
    val rvs = tagq.io.deq.valid +: (mask_idivs_resp_valid ++ mask_fdivs_resp_valid ++ mask_bwqs_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  tagq.io.deq.ready := fire_bwq(tagq.io.deq.valid)
  io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    idiv.resp.ready := fire_bwq(mask_idivs_resp_valid(i), deq_idivs_resp(i)) }
  io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    fdiv.resp.ready := fire_bwq(mask_fdivs_resp_valid(i), deq_fdivs_resp(i)) }
  io.bwqs.zipWithIndex.map { case (bwq, i) =>
    bwq.valid := fire_bwq(mask_bwqs_ready(i), enq_bwqs(i)) }

  val wraw = Wire(new BankDataPredEntry)
  wraw.pred := tagq.io.deq.bits.pred
  wraw.data := Mux(tagq.io.deq.bits.fusel.asBool,
    repack_slice(io.idiv.fus.map(_.resp.bits.out)),
    repack_slice(io.fdiv.fus.map(_.resp.bits.out)))
  val wpack = repack_bank(tagq.io.deq.bits.pack, UInt(0), wraw)

  io.bwqs.map { bwq =>
    bwq.bits.selff := tagq.io.deq.bits.selff
    bwq.bits.addr := tagq.io.deq.bits.addr
    bwq.bits.data := wpack.data
    bwq.bits.mask := wpack.mask
  }

  io.idiv.ack.valid := fire_bwq(null, tagq.io.deq.bits.fusel.asBool)
  io.idiv.ack.bits.pred := tagq.io.deq.bits.pred
  io.fdiv.ack.valid := fire_bwq(null, !tagq.io.deq.bits.fusel.asBool)
  io.fdiv.ack.bits.pred := tagq.io.deq.bits.pred
  io.fdiv.ack.bits.exc := io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    dgate(tagq.io.deq.bits.pred(i), fdiv.resp.bits.exc) } reduce(_|_)

  val icntr = Module(new LookAheadCounter(0, maxLookAhead))
  icntr.suggestName("icntrInst")
  icntr.io.inc.cnt := UInt(1)
  icntr.io.inc.update := fire_bwq(null, tagq.io.deq.bits.fusel.asBool)
  icntr.io.dec <> io.ila

  val fcntr = Module(new LookAheadCounter(0, maxLookAhead))
  fcntr.suggestName("fcntrInst")
  fcntr.io.inc.cnt := UInt(1)
  fcntr.io.inc.update := fire_bwq(null, !tagq.io.deq.bits.fusel.asBool)
  fcntr.io.dec <> io.fla
}

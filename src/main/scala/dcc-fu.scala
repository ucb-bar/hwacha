package hwacha

import Chisel._
import DataGating._

class VDUTag extends VXUBundle with BankPred {
  val bank = UInt(width = log2Up(nBanks))
  val selff = Bool() // select ff if true
  val addr = UInt(width = math.max(log2Up(nSRAM), log2Up(nFF)))
  val fusel = Bits(width = 1) // because we have 2 units idiv/fdiv
}

class VDU extends VXUModule {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val op = Decoupled(new DCCOp).flip
    val ack = new DCCAckIO
    val pla = new CounterLookAheadIO().flip // lpq entry
    val qla = Vec.fill(nVDUOperands){new CounterLookAheadIO}.flip // lrq entries
    val ila = new CounterLookAheadIO().flip // idiv output entries
    val fla = new CounterLookAheadIO().flip // fdiv output entries
    val lpq = new LPQIO().flip
    val lrqs = Vec.fill(nVDUOperands){new LRQIO}.flip
    val bwqs = Vec.fill(nBanks){new BWQIO}
  }

  val ctrl = Module(new VDUCtrl)

  ctrl.io.op <> io.op
  ctrl.io.ila <> io.ila
  ctrl.io.fla <> io.fla
  ctrl.io.cfg <> io.cfg

  val lpq = Module(new Queue(new LPQEntry, nBanks+2))
  lpq.io.enq <> io.lpq
  ctrl.io.lpq <> lpq.io.deq

  val pcntr = Module(new LookAheadCounter(nBanks+2, nBanks+2))
  pcntr.io.inc.cnt := UInt(1)
  pcntr.io.inc.update := lpq.io.deq.fire()
  pcntr.io.dec <> io.pla

  for (i <- 0 until nVDUOperands) {
    val lrq = Module(new Queue(new LRQEntry, nBanks+2))
    lrq.io.enq <> io.lrqs(i)
    ctrl.io.lrqs(i) <> lrq.io.deq

    val cntr = Module(new LookAheadCounter(nBanks+2, nBanks+2))
    cntr.io.inc.cnt := UInt(1)
    cntr.io.inc.update := lrq.io.deq.fire()
    cntr.io.dec <> io.qla(i)
  }

  for (i <- 0 until nSlices) {
    val idiv = Module(new IDivSlice)
    val fdiv = Module(new FDivSlice)
    idiv.io <> ctrl.io.idiv.fus(i)
    fdiv.io <> ctrl.io.fdiv.fus(i)
  }

  io.ack.vidu <> ctrl.io.idiv.ack
  io.ack.vfdu <> ctrl.io.fdiv.ack
  io.bwqs <> ctrl.io.bwqs
}

class VDUCtrl extends VXUModule with Packing {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val op = Decoupled(new DCCOp).flip
    val ila = new CounterLookAheadIO().flip
    val fla = new CounterLookAheadIO().flip
    val lpq = new LPQIO().flip
    val lrqs = Vec.fill(nVDUOperands){new LRQIO}.flip

    val idiv = new Bundle {
      val fus = Vec.fill(nSlices){new IDivIO}
      val ack = Valid(new VIDUAck)
    }

    val fdiv = new Bundle {
      val fus = Vec.fill(nSlices){new FDivIO}
      val ack = Valid(new VFDUAck)
    }

    val bwqs = Vec.fill(nBanks){new BWQIO}
  }

  val opq = Module(new Queue(new DCCOp, nDCCOpQ))
  opq.io.enq <> io.op

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)
  val op = Reg(new DCCOp)
  val bank = Reg(UInt(width = log2Up(nBanks)))

  val fire = Bool()
  val ecnt = Mux(op.vlen > UInt(nSlices), UInt(nSlices), op.vlen)
  val vlen_next = op.vlen - ecnt
  val pred = Vec((0 until nSlices).map(UInt(_) < ecnt)).toBits
  val idiv_active = op.active.vidiv

  opq.io.deq.ready := Bool(false)

  switch (state) {
    is (s_idle) {
      opq.io.deq.ready := Bool(true)
      when (opq.io.deq.valid) {
        state := s_busy
        op := opq.io.deq.bits
        bank := UInt(0)
      }
    }

    is (s_busy) {
      when (fire) {
        op.vlen := vlen_next 
        bank := bank + UInt(1)
        when (bank === UInt(nBanks-1)) {
          op.vd.id := op.vd.id + io.cfg.vstride
        }
        when (vlen_next === UInt(0)) {
          state := s_idle
        }
      }
    }
  }

  val tagq = Module(new Queue(new VDUTag, nDecoupledUnitWBQueue))

  val deq_lrq1 = op.fn.vfdu().op_is(FD_DIV)
  val mask_lrq1_valid = !deq_lrq1 || io.lrqs(1).valid
  val enq_idivs_req = (0 until nSlices).map { i => pred(i) && io.lpq.bits.pred(i) }
  val enq_fdivs_req = (0 until nSlices).map { i => pred(i) && io.lpq.bits.pred(i) }
  val mask_idivs_req_ready = io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    !enq_idivs_req(i) || idiv.req.ready }
  val mask_fdivs_req_ready = io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    !enq_fdivs_req(i) || fdiv.req.ready }

  def fire_idiv(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      state === s_busy, op.active.vidiv,
      io.lpq.valid, io.lrqs(0).valid, io.lrqs(1).valid,
      tagq.io.enq.ready) ++ mask_idivs_req_ready
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  def fire_fdiv(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      state === s_busy, op.active.vfdiv,
      io.lpq.valid, io.lrqs(0).valid, mask_lrq1_valid,
      tagq.io.enq.ready) ++ mask_fdivs_req_ready
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  fire := fire_idiv(null) || fire_fdiv(null)
  io.lpq.ready := fire_idiv(io.lpq.valid) || fire_fdiv(io.lpq.valid)
  io.lrqs(0).ready := fire_idiv(io.lrqs(0).valid) || fire_fdiv(io.lrqs(0).valid)
  io.lrqs(1).ready := fire_idiv(io.lrqs(1).valid) || fire_fdiv(mask_lrq1_valid, deq_lrq1)
  tagq.io.enq.valid := fire_idiv(tagq.io.enq.ready) || fire_fdiv(tagq.io.enq.ready)
  io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    idiv.req.valid := fire_idiv(mask_idivs_req_ready(i), enq_idivs_req(i)) }
  io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    fdiv.req.valid := fire_fdiv(mask_fdivs_req_ready(i), enq_fdivs_req(i)) }

  tagq.io.enq.bits.pred := pred
  tagq.io.enq.bits.bank := bank
  tagq.io.enq.bits.selff := Bool(false) // FIXME
  tagq.io.enq.bits.addr := op.vd.id
  tagq.io.enq.bits.fusel := op.active.vidiv

  io.idiv.fus.zipWithIndex.map { case (idiv, i) =>
    idiv.req.bits.fn := op.fn.vidu()
    idiv.req.bits.in0 := unpack_slice(io.lrqs(0).bits.data, i)
    idiv.req.bits.in1 := unpack_slice(io.lrqs(1).bits.data, i)
  }

  io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    fdiv.req.bits.fn := op.fn.vfdu()
    fdiv.req.bits.in0 := unpack_slice(io.lrqs(0).bits.data, i)
    fdiv.req.bits.in1 := unpack_slice(io.lrqs(1).bits.data, i)
  }

  val deq_idivs_resp = (0 until nSlices).map {  tagq.io.deq.bits.fusel.toBool && tagq.io.deq.bits.pred(_) }
  val deq_fdivs_resp = (0 until nSlices).map { !tagq.io.deq.bits.fusel.toBool && tagq.io.deq.bits.pred(_) }
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

  io.bwqs.map { bwq =>
    bwq.bits.selff := tagq.io.deq.bits.selff
    bwq.bits.addr := tagq.io.deq.bits.addr
    bwq.bits.data := Mux(tagq.io.deq.bits.fusel.toBool, repack_slice(io.idiv.fus.map(_.resp.bits.out)),
                                                        repack_slice(io.fdiv.fus.map(_.resp.bits.out)))
    bwq.bits.mask := FillInterleaved(regLen/8, tagq.io.deq.bits.pred)
  }

  io.idiv.ack.valid := fire_bwq(null, tagq.io.deq.bits.fusel.toBool)
  io.idiv.ack.bits.pred := tagq.io.deq.bits.pred
  io.fdiv.ack.valid := fire_bwq(null, !tagq.io.deq.bits.fusel.toBool)
  io.fdiv.ack.bits.pred := tagq.io.deq.bits.pred
  io.fdiv.ack.bits.exc := io.fdiv.fus.zipWithIndex.map { case (fdiv, i) =>
    dgate(tagq.io.deq.bits.pred(i), fdiv.resp.bits.exc) } reduce(_|_)

  val icntr = Module(new LookAheadCounter(0, maxLookAhead))
  icntr.io.inc.cnt := UInt(1)
  icntr.io.inc.update := fire_bwq(null, tagq.io.deq.bits.fusel.toBool)
  icntr.io.dec <> io.ila

  val fcntr = Module(new LookAheadCounter(0, maxLookAhead))
  fcntr.io.inc.cnt := UInt(1)
  fcntr.io.inc.update := fire_bwq(null, !tagq.io.deq.bits.fusel.toBool)
  fcntr.io.dec <> io.fla
}

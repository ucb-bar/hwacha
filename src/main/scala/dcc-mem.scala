package hwacha

import Chisel._
import cde.Parameters
import DataGating._

class BPQLookAheadIO(implicit p: Parameters) extends VXUBundle()(p) with LookAheadIO {
  val mask = Bits(OUTPUT, nBanks)
}

class BRQLookAheadIO(implicit p: Parameters) extends VXUBundle()(p) with LookAheadIO {
  val mask = Bits(OUTPUT, nBanks)
}

class VGU(implicit p: Parameters) extends VXUModule()(p) with Packing {
  val io = new DCCIssueIO {
    val pla = new CounterLookAheadIO().flip // lpq entry
    val qla = new CounterLookAheadIO().flip // lrq entry
    val lpq = new LPQIO().flip
    val lrq = new LRQIO().flip
    val vaq = new VVAQIO
  }

  val opq = Module(new Queue(new DCCOp, nDCCOpQ))
  opq.io.enq <> io.op

  val lpq = Module(new Queue(new LPQEntry, nBanks+2))
  lpq.io.enq <> io.lpq

  val lrq = Module(new Queue(new LRQEntry, nBanks+2))
  lrq.io.enq <> io.lrq

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)
  val op = Reg(new DCCOp)
  val slice = Reg(UInt(width = log2Up(nSlices)))

  // FIXME: I didn't have time to generalize, logic explained below
  require(nSlices == 2)

  // if slice === UInt(1), shave off last bit
  val pred = lpq.io.deq.bits.pred & Mux(slice === UInt(0), UInt(3), UInt(2))
  // dequeue lq when one hot
  val deq_lq = pred =/= UInt(3)
  // enqueue vaq when there's something
  val active_entry = pred =/= UInt(0)
  // find slice_next when more then two elements are alive
  val slice_next = Mux(pred === UInt(3), UInt(1), UInt(0))
  // find first one for pick
  val pick = Mux(pred(0), UInt(0), UInt(1))
  // process 2 elements only when pred is empty or popcount of 1
  // otherwise process 1 element
  val ecnt = Mux(pred =/= UInt(3) && slice === UInt(0), UInt(2), UInt(1))
  val vlen_cnt = Mux(ecnt > op.vlen, op.vlen, ecnt)
  val vlen_next = op.vlen - vlen_cnt

  val mask_lrq_valid = !active_entry || lrq.io.deq.valid
  val mask_vaq_ready = !active_entry || io.vaq.ready

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      state === s_busy,
      lpq.io.deq.valid, mask_lrq_valid, mask_vaq_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  opq.io.deq.ready := Bool(false)

  switch (state) {
    is (s_idle) {
      opq.io.deq.ready := Bool(true)
      when (opq.io.deq.valid) {
        state := s_busy
        op := opq.io.deq.bits
        slice := UInt(0)
      }
    }

    is (s_busy) {
      when (fire(null)) {
        op.vlen := vlen_next
        slice := slice_next
        when (vlen_next === UInt(0)) {
          state := s_idle
        }
      }
    }
  }

  lpq.io.deq.ready := fire(lpq.io.deq.valid, deq_lq)
  lrq.io.deq.ready := fire(mask_lrq_valid, deq_lq, active_entry)
  io.vaq.valid := fire(mask_vaq_ready, active_entry)

  io.vaq.bits.addr := MuxLookup(pick, Bits(0), (0 until nSlices) map {
    i => UInt(i) -> unpack_slice(lrq.io.deq.bits.data, i) })

  // using fire fn, just to be consistent with rcntr
  // don't really need to do this, because lpq entry should always be there
  val pcntr = Module(new LookAheadCounter(nBanks+2, nBanks+2))
  pcntr.io.inc.cnt := UInt(1)
  pcntr.io.inc.update := fire(null, deq_lq)
  pcntr.io.dec <> io.pla

  // have to update with fire fn, since there are cases where the lrq is empty
  val rcntr = Module(new LookAheadCounter(nBanks+2, nBanks+2))
  rcntr.io.inc.cnt := UInt(1)
  rcntr.io.inc.update := fire(null, deq_lq)
  rcntr.io.dec <> io.qla
}

class VPU(implicit p: Parameters) extends VXUModule()(p) with BankLogic {
  val io = new DCCIssueIO {
    val la = new BPQLookAheadIO().flip
    val bpqs = Vec.fill(nBanks)(new BPQIO).flip
    val pred = Decoupled(Bits(width = nPredSet))
    val lpred = Decoupled(Bits(width = nPredSet))
    val spred = Decoupled(Bits(width = nPredSet))
  }

  require(nBanks*2 == nPredSet)

  val opq = Module(new Queue(new DCCOp, nDCCOpQ))
  opq.io.enq <> io.op

  val bpqs = (io.bpqs zipWithIndex) map { case (enq, i) =>
    val bpq = Module(new Queue(new BPQEntry, nBPQ))
    val placntr = Module(new LookAheadCounter(nBPQ, nBPQ))
    val en = io.la.mask(i)
    bpq.io.enq <> enq
    placntr.io.inc.cnt := UInt(1)
    placntr.io.inc.update := bpq.io.deq.fire()
    placntr.io.dec.cnt := UInt(1)
    placntr.io.dec.reserve := io.la.reserve && en
    (bpq.io.deq, !en || placntr.io.dec.available)
  }
  io.la.available := bpqs.map(_._2).reduce(_ && _)
  val bpqs_deq = Vec(bpqs.map(_._1))

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)
  val op = Reg(new DCCOp)

  val strip = Mux(op.vlen > UInt(8), UInt(8), op.vlen)
  val vlen_next = op.vlen - strip

  val deq_bpqs = strip_to_bmask(strip)
  val mask_bpqs_valid = (bpqs_deq zipWithIndex) map { case (bpq, i) =>
    !deq_bpqs(i) || bpq.valid }
  val enq_lpred = op.active.enq_vlu()
  val enq_spred = op.active.enq_vsu()
  val mask_lpred_ready = !enq_lpred || io.lpred.ready
  val mask_spred_ready = !enq_spred || io.spred.ready

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      state === s_busy,
      io.pred.ready, mask_lpred_ready, mask_spred_ready) ++ mask_bpqs_valid
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  opq.io.deq.ready := Bool(false)

  switch (state) {
    is (s_idle) {
      opq.io.deq.ready := Bool(true)
      when (opq.io.deq.valid) {
        state := s_busy
        op := opq.io.deq.bits
      }
    }

    is (s_busy) {
      when (fire(null)) {
        op.vlen := vlen_next
        when (vlen_next === UInt(0)) {
          state := s_idle
        }
      }
    }
  }

  (bpqs_deq zipWithIndex) map { case (bpq, i) =>
    bpq.ready := fire(mask_bpqs_valid(i), deq_bpqs(i)) }
  io.pred.valid := fire(io.pred.ready)
  io.lpred.valid := fire(mask_lpred_ready, enq_lpred)
  io.spred.valid := fire(mask_spred_ready, enq_spred)

  val pred = Vec((bpqs_deq zipWithIndex) map { case (bpq, i) =>
    dgate(deq_bpqs(i), bpq.bits.pred) }).toBits
  io.pred.bits := pred
  io.lpred.bits := pred
  io.spred.bits := pred
}

class VSU(implicit p: Parameters) extends VXUModule()(p) {
  val io = new DCCIssueIO {
    val la = new BRQLookAheadIO().flip
    val brqs = Vec.fill(nBanks)(new BRQIO).flip
    val vsdq = new VSDQIO

    val pred = Decoupled(Bits(width = nPredSet)).flip
  }

  private val lgslices = log2Up(nSlices)
  private val lgbanks = log2Up(nBanks)
  private val lgbatch = log2Up(nBatch)

  val opq = Module(new Queue(io.op.bits, nDCCOpQ))
  opq.io.enq <> io.op
  opq.io.deq.ready := Bool(false)

  val op = Reg(new DCCOp)
  val mt = DecodedMemType(op.fn.vmu().mt)
  val mts = Seq(mt.d, mt.w, mt.h, mt.b)
  val mtb = Seq(SZ_D, SZ_W, SZ_H, SZ_B)

  require(wBank <= tlDataBits)

  /* Maximum number of active BRQs per VSDQ entry */
  val qcnts = mtb.map { sz =>
    val b = sz << lgslices
    require(tlDataBits % b == 0)
    math.min(tlDataBits / b, nBanks)
  }
  val qcnt_max = Mux1H(mts, qcnts.map(UInt(_)))
  val ecnt_max = Cat(qcnt_max, UInt(0, lgslices))

  val vlen_next = op.vlen.zext - ecnt_max
  val vlen_end = (vlen_next <= SInt(0))
  val ecnt = Mux(vlen_end, op.vlen(lgbatch, 0), ecnt_max)
  val qcnt = Ceil(ecnt, lgslices)

  val index = Reg(UInt(width = lgbanks))
  val index_next = index + qcnt_max
  val index_end = (index_next(lgbanks-1, 0) === UInt(0))

  //--------------------------------------------------------------------\\
  // predication / masking
  //--------------------------------------------------------------------\\

  val brqs_sel_base = EnableDecoder(qcnt, nBanks)
  val brqs_sel_bits = (brqs_sel_base << index)(nBanks-1, 0)
  val brqs_sel = (0 until nBanks).map(brqs_sel_bits(_))

  val predq = Module(new Queue(io.pred.bits, nDCCPredQ))
  predq.io.enq <> io.pred
  private val pred = predq.io.deq

  require(nPredSet == nBatch)
  val brqs_pred = pred.bits.toBools.grouped(nSlices).map(
    xs => xs.reduce(_ || _)).toSeq
  val brqs_mask = brqs_sel.zip(brqs_pred)
  val brqs_en = brqs_mask.map { case (sel, pred) => sel && pred }

  //--------------------------------------------------------------------\\
  // bank read queues
  //--------------------------------------------------------------------\\

  val brqs = io.brqs.map { enq =>
    val brq = Module(new Queue(enq.bits, nBRQ))
    brq.io.enq <> enq
    brq.io.deq.ready := Bool(false)
    brq.io.deq
  }

  val brqs_la = brqs.zipWithIndex.map { case (brq, i) =>
    val scntr = Module(new LookAheadCounter(nBRQ, maxSLA))
    val en = io.la.mask(i)
    scntr.io.inc.cnt := UInt(1)
    scntr.io.inc.update := Bool(false)
    scntr.io.dec.cnt := UInt(1)
    scntr.io.dec.reserve := io.la.reserve && en
    (scntr, !en || scntr.io.dec.available)
  }
  val brqs_cntr = brqs_la.map(_._1)
  io.la.available := brqs_la.map(_._2).reduce(_ && _)

  /* Preemptively increment the lookahead counter on false predicates
   * without waiting for the actual sequencer reservation, but constrain
   * runahead to prevent counter overflow.
   * The maxSLA parameter determines how much decoupling is possible.
   */
  val brqs_valid = brqs.zip(brqs_mask).zip(brqs_cntr).map {
    case ((brq, (sel, pred)), scntr) =>
      !sel || Mux(pred, brq.valid, !scntr.io.full)
  }
  val vsdq_en = brqs_en.reduce(_ || _)
  val vsdq_ready = !vsdq_en || io.vsdq.ready

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = brqs_valid ++ Seq(pred.valid, vsdq_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  pred.ready := Bool(false)
  io.vsdq.valid := Bool(false)

  require(nPredSet == nBatch)
  val pred_deq = index_end || vlen_end

  //--------------------------------------------------------------------\\
  // permutation network
  //--------------------------------------------------------------------\\

  private def repack(sz: Int, qcnt: Int) = {
    require(sz <= regLen)
    val elts = for (q <- brqs; i <- (0 until wBank by regLen))
      yield q.bits.data(sz-1+i, i)
    val sets = elts.grouped(qcnt << lgslices).map(xs =>
      Cat(xs.reverse)).toIterable
    if (sets.size > 1) {
      val sel = index(lgbanks-1, log2Ceil(qcnt))
      Vec(sets)(sel)
    } else sets.head
  }

  io.vsdq.bits.data := Mux1H(mts, mtb.zip(qcnts).map {
    case (sz, cnt) => repack(sz, cnt)
  })

  //--------------------------------------------------------------------\\
  // control
  //--------------------------------------------------------------------\\

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      opq.io.deq.ready := Bool(true)
      when (opq.io.deq.valid) {
        state := s_busy
        op := opq.io.deq.bits
      }
      index := UInt(0)
    }

    is (s_busy) {
      pred.ready := fire(pred.valid, pred_deq)
      brqs.zip(brqs_valid.zip(brqs_en)).foreach {
        case (brq, (valid, en)) =>
          brq.ready := fire(valid, en)
      }
      io.vsdq.valid := fire(vsdq_ready, vsdq_en)

      when (fire(null)) {
        brqs_cntr.zip(brqs_sel).foreach {
          case (scntr, sel) =>
            scntr.io.inc.update := sel
        }

        index := index_next
        op.vlen := vlen_next
        when (vlen_end) {
          state := s_idle
        }
      }
    }
  }
}

class VLUEntry(implicit p: Parameters) extends VXUBundle()(p) {
  val eidx = UInt(width = bVLen - log2Up(nBatch))
  val data = Bits(width = wBank)
  val mask = Bits(width = nSlices)
}

class VLU(implicit p: Parameters) extends VXUModule()(p) {
  val io = new DCCIssueIO {
    val vldq = new VLDQIO().flip
    val pred = Decoupled(Bits(width = nPredSet)).flip
    val bwqs = Vec.fill(nBanks)(new BWQIO)
    val la = new CounterLookAheadIO().flip

    val cfg = new HwachaConfigIO().flip
  }

  private val lgbanks = log2Up(nBanks)
  private val lgslices = log2Up(nSlices)
  private val lgbatch = log2Up(nBatch)

  val opq = Module(new Queue(io.op.bits, nDCCOpQ))
  opq.io.enq <> io.op
  opq.io.deq.ready := Bool(false)

  val op = Reg(new DCCOp)
  val mt = DecodedMemType(op.fn.vmu().mt)
  private val mts = Seq(mt.d, mt.w, mt.h, mt.b)

  val eidx = Reg(UInt(width = bVLen))
  val eidx_next = eidx + io.la.cnt
  val eidx_end = (eidx_next === op.vlen)

  val retire = Mux(io.la.reserve, io.la.cnt >> lgbatch, UInt(0))
  assert(!io.la.reserve || eidx_end ||
    (io.la.cnt(lgbatch-1, 0) === UInt(0)),
    "VLU: retire count not a batch multiple")

  //--------------------------------------------------------------------\\
  // predication
  //--------------------------------------------------------------------\\

  val predq = Module(new Queue(io.pred.bits, nDCCPredQ))
  predq.io.enq <> io.pred
  predq.io.deq.ready := Bool(false)

  require(nPredSet == nBatch)
  val pidx = Reg(UInt(width = log2Down(nvlreq) - lgbatch))
  val pcnt = Reg(UInt(width = bVLen - lgbatch))
  val pidx_end = (pidx === UInt((nvlreq - 1) >> lgbatch))
  val pcnt_end = (pcnt === UInt(0))

  val pred_base = Mux(predq.io.deq.valid, ~predq.io.deq.bits, UInt(0))
  val pred_shift = Cat(pidx, UInt(0, lgbatch))
  val pred = (pred_base << pred_shift)(nvlreq-1, 0)

  val pidx_step = predq.io.deq.fire()
  val pidx_next = pidx.zext + pidx_step.toUInt - retire
  pidx := pidx_next
  when (pidx_step) {
    pcnt := pcnt - UInt(1)
  }

  //--------------------------------------------------------------------\\
  // control
  //--------------------------------------------------------------------\\

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)
  val sink = Reg(Bool())

  switch (state) {
    is (s_idle) {
      eidx := UInt(0)
      pidx := UInt(0)
      sink := Bool(true)

      opq.io.deq.ready := Bool(true)
      when (opq.io.deq.valid) {
        state := s_busy
        op := opq.io.deq.bits
        pcnt := opq.io.deq.bits.vlen(bVLen-1, lgbatch) +
          opq.io.deq.bits.vlen(lgbatch-1, 0).orR.toUInt /* ceil */

        assert(opq.io.deq.bits.vd.valid, "VLU: invalid vd")
      }
    }

    is (s_busy) {
      when (io.la.reserve) {
        eidx := eidx_next
        when (eidx_end) {
          state := s_idle
        }
      }

      predq.io.deq.ready := !(pidx_end || pcnt_end)
      /* NOTE: Predicates should always arrive before the corresponding
         load due to VMU latency, thus precluding this race condition */
      assert(pidx_next >= SInt(0), "VLU: pidx underflow")
    }
  }

  // Avoid prematurely accepting responses for the next memory operation
  // before the current one has finished
  val vldq_valid = io.vldq.valid && sink
  when (io.vldq.fire() && io.vldq.bits.meta.last) {
    sink := Bool(false)
  }

  private val vldq = io.vldq
  private val meta = vldq.bits.meta

  //--------------------------------------------------------------------\\
  // permutation network
  //--------------------------------------------------------------------\\

  val eidx_slice = meta.eidx(lgslices-1, 0)
  val eidx_batch = meta.eidx(lgbatch-1, 0)
  val eidx_bank = meta.eidx(lgbatch-1, lgslices)
  val eidx_reg = meta.eidx(bVLen-1, lgbatch)
  val eidx_reg_next = eidx_reg + UInt(1)

  val rotamt = eidx_batch - meta.epad

  private def rotate[T <: Data](gen: T, in: Iterable[T]) = {
    val rot = Module(new Rotator(gen, in.size, nBatch))
    val out = Vec.fill(nBatch)(gen.clone)
    rot.io.sel := rotamt
    rot.io.in := in
    out := rot.io.out
    out
  }

  private def extend[T <: Bits](in: T, sz: Int) =
    if (sz < regLen) Cat(Fill(regLen-sz, in(sz-1) && mt.signed), in) else in

  private def rotate_data[T <: Bits](data: T, sz: Int) = {
    val w = data.getWidth
    require(w > 0)
    val in = (0 until w by sz).map(i => data(i+sz-1, i))
    require(in.size <= nBatch)
    val out = rotate(Bits(), in)
    Vec(out.map(extend(_, sz)))
  }

  private val tlDataMidBits = tlDataBits >> 1

  val load = vldq.bits.data
  val load_b = Mux(meta.epad(tlByteAddrBits - 1),
    load(tlDataBits-1, tlDataMidBits),
    load(tlDataMidBits-1, 0))

  val data_d = rotate_data(load, SZ_D)
  val data_w = rotate_data(load, SZ_W)
  val data_h = rotate_data(load, SZ_H)
  val data_b = rotate_data(load_b, SZ_B)
  val data = Mux1H(mts, Seq(data_d, data_w, data_h, data_b))

  //--------------------------------------------------------------------\\
  // masking / conflict arbitration
  //--------------------------------------------------------------------\\

  require(nSlices == 2)
  val tick = Reg(init = Bool(true))

  /* When the load represents a full batch of elements and meta.eidx is
   * odd, the first and last elements reside in the same bank but in
   * different SRAM entries.  If both are present, they must be written
   * separately over two cycles.
   */
  val slice_unaligned = (eidx_slice =/= UInt(0)) && (meta.epad === UInt(0))
  val slice_used = slice_unaligned && meta.mask(0)
  val slice_free = slice_unaligned && !meta.mask(0)
  val slice_conflict = slice_used && meta.mask(nBatch-1)
  val tick_next = !(slice_conflict && tick)

  val bwqs_fire = Bool()
  when (bwqs_fire) {
    tick := tick_next
  }

  val mask_head = tick
  val mask_tail = Mux(tick, !slice_used, Bool(true))
  val mask_beat = Cat(mask_tail, Fill(nBatch-1, mask_head))
  val mask_base = meta.mask & mask_beat
  val mask = rotate(Bool(), mask_base.toBools)

  /* Handle intra-load eidx increment */
  val eidx_step_head = EnableDecoder(eidx_bank, nBanks)
  val eidx_step_tail = Mux(tick, slice_free, Bool(true)) << eidx_bank
  val eidx_step = eidx_step_head | eidx_step_tail

  //--------------------------------------------------------------------\\
  // bank write queues
  //--------------------------------------------------------------------\\

  private def merge[T <: Data](in: Seq[T]): Seq[Bits] =
    in.grouped(nSlices).map(xs => Cat(xs.reverse)).toSeq

  val bwqs_data = merge(data)
  val bwqs_mask = merge(mask)
  val bwqs_en = bwqs_mask.map(_.orR)

  val wb_update = Vec.fill(nBanks)(Bits(width = nvlreq))

  val bwqs = io.bwqs.zipWithIndex.map { case (deq, i) =>
    val bwq = Module(new Queue(new VLUEntry, nBWQ))

    bwq.io.enq.bits.eidx := Mux(eidx_step(i), eidx_reg_next, eidx_reg)
    bwq.io.enq.bits.data := bwqs_data(i)
    bwq.io.enq.bits.mask := bwqs_mask(i)

    val wb_eidx = Cat(bwq.io.deq.bits.eidx, UInt(i, lgbanks), UInt(0, lgslices))
    val wb_shift = wb_eidx - eidx
    val wb_mask = bwq.io.deq.bits.mask & Fill(nSlices, bwq.io.deq.fire())
    wb_update(i) := wb_mask << wb_shift

    assert(!bwq.io.deq.valid ||
      ((eidx <= wb_eidx) && (wb_eidx < eidx + UInt(nvlreq))),
      "VLU: out-of-bounds eidx at writeback; check VMU valve logic");

    deq.valid := bwq.io.deq.valid
    bwq.io.deq.ready := deq.ready

    deq.bits.selff := Bool(false) // FIXME
    deq.bits.addr := op.vd.id + (bwq.io.deq.bits.eidx * io.cfg.vstride)
    deq.bits.data := bwq.io.deq.bits.data
    deq.bits.mask := FillInterleaved(regLen >> 3, bwq.io.deq.bits.mask)

    bwq.io.enq
  }

  val bwqs_ready = bwqs.zip(bwqs_en).map { case (bwq, en) => !en || bwq.ready }

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = vldq_valid +: bwqs_ready
    (rvs.filter(_.ne(exclude)) ++ include).reduce(_ && _)
  }

  val bwqs_ready_all = fire(vldq_valid)
  bwqs_fire := bwqs_ready_all && vldq_valid
  vldq.ready := bwqs_ready_all && tick_next && sink
  bwqs.zip(bwqs_ready.zip(bwqs_en)).foreach { case (bwq, (ready, en)) =>
    bwq.valid := fire(ready, en)
  }

  //--------------------------------------------------------------------\\
  // writeback status
  //--------------------------------------------------------------------\\

  val wb_status = Reg(Bits(width = nvlreq))
  val wb_next = wb_status | wb_update.reduce(_ | _) | pred
  val wb_shift = Cat(retire, UInt(0, lgbatch))
  val wb_locnt = CTZ(~wb_status, maxLookAhead)

  wb_status := Bits(0)
  io.la.available := Bool(false)

  when (state === s_busy) {
    wb_status := (wb_next >> wb_shift)
    io.la.available := (wb_locnt >= io.la.cnt)
  }
}

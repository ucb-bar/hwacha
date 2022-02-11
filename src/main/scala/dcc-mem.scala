package hwacha

import Chisel._
import freechips.rocketchip.config._
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
  opq.suggestName("opqInst")
  opq.io.enq <> io.op

  val lpq = Module(new Queue(new LPQEntry, nBanks+2))
  lpq.suggestName("lpqInst")
  lpq.io.enq <> io.lpq

  val lrq = Module(new Queue(new LRQEntry, nBanks+2))
  lrq.suggestName("lrqInst")
  lrq.io.enq <> io.lrq

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)
  val op = Reg(new DCCOp)
  val slice = Reg(UInt(width = bSlices))

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
  pcntr.suggestName("pcntrInst")
  pcntr.io.inc.cnt := UInt(1)
  pcntr.io.inc.update := fire(null, deq_lq)
  pcntr.io.dec <> io.pla

  // have to update with fire fn, since there are cases where the lrq is empty
  val rcntr = Module(new LookAheadCounter(nBanks+2, nBanks+2))
  rcntr.suggestName("rcntrInst")
  rcntr.io.inc.cnt := UInt(1)
  rcntr.io.inc.update := fire(null, deq_lq)
  rcntr.io.dec <> io.qla
}

class VPU(implicit p: Parameters) extends VXUModule()(p) with BankLogic {
  val io = new DCCIssueIO {
    val la = new BPQLookAheadIO().flip
    val bpqs = Vec(nBanks, new BPQIO).flip
    val pred = Decoupled(new PredEntry)
    val lpred = Decoupled(Bits(width = nStrip))
    val spred = Decoupled(Bits(width = nStrip))
  }

  require(nBanks*2 == nStrip)

  val opq = Module(new Queue(new DCCOp, nDCCOpQ))
  opq.suggestName("opqInst")
  opq.io.enq <> io.op

  val bpqs = (io.bpqs zipWithIndex) map { case (enq, i) =>
    val bpq = Module(new Queue(new BPQEntry, nBPQ))
    bpq.suggestName("bpqInst")
    val placntr = Module(new LookAheadCounter(nBPQ, nBPQ))
    placntr.suggestName("placntrInst")
    val en = io.la.mask(i)
    bpq.io.enq <> enq
    placntr.io.inc.cnt := UInt(1)
    placntr.io.inc.update := bpq.io.deq.fire
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
    dgate(deq_bpqs(i), bpq.bits.pred(nSlices-1,0)) }).asUInt
  io.pred.bits.pred := pred
  io.lpred.bits := pred
  io.spred.bits := pred
}

class VSU(implicit p: Parameters) extends VXUModule()(p)
  with MemParameters {
  val io = new DCCIssueIO {
    val la = new BRQLookAheadIO().flip
    val brqs = Vec(nBanks, new BRQIO).flip
    val vsdq = new VSDQIO

    val pred = Decoupled(Bits(width = nStrip)).flip
  }

  val opq = Module(new Queue(io.op.bits, nDCCOpQ))
  opq.suggestName("opqInst")
  opq.io.enq <> io.op
  opq.io.deq.ready := Bool(false)

  val op = Reg(new DCCOp)
  val mt = DecodedMemType(op.fn.vmu().mt)
  val mts = Seq(mt.d, mt.w, mt.h, mt.b)
  val mtb = Seq(SZ_D, SZ_W, SZ_H, SZ_B)

  require(wBank <= tlDataBits)

  /* Maximum number of active BRQs per VSDQ entry */
  val qcnts = mtb.map { sz =>
    val b = sz << bSlices
    require(tlDataBits % b == 0)
    math.min(tlDataBits / b, nBanks)
  }
  val qcnt_max = Mux1H(mts, qcnts.map(UInt(_)))
  val ecnt_max = Cat(qcnt_max, UInt(0, bSlices))

  val vlen_next = op.vlen.zext - ecnt_max.zext
  val vlen_end = (vlen_next <= SInt(0))
  val ecnt = Mux(vlen_end, op.vlen(bStrip, 0), ecnt_max)
  val qcnt = Ceil(ecnt, bSlices)

  val index = Reg(UInt(width = bBanks))
  val index_next = index + qcnt_max
  val index_end = (index_next(bBanks-1, 0) === UInt(0))

  //--------------------------------------------------------------------\\
  // predication / masking
  //--------------------------------------------------------------------\\

  val brqs_sel_base = EnableDecoder(qcnt, nBanks)
  val brqs_sel_bits = (brqs_sel_base << index)(nBanks-1, 0)
  val brqs_sel = (0 until nBanks).map(brqs_sel_bits(_))

  val predq = Module(new Queue(io.pred.bits, nDCCPredQ))
  predq.suggestName("predqInst")
  predq.io.enq <> io.pred
  private val pred = predq.io.deq

  val brqs_pred = pred.bits.asBools.grouped(nSlices).map(
    xs => xs.reduce(_ || _)).toSeq
  val brqs_mask = brqs_sel.zip(brqs_pred)
  val brqs_en = brqs_mask.map { case (sel, pred) => sel && pred }

  //--------------------------------------------------------------------\\
  // bank read queues
  //--------------------------------------------------------------------\\

  val brqs = io.brqs.map { enq =>
    val brq = Module(new Queue(enq.bits, nBRQ))
    brq.suggestName("brqInst")
    brq.io.enq <> enq
    brq.io.deq.ready := Bool(false)
    brq.io.deq
  }

  val brqs_la = brqs.zipWithIndex.map { case (brq, i) =>
    val scntr = Module(new LookAheadCounter(nBRQ, maxSLA))
    scntr.suggestName("scntrInst")
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

  val pred_deq = index_end || vlen_end

  //--------------------------------------------------------------------\\
  // permutation network
  //--------------------------------------------------------------------\\

  private def repack(sz: Int, qcnt: Int) = {
    require(sz <= regLen)
    val elts = for (q <- brqs; i <- (0 until wBank by regLen))
      yield q.bits.data(sz-1+i, i)
    val sets = elts.grouped(qcnt << bSlices).map(xs =>
      Cat(xs.reverse)).toSeq
    if (sets.size > 1) {
      val sel = index(bBanks-1, log2Ceil(qcnt))
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
        op.vlen := vlen_next.asUInt
        when (vlen_end) {
          state := s_idle
        }
      }
    }
  }
}

class VLUEntry(implicit p: Parameters) extends VXUBundle()(p)
  with VLUSelect with BankData with BankPred {
  val eidx = UInt(width = bVLen - log2Up(nStrip))
}

class VLU(implicit p: Parameters) extends VXUModule()(p)
  with MemParameters with PackLogic {
  val io = new DCCIssueIO {
    val vldq = new VLDQIO().flip
    val pred = Decoupled(Bits(width = nStrip)).flip
    val bwqs = Vec(nBanks, new BWQIO)
    val la = new CounterLookAheadIO().flip

    val map = new VLUSelectIO
    val cfg = new HwachaConfigIO().flip
  }

  private val vldq = io.vldq
  private val meta = vldq.bits.meta

  //--------------------------------------------------------------------\\
  // control
  //--------------------------------------------------------------------\\

  val opq = Module(new Queue(io.op.bits, nDCCOpQ))
  opq.suggestName("opqInst")
  opq.io.enq <> io.op

  val map = Module(new VLUMapper)
  map.suggestName("mapInst")
  map.io.op <> opq.io.deq
  map.io.free := Bool(false)
  io.map <> map.io.use

  val busy = map.io.busy
  val issue = map.io.use.fire

  /* NOTE: To avoid the need for a full shifter and special masking of
   * final predicate entries, vectors always begin at an nStrip-aligned
   * boundary in the writeback bitmap.  Extra padding proportional to
   * the maximum number of simultaneous vectors is therefore required in
   * case the vector lengths are not a multiple of nStrip.
   */
  require(nvlreq % nStrip == 0)
  private val szwb = nvlreq + ((nVLU - 1) * (nStrip - 1))
  private val lgwb = log2Up(szwb)

  val op = Reg(Vec(nVLU, new DCCOp))
  val wb = Reg(init = Bits(0, szwb))

  val vidx = map.io.vidx
  val vidx_tail = map.io.use.bits.vidx

  val op_head = op(vidx)
  val op_load = op(meta.vidx)
  val mt = DecodedMemType(op_load.fn.vmu().mt)
  private val mts = Seq(mt.d, mt.w, mt.h, mt.b)

  val rcnt = Ceil(io.la.cnt, bStrip)
  val rcnt_pulse = Mux(io.la.reserve, rcnt, UInt(0))
  val rcnt_residue = io.la.cnt(bStrip-1, 0)

  private val bbias = bVLen - bStrip + 1
  val bias = Reg(Vec(nVLU, SInt(width = bbias)))
  val bias_in = Wire(Vec(nVLU, SInt()))
  val bias_next = Vec(bias_in.map(_ + rcnt_pulse.zext))
  bias_in := bias
  bias := bias_next

  /* Count the number of strips in the latest vector have not yet been
   * retired, equal to (ceil(vlen / nstrip) - rcnt), where rcnt is the
   * number of strips retired by the vlu sequencer op thus far.
   *
   * The negated value becomes the initial bias for the next vector,
   * such that its first strip of elements is recorded in the writeback
   * bitmap immediately after the final strip of the preceding vector.
   */
  val bias_tail = Reg(init = SInt(0, bbias))
  val vlen_tail = Ceil(opq.io.deq.bits.vlen, bStrip)
  val bias_tail_sub = Mux(issue, vlen_tail, UInt(0))
  bias_tail := (bias_tail + rcnt_pulse.zext) - bias_tail_sub.zext
  assert(bias_tail <= SInt(0), "VLU: positive bias_tail")

  val vlen_next = bias_next(vidx)
  val vlen_end = (vlen_next === op_head.vlen.zext)
  assert(!io.la.reserve || vlen_end || (rcnt_residue === UInt(0)),
    "VLU: retire count not a strip multiple")

  map.io.free := vlen_end && busy

  val (vd_stride, vd_shift) = if (confprec) {
    val stride = Reg(Vec(nVLU, UInt()))
    val shift = Reg(Vec(nVLU, UInt()))
    val prec = confprec_decode(opq.io.deq.bits.vd.prec)
    when (issue) {
      stride(vidx_tail) := confprec_stride(prec, io.cfg)
      shift(vidx_tail) := Mux1H(prec.map(p => p._1 -> UInt(p._2)))
    }
    (stride, shift)
  } else (Vec.fill(nVLU)(io.cfg.vstride.d), Vec.fill(nVLU)(UInt(0)))

  //--------------------------------------------------------------------\\
  // predication
  //--------------------------------------------------------------------\\

  val predq = Module(new Queue(io.pred.bits, nDCCPredQ))
  predq.suggestName("predqInst")
  // NOTE: insert extra cycle of latency for empty predicate short vector race that results in vlu retiring before vcu
  // COLIN: could also be solved with anteq being a flow queue but need to test QoR on that path
  predq.io.enq <> Queue(io.pred)
  val pred_fire = predq.io.deq.fire

  private val bpcnt = bVLen - bStrip + log2Ceil(nVLU)
  val pcnt = Reg(init = UInt(0, bpcnt))
  val pcnt_end = (pcnt === UInt(0))
  val pcnt_add = Mux(issue, vlen_tail, UInt(0))
  val pcnt_next = (pcnt.zext + pcnt_add.zext) - pred_fire.zext
  pcnt := pcnt_next.asUInt
  assert(pcnt_next >= SInt(0), "VLU: pcnt underflow")

  private val maxpidx = (szwb >> bStrip) - 1
  val pidx = Reg(init = UInt(0, log2Up(maxpidx)))
  val pidx_end = (pidx === UInt(maxpidx))
  val pidx_next = (pidx.zext + pred_fire.zext) - rcnt_pulse.zext
  pidx := pidx_next.asUInt
  /* NOTE: Predicates should always arrive before the corresponding
     load due to VMU latency, thus precluding this race condition */
  assert(pidx_next >= SInt(0), "VLU: pidx underflow")

  val pred = Mux(pred_fire, ~predq.io.deq.bits, UInt(0))
  val pred_shift = Cat(pidx, UInt(0, bStrip))
  val wb_pred = pred << pred_shift
  predq.io.deq.ready := !(pcnt_end || pidx_end)

  when (!busy) {
    assert(wb === Bits(0), "VLU: non-zero quiescent wb")
    assert(bias_tail === SInt(0), "VLU: non-zero quiescent bias_tail")
    assert(pcnt === UInt(0), "VLU: non-zero quiescent pcnt")
    assert(pidx === UInt(0), "VLU: non-zero quiescent pidx")
  }

  //--------------------------------------------------------------------\\
  // permutation network
  //--------------------------------------------------------------------\\

  val eidx_slice = meta.eidx(bSlices-1, 0)
  val eidx_strip = meta.eidx(bStrip-1, 0)
  val eidx_bank = meta.eidx(bStrip-1, bSlices)
  val eidx_reg = meta.eidx(bVLen-1, bStrip)
  val eidx_reg_next = eidx_reg + UInt(1)

  require(tlDataBytes == (nStrip << 1))
  val epad_msb = meta.epad(bStrip)
  val epad_eff = meta.epad(bStrip-1, 0)

  val rotamt = eidx_strip - epad_eff

  private def rotate[T <: Data](gen: T, in: Seq[T]) = {
    val rot = Module(new Rotator(gen, in.size, nStrip))
    rot.suggestName("rotInst")
    val out = Wire(Vec(nStrip, gen))
    rot.io.sel := rotamt
    rot.io.in := in
    out := rot.io.out
    out
  }

  private def extend[T <: Bits](in: T, sz: Int) =
    if (sz < regLen) Cat(Fill(regLen-sz, (in(sz-1) && mt.signed).asUInt), in) else in

  private def rotate_data[T <: Bits](data: T, sz: Int) = {
    val w = data.getWidth
    require(w > 0)
    val in = (0 until w by sz).map(i => data(i+sz-1, i))
    require(in.size <= nStrip)
    val out = rotate(UInt(width = sz), in)
    Vec(out.map(extend(_, sz)))
  }

  private val tlDataMidBits = tlDataBits >> 1

  val load = vldq.bits.data
  val load_b = Mux(epad_msb,
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

  /* When the load represents a full strip of elements and meta.eidx is
   * odd, the first and last elements reside in the same bank but in
   * different SRAM entries.  If both are present, they must be written
   * separately over two cycles.
   */
  val slice_unaligned = (eidx_slice =/= UInt(0)) && (epad_eff === UInt(0))
  val slice_used = slice_unaligned && meta.mask(0)
  val slice_free = slice_unaligned && !meta.mask(0)
  val slice_conflict = slice_used && meta.mask(nStrip-1)
  val tick_next = !(slice_conflict && tick)

  val bwqs_fire = Wire(Bool())
  when (bwqs_fire) {
    tick := tick_next
  }

  val mask_head = tick
  val mask_tail = Mux(tick, !slice_used, Bool(true))
  val mask_beat = Cat(mask_tail, Fill(nStrip-1, mask_head))
  val mask_base = meta.mask & mask_beat
  val mask = rotate(Bool(), mask_base.asBools)

  /* Handle intra-load eidx increment */
  val eidx_step_head = EnableDecoder(eidx_bank, nBanks)
  val eidx_step_tail = Mux(tick, slice_free, Bool(true)) << eidx_bank
  val eidx_step = eidx_step_head | eidx_step_tail

  //--------------------------------------------------------------------\\
  // bank write queues
  //--------------------------------------------------------------------\\

  private def merge[T <: Bits](in: Seq[T]): Seq[Bits] =
    in.grouped(nSlices).map(xs => Cat(xs.reverse)).toSeq

  val bwqs_data = merge(data)
  val bwqs_mask = merge(mask)
  val bwqs_en = bwqs_mask.map(_.asUInt.orR)

  val wb_update = Wire(Vec(nBanks, Bits(width = szwb)))

  val bwqs = io.bwqs.zipWithIndex.map { case (deq, i) =>
    val bwq = Module(new Queue(new VLUEntry, nBWQ))
    bwq.suggestName("bwqInst")

    bwq.io.enq.bits.vidx := meta.vidx
    bwq.io.enq.bits.eidx := Mux(eidx_step(i), eidx_reg_next, eidx_reg)
    bwq.io.enq.bits.data := bwqs_data(i)
    bwq.io.enq.bits.pred := bwqs_mask(i)

    val wb_mask = Mux(bwq.io.deq.fire, bwq.io.deq.bits.pred, Bits(0))
    val wb_vidx = bwq.io.deq.bits.vidx
    val wb_eidx = bwq.io.deq.bits.eidx
    val wb_offset = wb_eidx.zext - bias(wb_vidx)
    // Minimize bitwidth to match actual range of values
    // Discarded upper bits should be zero, as checked by assertions below
    val wb_offset_w = Wire(UInt((lgwb - bStrip).W))
    wb_offset_w := wb_offset.asUInt
    val wb_shift = Cat(wb_offset_w, (i << bSlices).U(bStrip.W))
    wb_update(i) := wb_mask << wb_shift

    val max = (szwb + (nStrip-1)) >> bStrip
    assert(!deq.valid || (wb_offset >= SInt(0)), f"VLU: BWQ ${i}: wb_offset underflow")
    assert(!deq.valid || (wb_offset < SInt(max)), f"VLU: BWQ ${i}: wb_offset overflow")

    deq.valid := bwq.io.deq.valid
    bwq.io.deq.ready := deq.ready

    val vd = op(wb_vidx).vd
    val addr = vd.id + (if (confprec)
        (wb_eidx >> vd_shift(wb_vidx)) * vd_stride(wb_vidx)
      else (wb_eidx * io.cfg.vstride.d))

    val pack = Wire(new PackInfo)
    pack.prec := vd.prec
    pack.idx := wb_eidx
    val out = repack_bank(pack, UInt(0), bwq.io.deq.bits)

    deq.bits.selff := Bool(false) // FIXME
    deq.bits.addr := addr
    deq.bits.data := out.data
    deq.bits.mask := out.mask

    bwq.io.enq
  }

  val bwqs_ready = bwqs.zip(bwqs_en).map {
    case (bwq, en) => !en || bwq.ready
  }
  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = vldq.valid +: bwqs_ready
    (rvs.filter(_.ne(exclude)) ++ include).reduce(_ && _)
  }

  val bwqs_ready_all = fire(vldq.valid)
  bwqs_fire := bwqs_ready_all && vldq.valid
  vldq.ready := bwqs_ready_all && tick_next
  bwqs.zip(bwqs_ready.zip(bwqs_en)).foreach { case (bwq, (ready, en)) =>
    bwq.valid := fire(ready, en)
  }

  //--------------------------------------------------------------------\\
  // writeback status
  //--------------------------------------------------------------------\\

  val wb_sets = Seq(wb, wb_pred) ++ wb_update
  assert(wb_sets.reduce(_ & _) === Bits(0), "VLU: wb bitmap collision")

  val wb_merge = wb_sets.reduce(_ | _)
  val wb_shift = Cat(rcnt_pulse, UInt(0, bStrip))
  val wb_next = wb_merge >> wb_shift
  wb := wb_next

  val wb_cnt = CTZ(~wb, maxLookAhead)
  io.la.available := busy && (wb_cnt >= io.la.cnt)

  //--------------------------------------------------------------------\\
  // initialization
  //--------------------------------------------------------------------\\

  when (issue) {
    op(vidx_tail) := opq.io.deq.bits
    op(vidx_tail).vlen := vlen_tail
    bias_in(vidx_tail) := bias_tail

    assert(opq.io.deq.bits.vd.valid, "VLU: invalid vd")
  }
}

trait VLUSelect extends DCCParameters {
  val vidx = UInt(width = bVLU)
}
class VLUSelectBundle(implicit p: Parameters) extends VXUBundle()(p) with VLUSelect
class VLUSelectIO(implicit p: Parameters)
  extends DecoupledIO(new VLUSelectBundle()(p)) {
}

class VLUMapper(implicit p: Parameters) extends VXUModule()(p) {
  val io = new DCCIssueIO {
    val use = new VLUSelectIO
    val free = Bool(INPUT)
    val vidx = UInt(OUTPUT, bVLU)
    val busy = Bool(OUTPUT)
  }

  val head = Reg(init = UInt(0, bVLU))
  val tail = Reg(init = UInt(0, bVLU))
  val equal = (head === tail)

  private def next[T <: UInt](ptr: T) = {
    val ptr1 = ptr + UInt(1)
    if (isPow2(nVLU)) ptr1 else
      Mux(ptr === UInt(nVLU-1), UInt(0), ptr1)
  }
  val head_next = next(head)
  val tail_next = next(tail)

  val used = Reg(init = Bool(false))
  val valid = !(equal && used)
  io.busy := !(equal && !used)

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(valid, io.op.valid, io.use.ready)
    (rvs.filter(_.ne(exclude)) ++ include).reduce(_ && _)
  }

  io.op.ready := fire(io.op.valid)
  io.use.valid := fire(io.use.ready)
  io.use.bits.vidx := tail
  io.vidx := head

  when (io.free) {
    head := head_next
    used := Bool(false)
  }
  when (io.use.fire) {
    tail := tail_next
    used := Bool(true)
  }
}

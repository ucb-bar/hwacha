package hwacha

import Chisel._
import DataGating._

class BPQLookAheadIO extends VXUBundle with LookAheadIO {
  val mask = Bits(OUTPUT, nBanks)
}

class BRQLookAheadIO extends VXUBundle with LookAheadIO {
  val mask = Bits(OUTPUT, nBanks)
}

class VGU extends VXUModule with Packing {
  val io = new Bundle {
    val op = Decoupled(new DCCOp).flip
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

  val pcntr = Module(new LookAheadCounter(nBanks+2, nBanks+2))
  pcntr.io.inc.cnt := UInt(1)
  pcntr.io.inc.update := lpq.io.deq.fire()
  pcntr.io.dec <> io.pla

  val lrq = Module(new Queue(new LRQEntry, nBanks+2))
  lrq.io.enq <> io.lrq

  val rcntr = Module(new LookAheadCounter(nBanks+2, nBanks+2))
  rcntr.io.inc.cnt := UInt(1)
  rcntr.io.inc.update := lrq.io.deq.fire()
  rcntr.io.dec <> io.qla

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)
  val op = Reg(new DCCOp)
  val slice = Reg(UInt(width = log2Up(nSlices)))

  // FIXME: I didn't have time to generalize, logic explained below
  require(nSlices == 2)

  // if slice === UInt(1), shave off last bit
  val pred = lpq.io.deq.bits.pred & Mux(slice === UInt(0), UInt(3), UInt(2))
  // dequeue lq when one hot
  val deq_lq = pred != UInt(3)
  // enqueue vaq when there's something
  val enq_vaq = pred != UInt(0)
  // find slice_next when more then two elements are alive
  val slice_next = Mux(pred === UInt(3), UInt(1), UInt(0))
  // find first one for pick
  val pick = Mux(pred(0), UInt(0), UInt(1))
  // process 2 elements only when pred is empty, otherwise 1
  val ecnt = Mux(pred === UInt(0), UInt(2), UInt(1))
  val vlen_cnt = Mux(ecnt > op.vlen, op.vlen, ecnt)
  val vlen_next = op.vlen - vlen_cnt

  val mask_vaq_ready = !enq_vaq || io.vaq.ready

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      state === s_busy,
      lpq.io.deq.valid, lrq.io.deq.valid, mask_vaq_ready)
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
  lrq.io.deq.ready := fire(lrq.io.deq.valid, deq_lq)
  io.vaq.valid := fire(mask_vaq_ready, enq_vaq)

  io.vaq.bits.addr := MuxLookup(pick, Bits(0), (0 until nSlices) map {
    i => UInt(i) -> unpack_slice(lrq.io.deq.bits.data, i) })
}

class VPU extends VXUModule with BankLogic {
  val io = new Bundle {
    val op = Decoupled(new DCCOp).flip
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
    val bpq = Module(new Queue(new BPQEntry, nbpq))
    val placntr = Module(new LookAheadCounter(nbpq, nbpq))
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

class VSU extends VXUModule {
  val io = new Bundle {
    val op = Decoupled(new DCCOp).flip
    val xcpt = new XCPTIO().flip

    val pred = Decoupled(Bits(width = nPredSet)).flip
    val brqs = Vec.fill(nBanks)(new BRQIO).flip
    val la = new BRQLookAheadIO().flip

    val vsdq = new VSDQIO
  }

  val opq = Module(new Queue(new DCCOp, nDCCOpQ))
  opq.io.enq <> io.op
  val op = opq.io.deq

  val predq = Module(new Queue(Bits(width=nPredSet), nDCCPredQ))
  predq.io.enq <> io.pred
  val pred = predq.io.deq

  private val lgbank = log2Up(nBanks)
  private val nTotalSlices = nSlices * nBanks

  val mt_fn = Reg(Bits(width = MT_SZ))
  val mt = DecodedMemType(mt_fn)
  val mtsel = Seq(mt.d, mt.w, mt.h, mt.b)

  val beat = Reg(UInt(width = lgbank))
  val beat_cnt = Seq(SZ_D, SZ_W, SZ_H, SZ_B).map(sz =>
    // Maximum number of elements per beat
    math.min(tlDataBits / sz, nTotalSlices))

  val vlen = Reg(UInt(width = bVLen))
  val ecnt = Mux1H(mtsel, beat_cnt.map(i => UInt(i)))
  val vlen_next = vlen.zext - ecnt.zext
  val last = (vlen_next <= SInt(0))
  val next = Bool()

  op.ready := Bool(false)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      op.ready := Bool(true)
      when (op.valid) {
        state := s_busy
        vlen := op.bits.vlen
        mt_fn := op.bits.fn.vmu().mt
        beat := UInt(0)
      }
    }

    is (s_busy) {
      when (next) {
        vlen := vlen_next
        beat := beat + UInt(1)
        when (last) {
          state := s_idle
        }
      }
    }
  }

  //--------------------------------------------------------------------\\
  // predication / masking
  //--------------------------------------------------------------------\\

  val brqs_cnt = Seq(SZ_D, SZ_W, SZ_H, SZ_B).map { sz =>
    // Yields (n, m) where
    // n: number of active BRQs per VSDQ subblock
    // m: number of complete subblocks per BRQ entry
    val brqDataBits = sz * nSlices
    if (tlDataBits >= brqDataBits) {
      val n = math.min(nBanks, tlDataBits / brqDataBits)
      require(isPow2(n))
      (n, 1)
    } else {
      val m = brqDataBits / tlDataBits
      require(isPow2(m))
      (1, m)
    }
  }
  val brqs_mask = (0 until nBanks).map(i =>
    Mux1H(mtsel, brqs_cnt.map { case (n, m) =>
      if (n == nBanks) Bool(true) else {
        val lgn = log2Ceil(n)
        val lgm = log2Ceil(m)
        (beat(lgbank-lgn-1+lgm, lgm) === UInt(i >> lgn))
      }
    }))

  val pred_head = if (nPredSet > nTotalSlices) {
    require(nPredSet % nTotalSlices == 0)
    val pred_shift = Mux1H(mtsel, brqs_cnt.map { case (n, m) =>
      UInt((lgbank - log2Ceil(n)) + log2Ceil(m))
    })
    val pred_index = beat >> pred_shift
    Vec((0 until nPredSet by nTotalSlices).map(i =>
      pred.bits(i+nTotalSlices-1, i)))(pred_index)
  } else {
    require(nTotalSlices % nPredSet == 0)
    Cat(Seq.fill(nTotalSlices / nPredSet)(pred.bits))
  }

  val pred_split = (0 until nTotalSlices).map(pred_head(_))
  val pred_slice = Vec(pred_split.grouped(nSlices).map(x => x.reduce(_ || _)).toSeq)

  val pred_brqs = if (brqs_cnt.indexWhere(_._2 > 1) < 0)
      pred_slice // optimization for uniform (m == 1)
    else
      Mux1H(mtsel, brqs_cnt.map { case (n, m) =>
        if (m == 1) pred_slice else {
          val pred_blks = pred_split.grouped(nSlices / m).map(x => x.reduce(_ || _))
          val pred_ways = pred_blks.toSeq.zipWithIndex.groupBy(_._2 % m)
          val pred_group = (0 until m).map(i => Vec(pred_ways(i).map(_._1)))
          Vec(pred_group)(beat/*(log2Ceil(m)-1, 0)*/)
        }
      })

  //--------------------------------------------------------------------\\
  // bank read queues
  //--------------------------------------------------------------------\\

  private val sz_pending = log2Down(nbrq+1) + 1
  val pending = Reg(init = UInt(0, sz_pending))
  val pending_add = Mux(io.la.reserve, UInt(1), UInt(0))
  val pending_sub = Mux(io.vsdq.fire(), Mux(mt.b, UInt(2), UInt(1)), UInt(0))
  pending := pending + pending_add - pending_sub

  val brqs = io.brqs.zipWithIndex.map { case (enq, i) =>
    val brq = Module(new Queue(new BRQEntry, nbrq))
    val slacntr = Module(new LookAheadCounter(nbrq, nbrq))
    val en = io.la.mask(i)
    brq.io.enq <> enq
    slacntr.io.inc.cnt := UInt(1)
    slacntr.io.inc.update := brq.io.deq.fire()
    slacntr.io.dec.cnt := UInt(1)
    slacntr.io.dec.reserve := io.la.reserve && en
    (brq.io.deq, !en || slacntr.io.dec.available)
  }
  io.la.available := brqs.map(_._2).reduce(_ && _)
  val brqs_deq = Vec(brqs.map(_._1))

  val brqs_en = pred_brqs.zip(brqs_mask).map { case (pred, mask) => pred && mask }
  val brqs_valid = brqs_deq.zip(brqs_en).map { case (brq, en) => !en || brq.valid }
  val vsdq_en = brqs_en.reduce(_ || _)
  val vsdq_ready = !vsdq_en || io.vsdq.ready

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = brqs_valid ++ Seq(pred.valid, vsdq_ready)
    (rvs.filter(_ ne exclude) ++ include :+ (state === s_busy)).reduce(_ && _)
  }
  next := fire(null)

  brqs_deq.zip(brqs_valid.zip(brqs_en)).foreach { case (brq, (valid, en)) =>
    brq.ready := fire(valid, en) // FIXME: Delay dequeuing for (m > 1)
  }

  val pred_dequeue = Mux1H(mtsel, beat_cnt.map { cnt =>
    val n = nPredSet / cnt
    if (n > 1) (beat(log2Ceil(n)-1, 0) === UInt(n-1)) else Bool(true)
  }) || last
  pred.ready := fire(pred.valid, pred_dequeue)

  io.vsdq.valid := fire(vsdq_ready, vsdq_en)

  //--------------------------------------------------------------------\\
  // permutation network
  //--------------------------------------------------------------------\\

  private def repack(sz_elt: Int, sz_reg: Int) = {
    require((sz_elt <= sz_reg) && (tlDataBits % sz_elt == 0))
    val elts = for (g <- (0 until wBank by sz_reg).grouped(nSlices);
      q <- brqs_deq; i <- g.iterator) yield q.bits.data(i+sz_elt-1, i)
    val sets = elts.grouped(tlDataBits / sz_elt).map(Vec(_)).toIterable
    if (sets.size > 1) Vec(sets)(beat) else sets.head
  }

/*
  private def select(sz_elt: Int) = {
    val xs = Seq(SZ_D, SZ_W, SZ_H, SZ_B).zip(prec).span(_._1 >= sz_elt)._1
    require(xs.size > 0)
    if (xs.size > 1)
      Mux1H(xs.map(_._2), xs.map(p => repack(sz_elt, p._1)))
    else repack(sz_elt, xs.head._1)
  }
*/

  val data_d = repack(SZ_D, SZ_D)
  val data_w = repack(SZ_W, SZ_D)
  val data_h = repack(SZ_H, SZ_D)
  val data_b = repack(SZ_B, SZ_D)

  io.vsdq.bits.data := Mux1H(mtsel,
    Seq(data_d, data_w, data_h, data_b).map(x => Cat(x.reverse)))
}

class VLUEntry extends VXUBundle {
  val eidx = UInt(width = bVLen - log2Up(nBatch))
  val data = Bits(width = wBank)
  val mask = Bits(width = nSlices)
}

class VLU extends VXUModule {
  val io = new Bundle {
    val op = Decoupled(new DCCOp).flip

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

  require(nSlices == 2)

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
  // masking / overflow
  //--------------------------------------------------------------------\\

  val tick = Reg(init = Bool(true))
  val tock = !tick

  val mask_root = (0 until nBatch).map(meta.mask(_))

  val slice_unaligned = (eidx_slice != UInt(0)) && tick
  val slice_overflow = mask_root.head && mask_root.last && slice_unaligned

  val bwqs_fire = Bool()
  when (bwqs_fire) {
    tick := !slice_overflow
  }

  val mask_head = tick
  val mask_tail = !slice_unaligned
  val mask_beat = mask_root.init.map(_ && mask_head) :+ (mask_root.last && mask_tail)
  val mask = rotate(Bool(), mask_beat)

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
    val bwq = Module(new Queue(new VLUEntry, nbwq))

    val eidx_advance = (UInt(i) < eidx_bank) || tock
    bwq.io.enq.bits.eidx := Mux(eidx_advance, eidx_reg_next, eidx_reg)
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
  vldq.ready := bwqs_ready_all && !slice_overflow && sink
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

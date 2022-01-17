package hwacha

import Chisel._
import freechips.rocketchip.config._

class VMUMaskIO(implicit p: Parameters) extends VMUBundle()(p) {
  val ante = new VMUMaskIO_0
  val post = new VMUMaskIO_1
}

class PBox0(implicit p: Parameters) extends VMUModule()(p) {
  val io = new VMUIssueIO {
    val ingress = Decoupled(new PredEntry).flip
    val egress = Decoupled(new PredEntry)
    val sample = new VMUMaskIO_0
  }

  val op = Reg(new VMUDecodedOp)

  val index = Reg(UInt(width = bStrip))
  val head = io.ingress.bits.pred >> index
  val step_max = UInt(nStrip) - index

  /* Density-time approximation for non-coalesced operations:
   * Skip ahead by power-of-2 lengths when the next 2^i predicates are
   * uniformly false
   *
   * c.f. Smith et al., "Vector Instruction Set Support for Conditional
   *      Operations," in Proc. 27th Annual International Symp. on
   *      Computer Architecture, New York, NY, 2000, pp. 260-269.
   */
  private val scan = (1 to bStrip).scanLeft(
    (0, Bool(true))) {
      case ((j, zero_tail), i) =>
        val m = (1 << i)
        val n = (1 << j) - 1
        // Ensure that sufficient number of predicates remain in the set
        val valid = (index <= UInt(nStrip - m))
        val zero = zero_tail && (head(m-1, n) === UInt(0)) && valid
        (i, zero)
  }.tail

  val pred_n = MuxCase(head(0),
    scan.reverse.map { case (_, zero) => (zero -> Bool(false)) })

  val lgecnt_n = MuxCase(UInt(0),
    scan.reverse.map { case (i, zero) => (zero -> UInt(i)) })
  val ecnt_n = UInt(1) << lgecnt_n
  val step_n = ecnt_n

  val lead = Wire(Bool())
  lead := Bool(false)

  val pglen = Reg(UInt(width = bPgIdx + 1))
  val pglen_next = pglen.zext - step_max.zext
  val pglen_end = (pglen_next <= SInt(0))
  val pglen_skip = Mux(lead, io.op.bits.base(bPgIdx-1, 0), UInt(0))
  val pglen_shift = Mux(lead, io.op.bits.mt.shift(), op.mt.shift())
  val pglen_max = (UInt(pgSize) - pglen_skip) >> pglen_shift
  val pglen_final = Reg(Bool())
  val pglen_reset = Wire(Bool())
  pglen_reset := Bool(false)

  val head_mask = EnableDecoder(pglen, nStrip)
  val pred_u = (head & head_mask).orR
  val pred = Mux(op.mode.unit, pred_u, pred_n)

  val vlen_end = Wire(Bool())
  val ecnt_u_page = pred_u || pglen_end
  val ecnt_u = Mux(ecnt_u_page, pglen, step_max)
  val step_u = Mux(pglen_end, pglen(bStrip, 0), step_max)
  val ecnt_n_bounded = Mux(vlen_end, op.vlen, ecnt_n)
  val ecnt = Mux(op.mode.unit, ecnt_u, ecnt_n_bounded)

  val step = Mux(op.mode.unit, step_u, step_n)
  val vlen_next = op.vlen.zext - step.zext
  vlen_end := (vlen_next <= SInt(0))

  io.sample.bits.pred := pred
  io.sample.bits.ecnt := ecnt
  io.sample.bits.last := vlen_end ||
    (op.mode.unit && pglen_final && ecnt_u_page)
  io.sample.bits.unit.page := ecnt_u_page
  io.sample.bits.nonunit.shift := lgecnt_n
  io.egress.bits := io.ingress.bits

  val index_next = index + step
  val index_end = (index_next === UInt(nStrip))

  val egress_en = index_end || vlen_end
  val egress_ready = !egress_en || io.egress.ready

  val sample_en = Reg(Bool())
  val sample_ready = !sample_en || io.sample.ready

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(io.ingress.valid, egress_ready, sample_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.op.ready := Bool(false)
  io.ingress.ready := Bool(false)
  io.egress.valid := Bool(false)
  io.sample.valid := Bool(false)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      io.op.ready := Bool(true)
      pglen_reset := Bool(true)
    }

    is (s_busy) {
      io.ingress.ready := fire(io.ingress.valid, egress_en)
      io.egress.valid := fire(egress_ready, egress_en)
      io.sample.valid := fire(sample_ready, sample_en)

      when (fire(null)) {
        index := index_next
        assert(index_next <= UInt(nStrip), "PBox0: index overflow")

        when (op.mode.unit) {
          when (pred_u) {
            sample_en := Bool(false)
          }
          pglen := pglen_next.asUInt
          when (pglen_end) {
            pglen_reset := Bool(true)
          }
        }

        op.vlen := vlen_next.asUInt
        when (vlen_end) {
          state := s_idle
          assert(!op.mode.unit || pglen_end,
            "PBox0: desynchronized vlen and pglen counters");

          io.op.ready := Bool(true)
          pglen_reset := Bool(true)
        }
      }
    }
  }

  val _vlen_next = Mux(lead, io.op.bits.vlen, vlen_next(bVLen-1, 0))
  val _pglen_final = (_vlen_next <= pglen_max)
  when (pglen_reset) {
    pglen := Mux(_pglen_final, _vlen_next, pglen_max)
    pglen_final := _pglen_final
    sample_en := Bool(true)
  }

  when (io.op.fire) { /* initialization */
    state := s_busy
    op := io.op.bits
    index := UInt(0)
    lead := Bool(true)
  }
}

class PBox1(implicit p: Parameters) extends VMUModule()(p) {
  val io = new VMUIssueIO {
    val ingress = Decoupled(new PredEntry).flip
    val sample = new VMUMaskIO_1
  }

  private val limit = tlDataBytes >> 1
  private val lglimit = tlByteAddrBits - 1
  require(limit == nStrip)
  require(isPow2(limit))

  /* Number of doublewords per TileLink subblock */
  private val lgslices = tlByteAddrBits - 3
  private val nslices = 1 << lgslices
  require(lgslices >= 0)

  val op = Reg(new VMUDecodedOp)
  private val mts = Seq((op.mt.b || op.mt.h), op.mt.w, op.mt.d)
  private val meta = io.sample.meta

  val index = Reg(UInt(width = lglimit))
  val step_u = Cat(mts :+ UInt(0, lgslices))
  val step = Mux(op.mode.unit, step_u, UInt(1))
  val index_next = (index + step)(lglimit-1, 0)
  val index_end = (index_next === UInt(0))

  when (io.sample.fire) {
    index := index_next
  }

  val hold = Reg(Bits(width = limit - 1))
  when (io.ingress.fire) {
    hold := io.ingress.bits.pred(nStrip-1, nStrip-limit+1)
  }

  val surplus = Wire(Bool())
  val pred = Mux(surplus, Bits(0), io.ingress.bits.pred)
  val preds = (0 until nStrip).map(pred(_)).toSeq

  val head = Cat(pred, hold)
  /* Equivalence: index + (UInt(limit) - 1 - eoff) */
  val shift = Cat(UInt(0,1), index) + (~meta.eoff)
  val mask_data = (head >> shift)(limit-1, 0)

  /* Clear predicates unrelated to the current request */
//val mask_type_base = Seq(!op.mt.d, mts.head).map(_ && op.mode.unit)
//val mask_type_head = (op.mode.unit, nslices - 1)
//val mask_type_tail = mask_type_base.zipWithIndex.map {
//  case (m, i) => (m, 1 << (i + lgslices))
//}
//val mask_type_full = Cat((
//  Bool(true) +: /* First predicate is always relevant */
//  ((mask_type_head +: mask_type_tail).map {
//    case (m, w) => Fill(w, m) /* Trailing predicates */
//  })).reverse)
//val mask_type = (mask_type_full << meta.epad)(limit-1, 0)

  /* For each possible VLDQ entry, generate a flag indicating its
     presence or absence, collated by data width */
  val mask_vsdq_all = (1 until mts.size).scanRight(
    preds.grouped(nslices).map(_.reduce(_ || _)).toSeq) {
      case (_, xs) => xs.grouped(2).map(_.reduce(_ || _)).toSeq
    }
  /* Select flag associated with the current VSDQ entry */
  val mask_vsdq_mux = mask_vsdq_all.map { xs =>
    if (xs.size > 1) {
      val n = log2Up(xs.size)
      Vec(xs)(index(lglimit-1, lglimit-n))
    } else xs.head
  }
  val mask_vsdq = Mux1H(mts, mask_vsdq_mux)

  /* Truncation condition for coalesced operations:
   * After the first strip, if the number of remaining elements is less
   * than or equal to the (non-zero) offset, then the final mask is
   * derived exclusively from the hold register.
   * Note that truncation only needs to be handled for the end of a
   * vector and not premature termination after an exception, since the
   * predicates are already available in the latter case.
   *
   * Alternative formulation:
   * (possibly higher logic depth due to ecnt signal)
   * val truncate = op.mode.unit && !meta.first &&
   *                (meta.ecnt <= meta.eoff)
   */
  val remnant = (op.vlen + meta.eoff)(lglimit-1, 0)
  val truncate = op.mode.unit &&
    (remnant <= meta.eoff) && (remnant =/= UInt(0))
  surplus := meta.last && truncate

  io.sample.bits.data := mask_data
  io.sample.bits.vsdq := mask_vsdq && !surplus

  val ingress_deq = (index_end || meta.last) && !surplus
  val ingress_valid = surplus || io.ingress.valid

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(ingress_valid, io.sample.ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.op.ready := Bool(false)
  io.ingress.ready := Bool(false)
  io.sample.valid := Bool(false)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      io.op.ready := Bool(true)
    }

    is (s_busy) {
      io.ingress.ready := fire(ingress_valid, ingress_deq)
      io.sample.valid := fire(io.sample.ready)
      when (fire(null) && meta.last) {
        state := s_idle
        io.op.ready := Bool(true)
      }
    }
  }

  when (io.op.fire) { /* initialization */
    state := s_busy
    op := io.op.bits
    hold := Bits(0)
    index := UInt(0)
  }
}

class PBox(implicit p: Parameters) extends VMUModule()(p) {
  val io = new Bundle {
    val op = Vec(2, Decoupled(new VMUDecodedOp)).flip
    val pred = Decoupled(new PredEntry).flip
    val mask = new VMUMaskIO
  }

  /* NOTE: As a consequence of limited buffer space, predicates enqueued
   * in predq1 must be matched by at least an equal increment of the VCU
   * counter in order to avoid potential deadlock.
   */
  val predq0 = Module(new Queue(io.pred.bits, nVMUPredQ))
  predq0.suggestName("predq0Inst")
  val predq1 = Module(new Queue(io.pred.bits, nVMUPredQ))
  predq1.suggestName("predq1Inst")
  val pbox0 = Module(new PBox0)
  pbox0.suggestName("pbox0Inst")
  val pbox1 = Module(new PBox1)
  pbox1.suggestName("pbox1Inst")

  predq0.io.enq <> io.pred

  pbox0.io.op <> io.op(0)
  pbox0.io.ingress <> predq0.io.deq

  predq1.io.enq <> pbox0.io.egress

  pbox1.io.op <> io.op(1)
  pbox1.io.ingress <> predq1.io.deq

  val anteq = Module(new Queue(pbox0.io.sample.bits, 2))
  anteq.suggestName("anteqInst")
  anteq.io.enq <> pbox0.io.sample
  io.mask.ante <> anteq.io.deq
  io.mask.post <> pbox1.io.sample
}

/* Expand predicate set to byte-granular mask */
object PredicateByteMask {
  def apply[T <: Bits](pred: T, mt: DecodedMemType): Bits = {
    val sel = Seq(mt.b, mt.h, mt.w, mt.d)
    Mux1H(sel.zipWithIndex.map { case (s, i) =>
      val len = pred.getWidth
      val w = 1 << i
      val n = (len >> i) - 1
      require((len > 0) && (n >= 0))
      (s, FillInterleaved(w, pred(n, 0)))
    })
  }
}

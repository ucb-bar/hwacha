package hwacha

import Chisel._
import Constants._
import uncore.constants.MemoryOpConstants._

class BRQLookAheadIO extends HwachaBundle with LookAheadIO {
  val mask = Bits(OUTPUT, nbanks)
}

class VSU extends HwachaModule with LaneParameters with VMUParameters {
  val io = new Bundle {
    val op = Decoupled(new DCCMemOp).flip
    val xcpt = new XCPTIO().flip

    val pred = Decoupled(Bits(width = nPredSet)).flip
    val brqs = Vec.fill(nbanks)(new BRQIO).flip
    val la = new BRQLookAheadIO().flip

    val vsdq = new VSDQIO
  }

  private val lgbank = log2Up(nbanks)
  private val nTotalSlices = nSlices * nbanks
  private val sz_beat = math.max(1, nPredSet / nSlices) * lgbank

  val mt_fn = Reg(Bits(width = MT_SZ))
  val mt = DecodedMemType(mt_fn)
  val mtsel = Seq(mt.d, mt.w, mt.h, mt.b)

  val beat = Reg(UInt(width = sz_beat))
  val beat_mid = mt.b && !beat(0) // FIXME: parameterize
  val beat_cnt = Seq(SZ_D, SZ_W, SZ_H, SZ_B).map(sz =>
    // Maximum number of elements per beat
    math.min(tlDataBits / sz, nTotalSlices))

  val vlen = Reg(UInt(width = SZ_VLEN))
  val ecnt = Mux1H(mtsel, beat_cnt.map(i => UInt(i)))
  val vlen_next = vlen.zext - ecnt.zext
  val last = (vlen_next <= SInt(0))
  val next = Bool()

  io.op.ready := Bool(false)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      io.op.ready := Bool(true)
      when (io.op.valid) {
        state := s_busy
        vlen := io.op.bits.vlen
        mt_fn := io.op.bits.fn.mt
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
      val n = math.min(nbanks, tlDataBits / brqDataBits)
      require(isPow2(n))
      (n, 1)
    } else {
      val m = brqDataBits / tlDataBits
      require(isPow2(m))
      (1, m)
    }
  }
  val brqs_mask = (0 until nbanks).map(i =>
    Mux1H(mtsel, brqs_cnt.map { case (n, m) =>
      if (n == nbanks) Bool(true) else {
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
      io.pred.bits(i+nTotalSlices-1, i)))(pred_index)
  } else {
    require(nTotalSlices % nPredSet == 0)
    Cat(Seq.fill(nTotalSlices / nPredSet)(io.pred.bits))
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
    slacntr.io.la.cnt := UInt(1)
    slacntr.io.la.reserve := io.la.reserve && en
    slacntr.io.inc.cnt := UInt(1)
    slacntr.io.inc.update := brq.io.deq.fire()
    slacntr.io.dec.update := Bool(false)
    (brq.io.deq, slacntr.io.la.available && en)
  }
  io.la.available := brqs.map(_._2).reduce(_ && _)
  val brqs_deq = Vec(brqs.map(_._1))

  val brqs_en = pred_brqs.zip(brqs_mask).map { case (pred, mask) => pred && mask }
  val brqs_valid = brqs_deq.zip(brqs_en).map { case (brq, en) => !en || brq.valid }
  val vsdq_en = brqs_en.reduce(_ || _) && (!beat_mid || last)
  val vsdq_ready = !vsdq_en || io.vsdq.ready

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = brqs_valid ++ Seq(io.pred.valid, vsdq_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }
  next := fire(null)

  brqs_deq.zip(brqs_valid.zip(brqs_en)).foreach { case (brq, (valid, en)) =>
    brq.ready := fire(valid, en) // FIXME: Delay dequeuing for (m > 1)
  }

  val pred_dequeue = Mux1H(mtsel, beat_cnt.map { cnt =>
    val n = nPredSet / cnt
    if (n > 1) (beat(log2Ceil(n)-1, 0) === UInt(n-1)) else Bool(true)
  }) || last
  io.pred.ready := fire(io.pred.valid, pred_dequeue)

  io.vsdq.valid := fire(vsdq_ready, vsdq_en) ||
    (io.xcpt.prop.vmu.stall && mt.b && (pending === UInt(1)) && !beat_mid)

  //--------------------------------------------------------------------\\
  // permutation network
  //--------------------------------------------------------------------\\

  private def repack(sz_elt: Int, sz_reg: Int) = {
    require((sz_elt <= sz_reg) && (tlDataBits % sz_elt == 0))
    val elts = for (g <- (0 until SZ_DATA by sz_reg).grouped(nSlices);
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

  val data_b_hold = Reg(Vec.fill(data_b.size)(Bits()))
  when (next && beat_mid) {
    data_b_hold := data_b
  }

  // Bypass hold register if final beat is odd
  val data_b_head = Mux(last && beat_mid, data_b, data_b_hold)
  io.vsdq.bits := Mux1H(mtsel,
    Seq(data_b_head ++ data_b, data_h, data_w, data_d).map(x => Cat(x.reverse)))
}

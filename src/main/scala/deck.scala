package hwacha

import Chisel._
import Constants._
import Compaction._
import uncore.constants.MemoryOpConstants._

class DeckOpIO extends DecoupledIO(new DeckOp)

class Deck(resetSignal: Bool = null) extends HwachaModule(_reset = resetSignal) {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val op = new DeckOpIO().flip

    val lla = new LookAheadPortIO(log2Down(nvlreq)+1).flip
    val sla = new LookAheadPortIO(log2Down(nvsdq)+1).flip

    val brqs = Vec.fill(nbanks){new BRQIO().flip}
    val bwqs = Vec.fill(nbanks){new BWQIO}

    val vmu = new VMUIO
  }

  val vlu = Module(new VLU)
  val vsu = Module(new VSU)

  val cmd = vmu_op_mcmd(io.op.bits.fn)
  val cmd_amo = isAMO(cmd)
  val en_vlu = (cmd === M_XRD) || cmd_amo
  val en_vsu = (cmd === M_XWR) || cmd_amo

  io.op.ready :=
    en_vlu && vlu.io.op.ready ||
    en_vsu && vsu.io.op.ready

  vlu.io.cfg <> io.cfg
  vlu.io.op.valid := en_vlu && io.op.valid
  vlu.io.op.bits := io.op.bits
  vlu.io.la <> io.lla
  vlu.io.bwqs <> io.bwqs
  vlu.io.vldq <> io.vmu.vldq

  vsu.io.cfg <> io.cfg
  vsu.io.op.valid := en_vsu && io.op.valid
  vsu.io.op.bits := io.op.bits
  vsu.io.la <> io.sla
  vsu.io.brqs <> io.brqs
  vsu.io.vsdq <> io.vmu.vsdq
}

class VLU extends VMUModule {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val op = new DeckOpIO().flip

    val bwqs = Vec.fill(nbanks){new BWQIO}
    val la = new LookAheadPortIO(log2Down(nvlreq)+1).flip

    val vldq = new VLDQIO().flip
  }

  val op = Reg(new DeckOp)
  val mt = DecodedMemType(op.mt)
  val mt_seq = Seq(mt.b, mt.h, mt.w, mt.d)
  val utidx_next = op.utidx + io.la.cnt

  io.op.ready := Bool(false)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      io.op.ready := Bool(true)
      when (io.op.valid) {
        state := s_busy
        op := io.op.bits
      }
    }
    is (s_busy) {
      when (io.la.reserve) {
        op.utidx := utidx_next
        when (utidx_next === op.vlen) {
          state := s_idle
        }
      }
    }
  }

  val op_signext = !mt.unsigned

  //--------------------------------------------------------------------\\
  // floating-point recoding
  //--------------------------------------------------------------------\\

  private def unpack(w: Int, i: Int) = io.vldq.bits.data(((i+1)*w)-1, i*w)
  private def prefix(n: Bits, w: Int) = Cat(op_signext & n(w-1), n)
  private def extend(n: Bits, w: Int, recoded: Boolean) = {
    val m = if (recoded) n else prefix(n, w)
    Cat(Fill(SZ_DATA-w-1, m(w)), m)
  }

  val src_data_d = Vec.tabulate(tlDataDoubles){ i => {
    val elt = unpack(SZ_XD, i)
    val elt_rf = hardfloat.floatNToRecodedFloatN(elt, 52, 12)
    Mux(op.float, elt_rf, prefix(elt, SZ_XD))
  }}

  val src_data_w = Vec.tabulate(tlDataWords){ i => {
    val elt = unpack(SZ_XW, i)
    val elt_rf = hardfloat.floatNToRecodedFloatN(elt, 23, 9)
    Mux(op.float, elt_rf, prefix(elt, SZ_XW))
  }}

  val src_data_h = Vec.tabulate(tlDataHalves){ i => unpack(SZ_XH, i) }
  val src_data_b = Vec.tabulate(tlDataBytes/2){ i => unpack(SZ_XB, i) }

  //--------------------------------------------------------------------\\
  // permutation network
  //--------------------------------------------------------------------\\

  private val lgbank = log2Up(nbanks)
  val vldq = io.vldq
  val meta = io.vldq.bits.meta

  val bank_start = meta.eidx(lgbank-1,0)
  val bank_eidx = meta.eidx(SZ_VLEN-1, lgbank)
  val bank_eidx_next = bank_eidx + UInt(1)

  val src_rotamt = bank_start - meta.eskip

  private def permute[T <: Data](src: Vec[T], sel: UInt, rev: Boolean = false): Vec[T] = {
    require(src.size > 0)
    val rot = Module(new Rotator(src(0).clone, src.size, math.max(src.size, nbanks), rev))
    val dst = Vec.fill(nbanks){ src(0).clone }
    rot.io.in := src
    rot.io.sel := sel
    for (i <- 0 until nbanks) {
      dst(i) := rot.io.out(i) // retain output signal names
    }
    return dst
  }

  val dst_data_d = permute(src_data_d, src_rotamt)
  val dst_data_w = permute(src_data_w, src_rotamt)
  val dst_data_h = permute(src_data_h, src_rotamt)
  val dst_data_b = permute(src_data_b, src_rotamt)

  val src_en_raw = EnableDecoder(meta.ecnt, nbanks)
  val src_en_bits = Cat(src_en_raw.reverse) << meta.eskip
  val src_en = Vec.tabulate(nbanks)(i => src_en_bits(i))
  val bwqs_en = permute(src_en, src_rotamt)

  //--------------------------------------------------------------------\\
  // bank write queues
  //--------------------------------------------------------------------\\

  val bwstat_update = Vec.fill(nbanks)(Bits())

  val vd_stride = Mux(op.reg.vd.float, io.cfg.fstride, io.cfg.xstride)

  val bwqs = Vec.tabulate(nbanks){ i => {
    val bwq = Module(new Queue(new BWQInternalEntry, nbwq))
    bwq.io.enq.valid := vldq.valid && bwqs_en(i)

    bwq.io.enq.bits.data := Mux1H(mt_seq, Seq(
      extend(dst_data_b(i), SZ_XB, recoded=false),
      extend(dst_data_h(i), SZ_XH, recoded=false),
      extend(dst_data_w(i), SZ_XW, recoded=true),
      extend(dst_data_d(i), SZ_XD, recoded=true)))

    // Handle mid-load utidx increment due to rotation "wrap-around"
    bwq.io.enq.bits.tag := Mux(UInt(i) < bank_start, bank_eidx_next, bank_eidx)

    val vd_eidx = bwq.io.deq.bits.tag
    io.bwqs(i).bits.data := bwq.io.deq.bits.data
    io.bwqs(i).bits.addr := op.reg.vd.id + (vd_eidx * vd_stride)

    val bwstat_idx = Cat(vd_eidx, UInt(i, lgbank)) - op.utidx
    bwstat_update(i) := bwq.io.deq.fire() << bwstat_idx

    io.bwqs(i) <> bwq.io.deq
    bwq.io.enq
  }}

  val bwqs_ready = bwqs.zip(bwqs_en).map(i => !i._2 || i._1.ready)
  vldq.ready := bwqs_ready.reduce(_&&_)

  val bwstat = Reg(Bits(width = nvlreq))
  val bwstat_next = bwstat | bwstat_update.reduce(_|_)
  val bwstat_shift = io.la.cnt(SZ_LGBANK1,0) & Fill(SZ_LGBANK1, io.la.reserve)

  bwstat := (bwstat_next >> bwstat_shift) &
    Fill(nvlreq, state != s_idle) // initialization

  // Limited leading-ones count
  var sel = bwstat(0)
  var locnt = UInt(0, SZ_LGBANK1)
  for (i <- 0 until nbanks) {
    locnt = Mux(sel, UInt(i+1), locnt)
    sel = sel & bwstat(i+1)
  }
  io.la.available := (locnt >= io.la.cnt)
}

object EnableDecoder {
  def apply[T <: Data](in: T, n: Int) = {
    val out = Vec.fill(n)(Bool())
    val sel = (n until 0 by -1).map(i => (in === Bits(i)))
    out := Vec((0 until n).map(i => sel.take(sel.size - i).reduce(_||_)))
    out
  }
}

class VSU extends VMUModule {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val op = new DeckOpIO().flip

    val brqs = Vec.fill(nbanks)(new BRQIO()).flip
    val la = new LookAheadPortIO(log2Down(nvsdq)+1).flip

    val vsdq = new VSDQIO
  }

  private val lgbank = log2Up(nbanks)

  val op = Reg(new DeckOp)
  val mt = DecodedMemType(op.mt)
  private val mt_seq = Seq(mt.b, mt.h, mt.w, mt.d)

  val beat = Reg(UInt(width = lgbank - 1))
  val beat_mid = (mt.b && !beat(0))

  val ecnt_max = Cat(mt.b || mt.h, mt.w, mt.d, Bits(0,1)) // FIXME: parameterize
  val vlen_next = op.vlen.zext - ecnt_max.zext
  val last = (vlen_next <= UInt(0))
  val ecnt = Mux(last, op.vlen(lgbank,0), ecnt_max)

  val out_ready = io.vsdq.ready || beat_mid
  val next = Bool()

  io.op.ready := Bool(false)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      io.op.ready := Bool(true)
      when (io.op.valid) {
        state := s_busy
        op := io.op.bits
        beat := UInt(0)
      }
    }
    is (s_busy) {
      when (next) {
        op.vlen := vlen_next
        beat := beat + UInt(1)
        when (last) {
          state := s_idle
        }
      }
    }
  }

  val sla_en = EnableDecoder(io.la.cnt(lgbank,0), nbanks)
  val sla_avail = Vec.fill(nbanks)(Bool())
  io.la.available := sla_avail.reduce(_&&_)

  val brqs = Vec.tabulate(nbanks){ i => {
    val brq = Module(new Queue(new BRQEntry, nbrq))
    val slacntr = Module(new LookAheadCounter(nbrq, nbrq))
    brq.io.enq <> io.brqs(i)
    slacntr.io.la.cnt := UInt(1)
    slacntr.io.la.reserve := io.la.reserve && sla_en(i)
    slacntr.io.inc.cnt := UInt(1)
    slacntr.io.inc.update := brq.io.deq.fire()
    slacntr.io.dec.update := Bool(false)
    sla_avail(i) := slacntr.io.la.available
    brq.io.deq
  }}

  val brqs_en_raw = EnableDecoder(ecnt, nbanks)
  val brqs_shift = Cat(Mux1H(mt_seq, // FIXME: parameterize
      Seq(UInt(0), UInt(0), Cat(beat(0), Bits(0,1)), beat(1,0))),
    Bits(0,1))
  val brqs_en_bits = Cat(brqs_en_raw.reverse) << brqs_shift
  val brqs_en = Vec.tabulate(nbanks)(i => brqs_en_bits(i))

  val brqs_valid = brqs.zip(brqs_en).map(i => !i._2 || i._1.valid)
  for (i <- 0 until brqs.size) {
    val (a, b) = brqs_valid.splitAt(i) // Exclude own valid signal
    brqs(i).ready := out_ready && (a ++ b.tail).reduce(_&&_) && brqs_en(i) && (state === s_busy)
  }

  val brqs_valid_all = brqs_valid.reduce(_&&_)
  io.vsdq.valid := brqs_valid_all && !beat_mid && (state === s_busy)

  next := out_ready && brqs_valid_all

  type UnpackFn = (Bits, Int) => Bits
  private def unpack_b(n: Bits, i: Int) = n(7,0)
  private def subset(n: Int, unpack: UnpackFn) = {
    require(brqs.size % n == 0)
    val sets = brqs.map(i => unpack(i.bits.data, 0)).grouped(n).map(Vec(_)).toIterable
    if (sets.size > 1) Vec(sets)(beat) else sets.head
  }


  val brq_data_d = subset(tlDataDoubles, unpack_float_d)
  val brq_data_w = subset(tlDataWords, unpack_float_s)
  val brq_data_h = subset(tlDataHalves, unpack_float_h)
  val brq_data_b = subset(tlDataBytes/2, unpack_b)

  val data_d = Vec(brq_data_d.map{elt =>
    val elt_rf = hardfloat.recodedFloatNToFloatN(elt.toUInt, 52, 12)
    Mux(op.float, elt_rf, elt(SZ_XD-1,0))})

  val data_w = Vec(brq_data_w.map{elt =>
   val elt_rf = hardfloat.recodedFloatNToFloatN(elt.toUInt, 23, 9)
   Mux(op.float, elt_rf, elt(SZ_XW-1,0))})

  val data_h = brq_data_h
  val data_b = brq_data_b

  val data_b_hold = Reg(Vec.fill(data_b.size)(Bits()))
  when (beat_mid) {
    data_b_hold := data_b
  }

  io.vsdq.bits := Mux1H(mt_seq,
    Seq(data_b_hold ++ data_b, data_h, data_w, data_d).map(i => Cat(i.reverse)))

}

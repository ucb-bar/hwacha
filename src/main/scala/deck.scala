package hwacha

import Chisel._
import Node._
import Constants._
import Compaction._
import uncore.constants.MemoryOpConstants._
import scala.collection.mutable.ArrayBuffer

class DeckOpIO extends DecoupledIO(new DeckOp)

class Deck(resetSignal: Bool = null)(implicit conf: HwachaConfiguration) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val op = new DeckOpIO().flip

    val lla = new LookAheadPortIO(conf.sz_lla).flip()
    val sla = new LookAheadPortIO(conf.sz_sla).flip()

    val brqs = Vec.fill(conf.nbanks){new BRQIO().flip}
    val bwqs = Vec.fill(conf.nbanks){new BWQIO}

    val vmu = new VMUIO
  }

  val opq = Module(new Queue(new DeckOp, 2))
  val vlu = Module(new VLU)
  val vsu = Module(new VSU)

  opq.io.enq <> io.op
  opq.io.deq.ready :=
    (!opq.io.deq.bits.fn.lreq() || vlu.io.op.ready) &&
    (!opq.io.deq.bits.fn.sreq() || vsu.io.op.ready)

  vlu.io.cfg <> io.cfg
  vlu.io.op.valid := opq.io.deq.valid && opq.io.deq.bits.fn.lreq()
  vlu.io.op.bits := opq.io.deq.bits
  vlu.io.la <> io.lla
  io.bwqs <> vlu.io.bwqs
  io.vmu <> vlu.io.vmu

  vsu.io.cfg <> io.cfg
  vsu.io.op.valid := opq.io.deq.valid &&
    (opq.io.deq.bits.fn.sreq() || opq.io.deq.bits.fn.amoreq())
  vsu.io.op.bits := opq.io.deq.bits
  vsu.io.la <> io.sla
  vsu.io.brqs <> io.brqs
  io.vmu <> vsu.io.vmu
}

class VLU(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val op = new DeckOpIO().flip

    val bwqs = Vec.fill(conf.nbanks){new BWQIO}
    val la = new LookAheadPortIO(conf.sz_lla).flip

    val vmu = new VMUIO
  }

  val op = Reg(new DeckOp)
  val utidx_next = op.utidx + io.la.cnt

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  io.op.ready := (state === s_idle)

  switch (state) {
    is (s_idle) {
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

//--------------------------------------------------------------------\\
// floating-point recoding
//--------------------------------------------------------------------\\

  val op_signext = op.fn.signext()
  val op_type_d = is_mtype_doubleword(op.fn.typ)
  val op_type_w = is_mtype_word(op.fn.typ)
  val op_type_h = is_mtype_halfword(op.fn.typ)
  val op_type_b = is_mtype_byte(op.fn.typ)

  private def unpack(w: Int, i: Int) = io.vmu.ldata.bits.data(((i+1)*w)-1, i*w)
  private def prefix(n: Bits, w: Int) = Cat(op_signext & n(w-1), n)
  private def extend(n: Bits, w: Int, recoded: Boolean = true) = {
    val m = if (recoded) n else prefix(n, w)
    Cat(Fill(SZ_DATA-w-1, m(w)), m)
  }

  val src_data_d = Vec.tabulate(conf.vmu.nd){ i => {
    val element = unpack(SZ_XD, i)
    val f64rf64 = Module(new hardfloat.float64ToRecodedFloat64)
    f64rf64.io.in := element
    Mux(op.fn.float, f64rf64.io.out, prefix(element, SZ_XD))
  }}

  val src_data_w = Vec.tabulate(conf.vmu.nw){ i => {
    val element = unpack(SZ_XW, i)
    val f32rf32 = Module(new hardfloat.float32ToRecodedFloat32)
    f32rf32.io.in := element
    Mux(op.fn.float, f32rf32.io.out, prefix(element, SZ_XW))
  }}

  val src_data_h = Vec.tabulate(conf.vmu.nh){ i => unpack(SZ_XH, i) }
  val src_data_b = Vec.tabulate(conf.vmu.nb){ i => unpack(SZ_XB, i) }

//--------------------------------------------------------------------\\
// permutation network
//--------------------------------------------------------------------\\

  val lgbank = log2Up(conf.nbanks)

  val src_utidx = io.vmu.ldata.bits.meta.utidx
  // NOTE: 2^n encoded as 0
  val src_utcnt = Cat(io.vmu.ldata.bits.meta.utcnt === UInt(0), io.vmu.ldata.bits.meta.utcnt)
  val src_offset = io.vmu.ldata.bits.meta.offset

  // TODO: Handle configurations in which the number of elements per
  // load (src_utcnt) may exceed the number of banks

  val bank_start = src_utidx(lgbank-1, 0)
  val bank_utidx = src_utidx(SZ_VLEN-1, lgbank)
  val bank_utidx_next = bank_utidx + UInt(1)

  assert(!io.vmu.ldata.valid || src_offset === UInt(0) || bank_start === UInt(0),
    "unexpected non-zero load offset")

  val src_rotamt = Mux(src_offset != UInt(0), (UInt(conf.nbanks) - src_offset), bank_start)

  private def permute[T <: Data](src: Vec[T], sel: UInt, rev: Boolean = false): Vec[T] = {
    require(src.size > 0)
    val rot = Module(new Rotator(src(0).clone, src.size, conf.nbanks, rev))
    val dst = Vec.fill(conf.nbanks){ src(0).clone } // retain output signal names
    rot.io.in := src
    rot.io.sel := sel
    dst := rot.io.out
    return dst
  }
  val dst_data_d = permute(src_data_d, src_rotamt)
  val dst_data_w = permute(src_data_w, src_rotamt)
  val dst_data_h = permute(src_data_h, src_rotamt)
  val dst_data_b = permute(src_data_b, src_rotamt)

  val src_utcnt_sel = Vec.tabulate(src_data_b.size){
    i => (src_utcnt === Bits(i+1)) }
  val src_enable = Mux1H(src_utcnt_sel,
    Vec.tabulate(src_data_b.size, src_data_b.size){
      (i, k) => Bool(k <= i)
    })
  val dst_enable = permute(src_enable, bank_start)

//--------------------------------------------------------------------\\
// bank write queues
//--------------------------------------------------------------------\\

  val bwqs_enq_ready = Vec.fill(conf.nbanks){ Bool() }
  val vd_stride = Mux(op.reg.vd.float, MuxLookup(op.reg.vd.prec, io.cfg.ndfregs, Array(
                                                  PREC_DOUBLE -> io.cfg.ndfregs,
                                                  PREC_SINGLE -> io.cfg.nsfregs,
                                                    PREC_HALF -> io.cfg.nhfregs
                                       )),
                                       io.cfg.nxregs - UInt(1))

  val bw_stat_update = Vec.fill(conf.nbanks){ Bits() }

  val prec_d = (op.reg.vd.prec === PREC_DOUBLE)
  val prec_w = (op.reg.vd.prec === PREC_SINGLE)
  val prec_h = (op.reg.vd.prec === PREC_HALF)

  val mask = Mux1H(Vec(prec_d, prec_w, prec_h), Vec(
    Bits("b1111", SZ_BREGMASK),
    Bits("b0011", SZ_BREGMASK),
    Bits("b0001", SZ_BREGMASK)))

  for (i <- 0 until conf.nbanks) {
    val bwq = Module(new Queue(new BWQInternalEntry, conf.nbwq))

    bwq.io.enq.valid := io.vmu.ldata.valid && dst_enable(i)
    bwq.io.enq.bits.data := Mux1H(
      Vec(op_type_d, op_type_w, op_type_h, op_type_b), Vec(
        extend(dst_data_d(i), SZ_XD),
        extend(dst_data_w(i), SZ_XW),
        extend(dst_data_h(i), SZ_XH, recoded=false),
        extend(dst_data_b(i), SZ_XB, recoded=false)))

    // Handle intra-load utidx increment due to permutation "wrap-around"
    bwq.io.enq.bits.tag := Mux(UInt(i) < bank_start, bank_utidx_next, bank_utidx)
    bwqs_enq_ready(i) := bwq.io.enq.ready

    val vd_utidx = bwq.io.deq.bits.tag

    // For each halfword increment, determine which precisions have
    // valid shifts to this position, generate the corresponding enable
    // signals, and OR these together.
    // TODO: Abstract packed register format into Compaction
    val shift_sel = Vec((0 until N_XH).map(i =>
      Seq((prec_h, N_XH, 0), (prec_w, N_XW, 1), (prec_d, N_XD, 2))
        filter {case (prec_sel, density, shift) => (i & ((1 << shift) - 1)) == 0}
        map {case (prec_sel, density, shift) => if (density <= 1)
          prec_sel else
          prec_sel && (vd_utidx(log2Up(density)-1,0) === UInt(i >> shift))}
        reduce (_||_)
      ))

    def shift(data: Bits, n: Int, m: Int) = Mux1H(shift_sel,
      Vec(data +: (1 until shift_sel.length).map(i => {
        val shamt = m * i
        Cat(data(n-shamt-1,0), Fill(shamt, Bits(0)))
      })))

    io.bwqs(i).bits.data := shift(bwq.io.deq.bits.data, SZ_DATA, SZ_XH)
    io.bwqs(i).bits.mask := shift(mask, SZ_BREGMASK, 1)
    io.bwqs(i).bits.addr := op.reg.vd.id + (vd_utidx * vd_stride)
    io.bwqs(i) <> bwq.io.deq

    val bw_stat_idx = Cat(vd_utidx, UInt(i, lgbank)) - op.utidx
    bw_stat_update(i) := bwq.io.deq.fire() << bw_stat_idx
  }

  val src_ready = permute(bwqs_enq_ready, bank_start, rev=true)
  io.vmu.ldata.ready := Mux1H(src_utcnt_sel,
    Vec.tabulate(src_data_b.size) {
      i => (0 to i).map(k => src_ready(k)).reduce(_&&_)
    })

//--------------------------------------------------------------------\\
// bank write status array
//--------------------------------------------------------------------\\

  val bw_stat = Reg(Bits(width = conf.nvlreq))
  val bw_stat_next = bw_stat | bw_stat_update.reduce(_|_)
  val bw_stat_shift = io.la.cnt(SZ_LGBANK1,0) & Fill(SZ_LGBANK1, io.la.reserve)

  bw_stat := Mux1H(UIntToOH(bw_stat_shift),
    Vec((0 to conf.nbanks).map(i => (bw_stat_next >> UInt(i))))) &
    Fill(conf.nvlreq, state != s_idle) // initialization

  // Limited leading-ones count
  var sel = bw_stat(0)
  var locnt = UInt(0, log2Down(conf.max_lla) + 1)
  for (i <- 0 until conf.max_lla) {
    locnt = Mux(sel, UInt(i+1), locnt)
    sel = sel & bw_stat(i+1)
  }
  io.la.available := (locnt >= io.la.cnt)
}

class VSU(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val op = new DeckOpIO().flip

    val brqs = Vec.fill(conf.nbanks){new BRQIO().flip}
    val la = new LookAheadPortIO(conf.sz_sla).flip

    val vmu = new VMUIO
  }

  private val lgbank = log2Up(conf.nbanks)

  val op = Reg(new DeckOp)
  io.op.ready := Bool(false)

  val op_type_d = (op.fn.typ === MT_D)
  val op_type_w = (op.fn.typ === MT_W)
  val op_type_h = (op.fn.typ === MT_H)
  val op_type_b = (op.fn.typ === MT_B)
  val prec_d = (op.reg.vt.prec === PREC_DOUBLE)
  val prec_w = (op.reg.vt.prec === PREC_SINGLE)
  val prec_h = (op.reg.vt.prec === PREC_HALF)

  val type_sel = Vec(op_type_d, op_type_w, op_type_h, op_type_b)
  val prec_sel = Vec(prec_d, prec_w, prec_h)

  val utcnt = Mux1H(type_sel,
    Vec(UInt(conf.vmu.nd), UInt(conf.vmu.nw), UInt(conf.vmu.nh), UInt(conf.vmu.nb)))
  val utidx_next = op.utidx + utcnt
  val utcnt_resid = op.vlen - op.utidx
  val end = (utcnt_resid <= utcnt)
//  assert(op.utidx(lgbank-1,0) === UInt(0), "unaligned utidx")

  val id = Reg(UInt(width = log2Up(conf.nbanks * N_XH / conf.vmu.nh)))

  // Indicate transition to next subword index
  // op.utidx(log2Down(conf.nbanks / conf.vmu.n{d,w,h,b})-1,0).andR
  val wrap = Mux1H(type_sel, Vec(id(2,0).andR, id(1,0).andR, id(0), Bool(true)))

  val id_skip_d = Mux1H(type_sel, Vec(UInt(1), UInt(2), UInt(4), UInt(4)))
  val id_skip_w = Mux1H(type_sel, Vec(UInt(0), UInt(1), UInt(2), UInt(2)))
  val id_skip_h = Mux1H(type_sel, Vec(UInt(0), UInt(0), UInt(1), UInt(1)))
  val id_skip = Mux1H(prec_sel, Vec(id_skip_d, id_skip_w, id_skip_h))

  val id_stop_d = Mux1H(type_sel, Vec(UInt(7), UInt(3), UInt(1), UInt(0)))
  val id_stop_w = Mux1H(type_sel, Vec(UInt(0), UInt(7), UInt(5), UInt(2)))
  val id_stop_h = Mux1H(type_sel, Vec(UInt(0), UInt(0), UInt(7), UInt(3)))
  val id_stop = Mux1H(prec_sel, Vec(id_stop_d, id_stop_w, id_stop_h))

  val next = (id === id_stop)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      io.op.ready := Bool(true)
      when (io.op.valid) {
        state := s_busy
        op := io.op.bits
        id := UInt(0)
      }
    }
    is (s_busy) {
      when (io.vmu.sdata.fire()) {
        op.utidx := utidx_next
        id := id + Mux(wrap, id_skip, UInt(1))
        when (next) {
          id := UInt(0)
        }
        when (end) {
          state := s_idle
        }
      }
    }
  }

//--------------------------------------------------------------------\\
// bank read queues
//--------------------------------------------------------------------\\

  val slacntr_avail = Vec.fill(conf.nbanks){ Bool() }
  io.la.available := slacntr_avail.reduce(_&&_)

  val brqs_deq = Vec.tabulate(conf.nbanks){ i => {
    val brq = Module(new Queue(new BRQEntry, conf.nbrq))
    val slacntr = Module(new LookAheadCounter(conf.nbrq, conf.nbrq))

    brq.io.enq <> io.brqs(i)
    slacntr.io.la.cnt := (io.la.cnt > UInt(i))
    slacntr.io.la.reserve := io.la.reserve
    slacntr.io.inc.cnt := UInt(1)
    slacntr.io.inc.update := brq.io.deq.fire()
    slacntr.io.dec.update := Bool(false)
    slacntr_avail(i) := slacntr.io.la.available

    brq.io.deq.ready := io.vmu.sdata.ready && (state === s_busy) && (next || end)
    brq.io.deq
  }}

//--------------------------------------------------------------------\\
// floating-point recoding
//--------------------------------------------------------------------\\

  // Permute subword elements into continguous uTs
  // unpack: function to extract individual subword elements
  // ut_src: number of elements per register
  // ut_dst: number of elements per memory line
  private def permute(unpack: UnpackFn, ut_src: Int, ut_dst: Int) = {
    val ut_total = conf.nbanks * ut_src
    // Must contain at least enough elements to compose one line
    // Cannot result in a partially filled line
    require(ut_total % ut_dst == 0)
    (0 until ut_total).map(i => {
      val bank = i % conf.nbanks // bank index
      val slot = i / conf.nbanks // subword index
      unpack(brqs_deq(bank).bits.data, slot)
    }).grouped(ut_dst).map(i => Vec(i)).toIterable
  }
  require(conf.nbanks >= conf.vmu.nb)

  val brq_data_d = Vec(permute(unpack_d, N_XD, conf.vmu.nd))(id)
  val brq_data_w = Vec(permute(unpack_w, N_XW, conf.vmu.nw))(id)
  val brq_data_h = Vec(permute(unpack_h, N_XH, conf.vmu.nh))(id)
  val brq_data_b = Vec(permute(unpack_b, N_XB, conf.vmu.nb))(id)

  val data_d = Vec.tabulate(N_XD){ i => {
    val element = brq_data_d(i)
    val rf64f64 = Module(new hardfloat.recodedFloat64ToFloat64)
    rf64f64.io.in := element
    Mux(op.fn.float, rf64f64.io.out, element(SZ_XD-1,0))
  }}.reverse

  val data_w = Vec.tabulate(N_XW){ i => {
    val element = brq_data_w(i)
    val rf32f32 = Module(new hardfloat.recodedFloat32ToFloat32)
    rf32f32.io.in := element
    Mux(op.fn.float, rf32f32.io.out, element(SZ_XW-1,0))
  }}.reverse

  val data_h = brq_data_h.reverse
  val data_b = brq_data_b.reverse

  io.vmu.sdata.bits := Mux1H(type_sel,
    Vec(Cat(data_d), Cat(data_w), Cat(data_h), Cat(data_b)))

  val brq_valid_end = Mux1H(
    Vec((1 until conf.nbanks).map(i => (utcnt_resid(lgbank-1,0) === UInt(i)))),
    Vec((1 until conf.nbanks).map(i => brqs_deq.take(i).map(_.valid).reduce(_&&_))))
  val brq_valid = Mux(prec_d && (utcnt_resid(SZ_VLEN-1,lgbank) === UInt(0)),
    brq_valid_end, brqs_deq.map(_.valid).reduce(_&&_))
  io.vmu.sdata.valid := brq_valid && (state === s_busy)
}

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

    val lla = new LookAheadPortIO(log2Down(conf.nvlreq)+1).flip()
    val sla = new LookAheadPortIO(log2Down(conf.nvsdq)+1).flip()

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
    val la = new LookAheadPortIO(log2Down(conf.nvlreq)+1).flip

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

  val vldq = io.vmu.ldata
  val ld_data = vldq.bits.data
/* // Support for non-zero load shifts
  val ld_data = Mux1H(UIntToOH(vldq.bits.meta.shift),
    Vec((0 until SZ_VMU_DATA by 8).map(i => (vldq.bits.data >> UInt(i)))))
*/
//  assert(!vldq.fire() || vldq.bits.meta.utcnt === UInt(1),
//    "multiple-element loads currently unsupported by BWQs")

//--------------------------------------------------------------------\\
// floating-point recoding
//--------------------------------------------------------------------\\

  val op_signext = op.fn.signext()
  val op_type_d = is_mtype_doubleword(op.fn.typ)
  val op_type_w = is_mtype_word(op.fn.typ)
  val op_type_h = is_mtype_halfword(op.fn.typ)
  val op_type_b = is_mtype_byte(op.fn.typ)

  val lgbank = log2Up(conf.nbanks)

  private def volume(w: Int) = (conf.vmu.sz_data / w)
  private def unpack(w: Int, i: Int) = io.vmu.ldata.bits.data(((i+1)*w)-1, i*w)
  private def extend(n: Bits, w: Int) = Cat(op_signext & n(w-1), n)

  val src_data_d = Vec.tabulate(volume(SZ_XD)){ i => {
    val element = unpack(SZ_XD, i)
    val f64rf64 = Module(new hardfloat.float64ToRecodedFloat64)
    f64rf64.io.in := element
    Mux(op.fn.float, f64rf64.io.out, extend(element, SZ_XD))
  }}

  val src_data_w = Vec.tabulate(volume(SZ_XW)){ i => {
    val element = unpack(SZ_XW, i)
    val f32rf32 = Module(new hardfloat.float32ToRecodedFloat32)
    f32rf32.io.in := element
    Mux(op.fn.float, f32rf32.io.out, extend(element, SZ_XW))
  }}

  val src_data_h = Vec.tabulate(volume(SZ_XH)){ i => unpack(SZ_XH, i) }
  val src_data_b = Vec.tabulate(volume(SZ_XB)){ i => unpack(SZ_XB, i) }

//--------------------------------------------------------------------\\
// permutation network
//--------------------------------------------------------------------\\

  val src_utidx = io.vmu.ldata.bits.meta.utidx
  val src_utcnt = io.vmu.ldata.bits.meta.utcnt
  val src_shift = io.vmu.ldata.bits.meta.shift

  // TODO: Handle configurations in which the number of elements per
  // load (src_utcnt) may exceed the number of banks

  val bank_start = src_utidx(lgbank-1, 0)
  val bank_utidx = src_utidx(SZ_VLEN-1, lgbank)
  val bank_utidx_next = bank_utidx + UInt(1)

  assert(!io.vmu.ldata.valid || src_shift === UInt(0) || bank_start === UInt(0),
    "unexpected non-zero load offset")

  val src_rotamt = Mux(src_shift != UInt(0),
    (UInt(conf.nbanks) - src_shift), bank_start)

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

  val vd_bank_ut = bank_utidx
  val vd_stride = Mux(op.reg.vd.float, io.cfg.fstride, io.cfg.xstride)
  val vd_addr = (vd_bank_ut * vd_stride) + op.reg.vd.id

  val bw_stat_idx = src_utidx >> UInt(lgbank) // - op.utidx

  val bwqs_deq = new ArrayBuffer[DecoupledIO[BWQInternalEntry]]
  val bwqs_enq_ready = Vec.fill(conf.nbanks){ Bool() }

  for (i <- 0 until conf.nbanks) {
    val bwq = Module(new Queue(new BWQInternalEntry, conf.nbwq))

    bwq.io.enq.valid := io.vmu.ldata.valid && dst_enable(i)
    bwq.io.enq.bits.data := Mux1H(
      Vec(op_type_d, op_type_w, op_type_h, op_type_b),
      Vec(
        pack_d(dst_data_d(i), 0),
        pack_w(dst_data_w(i), 0),
        pack_h(extend(dst_data_h(i), SZ_XH), 0),
        pack_b(extend(dst_data_b(i), SZ_XB), 0)))

    // Handle mid-load utidx increment due to permutation "wrap-around"
    bwq.io.enq.bits.tag := Mux(UInt(i) < bank_start, bank_utidx_next, bank_utidx)
    bwq.io.enq.bits.addr := vd_addr
    bwqs_enq_ready(i) := bwq.io.enq.ready

    bwqs_deq += bwq.io.deq
  }
  io.bwqs <> Vec(bwqs_deq)

  val src_ready = permute(bwqs_enq_ready, bank_start, rev=true)
  io.vmu.ldata.ready := Mux1H(src_utcnt_sel,
    Vec.tabulate(src_data_b.size) {
      i => (0 to i).map(k => src_ready(k)).reduce(_&&_)
    })

//--------------------------------------------------------------------\\
// bank write status array
//--------------------------------------------------------------------\\

  val bw_stat = Reg(Bits(width = conf.nvlreq))
  val bw_stat_update = (0 until conf.nbanks).map(i =>
    (UIntToOH(Cat(bwqs_deq(i).bits.tag, UInt(i, lgbank)) - op.utidx) &
      Fill(conf.nvlreq, bwqs_deq(i).fire()))
    ).reduce(_|_)
  val bw_stat_next = bw_stat | bw_stat_update
  val bw_stat_shift = io.la.cnt(SZ_LGBANK1,0) & Fill(SZ_LGBANK1, io.la.reserve)

  bw_stat := Mux1H(UIntToOH(bw_stat_shift),
    Vec((0 to conf.nbanks).map(i => (bw_stat_next >> UInt(i))))) &
    Fill(conf.nvlreq, state != s_idle) // initialization

  // Limited leading-ones count
  var sel = bw_stat(0)
  var locnt = UInt(0, SZ_LGBANK1)
  for (i <- 0 until conf.nbanks) {
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
    val la = new LookAheadPortIO(log2Down(conf.nvsdq)+1).flip

    val vmu = new VMUIO
  }

  val op = Reg(new DeckOp)
  val utidx_next = op.utidx + UInt(1)

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
      when (io.vmu.sdata.fire()) {
        op.utidx := utidx_next
        when (utidx_next === op.vlen) {
          state := s_idle
        }
      }
    }
  }

//--------------------------------------------------------------------\\
// bank read queues
//--------------------------------------------------------------------\\

  val lgbank = log2Up(conf.nbanks)
  val bank_id = op.utidx(lgbank-1, 0)

  val brqs = new ArrayBuffer[DecoupledIO[BRQEntry]]
  val slacntr_avail = new ArrayBuffer[Bool]

  for (i <- 0 until conf.nbanks) {
    val brq = Module(new Queue(new BRQEntry, conf.nbrq))
    val slacntr = Module(new LookAheadCounter(conf.nbrq, conf.nbrq))

    brq.io.enq <> io.brqs(i)
    slacntr.io.la.cnt := (io.la.cnt > UInt(i))
    slacntr.io.la.reserve := io.la.reserve
    slacntr.io.inc.cnt := UInt(1)
    slacntr.io.inc.update := (bank_id === UInt(i)) && io.vmu.sdata.fire()
    slacntr.io.dec.update := Bool(false)
    slacntr_avail += slacntr.io.la.available

    brqs += brq.io.deq
  }
  io.la.available := slacntr_avail.reduce(_&&_)

  val brqs_deq = Vec(brqs)

  for (i <- 0 until conf.nbanks) {
    brqs_deq(i).ready := Bool(false)
  }

//--------------------------------------------------------------------\\
// floating-point recoding
//--------------------------------------------------------------------\\

  val br_data = brqs_deq(bank_id).bits.data

  val rf64f64 = Module(new hardfloat.recodedFloat64ToFloat64)
  rf64f64.io.in := unpack_float_d(br_data, 0)
  val br_data_f_dp = rf64f64.io.out

  val rf32f32 = Module(new hardfloat.recodedFloat32ToFloat32)
  rf32f32.io.in := unpack_float_s(br_data, 0)
  val br_data_f_sp = rf32f32.io.out

  val br_data_f_hp = unpack_float_h(br_data, 0)

  val op_fp_d = op.fn.float && (op.fn.typ === MT_D)
  val op_fp_s = op.fn.float && (op.fn.typ === MT_W)
  val op_fp_h = op.fn.float && (op.fn.typ === MT_H)

  val st_data = Mux1H(
    Vec(!op.fn.float, op_fp_d, op_fp_s, op_fp_h),
    Vec(br_data, br_data_f_dp, br_data_f_sp, br_data_f_hp))

  io.vmu.sdata.bits := st_data
  io.vmu.sdata.valid := brqs_deq(bank_id).valid && (state === s_busy)
  brqs_deq(bank_id).ready := io.vmu.sdata.ready
}

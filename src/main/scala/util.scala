package hwacha

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import scala.math._

abstract trait Packing extends LaneParameters {
  def splat_d(n: Bits) = Fill(SZ_D/SZ_D, n.asUInt)
  def splat_w(n: Bits) = Fill(SZ_D/SZ_W, n.asUInt)
  def splat_h(n: Bits) = Fill(SZ_D/SZ_H, n.asUInt)
  def splat_b(n: Bits) = Fill(SZ_D/SZ_B, n.asUInt)

  def _expand(n: Bits, s: Bits, width: Int) = {
    Cat(Fill(SZ_D - width, s.asUInt), n)
  }

  def expand_d(n: Bits) = n
  def expand_w(n: Bits) = _expand(n, n(SZ_W-1), SZ_W)
  def expand_h(n: Bits) = _expand(n, n(SZ_H-1), SZ_H)
  def expand_b(n: Bits) = _expand(n, n(SZ_B-1), SZ_B)
  def expand_float_d(n: Bits) = expand_d(n)
  def expand_float_s(n: Bits) = expand_w(n)
  def expand_float_h(n: Bits) = expand_h(n)

  def _repack(n: Seq[Bits], len: Int) = {
    require(n.length == len)
    Cat(n.reverse)
  }

  def repack_d(n: Seq[Bits]) = _repack(n, SZ_D/SZ_D)
  def repack_w(n: Seq[Bits]) = _repack(n, SZ_D/SZ_W)
  def repack_h(n: Seq[Bits]) = _repack(n, SZ_D/SZ_H)
  def repack_b(n: Seq[Bits]) = _repack(n, SZ_D/SZ_B)

  def _unpack(n: Bits, idx: Int, extent: Int, period: Int, width: Int): UInt = {
    require((idx+1)*period <= extent)
    val base = idx*period
    n(width+base-1, base)
  }
  def _unpack(n: Bits, idx: Int, extent: Int, period: Int): UInt =
    _unpack(n, idx, extent, period, period)

  def unpack_d(n: Bits, idx: Int) = _unpack(n, idx, SZ_D, SZ_D)
  def unpack_w(n: Bits, idx: Int) = _unpack(n, idx, SZ_D, SZ_W)
  def unpack_h(n: Bits, idx: Int) = _unpack(n, idx, SZ_D, SZ_H)
  def unpack_b(n: Bits, idx: Int) = _unpack(n, idx, SZ_D, SZ_B)

  def splat_slice(n: Bits) = Fill(nSlices, n.asUInt)
  def repack_slice(n: Seq[Bits]) = _repack(n, nSlices)
  def unpack_slice(n: Bits, idx: Int) =
    _unpack(n, idx, wBank, p(HwachaRegLen))
}

abstract trait BankLogic extends LaneParameters {
  def strip_to_bcnt(strip: UInt) = {
    val stripp1 = strip + 1.U
    if (nSlices > 1) stripp1 >> bSlices else strip
  }

  def strip_to_bmask(strip: UInt) = {
    EnableDecoder(strip_to_bcnt(strip), nBanks).asUInt
  }
}

abstract trait MinMax {
  def min(x: UInt, y: UInt) = Mux(x > y, y, x)
  def max(x: UInt, y: UInt) = Mux(x > y, x, y)
}

abstract trait SeqLogic extends SeqParameters {
  def find_first(v: Vec[Bool], head: UInt, fn: Int=>Bool) = {
    val internal = Wire(Vec(2*nSeq, Bool()))
    for (i <- 0 until nSeq) {
      internal(i+nSeq) := v(i) && fn(i)
      internal(i) := internal(i+nSeq) && (i.U >= head)
    }
    val priority_oh = PriorityEncoderOH(internal)
    val out = Wire(Vec(nSeq, Bool()))
    for (i <- 0 until nSeq) {
      out(i) := priority_oh(i) | priority_oh(i+nSeq)
    }
    out
  }

  def mreadfn[T <: Data](sched: Vec[Bool], me: Vec[MasterSeqEntry], rfn: MasterSeqEntry=>T) =
    rfn(me(0)).cloneType.fromBits(Mux1H(sched, me.map(rfn(_).asUInt)))

  def readfn[T <: Data](sched: Vec[Bool], e: Vec[SeqEntry], rfn: SeqEntry=>T) =
    rfn(e(0)).cloneType.fromBits(Mux1H(sched, e.map(rfn(_).asUInt)))

  def step(ptr: UInt, n: Int): UInt = {
    require(n < nSeq)
    if (isPow2(nSeq))
      ptr + UInt(n)
    else if (n == 1)
      Mux(ptr === UInt(nSeq-1), 0.U, ptr + 1.U)
    else
      ptr + Mux(ptr < UInt(nSeq-n), UInt(n), -UInt(nSeq-n, log2Up(nSeq)))
  }
}

object DataGating {
  def dgate[T <: Data](valid: Bool, b: T) = Mux(valid, b, 0.U.asTypeOf(b.cloneType))
}

object HardFloatHelper {
  def recode_dp(n: Bits) = hardfloat.recFNFromFN(11, 53, n.asUInt)
  def recode_sp(n: Bits) = hardfloat.recFNFromFN(8, 24, n.asUInt)
  def recode_hp(n: Bits) = hardfloat.recFNFromFN(5, 11, n.asUInt)
  def ieee_dp(n: Bits) = hardfloat.fNFromRecFN(11, 53, n.asUInt)
  def ieee_sp(n: Bits) = hardfloat.fNFromRecFN(8, 24, n.asUInt)
  def ieee_hp(n: Bits) = hardfloat.fNFromRecFN(5, 11, n.asUInt)
}

class MaskStall[T <: Data](data: => T) extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(data))
    val output = Decoupled(data)
    val stall = Input(Bool())
  })

  io.output.valid := io.input.valid && !io.stall
  io.output.bits := io.input.bits
  io.input.ready := io.output.ready && !io.stall
}

object MaskStall {
  def apply[T <: Data](deq: DecoupledIO[T], stall: Bool) = {
    val ms = Module(new MaskStall(deq.bits.cloneType))
    ms.suggestName("msInst")
    ms.io.input <> deq
    ms.io.stall := stall
    ms.io.output
  }
}

class QCounter(reset_cnt: Int, max_cnt: Int) extends Module() {
  val sz = log2Down(max_cnt)+1
  val io = new Bundle {
    val inc = Input(Bool())
    val dec = Input(Bool())
    val qcnt = UInt(INPUT, sz)
    val watermark = Output(Bool())
    val full = Output(Bool())
    val empty = Output(Bool())
  }

  val count = RegInit(UInt(reset_cnt, sz))

  when (io.inc ^ io.dec) {
    when (io.inc) { count := count + 1.U }
    when (io.dec) { count := count - 1.U }
  }

  io.watermark := count >= io.qcnt
  io.full := count === UInt(max_cnt)
  io.empty := count === 0.U
}

trait LookAheadIO extends HwachaBundle {
  val reserve = Output(Bool())
  val available = Input(Bool())
}

class CounterLookAheadIO(implicit p: Parameters) extends LookAheadIO with SeqParameters {
  val cnt = Output(UInt(bLookAhead.W))
}

class CounterUpdateIO(sz: Int) extends Bundle {
  val cnt = Output(UInt(sz.W))
  val update = Output(Bool())

}

class LookAheadCounter(reset_cnt: Int, max_cnt: Int, resetSignal: Bool = null)(implicit p: Parameters) extends HwachaModule(_reset = resetSignal)(p) with LaneParameters {
  require(reset_cnt <= max_cnt)
  val sz = log2Down(max_cnt)+1
  val io = IO(new Bundle {
    val inc = Flipped(new CounterUpdateIO(sz))
    val dec = Flipped(new CounterLookAheadIO())
    val full = Output(Bool())
  })

  val count = RegInit(UInt(reset_cnt, sz))
  io.dec.available := (count >= io.dec.cnt)

  val add = (io.inc.cnt & Fill(sz, io.inc.update))
  val sub = (io.dec.cnt & Fill(sz, io.dec.reserve))
  count := count + add - sub

  io.full := (count === UInt(max_cnt))
}

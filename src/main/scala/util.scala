package hwacha

import Chisel._
import scala.math._

abstract trait Packing extends LaneParameters {
  def splat_d(n: Bits) = Fill(SZ_D/SZ_D, n.toUInt)
  def splat_w(n: Bits) = Fill(SZ_D/SZ_W, n.toUInt)
  def splat_h(n: Bits) = Fill(SZ_D/SZ_H, n.toUInt)
  def splat_b(n: Bits) = Fill(SZ_D/SZ_B, n.toUInt)

  def _expand(n: Bits, s: Bits, width: Int) = {
    Cat(Fill(SZ_D - width, s.toUInt), n)
  }

  def expand_d(n: Bits) = n
  def expand_w(n: Bits) = _expand(n, n(SZ_W-1), SZ_W)
  def expand_h(n: Bits) = _expand(n, n(SZ_H-1), SZ_H)
  def expand_b(n: Bits) = _expand(n, n(SZ_B-1), SZ_B)
  def expand_float_d(n: Bits) = n
  def expand_float_s(n: Bits) = _expand(n, Bits(1), SZ_W)
  def expand_float_h(n: Bits) = _expand(n, Bits(1), SZ_H)

  def _repack(n: Seq[Bits], len: Int) = {
    require(n.length == len)
    Vec(n).toBits
  }

  def repack_d(n: Seq[Bits]) = _repack(n, SZ_D/SZ_D)
  def repack_w(n: Seq[Bits]) = _repack(n, SZ_D/SZ_W)
  def repack_h(n: Seq[Bits]) = _repack(n, SZ_D/SZ_H)
  def repack_b(n: Seq[Bits]) = _repack(n, SZ_D/SZ_B)

  def _unpack(n: Bits, idx: Int, width: Int, limit: Int) = {
    require((idx+1)*width <= limit)
    n((idx+1)*width-1, idx*width)
  }

  def unpack_d(n: Bits, idx: Int) = _unpack(n, idx, SZ_D, SZ_D)
  def unpack_w(n: Bits, idx: Int) = _unpack(n, idx, SZ_W, SZ_D)
  def unpack_h(n: Bits, idx: Int) = _unpack(n, idx, SZ_H, SZ_D)
  def unpack_b(n: Bits, idx: Int) = _unpack(n, idx, SZ_B, SZ_D)

  def splat_slice(n: Bits) = Fill(nSlices, n.toUInt)
  def repack_slice(n: Seq[Bits]) = _repack(n, nSlices)
  def unpack_slice(n: Bits, idx: Int) = _unpack(n, idx, p(HwachaRegLen), wBank)
}

abstract trait BankLogic extends LaneParameters {
  def strip_to_bcnt(strip: UInt) = {
    val stripp1 = strip + UInt(1)
    if (nSlices > 1) stripp1 >> UInt(log2Up(nSlices)) else strip
  }

  def strip_to_bmask(strip: UInt) = {
    EnableDecoder(strip_to_bcnt(strip), nBanks).toBits
  }
}

abstract trait MinMax {
  def min(x: UInt, y: UInt) = Mux(x > y, y, x)
  def max(x: UInt, y: UInt) = Mux(x > y, x, y)
}

object DataGating {
  def dgate(valid: Bool, b: Bits) = Fill(b.getWidth, valid) & b
}

object HardFloatHelper {
  def recode_dp(n: Bits) = hardfloat.floatNToRecodedFloatN(n.toUInt, 52, 12)
  def recode_sp(n: Bits) = hardfloat.floatNToRecodedFloatN(n.toUInt, 23, 9)
  def recode_hp(n: Bits) = hardfloat.floatNToRecodedFloatN(n.toUInt, 10, 6)
  def ieee_dp(n: Bits) = hardfloat.recodedFloatNToFloatN(n.toUInt, 52, 12)
  def ieee_sp(n: Bits) = hardfloat.recodedFloatNToFloatN(n.toUInt, 23, 9)
  def ieee_hp(n: Bits) = hardfloat.recodedFloatNToFloatN(n.toUInt, 10, 6)
}

class MaskStall[T <: Data](data: => T) extends Module {
  val io = new Bundle {
    val input = Decoupled(data).flip
    val output = Decoupled(data)
    val stall = Bool(INPUT)
  }

  io.output.valid := io.input.valid && !io.stall
  io.output.bits := io.input.bits
  io.input.ready := io.output.ready && !io.stall
}

object MaskStall {
  def apply[T <: Data](deq: DecoupledIO[T], stall: Bool) = {
    val ms = Module(new MaskStall(deq.bits.clone))
    ms.io.input <> deq
    ms.io.stall := stall
    ms.io.output
  }
}

class QCounter(reset_cnt: Int, max_cnt: Int, resetSignal: Bool = null) extends Module(_reset = resetSignal) {
  val sz = log2Down(max_cnt)+1
  val io = new Bundle {
    val inc = Bool(INPUT)
    val dec = Bool(INPUT)
    val qcnt = UInt(INPUT, sz)
    val watermark = Bool(OUTPUT)
    val full = Bool(OUTPUT)
    val empty = Bool(OUTPUT)
  }

  val count = Reg(init = UInt(reset_cnt, sz))

  when (io.inc ^ io.dec) {
    when (io.inc) { count := count + UInt(1) }
    when (io.dec) { count := count - UInt(1) }
  }

  io.watermark := count >= io.qcnt
  io.full := count === UInt(max_cnt)
  io.empty := count === UInt(0)
}

trait LookAheadIO extends HwachaBundle {
  val reserve = Bool(OUTPUT)
  val available = Bool(INPUT)
}

class CounterLookAheadIO(implicit p: Parameters) extends LookAheadIO with SeqParameters {
  val cnt = UInt(OUTPUT, bLookAhead)
}

class CounterUpdateIO(sz: Int) extends Bundle {
  val cnt = UInt(OUTPUT, sz)
  val update = Bool(OUTPUT)
}

class LookAheadCounter(reset_cnt: Int, max_cnt: Int, resetSignal: Bool = null)(implicit p: Parameters) extends HwachaModule(_reset = resetSignal)(p) with LaneParameters {
  val sz = log2Down(max_cnt)+1
  val io = new Bundle {
    val inc = new CounterUpdateIO(sz).flip
    val dec = new CounterLookAheadIO().flip
    val full = Bool(OUTPUT)
  }

  val count = Reg(init = UInt(reset_cnt, sz))
  io.dec.available := (count >= io.dec.cnt)

  val add = (io.inc.cnt & Fill(sz, io.inc.update))
  val sub = (io.dec.cnt & Fill(sz, io.dec.reserve))
  count := count + add - sub

  io.full := (count === UInt(max_cnt))
}

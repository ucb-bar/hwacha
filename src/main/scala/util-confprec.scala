
package hwacha

import Chisel._
import freechips.rocketchip.config._

trait PrecLogic {
  def confprec_decode(prec: UInt): Seq[(Bool, Int)] =
    Seq(PREC_D, PREC_W, PREC_H).map(_ === prec).zipWithIndex
  def confprec_stride(prec: Seq[(Bool, Int)], cfg: HwachaConfigIO): UInt =
    Mux1H(prec.map(_._1), Seq(cfg.vstride.d, cfg.vstride.w, cfg.vstride.h))
  def confprec_step(prec: UInt, idx: UInt, cfg: HwachaConfigIO): (Bool, UInt) = {
    val selp = confprec_decode(prec)
    val stride = confprec_stride(selp, cfg)
    val update = Mux1H(selp.map { case (p, i) =>
      p -> (if (i > 0) idx(i-1, 0) === UInt(0) else Bool(true)) })
    (update, stride)
  }
}

trait RateLogic extends LaneParameters {
  def rate_decode(rate: UInt): Seq[(Bool, Int)] =
    (0 to bPack).map(r => rate === UInt(r)).zipWithIndex

  def unpack_pred(n: UInt, i: Int, rate: UInt): Bits = {
    require(i <= nSlices)
    if (confprec) {
      val shift = UInt(i) << rate
      val mask = Mux1H(rate_decode(rate).map { case (r, k) =>
        r -> Fill(1 << k, UInt(1,1)) })
      ((n >> shift) & mask)(nPack-1, 0)
    } else n(i)
  }
  def repack_pred(n: Bits, rate: UInt): Bits =
    if (confprec)
      Mux1H(rate_decode(rate).map { case (r, i) =>
        val w = (1 << i) - 1
        r -> Vec((0 until wPred by nPack).map(k => n(w+k, k))).asUInt })
    else n

  def splat_scalar(uop: SRegMicroOp) =
    if (confprec)
      Mux1H(rate_decode(uop.rate).map { case (r, i) =>
        r -> Fill(nSlices << i, uop.operand((regLen >> i)-1, 0)) })
    else Fill(nSlices, uop.operand)
}

trait PackLogic extends PrecLogic with RateLogic with Packing {
  private def _prologue(pack: PackInfo, rate: UInt) = {
    val selp = confprec_decode(pack.prec)
    val shift = Mux1H(selp.map { case (p, i) =>
      p -> (pack.idx << bPack-i)(bPack-1, 0) })
    (selp, rate_decode(rate), shift)
  }

  def unpack_bank(pack: PackInfo, rate: UInt, in: BankData) = {
    val out = Wire(new BankDataEntry)
    if (confprec) {
      val (selp, selr, shift) = _prologue(pack, rate)
      val data = in.data >> Cat(shift, UInt(0, bSlices + 4))
      val fn = Seq(
        (unpack_d _, expand_d _),
        (unpack_w _, expand_w _),
        (unpack_h _, expand_h _))

      val pass = selp.map { case (p, n) => p && selr(n)._1 }
      val opts = for {
          ((p, n), (unpack, expand)) <- selp.zip(fn)
          (r, i) <- selr.take(n)
        } yield (p && r) -> {
          val msb = (regLen >> i) - 1
          Vec((0 until (nSlices << i)).map(k =>
            expand(unpack(data, k))(msb, 0))).asUInt }

      out.data := Mux1H((pass.reduce(_ || _), in.data) +: opts)
    } else {
      out.data := in.data
    }
    out
  }
  def unpack_bank(op: MicroOp with BankPack, in: Bits): BankDataEntry =
    unpack_bank(op.pack, op.rate, new BankDataEntry().fromBits(in))

  def repack_bank(pack: PackInfo, rate: UInt, in: BankData with BankPred) = {
    val out = Wire(new BankDataMaskEntry)
    if (confprec) {
      val (selp, selr, shift) = _prologue(pack, rate)
      val shift_data = Cat(shift, UInt(0, bSlices + 4))
      val shift_mask = Cat(shift, UInt(0, bSlices))

      val pass = selp.map { case (p, n) => p && selr(n)._1 }
      val opts = for ((p, n) <- selp; (r, i) <- selr.take(n))
        yield (p && r) -> {
          val (width, period) = (regLen >> n, regLen >> i)
          Vec((0 until (nSlices << i)).map(k =>
            _unpack(in.data, k, wBank, period, width))).asUInt }
      val data = Mux1H((pass.reduce(_ || _), in.data) +: opts)

      val _mask = Mux1H(selp.map { case (p, i) =>
        p -> FillInterleaved(1 << (bPack-i), in.pred) })

      val mask = (_mask << shift_mask)(wPred-1, 0)
      out.data := (data << shift_data)(wBank-1, 0)
      out.mask := FillInterleaved(SZ_H/SZ_B, mask)
    } else {
      out.data := in.data
      out.mask := FillInterleaved(regLen/SZ_B, in.pred)
    }
    out
  }
  def repack_bank(op: MicroOp with BankPack, in: BankData with BankPred)
    : BankDataMaskEntry = {
    val tmp = Wire(new BankDataPredEntry)
    tmp.data := in.data
    tmp.pred := op.pred & in.pred
    repack_bank(op.pack, op.rate, tmp)
  }
}

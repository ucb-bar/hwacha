
package hwacha

import Chisel._
import cde.Parameters

trait PrecLogic {
  def confprec_decode(prec: UInt): Seq[Bool] =
    Seq(PREC_H, PREC_W, PREC_D).map(_ === prec)
  def confprec_stride(prec: Seq[Bool], cfg: HwachaConfigIO): UInt =
    Mux1H(prec, Seq(cfg.vstride.h, cfg.vstride.w, cfg.vstride.d))
  def confprec_step(prec: UInt, idx: UInt, cfg: HwachaConfigIO): (Bool, UInt) = {
    val sel = confprec_decode(prec)
    val stride = confprec_stride(sel, cfg)
    val update = Bool(true) +:
      (1 until sel.size).map(i => idx(i-1, 0) === UInt(0))
    (Mux1H(sel, update.reverse), stride)
  }
}

trait PackLogic extends PrecLogic with Packing {
  private def _prologue(pack: PackInfo) = {
    val prec = confprec_decode(pack.prec).zipWithIndex
    val shift = Mux1H(prec.map { case (s, i) =>
      s -> Cat(pack.idx, UInt(0, i))(prec.size-2, 0)
    })
    (prec, shift)
  }

  def unpack_bank(pack: PackInfo, in: BankData) = {
    val out = new BankDataEntry
    if (confprec) {
      val (prec, shift) = _prologue(pack)
      val data = in.data >> Cat(shift, UInt(0, bSlices + 4))
      val fn = Seq(
        (unpack_h _, expand_h _),
        (unpack_w _, expand_w _))
      out.data := Mux1H((prec.init.zip(fn).map {
        case ((s, _), (ufn, efn)) =>
          s -> Vec((0 until nSlices).map(k => efn(ufn(data, k)))).toBits
        }) :+ (prec.last._1, in.data)) /* passthrough */
    } else {
      out.data := in.data
    }
    out
  }
  def unpack_bank(op: BankPack, in: Bits): BankDataEntry =
    unpack_bank(op.pack, new BankDataEntry().fromBits(in))

  def repack_bank(pack: PackInfo, in: BankData with BankPred) = {
    val out = new BankDataMaskEntry
    if (confprec) {
      val (prec, shift) = _prologue(pack)
      val shift_data = Cat(shift, UInt(0, bSlices + 4))
      val shift_mask = Cat(shift, UInt(0, bSlices))
      val data = Mux1H(prec.map { case (s, i) =>
        val sz = (1 << i) * SZ_H
        s -> Vec((0 until nSlices).map(
          unpack_slice(in.data, _, sz))).toBits
      })
      val _mask = Mux1H(prec.map { case (s, i) =>
        s -> FillInterleaved(1 << i, in.pred)
      })
      val mask = (_mask << shift_mask)((nSlices<<2)-1, 0)
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
    val tmp = new BankDataPredEntry
    tmp.data := in.data
    tmp.pred := op.pred & in.pred
    repack_bank(op.pack, tmp)
  }
}

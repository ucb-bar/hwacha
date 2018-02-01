package hwacha

import Chisel._
import freechips.rocketchip.config._
import DataGating._
import HardFloatHelper._
import scala.collection.mutable.ArrayBuffer

class FMAOperand(implicit p: Parameters) extends VXUBundle()(p)
  with LanePred with Rate {
  val fn = new VFMUFn
  val in0 = Bits(width = SZ_D)
  val in1 = Bits(width = SZ_D)
  val in2 = Bits(width = SZ_D)
}

class FMAResult extends Bundle {
  val out = Bits(OUTPUT, SZ_D)
  val exc = Bits(OUTPUT, freechips.rocketchip.tile.FPConstants.FLAGS_SZ)
}

class FMASlice(implicit p: Parameters) extends VXUModule()(p) with Packing {
  val io = new Bundle {
    val req = Valid(new FMAOperand).flip
    val resp = Valid(new FMAResult)
  }

  val pred = Mux(io.req.valid, io.req.bits.pred, Bits(0))
  val active = io.req.valid && io.req.bits.active()
  val fn = io.req.bits.fn.dgate(active)
  val in0 = io.req.bits.in0
  val in1 = io.req.bits.in1
  val in2 = io.req.bits.in2

  val fma_op = MuxCase(
    Bits("b00",2), Array(
      fn.op_is(FM_SUB, FM_MSUB) -> Bits("b01",2),
      fn.op_is(FM_NMSUB) -> Bits("b10",2),
      fn.op_is(FM_NMADD) -> Bits("b11",2)
    ))

  val one_dp = splat_d(Bits("h3FF0_0000_0000_0000", SZ_D))
  val one_sp = splat_w(Bits("h3F80_0000", SZ_W))
  val one_hp = splat_h(Bits("h3C00", SZ_H))
  val fma_multiplicand = in0
  val fma_multiplier = MuxCase(
    in1, Array(
      (fn.fp_is(FPD) && fn.op_is(FM_ADD, FM_SUB)) -> one_dp,
      (fn.fp_is(FPS) && fn.op_is(FM_ADD, FM_SUB)) -> one_sp,
      (fn.fp_is(FPH) && fn.op_is(FM_ADD, FM_SUB)) -> one_hp
    ))

  val fma_addend = MuxCase(
    in2, Array(
      fn.op_is(FM_ADD, FM_SUB) -> in1,
      fn.op_is(FM_MUL) -> Bits(0, SZ_D)
    ))

  val results =
    List((stagesDFMA, SZ_D, FPD, recode_dp _, unpack_d _, ieee_dp _, repack_d _, expand_float_d _, (11, 53)),
         (stagesSFMA, SZ_W, FPS, recode_sp _, unpack_w _, ieee_sp _, repack_w _, expand_float_s _, (8, 24)),
         (stagesHFMA, SZ_H, FPH, recode_hp _, unpack_h _, ieee_hp _, repack_h _, expand_float_h _, (5, 11))) map {
      case (stages, sz, fp, recode, unpack, ieee, repack, expand, (exp, sig)) => {
        val n = SZ_D / sz
        val val_fp = fn.fp_is(fp)
        val results = for (i <- (0 until n) if (confprec || i == 0)) yield {
          val fma = Module(new freechips.rocketchip.tile.MulAddRecFNPipe(stages min 2, exp, sig))
          fma.suggestName("fmaInst")
          val valid = pred(i) && val_fp
          fma.io.validin := valid
          fma.io.op := dgate(valid, fma_op)
          fma.io.a := recode(dgate(valid, unpack(fma_multiplicand, i)))
          fma.io.b := recode(dgate(valid, unpack(fma_multiplier, i)))
          fma.io.c := recode(dgate(valid, unpack(fma_addend, i)))
          fma.io.roundingMode := dgate(valid, fn.rm)
          val out = Pipe(fma.io.validout, ieee(fma.io.out), (stages - 2) max 0).bits
          val exc = Pipe(fma.io.validout, fma.io.exceptionFlags, (stages - 2) max 0).bits
          (out, exc)
        }
        val valid = active && val_fp
        val out_head = expand(results.head._1)
        val out = if (results.size > 1) {
          val rmatch = (io.req.bits.rate === UInt(log2Ceil(n)))
          Mux(Pipe(valid, rmatch, stages).bits, repack(results.map(_._1)), out_head)
        } else out_head
        val exc = results.map(_._2).reduce(_|_)
        (ShiftRegister(valid, stages), out, exc)
      }
    }

  val fpmatch = results.map(_._1)
  io.resp.valid := fpmatch.reduce(_ || _)
  io.resp.bits.out := Mux1H(fpmatch, results.map(_._2))
  io.resp.bits.exc := Mux1H(fpmatch, results.map(_._3))
}

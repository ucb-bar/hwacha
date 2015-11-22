package hwacha

import Chisel._
import cde.Parameters
import DataGating._
import HardFloatHelper._
import scala.collection.mutable.ArrayBuffer

class FMAOperand(implicit p: Parameters) extends VXUBundle()(p) with Rate {
  val fn = new VFMUFn
  val in0 = Bits(width = SZ_D)
  val in1 = Bits(width = SZ_D)
  val in2 = Bits(width = SZ_D)
}

class FMAResult extends Bundle {
  val out = Bits(OUTPUT, SZ_D)
  val exc = Bits(OUTPUT, rocket.FPConstants.FLAGS_SZ)
}

class FMASlice(implicit p: Parameters) extends VXUModule()(p) with Packing {
  val io = new Bundle {
    val req = new LaneValidIO(new FMAOperand).flip
    val resp = Valid(new FMAResult)
  }

  val active = io.req.active()
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
    List((SZ_D, FPD, recode_dp _, unpack_d _, ieee_dp _, repack_d _, expand_float_d _, (11, 53)),
         (SZ_W, FPS, recode_sp _, unpack_w _, ieee_sp _, repack_w _, expand_float_s _, (8, 24)),
         (SZ_H, FPH, recode_hp _, unpack_h _, ieee_hp _, repack_h _, expand_float_h _, (5, 11))) map {
      case (sz, fp, recode, unpack, ieee, repack, expand, (exp, sig)) => {
        val n = SZ_D / sz
        val outs = new ArrayBuffer[Bits](n)
        val excs = new ArrayBuffer[Bits](n)
        val valid_fp = fn.fp_is(fp)
        for (i <- (0 until n)) {
          if (confprec || i == 0) {
            val fma = Module(new hardfloat.MulAddRecFN(exp, sig))
            val valid = valid_fp && io.req.valid(i)
            fma.io.op := dgate(valid, fma_op)
            fma.io.a := recode(dgate(valid, unpack(fma_multiplicand, i)))
            fma.io.b := recode(dgate(valid, unpack(fma_multiplier, i)))
            fma.io.c := recode(dgate(valid, unpack(fma_addend, i)))
            fma.io.roundingMode := dgate(valid, fn.rm)
            outs += ieee(fma.io.out)
            excs += fma.io.exceptionFlags
          }
        }
        val out_solo = expand(outs.head)
        val out = if (confprec) {
          val rmatch = (io.req.bits.rate === UInt(log2Ceil(n)))
          Mux(rmatch, repack(outs), out_solo)
        } else out_solo
        (out, excs.reduce(_|_))
      }
    }

  val fpmatch = List(FPD, FPS, FPH).map { fn.fp_is(_) }
  val result = new FMAResult
  result.out := Mux1H(fpmatch, results.map { _._1 })
  result.exc := Mux1H(fpmatch, results.map { _._2 })

  io.resp := Pipe(active, result, stagesFMA)
}

package hwacha

import Chisel._
import DataGating._
import HardFloatHelper._
import scala.collection.mutable.ArrayBuffer

class FMAOperand extends Bundle {
  val fn = new VFMUFn
  val in0 = Bits(width = SZ_D)
  val in1 = Bits(width = SZ_D)
  val in2 = Bits(width = SZ_D)
}

class FMAResult extends Bundle {
  val out = Bits(OUTPUT, SZ_D)
  val exc = Bits(OUTPUT, rocket.FPConstants.FLAGS_SZ)
}

class FMASlice extends VXUModule with Packing {
  val io = new Bundle {
    val req = Valid(new FMAOperand).flip
    val resp = Valid(new FMAResult)
  }

  val fn = io.req.bits.fn.dgate(io.req.valid)
  val in0 = dgate(io.req.valid, io.req.bits.in0)
  val in1 = dgate(io.req.valid, io.req.bits.in1)
  val in2 = dgate(io.req.valid, io.req.bits.in2)

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
      (fn.fp_is(FPH) && fn.op_is(FM_ADD, FM_SUB)) -> one_hp,
      fn.op_is(FM_MUL) -> in2
    ))

  val fma_addend = MuxCase(
    in2, Array(
      fn.op_is(FM_ADD, FM_SUB) -> in1,
      fn.op_is(FM_MUL) -> Bits(0, SZ_D)
    ))

  val results =
    List((SZ_D, FPD, recode_dp _, unpack_d _, ieee_dp _, repack_d _, expand_float_d _, (52, 12)),
         (SZ_W, FPS, recode_sp _, unpack_w _, ieee_sp _, repack_w _, expand_float_s _, (23, 9)),
         (SZ_H, FPH, recode_hp _, unpack_h _, ieee_hp _, repack_h _, expand_float_h _, (10, 6))) map {
      case (sz, fp, recode, unpack, ieee, repack, expand, (sig, exp)) => {
        val outs = new ArrayBuffer[Bits]
        val excs = new ArrayBuffer[Bits]
        for (i <- 0 until (SZ_D/sz)) {
          if (confprec || i == 0) {
            val fma = Module(new hardfloat.mulAddSubRecodedFloatN(sig, exp))
            val valid = fn.fp_is(fp)
            fma.io.op := dgate(valid, fma_op)
            fma.io.a := recode(dgate(valid, unpack(fma_multiplicand, i)))
            fma.io.b := recode(dgate(valid, unpack(fma_multiplier, i)))
            fma.io.c := recode(dgate(valid, unpack(fma_addend, i)))
            fma.io.roundingMode := dgate(valid, fn.rm)
            outs += ieee(fma.io.out)
            excs += fma.io.exceptionFlags
          }
        }
        (if (confprec) repack(outs) else expand(outs(0)), excs.reduce(_|_))
      }
    }

  val fpmatch = List(FPD, FPS, FPH).map { fn.fp_is(_) }
  val result = new FMAResult
  result.out := Mux1H(fpmatch, results.map { _._1 })
  result.exc := Mux1H(fpmatch, results.map { _._2 })

  io.resp := Pipe(io.req.valid, result, stagesFMA)
}

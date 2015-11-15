package hwacha

import Chisel._
import cde.Parameters
import DataGating._
import HardFloatHelper._

class FConvOperand(implicit p: Parameters) extends VXUBundle()(p) {
  val fn = new VFVUFn
  val in = Bits(width = SZ_D)
}

class FConvResult extends Bundle {
  val out = Bits(OUTPUT, SZ_D)
  val exc = Bits(OUTPUT, rocket.FPConstants.FLAGS_SZ)
}

class FConvSlice(implicit p: Parameters) extends VXUModule()(p) with Packing {
  val io = new Bundle {
    val req = Valid(new FConvOperand).flip
    val resp = Valid(new FConvResult)
  }

  val fn = io.req.bits.fn.dgate(io.req.valid)
  val in = dgate(io.req.valid, io.req.bits.in)

  val op_int2float = MuxCase(
    Bits(0), Array(
      fn.op_is(FV_CLTF)  -> UInt("b11"),
      fn.op_is(FV_CLUTF) -> UInt("b10"),
      fn.op_is(FV_CWTF)  -> UInt("b01"),
      fn.op_is(FV_CWUTF) -> UInt("b00")
    ))

  val op_float2int = MuxCase(
    Bits(0), Array(
      fn.op_is(FV_CFTL)  -> UInt("b11"),
      fn.op_is(FV_CFTLU) -> UInt("b10"),
      fn.op_is(FV_CFTW)  -> UInt("b01"),
      fn.op_is(FV_CFTWU) -> UInt("b00")
    ))

  val val_int2float = fn.op_is(FV_CLTF,FV_CLUTF,FV_CWTF,FV_CWUTF)
  val val_float2int32 = fn.op_is(FV_CFTW,FV_CFTWU)
  val val_float2int64 = fn.op_is(FV_CFTL,FV_CFTLU)
  val val_float2int = val_float2int32 || val_float2int64

  val wdp = (11, 53)
  val wsp = (8, 24)
  val whp = (5, 11)

  val results_int2float =
    List((FPD, ieee_dp _, expand_float_d _, wdp),
         (FPS, ieee_sp _, expand_float_s _, wsp),
         (FPH, ieee_hp _, expand_float_h _, whp)) map {
      case (fp, ieee, expand, (exp, sig)) => {
        val valid = fn.fp_is(fp) && val_int2float
        val input = dgate(valid, in)
        val rm = dgate(valid, fn.rm)
        val op = dgate(valid, op_int2float)
        val l2fp = Module(new hardfloat.INToRecFN(SZ_D, exp, sig))
        val w2fp = Module(new hardfloat.INToRecFN(SZ_W, exp, sig))
        l2fp.io.signedIn := op(0)
        l2fp.io.in := input
        l2fp.io.roundingMode := rm
        w2fp.io.signedIn := op(0)
        w2fp.io.in := input
        w2fp.io.roundingMode := rm
        val output = Mux(op(1), l2fp.io.out, w2fp.io.out)
        val exc = Mux(op(1), l2fp.io.exceptionFlags, w2fp.io.exceptionFlags)
        (expand(ieee(output)), exc)
      }
    }

  val results_float2int =
    List((FPD, recode_dp _, unpack_d _, wdp),
         (FPS, recode_sp _, unpack_w _, wsp),
         (FPH, recode_hp _, unpack_h _, whp)) map {
      case (fp, recode, unpack, (exp, sig)) => {
        val valid = fn.fp_is(fp) && val_float2int
        val input = recode(dgate(valid, unpack(in, 0)))
        val rm = dgate(valid, fn.rm)
        val op = dgate(valid, op_float2int)
        val fp2l = Module(new hardfloat.RecFNToIN(exp, sig, SZ_D))
        val fp2w = Module(new hardfloat.RecFNToIN(exp, sig, SZ_W))
        fp2l.io.signedOut := op(0)
        fp2l.io.in := input
        fp2l.io.roundingMode := rm
        fp2w.io.signedOut := op(0)
        fp2w.io.in := input
        fp2w.io.roundingMode := rm
        val output = Mux(op(1), fp2l.io.out, expand_w(fp2w.io.out))
        val iexc = Mux(op(1), fp2l.io.intExceptionFlags, fp2w.io.intExceptionFlags)
        (output, Cat(iexc(2, 1).orR, UInt(0, 3), iexc(0)))
      }
    }

  val results_float2float =
    List((FV_CSTD, recode_sp _, unpack_w _, ieee_dp _, expand_float_d _, wsp, wdp),
         (FV_CHTD, recode_hp _, unpack_h _, ieee_dp _, expand_float_d _, whp, wdp),
         (FV_CDTS, recode_dp _, unpack_d _, ieee_sp _, expand_float_s _, wdp, wsp),
         (FV_CHTS, recode_hp _, unpack_h _, ieee_sp _, expand_float_s _, whp, wsp),
         (FV_CDTH, recode_dp _, unpack_d _, ieee_hp _, expand_float_h _, wdp, whp),
         (FV_CSTH, recode_sp _, unpack_w _, ieee_hp _, expand_float_h _, wsp, whp)) map {
      case (op, recode, unpack, ieee, expand, (exps, sigs), (expd, sigd)) => {
        val valid = fn.op_is(op)
        val input = recode(dgate(valid, unpack(in, 0)))
        val rm = dgate(valid, fn.rm)
        val fp2fp = Module(new hardfloat.RecFNToRecFN(exps, sigs, expd, sigd))
        fp2fp.io.in := input
        fp2fp.io.roundingMode := rm
        (expand(ieee(fp2fp.io.out)), fp2fp.io.exceptionFlags)
      }
    }

  val outs =
    List((FV_CSTD, FV_CHTD), (FV_CDTS, FV_CHTS), (FV_CDTH, FV_CSTH)).zipWithIndex.map {
      case ((op0, op1), i) =>
        MuxCase(Bits(0), Array(
          val_int2float -> results_int2float(i)._1,
          val_float2int -> results_float2int(i)._1,
          fn.op_is(op0) -> results_float2float(2*i)._1,
          fn.op_is(op1) -> results_float2float(2*i+1)._1
        ))
    }

  val excs =
    List((FV_CSTD, FV_CHTD), (FV_CDTS, FV_CHTS), (FV_CDTH, FV_CSTH)).zipWithIndex.map {
      case ((op0, op1), i) =>
        MuxCase(Bits(0), Array(
          val_int2float -> results_int2float(i)._2,
          val_float2int -> results_float2int(i)._2,
          fn.op_is(op0) -> results_float2float(2*i)._2,
          fn.op_is(op1) -> results_float2float(2*i+1)._2
        ))
    }

  val fpmatch = List(FPD, FPS, FPH).map { fn.fp_is(_) }
  val result = new FConvResult
  result.out := Mux1H(fpmatch, outs)
  result.exc := Mux1H(fpmatch, excs)

  io.resp := Pipe(io.req.valid, result, stagesFConv)
}

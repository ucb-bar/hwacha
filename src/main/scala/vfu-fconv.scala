package hwacha

import Chisel._
import freechips.rocketchip.config._
import DataGating._
import HardFloatHelper._
import scala.collection.mutable.ArrayBuffer

class FConvOperand(implicit p: Parameters) extends VXUBundle()(p)
  with LanePred with Rate {
  val fn = new VFVUFn
  val in = Bits(width = SZ_D)
}

class FConvResult extends Bundle {
  val out = Bits(OUTPUT, SZ_D)
  val exc = Bits(OUTPUT, freechips.rocketchip.tile.FPConstants.FLAGS_SZ)
}

class FConvSlice(implicit p: Parameters) extends VXUModule()(p) with Packing {
  val io = new Bundle {
    val req = Valid(new FConvOperand).flip
    val resp = Valid(new FConvResult)
  }

  val pred = Mux(io.req.valid, io.req.bits.pred, Bits(0))
  val active = io.req.valid && io.req.bits.active()
  val fn = io.req.bits.fn.dgate(active)
  val in = io.req.bits.in

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

  private def pipe[T <: Data](in: T) = Pipe(active, in, stagesFConv).bits
  private def pipe(valid: Bool, out: Bits, exc: Bits, fn: Bits=>Bits = identity) =
    (fn(Pipe(valid, out, stagesFConv).bits), Pipe(valid, exc, stagesFConv).bits)

  val results_int2float =
    List((FPD, ieee_dp _, expand_float_d _, wdp),
         (FPS, ieee_sp _, expand_float_s _, wsp),
         (FPH, ieee_hp _, expand_float_h _, whp)) map {
      case (fp, ieee, expand, (exp, sig)) => {
        val valid = fn.fp_is(fp) && val_int2float && pred(0)
        val input = dgate(valid, in)
        val rm = dgate(valid, fn.rm)
        val op = dgate(valid, op_int2float)
        val l2fp = Module(new hardfloat.INToRecFN(SZ_D, exp, sig))
        l2fp.suggestName("l2fpInst")
        val w2fp = Module(new hardfloat.INToRecFN(SZ_W, exp, sig))
        w2fp.suggestName("w2fpInst")
        l2fp.io.signedIn := op(0)
        l2fp.io.in := input
        l2fp.io.roundingMode := rm
        w2fp.io.signedIn := op(0)
        w2fp.io.in := input
        w2fp.io.roundingMode := rm
        val output = Mux(op(1), l2fp.io.out, w2fp.io.out)
        val exc = Mux(op(1), l2fp.io.exceptionFlags, w2fp.io.exceptionFlags)
        pipe(valid, ieee(output), exc, expand)
      }
    }

  val results_float2int =
    List((FPD, recode_dp _, unpack_d _, wdp),
         (FPS, recode_sp _, unpack_w _, wsp),
         (FPH, recode_hp _, unpack_h _, whp)) map {
      case (fp, recode, unpack, (exp, sig)) => {
        val valid = fn.fp_is(fp) && val_float2int && pred(0)
        val input = recode(dgate(valid, unpack(in, 0)))
        val rm = dgate(valid, fn.rm)
        val op = dgate(valid, op_float2int)
        val fp2l = Module(new hardfloat.RecFNToIN(exp, sig, SZ_D))
        fp2l.suggestName("fp2lInst")
        val fp2w = Module(new hardfloat.RecFNToIN(exp, sig, SZ_W))
        fp2w.suggestName("fp2wInst")
        fp2l.io.signedOut := op(0)
        fp2l.io.in := input
        fp2l.io.roundingMode := rm
        fp2w.io.signedOut := op(0)
        fp2w.io.in := input
        fp2w.io.roundingMode := rm
        val output = Mux(op(1), fp2l.io.out, expand_w(fp2w.io.out))
        val iexc = Mux(op(1), fp2l.io.intExceptionFlags, fp2w.io.intExceptionFlags)
        pipe(valid, output, Cat(iexc(2, 1).orR, UInt(0, 3), iexc(0)))
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
        val (szs, szd) = (exps + sigs, expd + sigd)
        val sz = math.max(szs, szd)
        val (m, n) = (math.max(szd / szs, 1), regLen / sz)
        val val_op = fn.op_is(op)
        val results = for (i <- (0 until n) if (confprec || i == 0)) yield {
          val fp2fp = Module(new hardfloat.RecFNToRecFN(exps, sigs, expd, sigd))
          fp2fp.suggestName("fp2fpInst")
          val valid = pred(i) && val_op
          fp2fp.io.in := recode(dgate(valid, unpack(in, i * m)))
          fp2fp.io.roundingMode := dgate(valid, fn.rm)
          pipe(valid, ieee(fp2fp.io.out), fp2fp.io.exceptionFlags, expand)
        }
        val valid = active && val_op
        val output = if (results.size > 1) {
          val rmatch = (io.req.bits.rate === UInt(log2Ceil(n)))
          Mux(Pipe(valid, rmatch, stagesFConv).bits,
            Vec(results.map(_._1(sz-1, 0))).asUInt, results.head._1)
        } else results.head._1
        (output, results.map(_._2).reduce(_.asUInt | _.asUInt ))
      }
    }

  val val_int2float_pipe = pipe(val_int2float)
  val val_float2int_pipe = pipe(val_float2int)
  val fn_pipe = pipe(fn)

  val results =
    List((FV_CSTD, FV_CHTD), (FV_CDTS, FV_CHTS), (FV_CDTH, FV_CSTH)).zipWithIndex.map {
      case ((op0, op1), i) => Mux1H(Seq(
        val_int2float_pipe -> results_int2float(i),
        val_float2int_pipe -> results_float2int(i),
        fn_pipe.op_is(op0) -> results_float2float(2*i),
        fn_pipe.op_is(op1) -> results_float2float(2*i+1)
      ) map { case (sel, (out, exc)) =>
        val result = Wire(new FConvResult)
        result.out := out
        result.exc := exc
        sel -> result
      })
    }

  val fpmatch = Seq(FPD, FPS, FPH).map(fn_pipe.fp_is(_))
  io.resp.valid := ShiftRegister(active, stagesFConv)
  io.resp.bits := Mux1H(fpmatch, results)
}

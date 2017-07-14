package hwacha

import Chisel._
import freechips.rocketchip.config._
import DataGating._
import HardFloatHelper._
import freechips.rocketchip.tile.FType

class FCmpOperand(implicit p: Parameters) extends VXUBundle()(p) {
  val fn = new VFCUFn
  val in0 = Bits(width = SZ_D)
  val in1 = Bits(width = SZ_D)
}

class FCmpResult(implicit p: Parameters) extends VXUBundle()(p) {
  val out = Bits(width = SZ_D)
  val cmp = Bits(width = nPack)
}

class FCmpSlice(implicit p: Parameters) extends VXUModule()(p) with Packing with freechips.rocketchip.tile.HasFPUParameters {
  val io = new Bundle {
    val req = Valid(new FCmpOperand).flip
    val resp = Valid(new FCmpResult)
  }

  val fn = io.req.bits.fn.dgate(io.req.valid)
  val in0 = dgate(io.req.valid, io.req.bits.in0)
  val in1 = dgate(io.req.valid, io.req.bits.in1)

  val wdp = (11, 53)
  val wsp = (8, 24)
  val whp = (5, 11)

  val val_cmp = fn.op_is(FC_CEQ,FC_CLT,FC_CLE,FC_MIN,FC_MAX)

  val ins =
    List((FPD, recode_dp _, unpack_d _),
         (FPS, recode_sp _, unpack_w _),
         (FPH, recode_hp _, unpack_h _)) map {
      case (fp, recode, unpack) => {
        val valid = fn.fp_is(fp)
        val input0 = recode(dgate(valid, unpack(in0, 0)))
        val input1 = recode(dgate(valid, unpack(in1, 0)))
        (input0, input1)
      }
    }

  val cmps =
    ins zip List(wdp, wsp, whp) map {
      case ((input0, input1), (exp, sig)) => {
        val comp = Module(new hardfloat.CompareRecFN(exp, sig))
        comp.suggestName("compInst")
        comp.io.a := input0
        comp.io.b := input1
        comp.io.signaling := Bool(true)
        comp.io
      }
    }

  val classifys =
    ins zip List(wdp, wsp, whp) map {
      case ((input0, input1), (exp, sig)) => {
        val c0 = FType(exp, sig).classify(input0)
        val c1 = FType(exp, sig).classify(input1)
        (c0, c1)
      }
    }

  val results =
    List((unpack_d _, expand_float_d _, FType.D),
         (unpack_w _, expand_float_s _, FType.S),
         (unpack_h _, expand_float_h _, FType(whp._1, whp._2))) zip cmps zip ins zip classifys map {
      case ((((unpack, expand, fType), cmp), (input0, input1)), classify) => {
        val less = cmp.lt || (input0.asSInt < 0.S && input1.asSInt >= 0.S)
        val in0_nan = fType.isNaN(input0)
        val in1_nan = fType.isNaN(input1)
        val isInvalid = fType.isSNaN(input0) || fType.isSNaN(input1)
        val isNaNOut = (in0_nan && in1_nan)
        val want_min = in1_nan || (fn.op_is(FC_MIN) === less) && !in0_nan
        val in0_minmax = expand(unpack(in0, 0))
        val in1_minmax = expand(unpack(in1, 0))
        val qnan = fType.qNaN
        val ieeeNaN = if(fType == FType.S || fType == FType.D) ieee(qnan, fType) else qnan
        val minmax =
          Mux(isNaNOut, ieeeNaN, Mux(want_min, in0_minmax, in1_minmax))
        val sel = List(FC_MIN,FC_MAX,FC_CLASS).map(fn.op_is(_))
        val in = List(
          minmax,        // FC_MIN
          minmax,        // FC_MAX
          classify._1)   // FC_CLASS
        Mux1H(sel, in)
      }
    }

  val cmp_results =
    List(expand_float_d _, expand_float_s _, expand_float_h _) zip cmps zip classifys map {
      case ((expand, cmp), classify) => {
        val less = cmp.lt
        val equal = cmp.eq
        val sel = List(FC_CEQ,FC_CLT,FC_CLE).map(fn.op_is(_))
        val in = List(
          equal,         // FC_CEQ
          less,          // FC_CLT
          equal || less) // FC_CLE
        Mux1H(sel, in)
      }
    }


  val fpmatch = List(FPD, FPS, FPH).map { fn.fp_is(_) }
  val result = Wire(new FCmpResult)
  result.out := Mux1H(fpmatch, results)
  result.cmp := Mux1H(fpmatch, cmp_results)

  io.resp := Pipe(io.req.valid, result, stagesFCmp)
}

package hwacha

import Chisel._
import cde.Parameters
import DataGating._
import HardFloatHelper._

class FCmpOperand(implicit p: Parameters) extends VXUBundle()(p) {
  val fn = new VFCUFn
  val in0 = Bits(width = SZ_D)
  val in1 = Bits(width = SZ_D)
}

class FCmpResult extends Bundle {
  val out = Bits(OUTPUT, SZ_D)
  val cmp = Bool()
}

class FCmpSlice(implicit p: Parameters) extends VXUModule()(p) with Packing {
  val io = new Bundle {
    val req = Valid(new FCmpOperand).flip
    val resp = Valid(new FCmpResult)
  }

  val fn = io.req.bits.fn.dgate(io.req.valid)
  val in0 = dgate(io.req.valid, io.req.bits.in0)
  val in1 = dgate(io.req.valid, io.req.bits.in1)

  val wdp = (52, 12)
  val wsp = (23, 9)
  val whp = (10, 6)

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
      case ((input0, input1), (sig, exp)) => {
        val comp = Module(new hardfloat.recodedFloatNCompare(sig, exp))
        comp.io.a := input0
        comp.io.b := input1
        comp.io
      }
    }

  val classifys =
    ins zip List(wdp, wsp, whp) map {
      case ((input0, input1), (sig, exp)) => {
        val c0 = hardfloat.recodedFloatNClassify(input0, sig, exp)
        val c1 = hardfloat.recodedFloatNClassify(input1, sig, exp)
        (c0, c1)
      }
    }

  val results =
    List(expand_float_d _, expand_float_s _, expand_float_h _) zip cmps zip classifys map {
      case ((expand, cmp), classify) => {
        val less = cmp.a_lt_b
        val want_min = fn.op_is(FC_MIN)
        val in0_nan = classify._1(8) || classify._1(9) // isNaN, 8: sNaN, 9: qNaN
        val in1_nan = classify._2(8) || classify._2(9) // isNaN, 8: sNaN, 9: qNaN
        val minmax =
          Mux(in1_nan || !in0_nan && (want_min === less), expand(in0), expand(in1))
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
        val less = cmp.a_lt_b
        val equal = cmp.a_eq_b
        val sel = List(FC_CEQ,FC_CLT,FC_CLE).map(fn.op_is(_))
        val in = List(
          equal,         // FC_CEQ
          less,          // FC_CLT
          equal || less) // FC_CLE
        Mux1H(sel, in)
      }
    }


  val fpmatch = List(FPD, FPS, FPH).map { fn.fp_is(_) }
  val result = new FCmpResult
  result.out := Mux1H(fpmatch, results)
  result.cmp := Mux1H(fpmatch, cmp_results)

  io.resp := Pipe(io.req.valid, result, stagesFCmp)
}

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
        comp.io.a := input0
        comp.io.b := input1
        comp.io.signaling := Bool(true)
        comp.io
      }
    }

  val classifys =
    ins zip List(wdp, wsp, whp) map {
      case ((input0, input1), (exp, sig)) => {
        val c0 = rocket.ClassifyRecFN(exp, sig, input0)
        val c1 = rocket.ClassifyRecFN(exp, sig, input1)
        (c0, c1)
      }
    }

  val results =
    List((unpack_d _, expand_float_d _),
         (unpack_w _, expand_float_s _),
         (unpack_h _, expand_float_h _)) zip cmps zip classifys map {
      case (((unpack, expand), cmp), classify) => {
        val less = cmp.lt
        val want_min = fn.op_is(FC_MIN)
        val in0_nan = classify._1(8) || classify._1(9) // isNaN, 8: sNaN, 9: qNaN
        val in1_nan = classify._2(8) || classify._2(9) // isNaN, 8: sNaN, 9: qNaN
        val in0_minmax = expand(unpack(in0, 0))
        val in1_minmax = expand(unpack(in1, 0))
        val minmax =
          Mux(in1_nan || !in0_nan && (want_min === less), in0_minmax, in1_minmax)
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
  val result = new FCmpResult
  result.out := Mux1H(fpmatch, results)
  result.cmp := Mux1H(fpmatch, cmp_results)

  io.resp := Pipe(io.req.valid, result, stagesFCmp)
}

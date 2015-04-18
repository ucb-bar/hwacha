package hwacha

import Chisel._
import DataGating._
import HardFloatHelper._

class FDivOperand extends Bundle {
  val fn = new VFDUFn
  val in0 = Bits(width = SZ_D)
  val in1 = Bits(width = SZ_D)
}

class FDivResult extends Bundle {
  val out = Bits(width = SZ_D)
  val exc = Bits(width = rocket.FPConstants.FLAGS_SZ)
}

class FDivIO extends Bundle {
  val req = Decoupled(new FDivOperand)
  val resp = Decoupled(new FDivResult).flip
}

class FDivTag extends Bundle {
  val fn = new VFDUFn
  val exc = Bits(width = rocket.FPConstants.FLAGS_SZ)
}

class FDivSlice extends VXUModule with Packing {
  val io = new FDivIO().flip

  val qcnt = Module(new QCounter(nDecoupledUnitWBQueue, nDecoupledUnitWBQueue))

  qcnt.io.dec := io.req.fire()
  qcnt.io.inc := io.resp.fire()

  // stage0
  val ins = List(io.req.bits.in0, io.req.bits.in1) map { in =>
    val dp = recode_dp(in)
    val sp = hardfloat.recodedFloatNToRecodedFloatM(recode_sp(in), io.req.bits.fn.rm, 23, 9, 52, 12)
    val hp = hardfloat.recodedFloatNToRecodedFloatM(recode_hp(in), io.req.bits.fn.rm, 10, 6, 52, 12)
    val out = Mux(io.req.bits.fn.fp_is(FPD), dp,
              Mux(io.req.bits.fn.fp_is(FPS), sp._1, hp._1)) 
    val exc = Mux(io.req.bits.fn.fp_is(FPD), Bits(0),
              Mux(io.req.bits.fn.fp_is(FPS), sp._2, hp._2))
    (out, exc)
  }

  val in0q = Module(new Queue(Bits(width = 65), 2))
  val in1q = Module(new Queue(Bits(width = 65), 2))
  val intagq = Module(new Queue(new FDivTag, 2))

  val s0_op_div = io.req.bits.fn.op_is(FD_DIV)

  in0q.io.enq.valid := io.req.valid && s0_op_div
  in0q.io.enq.bits := ins(0)._1
  in1q.io.enq.valid := io.req.valid
  in1q.io.enq.bits := ins(1)._1
  intagq.io.enq.valid := io.req.valid
  intagq.io.enq.bits.fn := io.req.bits.fn
  intagq.io.enq.bits.exc := dgate(s0_op_div, ins(0)._2) | ins(1)._2

  io.req.ready :=
    !qcnt.io.empty && intagq.io.enq.ready &&
    (!s0_op_div || in0q.io.enq.ready) && in1q.io.enq.ready

  // stage1
  val div = Module(new hardfloat.divSqrtRecodedFloat64)
  val outtagq = Module(new Queue(new FDivTag, nDecoupledUnitWBQueue))

  val s1_op_div = intagq.io.deq.bits.fn.op_is(FD_DIV)
  val mask_in0q_valid = !s1_op_div || in0q.io.deq.valid
  val mask_div_ready = s1_op_div && div.io.inReady_div || !s1_op_div && div.io.inReady_sqrt

  div.io.inValid :=
    mask_in0q_valid && in1q.io.deq.valid &&
    intagq.io.deq.valid && outtagq.io.enq.ready

  in0q.io.deq.ready :=
    in1q.io.deq.valid &&
    mask_div_ready && intagq.io.deq.valid && outtagq.io.enq.ready

  in1q.io.deq.ready :=
    mask_in0q_valid &&
    mask_div_ready && intagq.io.deq.valid && outtagq.io.enq.ready

  intagq.io.deq.ready :=
    mask_in0q_valid && in1q.io.deq.valid &&
    mask_div_ready && outtagq.io.enq.ready

  outtagq.io.enq.valid :=
    mask_in0q_valid && in1q.io.deq.valid &&
    mask_div_ready && intagq.io.deq.valid

  div.io.sqrtOp := !s1_op_div
  div.io.a := in0q.io.deq.bits
  div.io.b := in1q.io.deq.bits
  div.io.roundingMode := intagq.io.deq.bits.fn.rm

  outtagq.io.enq.bits := intagq.io.deq.bits

  // output
  val s0_result_valid = div.io.outValid_div || div.io.outValid_sqrt

  // stage output+1
  val s1_result_valid = Reg(next=s0_result_valid)
  val s1_result_out = RegEnable(div.io.out, s0_result_valid).toUInt
  val s1_result_exc = RegEnable(div.io.exceptionFlags, s0_result_valid)

  val rq = Module(new Queue(new FDivResult, nDecoupledUnitWBQueue))

  val s1_result_dp = ieee_dp(s1_result_out)
  val s1_result_sp = hardfloat.recodedFloatNToRecodedFloatM(s1_result_out, outtagq.io.deq.bits.fn.rm, 52, 12, 23, 9)
  val s1_result_hp = hardfloat.recodedFloatNToRecodedFloatM(s1_result_out, outtagq.io.deq.bits.fn.rm, 52, 12, 10, 6)

  val s1_out = Mux(outtagq.io.deq.bits.fn.fp_is(FPD), s1_result_dp,
            Mux(outtagq.io.deq.bits.fn.fp_is(FPS), expand_float_s(ieee_sp(s1_result_sp._1)),
                                                   expand_float_h(ieee_hp(s1_result_hp._1))))
  val s1_exc = Mux(outtagq.io.deq.bits.fn.fp_is(FPD), Bits(0),
            Mux(outtagq.io.deq.bits.fn.fp_is(FPS), s1_result_sp._2, s1_result_hp._2))

  rq.io.enq.valid := s1_result_valid
  outtagq.io.deq.ready := s1_result_valid

  rq.io.enq.bits.out := s1_out
  rq.io.enq.bits.exc := s1_result_exc | s1_exc | outtagq.io.deq.bits.exc

  assert(!s1_result_valid || rq.io.enq.ready, "result queue should always be ready when a result is about to enqueue")
  assert(!io.req.fire() || rq.io.enq.ready, "result queue should always be ready when a request fires")

  io.resp <> rq.io.deq
}

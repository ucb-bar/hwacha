package hwacha

import Chisel._
import freechips.rocketchip.config._
import DataGating._
import HardFloatHelper._

class FDivOperand(implicit p: Parameters) extends VXUBundle()(p) {
  val fn = new VFDUFn
  val in0 = Bits(width = SZ_D)
  val in1 = Bits(width = SZ_D)
}

class FDivResult extends Bundle {
  val out = Bits(width = SZ_D)
  val exc = Bits(width = freechips.rocketchip.tile.FPConstants.FLAGS_SZ)
}

class FDivIO(implicit p: Parameters) extends VXUBundle()(p) {
  val req = Decoupled(new FDivOperand)
  val resp = Decoupled(new FDivResult).flip
}

class FDivTag(implicit p: Parameters) extends VXUBundle()(p) {
  val fn = new VFDUFn
  val exc = Bits(width = freechips.rocketchip.tile.FPConstants.FLAGS_SZ)
}

class FDivSlice(implicit p: Parameters) extends VXUModule()(p) with Packing {
  val io = new FDivIO().flip

  val qcnt = Module(new QCounter(nDecoupledUnitWBQueue, nDecoupledUnitWBQueue))
  qcnt.suggestName("qcntInst")

  qcnt.io.dec := io.req.fire
  qcnt.io.inc := io.resp.fire

  // stage0
  val ins = List(io.req.bits.in0, io.req.bits.in1) map { in =>
    val dp = recode_dp(in)
    val sp = Module(new hardfloat.RecFNToRecFN(8, 24, 11, 53))
    sp.suggestName("spInst")
    val hp = Module(new hardfloat.RecFNToRecFN(5, 11, 11, 53))
    hp.suggestName("hpInst")
    sp.io.in := recode_sp(in)
    sp.io.roundingMode := io.req.bits.fn.rm
    hp.io.in := recode_hp(in)
    hp.io.roundingMode := io.req.bits.fn.rm
    val out = Mux(io.req.bits.fn.fp_is(FPD), dp,
              Mux(io.req.bits.fn.fp_is(FPS), sp.io.out, hp.io.out))
    val exc = Mux(io.req.bits.fn.fp_is(FPD), Bits(0),
              Mux(io.req.bits.fn.fp_is(FPS), sp.io.exceptionFlags, hp.io.exceptionFlags))
    (out, exc)
  }

  val in0q = Module(new Queue(Bits(width = 65), 2))
  in0q.suggestName("in0qInst")
  val in1q = Module(new Queue(Bits(width = 65), 2))
  in1q.suggestName("in1qInst")
  val intagq = Module(new Queue(new FDivTag, 2))
  intagq.suggestName("intagqInst")

  val s0_op_div = io.req.bits.fn.op_is(FD_DIV)

  in0q.io.enq.valid := io.req.valid
  in0q.io.enq.bits := ins(0)._1
  in1q.io.enq.valid := io.req.valid && s0_op_div
  in1q.io.enq.bits := ins(1)._1
  intagq.io.enq.valid := io.req.valid
  intagq.io.enq.bits.fn := io.req.bits.fn
  intagq.io.enq.bits.exc := ins(0)._2 | dgate(s0_op_div, ins(1)._2)

  io.req.ready :=
    !qcnt.io.empty && intagq.io.enq.ready &&
    in0q.io.enq.ready && (!s0_op_div || in1q.io.enq.ready)

  // stage1
  val div = Module(new hardfloat.DivSqrtRecF64)
  div.suggestName("divInst")
  val outtagq = Module(new Queue(new FDivTag, nDecoupledUnitWBQueue))
  outtagq.suggestName("outtagqInst")

  val s1_op_div = intagq.io.deq.bits.fn.op_is(FD_DIV)
  val mask_in1q_valid = !s1_op_div || in1q.io.deq.valid
  val mask_div_ready = s1_op_div && div.io.inReady_div || !s1_op_div && div.io.inReady_sqrt

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      in0q.io.deq.valid, mask_in1q_valid, intagq.io.deq.valid,
      mask_div_ready, outtagq.io.enq.ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  in0q.io.deq.ready := fire(in0q.io.deq.valid)
  in1q.io.deq.ready := fire(mask_in1q_valid, s1_op_div)
  intagq.io.deq.ready := fire(intagq.io.deq.valid)
  div.io.inValid := fire(mask_div_ready)
  outtagq.io.enq.valid := fire(outtagq.io.enq.ready)

  div.io.sqrtOp := !s1_op_div
  div.io.a := in0q.io.deq.bits
  div.io.b := Mux(s1_op_div, in1q.io.deq.bits, in0q.io.deq.bits)
  div.io.roundingMode := intagq.io.deq.bits.fn.rm

  outtagq.io.enq.bits := intagq.io.deq.bits

  // output
  val s0_result_valid = div.io.outValid_div || div.io.outValid_sqrt

  // stage output+1
  val s1_result_valid = Reg(next=s0_result_valid)
  val s1_result_out = RegEnable(div.io.out, s0_result_valid).asUInt
  val s1_result_exc = RegEnable(div.io.exceptionFlags, s0_result_valid)

  val rq = Module(new Queue(new FDivResult, nDecoupledUnitWBQueue))
  rq.suggestName("rqInst")

  val s1_result_dp = ieee_dp(s1_result_out)
  val s1_result_sp = Module(new hardfloat.RecFNToRecFN(11, 53, 8, 24))
  s1_result_sp.suggestName("s1_result_spInst")
  val s1_result_hp = Module(new hardfloat.RecFNToRecFN(11, 53, 5, 11))
  s1_result_hp.suggestName("s1_result_hpInst")
  s1_result_sp.io.in := s1_result_out
  s1_result_sp.io.roundingMode := outtagq.io.deq.bits.fn.rm
  s1_result_hp.io.in := s1_result_out
  s1_result_hp.io.roundingMode := outtagq.io.deq.bits.fn.rm

  val s1_out = Mux(outtagq.io.deq.bits.fn.fp_is(FPD), s1_result_dp,
               Mux(outtagq.io.deq.bits.fn.fp_is(FPS), expand_float_s(ieee_sp(s1_result_sp.io.out)),
                                                      expand_float_h(ieee_hp(s1_result_hp.io.out))))
  val s1_exc = Mux(outtagq.io.deq.bits.fn.fp_is(FPD), Bits(0),
               Mux(outtagq.io.deq.bits.fn.fp_is(FPS), s1_result_sp.io.exceptionFlags,
                                                      s1_result_hp.io.exceptionFlags))

  rq.io.enq.valid := s1_result_valid
  outtagq.io.deq.ready := s1_result_valid

  rq.io.enq.bits.out := s1_out
  rq.io.enq.bits.exc := s1_result_exc | s1_exc | outtagq.io.deq.bits.exc

  assert(!s1_result_valid || rq.io.enq.ready, "result queue should always be ready when a result is about to enqueue")
  assert(!io.req.fire || rq.io.enq.ready, "result queue should always be ready when a request fires")

  io.resp <> rq.io.deq
}

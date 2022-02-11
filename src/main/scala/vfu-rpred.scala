package hwacha

import Chisel._
import freechips.rocketchip.config._

class RPredOperand(implicit p: Parameters) extends VXUBundle()(p) {
  val active = Bits(width = nSlices)
  val pred = Bits(width = nSlices)
}

class RPredResult(implicit p: Parameters) extends VXUBundle()(p) {
  val cond = Bool()
}

class RPredIO(implicit p: Parameters) extends VXUBundle()(p) {
  val op = Valid(new VRPUFn)
  val req = Decoupled(new RPredOperand)
  val result = Decoupled(new RPredResult).flip
}

class RPredLane(implicit p: Parameters) extends VXUModule()(p) {
  val io = new RPredIO().flip

  val fn = Reg(new VRPUFn)
  val cond = Reg(Bool())

  when (io.op.valid) {
    fn := io.op.bits
    when (io.op.bits.op_is(FR_ALL)) { cond := Bool(true) }
    when (io.op.bits.op_is(FR_ANY)) { cond := Bool(false) }
  }

  io.req.ready := Bool(true)
  when (io.req.fire) {
    when (fn.op_is(FR_ALL)) { cond := cond & (io.req.bits.pred | ~io.req.bits.active).orR }
    when (fn.op_is(FR_ANY)) { cond := cond | (io.req.bits.pred & io.req.bits.active).orR }
  }

  io.result.bits.cond := cond
}

class RPredMaster(implicit p: Parameters) extends VXUModule()(p) {
  val io = new Bundle {
    val op = Decoupled(new IssueOpML).flip
    val lane = Vec(nLanes, Decoupled(new RPredResult)).flip
    val result = Decoupled(new RPredResult)
  }

  val opq = Module(new Queue(new IssueOpML, 2))
  opq.suggestName("opqInst")
  opq.io.enq <> io.op

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)
  val deq_lane = Reg(Vec(nLanes, Bool()))
  val fn = Reg(new VRPUFn)

  val mask_lane_valid = (deq_lane zip io.lane) map { case (deq, lane) => !deq || lane.valid }

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(state === s_busy, io.result.ready) ++ mask_lane_valid
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.result.valid := fire(io.result.ready)
  (io.lane zipWithIndex) map { case (lane, i) =>
    lane.ready := fire(mask_lane_valid(i), deq_lane(i)) }

  opq.io.deq.ready := Bool(false)

  switch (state) {
    is (s_idle) {
      opq.io.deq.ready := Bool(true)
      when (opq.io.deq.valid) {
        state := s_busy
        deq_lane := Vec(opq.io.deq.bits.lane.map(_.active))
        fn := opq.io.deq.bits.fn.vrpu()
      }
    }
    is (s_busy) {
      when (fire(null)) { state := s_idle }
    }
  }

  val cond =
    fn.op_is(FR_ALL) && io.lane.map(r => r.bits.cond | ~r.valid).reduce(_ && _) ||
    fn.op_is(FR_ANY) && io.lane.map(r => r.bits.cond & r.valid).reduce(_ || _)

  io.result.bits.cond := cond
}

package hwacha

import Chisel._
import cde.Parameters

class RPredOperand(implicit p: Parameters) extends VXUBundle()(p) {
  val pred = Bits(width = nSlices)
  val active = Bits(width = nSlices)
}

class RPredResult(implicit p: Parameters) extends VXUBundle()(p) {
  val cond = Bool()
}

class RPredIO(implicit p: Parameters) extends VXUBundle()(p) {
  val op = Valid(new VRPUFn)
  val req = Decoupled(new RPredOperand)
  val consumed = Bool(INPUT)
  val result = Valid(new RPredResult).flip
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
  when (io.req.fire()) {
    when (fn.op_is(FR_ALL)) { cond := cond & (io.req.bits.pred | ~io.req.bits.active).orR }
    when (fn.op_is(FR_ANY)) { cond := cond | (io.req.bits.pred & io.req.bits.active).orR }
  }

  io.consumed := io.req.fire()
  // io.result.valid gets populated by LaneSequencer
  io.result.bits.cond := cond
}

class RPredMaster(implicit p: Parameters) extends VXUModule()(p) {
  val io = new Bundle {
    val op = Valid(new VRPUFn).flip
    val lane = Vec.fill(nLanes){Valid(new RPredResult)}.flip
    val result = Valid(new RPredResult)
  }

  val cond =
    io.op.bits.op_is(FR_ALL) && io.lane.map(r => r.bits.cond | ~r.valid).reduce(_ && _) ||
    io.op.bits.op_is(FR_ANY) && io.lane.map(r => r.bits.cond & r.valid).reduce(_ || _)

  // register result to cut combinational path
  io.result.valid := Reg(next=io.op.valid)
  io.result.bits.cond := RegEnable(cond, io.op.valid)
}

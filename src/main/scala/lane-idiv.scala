package hwacha

import Chisel._
import Node._
import Constants._

case object FastMulDiv extends Field[Boolean]

object RocketConstants extends rocket.constants.ScalarOpConstants

class LaneIDivIO extends Bundle
{
  val req = Decoupled(new Bundle {
    val fn = new VIDUFn
    val in0 = Bits(width = SZ_D)
    val in1 = Bits(width = SZ_D)
  })
  val resp = Decoupled(new LaneIDivResult).flip
}

class LaneIDivResult extends Bundle
{
  val out = Bits(width = SZ_D)
}

class LaneIDivSlice extends HwachaModule with LaneParameters
{
  val io = new LaneIDivIO().flip

  val qcnt = Module(new QCounter(nDecoupledUnitWBQueue, nDecoupledUnitWBQueue))

  qcnt.io.dec := io.req.fire()
  qcnt.io.inc := io.resp.fire()

  val div = Module(new rocket.MulDiv(mulUnroll = 8, earlyOut = true))

  div.io.req.valid := io.req.valid
  io.req.ready := !qcnt.io.empty && div.io.req.ready
  div.io.req.bits.dw :=
    Mux(io.req.bits.fn.dw_is(DW32), RocketConstants.DW_32,
                                    RocketConstants.DW_64)
  div.io.req.bits.fn :=
    Mux(io.req.bits.fn.op_is(ID_DIV),  rocket.ALU.FN_DIV,
    Mux(io.req.bits.fn.op_is(ID_DIVU), rocket.ALU.FN_DIVU,
    Mux(io.req.bits.fn.op_is(ID_REM),  rocket.ALU.FN_REM,
                                       rocket.ALU.FN_REMU)))
  div.io.req.bits.in1 := io.req.bits.in0
  div.io.req.bits.in2 := io.req.bits.in1
  div.io.kill := Bool(false)

  val rq = Module(new Queue(new LaneIDivResult, nDecoupledUnitWBQueue))

  rq.io.enq.valid := div.io.resp.valid
  rq.io.enq.bits.out := div.io.resp.bits.data
  div.io.resp.ready := rq.io.enq.ready

  assert(!div.io.resp.valid || rq.io.enq.ready, "result queue should always be ready when a result is about to enqueue")
  assert(!io.req.fire() || rq.io.enq.ready, "result queue should always be ready when a request fires")

  io.resp <> rq.io.deq
}

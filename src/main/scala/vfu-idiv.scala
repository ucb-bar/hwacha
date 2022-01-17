package hwacha

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._

case object FastMulDiv extends Field[Boolean]

object RocketConstants extends freechips.rocketchip.rocket.constants.ScalarOpConstants

class IDivOperand(implicit p: Parameters) extends VXUBundle()(p) {
  val fn = new VIDUFn
  val in0 = Bits(width = SZ_D)
  val in1 = Bits(width = SZ_D)
}

class IDivResult extends Bundle {
  val out = Bits(width = SZ_D)
}

class IDivIO(implicit p: Parameters) extends VXUBundle()(p) {
  val req = Decoupled(new IDivOperand)
  val resp = Decoupled(new IDivResult).flip
}

class IDivSlice(implicit p: Parameters) extends VXUModule()(p) {
  val io = new IDivIO().flip

  implicit def BitPatToUInt(x: BitPat): UInt = {
    require(x.mask == (BigInt(1) << x.getWidth)-1)
    UInt(x.value, x.getWidth)
  }

  val qcnt = Module(new QCounter(nDecoupledUnitWBQueue, nDecoupledUnitWBQueue))
  qcnt.suggestName("qcntInst")

  qcnt.io.dec := io.req.fire
  qcnt.io.inc := io.resp.fire

  val div = Module(new MulDiv(cfg = MulDivParams(mulUnroll = 8, mulEarlyOut = true, divEarlyOut = true), width = p(HwachaRegLen)))
  div.suggestName("divInst")

  div.io.req.valid := io.req.valid
  io.req.ready := !qcnt.io.empty && div.io.req.ready
  div.io.req.bits.dw :=
    Mux(io.req.bits.fn.dw_is(DW32), RocketConstants.DW_32,
                                    RocketConstants.DW_64)
  div.io.req.bits.fn :=
    Mux(io.req.bits.fn.op_is(ID_DIV),  ALU.FN_DIV,
    Mux(io.req.bits.fn.op_is(ID_DIVU), ALU.FN_DIVU,
    Mux(io.req.bits.fn.op_is(ID_REM),  ALU.FN_REM,
                                       ALU.FN_REMU)))
  div.io.req.bits.in1 := io.req.bits.in0
  div.io.req.bits.in2 := io.req.bits.in1
  div.io.kill := Bool(false)

  val rq = Module(new Queue(new IDivResult, nDecoupledUnitWBQueue))
  rq.suggestName("rqInst")

  rq.io.enq.valid := div.io.resp.valid
  rq.io.enq.bits.out := div.io.resp.bits.data
  div.io.resp.ready := rq.io.enq.ready

  assert(!div.io.resp.valid || rq.io.enq.ready, "result queue should always be ready when a result is about to enqueue")
  assert(!io.req.fire || rq.io.enq.ready, "result queue should always be ready when a request fires")

  io.resp <> rq.io.deq
}

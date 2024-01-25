package hwacha

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._

case object FastMulDiv extends Field[Boolean]

object RocketConstants extends freechips.rocketchip.rocket.constants.ScalarOpConstants

class IDivOperand(implicit p: Parameters) extends VXUBundle()(p) {
  val fn = new VIDUFn
  val in0 = UInt(SZ_D.W)
  val in1 = UInt(SZ_D.W)
}

class IDivResult extends Bundle {
  val out = UInt(SZ_D.W)
}

class IDivIO(implicit p: Parameters) extends VXUBundle()(p) {
  val req = Decoupled(new IDivOperand)
  val resp = Flipped(Decoupled(new IDivResult))
}

class IDivSlice(implicit p: Parameters) extends VXUModule()(p) {
  val io = IO(Flipped(new IDivIO()))

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
    Mux(io.req.bits.fn.op_is(ID_DIV),  aluFn.FN_DIV,
    Mux(io.req.bits.fn.op_is(ID_DIVU), aluFn.FN_DIVU,
    Mux(io.req.bits.fn.op_is(ID_REM),  aluFn.FN_REM,
                                       aluFn.FN_REMU)))
  div.io.req.bits.in1 := io.req.bits.in0
  div.io.req.bits.in2 := io.req.bits.in1
  div.io.kill := false.B

  val rq = Module(new Queue(new IDivResult, nDecoupledUnitWBQueue))
  rq.suggestName("rqInst")

  rq.io.enq.valid := div.io.resp.valid
  rq.io.enq.bits.out := div.io.resp.bits.data
  div.io.resp.ready := rq.io.enq.ready

  assert(!div.io.resp.valid || rq.io.enq.ready, "result queue should always be ready when a result is about to enqueue")
  assert(!io.req.fire || rq.io.enq.ready, "result queue should always be ready when a request fires")

  io.resp <> rq.io.deq
}

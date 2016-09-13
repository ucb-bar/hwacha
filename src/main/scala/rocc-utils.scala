package hwacha

import Chisel._
import rocket._
import cde.{Parameters, Field}
import uncore.util._
import rocketchip.LatencyPipe

class RoccBusyDecoupler(commands: Seq[BitPat], counterSz: Int)(implicit p: Parameters) extends RoCC()(p) {
def connect(r: RoCCInterface, delay: Int): Unit = {
    val ro = this.io.roccOut

    r.cmd <> LatencyPipe(ro.cmd, delay)
    ro.resp <> LatencyPipe(r.resp, delay)
    ro.mem <> ShiftRegister(r.mem, delay)
    ro.busy <> ShiftRegister(r.busy, delay)
    ro.interrupt <> ShiftRegister(r.interrupt, delay)
    ro.autl <> ClientUncachedTileLinkEnqueuer(r.autl, UncachedTileLinkDepths(1,1))(p)
    ro.utl zip r.utl map {
      case (out, in) =>
        out <> ClientUncachedTileLinkEnqueuer(in, UncachedTileLinkDepths(1,1))(p)
    }
    ro.ptw zip r.ptw map {
      case (out, in) => {
        out.req <> LatencyPipe(in.req, delay)
        in.resp <> ShiftRegister(out.resp, delay) // resp is flipped
        in.ptbr <> ShiftRegister(out.ptbr, delay)
        in.invalidate <> ShiftRegister(out.invalidate, delay)
        in.status <> ShiftRegister(out.status, delay)
      }
    }
    ro.fpu_req <> LatencyPipe(r.fpu_req, delay)
    r.fpu_resp <> LatencyPipe(ro.fpu_resp, delay)
    r.exception <> ShiftRegister(ro.exception, delay)
  }
  override val io = new RoCCInterface {
    val roccOut = new RoCCInterface().flip

    val twoPhase = Bool(OUTPUT)
    val delayTwoPhase = Bool(INPUT)
  }
  io.mem.req.bits.phys := Bool(true) // don't perform address translation
  io.mem.invalidate_lr := Bool(false) // don't mess with LR/SC
  io <> io.roccOut

  val count = Reg(init = UInt(value = 0, width = counterSz))

  val reg_twoPhase = Reg(init = Bool(true))
  io.twoPhase := reg_twoPhase

  val reg_delayTwoPhase = RegNext(io.delayTwoPhase)
  val inc = io.cmd.valid && commands.map(b => b === io.cmd.bits.inst.asUInt).reduce(_||_)
  val dec = io.delayTwoPhase =/= reg_delayTwoPhase
  when(inc) {
    io.twoPhase := !reg_twoPhase
    reg_twoPhase := !reg_twoPhase
  }
  when (inc ^ dec) {
    when (inc) {
      count := count + UInt(1)
      assert(count < UInt(1 << counterSz), "RoCCDecoupler counter overflow")
    }
    when (dec) {
      count := count - UInt(1)
      assert(count >= UInt(1), "RoCCDecoupler counter underflow")
    }
  }

  io.busy := Mux(count > UInt(0) || inc, Bool(true), io.roccOut.busy)
}

package hwacha

import Chisel._
import rocket._
import cde.{Parameters, Field}
import uncore.util._
import util.LatencyPipe

object Queueify {
  def apply[T <: Data](ri: Bundle, ro: Bundle, delay: Int): Unit = {
    ri.elements.zip(ro.elements).foreach {
      _ match {
        case ((pName, dIn: DecoupledIO[_]), (pNameOut, dOut:DecoupledIO[_])) =>
          if(dIn.ready.dir == OUTPUT) dIn <> LatencyPipe(dOut, delay)
          else if(dIn.ready.dir == INPUT) dOut <> LatencyPipe(dIn, delay)
        case ((pName, vIn: Vec[_]), (_, vOut:Vec[_])) =>
          vIn.zip(vOut).map {
            case(in:Bundle, out:Bundle) => Queueify(in, out, delay)
            case _ =>
          }
        case ((_, bIn: Bundle), (_, bOut: Bundle)) => Queueify(bIn, bOut, delay)
        case ((_, wIn), (_, wOut)) =>
          if(wIn.dir == OUTPUT) wOut <> ShiftRegister(wIn, delay)
          else if(wIn.dir == INPUT) wIn <> ShiftRegister(wOut, delay)
      }
    }
  }
}

class RoccBusyDecoupler(commands: Seq[BitPat], counterSz: Int)(implicit p: Parameters) extends RoCC()(p) {
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

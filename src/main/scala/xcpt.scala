package hwacha

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.rocket.Causes

class XCPTIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val raise = Bool(OUTPUT)
  val hold = Bool(OUTPUT)
  val replay = Bool(OUTPUT)
}

abstract class IRQ(implicit p: Parameters) extends HwachaBundle()(p) {
  val tval = UInt(width = vaddrBitsExtended)

  protected def _check(x: (Bool, UInt)*): (Bool, UInt) = {
    (x.map(_._1).reduce(_ || _), PriorityMux(x))
  }
  def check(): (Bool, UInt)
}

class DTLBExceptions extends Bundle {
  val ld = Bool()
  val st = Bool()
}

class ITLBExceptions extends Bundle {
  val inst = Bool()
}

class IRQIssue(implicit p: Parameters) extends IRQ()(p) {
  val epc = UInt(width = vaddrBitsExtended)
  val illegal = new Bundle {
    val inst = Bool()
    val vreg = Bool()
  }
  val ma = new ITLBExceptions
  val pf = new ITLBExceptions
  val ae = new ITLBExceptions

  def check(): (Bool, UInt) = _check(
    this.ma.inst -> Causes.misaligned_fetch.U,
    this.pf.inst -> Causes.fetch_page_fault.U,
    this.ae.inst -> Causes.fetch_access.U,
    this.illegal.inst -> Causes.illegal_instruction.U,
    this.illegal.vreg -> Causes.illegal_instruction.U)
}

class XCPTIssueIO(implicit p: Parameters) extends XCPTIO()(p) {
  val irq = new IRQIssue().asInput
}

class IRQMem(implicit p: Parameters) extends IRQ()(p) {
  val ma = new DTLBExceptions
  val pf = new DTLBExceptions
  val ae = new DTLBExceptions

  def check(): (Bool, UInt) = _check(
    this.ma.st -> Causes.misaligned_store.U,
    this.ma.ld -> Causes.misaligned_load.U,
    this.pf.st -> Causes.store_page_fault.U,
    this.pf.ld -> Causes.load_page_fault.U,
    this.ae.st -> Causes.store_access.U,
    this.ae.ld -> Causes.load_access.U)
}

class XCPTMemIO(implicit p: Parameters) extends XCPTIO()(p) {
  val irq = new IRQMem().asInput
}

class XCPTStatus(implicit p: Parameters) extends HwachaBundle()(p) {
  val cause = UInt(width = 5)
  val tval = UInt(width = vaddrBitsExtended)
  val epc = UInt(width = vaddrBitsExtended)
}

class XCPTRoCCIO(implicit p: Parameters) extends XCPTIO()(p) {
  val status = new XCPTStatus().asOutput
  val cmd = new Bundle {
    val ret = Bool(INPUT)
  }
}

class ExceptionUnit(implicit p: Parameters) extends HwachaModule()(p) {
  val io = new Bundle {
    val rocc = new XCPTRoCCIO
    val issue = new XCPTIssueIO
    val vmu = Vec(nLanes, new XCPTMemIO)
    val smu = new XCPTMemIO
  }

  val status = RegInit(0.U.asTypeOf(io.rocc.status))

  val irqs = io.issue.irq +: io.vmu.map(_.irq) :+ io.smu.irq
  val irqs_cause = irqs.map(_.check())
  val irqs_sel = irqs_cause.map(_._1)
  val raise = irqs_sel.reduce(_ || _)
  val hold = RegInit(Bool(false))
  val replay = RegNext(io.rocc.cmd.ret, Bool(false))

  when (raise) {
    hold := Bool(true)
    status.tval := PriorityMux(irqs_sel, irqs.map(_.tval))
    status.cause := PriorityMux(irqs_cause)
    status.epc := io.issue.irq.epc
  }
  io.rocc.status := status

  when (io.rocc.cmd.ret) {
    hold := Bool(false)
  }

  (Seq(io.rocc, io.issue, io.smu) ++ io.vmu).foreach { x =>
    x.raise := raise
    x.hold := hold
    x.replay := replay
  }
}

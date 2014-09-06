package hwacha

import Chisel._
import Node._
import Constants._

class IssueOpIO extends ValidIO(new IssueOp)

class Issue(resetSignal: Bool = null) extends HwachaModule(_reset = resetSignal)
{
  val io = new Bundle {
    val cfg = new HwachaConfigIO
    val irq = new IRQIO
    val xcpt = new XCPTIO().flip

    val vcmdq = new VCMDQIO().flip
    val imem = new rocket.CPUFrontendIO
    val deckop = new DeckOpIO
    val vmu = new VMUIO

    val tvec = new Bundle {
      val active = Bool(OUTPUT)
      val ready = Bool(INPUT)
      val op = new IssueOpIO
    }

    val vt = new Bundle {
      val ready = Bool(INPUT)
      val op = new IssueOpIO
    }
    
    val pending_vf = Bool(OUTPUT)

    val aiw = new AIWVXUIO
  }

  val tvec = Module(new IssueTVEC)
  val vt = Module(new IssueVT)

  def arb_producers[T<:Data](issue: DecoupledIO[T], sel: Bool, tvec: DecoupledIO[T], vt: DecoupledIO[T]) = {
    tvec.ready := issue.ready
    vt.ready := issue.ready
    issue.valid := Mux(sel, tvec.valid, vt.valid)
    issue.bits := Mux(sel, tvec.bits, vt.bits)
  }

  io.cfg <> tvec.io.cfg
  vt.io.cfg <> tvec.io.cfg
  io.irq <> vt.io.irq
  vt.io.vf <> tvec.io.vf
  io.pending_vf := tvec.io.vf.active

  // vcmdq
  io.vcmdq.cnt.ready := tvec.io.vcmdq.cnt.ready || vt.io.vcmdq.cnt.ready
  tvec.io.vcmdq.cmd <> io.vcmdq.cmd
  tvec.io.vcmdq.imm1 <> io.vcmdq.imm1
  tvec.io.vcmdq.imm2 <> io.vcmdq.imm2
  tvec.io.vcmdq.cnt.valid := io.vcmdq.cnt.valid
  tvec.io.vcmdq.cnt.bits := io.vcmdq.cnt.bits
  vt.io.vcmdq.cnt.valid := io.vcmdq.cnt.valid
  vt.io.vcmdq.cnt.bits := io.vcmdq.cnt.bits

  // imem
  vt.io.imem <> io.imem

  // issue op
  io.tvec.active := tvec.io.active
  io.tvec.op <> tvec.io.op
  tvec.io.ready <> io.tvec.ready
  io.vt.op <> vt.io.op
  vt.io.ready <> io.vt.ready

  // aiw
  io.aiw.issue.enq.cmdb <> tvec.io.aiw.issue.enq.cmdb
  io.aiw.issue.enq.imm1b <> tvec.io.aiw.issue.enq.imm1b
  io.aiw.issue.enq.imm2b <> tvec.io.aiw.issue.enq.imm2b
  arb_producers(io.aiw.issue.enq.cntb, tvec.io.active, tvec.io.aiw.issue.enq.cntb, vt.io.aiw.issue.enq.cntb)
  io.aiw.issue.enq.numcntb <> tvec.io.aiw.issue.enq.numcntb

  tvec.io.aiw.issue.rtag <> io.aiw.issue.rtag
  vt.io.aiw.issue.rtag <> io.aiw.issue.rtag

  io.aiw.issue.marklast := Mux(tvec.io.active, tvec.io.aiw.issue.marklast, vt.io.aiw.issue.marklast)
  io.aiw.issue.update <> vt.io.aiw.issue.update

  // vmu
  arb_producers(io.deckop, tvec.io.active, tvec.io.deckop, vt.io.deckop)
  arb_producers(io.vmu.issue.cmdq, tvec.io.active, tvec.io.vmu.issue.cmdq, vt.io.vmu.issue.cmdq)
  io.vmu.issue.addrq <> tvec.io.vmu.issue.addrq

  // xcpt
  tvec.io.xcpt <> io.xcpt
  vt.io.xcpt <> io.xcpt
}

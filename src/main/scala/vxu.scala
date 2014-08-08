package hwacha

import Chisel._
import Node._
import Constants._

class VXU(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val irq = new IRQIO
    val xcpt = new XCPTIO().flip

    val vcmdq = new VCMDQIO().flip
    val imem = new rocket.CPUFrontendIO
    val vmu = new VMUIO

    val lreq = new LookAheadPortIO(log2Down(conf.nvlreq)+1)
    val sreq = new LookAheadPortIO(log2Down(conf.nvsreq)+1)
    val lret = new MRTLoadRetireIO
    
    val pending_vf = Bool(OUTPUT)
    val pending_seq = Bool(OUTPUT)
    val pending_memop = Bool(OUTPUT)

    val aiw = new AIWVXUIO
  }

  val flush = this.reset || io.xcpt.prop.vu.flush_vxu

  val issue = Module(new Issue(resetSignal = flush))
  val hazard = Module(new Hazard(resetSignal = flush))
  val seq = Module(new Sequencer(resetSignal = flush))
  val exp = Module(new Expander)
  val lane = Module(new Lane)
  val deck = Module(new Deck(resetSignal = flush))

  io.irq <> issue.io.irq

  issue.io.xcpt <> io.xcpt
  issue.io.vcmdq <> io.vcmdq
  issue.io.imem <> io.imem

  hazard.io.cfg <> issue.io.cfg
  hazard.io.update <> seq.io.hazard
  hazard.io.update <> exp.io.hazard
  hazard.io.tvec <> issue.io.tvec
  hazard.io.vt <> issue.io.vt

  seq.io.cfg <> issue.io.cfg
  seq.io.xcpt <> io.xcpt
  seq.io.issueop <> hazard.io.issueop

  exp.io.xcpt <> io.xcpt
  exp.io.seqop <> seq.io.seqop

  lane.io.op <> exp.io.laneop
  lane.io.bwqs <> deck.io.bwqs

  deck.io.cfg <> issue.io.cfg
  deck.io.op <> issue.io.deckop
  deck.io.lla <> seq.io.lla
  deck.io.sla <> seq.io.sla
  deck.io.brqs <> lane.io.brqs

  io.vmu <> issue.io.vmu
  io.vmu <> seq.io.vmu
  io.vmu <> lane.io.vmu
  io.vmu <> deck.io.vmu
  io.lreq <> seq.io.lreq
  io.sreq <> seq.io.sreq
  io.lret <> seq.io.lret

  io.aiw <> issue.io.aiw
  io.aiw <> seq.io.aiw

  io.pending_vf := issue.io.pending_vf
  io.pending_seq := seq.io.busy
  io.pending_memop := hazard.io.pending_memop
}

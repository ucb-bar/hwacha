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
    val imem = new rocket.CPUFrontendIO()(conf.vicache)
    val vmu = new vmunit.VMUIO

    val lreq = new LookAheadPortIO(log2Down(conf.nvlreq)+1)
    val sreq = new LookAheadPortIO(log2Down(conf.nvsreq)+1)
    val lret = new MRTLoadRetireIO
    
    val pending_memop = Bool(OUTPUT)
    val pending_vf = Bool(OUTPUT)

    val aiw_cmdb = new io_vxu_cmdq()
    val aiw_imm1b = new io_vxu_immq()
    val aiw_imm2b = new io_vxu_imm2q()
    val aiw_cntb = new io_vxu_cntq()
    val aiw_numCntB = new io_vxu_numcntq()

    val issue_to_aiw = new io_issue_to_aiw()
    val aiw_to_issue = new io_aiw_to_issue().flip()

    val aiwop = new AIWOpIO
  }

  val flush = this.reset || io.xcpt.prop.vu.flush_vxu

  val issue = Module(new Issue(resetSignal = flush))
  val hazard = Module(new Hazard(resetSignal = flush))
  val seq = Module(new Sequencer(resetSignal = flush))
  val exp = Module(new Expander)
  val lane = Module(new Lane)
  val deck = Module(new Deck)

  io.irq <> issue.io.irq

  issue.io.xcpt <> io.xcpt
  issue.io.vcmdq <> io.vcmdq
  issue.io.imem <> io.imem
  issue.io.aiw_cmdb <> io.aiw_cmdb
  issue.io.aiw_imm1b <> io.aiw_imm1b
  issue.io.aiw_imm2b <> io.aiw_imm2b
  issue.io.aiw_cntb <> io.aiw_cntb
  issue.io.aiw_numCntB <> io.aiw_numCntB
  issue.io.issue_to_aiw <> io.issue_to_aiw
  issue.io.aiw_to_issue <> io.aiw_to_issue

  hazard.io.cfg <> issue.io.cfg
  hazard.io.seq_to_hazard <> seq.io.seq_to_hazard
  hazard.io.expand_to_hazard <> exp.io.expand_to_hazard
  hazard.io.tvec <> issue.io.tvec
  hazard.io.vt <> issue.io.vt

  seq.io.cfg <> issue.io.cfg
  seq.io.xcpt <> io.xcpt
  seq.io.issueop <> hazard.io.issueop
  seq.io.aiwop <> io.aiwop

  exp.io.xcpt <> io.xcpt
  exp.io.seqop <> seq.io.seqop

  lane.io.cfg <> issue.io.cfg
  lane.io.op <> exp.io.laneop
  lane.io.bwqs <> deck.io.bwqs

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

  io.pending_memop := hazard.io.pending_memop
  io.pending_vf := issue.io.pending_vf
}

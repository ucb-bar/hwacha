package hwacha

import Chisel._
import Node._
import Constants._

class io_vxu_to_xcpt_handler extends Bundle
{
  val expand = new io_expand_to_xcpt_handler()
}

class VXU(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val cfg = new HwachaConfigIO

    val irq = new io_issue_to_irq_handler()
    val vcmdq = new VCMDQIO().flip
    val imem = new rocket.CPUFrontendIO()(conf.vicache)

    val vmu = new VMUIO
    val qcntp1 = UInt(OUTPUT, SZ_QCNT)
    val qcntp2 = UInt(OUTPUT, SZ_QCNT)
    
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

    val xcpt_to_vxu = new io_xcpt_handler_to_vxu().flip()
    val vxu_to_xcpt = new io_vxu_to_xcpt_handler()
  }

  val flush = this.reset || io.xcpt_to_vxu.flush

  val issue = Module(new Issue(resetSignal = flush))
  val hazard = Module(new Hazard(resetSignal = flush))
  val seq = Module(new Sequencer(resetSignal = flush))
  val exp = Module(new Expander)
  val lane = Module(new Lane)

  io.cfg <> issue.io.cfg
  io.irq <> issue.io.irq

  issue.io.vcmdq <> io.vcmdq
  issue.io.imem <> io.imem
  issue.io.aiw_cmdb <> io.aiw_cmdb
  issue.io.aiw_imm1b <> io.aiw_imm1b
  issue.io.aiw_imm2b <> io.aiw_imm2b
  issue.io.aiw_cntb <> io.aiw_cntb
  issue.io.aiw_numCntB <> io.aiw_numCntB
  issue.io.issue_to_aiw <> io.issue_to_aiw
  issue.io.aiw_to_issue <> io.aiw_to_issue
  issue.io.xcpt_to_issue <> io.xcpt_to_vxu.issue

  hazard.io.cfg <> issue.io.cfg
  hazard.io.seq_to_hazard <> seq.io.seq_to_hazard
  hazard.io.expand_to_hazard <> exp.io.expand_to_hazard
  hazard.io.lane_to_hazard <> lane.io.lane_to_hazard
  hazard.io.tvec <> issue.io.tvec
  hazard.io.vt <> issue.io.vt

  seq.io.cfg <> issue.io.cfg
  seq.io.qstall.vaq := !io.vmu.vaq.ready
  seq.io.qstall.vldq := !io.vmu.vldq.valid
  seq.io.qstall.vsdq := !io.vmu.vsdq.ready
  seq.io.issueop <> hazard.io.issueop
  seq.io.aiwop <> io.aiwop
  seq.io.xcpt_to_seq <> io.xcpt_to_vxu.seq

  exp.io.seqop <> seq.io.seqop
  exp.io.expand_to_xcpt <> io.vxu_to_xcpt.expand

  lane.io.cfg <> issue.io.cfg
  lane.io.op <> exp.io.laneop
  lane.io.vmu <> io.vmu

  io.qcntp1 <> seq.io.qcntp1
  io.qcntp2 <> seq.io.qcntp2

  io.pending_memop := hazard.io.pending_memop
  io.pending_vf := issue.io.pending_vf
}

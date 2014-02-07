package hwacha

import Chisel._
import Node._
import Constants._

class io_vxu_to_xcpt_handler extends Bundle
{
  val expand = new io_expand_to_xcpt_handler()
}

class io_vxu_to_vmu extends Bundle
{
  val vaq_valid = Bool(OUTPUT)
  val vaq_cmd = Bits(OUTPUT, 4)
  val vsdq_valid = Bool(OUTPUT)
}

class VXU(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val irq = new io_issue_to_irq_handler()

    val vcmdq = new VCMDQIO().flip

    val imem = new rocket.CPUFrontendIO()(conf.vicache)

    val vaq = new io_vvaq()
    val vldq = new io_vldq().flip
    val vsdq = new io_vsdq()

    val vxu_to_vmu = new io_vxu_to_vmu()

    val early_vaq_valid = Bool(OUTPUT)
    val early_vaq_cmd = Bits(OUTPUT, 4)
    val early_vsdq_valid = Bool(OUTPUT)

    val qcntp1 = UInt(OUTPUT, SZ_QCNT)
    val qcntp2 = UInt(OUTPUT, SZ_QCNT)
    
    val pending_store = Bool(INPUT)
    val pending_memop = Bool(OUTPUT)
    val pending_vf = Bool(OUTPUT)

    val aiw_cmdb = new io_vxu_cmdq()
    val aiw_imm1b = new io_vxu_immq()
    val aiw_imm2b = new io_vxu_imm2q()
    val aiw_cntb = new io_vxu_cntq()
    val aiw_numCntB = new io_vxu_numcntq()

    val issue_to_aiw = new io_issue_to_aiw()
    val aiw_to_issue = new io_aiw_to_issue().flip()

    val seq_to_aiw = new io_seq_to_aiw()

    val xcpt_to_vxu = new io_xcpt_handler_to_vxu().flip()
    val vxu_to_xcpt = new io_vxu_to_xcpt_handler()

    val prec = Bits(OUTPUT, SZ_PREC)
  }

  val flush = this.reset || io.xcpt_to_vxu.flush
  val issue = Module(new Issue(resetSignal = flush))

  io.irq := issue.io.irq

  io.prec := issue.io.prec

  issue.io.imem <> io.imem

  issue.io.vcmdq <> io.vcmdq
  issue.io.pending_store <> io.pending_store
  
  issue.io.aiw_cmdb <> io.aiw_cmdb
  issue.io.aiw_imm1b <> io.aiw_imm1b
  issue.io.aiw_imm2b <> io.aiw_imm2b
  issue.io.aiw_cntb <> io.aiw_cntb
  issue.io.aiw_numCntB <> io.aiw_numCntB

  issue.io.issue_to_aiw <> io.issue_to_aiw
  issue.io.aiw_to_issue <> io.aiw_to_issue

  issue.io.xcpt_to_issue <> io.xcpt_to_vxu.issue


  val b8fire = Module(new Fire)

  b8fire.io.tvec_valid <> issue.io.tvec_valid
  b8fire.io.tvec_dhazard <> issue.io.tvec_dhazard
  b8fire.io.tvec_shazard <> issue.io.tvec_shazard
  b8fire.io.tvec_bhazard <> issue.io.tvec_bhazard
  b8fire.io.tvec_fn <> issue.io.tvec_fn
  b8fire.io.tvec_regid_imm <> issue.io.tvec_regid_imm

  b8fire.io.vt_valid <> issue.io.vt_valid
  b8fire.io.vt_dhazard <> issue.io.vt_dhazard
  b8fire.io.vt_shazard <> issue.io.vt_shazard
  b8fire.io.vt_bhazard <> issue.io.vt_bhazard
  b8fire.io.vt_fn <> issue.io.vt_fn
  b8fire.io.vt_regid_imm <> issue.io.vt_regid_imm


  val b8hazard = Module(new Hazard(resetSignal = flush))

  b8hazard.io.issue_to_hazard <> issue.io.issue_to_hazard
  b8hazard.io.hazard_to_issue <> issue.io.hazard_to_issue
 
  b8hazard.io.tvec_valid <> issue.io.tvec_valid
  b8hazard.io.tvec_ready <> issue.io.tvec_ready
  b8hazard.io.tvec_ready <> b8fire.io.tvec_ready
  b8hazard.io.tvec_dhazard <> issue.io.tvec_dhazard
  b8hazard.io.tvec_shazard <> issue.io.tvec_shazard
  b8hazard.io.tvec_bhazard <> issue.io.tvec_bhazard
  b8hazard.io.tvec_fn <> issue.io.tvec_fn
  b8hazard.io.tvec_regid_imm <> issue.io.tvec_regid_imm

  b8hazard.io.vt_valid <> issue.io.vt_valid
  b8hazard.io.vt_ready <> issue.io.vt_ready
  b8hazard.io.vt_ready <> b8fire.io.vt_ready
  b8hazard.io.vt_dhazard <> issue.io.vt_dhazard
  b8hazard.io.vt_shazard <> issue.io.vt_shazard
  b8hazard.io.vt_bhazard <> issue.io.vt_bhazard
  b8hazard.io.vt_fn <> issue.io.vt_fn
  b8hazard.io.vt_regid_imm <> issue.io.vt_regid_imm

  b8hazard.io.fire <> b8fire.io.fire
  b8hazard.io.fire_fn <> b8fire.io.fire_fn
  b8hazard.io.fire_regid_imm <> b8fire.io.fire_regid_imm

  io.pending_memop := b8hazard.io.hazard_to_issue.tvec.pending_memop
  io.pending_vf := issue.io.pending_vf


  val b8seq = Module(new Sequencer(resetSignal = flush))

  b8seq.io.issue_to_seq <> issue.io.issue_to_seq
  b8seq.io.seq_to_hazard <> b8hazard.io.seq_to_hazard

  b8seq.io.qstall.vaq := ~io.vaq.ready
  b8seq.io.qstall.vldq := ~io.vldq.valid
  b8seq.io.qstall.vsdq := ~io.vsdq.ready

  b8seq.io.fire <> b8fire.io.fire
  b8seq.io.fire_fn <> b8fire.io.fire_fn
  b8seq.io.fire_regid_imm <> b8fire.io.fire_regid_imm

  b8seq.io.seq_to_aiw <> io.seq_to_aiw

  b8seq.io.xcpt_to_seq <> io.xcpt_to_vxu.seq

  b8seq.io.prec := issue.io.prec


  val b8expand = Module(new Expander)

  b8expand.io.seq_to_expand <> b8seq.io.seq_to_expand
  b8expand.io.expand_to_hazard <> b8hazard.io.expand_to_hazard

  b8expand.io.seq <> b8seq.io.seq
  b8expand.io.seq_fn <> b8seq.io.seq_fn
  b8expand.io.seq_regid_imm <> b8seq.io.seq_regid_imm

  b8expand.io.expand_to_xcpt <> io.vxu_to_xcpt.expand


  val b8lane = Module(new Lane)

  b8lane.io.issue_to_lane <> issue.io.issue_to_lane

  b8lane.io.uop <> b8expand.io.laneuop

  b8lane.io.lane_to_hazard <> b8hazard.io.lane_to_hazard

  b8lane.io.vmu.vldq_rdy <> io.vldq.ready
  b8lane.io.vmu.vldq_bits <> io.vldq.bits

  b8lane.io.prec := issue.io.prec


  val b8mem = Module(new LaneMem)

  b8mem.io.lane_vaq_valid := b8lane.io.vmu.vaq_val
  b8mem.io.lane_vaq_check <> b8lane.io.vmu.vaq_check
  b8mem.io.lane_vaq_mem <> b8lane.io.vmu.vaq_mem
  b8mem.io.lane_vaq_imm := b8lane.io.vmu.vaq_imm
  b8mem.io.lane_vaq_utmemop := b8lane.io.vmu.vaq_utmemop
  b8mem.io.lane_vaq_rf := b8lane.io.vmu.vaq_rf

  b8mem.io.lane_vsdq_valid := b8lane.io.vmu.vsdq_val 
  b8mem.io.lane_vsdq_mem <> b8lane.io.vmu.vsdq_mem
  b8mem.io.lane_vsdq_bits := b8lane.io.vmu.vsdq_bits

  io.vaq.valid := b8mem.io.vmu_vaq_valid
  io.vaq.bits <> b8mem.io.vmu_vaq_bits

  io.vsdq.valid := b8mem.io.vmu_vsdq_valid
  io.vsdq.bits := b8mem.io.vmu_vsdq_bits

  io.vxu_to_vmu.vaq_valid := b8lane.io.vmu.vaq_val
  io.vxu_to_vmu.vaq_cmd := b8lane.io.vmu.vaq_mem.cmd
  io.vxu_to_vmu.vsdq_valid := b8lane.io.vmu.vsdq_val
  
  io.qcntp1 := b8seq.io.qcntp1
  io.qcntp2 := b8seq.io.qcntp2
}

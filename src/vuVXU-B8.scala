package hwacha

import Chisel._
import Node._
import Constants._

class io_vxu_to_xcpt_handler extends Bundle
{
  val seq = new io_seq_to_xcpt_handler()
  val expand = new io_expand_to_xcpt_handler()
}

class io_vxu extends Bundle
{
  val illegal = Bool(OUTPUT)

  val vxu_cmdq = new io_vxu_cmdq().flip
  val vxu_immq = new io_vxu_immq().flip
  val vxu_imm2q = new io_vxu_imm2q().flip
  val vxu_cntq = new io_vxu_cntq().flip

  val vec_ackq = new io_vec_ackq

  val cp_imul_req = new io_imul_req().flip
  val cp_imul_resp = Bits(SZ_XLEN, OUTPUT)
  val cp_dfma = new io_cp_dfma()
  val cp_sfma = new io_cp_sfma()

  val imem_req = new io_imem_req()
  val imem_resp = new io_imem_resp().flip

  val lane_vaq = new io_vvaq()
  val lane_vldq = new io_vldq().flip
  val lane_vsdq = new io_vsdq()

  val lane_vaq_dec = Bool(OUTPUT)
  val lane_vsdq_dec = Bool(OUTPUT)

  val qcntp1 = UFix(SZ_QCNT, OUTPUT)
  val qcntp2 = UFix(SZ_QCNT, OUTPUT)
  
  val pending_store = Bool(INPUT)

  val irb_cmdb = new io_vxu_cmdq()
  val irb_imm1b = new io_vxu_immq()
  val irb_imm2b = new io_vxu_imm2q()
  val irb_cntb = new io_vxu_cntq()

  val issue_to_irb = new io_issue_to_irb()
  val irb_to_issue = new io_irb_to_issue().flip()

  val seq_to_irb = new io_seq_to_irb()

  val xcpt_to_vxu = new io_xcpt_handler_to_vxu().flip()
  val vxu_to_xcpt = new io_vxu_to_xcpt_handler()
}

class vuVXU extends Component
{
  val io = new io_vxu()


  val issue = new vuVXU_Issue()

  issue.io.illegal <> io.illegal
  issue.io.imem_req <> io.imem_req
  issue.io.imem_resp <> io.imem_resp
  issue.io.vec_ackq <> io.vec_ackq
  issue.io.vxu_cmdq <> io.vxu_cmdq
  issue.io.vxu_immq <> io.vxu_immq
  issue.io.vxu_imm2q <> io.vxu_imm2q
  issue.io.vxu_cntq <> io.vxu_cntq
  issue.io.pending_store <> io.pending_store
  
  issue.io.irb_cmdb <> io.irb_cmdb
  issue.io.irb_imm1b <> io.irb_imm1b
  issue.io.irb_imm2b <> io.irb_imm2b
  issue.io.irb_cntb <> io.irb_cntb

  issue.io.issue_to_irb <> io.issue_to_irb
  issue.io.irb_to_issue <> io.irb_to_issue

  issue.io.flush <> io.xcpt_to_vxu.flush
  issue.io.xcpt_to_issue <> io.xcpt_to_vxu.issue

  val b8fire = new vuVXU_Banked8_Fire()

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


  val b8hazard = new vuVXU_Banked8_Hazard()

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

  b8hazard.io.flush <> io.xcpt_to_vxu.flush


  val b8seq = new vuVXU_Banked8_Seq()

  b8seq.io.issue_to_seq <> issue.io.issue_to_seq
  b8seq.io.seq_to_hazard <> b8hazard.io.seq_to_hazard

  b8seq.io.qstall.vaq := ~io.lane_vaq.ready
  b8seq.io.qstall.vldq := ~io.lane_vldq.valid
  b8seq.io.qstall.vsdq := ~io.lane_vsdq.ready

  b8seq.io.fire <> b8fire.io.fire
  b8seq.io.fire_fn <> b8fire.io.fire_fn
  b8seq.io.fire_regid_imm <> b8fire.io.fire_regid_imm

  b8seq.io.seq_to_irb <> io.seq_to_irb

  b8seq.io.flush <> io.xcpt_to_vxu.flush
  b8seq.io.xcpt_to_seq <> io.xcpt_to_vxu.seq
  b8seq.io.seq_to_xcpt <> io.vxu_to_xcpt.seq

  val b8expand = new vuVXU_Banked8_Expand()

  b8expand.io.seq_to_expand <> b8seq.io.seq_to_expand
  b8expand.io.expand_to_hazard <> b8hazard.io.expand_to_hazard

  b8expand.io.seq <> b8seq.io.seq
  b8expand.io.seq_fn <> b8seq.io.seq_fn
  b8expand.io.seq_regid_imm <> b8seq.io.seq_regid_imm

  b8expand.io.expand_to_xcpt <> io.vxu_to_xcpt.expand

  val b8lane = new vuVXU_Banked8_Lane()

  b8lane.io.issue_to_lane <> issue.io.issue_to_lane

  b8lane.io.cp.imul_val <> io.cp_imul_req.valid
  b8lane.io.cp.imul_rdy <> io.cp_imul_req.ready
  b8lane.io.cp.imul_fn <> io.cp_imul_req.bits.fn
  b8lane.io.cp.imul_in0 <> io.cp_imul_req.bits.in0
  b8lane.io.cp.imul_in1 <> io.cp_imul_req.bits.in1
  b8lane.io.cp.imul_out <> io.cp_imul_resp

  io.cp_dfma <> b8lane.io.cp_dfma
  io.cp_sfma <> b8lane.io.cp_sfma

  b8lane.io.expand_read <> b8expand.io.expand_read
  b8lane.io.expand_write <> b8expand.io.expand_write
  b8lane.io.expand_fu_fn <> b8expand.io.expand_fu_fn
  b8lane.io.expand_lfu_fn <> b8expand.io.expand_lfu_fn

  b8lane.io.lane_to_hazard <> b8hazard.io.lane_to_hazard

  b8lane.io.vmu.vldq_rdy <> io.lane_vldq.ready
  b8lane.io.vmu.vldq_bits <> io.lane_vldq.bits

  // memory interface
  val b8mem = new vuVXU_Banked8_Mem()
  b8mem.io.lane_vaq_valid := b8lane.io.vmu.vaq_val
  b8mem.io.lane_vaq_check <> b8lane.io.vmu.vaq_check
  b8mem.io.lane_vaq_mem <> b8lane.io.vmu.vaq_mem
  b8mem.io.lane_vaq_imm := b8lane.io.vmu.vaq_imm
  b8mem.io.lane_vaq_utmemop := b8lane.io.vmu.vaq_utmemop
  b8mem.io.lane_vaq_rf := b8lane.io.vmu.vaq_rf

  b8mem.io.lane_vsdq_valid := b8lane.io.vmu.vsdq_val 
  b8mem.io.lane_vsdq_mem <> b8lane.io.vmu.vsdq_mem
  b8mem.io.lane_vsdq_bits := b8lane.io.vmu.vsdq_bits

  io.lane_vaq.valid := b8mem.io.vmu_vaq_valid
  io.lane_vaq.bits <> b8mem.io.vmu_vaq_bits
  io.lane_vaq_dec := b8lane.io.vmu.vaq_val

  io.lane_vsdq.valid := b8mem.io.vmu_vsdq_valid
  io.lane_vsdq.bits := b8mem.io.vmu_vsdq_bits
  io.lane_vsdq_dec := b8lane.io.vmu.vsdq_val
  
  io.qcntp1 := b8seq.io.qcntp1
  io.qcntp2 := b8seq.io.qcntp2
}

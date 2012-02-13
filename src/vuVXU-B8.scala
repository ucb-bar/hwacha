package hwacha

import Chisel._
import Node._
import Config._
import Interface._

class vuVXU extends Component
{
  val io = new io_vxu()


  val issue = new vuVXU_Issue()

  issue.io.illegal <> io.illegal
  issue.io.imem_req <> io.imem_req
  issue.io.imem_resp <> io.imem_resp
  issue.io.vec_ackq <> io.vec_ackq
  issue.io.vmu_vackq <> io.vmu_vackq
  issue.io.vxu_cmdq <> io.vxu_cmdq
  issue.io.vxu_immq <> io.vxu_immq
  issue.io.vxu_imm2q <> io.vxu_imm2q
  issue.io.vmu_vcmdq.ready <> io.vmu_vcmdq.ready
  issue.io.vmu_utcmdq.ready <> io.vmu_utcmdq.ready


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


  val b8seq = new vuVXU_Banked8_Seq()

  b8seq.io.issue_to_seq <> issue.io.issue_to_seq
  b8seq.io.seq_to_hazard <> b8hazard.io.seq_to_hazard

  b8seq.io.qstall.vaq := ~io.lane_vaq.ready
  b8seq.io.qstall.vldq := ~io.lane_vldq.valid
  b8seq.io.qstall.vsdq := ~io.lane_vsdq.ready
  b8seq.io.qstall.utaq := ~io.lane_utaq.ready
  b8seq.io.qstall.utldq := ~io.lane_utldq.valid
  b8seq.io.qstall.utsdq := ~io.lane_utsdq.ready

  b8seq.io.fire <> b8fire.io.fire
  b8seq.io.fire_fn <> b8fire.io.fire_fn
  b8seq.io.fire_regid_imm <> b8fire.io.fire_regid_imm


  val b8expand = new vuVXU_Banked8_Expand()

  b8expand.io.seq_to_expand <> b8seq.io.seq_to_expand
  b8expand.io.expand_to_hazard <> b8hazard.io.expand_to_hazard

  b8expand.io.seq <> b8seq.io.seq
  b8expand.io.seq_fn <> b8seq.io.seq_fn
  b8expand.io.seq_regid_imm <> b8seq.io.seq_regid_imm


  val b8lane = new vuVXU_Banked8_Lane()

  b8lane.io.issue_to_lane <> issue.io.issue_to_lane

  b8lane.io.cp.imul_val <> io.cp_imul_req.valid
  b8lane.io.cp.imul_rdy <> io.cp_imul_req.ready
  b8lane.io.cp.imul_fn <> io.cp_imul_req.bits.fn
  b8lane.io.cp.imul_in0 <> io.cp_imul_req.bits.in0
  b8lane.io.cp.imul_in1 <> io.cp_imul_req.bits.in1
  b8lane.io.cp.imul_out <> io.cp_imul_resp

  b8lane.io.cp.fma_val <> io.cp_fma_req.valid
  b8lane.io.cp.fma_rdy <> io.cp_fma_req.ready
  b8lane.io.cp.fma_fn <> io.cp_fma_req.bits.fn
  b8lane.io.cp.fma_in0 <> io.cp_fma_req.bits.in0
  b8lane.io.cp.fma_in1 <> io.cp_fma_req.bits.in1
  b8lane.io.cp.fma_in2 <> io.cp_fma_req.bits.in2
  b8lane.io.cp.fma_out <> io.cp_fma_resp.out
  b8lane.io.cp.fma_exc <> io.cp_fma_resp.exc

  b8lane.io.expand_read <> b8expand.io.expand_read
  b8lane.io.expand_write <> b8expand.io.expand_write
  b8lane.io.expand_fu_fn <> b8expand.io.expand_fu_fn
  b8lane.io.expand_lfu_fn <> b8expand.io.expand_lfu_fn

  b8lane.io.lane_to_hazard <> b8hazard.io.lane_to_hazard

  b8lane.io.vmu.vldq_rdy <> io.lane_vldq.ready
  b8lane.io.vmu.vldq_bits <> io.lane_vldq.bits
  b8lane.io.vmu.vsdq_val <> io.lane_vsdq.valid
  b8lane.io.vmu.vsdq_bits <> io.lane_vsdq.bits
  b8lane.io.vmu.utaq_val <> io.lane_utaq.valid
  b8lane.io.vmu.utaq_bits <> io.lane_utaq.bits
  b8lane.io.vmu.utldq_rdy <> io.lane_utldq.ready
  b8lane.io.vmu.utldq_bits <> io.lane_utldq.bits
  b8lane.io.vmu.utsdq_val <> io.lane_utsdq.valid
  b8lane.io.vmu.utsdq_bits <> io.lane_utsdq.bits


  // memory interface
  io.vxu_to_vmu.qcnt := b8seq.io.seq_regid_imm.qcnt

  io.vmu_vcmdq.valid := b8lane.io.vmu.vaq_val || issue.io.vmu_vcmdq.valid
  io.vmu_vbaseq.valid := b8lane.io.vmu.vaq_val
  io.vmu_vstrideq.valid := b8lane.io.vmu.vaq_val & b8lane.io.vmu.cmd(19)
  
  io.vmu_vcmdq.bits := 
    Mux(issue.io.vmu_vcmdq.valid, issue.io.vmu_vcmdq.bits,
        b8lane.io.vmu.cmd(18,0))
  io.vmu_vbaseq.bits := b8lane.io.vmu.imm(63,0)
  io.vmu_vstrideq.bits := Bits(0, DEF_VXU_IMM2Q)

  io.vmu_utcmdq.valid := b8lane.io.vmu.utaq_val || issue.io.vmu_utcmdq.valid
  io.vmu_utimmq.valid := b8lane.io.vmu.utaq_val & b8lane.io.vmu.cmd(19)

  io.vmu_utcmdq.bits :=
    Mux(issue.io.vmu_utcmdq.valid, issue.io.vmu_utcmdq.bits,
        b8lane.io.vmu.cmd(18,0))
  io.vmu_utimmq.bits := b8lane.io.vmu.imm(31,0)


  // responses
  issue.io.vxu_ackq.bits <> io.vmu_utackq.bits
  issue.io.vxu_ackq.valid <> io.vmu_utackq.valid
  io.vmu_utackq.ready <> issue.io.vxu_ackq.ready
}

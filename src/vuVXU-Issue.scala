package hwacha

import Chisel._
import Node._
import Constants._

class vuVXU_Issue extends Component
{
  val io = new io_vxu_issue()

  val tvec = new vuVXU_Issue_TVEC()
  val vt = new vuVXU_Issue_VT()

  tvec.io.vf <> vt.io.vf

  vt.io.illegal <> io.illegal
  vt.io.imem_req <> io.imem_req
  vt.io.imem_resp <> io.imem_resp

  tvec.io.vxu_cmdq <> io.vxu_cmdq
  tvec.io.vxu_immq <> io.vxu_immq
  tvec.io.vxu_imm2q <> io.vxu_imm2q

  tvec.io.issue_to_hazard <> io.issue_to_hazard
  tvec.io.issue_to_seq <> io.issue_to_seq
  tvec.io.issue_to_lane <> io.issue_to_lane
  tvec.io.hazard_to_issue <> io.hazard_to_issue

  tvec.io.vec_ackq <> io.vec_ackq

  tvec.io.valid <> io.tvec_valid
  tvec.io.ready <> io.tvec_ready
  tvec.io.dhazard <> io.tvec_dhazard
  tvec.io.shazard <> io.tvec_shazard
  tvec.io.bhazard <> io.tvec_bhazard
  tvec.io.fn <> io.tvec_fn
  tvec.io.decoded <> io.tvec_regid_imm
  tvec.io.pending_store <> io.pending_store

  tvec.io.irb_cmdb <> io.irb_cmdb
  tvec.io.irb_imm1b <> io.irb_imm1b
  tvec.io.irb_imm2b <> io.irb_imm2b
  tvec.io.irb_cntb.ready := io.irb_cntb.ready
  tvec.io.irb_to_issue <> io.irb_to_issue

  vt.io.valid <> io.vt_valid
  vt.io.ready <> io.vt_ready
  vt.io.dhazard <> io.vt_dhazard
  vt.io.shazard <> io.vt_shazard
  vt.io.bhazard <> io.vt_bhazard
  vt.io.fn <> io.vt_fn
  vt.io.decoded <> io.vt_regid_imm

  vt.io.irb_cntb.ready := io.irb_cntb.ready
  vt.io.irb_to_issue <> io.irb_to_issue
  vt.io.issue_to_irb <> io.issue_to_irb

  io.irb_cntb.valid := Mux(tvec.io.active, tvec.io.irb_cntb.valid, vt.io.irb_cntb.valid)
  io.irb_cntb.bits := Mux(tvec.io.active, tvec.io.irb_cntb.bits, vt.io.irb_cntb.bits)
}

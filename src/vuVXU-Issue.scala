package hwacha

import Chisel._
import Node._
import Constants._

class io_vxu_issue_fire extends Bundle
{
  val viu = Bool()
  val vau0 = Bool()
  val vau1 = Bool()
  val vau2 = Bool()
  val amo = Bool()
  val utld = Bool()
  val utst = Bool()
  val vld = Bool()
  val vst = Bool()
}

class io_vxu_issue_fu extends Bundle
{
  val viu = Bool()
  val vau0 = Bool()
  val vau1 = Bool()
  val vau2 = Bool()
  val vgu = Bool()
  val vlu = Bool()
  val vsu = Bool()
}

class io_vxu_issue_fn extends Bundle
{
  val viu = Bits(width = SZ_VIU_FN)
  val vau0 = Bits(width = SZ_VAU0_FN)
  val vau1 = Bits(width = SZ_VAU1_FN)
  val vau2 = Bits(width = SZ_VAU2_FN)
}

class io_vxu_issue_reg extends Bundle
{
  val vs = Bool()
  val vt = Bool()
  val vr = Bool()
  val vd = Bool()
}

class io_vxu_issue_regid_imm extends Bundle
{
  val vs_zero = Bool()
  val vt_zero = Bool()
  val vr_zero = Bool()
  val vd_zero = Bool()
  val vs = Bits(width = SZ_REGLEN)
  val vt = Bits(width = SZ_REGLEN)
  val vr = Bits(width = SZ_REGLEN)
  val vd = Bits(width = SZ_REGLEN)
  val mem = new io_vxu_mem_cmd()
  val imm = Bits(width = SZ_DATA)
  val imm2 = Bits(width = SZ_DATA)
  val irb = new io_vxu_irb_bundle()
}

class io_vxu_issue_op extends Bundle
{
  val r1w1 = Bool()
  val r2w1 = Bool()
  val r3w1 = Bool()
  val amo = Bool()
  val utld = Bool()
  val utst = Bool()
  val vld = Bool()
  val vst = Bool()
}

class io_vxu_issue_to_hazard extends Bundle
{
  val bcnt = Bits(width = SZ_BCNT)
}

class io_vxu_issue_to_seq extends Bundle
{
  val vlen = Bits(width = SZ_VLEN)
  val stride = Bits(width = SZ_REGLEN)
  val bcnt = Bits(width = SZ_BCNT)
}

class io_vxu_issue_to_lane extends Bundle
{
  val bactive = Bits(width = SZ_BANK)
}

class io_issue_to_irb extends Bundle
{
  val markLast = Bool(OUTPUT)
}

class io_vxu_issue extends Bundle
{
  val illegal = Bool(OUTPUT)

  val imem_req = new io_imem_req()
  val imem_resp = new io_imem_resp().flip()

  val vec_ackq = new io_vec_ackq  

  val vxu_cmdq = new io_vxu_cmdq().flip()
  val vxu_immq = new io_vxu_immq().flip()
  val vxu_imm2q = new io_vxu_imm2q().flip()

  val issue_to_hazard = new io_vxu_issue_to_hazard().asOutput
  val issue_to_seq = new io_vxu_issue_to_seq().asOutput
  val issue_to_lane = new io_vxu_issue_to_lane().asOutput
  val hazard_to_issue = new io_vxu_hazard_to_issue().asInput

  val tvec_valid = new io_vxu_issue_fire().asOutput
  val tvec_ready = Bool(INPUT)
  val tvec_dhazard = new io_vxu_issue_reg().asOutput
  val tvec_shazard = new io_vxu_issue_fu().asOutput
  val tvec_bhazard = new io_vxu_issue_op().asOutput
  val tvec_fn = new io_vxu_issue_fn().asOutput
  val tvec_regid_imm = new io_vxu_issue_regid_imm().asOutput

  val vt_valid = new io_vxu_issue_fire().asOutput
  val vt_ready = Bool(INPUT)
  val vt_dhazard = new io_vxu_issue_reg().asOutput
  val vt_shazard = new io_vxu_issue_fu().asOutput
  val vt_bhazard = new io_vxu_issue_op().asOutput
  val vt_fn = new io_vxu_issue_fn().asOutput
  val vt_regid_imm = new io_vxu_issue_regid_imm().asOutput
  
  val pending_store = Bool(INPUT)

  val irb_cmdb = new io_vxu_cmdq()
  val irb_imm1b = new io_vxu_immq()
  val irb_imm2b = new io_vxu_imm2q()
  val irb_cntb = new io_vxu_cntq()

  val issue_to_irb = new io_issue_to_irb()
  val irb_to_issue = new io_irb_to_issue().flip()

  val cpu_exception = new io_cpu_exception().flip()
}

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
  
  tvec.io.cpu_exception <> io.cpu_exception

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

  vt.io.cpu_exception <> io.cpu_exception

  io.irb_cntb.valid := Mux(tvec.io.active, tvec.io.irb_cntb.valid, vt.io.irb_cntb.valid)
  io.irb_cntb.bits := Mux(tvec.io.active, tvec.io.irb_cntb.bits, vt.io.irb_cntb.bits)
}

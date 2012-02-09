package riscvVector

import Chisel._
import Node._
import Config._
import Interface._

class io_ready_valid[T <: Data]()(data: => T) extends Bundle
{
  val ready = Bool(INPUT)
  val valid = Bool(OUTPUT)
  val bits = data.asOutput
}

class io_valid[T <: Data]()(data: => T) extends Bundle
{
  val valid = Bool(OUTPUT)
  val bits = data.asOutput
}

class io_arbiter[T <: Data](n: Int)(data: => io_ready_valid[T]) extends Bundle
{
  val in = Vec(n){ data.flip() }
  val out = data
}

class Arbiter[T <: Data](n: Int)(data: => io_ready_valid[T]) extends Component
{
  val io = new io_arbiter(n)(data)

  io.in(0).ready := io.out.ready
  for (i <- 1 to n-1)
    io.in(i).ready := !io.in(i-1).valid && io.in(i-1).ready

  var dout = io.in(n-1).bits
  for (i <- 1 to n-1)
    dout = Mux(io.in(n-1-i).valid, io.in(n-1-i).bits, dout)

  var vout = io.in(0).valid
  for (i <- 1 to n-1)
    vout = vout || io.in(i).valid

  vout <> io.out.valid
  dout <> io.out.bits
}

class io_imem_req extends io_ready_valid()( { Bits(width = DEF_ADDR) } )
class io_imem_resp extends io_valid()( { Bits(width = DEF_INST) } )
class io_vxu_cmdq extends io_ready_valid()( { Bits(width = DEF_VXU_CMDQ) } )
class io_vxu_immq extends io_ready_valid()( { Bits(width = DEF_VXU_IMMQ) } )
class io_vxu_imm2q extends io_valid()( { Bitws(width = DEF_VXU_IMM2Q) } )
class io_vxu_ackq extends io_ready_valid()( { Bits(width = DEF_VXU_ACKQ) } )
class io_vmu_utcmdq extends io_ready_valid()( { Bits(width = DEF_VMU_UTCMDQ) } )
class io_vmu_utimmq extends io_ready_valid()( { Bits(width = DEF_VMU_UTIMMQ) } )
class io_vmu_utackq extends io_ready_valid()( { Bits(width = DEF_VMU_UTACKQ) } )
class io_vldq extends io_ready_valid()( { Bits(width = DEF_DATA) } )
class io_vsdq extends io_ready_valid()( { Bits(width = DEF_DATA) } )
class io_utaq extends io_ready_valid()( { Bits(width = DEF_ADDR) } )
class io_utldq extends io_ready_valid()( { Bits(width = DEF_DATA) } )
class io_utstq extends io_ready_valid()( { Bits(width = DEF_DATA) } )

class io_vec_cmdq extends io_ready_valid()( { Bits(width = VCMD_SZ) } )
class io_vec_ximm1q extends io_ready_valid()( { Bits(width = VIMM_SZ) } )
class io_vec_ximm2q extends io_ready_valid()( { Bits(width = VSTRIDE_SZ) } )
class io_vmu_vcmdq extends io_ready_valid()( { Bits(width = VMCMD_SZ) } )
class io_vmu_vbaseq extends io_ready_valid()( { Bits(width = VMIMM_SZ) } )
class io_vmu_vstrideq extends io_ready_valid()( { Bits(width = VMSTRIDE_SZ) } )
class io_vec_ackq extends io_ready_valid()( { Bits(width = VRESP_SZ) } )
class io_vmu_vackq extends io_ready_valid()( { Bits(width = VMRESP_SZ) } )
class io_lane_vldq extends io_ready_valid()( { Bits(width = DEF_DATA) } )
class io_lane_vsdq extends io_ready_valid()( { Bits(width = DEF_DATA) } )
class io_lane_utaq extends io_ready_valid()( { Bits(width = DEF_ADDR) } )
class io_lane_utldq extends io_ready_valid()( { Bits(width = DEF_DATA) } )
class io_lane_utsdq extends io_ready_valid()( { Bits(width = DEF_DATA) } )

class io_imul_req_bundle extends Bundle
{
  val fn = Bits(width = DEF_VAU0_FN)
  val in0 = Bits(width = DEF_XLEN)
  val in1 = Bits(width = DEF_XLEN)
}

class io_fma_req_bundle extends Bundle
{
  val fn = Bits(width = DEF_VAU1_FN)
  val in0 = Bits(width = DEF_FLEN)
  val in1 = Bits(width = DEF_FLEN)
  val in2 = Bits(width = DEF_FLEN)
}

class io_imul_req extends io_ready_valid()( { new io_imul_req_bundle() } )
class io_fma_req extends io_ready_valid()( { new io_fma_req_bundle() } )

class io_fma_resp extends Bundle
{
  val out = Bits(width=DEF_FLEN)
  val exc = Bits(width=DEF_EXC)
}

class io_dmem_ut_req_bundle extends Bundle
{
  val addr = Bits(width = 30)
  val op = Bits(width = 4)
  val data = Bits(width = 64)
  val wmask = Bits(width = 8)
  val tag = Bits(width = 12)
}

class io_dmem_ut_resp_bundle extends Bundle 
{
  val tag = Bits(width = 12)
  val data = Bits(width = 64)
}

class io_dmem_vec_req_bundle extends Bundle
{
  val addr = Bits(width = 28)
  val op = Bits(width = 4)
  val data = Bits(width = 128)
  val wmask = Bits(width = 16)
  val tag = Bits(width = 12)  
}

class io_dmem_vec_resp_bundle extends Bundle
{
  val tag = Bits(width = 12)
  val data = Bits(width = 128)
}

class io_dmem_ut_req extends io_ready_valid()( { new io_dmem_ut_req_bundle() } )
class io_dmem_ut_resp extends io_valid()( { new io_dmem_ut_resp_bundle() } )

class io_dmem_vec_req extends io_ready_valid()( { new io_dmem_vec_req_bundle() } )
class io_dmem_vec_resp extends io_valid()( { new io_dmem_vec_resp_bundle() } )

class io_vf extends Bundle
{
  val active = Bool(OUTPUT)
  val fire = Bool(OUTPUT)
  val stop = Bool(INPUT)
  val pc = Bits(DEF_ADDR, OUTPUT)
  val vlen = Bits(DEF_VLEN, OUTPUT)
  val nxregs = Bits(DEF_REGCNT, OUTPUT)
}

class io_qstall extends Bundle
{
  val vldq = Bool()
  val vsdq = Bool()
  val utaq = Bool()
  val utldq = Bool()
  val utsdq = Bool()
}

class io_vxu_issue_fu extends Bundle
{
  val viu = Bool()
  val vau0 = Bool()
  val vau1 = Bool()
  val vau2 = Bool()
  val vgslu = Bool()
  val vglu = Bool()
  val vgsu = Bool()
  val vgu = Bool()
  val vlu = Bool()
  val vsu = Bool()
}

class io_vxu_issue_fn extends Bundle
{
  val viu = Bits(width = DEF_VIU_FN)
  val vau0 = Bits(width = DEF_VAU0_FN)
  val vau1 = Bits(width = DEF_VAU1_FN)
  val vau2 = Bits(width = DEF_VAU2_FN)
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
  val vs = Bits(width = DEF_REGLEN)
  val vt = Bits(width = DEF_REGLEN)
  val vr = Bits(width = DEF_REGLEN)
  val vd = Bits(width = DEF_REGLEN)
  val imm = Bits(width = DEF_DATA)
  val imm2 = Bits(width = DEF_VXU_IMM2Q)
}

class io_vxu_issue_op extends Bundle
{
  val r1w1 = Bool()
  val r2w1 = Bool()
  val r3w1 = Bool()
  val vgslu = Bool()
  val vglu = Bool()
  val vgsu = Bool()
  val vlu = Bool()
  val vsu = Bool()
}

class io_vxu_seq_fu extends Bundle
{
  val viu = Bool()
  val vau0 = Bool()
  val vau1 = Bool()
  val vau2 = Bool()
  val vldq = Bool()
  val vsdq = Bool()
  val utaq = Bool()
  val utldq = Bool()
  val utsdq = Bool()
}

class io_vxu_seq_fn extends Bundle
{
  val viu = Bits(width = DEF_VIU_FN)
  val vau0 = Bits(width = DEF_VAU0_FN)
  val vau1 = Bits(width = DEF_VAU1_FN)
  val vau2 = Bits(width = DEF_VAU2_FN)
}

class io_vxu_seq_regid_imm extends Bundle
{
  val cnt = Bits(width = DEF_BVLEN)
  val utidx = Bits(width = DEF_VLEN)
  val vs_zero = Bool()
  val vt_zero = Bool()
  val vr_zero = Bool()
  val vs = Bits(width = DEF_BREGLEN)
  val vt = Bits(width = DEF_BREGLEN)
  val vr = Bits(width = DEF_BREGLEN)
  val vd = Bits(width = DEF_BREGLEN)
  val imm = Bits(width = DEF_DATA)
  val imm2 = Bits(width = DEF_VXU_IMM2Q);
}

class io_vxu_expand_read extends Bundle
{
  val ren = Bool()
  val rlast = Bool()
  val rcnt = Bits(width = DEF_BVLEN)
  val raddr = Bits(width = DEF_BREGLEN)
  val roplen = Bits(width = DEF_BOPL)
  val rblen = Bits(width = DEF_BRPORT)
}

class io_vxu_expand_write extends Bundle
{
  val wen = Bool()
  val wlast = Bool()
  val wcnt = Bits(width = DEF_BVLEN)
  val waddr = Bits(width = DEF_BREGLEN)
  val wsel = Bits(width = DEF_BWPORT)
}

class io_vxu_expand_fu_fn extends Bundle
{
  val viu = Bool()
  val viu_fn = Bits(width = DEF_VIU_FN)
  val viu_utidx = Bits(width = DEF_VLEN)
  val viu_imm = Bits(width = DEF_DATA)
}

class io_vxu_expand_lfu_fn extends Bundle
{
  val vau0 = Bool()
  val vau0_fn = Bits(width = DEF_VAU0_FN)
  val vau1 = Bool()
  val vau1_fn = Bits(width = DEF_VAU1_FN)
  val vau2 = Bool()
  val vau2_fn = Bits(width = DEF_VAU2_FN)
  val vldq = Bool()
  val vsdq = Bool()
  val utaq = Bool()
  val utldq = Bool()
  val utsdq = Bool()
}

class io_vxu_issue_to_hazard extends Bundle
{
  val bcnt = Bits(width = DEF_BCNT)
}

class io_vxu_issue_to_seq extends Bundle
{
  val vlen = Bits(width = DEF_VLEN)
  val stride = Bits(width = DEF_REGLEN)
  val bcnt = Bits(width = DEF_BCNT)
}

class io_vxu_issue_to_lane extends Bundle
{
  val bactive = Bits(width = DEF_BANK)
}

class io_vxu_seq_to_hazard extends Bundle
{
  val stall = Bits(width = DEF_STALL)
  val last = Bool()
}

class io_vxu_seq_to_expand extends Bundle
{
  val last = Bool()
}

class io_vxu_expand_to_hazard extends Bundle
{
  val ren = Bool()
  val wen = Bool()
}

class io_lane_to_hazard extends Bundle
{
  val rlast = Bool()
  val wlast = Bool()
}

class io_vxu_issue_tvec extends Bundle
{
  val vf = new io_vf()

  val issue_to_hazard = new io_vxu_issue_to_hazard().asOutput
  val issue_to_seq = new io_vxu_issue_to_seq().asOutput
  val issue_to_lane = new io_vxu_issue_to_lane().asOutput

  val vxu_cmdq = new io_vxu_cmdq().flip()
  val vxu_immq = new io_vxu_immq().flip()
  val vmu_utcmdq = new io_vmu_utcmdq()

  val valid = new io_vxu_issue_fu().asOutput
  val ready = Bool(INPUT)
  val dhazard = new io_vxu_issue_reg().asOutput
  val shazard = new io_vxu_issue_fu().asOutput
  val bhazard = new io_vxu_issue_op().asOutput
  val fn = new io_vxu_issue_fn().asOutput
  val decoded = new io_vxu_issue_regid_imm().asOutput
}

class io_vcu extends Bundle
{
  val vec_cmdq = new io_vec_cmdq().flip()
  val vec_ximm1q = new io_vec_ximm1q().flip()
  val vec_ximm2q = new io_vec_ximm2q().flip()
  val vxu_cmdq = new io_vxu_cmdq
  val vxu_immq = new io_vxu_immq
  val vxu_imm2q = new io_vxu_imm2q
  val vmu_vcmdq = new io_vmu_vcmdq
  val vmu_vbaseq = new io_vmu_vbaseq
  val vmu_vstrideq = new io_vmu_vstrideq
  val vec_ackq = new io_vec_ackq
  val vxu_ackq = new io_vxu_ackq().flip()
  val vmu_vackq = new io_vmu_vackq().flip()
}

class io_vxu_issue_vt extends Bundle
{
  val illegal = Bool(OUTPUT)

  val imem_req = new io_imem_req()
  val imem_resp = new io_imem_resp().flip()

  val vmu_utcmdq = new io_vmu_utcmdq
  val vmu_utimmq = new io_vmu_utimmq

  val vf = new io_vf().flip()

  val valid = new io_vxu_issue_fu().asOutput
  val ready = Bool(INPUT)
  val dhazard = new io_vxu_issue_reg().asOutput
  val shazard = new io_vxu_issue_fu().asOutput
  val bhazard = new io_vxu_issue_op().asOutput
  val fn = new io_vxu_issue_fn().asOutput
  val decoded = new io_vxu_issue_regid_imm().asOutput
}

class io_vu extends Bundle 
{
  val illegal = Bool(OUTPUT)

  val vec_cmdq = new io_vec_cmdq().flip()
  val vec_ximm1q = new io_vec_ximm1q().flip()
  val vec_ximm2q = new io_vec_ximm2q().flip()
  val vec_ackq = new io_vec_ackq()

  val cp_imul_req = new io_imul_req().flip()
  val cp_imul_resp = Bits(DEF_XLEN, OUTPUT)
  
  val cp_fma_req = new io_fma_req().flip()
  val cp_fma_resp = new io_fma_resp().asOutput

  val imem_req = new io_imem_req()
  val imem_resp = new io_imem_resp().flip()

  val dmem_req_ut = new io_dmem_ut_req()
  val dmem_resp_ut = new io_dmem_ut_resp().flip()

  val dmem_req_vec = new io_dmem_vec_req()
  val dmem_resp_vec = new io_dmem_vec_resp().flip()
}

class io_vxu extends Bundle
{
  val illegal = Bool(OUTPUT)

  val vxu_cmdq = new io_vxu_cmdq().flip()
  val vxu_immq = new io_vxu_immq().flip()
  val vxu_imm2q = new io_vxu_imm2q().flip()
  val vxu_ackq = new io_vxu_ackq()

  val vmu_utcmdq = new io_vmu_utcmdq()
  val vmu_utimmq = new io_vmu_utimmq()
  val vmu_utackq = new io_vmu_utackq().flip()

  val cp_imul_req = new io_imul_req().flip()
  val cp_imul_resp = Bits(DEF_XLEN, OUTPUT)
  val cp_fma_req = new io_fma_req().flip()
  val cp_fma_resp = new io_fma_resp().asOutput

  val imem_req = new io_imem_req()
  val imem_resp = new io_imem_resp().flip()

  val lane_vldq = new io_lane_vldq().flip()
  val lane_vsdq = new io_lane_vsdq()
  val lane_utaq = new io_lane_utaq()
  val lane_utldq = new io_lane_utldq().flip()
  val lane_utsdq = new io_lane_utsdq()
}

class io_vxu_issue extends Bundle
{
  val illegal = Bool(OUTPUT)

  val imem_req = new io_imem_req()
  val imem_resp = new io_imem_resp().flip()

  val vxu_cmdq = new io_vxu_cmdq().flip()
  val vxu_immq = new io_vxu_immq().flip()
  val vxu_imm2q = new io_vxu_imm2q().flip()
  val vmu_utcmdq = new io_vmu_utcmdq()
  val vmu_utimmq = new io_vmu_utimmq()

  val issue_to_hazard = new io_vxu_issue_to_hazard().asOutput
  val issue_to_seq = new io_vxu_issue_to_seq().asOutput
  val issue_to_lane = new io_vxu_issue_to_lane().asOutput

  val tvec_valid = new io_vxu_issue_fu().asOutput
  val tvec_ready = Bool(INPUT)
  val tvec_dhazard = new io_vxu_issue_reg().asOutput
  val tvec_shazard = new io_vxu_issue_fu().asOutput
  val tvec_bhazard = new io_vxu_issue_op().asOutput
  val tvec_fn = new io_vxu_issue_fn().asOutput
  val tvec_regid_imm = new io_vxu_issue_regid_imm().asOutput

  val vt_valid = new io_vxu_issue_fu().asOutput
  val vt_ready = Bool(INPUT)
  val vt_dhazard = new io_vxu_issue_reg().asOutput
  val vt_shazard = new io_vxu_issue_fu().asOutput
  val vt_bhazard = new io_vxu_issue_op().asOutput
  val vt_fn = new io_vxu_issue_fn().asOutput
  val vt_regid_imm = new io_vxu_issue_regid_imm().asOutput
}

class io_vxu_fire extends Bundle
{
  val tvec_valid = new io_vxu_issue_fu().asInput
  val tvec_ready = Bool(INPUT)
  val tvec_dhazard = new io_vxu_issue_reg().asInput
  val tvec_shazard = new io_vxu_issue_fu().asInput
  val tvec_bhazard = new io_vxu_issue_op().asInput
  val tvec_fn = new io_vxu_issue_fn().asInput
  val tvec_regid_imm = new io_vxu_issue_regid_imm().asInput

  val vt_valid = new io_vxu_issue_fu().asInput
  val vt_ready = Bool(INPUT)
  val vt_dhazard = new io_vxu_issue_reg().asInput
  val vt_shazard = new io_vxu_issue_fu().asInput
  val vt_bhazard = new io_vxu_issue_op().asInput
  val vt_fn = new io_vxu_issue_fn().asInput
  val vt_regid_imm = new io_vxu_issue_regid_imm().asInput

  val fire = new io_vxu_issue_fu().asOutput
  val fire_fn = new io_vxu_issue_fn().asOutput
  val fire_regid_imm = new io_vxu_issue_regid_imm().asOutput
}

class io_vxu_hazard extends Bundle
{
  val issue_to_hazard = new io_vxu_issue_to_hazard().asInput
  val seq_to_hazard = new io_vxu_seq_to_hazard().asInput
  val expand_to_hazard = new io_vxu_expand_to_hazard().asInput
  val lane_to_hazard = new io_lane_to_hazard().asInput

  val tvec_valid = new io_vxu_issue_fu().asInput
  val tvec_ready = Bool(OUTPUT)
  val tvec_dhazard = new io_vxu_issue_reg().asInput
  val tvec_shazard = new io_vxu_issue_fu().asInput
  val tvec_bhazard = new io_vxu_issue_op().asInput
  val tvec_fn = new io_vxu_issue_fn().asInput
  val tvec_regid_imm = new io_vxu_issue_regid_imm().asInput

  val vt_valid = new io_vxu_issue_fu().asInput
  val vt_ready = Bool(OUTPUT)
  val vt_dhazard = new io_vxu_issue_reg().asInput
  val vt_shazard = new io_vxu_issue_fu().asInput
  val vt_bhazard = new io_vxu_issue_op().asInput
  val vt_fn = new io_vxu_issue_fn().asInput
  val vt_regid_imm = new io_vxu_issue_regid_imm().asInput

  val fire = new io_vxu_issue_fu().asInput
  val fire_fn = new io_vxu_issue_fn().asInput
  val fire_regid_imm = new io_vxu_issue_regid_imm().asInput
}

class io_vxu_seq extends Bundle
{
  val issue_to_seq = new io_vxu_issue_to_seq().asInput
  val seq_to_hazard = new io_vxu_seq_to_hazard().asOutput
  val seq_to_expand = new io_vxu_seq_to_expand().asOutput

  val qstall = new io_qstall().asInput

  val fire = new io_vxu_issue_fu().asInput
  val fire_fn = new io_vxu_issue_fn().asInput
  val fire_regid_imm = new io_vxu_issue_regid_imm().asInput

  val seq = new io_vxu_seq_fu().asOutput
  val seq_fn = new io_vxu_seq_fn().asOutput
  val seq_regid_imm = new io_vxu_seq_regid_imm().asOutput
}

class io_vxu_expand extends Bundle
{
  val seq_to_expand = new io_vxu_seq_to_expand().asInput
  val expand_to_hazard = new io_vxu_expand_to_hazard().asOutput

  val seq = new io_vxu_seq_fu().asInput
  val seq_fn = new io_vxu_seq_fn().asInput
  val seq_regid_imm = new io_vxu_seq_regid_imm().asInput

  val expand_read = new io_vxu_expand_read().asOutput
  val expand_write = new io_vxu_expand_write().asOutput
  val expand_fu_fn = new io_vxu_expand_fu_fn().asOutput
  val expand_lfu_fn = new io_vxu_expand_lfu_fn().asOutput
  val rblen_0 = Bits(8, OUTPUT)
  val rblen_1 = Bits(8, OUTPUT)
  val rblen_2 = Bits(8, OUTPUT)
}

// class vec_dcachereqIO extends Bundle
// {
//   // D$ interface
//   // request
//   val addr  = Bits(28, OUTPUT)
//   val tag   = Bits(12, OUTPUT)
//   val data  = Bits(128, OUTPUT)
//   val wmask = Bits(16, OUTPUT)
//   val op    = Bits(4, OUTPUT)
//   val valid = Bool(OUTPUT)
//   val rdy   = Bool(INPUT)
// }


// class vec_dcacherespIO extends Bundle
// {
//   // response
//   val data   = UFix(128, INPUT)
//   val tag    = UFix(12, INPUT)
//   val valid  = Bool(INPUT)
// }


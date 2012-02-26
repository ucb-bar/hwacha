package hwacha

import Chisel._
import Node._
import Constants._
import queues._

class io_ready_valid[T <: Data](view: List[String] = null)(data: => T) extends Bundle(view)
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
  val chosen = Bits(log2up(n),OUTPUT)
}

class cArbiter[T <: Data](n: Int)(data: => io_ready_valid[T]) extends Component
{
  val io = new io_arbiter(n)(data)

  io.in(0).ready := io.out.ready
  for (i <- 1 to n-1)
    io.in(i).ready := !io.in(i-1).valid && io.in(i-1).ready

  var dout = io.in(n-1).bits
  var choose = Bits(n-1)
  for (i <- 1 to n-1)
  {
    val actual = n-1-i
    dout = Mux(io.in(actual).valid, io.in(actual).bits, dout)
    choose = Mux(io.in(actual).valid, Bits(actual,log2up(n)), choose)
  }

  io.chosen := choose

  var vout = io.in(0).valid
  for (i <- 1 to n-1)
    vout = vout || io.in(i).valid

  vout <> io.out.valid
  dout <> io.out.bits
}


class hArbiter[T <: Data](n: Int)(data: => io_ready_valid[T]) extends Component
{
  val io = new io_arbiter(n)(data)

  io.in(0).ready := io.out.ready
  for (i <- 1 to n-1)
    io.in(i).ready := !io.in(i-1).valid && io.in(i-1).ready

  var dout = io.in(n-1).bits
  var choose = Bits(n-1)
  for (i <- 1 to n-1)
  {
    val actual = n-1-i
    dout = Mux(io.in(actual).valid, io.in(actual).bits, dout)
    choose = Mux(io.in(actual).valid, Bits(actual,log2up(n)), choose)
  }

  io.chosen := choose

  var vout = io.in(0).valid
  for (i <- 1 to n-1)
    vout = vout || io.in(i).valid

  vout <> io.out.valid
  dout <> io.out.bits
}

class io_imem_req extends io_ready_valid()( { Bits(width = SZ_ADDR) } )
class io_imem_resp extends io_valid()( { Bits(width = SZ_INST) } )
class io_vxu_cmdq extends io_ready_valid()( { Bits(width = SZ_XCMD) } )
class io_vxu_immq extends io_ready_valid()( { Bits(width = SZ_XIMM) } )
class io_vxu_imm2q extends io_ready_valid()( { Bits(width = SZ_XIMM2) } )
class io_vxu_cntq extends io_ready_valid()( Bits(width = SZ_VLEN) )
class io_vxu_ackq extends io_ready_valid()( { Bits(width = SZ_XRESP) } )
class io_vldq extends io_ready_valid()( { Bits(width = SZ_DATA) } )
class io_vsdq extends io_ready_valid()( { Bits(width = SZ_DATA) } )
class io_irb_sdb extends io_ready_valid()( Bits(width=SZ_DATA) )

class io_vec_cmdq(view: List[String] = null) extends io_ready_valid(view)( { Bits(width = SZ_VCMD) } )
class io_vec_ximm1q(view: List[String] = null) extends io_ready_valid(view)( { Bits(width = SZ_VIMM) } )
class io_vec_ximm2q(view: List[String] = null) extends io_ready_valid(view)( { Bits(width = SZ_VSTRIDE) } )
class io_vec_cntq() extends io_ready_valid()( Bits(width=SZ_VLEN) )
class io_vec_ackq extends io_ready_valid()( { Bits(width = SZ_VRESP) } )
class io_lane_vaq extends io_ready_valid()( { new io_vaq_bundle() } )
class io_lane_vldq extends io_ready_valid()( { Bits(width = SZ_DATA) } )
class io_lane_vsdq extends io_ready_valid()( { Bits(width = SZ_DATA) } )

class io_cpu_exception extends Bundle 
{
  val supervisor_mode = Bool(OUTPUT)
  val exception = Bool(OUTPUT)
  val addr = UFix(SZ_ADDR, OUTPUT)
}

class io_imul_req_bundle extends Bundle
{
  val fn = Bits(width = SZ_VAU0_FN)
  val in0 = Bits(width = SZ_XLEN)
  val in1 = Bits(width = SZ_XLEN)
}

class io_imul_req extends io_ready_valid()( { new io_imul_req_bundle() } )

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

class io_dmem_req_bundle extends Bundle
{
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val idx = Bits(width = PGIDX_BITS)
  val ppn = Bits(width = PPN_BITS)
  val data = Bits(width = 64)
  val tag = Bits(width = 10)
}

class io_dmem_resp_bundle extends Bundle
{
  val nack = Bool()
  val data = Bits(width = 64)
  val tag = Bits(width = 10)
  val typ = Bits(width = 3)
}

class io_dmem_ut_req extends io_ready_valid()( { new io_dmem_ut_req_bundle() } )
class io_dmem_ut_resp extends io_valid()( { new io_dmem_ut_resp_bundle() } )

class io_dmem_vec_req extends io_ready_valid()( { new io_dmem_vec_req_bundle() } )
class io_dmem_vec_resp extends io_valid()( { new io_dmem_vec_resp_bundle() } )

class io_dmem_req extends io_valid()({ new io_dmem_req_bundle() })
class io_dmem_resp extends io_valid()({ new io_dmem_resp_bundle() })

class io_irbUpdateReq(DATA_SIZE: Int, ADDR_SIZE: Int) extends Bundle 
{
  val data = Bits(width=DATA_SIZE)
  val addr = UFix(width=ADDR_SIZE)
}

class io_vf extends Bundle
{
  val active = Bool(OUTPUT)
  val fire = Bool(OUTPUT)
  val stop = Bool(INPUT)
  val pc = Bits(SZ_ADDR, OUTPUT)
  val vlen = Bits(SZ_VLEN, OUTPUT)
  val nxregs = Bits(SZ_REGCNT, OUTPUT)
  val imm1_rtag = Bits(SZ_IRB_IMM1, OUTPUT)
}

class io_qstall extends Bundle
{
  val vaq = Bool()
  val vldq = Bool()
  val vsdq = Bool()
}

class io_vxu_mem_cmd extends Bundle
{
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val typ_float = Bits(width = 1)
}

class io_vaq_bundle extends Bundle
{
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val typ_float = Bits(width = 1)
  val idx = Bits(width = PGIDX_BITS)
  val ppn = Bits(width = PPN_BITS)
}

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

class io_vxu_seq_fu extends Bundle
{
  val viu = Bool()
  val vau0 = Bool()
  val vau1 = Bool()
  val vau2 = Bool()
  val vaq = Bool()
  val vldq = Bool()
  val vsdq = Bool()
}

class io_vxu_seq_fn extends Bundle
{
  val viu = Bits(width = SZ_VIU_FN)
  val vau0 = Bits(width = SZ_VAU0_FN)
  val vau1 = Bits(width = SZ_VAU1_FN)
  val vau2 = Bits(width = SZ_VAU2_FN)
}

class io_vxu_seq_regid_imm extends Bundle
{
  val cnt = Bits(width = SZ_BVLEN)
  val utidx = Bits(width = SZ_VLEN)
  val vs_zero = Bool()
  val vt_zero = Bool()
  val vr_zero = Bool()
  val vs = Bits(width = SZ_BREGLEN)
  val vt = Bits(width = SZ_BREGLEN)
  val vr = Bits(width = SZ_BREGLEN)
  val vd = Bits(width = SZ_BREGLEN)
  val qcnt = UFix(width = 5)
  val mem = new io_vxu_mem_cmd()
  val imm = Bits(width = SZ_DATA)
  val imm2 = Bits(width = SZ_XIMM2)
  val utmemop = Bool()
  val irb = new io_vxu_irb_bundle()
}

class io_vxu_expand_read extends Bundle
{
  val ren = Bool()
  val rlast = Bool()
  val rcnt = Bits(width = SZ_BVLEN)
  val raddr = Bits(width = SZ_BREGLEN)
  val roplen = Bits(width = SZ_BOPL)
  val rblen = Bits(width = SZ_BRPORT)
}

class io_vxu_expand_write extends Bundle
{
  val wen = Bool()
  val wlast = Bool()
  val wcnt = Bits(width = SZ_BVLEN)
  val waddr = Bits(width = SZ_BREGLEN)
  val wsel = Bits(width = SZ_BWPORT)
}

class io_vxu_expand_fu_fn extends Bundle
{
  val viu = Bool()
  val viu_fn = Bits(width = SZ_VIU_FN)
  val viu_utidx = Bits(width = SZ_VLEN)
  val viu_imm = Bits(width = SZ_DATA)
}

class io_vxu_expand_lfu_fn extends Bundle
{
  val vau0 = Bool()
  val vau0_fn = Bits(width = SZ_VAU0_FN)
  val vau1 = Bool()
  val vau1_fn = Bits(width = SZ_VAU1_FN)
  val vau2 = Bool()
  val vau2_fn = Bits(width = SZ_VAU2_FN)
  val mem = new io_vxu_mem_cmd()
  val imm = Bits(width = SZ_DATA)
  val imm2 = Bits(width = SZ_XIMM2)
  val vaq = Bool()
  val vldq = Bool()
  val vsdq = Bool()
  val utmemop = Bool()
}

class io_vxu_to_vmu extends Bundle
{
  val qcnt = UFix(width = 5)
}

class io_vxu_issue_to_hazard extends Bundle
{
  val bcnt = Bits(width = SZ_BCNT)
}

class io_vxu_hazard_to_issue extends Bundle
{
  val pending_memop = Bool()
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

class io_vxu_seq_to_hazard extends Bundle
{
  val stall = Bits(width = SZ_STALL)
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

class io_issue_to_irb extends Bundle
{
  val markLast = Bool(OUTPUT)
}

class io_irb_to_issue extends Bundle 
{
  val imm1_rtag = Bits(SZ_IRB_IMM1, OUTPUT)
  val cnt_rtag = Bits(SZ_IRB_CNT, OUTPUT)
}

class io_vxu_irb_bundle extends Bundle
{
  val imm1_rtag = Bits(SZ_IRB_IMM1, OUTPUT)
  val cnt_rtag = Bits(SZ_IRB_CNT, OUTPUT)
  val pc_next = Bits(SZ_ADDR, OUTPUT)
  val update_imm1 = Bool(OUTPUT)
}

class io_seq_to_irb extends Bundle
{
  val last = Bool(OUTPUT)
  val update_imm1 = new io_valid()( new io_irbUpdateReq(SZ_VIMM, 3) )
  val update_cnt = new io_valid()( new io_irbUpdateReq(SZ_VLEN, 3) )
}


class io_vxu_issue_tvec extends Bundle
{
  val vf = new io_vf()

  val active = Bool(OUTPUT)

  val issue_to_hazard = new io_vxu_issue_to_hazard().asOutput
  val issue_to_seq = new io_vxu_issue_to_seq().asOutput
  val issue_to_lane = new io_vxu_issue_to_lane().asOutput
  val hazard_to_issue = new io_vxu_hazard_to_issue().asInput

  val vec_ackq = new io_vec_ackq

  val vxu_cmdq = new io_vxu_cmdq().flip()
  val vxu_immq = new io_vxu_immq().flip()
  val vxu_imm2q = new io_vxu_imm2q().flip()
  
  val valid = new io_vxu_issue_fire().asOutput
  val ready = Bool(INPUT)
  val dhazard = new io_vxu_issue_reg().asOutput
  val shazard = new io_vxu_issue_fu().asOutput
  val bhazard = new io_vxu_issue_op().asOutput
  val fn = new io_vxu_issue_fn().asOutput
  val decoded = new io_vxu_issue_regid_imm().asOutput

  val pending_store = Bool(INPUT)

  val irb_cmdb = new io_vxu_cmdq()
  val irb_imm1b = new io_vxu_immq()
  val irb_imm2b = new io_vxu_imm2q()
  val irb_cntb = new io_vxu_cntq()
  val irb_to_issue = new io_irb_to_issue().flip()

  val cpu_exception = new io_cpu_exception().flip()
}

class io_vxu_issue_vt extends Bundle
{
  val illegal = Bool(OUTPUT)

  val imem_req = new io_imem_req()
  val imem_resp = new io_imem_resp().flip()

  val vf = new io_vf().flip()

  val valid = new io_vxu_issue_fire().asOutput
  val ready = Bool(INPUT)
  val dhazard = new io_vxu_issue_reg().asOutput
  val shazard = new io_vxu_issue_fu().asOutput
  val bhazard = new io_vxu_issue_op().asOutput
  val fn = new io_vxu_issue_fn().asOutput
  val decoded = new io_vxu_issue_regid_imm().asOutput

  val irb_cntb = new io_vxu_cntq()

  val issue_to_irb = new io_issue_to_irb()
  val irb_to_issue = new io_irb_to_issue().flip()

  val cpu_exception = new io_cpu_exception().flip()
}

class ioDTLB_CPU_req_bundle extends Bundle
{
  // lookup requests
  val kill  = Bool()
  val cmd  = Bits(width=4) // load/store/amo
  val asid = Bits(width=ASID_BITS)
  val vpn  = UFix(width=VPN_BITS+1)
}
class ioDTLB_CPU_req extends io_ready_valid()( { new ioDTLB_CPU_req_bundle() } )

class ioDTLB_CPU_resp extends Bundle
{
  // lookup responses
  val miss = Bool(OUTPUT)
  val ppn = UFix(PPN_BITS, OUTPUT)
  val xcpt_ld = Bool(OUTPUT)
  val xcpt_st = Bool(OUTPUT)
}

class io_vu extends Bundle 
{
  val illegal = Bool(OUTPUT)

  val vec_cmdq = new io_vec_cmdq().flip()
  val vec_ximm1q = new io_vec_ximm1q().flip()
  val vec_ximm2q = new io_vec_ximm2q().flip()
  val vec_cntq = new io_vec_cntq().flip()
  val vec_ackq = new io_vec_ackq()

  val vec_pfcmdq = new io_vec_cmdq().flip()
  val vec_pfximm1q = new io_vec_ximm1q().flip()
  val vec_pfximm2q = new io_vec_ximm2q().flip()

  val cp_imul_req = new io_imul_req().flip()
  val cp_imul_resp = Bits(SZ_XLEN, OUTPUT)
  
  val imem_req = new io_imem_req()
  val imem_resp = new io_imem_resp().flip()

  val dmem_req = new io_dmem_req()
  val dmem_resp = new io_dmem_resp().flip()

  val cpu_exception = new io_cpu_exception().flip()

  val vec_tlb_req = new ioDTLB_CPU_req()
  val vec_tlb_resp = new ioDTLB_CPU_resp()

  val vec_pftlb_req = new ioDTLB_CPU_req()
  val vec_pftlb_resp = new ioDTLB_CPU_resp()
}

class io_vxu extends Bundle
{
  val illegal = Bool(OUTPUT)

  val vxu_cmdq = new io_vxu_cmdq().flip()
  val vxu_immq = new io_vxu_immq().flip()
  val vxu_imm2q = new io_vxu_imm2q().flip()
  val vxu_cntq = new io_vxu_cntq().flip()

  val vec_ackq = new io_vec_ackq

  val cp_imul_req = new io_imul_req().flip()
  val cp_imul_resp = Bits(SZ_XLEN, OUTPUT)

  val imem_req = new io_imem_req()
  val imem_resp = new io_imem_resp().flip()

  val lane_vaq = new io_lane_vaq()
  val lane_vldq = new io_lane_vldq().flip()
  val lane_vsdq = new io_lane_vsdq()

  val lane_vaq_dec = Bool(OUTPUT)
  val lane_vsdq_dec = Bool(OUTPUT)

  val qcnt = UFix(5, OUTPUT)
  
  val pending_store = Bool(INPUT)

  val irb_cmdb = new io_vxu_cmdq()
  val irb_imm1b = new io_vxu_immq()
  val irb_imm2b = new io_vxu_imm2q()
  val irb_cntb = new io_vxu_cntq()

  val issue_to_irb = new io_issue_to_irb()
  val irb_to_issue = new io_irb_to_issue().flip()

  val seq_to_irb = new io_seq_to_irb()

  val cpu_exception = new io_cpu_exception().flip()
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

class io_vxu_fire extends Bundle
{
  val tvec_valid = new io_vxu_issue_fire().asInput
  val tvec_ready = Bool(INPUT)
  val tvec_dhazard = new io_vxu_issue_reg().asInput
  val tvec_shazard = new io_vxu_issue_fu().asInput
  val tvec_bhazard = new io_vxu_issue_op().asInput
  val tvec_fn = new io_vxu_issue_fn().asInput
  val tvec_regid_imm = new io_vxu_issue_regid_imm().asInput

  val vt_valid = new io_vxu_issue_fire().asInput
  val vt_ready = Bool(INPUT)
  val vt_dhazard = new io_vxu_issue_reg().asInput
  val vt_shazard = new io_vxu_issue_fu().asInput
  val vt_bhazard = new io_vxu_issue_op().asInput
  val vt_fn = new io_vxu_issue_fn().asInput
  val vt_regid_imm = new io_vxu_issue_regid_imm().asInput

  val fire = new io_vxu_issue_fire().asOutput
  val fire_fn = new io_vxu_issue_fn().asOutput
  val fire_regid_imm = new io_vxu_issue_regid_imm().asOutput
}

class io_vxu_hazard extends Bundle
{
  val hazard_to_issue = new io_vxu_hazard_to_issue().asOutput
  val issue_to_hazard = new io_vxu_issue_to_hazard().asInput
  val seq_to_hazard = new io_vxu_seq_to_hazard().asInput
  val expand_to_hazard = new io_vxu_expand_to_hazard().asInput
  val lane_to_hazard = new io_lane_to_hazard().asInput

  val tvec_valid = new io_vxu_issue_fire().asInput
  val tvec_ready = Bool(OUTPUT)
  val tvec_dhazard = new io_vxu_issue_reg().asInput
  val tvec_shazard = new io_vxu_issue_fu().asInput
  val tvec_bhazard = new io_vxu_issue_op().asInput
  val tvec_fn = new io_vxu_issue_fn().asInput
  val tvec_regid_imm = new io_vxu_issue_regid_imm().asInput

  val vt_valid = new io_vxu_issue_fire().asInput
  val vt_ready = Bool(OUTPUT)
  val vt_dhazard = new io_vxu_issue_reg().asInput
  val vt_shazard = new io_vxu_issue_fu().asInput
  val vt_bhazard = new io_vxu_issue_op().asInput
  val vt_fn = new io_vxu_issue_fn().asInput
  val vt_regid_imm = new io_vxu_issue_regid_imm().asInput

  val fire = new io_vxu_issue_fire().asInput
  val fire_fn = new io_vxu_issue_fn().asInput
  val fire_regid_imm = new io_vxu_issue_regid_imm().asInput
}

class io_vxu_seq extends Bundle
{
  val issue_to_seq = new io_vxu_issue_to_seq().asInput
  val seq_to_hazard = new io_vxu_seq_to_hazard().asOutput
  val seq_to_expand = new io_vxu_seq_to_expand().asOutput

  val qstall = new io_qstall().asInput

  val fire = new io_vxu_issue_fire().asInput
  val fire_fn = new io_vxu_issue_fn().asInput
  val fire_regid_imm = new io_vxu_issue_regid_imm().asInput

  val seq = new io_vxu_seq_fu().asOutput
  val seq_fn = new io_vxu_seq_fn().asOutput
  val seq_regid_imm = new io_vxu_seq_regid_imm().asOutput

  val seq_to_irb = new io_seq_to_irb()

  val cpu_exception = new io_cpu_exception().flip()  
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
}

class io_vxu_mem extends Bundle
{
  val lane_vaq_valid = Bool(INPUT)
  val lane_vaq_mem = new io_vxu_mem_cmd().asInput
  val lane_vaq_imm = Bits(SZ_DATA, INPUT)
  val lane_vaq_utmemop = Bool(INPUT)
  val lane_vaq_rf = Bits(SZ_DATA, INPUT)
  
  val vmu_vaq_valid = Bool(OUTPUT)
  val vmu_vaq_bits = new io_vaq_bundle().asOutput
  
  val lane_vsdq_valid = Bool(INPUT)
  val lane_vsdq_mem = new io_vxu_mem_cmd().asInput 
  val lane_vsdq_bits = Bits(SZ_DATA, INPUT) 
  
  val vmu_vsdq_valid = Bool(OUTPUT)
  val vmu_vsdq_bits = Bits(SZ_DATA, OUTPUT) 
}

class io_vu_memif extends Bundle
{
  val vaq_deq = new io_ready_valid()({ new io_vaq_bundle() }).flip()
  val vaq_ack = Bool(OUTPUT)
  val vaq_nack = Bool(OUTPUT)

  val vsdq_deq = new io_ready_valid()({ Bits(width = 65) }).flip()
  val vsdq_ack = Bool(OUTPUT)
  val vsdq_nack = Bool(OUTPUT)

  val vldq_deq_rtag = new io_ready_valid()({ Bits(width = 8) }).flip()
  val vldq_ack = Bool(OUTPUT)
  val vldq_nack = Bool(OUTPUT)
  val vldq_enq = new io_ready_valid()({ new io_queue_reorder_qcnt_enq_bundle(65, 8) })

  val mem_req = new io_dmem_req()
  val mem_resp = new io_dmem_resp().flip()
}

class io_vru extends Bundle
{
  val vpfaq = new io_ready_valid()({ new io_vaq_bundle() })
  
  // command
  val vec_pfcmdq = new io_vec_cmdq().flip()
  // base
  val vec_pfximm1q = new io_vec_ximm1q().flip()
  // stride
  val vec_pfximm2q = new io_vec_ximm2q().flip()
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


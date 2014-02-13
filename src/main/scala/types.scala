package hwacha

import Chisel._
import Node._
import Constants._
import uncore.constants.AddressConstants._
import uncore.constants.MemoryOpConstants._

class HwachaCommand extends Bundle
{
  val cmcode = Bits(width = 8)
  val vd = UInt(width = 6)
  val vt = UInt(width = 6)
}

class HwachaImm1 extends Bundle
{
  val prec = Bits(width = 2)
  val xf_split = UInt(width = SZ_BREGLEN) 
  val bcnt = UInt(width = SZ_BCNT)
  val bactive = Bits(width = SZ_BANK)
  val nfregs = UInt(width = SZ_REGCNT)
  val nxregs = UInt(width = SZ_REGCNT)
  val vlen = UInt(width = SZ_VLEN)
}

class VIUFn extends Bundle
{
  val t0 = Bits(width = SZ_VIU_T0)
  val t1 = Bits(width = SZ_VIU_T0)
  val dw = Bits(width = SZ_DW)
  val fp = Bits(width = SZ_FP)
  val op = Bits(width = SZ_VIU_OP)
}

class VAU0Fn extends Bundle
{
  val dw = Bits(width = SZ_DW)
  val op = Bits(width = SZ_VAU0_OP)
}

class VAU1Fn extends Bundle
{
  val fp = Bits(width = SZ_FP)
  val rm = Bits(width = rocket.FPConstants.RM_SZ)
  val op = Bits(width = SZ_VAU1_OP)
}

class VAU2Fn extends Bundle
{
  val fp = Bits(width = SZ_FP)
  val rm = Bits(width = rocket.FPConstants.RM_SZ)
  val op = Bits(width = SZ_VAU2_OP)
}

class VMUFn extends Bundle
{
  val float = Bool()
  val typ = Bits(width = MT_SZ)
  val cmd = Bits(width = M_SZ)
  val op = Bits(width = SZ_VMU_OP)
}

class LaneOpBundle extends Bundle
{
  val cnt = Bits(width = SZ_BCNT)
}

class ReadBankOp extends LaneOpBundle
{
  val last = Bool()
  val addr = Bits(width = SZ_BREGLEN)
  val oplen = Bits(width = SZ_BOPL)
  val rblen = Vec.fill(SZ_BRPORT){Bool()}
}

class WriteBankOp extends LaneOpBundle
{
  val last = Bool()
  val addr = Bits(width = SZ_BREGLEN)
  val sel = Bits(width = SZ_BWPORT)
}

class VIUBankOp extends LaneOpBundle
{
  val fn = Bits(width = SZ_VIU_FN)
  val utidx = Bits(width = SZ_VLEN)
  val imm = Bits(width = SZ_DATA)
}

class VAU0LaneFUOp extends LaneOpBundle
{
  val fn = Bits(width = SZ_VAU0_FN)
}

class VAU1LaneFUOp extends LaneOpBundle
{
  val fn = Bits(width = SZ_VAU1_FN)
}

class VAU2LaneFUOp extends LaneOpBundle
{
  val fn = Bits(width = SZ_VAU2_FN)
}

class VGULaneFUOp extends LaneOpBundle
{
  val mem = new io_vxu_mem_cmd()
  val imm = Bits(width = SZ_DATA)
  val imm2 = Bits(width = SZ_XIMM2)
  val utmemop = Bool()
}

class VLULaneFUOp extends LaneOpBundle

class VSULaneFUOp extends LaneOpBundle
{
  val mem = new io_vxu_mem_cmd()
}

class io_vxu_cmdq extends DecoupledIO(Bits(width = SZ_XCMD))
class io_vxu_immq extends DecoupledIO(Bits(width = SZ_XIMM))
class io_vxu_imm2q extends DecoupledIO(Bits(width = SZ_XIMM2))
class io_vxu_cntq extends DecoupledIO(Bits(width = SZ_VLEN))
class io_vxu_numcntq extends DecoupledIO(Bits(width = 1))

class io_vvaq extends DecoupledIO(new io_vvaq_bundle)
class io_vpaq extends DecoupledIO(new io_vpaq_bundle)
class io_vldq extends DecoupledIO(Bits(width = SZ_DATA))
class io_vsdq extends DecoupledIO(Bits(width = SZ_DATA))

class io_update_num_cnt extends ValidIO(Bits(width=SZ_AIW_NUMCNT))

class io_cpu_exception extends Bundle 
{
  val supervisor_mode = Bool(OUTPUT)
  val exception = Bool(OUTPUT)
  val kill = Bool(OUTPUT)
  val hold = Bool(OUTPUT)
  val addr = UInt(OUTPUT, SZ_ADDR)
}

class io_aiwUpdateReq(DATA_SIZE: Int, ADDR_SIZE: Int) extends Bundle 
{
  val data = Bits(width=DATA_SIZE)
  val addr = UInt(width=ADDR_SIZE)
  override def clone = new io_aiwUpdateReq(DATA_SIZE, ADDR_SIZE).asInstanceOf[this.type]
}

class io_qstall extends Bundle
{
  val vaq = Bool()
  val vldq = Bool()
  val vsdq = Bool()
}

class io_vxu_mem_check extends Bundle
{
  val checkcnt = Bool()
  val cnt = UInt(width = 4)
}

class io_vxu_mem_cmd extends Bundle
{
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val typ_float = Bool()
}

class io_vvaq_bundle extends Bundle
{
  val checkcnt = Bool()
  val cnt = UInt(width = 4)
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val typ_float = Bool()
  val idx = Bits(width = PGIDX_BITS)
  val vpn = Bits(width = VPN_BITS)
}

class io_vpaq_bundle extends Bundle
{
  val checkcnt = Bool()
  val cnt = UInt(width = 4)
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val typ_float = Bool()
  val addr = Bits(width = PADDR_BITS)
}

class io_vxu_aiw_bundle extends Bundle
{
  val imm1_rtag = Bits(OUTPUT, SZ_AIW_IMM1)
  val numCnt_rtag = Bits(OUTPUT, SZ_AIW_CMD)
  val cnt_rtag = Bits(OUTPUT, SZ_AIW_CNT)
  val pc_next = Bits(OUTPUT, SZ_ADDR)
  val update_imm1 = Bool(OUTPUT)
}

class TLBIO extends Bundle
{
  val req = Decoupled(new rocket.TLBReq)
  val resp = new rocket.TLBResp(1).flip // we don't use hit_idx
}

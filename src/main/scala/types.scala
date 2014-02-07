package hwacha

import Chisel._
import Node._
import Constants._
import uncore.constants.AddressConstants._

class HwachaCommand extends Bundle
{
  val cmcode = Bits(width = 8)
  val vd = UInt(width = 6)
  val vt = UInt(width = 6)
}

class HwachaImm1 extends Bundle with
  MachineConstants with
  LaneConstants
{
  val prec = Bits(width = 2)
  val xf_split = UInt(width = SZ_BREGLEN) 
  val bcnt = UInt(width = SZ_BCNT)
  val bactive = Bits(width = SZ_BANK)
  val nfregs = UInt(width = SZ_REGCNT)
  val nxregs = UInt(width = SZ_REGCNT)
  val vlen = UInt(width = SZ_VLEN)
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

package hwacha

import Chisel._
import Node._
import Constants._

class io_imem_resp_bundle extends Bundle
{
  val pc = UFix(width = VADDR_BITS+1)
  val data = Bits(width = SZ_INST)
  val xcpt_ma = Bool()
  val xcpt_if = Bool()
}

class io_imem_req extends PipeIO()( { UFix(width = VADDR_BITS+1) } )
class io_imem_resp extends FIFOIO()( { new io_imem_resp_bundle() } )

class io_vxu_cmdq extends FIFOIO()( { Bits(width = SZ_XCMD) } )
class io_vxu_immq extends FIFOIO()( { Bits(width = SZ_XIMM) } )
class io_vxu_imm2q extends FIFOIO()( { Bits(width = SZ_XIMM2) } )
class io_vxu_cntq extends FIFOIO()( Bits(width = SZ_VLEN) )
class io_vxu_numcntq extends FIFOIO()( Bits(width = 1) )
class io_vxu_ackq extends FIFOIO()( { Bits(width = SZ_XRESP) } )

class io_vec_cmdq extends FIFOIO()( { Bits(width = SZ_VCMD) } )
class io_vec_ximm1q extends FIFOIO()( { Bits(width = SZ_VIMM) } )
class io_vec_ximm2q extends FIFOIO()( { Bits(width = SZ_VSTRIDE) } )
class io_vec_cntq() extends FIFOIO()( Bits(width=SZ_VLEN+1) )
class io_vec_ackq extends FIFOIO()( { Bits(width = SZ_VRESP) } )

class io_vvaq extends FIFOIO()( { new io_vvaq_bundle() } )
class io_vpaq extends FIFOIO()( { new io_vpaq_bundle() } )
class io_vldq extends FIFOIO()( { Bits(width = SZ_DATA) } )
class io_vsdq extends FIFOIO()( { Bits(width = SZ_DATA) } )

class io_update_num_cnt extends PipeIO()( { Bits(width=SZ_AIW_NUMCNT) } )

class io_cpu_exception extends Bundle 
{
  val supervisor_mode = Bool(OUTPUT)
  val exception = Bool(OUTPUT)
  val kill = Bool(OUTPUT)
  val hold = Bool(OUTPUT)
  val addr = UFix(OUTPUT, SZ_ADDR)
}

class io_imul_req_bundle extends Bundle
{
  val fn = Bits(width = SZ_VAU0_FN)
  val in0 = Bits(width = SZ_XLEN)
  val in1 = Bits(width = SZ_XLEN)
}

class io_imul_req extends FIFOIO()( { new io_imul_req_bundle() } )

class io_dmem_req_bundle extends Bundle
{
  val kill = Bool()
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val addr = UFix(width = PADDR_BITS)
  val phys = Bool()
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

class io_dmem_req extends FIFOIO()({ new io_dmem_req_bundle() })
class io_dmem_resp extends PipeIO()({ new io_dmem_resp_bundle() })

class io_aiwUpdateReq(DATA_SIZE: Int, ADDR_SIZE: Int) extends Bundle 
{
  val data = Bits(width=DATA_SIZE)
  val addr = UFix(width=ADDR_SIZE)
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
  val cnt = UFix(width = 4)
}

class io_vxu_mem_cmd extends Bundle
{
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val typ_float = Bits(width = 1)
}

class io_vvaq_bundle extends Bundle
{
  val checkcnt = Bool()
  val cnt = UFix(width = 4)
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val typ_float = Bits(width = 1)
  val idx = Bits(width = PGIDX_BITS)
  val vpn = Bits(width = VPN_BITS)
}

class io_vpaq_bundle extends Bundle
{
  val checkcnt = Bool()
  val cnt = UFix(width = 4)
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val typ_float = Bits(width = 1)
  val addr = Bits(width = PADDR_BITS)
}

class io_vxu_to_vmu extends Bundle
{
  val qcnt = UFix(width = SZ_QCNT)
}

class io_vxu_aiw_bundle extends Bundle
{
  val imm1_rtag = Bits(OUTPUT, SZ_AIW_IMM1)
  val numCnt_rtag = Bits(OUTPUT, SZ_AIW_CMD)
  val cnt_rtag = Bits(OUTPUT, SZ_AIW_CNT)
  val pc_next = Bits(OUTPUT, SZ_ADDR)
  val update_imm1 = Bool(OUTPUT)
}

class TLBReq extends Bundle
{
  val asid = UFix(width = ASID_BITS)
  val vpn = UFix(width = VPN_BITS+1)
  val passthrough = Bool()
  val instruction = Bool()
}

class TLBResp extends Bundle
{
  val miss = Bool(OUTPUT)
  val ppn = UFix(OUTPUT, PPN_BITS)
  val xcpt_ld = Bool(OUTPUT)
  val xcpt_st = Bool(OUTPUT)
}

class io_tlb extends Bundle
{
  val req = new FIFOIO()(new TLBReq)
  val resp = new TLBResp().flip
}

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

  def rtype(dummy: Int = 0) = t0 === ML && t1 === MR
  def itype(dummy: Int = 0) = t0 === M0 && t1 === MR
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

  def fma(dummy: Int = 0) = IS_A1_OP_FMA(op)
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

  def utmemop(dummy: Int = 0) = IS_VM_OP_UTMEMOP(op)
}

class LaneOp extends Bundle
{
  val cnt = Bits(width = SZ_BCNT)
}

class ReadBankOp extends LaneOp
{
  val last = Bool()
  val addr = Bits(width = SZ_BREGLEN)
  val oplen = Bits(width = SZ_BOPL)
  val rblen = Vec.fill(SZ_BRPORT){Bool()}
}

class WriteBankOp extends LaneOp
{
  val last = Bool()
  val addr = Bits(width = SZ_BREGLEN)
  val sel = Bits(width = SZ_BWPORT)
}

class VIUOp extends LaneOp
{
  val fn = new VIUFn
  val utidx = Bits(width = SZ_VLEN)
  val imm = Bits(width = SZ_DATA)
}

class VAU0Op extends LaneOp
{
  val fn = new VAU0Fn
}

class VAU1Op extends LaneOp
{
  val fn = new VAU1Fn
}

class VAU2Op extends LaneOp
{
  val fn = new VAU2Fn
}

class VGUOp extends LaneOp
{
  val fn = new VMUFn
  val base = Bits(width = SZ_DATA)
  val stride = Bits(width = SZ_XIMM2)
  val check = new io_vxu_mem_check
}

class VLUOp extends LaneOp
{
  val fn = new VMUFn
}

class VSUOp extends LaneOp
{
  val fn = new VMUFn
}

class RegInfo extends Bundle
{
  val zero = Bool()
  val float = Bool()
  val id = Bits(width = SZ_BREGLEN)
}

class DecodedRegister extends Bundle
{
  val vs = new RegInfo
  val vt = new RegInfo
  val vr = new RegInfo
  val vd = new RegInfo
}

class DecodedImmediate extends Bundle
{
  val imm = Bits(width = SZ_DATA)
  val stride = Bits(width = SZ_XIMM2)
}

class DecodedInstruction extends Bundle
{
  val utidx = UInt(width = SZ_VLEN)
  val fn = new Bundle {
    val viu = new VIUFn
    val vau0 = new VAU0Fn
    val vau1 = new VAU1Fn
    val vau2 = new VAU2Fn
    val vmu = new VMUFn
  }
  val reg = new DecodedRegister
  val imm = new DecodedImmediate
}

class SequencerEntry extends DecodedInstruction
{
  val active = new Bundle {
    val viu = Bool()
    val vau0 = Bool()
    val vau1 = Bool()
    val vau2 = Bool()
    val vgu = Bool()
    val vlu = Bool()
    val vsu = Bool()
  }
}

class SequencerOp extends SequencerEntry
{
  val cnt = Bits(width = SZ_BCNT)
  val last = Bool()
}

class AIWImm1Entry extends Bundle
{
  val rtag = Bits(width = SZ_AIW_IMM1)
  val pc_next = Bits(width = SZ_ADDR)
}

class AIWImm1Op extends AIWImm1Entry
{
  val base = Bits(width = SZ_VIMM)
  val ldst = Bool()
}

class AIWCntEntry extends Bundle
{
  val rtag = Bits(width = SZ_AIW_CNT)
  val utidx = UInt(width = SZ_VLEN)
}

class AIWCntOp extends AIWCntEntry

class AIWNumCntEntry extends Bundle
{
  val rtag = Bits(width = SZ_AIW_NUMCNT)
}

class AIWNumCntOp extends AIWNumCntEntry
{
  val last = Bool()
}

class AIWEntry extends Bundle
{
  val active = new Bundle {
    val imm1 = Bool()
    val cnt = Bool()
  }
  val imm1 = new AIWImm1Entry
  val cnt = new AIWCntEntry
  val numcnt = new AIWNumCntEntry
}

class IssueOp extends DecodedInstruction
{
  val vlen = UInt(width = SZ_VLEN)
  val active = new Bundle {
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
  val aiw = new AIWEntry
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

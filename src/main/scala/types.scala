package hwacha

import Chisel._
import Node._
import Constants._
import uncore.constants.AddressConstants._
import uncore.constants.MemoryOpConstants._


//-------------------------------------------------------------------------\\
// vector command queue types
//-------------------------------------------------------------------------\\

class HwachaCommand extends Bundle
{
  val cmcode = Bits(width = 8)
  val vd = UInt(width = 5)
  val vt = UInt(width = 5)
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


//-------------------------------------------------------------------------\\
// vector functional unit fn types
//-------------------------------------------------------------------------\\

class VIUFn extends Bundle
{
  val t0 = Bits(width = SZ_BMUXSEL)
  val t1 = Bits(width = SZ_BMUXSEL)
  val dw = Bits(width = SZ_DW)
  val fp = Bits(width = SZ_FP)
  val op = Bits(width = SZ_VIU_OP)

  def rtype(dummy: Int = 0) = t0 === ML && t1 === MR
  def s2only(dummy: Int = 0) = t0 === M0 && t1 === MR
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


//-------------------------------------------------------------------------\\
// decoded information types
//-------------------------------------------------------------------------\\

class RegInfo extends Bundle
{
  val active = Bool()
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
  val stride = Bits(width = SZ_VSTRIDE)
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


//-------------------------------------------------------------------------\\
// aiw types
//-------------------------------------------------------------------------\\

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


//-------------------------------------------------------------------------\\
// issue op
//-------------------------------------------------------------------------\\

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


//-------------------------------------------------------------------------\\
// sequencer op
//-------------------------------------------------------------------------\\

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


//-------------------------------------------------------------------------\\
// bank, lane op
//-------------------------------------------------------------------------\\

class LaneOp extends Bundle
{
  val cnt = Bits(width = SZ_BCNT)
}

class ReadBankOp extends LaneOp
{
  val last = Bool()
  val addr = Bits(width = SZ_BREGLEN)
  val ren = Bool()
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
  val stride = Bits(width = SZ_VSTRIDE)
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

class io_vxu_cmdq extends DecoupledIO(Bits(width = SZ_VCMD))
class io_vxu_immq extends DecoupledIO(Bits(width = SZ_VIMM))
class io_vxu_imm2q extends DecoupledIO(Bits(width = SZ_VSTRIDE))
class io_vxu_cntq extends DecoupledIO(Bits(width = SZ_VLEN))
class io_vxu_numcntq extends DecoupledIO(Bits(width = 1))

class io_vvaq extends DecoupledIO(new io_vvaq_bundle)
class io_vpaq extends DecoupledIO(new io_vpaq_bundle)
class io_vldq extends DecoupledIO(Bits(width = SZ_DATA))
class io_vsdq extends DecoupledIO(Bits(width = SZ_DATA))

class io_update_num_cnt extends ValidIO(Bits(width=SZ_AIW_NUMCNT))

class io_aiwUpdateReq(DATA_SIZE: Int, ADDR_SIZE: Int) extends Bundle 
{
  val data = Bits(width=DATA_SIZE)
  val addr = UInt(width=ADDR_SIZE)
  override def clone = new io_aiwUpdateReq(DATA_SIZE, ADDR_SIZE).asInstanceOf[this.type]
}

class io_vxu_mem_check extends Bundle
{
  val checkcnt = Bool()
  val cnt = UInt(width = 4)
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

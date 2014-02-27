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

class HwachaCnt extends Bundle
{
  val cnt = UInt(width = SZ_VLEN)
  val last = Bool()
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
  val op = Bits(width = SZ_VMU_OP)

  def utmemop(dummy: Int = 0) = !vmu_op_tvec(op)
  def lreq(dummy: Int = 0) = is_mcmd_amo(vmu_op_mcmd(op)) || (op === VM_ULD) || (op === VM_VLD)
  def sreq(dummy: Int = 0) = is_mcmd_amo(vmu_op_mcmd(op)) || (op === VM_UST) || (op === VM_VST)
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
    val vmu = new vmunit.VMUFn
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
  val vmu = new Bundle {
    val op = new vmunit.VMUOp
    val stride = Bits(width = SZ_VSTRIDE)
  }
}


//-------------------------------------------------------------------------\\
// sequencer op
//-------------------------------------------------------------------------\\

class VFU extends Bundle // vector functional unit
{
  val viu = Bool()  // vector integer unit
  val vau0 = Bool() // vector arithmetic 0 unit; imul
  val vau1 = Bool() // vector arithmetic 1 unit; fma
  val vau2 = Bool() // vector arithmetic 2 unit; fconv
  val vgu = Bool()  // vector address generation unit
  val vcu = Bool()  // vector address check unit
  val vlu = Bool()  // vector load (data) unit
  val vsu = Bool()  // vector store (data) unit
}

class SequencerEntry extends DecodedInstruction
{
  val active = new VFU
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
  val addr = Bits(width = SZ_BREGLEN)
  val ren = Bool()
  val oplen = Bits(width = SZ_BOPL)
  val rblen = Vec.fill(SZ_BRPORT){Bool()}
  val brqen = Bool()
}

class WriteBankOp extends LaneOp
{
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
}

class VCUOp extends LaneOp
{
  val fn = new VMUFn
}

class VLUOp extends LaneOp
{
  val fn = new VMUFn
}

class VSUOp extends LaneOp
{
  val fn = new VMUFn
}


//-------------------------------------------------------------------------\\
// deck types
//-------------------------------------------------------------------------\\

class DeckOp extends Bundle
{
  val vlen = UInt(width = SZ_VLEN)
  val utidx = UInt(width = SZ_VLEN)
  val fn = new vmunit.VMUFn
  val reg = new DecodedRegister
}

class BRQEntry extends Bundle
{
  val data = Bits(width = SZ_DATA)
}

class BWQEntry extends Bundle
{
  val addr = UInt(width = SZ_BREGLEN)
  val data = Bits(width = SZ_DATA)
}

class BWQInternalEntry(implicit conf: HwachaConfiguration) extends BWQEntry
{
  val tag = UInt(width = log2Up(conf.nvlreq))
  override def clone = new BWQInternalEntry().asInstanceOf[this.type]
}

//-------------------------------------------------------------------------\\
// vmu types
//-------------------------------------------------------------------------\\

class VVAQEntry extends Bundle
{
  val cmd = Bits(width = M_SZ)
  val typ = Bits(width = MT_SZ)
  val idx = Bits(width = PGIDX_BITS)
  val vpn = Bits(width = VPN_BITS)
}

class VPAQEntry extends Bundle
{
  val cmd = Bits(width = M_SZ)
  val typ = Bits(width = MT_SZ)
  val addr = Bits(width = PADDR_BITS)
}


// aiw FIXME
class io_vxu_cmdq extends DecoupledIO(Bits(width = SZ_VCMD))
class io_vxu_immq extends DecoupledIO(Bits(width = SZ_VIMM))
class io_vxu_imm2q extends DecoupledIO(Bits(width = SZ_VSTRIDE))
class io_vxu_cntq extends DecoupledIO(Bits(width = SZ_VLEN))
class io_vxu_numcntq extends DecoupledIO(Bits(width = 1))

class io_update_num_cnt extends ValidIO(Bits(width=SZ_AIW_NUMCNT))

class io_aiwUpdateReq(DATA_SIZE: Int, ADDR_SIZE: Int) extends Bundle 
{
  val data = Bits(width=DATA_SIZE)
  val addr = UInt(width=ADDR_SIZE)
  override def clone = new io_aiwUpdateReq(DATA_SIZE, ADDR_SIZE).asInstanceOf[this.type]
}

package hwacha

import Chisel._
import Node._
import Constants._
import uncore.constants.MemoryOpConstants._

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

  def rtype(dummy: Int = 0) = t0 === ML
  def itype(dummy: Int = 0) = t0 === MR
  def rs1(dummy: Int = 0) = rtype() || itype()
  def rs2(dummy: Int = 0) = rtype()
  def wptr_sel(wptr0: Bits, wptr1: Bits, wptr2: Bits) =
    Mux(rtype(), wptr2, Mux(itype(), wptr1, wptr0)).toUInt
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

  def r4type(dummy: Int = 0) = IS_A1_OP_FMA(op)
  def wptr_sel(wptr2: Bits, wptr3: Bits) =
    Mux(r4type(), wptr3, wptr2).toUInt
}

class VAU2Fn extends Bundle
{
  val fp = Bits(width = SZ_FP)
  val rm = Bits(width = rocket.FPConstants.RM_SZ)
  val op = Bits(width = SZ_VAU2_OP)
}

class VMULaneFn extends VMUFn
{
  val float = Bool()
}


//-------------------------------------------------------------------------\\
// decoded information types
//-------------------------------------------------------------------------\\

class RegInfo extends Bundle
{
  val zero = Bool()
  val float = Bool()
  val id = Bits(width = SZ_BREGLEN)
  val prec = Bits(width = SZ_PREC)
}

class RegHazardInfo extends Bundle
{
  val active = Bool()
  val base = Bits(width = 5)
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
    val vmu = new VMULaneFn
  }
  val reg = new DecodedRegister
  val imm = new DecodedImmediate
}


//-------------------------------------------------------------------------\\
// aiw types
//-------------------------------------------------------------------------\\

class AIWUpdateImm1Entry extends Bundle
{
  val rtag = Bits(width = SZ_AIW_IMM1)
  val pc_next = Bits(width = SZ_ADDR)
}

class AIWUpdateImm1Op extends AIWUpdateImm1Entry
{
  val base = Bits(width = SZ_VIMM)
  val ldst = Bool()
}

class AIWUpdateCntEntry extends Bundle
{
  val rtag = Bits(width = SZ_AIW_CNT)
  val utidx = UInt(width = SZ_VLEN)
}

class AIWUpdateCntOp extends AIWUpdateCntEntry

class AIWUpdateNumCntEntry extends Bundle
{
  val rtag = Bits(width = SZ_AIW_NUMCNT)
}

class AIWUpdateNumCntOp extends AIWUpdateNumCntEntry
{
  val last = Bool()
}

class AIWUpdateEntry extends Bundle
{
  val active = new Bundle {
    val imm1 = Bool()
    val cnt = Bool()
  }
  val imm1 = new AIWUpdateImm1Entry
  val cnt = new AIWUpdateCntEntry
  val numcnt = new AIWUpdateNumCntEntry
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
  val sel = new Bundle {
    val vau1 = Bool() // once true, vau1t is scheduled
    val vau2 = Bool() // once true, vau2t is scheduled
  }
  val regcheck = new Bundle {
    val vs = new RegHazardInfo
    val vt = new RegHazardInfo
    val vr = new RegHazardInfo
    val vd = new RegHazardInfo
  }
  val aiw = new AIWUpdateEntry
}


//-------------------------------------------------------------------------\\
// sequencer op
//-------------------------------------------------------------------------\\

class VFU extends Bundle // vector functional unit
{
  val viu = Bool()  // vector integer unit
  val vau0 = Bool() // vector arithmetic 0 unit; imul
  val vau1t = Bool() // vector arithmetic 1 unit; fma0
  val vau1f = Bool() // vector arithmetic 1 unit; fma1
  val vau2t = Bool() // vector arithmetic 2 unit; fconv0
  val vau2f = Bool() // vector arithmetic 2 unit; fconv1
  val vgu = Bool()  // vector address generation unit
  val vcu = Bool()  // vector address check unit
  val vlu = Bool()  // vector load (data) unit
  val vsu = Bool()  // vector store (data) unit
}

class SequencerEntry extends DecodedInstruction
{
  val active = new VFU
  val turbo_capable = Bool()
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
  val fn = new VMULaneFn
  val base = Bits(width = SZ_DATA)
}

class VCUOp extends LaneOp
{
  val fn = new VMULaneFn
}

class VLUOp extends LaneOp
{
  val fn = new VMULaneFn
}

class VSUOp extends LaneOp
{
  val fn = new VMULaneFn
}


//-------------------------------------------------------------------------\\
// deck types
//-------------------------------------------------------------------------\\

class DeckOp extends Bundle
{
  val vlen = UInt(width = SZ_VLEN)
  val utidx = UInt(width = SZ_VLEN)
  val fn = new VMULaneFn
  val reg = new DecodedRegister
}

class BRQEntry extends Bundle
{
  val data = Bits(width = SZ_DATA)
}

class BWQEntry extends HwachaBundle
{
  val addr = UInt(width = SZ_BREGLEN)
  val mask = Bits(width = SZ_BREGMASK)
  val data = Bits(width = SZ_DATA)
}

class BWQInternalEntry extends HwachaBundle
{
  // Lower utidx bits are evident from the bank ID and therefore omitted
  val tag = UInt(width = SZ_VLEN - log2Up(nbanks))
  val data = Bits(width = SZ_DATA)
}

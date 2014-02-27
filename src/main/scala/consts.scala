package hwacha

import Chisel._
import Node._
import uncore.constants.MemoryOpConstants._

object Constants extends
  MachineConstants with
  PrecConstants with
  VectorCommandQueueConstants with
  LaneConstants with
  AIWConstants with
  DecodeConstants with
  VIUConstants with
  VAU0Constants with
  VAU1Constants with
  VAU2Constants with
  VMUConstants

trait MachineConstants
{
  val SZ_VLEN = 12
  val SZ_REGLEN = 6
  val SZ_REGCNT = 6

  val SZ_ADDR = 64
  val SZ_DATA = 66
}

trait VectorCommandQueueConstants
{
  val SZ_VCMD = 18
  val SZ_VIMM = 64
  val SZ_VSTRIDE = 64
}

trait PrecConstants
{
  val SZ_PREC = 2
  val SZ_BUF_PREC = 3

  val PREC_DOUBLE = Bits("b00", SZ_PREC)
  val PREC_SINGLE = Bits("b01", SZ_PREC)
  val PREC_HALF = Bits("b10", SZ_PREC)
}

trait LaneConstants
{
  val SZ_BANK = 8
  val SZ_LGBANK = 3
  val SZ_LGBANK1 = 4
  val SZ_BPTR = SZ_LGBANK
  val SZ_BPTR1 = SZ_LGBANK+1
  val SZ_BPTR2 = SZ_LGBANK+2
  val SZ_BCNT = SZ_LGBANK+1
  val SZ_BREGLEN = 8
  val SZ_BOPL = 3
  val SZ_BRPORT = SZ_BANK
  val SZ_BWPORT = 3
}

trait AIWConstants
{
  val AIW_CMD_DEPTH = 8
  val AIW_IMM1_DEPTH = 8
  val AIW_IMM2_DEPTH = 8
  val AIW_CNT_DEPTH = 8
  val AIW_NUMCNT_DEPTH = AIW_CMD_DEPTH

  val SZ_AIW_CMD = log2Up(AIW_CMD_DEPTH)
  val SZ_AIW_IMM1 = log2Up(AIW_IMM1_DEPTH)
  val SZ_AIW_IMM2E = log2Up(AIW_IMM2_DEPTH)
  val SZ_AIW_CNT = log2Up(AIW_IMM2_DEPTH)
  val SZ_AIW_NUMCNT = log2Up(AIW_NUMCNT_DEPTH)
}

trait DecodeConstants
{
  val T = Bool(true)
  val F = Bool(false)

  val M0 = Bits("b00", 2)
  val MR = Bits("b01", 2)
  val ML = Bits("b10", 2)
  val MI = Bits("b11", 2)

  val R_ = Bits("b?0", 2)
  val RX = Bits("b01", 2)
  val RF = Bits("b11", 2)
  def parse_rinfo(x: Bits) = (0 until x.getWidth).map(x(_).toBool).toList

  val SZ_I = 2
  val IMM_X = UInt.DC(SZ_I)
  val IMM_0 = UInt(0, SZ_I)
  val IMM_I = UInt(1, SZ_I)
  val IMM_S = UInt(2, SZ_I)
  val IMM_U = UInt(3, SZ_I)

  val DW__ = Bool.DC
  val DW32 = Bits("b0", 1)
  val DW64 = Bits("b1", 1)

  val FP_ = Bits.DC(2)
  val FPS = Bits("b00", 2)
  val FPD = Bits("b01", 2)
  val FPH = Bits("b10", 2)

  val SZ_BMUXSEL = 2
  val SZ_DW = 1
  val SZ_FP = 2
}

trait VIUConstants
{
  val SZ_VIU_OP = 5

  val I_X    = UInt.DC(SZ_VIU_OP)
  val I_ADD  = UInt(0, SZ_VIU_OP)
  val I_SLL  = UInt(1, SZ_VIU_OP)
  val I_SLT  = UInt(2, SZ_VIU_OP)
  val I_SLTU = UInt(3, SZ_VIU_OP)
  val I_XOR  = UInt(4, SZ_VIU_OP)
  val I_SRL  = UInt(5, SZ_VIU_OP)
  val I_SRA  = UInt(6, SZ_VIU_OP)
  val I_OR   = UInt(7, SZ_VIU_OP)
  val I_AND  = UInt(8, SZ_VIU_OP)
  val I_SUB  = UInt(9, SZ_VIU_OP)
  val I_IDX  = UInt(10, SZ_VIU_OP)
  val I_MOV1 = UInt(11, SZ_VIU_OP)
  val I_MOV2 = UInt(12, SZ_VIU_OP)
  val I_FSJ  = UInt(13, SZ_VIU_OP)
  val I_FSJN = UInt(14, SZ_VIU_OP)
  val I_FSJX = UInt(15, SZ_VIU_OP)
  val I_FEQ  = UInt(16, SZ_VIU_OP)
  val I_FLT  = UInt(17, SZ_VIU_OP)
  val I_FLE  = UInt(18, SZ_VIU_OP)
  val I_FMIN = UInt(19, SZ_VIU_OP)
  val I_FMAX = UInt(20, SZ_VIU_OP)
  val I_MOVZ = UInt(21, SZ_VIU_OP)
  val I_MOVN = UInt(22, SZ_VIU_OP)
}

trait VAU0Constants extends DecodeConstants
{
  val SZ_VAU0_OP = 2

  val A0_X    = UInt.DC(SZ_VAU0_OP)
  val A0_M    = UInt(0, SZ_VAU0_OP)
  val A0_MH   = UInt(1, SZ_VAU0_OP)
  val A0_MHSU = UInt(2, SZ_VAU0_OP)
  val A0_MHU  = UInt(3, SZ_VAU0_OP)
}

trait VAU1Constants
{
  val SZ_VAU1_OP = 3

  val A1_X     = UInt.DC(SZ_VAU1_OP)
  val A1_ADD   = UInt(0, SZ_VAU1_OP)
  val A1_SUB   = UInt(1, SZ_VAU1_OP)
  val A1_MUL   = UInt(2, SZ_VAU1_OP)
  val A1_MADD  = UInt(4, SZ_VAU1_OP)
  val A1_MSUB  = UInt(5, SZ_VAU1_OP)
  val A1_NMSUB = UInt(6, SZ_VAU1_OP)
  val A1_NMADD = UInt(7, SZ_VAU1_OP)

  val IS_A1_OP_FMA = (x: Bits) => x(2)
}

trait VAU2Constants
{
  val SZ_VAU2_OP = 4

  val A2_X     = UInt.DC(SZ_VAU2_OP)
  val A2_CLTF  = UInt(0, SZ_VAU2_OP)
  val A2_CLUTF = UInt(1, SZ_VAU2_OP)
  val A2_CWTF  = UInt(2, SZ_VAU2_OP)
  val A2_CWUTF = UInt(3, SZ_VAU2_OP)
  val A2_MXTF  = UInt(4, SZ_VAU2_OP)
  val A2_CFTL  = UInt(5, SZ_VAU2_OP)
  val A2_CFTLU = UInt(6, SZ_VAU2_OP)
  val A2_CFTW  = UInt(7, SZ_VAU2_OP)
  val A2_CFTWU = UInt(8, SZ_VAU2_OP)
  val A2_MFTX  = UInt(9, SZ_VAU2_OP)
  val A2_CDTS  = UInt(10, SZ_VAU2_OP)
  val A2_CSTD  = UInt(11, SZ_VAU2_OP)
}

trait VMUConstants extends LaneConstants
{
  def is_mtype_byte(typ: UInt) = (typ === MT_B || typ === MT_BU)
  def is_mtype_halfword(typ: UInt) = (typ === MT_H || typ === MT_HU)
  def is_mtype_word(typ: UInt) = (typ === MT_W || typ === MT_WU)
  def is_mtype_doubleword(typ: UInt) = (typ === MT_D)

  def is_mcmd_load(cmd: UInt) = (cmd === M_XRD)
  def is_mcmd_store(cmd: UInt) = (cmd === M_XWR)
  def is_mcmd_amo(cmd: UInt) = isAMO(cmd)
  def is_mcmd_pfr(cmd: UInt) = (cmd === M_PFR)
  def is_mcmd_pfw(cmd: UInt) = (cmd === M_PFW)
  def is_mcmd_pf(cmd: UInt) = (is_mcmd_pfr(cmd) || is_mcmd_pfw(cmd))

  val SZ_QCNT = SZ_LGBANK1

  val SZ_VMU_OP = 3

  val VM_X   = UInt.DC(SZ_VMU_OP)
  val VM_VLD = UInt(0, SZ_VMU_OP)
  val VM_VST = UInt(1, SZ_VMU_OP)
  val VM_ULD = UInt(2, SZ_VMU_OP)
  val VM_UST = UInt(3, SZ_VMU_OP)
  val VM_AMO = UInt(4, SZ_VMU_OP)

  val IS_VM_OP_UTMEMOP =
    (x: UInt) => x === VM_ULD || x === VM_UST || x === VM_AMO
}

object Commands extends Commands
trait Commands
{
  // command bits for the vector command queue
  val CMD_X = Bits("b????_????",8)

  val CMD_VSETCFG = Bits("b00_0000_00",8)
  val CMD_VSETVL =  Bits("b00_0000_10",8)
  val CMD_VF =      Bits("b00_0000_11",8)

  val CMD_LDWB = Bits("b00_010_000",8)
  val CMD_STAC = Bits("b00_010_001",8)

  val CMD_VMVV =  Bits("b01_000_000",8)
  val CMD_VMSV =  Bits("b01_001_000",8)
  val CMD_VFMVV = Bits("b01_000_001",8)
  val CMD_VFMSV = Bits("b01_001_001",8)

  val CMD_VLD =  Bits("b1_00_0_0_0_11",8)
  val CMD_VLW =  Bits("b1_00_0_0_0_10",8)
  val CMD_VLWU = Bits("b1_00_0_0_1_10",8)
  val CMD_VLH =  Bits("b1_00_0_0_0_01",8)
  val CMD_VLHU = Bits("b1_00_0_0_1_01",8)
  val CMD_VLB =  Bits("b1_00_0_0_0_00",8)
  val CMD_VLBU = Bits("b1_00_0_0_1_00",8)
  val CMD_VSD =  Bits("b1_00_1_0_0_11",8)
  val CMD_VSW =  Bits("b1_00_1_0_0_10",8)
  val CMD_VSH =  Bits("b1_00_1_0_0_01",8)
  val CMD_VSB =  Bits("b1_00_1_0_0_00",8)

  val CMD_VFLD = Bits("b1_00_0_1_0_11",8)
  val CMD_VFLW = Bits("b1_00_0_1_0_10",8)
  val CMD_VFSD = Bits("b1_00_1_1_0_11",8)
  val CMD_VFSW = Bits("b1_00_1_1_0_10",8)

  val CMD_VLSTD =  Bits("b1_01_0_0_0_11",8)
  val CMD_VLSTW =  Bits("b1_01_0_0_0_10",8)
  val CMD_VLSTWU = Bits("b1_01_0_0_1_10",8)
  val CMD_VLSTH =  Bits("b1_01_0_0_0_01",8)
  val CMD_VLSTHU = Bits("b1_01_0_0_1_01",8)
  val CMD_VLSTB =  Bits("b1_01_0_0_0_00",8)
  val CMD_VLSTBU = Bits("b1_01_0_0_1_00",8)
  val CMD_VSSTD =  Bits("b1_01_1_0_0_11",8)
  val CMD_VSSTW =  Bits("b1_01_1_0_0_10",8)
  val CMD_VSSTH =  Bits("b1_01_1_0_0_01",8)
  val CMD_VSSTB =  Bits("b1_01_1_0_0_00",8)

  val CMD_VFLSTD = Bits("b1_01_0_1_0_11",8)
  val CMD_VFLSTW = Bits("b1_01_0_1_0_10",8)
  val CMD_VFSSTD = Bits("b1_01_1_1_0_11",8)
  val CMD_VFSSTW = Bits("b1_01_1_1_0_10",8)
}

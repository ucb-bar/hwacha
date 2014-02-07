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
  val SZ_VLENMAX = 11
  val SZ_VLEN = 11
  val SZ_REGLEN = 6
  val SZ_REGCNT = 6

  val SZ_ADDR = 64
  val SZ_INST = 32
  val SZ_DATA = 66
  val SZ_EXC = 5
  val SZ_XLEN = 64
  val SZ_FLEN = 65
}

trait VectorCommandQueueConstants
{
  val SZ_VCMD = 20
  val SZ_XCMD = 20

  val SZ_VIMM = 64
  val SZ_XIMM = 64

  val SZ_VSTRIDE = 64
  val SZ_XIMM2 = 64

  val SZ_VRESP = 32
  val SZ_XRESP = 1
}

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
  val vlen = UInt(width = SZ_VLEN)
  val nxregs = UInt(width = SZ_REGCNT)
  val nfregs = UInt(width = SZ_REGCNT)
  val bactive = Bits(width = SZ_BANK)
  val bcnt = UInt(width = SZ_BCNT)
  val xf_split = UInt(width = 11) 
  val prec = Bits(width = 2)
}

object PrecConstants
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
  val SZ_BVLEN = 3
  val SZ_BREGLEN = 8
  val SZ_BOPL = 2
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
  val M0 = Bits("b00", 2)
  val MR = Bits("b01", 2)
  val ML = Bits("b10", 2)
  val MI = Bits("b11", 2)

  val R_ = Bits("b00", 2)
  val RX = Bits("b01", 2)
  val RF = Bits("b11", 2)

  val ENUM_I = 4
  val imm_0 :: imm_I :: imm_S :: imm_U :: Nil = Enum( UInt(), ENUM_I )
  val SZ_I = imm_0.getWidth
  val imm_X = UInt(0, SZ_I)

  val DW__ = Bits("b0", 1)
  val DW32 = Bits("b0", 1)
  val DW64 = Bits("b1", 1)

  val FP_ = Bits("b00", 2)
  val FPS = Bits("b00", 2)
  val FPD = Bits("b01", 2)
  val FPH = Bits("b10", 2)

  val MTF_X = Bool(false)
  val MTF_N = Bool(false)
  val MTF_Y = Bool(true)
}

trait VIUConstants
{
  val ENUM_VIU = 28
  val viu_ADD :: viu_SLL :: viu_SLT :: viu_SLTU :: viu_XOR :: viu_SRL :: viu_SRA :: viu_OR :: viu_AND :: viu_SUB :: viu_IDX :: viu_MOV :: viu_FP_BRANCH = Enum( UInt(), ENUM_VIU)
  val viu_FSJ :: viu_FSJN :: viu_FSJX :: viu_FEQ :: viu_FLT :: viu_FLE :: viu_FMIN  :: viu_FMAX :: viu_MOVZ :: viu_MOVN :: viu_BRANCH = viu_FP_BRANCH
  val viu_BNE :: viu_BEQ :: viu_BLT :: viu_BLTU :: viu_BGE :: viu_BGEU :: Nil = viu_BRANCH
  val SZ_VIU_OP = viu_ADD.getWidth
  val viu_X = Bits(0, SZ_VIU_OP)
  val SZ_VIU_FP = 2
  val SZ_VIU_DW = 1
  val SZ_VIU_T1 = 2
  val SZ_VIU_T0 = 2
  val SZ_VIU_T = SZ_VIU_T1 + SZ_VIU_T0

  def isVIUBranch(viu: UInt) = viu >= viu_BNE && viu <= viu_BGEU

  val SZ_VBR_FN = 12
  val SZ_VIU_FN = 12

  val RG_VIU_T = (SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW+SZ_VIU_T-1           , SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW)
  val RG_VIU_T0 = (SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW+SZ_VIU_T1+SZ_VIU_T0-1, SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW+SZ_VIU_T1)
  val RG_VIU_T1 = (SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW+SZ_VIU_T1-1          , SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW)
  val RG_VIU_DW = (SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW-1                    , SZ_VIU_OP+SZ_VIU_FP) 
  val RG_VIU_FP = (SZ_VIU_OP+SZ_VIU_FP-1                              , SZ_VIU_OP)
  val RG_VIU_FN = (SZ_VIU_OP-1,0)
}

trait VAU0Constants extends DecodeConstants
{
  val ENUM_VAU0 = 4
  val vau0_M :: vau0_MH :: vau0_MHSU :: vau0_MHU :: Nil = Enum( UInt(), ENUM_VAU0 )
  val SZ_VAU0 = vau0_M.getWidth
  val vau0_X = Bits(0, SZ_VAU0)

  // acutal ops
  val VAU0_32 = Cat(DW32,vau0_M)
  val VAU0_32H = Cat(DW32,vau0_MH)
  val VAU0_32HSU = Cat(DW32,vau0_MHSU)
  val VAU0_32HU = Cat(DW32,vau0_MHU)
  val VAU0_64 = Cat(DW64,vau0_M)
  val VAU0_64H = Cat(DW64,vau0_MH)
  val VAU0_64HSU = Cat(DW64,vau0_MHSU)
  val VAU0_64HU = Cat(DW64,vau0_MHU)

  val SZ_VAU0_FN = 3
}

trait VAU1Constants
{
  // Can't use enums for VAU1 because decode depends on the top bit
  val VAU1_X = UInt(0, 3)
  val VAU1_ADD = UInt(0, 3)
  val VAU1_SUB = UInt(1, 3)
  val VAU1_MUL = UInt(2, 3)
  val VAU1_MADD = UInt(4, 3)
  val VAU1_MSUB = UInt(5, 3)
  val VAU1_NMSUB = UInt(6, 3)
  val VAU1_NMADD = UInt(7, 3)

  // val ENUM_VAU1 = 7
  // val vau1_ADD :: vau1_SUB :: vau1_MUL :: vau1_MADD :: vau1_MSUB :: vau1_NMSUB :: vau1_NMADD :: Nil = Enum(UInt(), ENUM_VAU1)
  // val SZ_VAU1 = vau1_ADD.getWidth
  // val vau1_X = Bits(0, SZ_VAU1)

  val SZ_VAU1_FN = 8

  val RG_VAU1_FP = (7,6)
  val RG_VAU1_RM = (5,3)
  val RG_VAU1_FN = (2,0)

  val FN_VAU1_FMA = (x: Bits) => x(2)
}

trait VAU2Constants
{
  val ENUM_VAU2 = 12
  val vau2_CLTF :: vau2_CLUTF :: vau2_CWTF :: vau2_CWUTF :: vau2_MXTF :: vau2_CFTL :: vau2_CFTLU :: vau2_CFTW :: vau2_CFTWU :: vau2_MFTX :: vau2_CDTS :: vau2_CSTD :: Nil = Enum( UInt(), ENUM_VAU2 )
  val SZ_VAU2 = vau2_CLTF.getWidth
  val vau2_X = Bits(0, SZ_VAU2)

  val SZ_VAU2_FN = 9

  val RG_VAU2_FP = (8,7)
  val RG_VAU2_RM = (6,4)
  val RG_VAU2_FN = (SZ_VAU2-1,0)
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
}

object Commands extends Commands
trait Commands
{
  // command bits for the vector command queue
  val CMD_X = Bits(0,8)

  val CMD_VVCFGIVL = Bits("b00_0000_00",8)
  val CMD_VSETPREC = Bits("b00_0000_01",8)
  val CMD_VSETVL =   Bits("b00_0000_10",8)
  val CMD_VF =       Bits("b00_0000_11",8)

  val CMD_FENCE_L_V = Bits("b00_001_100",8)
  val CMD_FENCE_G_V = Bits("b00_001_101",8)
  val CMD_FENCE_L_CV = Bits("b00_001_110",8)
  val CMD_FENCE_G_CV = Bits("b00_001_111",8)

  val CMD_LDWB = Bits("b00_010_000",8)
  val CMD_STAC = Bits("b00_010_001",8)

  val CMD_VMVV = Bits("b01_000_000",8)
  val CMD_VMSV = Bits("b01_001_000",8)
  val CMD_VMST = Bits("b01_010_000",8)
  val CMD_VMTS = Bits("b01_011_000",8)
  val CMD_VFMVV = Bits("b01_000_001",8)
  val CMD_VFMSV = Bits("b01_001_001",8)
  val CMD_VFMST = Bits("b01_010_001",8)
  val CMD_VFMTS = Bits("b01_011_001",8)

  val CMD_VLD = Bits("b1_00_0_0_0_11",8)
  val CMD_VLW = Bits("b1_00_0_0_0_10",8)
  val CMD_VLWU = Bits("b1_00_0_0_1_10",8)
  val CMD_VLH = Bits("b1_00_0_0_0_01",8)
  val CMD_VLHU = Bits("b1_00_0_0_1_01",8)
  val CMD_VLB = Bits("b1_00_0_0_0_00",8)
  val CMD_VLBU = Bits("b1_00_0_0_1_00",8)
  val CMD_VSD = Bits("b1_00_1_0_0_11",8)
  val CMD_VSW = Bits("b1_00_1_0_0_10",8)
  val CMD_VSH = Bits("b1_00_1_0_0_01",8)
  val CMD_VSB = Bits("b1_00_1_0_0_00",8)

  val CMD_VFLD = Bits("b1_00_0_1_0_11",8)
  val CMD_VFLW = Bits("b1_00_0_1_0_10",8)
  val CMD_VFSD = Bits("b1_00_1_1_0_11",8)
  val CMD_VFSW = Bits("b1_00_1_1_0_10",8)

  val CMD_VLSTD = Bits("b1_01_0_0_0_11",8)
  val CMD_VLSTW = Bits("b1_01_0_0_0_10",8)
  val CMD_VLSTWU = Bits("b1_01_0_0_1_10",8)
  val CMD_VLSTH = Bits("b1_01_0_0_0_01",8)
  val CMD_VLSTHU = Bits("b1_01_0_0_1_01",8)
  val CMD_VLSTB = Bits("b1_01_0_0_0_00",8)
  val CMD_VLSTBU = Bits("b1_01_0_0_1_00",8)
  val CMD_VSSTD = Bits("b1_01_1_0_0_11",8)
  val CMD_VSSTW = Bits("b1_01_1_0_0_10",8)
  val CMD_VSSTH = Bits("b1_01_1_0_0_01",8)
  val CMD_VSSTB = Bits("b1_01_1_0_0_00",8)

  val CMD_VFLSTD = Bits("b1_01_0_1_0_11",8)
  val CMD_VFLSTW = Bits("b1_01_0_1_0_10",8)
  val CMD_VFSSTD = Bits("b1_01_1_1_0_11",8)
  val CMD_VFSSTW = Bits("b1_01_1_1_0_10",8)

  val CMD_VLXD = Bits("b1_10_0_0_0_11",8)
  val CMD_VLXW = Bits("b1_10_0_0_0_10",8)
  val CMD_VLXWU = Bits("b1_10_0_0_1_10",8)
  val CMD_VLXH = Bits("b1_10_0_0_0_01",8)
  val CMD_VLXHU = Bits("b1_10_0_0_1_01",8)
  val CMD_VLXB = Bits("b1_10_0_0_0_00",8)
  val CMD_VLXBU = Bits("b1_10_0_0_1_00",8)
  val CMD_VSXD = Bits("b1_10_1_0_0_11",8)
  val CMD_VSXW = Bits("b1_10_1_0_0_10",8)
  val CMD_VSXH = Bits("b1_10_1_0_0_01",8)
  val CMD_VSXB = Bits("b1_10_1_0_0_00",8)

  val CMD_VFLXD = Bits("b1_10_0_1_0_11",8)
  val CMD_VFLXW = Bits("b1_10_0_1_0_10",8)
  val CMD_VFSXD = Bits("b1_10_1_1_0_11",8)
  val CMD_VFSXW = Bits("b1_10_1_1_0_10",8)

  val CMD_VAMOADDD = Bits("b1_11_000_11",8)
  val CMD_VAMOSWAPD = Bits("b1_11_001_11",8)
  val CMD_VAMOANDD = Bits("b1_11_010_11",8)
  val CMD_VAMOORD = Bits("b1_11_011_11",8)
  val CMD_VAMOMIND = Bits("b1_11_100_11",8)
  val CMD_VAMOMAXD = Bits("b1_11_101_11",8)
  val CMD_VAMOMINUD = Bits("b1_11_110_11",8)
  val CMD_VAMOMAXUD = Bits("b1_11_111_11",8)

  val CMD_VAMOADDW = Bits("b1_11_000_10",8)
  val CMD_VAMOSWAPW = Bits("b1_11_001_10",8)
  val CMD_VAMOANDW = Bits("b1_11_010_10",8)
  val CMD_VAMOORW = Bits("b1_11_011_10",8)
  val CMD_VAMOMINW = Bits("b1_11_100_10",8)
  val CMD_VAMOMAXW = Bits("b1_11_101_10",8)
  val CMD_VAMOMINUW = Bits("b1_11_110_10",8)
  val CMD_VAMOMAXUW = Bits("b1_11_111_10",8)
}

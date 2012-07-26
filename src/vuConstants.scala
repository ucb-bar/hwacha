package hwacha

import Chisel._
import Node._

object Constants
{
  val HAVE_VRU = true

  val SZ_VLENMAX  = 11

  val SZ_VCMD = 20
  val SZ_VCMD_CMD = 8
  val SZ_VCMD_VD = 6
  val SZ_VCMD_VS = 6
  
  val SZ_XCMD = 20
  val SZ_XCMD_CMD = 8
  val SZ_XCMD_VD = 6
  val SZ_XCMD_VS = 6
  val RG_XCMD_CMCODE = (19,12)
  val RG_XCMD_VD = (11,6)
  val RG_XCMD_VT = (5,0)
  val RG_XIMM1_VLEN = (10,0)
  val RG_XIMM1_NXREGS = (16,11)
  val RG_XIMM1_NFREGS = (22,17)
  val RG_XIMM1_BACTIVE = (30,23)
  val RG_XIMM1_BCNT = (34,31)

  val SZ_VIMM      = 64
  val SZ_XIMM      = 64

  val SZ_VSTRIDE   = 64
  val SZ_XIMM2     = 64

  val SZ_VRESP     = 32
  val SZ_XRESP     = 1

  val SZ_ADDR = 64
  val SZ_INST = 32
  val SZ_DATA = 65
  val SZ_EXC = 5
  val SZ_XLEN = 64
  val SZ_FLEN = 65

  val SZ_STALL = 3

  val RG_VAQ = 2
  val RG_VLDQ = 1
  val RG_VSDQ = 0

  val SZ_VLEN = 11
  val SZ_REGLEN = 6
  val SZ_REGCNT = 6

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
  
  val VACKCNT = 32
  val VACKCNT_FX = UFix(VACKCNT)
  val SZ_VACKCNT = log2Up(VACKCNT)

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

  // pipe stages
  val INT_STAGES   = 2
  val IMUL_STAGES  = 4
  val FMA_STAGES   = 5
  val FCONV_STAGES = 3

  // constants for expander
  val SHIFT_BUF_READ = 3
  val SHIFT_BUF_WRITE = FMA_STAGES + 4

  // constants for vmu
  val VVAQARB_LANE = 0
  val VVAQARB_EVAC = 1

  val VPAQARB_VPAQ = 0
  val VPAQARB_VPFPAQ = 1

  val VSDQARB_LANE = 0
  val VSDQARB_EVAC = 1

  val LATE_TLB_MISS = true
  val LATE_DMEM_NACK = true

  val ENTRIES_VVAQ = 16
  val ENTRIES_VPAQ = 16
  val ENTRIES_VPFVAQ = 16
  val ENTRIES_VPFPAQ = 16
  val ENTRIES_VLDQ = 128
  val ENTRIES_VSDQ = 16
  val ENTRIES_VPASDQ = 31
  val ENTRIES_VSREQ = 31
  val ENTRIES_VLREQ = ENTRIES_VLDQ

  val LG_ENTRIES_VLDQ = log2Up(ENTRIES_VLDQ)

  val SZ_QCNT = SZ_LGBANK1

  // constans for vau0, vau1, vau2
  val M0 = Bits("b00", 2)
  val MR = Bits("b01", 2)
  val ML = Bits("b10", 2)
  val MI = Bits("b11", 2)

  val R_ = Bits("b00", 2)
  val RX = Bits("b01", 2)
  val RF = Bits("b11", 2)

  val ENUM_I = 4
  val imm_0 :: imm_I :: imm_B :: imm_L :: Nil = Enum(ENUM_I){UFix()}
  val SZ_I = imm_0.getWidth
  val imm_X = UFix(0, SZ_I)

  val DW__ = Bits("b0", 1)
  val DW32 = Bits("b0", 1)
  val DW64 = Bits("b1", 1)

  val FP_ = Bits("b0", 1)
  val FPS = Bits("b0", 1)
  val FPD = Bits("b1", 1)

  val ENUM_VIU = 28
  val viu_ADD :: viu_SLL :: viu_SLT :: viu_SLTU :: viu_XOR :: viu_SRL :: viu_SRA :: viu_OR :: viu_AND :: viu_SUB :: viu_IDX :: viu_MOV :: viu_FP_BRANCH = Enum(ENUM_VIU){Bits()}
  val viu_FSJ :: viu_FSJN :: viu_FSJX :: viu_FEQ :: viu_FLT :: viu_FLE :: viu_FMIN  :: viu_FMAX :: viu_MOVZ :: viu_MOVN :: viu_BRANCH = viu_FP_BRANCH
  val viu_BNE :: viu_BEQ :: viu_BLT :: viu_BLTU :: viu_BGE :: viu_BGEU :: Nil = viu_BRANCH
  val SZ_VIU_OP = viu_ADD.getWidth
  val viu_X = Bits(0, SZ_VIU_OP)
  val SZ_VIU_FP = 1
  val SZ_VIU_DW = 1
  val SZ_VIU_T1 = 2
  val SZ_VIU_T0 = 2
  val SZ_VIU_T = SZ_VIU_T1 + SZ_VIU_T0

  def isVIUBranch(viu: Bits) = viu >= viu_BNE && viu <= viu_BGEU

  // in the decode table
  val ENUM_VAU0 = 4
  val vau0_M :: vau0_MH :: vau0_MHSU :: vau0_MHU :: Nil = Enum(ENUM_VAU0) { Bits() }
  val SZ_VAU0 = vau0_M.getWidth
  val vau0_X = Bits(0, SZ_VAU0)

  // acutal ops
  val VAU0_32    = Cat(DW32,vau0_M)
  val VAU0_32H   = Cat(DW32,vau0_MH)
  val VAU0_32HSU = Cat(DW32,vau0_MHSU)
  val VAU0_32HU  = Cat(DW32,vau0_MHU)
  val VAU0_64    = Cat(DW64,vau0_M)
  val VAU0_64H   = Cat(DW64,vau0_MH)
  val VAU0_64HSU = Cat(DW64,vau0_MHSU)
  val VAU0_64HU  = Cat(DW64,vau0_MHU)

  // Can't use enums for VAU1 because decode depends on the top bit
  val VAU1_X     = UFix(0, 3)
  val VAU1_ADD   = UFix(0, 3)
  val VAU1_SUB   = UFix(1, 3)
  val VAU1_MUL   = UFix(2, 3)
  val VAU1_MADD  = UFix(4, 3)
  val VAU1_MSUB  = UFix(5, 3)
  val VAU1_NMSUB = UFix(6, 3)
  val VAU1_NMADD = UFix(7, 3)

  // val ENUM_VAU1 = 7
  // val vau1_ADD :: vau1_SUB :: vau1_MUL :: vau1_MADD :: vau1_MSUB :: vau1_NMSUB :: vau1_NMADD :: Nil = Enum(ENUM_VAU1){ UFix() }
  // val SZ_VAU1 = vau1_ADD.getWidth
  // val vau1_X = Bits(0, SZ_VAU1)

  val ENUM_VAU2 = 12
  val vau2_CLTF :: vau2_CLUTF :: vau2_CWTF :: vau2_CWUTF :: vau2_MXTF :: vau2_CFTL :: vau2_CFTLU :: vau2_CFTW :: vau2_CFTWU :: vau2_MFTX :: vau2_CDTS :: vau2_CSTD :: Nil = Enum(ENUM_VAU2){ Bits() }
  val SZ_VAU2 = vau2_CLTF.getWidth
  val vau2_X = Bits(0, SZ_VAU2)

  val SZ_VBR_FN = 11
  val SZ_VIU_FN  = 11
  val SZ_VAU0_FN = 3
  val SZ_VAU1_FN = 7
  val SZ_VAU2_FN = 8

  val RG_VIU_T  = (SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW+SZ_VIU_T-1           , SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW)
  val RG_VIU_T0 = (SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW+SZ_VIU_T1+SZ_VIU_T0-1, SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW+SZ_VIU_T1)
  val RG_VIU_T1 = (SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW+SZ_VIU_T1-1          , SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW)
  val RG_VIU_DW = (SZ_VIU_OP+SZ_VIU_FP+SZ_VIU_DW-1                    , SZ_VIU_OP+SZ_VIU_FP) 
  val RG_VIU_FP = (SZ_VIU_OP+SZ_VIU_FP-1                              , SZ_VIU_OP)
  val RG_VIU_FN = (SZ_VIU_OP-1,0)

  val RG_VAU1_FP = 6
  val RG_VAU1_RM = (5,3)
  val RG_VAU1_FN = (2,0)

  val FN_VAU1_FMA = (x: Bits) => x(2)

  val RG_VAU2_FP = 7
  val RG_VAU2_RM = (6,4)
  val RG_VAU2_FN = (SZ_VAU2-1,0)

  // the following constants are from the rocket pipeline
  val PADDR_BITS = 40
  val VADDR_BITS = 43
  val PGIDX_BITS = 13
  val PPN_BITS = PADDR_BITS-PGIDX_BITS
  val VPN_BITS = VADDR_BITS-PGIDX_BITS
  val ASID_BITS = 7
  val OFFSET_BITS = 6 // log2(cache line size in bytes)

  val MTF_X = Bits(0, 1)
  val MTF_N = Bits(0, 1)
  val MTF_Y = Bits(1, 1)

  val ENUM_MTYPS = 7
  val mtyp_B :: mtyp_H :: mtyp_W :: mtyp_D :: mtyp_BU :: mtyp_HU :: mtyp_WU :: Nil = Enum(ENUM_MTYPS) { Bits() }
  val SZ_MTYPS = mtyp_B.getWidth
  val mtyp_X = Bits(0, SZ_MTYPS)

  def is_mtype_byte(typ: Bits) = (typ === mtyp_B || typ === mtyp_BU)
  def is_mtype_halfword(typ: Bits) = (typ === mtyp_H || typ === mtyp_HU)
  def is_mtype_word(typ: Bits) = (typ === mtyp_W || typ === mtyp_WU)
  def is_mtype_doubleword(typ: Bits) = (typ === mtyp_D)

  val ENUM_MCMDS = 16
  val mcmd_XRD :: mcmd_XWR :: mcmd_PFR :: mcmd_PFW :: mcmd_FLA :: mcmd_FENCE :: mcmd_INV :: mcmd_CLN ::  mcmd_XA_ADD :: mcmd_XA_SWAP :: mcmd_XA_AND :: mcmd_XA_OR :: mcmd_XA_MIN :: mcmd_XA_MAX :: mcmd_XA_MINU :: mcmd_XA_MAXU :: Nil = Enum(ENUM_MCMDS){ Bits() }
  // First few:
  // int load - XRD
  // int store - XWR
  // prefetch with intent to read - PFR
  // prefetch with intent to write - PFW
  // write back and invlaidate all lines - FLA
  // memory fence - FENCE
  // write back and invalidate line - INV
  // write back line - CLN
  val SZ_MCMDS = mcmd_XRD.getWidth
  val mcmd_X = Bits(0, SZ_MCMDS)

  def is_mcmd_load(cmd: Bits) = (cmd === mcmd_XRD)
  def is_mcmd_store(cmd: Bits) = (cmd === mcmd_XWR)
  def is_mcmd_pf(cmd: Bits) = (cmd === mcmd_PFR || cmd === mcmd_PFW)
  def is_mcmd_amo(cmd: Bits) = (mcmd_XA_ADD <= cmd && cmd <= mcmd_XA_MAXU)

  val HAVE_FMA = false

  val FCMD_X =          Bits("b000000")
  val FCMD_ADD =        Bits("b000000")
  val FCMD_SUB =        Bits("b000001")
  val FCMD_MUL =        Bits("b000010")
  val FCMD_MADD =       Bits("b100100")
  val FCMD_MSUB =       Bits("b100101")
  val FCMD_NMSUB =      Bits("b100110")
  val FCMD_NMADD =      Bits("b100111")
  val FCMD_WIDTH = 6

  val DFMA_STAGES = 6
  val SFMA_STAGES = 4

  // PVFB Constants
  var coarseGrained = false
  var HAVE_PVFB = false
  var NUM_PVFB = 4
  var WIDTH_PVFB = 64
  var DEPTH_PVFB = 64
  assert(DEPTH_PVFB >= WIDTH_PVFB, println("DEPTH_PVFB MUST BE GREATER THAN OR EQUAL TO DEPTH_PVFB"))
  assert(!coarseGrained || (coarseGrained && NUM_PVFB > 1))
  def SZ_PVFB_TAG = log2Up(NUM_PVFB)
  def SZ_MASK = log2Up(WIDTH_PVFB)
  def WIDTH_BMASK = NUM_PVFB * WIDTH_PVFB / SZ_BANK
  def SZ_BMASK = log2Up(WIDTH_BMASK)
}

object Commands
{
  // command bits for the vector command queue
  val CMD_X          = Bits(0,8)

  val CMD_VVCFGIVL   = Bits("b00_0000_00",8)
  val CMD_VSETVL     = Bits("b00_0000_10",8)
  val CMD_VF         = Bits("b00_0000_11",8)

  val CMD_FENCE_L_V  = Bits("b00_001_100",8)
  val CMD_FENCE_G_V  = Bits("b00_001_101",8)
  val CMD_FENCE_L_CV = Bits("b00_001_110",8)
  val CMD_FENCE_G_CV = Bits("b00_001_111",8)

  val CMD_LDWB       = Bits("b00_010_000",8)
  val CMD_STAC       = Bits("b00_010_001",8)

  val CMD_VMVV       = Bits("b01_000_000",8)
  val CMD_VMSV       = Bits("b01_001_000",8)
  val CMD_VMST       = Bits("b01_010_000",8)
  val CMD_VMTS       = Bits("b01_011_000",8)
  val CMD_VFMVV      = Bits("b01_000_001",8)
  val CMD_VFMSV      = Bits("b01_001_001",8)
  val CMD_VFMST      = Bits("b01_010_001",8)
  val CMD_VFMTS      = Bits("b01_011_001",8)

  val CMD_VLD        = Bits("b1_00_0_0_0_11",8)
  val CMD_VLW        = Bits("b1_00_0_0_0_10",8)
  val CMD_VLWU       = Bits("b1_00_0_0_1_10",8)
  val CMD_VLH        = Bits("b1_00_0_0_0_01",8)
  val CMD_VLHU       = Bits("b1_00_0_0_1_01",8)
  val CMD_VLB        = Bits("b1_00_0_0_0_00",8)
  val CMD_VLBU       = Bits("b1_00_0_0_1_00",8)
  val CMD_VSD        = Bits("b1_00_1_0_0_11",8)
  val CMD_VSW        = Bits("b1_00_1_0_0_10",8)
  val CMD_VSH        = Bits("b1_00_1_0_0_01",8)
  val CMD_VSB        = Bits("b1_00_1_0_0_00",8)

  val CMD_VFLD       = Bits("b1_00_0_1_0_11",8)
  val CMD_VFLW       = Bits("b1_00_0_1_0_10",8)
  val CMD_VFSD       = Bits("b1_00_1_1_0_11",8)
  val CMD_VFSW       = Bits("b1_00_1_1_0_10",8)

  val CMD_VLSTD      = Bits("b1_01_0_0_0_11",8)
  val CMD_VLSTW      = Bits("b1_01_0_0_0_10",8)
  val CMD_VLSTWU     = Bits("b1_01_0_0_1_10",8)
  val CMD_VLSTH      = Bits("b1_01_0_0_0_01",8)
  val CMD_VLSTHU     = Bits("b1_01_0_0_1_01",8)
  val CMD_VLSTB      = Bits("b1_01_0_0_0_00",8)
  val CMD_VLSTBU     = Bits("b1_01_0_0_1_00",8)
  val CMD_VSSTD      = Bits("b1_01_1_0_0_11",8)
  val CMD_VSSTW      = Bits("b1_01_1_0_0_10",8)
  val CMD_VSSTH      = Bits("b1_01_1_0_0_01",8)
  val CMD_VSSTB      = Bits("b1_01_1_0_0_00",8)

  val CMD_VFLSTD     = Bits("b1_01_0_1_0_11",8)
  val CMD_VFLSTW     = Bits("b1_01_0_1_0_10",8)
  val CMD_VFSSTD     = Bits("b1_01_1_1_0_11",8)
  val CMD_VFSSTW     = Bits("b1_01_1_1_0_10",8)

  val CMD_VLXD       = Bits("b1_10_0_0_0_11",8)
  val CMD_VLXW       = Bits("b1_10_0_0_0_10",8)
  val CMD_VLXWU      = Bits("b1_10_0_0_1_10",8)
  val CMD_VLXH       = Bits("b1_10_0_0_0_01",8)
  val CMD_VLXHU      = Bits("b1_10_0_0_1_01",8)
  val CMD_VLXB       = Bits("b1_10_0_0_0_00",8)
  val CMD_VLXBU      = Bits("b1_10_0_0_1_00",8)
  val CMD_VSXD       = Bits("b1_10_1_0_0_11",8)
  val CMD_VSXW       = Bits("b1_10_1_0_0_10",8)
  val CMD_VSXH       = Bits("b1_10_1_0_0_01",8)
  val CMD_VSXB       = Bits("b1_10_1_0_0_00",8)

  val CMD_VFLXD      = Bits("b1_10_0_1_0_11",8)
  val CMD_VFLXW      = Bits("b1_10_0_1_0_10",8)
  val CMD_VFSXD      = Bits("b1_10_1_1_0_11",8)
  val CMD_VFSXW      = Bits("b1_10_1_1_0_10",8)

  val CMD_VAMOADDD   = Bits("b1_11_000_11",8)
  val CMD_VAMOSWAPD  = Bits("b1_11_001_11",8)
  val CMD_VAMOANDD   = Bits("b1_11_010_11",8)
  val CMD_VAMOORD    = Bits("b1_11_011_11",8)
  val CMD_VAMOMIND   = Bits("b1_11_100_11",8)
  val CMD_VAMOMAXD   = Bits("b1_11_101_11",8)
  val CMD_VAMOMINUD  = Bits("b1_11_110_11",8)
  val CMD_VAMOMAXUD  = Bits("b1_11_111_11",8)

  val CMD_VAMOADDW   = Bits("b1_11_000_10",8)
  val CMD_VAMOSWAPW  = Bits("b1_11_001_10",8)
  val CMD_VAMOANDW   = Bits("b1_11_010_10",8)
  val CMD_VAMOORW    = Bits("b1_11_011_10",8)
  val CMD_VAMOMINW   = Bits("b1_11_100_10",8)
  val CMD_VAMOMAXW   = Bits("b1_11_101_10",8)
  val CMD_VAMOMINUW  = Bits("b1_11_110_10",8)
  val CMD_VAMOMAXUW  = Bits("b1_11_111_10",8)

}

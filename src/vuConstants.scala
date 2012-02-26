package hwacha

import Chisel._
import Node._

object Constants
{
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
  val RG_XCMD_VS = (5,0)
  val RG_XCMD_VLEN = (10,0)

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
  val SZ_VACKCNT = log2up(VACKCNT)

  val IRB_CMD_DEPTH = 8
  val IRB_IMM1_DEPTH = 8
  val IRB_IMM2_DEPTH = 8
  val IRB_CNT_DEPTH = 8

  val SZ_IRB_CMD = log2up(IRB_CMD_DEPTH)
  val SZ_IRB_IMM1 = log2up(IRB_IMM1_DEPTH)
  val SZ_IRB_IMM2E = log2up(IRB_IMM2_DEPTH)
  val SZ_IRB_CNT = log2up(IRB_IMM2_DEPTH)

  // pipe stages
  val INT_STAGES   = 2
  val IMUL_STAGES  = 3
  val FMA_STAGES   = 5
  val FCONV_STAGES = 3

  // constants for expander
  val SHIFT_BUF_READ = 3
  val SHIFT_BUF_WRITE = FMA_STAGES + 4

  // constans for vau0, vau1, vau2
  val M0 = Bits("b00", 2)
  val MR = Bits("b01", 2)
  val ML = Bits("b10", 2)
  val MI = Bits("b11", 2)

  val R_ = Bits("b0", 1)
  val RX = Bits("b0", 1)
  val RF = Bits("b1", 1)

  val I_ = UFix(0, 2)
  val II = UFix(0, 2)
  val IB = UFix(1, 2)
  val IL = UFix(2, 2)

  val DW__ = Bits("b0", 1)
  val DW32 = Bits("b0", 1)
  val DW64 = Bits("b1", 1)

  val FP_ = Bits("b0", 1)
  val FPS = Bits("b0", 1)
  val FPD = Bits("b1", 1)

  val VIU_X     = Bits(0, 5)
  val VIU_ADD   = Bits(1, 5)
  val VIU_SLL   = Bits(2, 5)
  val VIU_SLT   = Bits(3, 5)
  val VIU_SLTU  = Bits(4, 5)
  val VIU_XOR   = Bits(5, 5)
  val VIU_SRL   = Bits(6, 5)
  val VIU_SRA   = Bits(7, 5)
  val VIU_OR    = Bits(8, 5)
  val VIU_AND   = Bits(9, 5)
  val VIU_SUB   = Bits(10, 5)
  val VIU_IDX   = Bits(11, 5)
  val VIU_MOV   = Bits(12, 5)
  val VIU_FSJ   = Bits(13, 5)
  val VIU_FSJN  = Bits(14, 5)
  val VIU_FSJX  = Bits(15, 5)
  val VIU_FEQ   = Bits(16, 5)
  val VIU_FLT   = Bits(17, 5)
  val VIU_FLE   = Bits(18, 5)
  val VIU_FMIN  = Bits(19, 5)
  val VIU_FMAX  = Bits(20, 5)
  val VIU_MOVZ  = Bits(21, 5)
  val VIU_MOVN  = Bits(22, 5)

  // in the decode table
  val VAU0_X     = Bits(0, 2)
  val VAU0_M     = Bits(0, 2)
  val VAU0_MH    = Bits(1, 2)
  val VAU0_MHSU  = Bits(2, 2)
  val VAU0_MHU   = Bits(3, 2)

  // acutal ops
  val VAU0_32    = Cat(DW32,VAU0_M)
  val VAU0_32H   = Cat(DW32,VAU0_MH)
  val VAU0_32HSU = Cat(DW32,VAU0_MHSU)
  val VAU0_32HU  = Cat(DW32,VAU0_MHU)
  val VAU0_64    = Cat(DW64,VAU0_M)
  val VAU0_64H   = Cat(DW64,VAU0_MH)
  val VAU0_64HSU = Cat(DW64,VAU0_MHSU)
  val VAU0_64HU  = Cat(DW64,VAU0_MHU)

  val VAU1_X     = UFix(0, 3)
  val VAU1_ADD   = UFix(0, 3)
  val VAU1_SUB   = UFix(1, 3)
  val VAU1_MUL   = UFix(2, 3)
  val VAU1_MADD  = UFix(4, 3)
  val VAU1_MSUB  = UFix(5, 3)
  val VAU1_NMSUB = UFix(6, 3)
  val VAU1_NMADD = UFix(7, 3)

  val VAU2_X     = Bits("b0000", 4)
  val VAU2_CLTF  = Bits("b0000", 4)
  val VAU2_CLUTF = Bits("b0001", 4)
  val VAU2_CWTF  = Bits("b0010", 4)
  val VAU2_CWUTF = Bits("b0011", 4)
  val VAU2_MXTF  = Bits("b0100", 4)
  val VAU2_CFTL  = Bits("b1000", 4)
  val VAU2_CFTLU = Bits("b1001", 4)
  val VAU2_CFTW  = Bits("b1010", 4)
  val VAU2_CFTWU = Bits("b1011", 4)
  val VAU2_MFTX  = Bits("b1100", 4)
  val VAU2_CDTS  = Bits("b1110", 4)
  val VAU2_CSTD  = Bits("b1111", 4)

  val SZ_VIU_FN  = 11
  val SZ_VAU0_FN = 3
  val SZ_VAU1_FN = 7
  val SZ_VAU2_FN = 8

  val RG_VIU_T  = (10,7)
  val RG_VIU_T0 = (10,9)
  val RG_VIU_T1 = (8,7)
  val RG_VIU_DW = 6 
  val RG_VIU_FP = 5
  val RG_VIU_FN = (4,0)

  val RG_VAU1_FP = 6
  val RG_VAU1_RM = (5,3)
  val RG_VAU1_FN = (2,0)

  val FN_VAU1_FMA = (x: Bits) => x(2)

  val RG_VAU2_FP = 7
  val RG_VAU2_RM = (6,4)
  val RG_VAU2_FN = (3,0)

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

  val MT_X  = Bits("b000", 3)
  val MT_B  = Bits("b000", 3)
  val MT_H  = Bits("b001", 3)
  val MT_W  = Bits("b010", 3)
  val MT_D  = Bits("b011", 3)
  val MT_BU = Bits("b100", 3)
  val MT_HU = Bits("b101", 3)
  val MT_WU = Bits("b110", 3)

  val M_X       = UFix(0, 4)
  val M_XRD     = Bits("b0000", 4) // int load
  val M_XWR     = Bits("b0001", 4) // int store
  val M_PFR     = Bits("b0010", 4) // prefetch with intent to read
  val M_PFW     = Bits("b0011", 4) // prefetch with intent to write
  val M_FLA     = Bits("b0100", 4) // write back and invlaidate all lines
  val M_FENCE   = Bits("b0101", 4) // memory fence
  val M_INV     = Bits("b0110", 4) // write back and invalidate line
  val M_CLN     = Bits("b0111", 4) // write back line
  val M_XA_ADD  = Bits("b1000", 4)
  val M_XA_SWAP = Bits("b1001", 4)
  val M_XA_AND  = Bits("b1010", 4)
  val M_XA_OR   = Bits("b1011", 4)
  val M_XA_MIN  = Bits("b1100", 4)
  val M_XA_MAX  = Bits("b1101", 4)
  val M_XA_MINU = Bits("b1110", 4)
  val M_XA_MAXU = Bits("b1111", 4)

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

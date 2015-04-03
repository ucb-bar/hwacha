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
  VIMUConstants with
  VIDUConstants with
  VFMUConstants with
  VFDUConstants with
  VFCUConstants with
  VFVUConstants with
  VMUConstants

trait MachineConstants
{
  val SZ_VLEN = 12
  val SZ_REGLEN = 6
  val SZ_REGCNT = 6

  val SZ_ADDR = 64
  val SZ_DATA = 128

  val SZ_D = 64
  val SZ_W = 32
  val SZ_H = 16
  val SZ_B = 8

  val N_D = SZ_DATA / SZ_D
  val N_W = SZ_DATA / SZ_W
  val N_H = SZ_DATA / SZ_H
  val N_B = SZ_DATA / SZ_B
}

trait HwachaDecodeConstants
{
  val Y = Bool(true)
  val N = Bool(false)
  val X = Bits("b?", 1)

  val VRT_X = Bits("b?", 1)
  val VRT_S = Bits(0, 1)
  val VRT_A = Bits(1, 1)

  val VR_X   = Bits("b?", 1)
  val VR_RS1 = Bits(0, 1)
  val VR_RD  = Bits(1, 1)

  val IMM_X    = Bits("b???",3)
  val IMM_VLEN = Bits(0,3)
  val IMM_RS1  = Bits(1,3)
  val IMM_RS2  = Bits(2,3)
  val IMM_ADDR = Bits(3,3)

  val RESP_X     = Bits("b???",3)
  val RESP_NVL   = Bits(0,3)
  val RESP_CAUSE = Bits(1,3)
  val RESP_AUX   = Bits(2,3)
  val RESP_CFG   = Bits(3,3)
  val RESP_VL    = Bits(4,3)

  val SZ_PREC = 2
  val PREC_DOUBLE = Bits("b00", SZ_PREC)
  val PREC_SINGLE = Bits("b01", SZ_PREC)
  val PREC_HALF   = Bits("b10", SZ_PREC)

}

trait VectorCommandQueueConstants
{
  val SZ_VCMD = 18
  val SZ_IMM = 64
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
  val SZ_BRPORT = 12 // 2 imul, 3 fma0, 3 fma1, 1 fconv0, 1 fconv1, 1 vgu, 1 vsu
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
  val Y = Bool(true)
  val N = Bool(false)
  val X = Bits("b?", 1)

  val M0 = Bits("b00", 2)
  val MR = Bits("b01", 2)
  val ML = Bits("b10", 2)
  val MI = Bits("b11", 2)

  val R_ = Bits("b???0", 4)
  val RA = Bits("b0011", 4)
  val RS = Bits("b0111", 4)
  val RV = Bits("b0001", 4)
  val RP = Bits("b0101", 4)
  val RX = Bits("b1101", 4)
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

  val A1_X    = Bits("b??", 2)
  val A1_ZERO = UInt(0, 2)
  val A1_RS1  = UInt(1, 2)
  val A1_PC   = UInt(2, 2)

  val A2_X    = Bits("b??", 2)
  val A2_ZERO = UInt(0, 2)
  val A2_FOUR = UInt(1, 2)
  val A2_RS2  = UInt(2, 2)
  val A2_IMM  = UInt(3, 2)

  val NBYP = 2
  val SZ_BYP = log2Up(NBYP)
  val BYP_0   = 0
  val BYP_EX  = 1

  val DW_X  = X
  val DW_32 = N
  val DW_64 = Y
  val DW_XPR = Y

  //riscv-opcode fields
  val OPC_VD  = UInt(63)
  val OPC_VS1 = UInt(62)
  val OPC_VS2 = UInt(61)
  val OPC_VS3 = UInt(60)
}

trait VIUConstants
{
  val SZ_VIU_OP = 4

  val I_X    = UInt.DC(SZ_VIU_OP)
  val I_ADD  = UInt(0, SZ_VIU_OP)
  val I_ADDU = UInt(1, SZ_VIU_OP)
  val I_SLL  = UInt(2, SZ_VIU_OP)
  val I_SLT  = UInt(3, SZ_VIU_OP)
  val I_SLTU = UInt(4, SZ_VIU_OP)
  val I_XOR  = UInt(5, SZ_VIU_OP)
  val I_SRL  = UInt(6, SZ_VIU_OP)
  val I_SRA  = UInt(7, SZ_VIU_OP)
  val I_OR   = UInt(8, SZ_VIU_OP)
  val I_AND  = UInt(9, SZ_VIU_OP)
  val I_SUB  = UInt(10, SZ_VIU_OP)
  val I_IDX  = UInt(11, SZ_VIU_OP)
  val I_MOV0 = UInt(12, SZ_VIU_OP)
  val I_FSJ  = UInt(13, SZ_VIU_OP)
  val I_FSJN = UInt(14, SZ_VIU_OP)
  val I_FSJX = UInt(15, SZ_VIU_OP)
}

trait VIMUConstants
{
  val SZ_VIMU_OP = 2

  val IM_X    = UInt.DC(SZ_VIMU_OP)
  val IM_M    = UInt(0, SZ_VIMU_OP)
  val IM_MH   = UInt(1, SZ_VIMU_OP)
  val IM_MHSU = UInt(2, SZ_VIMU_OP)
  val IM_MHU  = UInt(3, SZ_VIMU_OP)
}

trait VIDUConstants
{
  val SZ_VIDU_OP = 2

  val ID_X    = UInt.DC(SZ_VIDU_OP)
  val ID_DIV  = UInt(0, SZ_VIDU_OP)
  val ID_DIVU = UInt(1, SZ_VIDU_OP)
  val ID_REM  = UInt(2, SZ_VIDU_OP)
  val ID_REMU = UInt(3, SZ_VIDU_OP)
}

trait VFMUConstants
{
  val SZ_VFMU_OP = 3

  val FM_X     = UInt.DC(SZ_VFMU_OP)
  val FM_ADD   = UInt(0, SZ_VFMU_OP)
  val FM_SUB   = UInt(1, SZ_VFMU_OP)
  val FM_MUL   = UInt(2, SZ_VFMU_OP)
  val FM_MADD  = UInt(4, SZ_VFMU_OP)
  val FM_MSUB  = UInt(5, SZ_VFMU_OP)
  val FM_NMSUB = UInt(6, SZ_VFMU_OP)
  val FM_NMADD = UInt(7, SZ_VFMU_OP)

  val IS_FM_OP_MA = (x: Bits) => x(2)
}

trait VFDUConstants
{
  val SZ_VFDU_OP = 1

  val FD_X    = UInt.DC(SZ_VFDU_OP)
  val FD_DIV  = UInt(0, SZ_VFDU_OP)
  val FD_SQRT = UInt(1, SZ_VFDU_OP)
}

trait VFCUConstants
{
  val SZ_VFCU_OP = 3

  val FC_X     = UInt.DC(SZ_VFCU_OP)
  val FC_CEQ   = UInt(0, SZ_VFCU_OP)
  val FC_CLT   = UInt(1, SZ_VFCU_OP)
  val FC_CLE   = UInt(2, SZ_VFCU_OP)
  val FC_MIN   = UInt(3, SZ_VFCU_OP)
  val FC_MAX   = UInt(4, SZ_VFCU_OP)
  val FC_CLASS = UInt(5, SZ_VFCU_OP)
}

trait VFVUConstants
{
  val SZ_VFVU_OP = 4

  val FV_X     = UInt.DC(SZ_VFVU_OP)
  val FV_CLTF  = UInt(0, SZ_VFVU_OP)
  val FV_CLUTF = UInt(1, SZ_VFVU_OP)
  val FV_CWTF  = UInt(2, SZ_VFVU_OP)
  val FV_CWUTF = UInt(3, SZ_VFVU_OP)
  val FV_CFTL  = UInt(4, SZ_VFVU_OP)
  val FV_CFTLU = UInt(5, SZ_VFVU_OP)
  val FV_CFTW  = UInt(6, SZ_VFVU_OP)
  val FV_CFTWU = UInt(7, SZ_VFVU_OP)
  val FV_CDTS  = UInt(8, SZ_VFVU_OP)
  val FV_CDTH  = UInt(9, SZ_VFVU_OP)
  val FV_CSTD  = UInt(10, SZ_VFVU_OP)
  val FV_CSTH  = UInt(11, SZ_VFVU_OP)
  val FV_CHTD  = UInt(12, SZ_VFVU_OP)
  val FV_CHTS  = UInt(13, SZ_VFVU_OP)
}

trait VMUConstants extends LaneConstants
{
  val SZ_VMU_OP = 1 + M_SZ

  val VM_X = Bits.DC(SZ_VMU_OP)

  val VM_VLD  = (Bool(true)  ## M_XRD)
  val VM_VST  = (Bool(true)  ## M_XWR)
  val VM_VLDX = (Bool(false) ## M_XRD)
  val VM_VSTX = (Bool(false) ## M_XWR)

  val VM_AMO_SWAP = (Bool(false) ## M_XA_SWAP)
  val VM_AMO_ADD  = (Bool(false) ## M_XA_ADD)
  val VM_AMO_XOR  = (Bool(false) ## M_XA_XOR)
  val VM_AMO_OR   = (Bool(false) ## M_XA_OR)
  val VM_AMO_AND  = (Bool(false) ## M_XA_AND)
  val VM_AMO_MIN  = (Bool(false) ## M_XA_MIN)
  val VM_AMO_MAX  = (Bool(false) ## M_XA_MAX)
  val VM_AMO_MINU = (Bool(false) ## M_XA_MINU)
  val VM_AMO_MAXU = (Bool(false) ## M_XA_MAXU)

  def vmu_op_tvec(op: Bits) = op(M_SZ)
  def vmu_op_mcmd(op: Bits) = op(M_SZ-1, 0)

  def is_mcmd_load(cmd: Bits) = (cmd === M_XRD)
  def is_mcmd_store(cmd: Bits) = (cmd === M_XWR)
  def is_mcmd_amo(cmd: Bits) = isAMO(cmd)
  def is_mcmd_pfr(cmd: Bits) = (cmd === M_PFR)
  def is_mcmd_pfw(cmd: Bits) = (cmd === M_PFW)
  def is_mcmd_pf(cmd: Bits) = (is_mcmd_pfr(cmd) || is_mcmd_pfw(cmd))

  def is_mtype_byte(typ: Bits) = (typ === MT_B || typ === MT_BU)
  def is_mtype_halfword(typ: Bits) = (typ === MT_H || typ === MT_HU)
  def is_mtype_word(typ: Bits) = (typ === MT_W || typ === MT_WU)
  def is_mtype_doubleword(typ: Bits) = (typ === MT_D)
}

object Commands extends Commands
trait Commands
{
  // command bits for the vector command queue
  val CMD_X = Bits("b????_????",8)

  val CMD_VSETCFG = Bits("b00_0000_00",8)
  val CMD_VSETVL =  Bits("b00_0000_10",8)
  val CMD_VF =      Bits("b00_0000_11",8)
  val CMD_VFT =     Bits("b00_0001_11",8)

  val CMD_LDWB = Bits("b00_010_000",8)
  val CMD_STAC = Bits("b00_010_001",8)

  val CMD_VMSA =  Bits("b01_000_000",8)
  val CMD_VMSS =  Bits("b01_001_000",8)

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

  def is_cmd_pfw(cmd: Bits) = cmd(4)
}

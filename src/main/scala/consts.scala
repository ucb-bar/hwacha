package hwacha

import Chisel._
import Node._
import uncore.constants.MemoryOpConstants._

object Constants extends
  MachineConstants with
  DecodeConstants with
  VIUConstants with
  VIMUConstants with
  VIDUConstants with
  VFMUConstants with
  VFDUConstants with
  VFCUConstants with
  VFVUConstants with
  VMUConstants

trait MachineConstants {
  val SZ_D = 64
  val SZ_W = 32
  val SZ_H = 16
  val SZ_B = 8
}

trait HwachaDecodeConstants {
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

trait DecodeConstants {
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

trait VIUConstants {
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

trait VIMUConstants {
  val SZ_VIMU_OP = 2

  val IM_X    = UInt.DC(SZ_VIMU_OP)
  val IM_M    = UInt(0, SZ_VIMU_OP)
  val IM_MH   = UInt(1, SZ_VIMU_OP)
  val IM_MHSU = UInt(2, SZ_VIMU_OP)
  val IM_MHU  = UInt(3, SZ_VIMU_OP)
}

trait VIDUConstants {
  val SZ_VIDU_OP = 2

  val ID_X    = UInt.DC(SZ_VIDU_OP)
  val ID_DIV  = UInt(0, SZ_VIDU_OP)
  val ID_DIVU = UInt(1, SZ_VIDU_OP)
  val ID_REM  = UInt(2, SZ_VIDU_OP)
  val ID_REMU = UInt(3, SZ_VIDU_OP)
}

trait VFMUConstants {
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

trait VFDUConstants {
  val SZ_VFDU_OP = 1

  val FD_X    = UInt.DC(SZ_VFDU_OP)
  val FD_DIV  = UInt(0, SZ_VFDU_OP)
  val FD_SQRT = UInt(1, SZ_VFDU_OP)
}

trait VFCUConstants {
  val SZ_VFCU_OP = 3

  val FC_X     = UInt.DC(SZ_VFCU_OP)
  val FC_CEQ   = UInt(0, SZ_VFCU_OP)
  val FC_CLT   = UInt(1, SZ_VFCU_OP)
  val FC_CLE   = UInt(2, SZ_VFCU_OP)
  val FC_MIN   = UInt(3, SZ_VFCU_OP)
  val FC_MAX   = UInt(4, SZ_VFCU_OP)
  val FC_CLASS = UInt(5, SZ_VFCU_OP)
}

trait VFVUConstants {
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

trait VMUConstants {
  val SZ_VMU_MODE = 2

  val MM_X  = UInt.DC(SZ_VMU_MODE)
  val MM_VS = UInt(0, SZ_VMU_MODE) // vector strided
  val MM_VX = UInt(1, SZ_VMU_MODE) // vector indexed
  val MM_S  = UInt(2, SZ_VMU_MODE) // scalar

  def is_indexed(mode: Bits) = mode(0)
  def is_scalar(mode: Bits) = mode(1)
}

object Commands extends Commands
trait Commands {
  // command bits for the vector command queue
  val CMD_X = Bits("b???",3)

  val CMD_VSETCFG = Bits("b000",3)
  val CMD_VSETVL  = Bits("b001",3)
  val CMD_VF      = Bits("b010",3)
  val CMD_VFT     = Bits("b011",3)
  val CMD_VMSA    = Bits("b100",3)
  val CMD_VMSS    = Bits("b101",3)
}

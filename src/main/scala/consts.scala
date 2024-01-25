package hwacha

import chisel3._
import chisel3.util._

object HwachaConstants extends HwachaConstants
trait HwachaConstants
  extends MachineConstants
  with PrecisionConstants
  with HwachaDecodeConstants
  with DecodeConstants
  with VIUConstants
  with VIPUConstants
  with VIMUConstants
  with VIDUConstants
  with VFMUConstants
  with VFDUConstants
  with VFCUConstants
  with VFVUConstants
  with VRPUConstants
  with VMUConstants
  with SMUConstants

trait MachineConstants {
  val SZ_D = 64
  val SZ_W = 32
  val SZ_H = 16
  val SZ_B = 8

  val HwachaElementInstBytes = HwachaElementInstructions.VSTOP.getWidth/8
}

trait PrecisionConstants {
  val SZ_PREC = 2

  val PREC_X = BitPat("b??")
  val PREC_D = 0.U(SZ_PREC.W)
  val PREC_W = 1.U(SZ_PREC.W)
  val PREC_H = 2.U(SZ_PREC.W)
}

trait HwachaDecodeConstants {
  val VRT_X = BitPat("b?")
  val VRT_S = 0.U(1.W)
  val VRT_A = 1.U(1.W)

  val VR_X   = BitPat("b?")
  val VR_RS1 = 0.U(1.W)
  val VR_RD  = 1.U(1.W)

  val RIMM_X    = BitPat("b???")
  val RIMM_VLEN = 0.U(3.W)
  val RIMM_RS1  = 1.U(3.W)
  val RIMM_RS2  = 2.U(3.W)
  val RIMM_ADDR = 3.U(3.W)

  val RESP_X     = BitPat("b???")
  val RESP_NVL   = 0.U(3.W)
  val RESP_CAUSE = 1.U(3.W)
  val RESP_AUX   = 2.U(3.W)
  val RESP_CFG   = 3.U(3.W)
  val RESP_VL    = 4.U(3.W)

}

trait DecodeConstants {
  val Y = BitPat("b1")
  val N = BitPat("b0")
  val X = BitPat("b?")

  val M0 = 0.U(2.W)
  val MR = 1.U(2.W)
  val ML = 2.U(2.W)
  val MI = 3.U(2.W)

  val RX = BitPat("b??")
  val RS = 0.U(2.W)
  val RA = 1.U(2.W)
  val RP = 2.U(2.W)
  val RV = 3.U(2.W)

  val REG_SHR = 0.U(2.W)
  val REG_ADDR = 1.U(2.W)
  val REG_PRED = 2.U(2.W)
  val REG_VEC = 3.U(2.W)

  def reg_type(t: Bits, d: Bool, i: Bool) = Mux(d, Mux(i, REG_VEC, REG_SHR), t)

  val SZ_I = 2
  val IMM_X = BitPat("b??")
  val IMM_I = 0.U(SZ_I.W)
  val IMM_L = 1.U(SZ_I.W)
  val IMM_U = 2.U(SZ_I.W)

  val MT_SZ = 3
  val MT_X  = BitPat("b???")
  val MT_B  = "b000".U
  val MT_H  = "b001".U
  val MT_W  = "b010".U
  val MT_D  = "b011".U
  val MT_BU = "b100".U
  val MT_HU = "b101".U
  val MT_WU = "b110".U

  val DW__ = BitPat("b?")
  val DW32 = 0.U(1.W)
  val DW64 = 1.U(1.W)

  val FP_ = BitPat("b??")
  val FPS = 0.U(2.W)
  val FPD = 1.U(2.W)
  val FPH = 2.U(2.W)

  val SZ_BMUXSEL = 2
  val SZ_DW = 1
  val SZ_FP = 2

  val A1_X    = BitPat("b??")
  val A1_ZERO = 0.U(2.W)
  val A1_RS1  = 1.U(2.W)
  val A1_PC   = 2.U(2.W)

  val A2_X    = BitPat("b??")
  val A2_8    = 0.U(2.W)
  val A2_RS2  = 1.U(2.W)
  val A2_IMM  = 2.U(2.W)

  //riscv-opcode fields
  val OPC_VD  = 63.U
  val OPC_VS1 = 62.U
  val OPC_VS2 = 61.U
  val OPC_VS3 = 60.U
  val OPC_NEG = 32.U
}

trait VIUConstants {
  val SZ_VIU_OP = 5

  val I_X    = BitPat("b?????")
  val I_ADD  = 0.U(SZ_VIU_OP.W)
  val I_ADDU = 1.U(SZ_VIU_OP.W)
  val I_SLL  = 2.U(SZ_VIU_OP.W)
  val I_SLT  = 3.U(SZ_VIU_OP.W)
  val I_SLTU = 4.U(SZ_VIU_OP.W)
  val I_XOR  = 5.U(SZ_VIU_OP.W)
  val I_SRL  = 6.U(SZ_VIU_OP.W)
  val I_SRA  = 7.U(SZ_VIU_OP.W)
  val I_OR   = 8.U(SZ_VIU_OP.W)
  val I_AND  = 9.U(SZ_VIU_OP.W)
  val I_SUB  = 10.U(SZ_VIU_OP.W)
  val I_IDX  = 11.U(SZ_VIU_OP.W)
  val I_MOV0 = 12.U(SZ_VIU_OP.W)
  val I_FSJ  = 13.U(SZ_VIU_OP.W)
  val I_FSJN = 14.U(SZ_VIU_OP.W)
  val I_FSJX = 15.U(SZ_VIU_OP.W)
  val I_CEQ  = 16.U(SZ_VIU_OP.W)
  val I_CLT  = 17.U(SZ_VIU_OP.W)
  val I_CLTU = 18.U(SZ_VIU_OP.W)
}

trait VIPUConstants {
  val SZ_VIPU_OP = 8
}

trait VIMUConstants {
  val SZ_VIMU_OP = 2

  val IM_X    = BitPat("b??")
  val IM_M    = 0.U(SZ_VIMU_OP.W)
  val IM_MH   = 1.U(SZ_VIMU_OP.W)
  val IM_MHSU = 2.U(SZ_VIMU_OP.W)
  val IM_MHU  = 3.U(SZ_VIMU_OP.W)
}

trait VIDUConstants {
  val SZ_VIDU_OP = 2

  val ID_X    = BitPat("b??")
  val ID_DIV  = 0.U(SZ_VIDU_OP.W)
  val ID_DIVU = 1.U(SZ_VIDU_OP.W)
  val ID_REM  = 2.U(SZ_VIDU_OP.W)
  val ID_REMU = 3.U(SZ_VIDU_OP.W)
}

trait VFMUConstants {
  val SZ_VFMU_OP = 3

  val FM_X     = BitPat("b???")
  val FM_ADD   = 0.U(SZ_VFMU_OP.W)
  val FM_SUB   = 1.U(SZ_VFMU_OP.W)
  val FM_MUL   = 2.U(SZ_VFMU_OP.W)
  val FM_MADD  = 4.U(SZ_VFMU_OP.W)
  val FM_MSUB  = 5.U(SZ_VFMU_OP.W)
  val FM_NMSUB = 6.U(SZ_VFMU_OP.W)
  val FM_NMADD = 7.U(SZ_VFMU_OP.W)

  val IS_FM_OP_MA = (x: Bits) => x(2)
}

trait VFDUConstants {
  val SZ_VFDU_OP = 1

  val FD_X    = BitPat("b?")
  val FD_DIV  = 0.U(SZ_VFDU_OP.W)
  val FD_SQRT = 1.U(SZ_VFDU_OP.W)
}

trait VFCUConstants {
  val SZ_VFCU_OP = 3

  val FC_X     = BitPat("b???")
  val FC_CEQ   = 0.U(SZ_VFCU_OP.W)
  val FC_CLT   = 1.U(SZ_VFCU_OP.W)
  val FC_CLE   = 2.U(SZ_VFCU_OP.W)
  val FC_MIN   = 3.U(SZ_VFCU_OP.W)
  val FC_MAX   = 4.U(SZ_VFCU_OP.W)
  val FC_CLASS = 5.U(SZ_VFCU_OP.W)
}

trait VFVUConstants {
  val SZ_VFVU_OP = 4

  val FV_X     = BitPat("b????")
  val FV_CLTF  = 0.U(SZ_VFVU_OP.W)
  val FV_CLUTF = 1.U(SZ_VFVU_OP.W)
  val FV_CWTF  = 2.U(SZ_VFVU_OP.W)
  val FV_CWUTF = 3.U(SZ_VFVU_OP.W)
  val FV_CFTL  = 4.U(SZ_VFVU_OP.W)
  val FV_CFTLU = 5.U(SZ_VFVU_OP.W)
  val FV_CFTW  = 6.U(SZ_VFVU_OP.W)
  val FV_CFTWU = 7.U(SZ_VFVU_OP.W)
  val FV_CDTS  = 8.U(SZ_VFVU_OP.W)
  val FV_CDTH  = 9.U(SZ_VFVU_OP.W)
  val FV_CSTD  = 10.U(SZ_VFVU_OP.W)
  val FV_CSTH  = 11.U(SZ_VFVU_OP.W)
  val FV_CHTD  = 12.U(SZ_VFVU_OP.W)
  val FV_CHTS  = 13.U(SZ_VFVU_OP.W)
}

trait VRPUConstants {
  val SZ_VRPU_OP = 2

  val FR_X   = BitPat("b??")
  val FR_ALL = 0.U(SZ_VRPU_OP.W)
  val FR_ANY = 1.U(SZ_VRPU_OP.W)
}

trait VMUConstants {
  val SZ_VMU_MODE = 2

  val VM_X = BitPat("b??")
  val VM_U = 0.U(SZ_VMU_MODE.W) // unit-stride
  val VM_S = 1.U(SZ_VMU_MODE.W) // constant-stride
  val VM_I = 2.U(SZ_VMU_MODE.W) // indexed

  def vmu_unit(mode: UInt): Bool = (mode === VM_U).suggestName("vmu_unitWire")
  def vmu_indexed(mode: UInt): Bool = mode(1).suggestName("vmu_indexedWire")
}

trait SMUConstants {
  val SZ_SMU_CMD = 1

  val SM_X = BitPat("b?")
  val SM_L = 0.U(SZ_SMU_CMD.W)
  val SM_S = 1.U(SZ_SMU_CMD.W)
}

object Commands extends Commands
trait Commands {
  // command bits for the vector command queue
  val CMD_X = BitPat("b???")

  val CMD_VSETCFG = 0.U(3.W)
  val CMD_VSETVL  = 1.U(3.W)
  val CMD_VF      = 2.U(3.W)
  val CMD_VFT     = 3.U(3.W)
  val CMD_VMCA    = 4.U(3.W)
  val CMD_VMCS    = 5.U(3.W)
}

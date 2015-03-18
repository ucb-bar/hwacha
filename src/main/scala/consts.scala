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
  VMUConstants

trait MachineConstants
{
  val SZ_VLEN = 12
  val SZ_REGLEN = 6
  val SZ_REGCNT = 6

  val SZ_ADDR = 64
  val SZ_DATA = 66

  val SZ_XD = 64
  val SZ_XW = 32
  val SZ_XH = 16
  val SZ_XB = 8

  val N_XD = SZ_DATA / SZ_XD
  val N_XW = SZ_DATA / SZ_XW
  val N_XH = SZ_DATA / SZ_XH
  val N_XB = SZ_DATA / SZ_XB

  val SZ_FPD = SZ_XD + 1
  val SZ_FPS = SZ_XW + 1
  val SZ_FPH = SZ_XH

  val N_FPD = SZ_DATA / SZ_FPD
  val N_FPS = SZ_DATA / SZ_FPS
  val N_FPH = SZ_DATA / SZ_FPH
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
  val RA = Bits("b0001", 4)
  val RS = Bits("b0101", 4)
  val RV = Bits("b0011", 4)
  val RP = Bits("b0111", 4)
  val RX = Bits("b1111", 4)
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
  val SZ_VIU_OP = 5

  val FN_X    = Bits("b????")
  val FN_ADD  = Bits(0)
  val FN_SL   = Bits(1)
  val FN_XOR  = Bits(4)
  val FN_OR   = Bits(6)
  val FN_AND  = Bits(7)
  val FN_SR   = Bits(5)
  val FN_SEQ  = Bits(8)
  val FN_SNE  = Bits(9)
  val FN_SUB  = Bits(10)
  val FN_SRA  = Bits(11)
  val FN_SLT  = Bits(12)
  val FN_SGE  = Bits(13)
  val FN_SLTU = Bits(14)
  val FN_SGEU = Bits(15)

  val FN_DIV  = FN_XOR
  val FN_DIVU = FN_SR
  val FN_REM  = FN_OR
  val FN_REMU = FN_AND

  val FN_MUL    = FN_ADD
  val FN_MULH   = FN_SL
  val FN_MULHSU = FN_SLT
  val FN_MULHU  = FN_SLTU
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

trait VMUConstants extends LaneConstants
{
  val SZ_VMU_OP = 1 + M_SZ

  val VM_X = Bits.DC(SZ_VMU_OP)

  val VM_VLD = (Bool(true)  ## M_XRD)
  val VM_VST = (Bool(true)  ## M_XWR)
  val VM_ULD = (Bool(false) ## M_XRD)
  val VM_UST = (Bool(false) ## M_XWR)

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

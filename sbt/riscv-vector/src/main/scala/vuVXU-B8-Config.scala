package riscvVector {

import Chisel._
import Node._

object riscvVectorConfig{

  val INT_STAGES = UFix(2, 3)
  val IMUL_STAGES = UFix(3, 3)
  val FMA_STAGES = UFix(5, 3)
  val FCONV_STAGES = UFix(3, 3)

  val SHIFT_BUF_READ = 3
  val SHIFT_BUF_WRITE = FMA_STAGES + UFix(4)

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

  val VIU_X     = UFix(0, 5)
  val VIU_ADD   = UFix(1, 5)
  val VIU_SLL   = UFix(2, 5)
  val VIU_SLT   = UFix(3, 5)
  val VIU_SLTU  = UFix(4, 5)
  val VIU_XOR   = UFix(5, 5)
  val VIU_SRL   = UFix(6, 5)
  val VIU_SRA   = UFix(7, 5)
  val VIU_OR    = UFix(8, 5)
  val VIU_AND   = UFix(9, 5)
  val VIU_SUB   = UFix(10, 5)
  val VIU_IDX   = UFix(11, 5)
  val VIU_MOV   = UFix(12, 5)
  val VIU_FSJ   = UFix(13, 5)
  val VIU_FSJN  = UFix(14, 5)
  val VIU_FSJX  = UFix(15, 5)
  val VIU_FEQ   = UFix(16, 5)
  val VIU_FLT   = UFix(17, 5)
  val VIU_FLE   = UFix(18, 5)
  val VIU_FMIN  = UFix(19, 5)
  val VIU_FMAX  = UFix(20, 5)
  val VIU_MOVZ  = UFix(21, 5)
  val VIU_MOVN  = UFix(22, 5)

  // in the decode table
  val VAU0_X     = UFix(0, 2)
  val VAU0_M     = UFix(0, 2)
  val VAU0_MH    = UFix(1, 2)
  val VAU0_MHU   = UFix(2, 2)
  val VAU0_MHSU  = UFix(3, 2)

  // acutal ops
  /*
   val VAU0_32    {`DW32,`VAU0_M}
   val VAU0_32H   {`DW32,`VAU0_MH}
   val VAU0_32HU  {`DW32,`VAU0_MHU}
   val VAU0_32HSU {`DW32,`VAU0_MHSU}
   val VAU0_64    {`DW64,`VAU0_M}
   val VAU0_64H   {`DW64,`VAU0_MH}
   val VAU0_64HU  {`DW64,`VAU0_MHU}
   val VAU0_64HSU {`DW64,`VAU0_MHSU}
   */
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

  val SZ_ADDR = 32
  val SZ_INST = 32
  val SZ_DATA = 65
  val SZ_EXC = 5
  val SZ_XLEN = 64
  val SZ_FLEN = 65
  /*
   val DEF_ADDR [`SZ_ADDR-1:0]
   val DEF_INST [`SZ_INST-1:0]
   val DEF_DATA [`SZ_DATA-1:0] // data width
   val DEF_EXC [`SZ_EXC-1:0]
   val DEF_XLEN [`SZ_XLEN-1:0]
   val DEF_FLEN [`SZ_FLEN-1:0]
   * */
  val SZ_STALL = 5

  val RG_VLDQ = 4
  val RG_VSDQ = 3
  val RG_UTAQ = 2
  val RG_UTLDQ = 1
  val RG_UTSDQ = 0

  //val DEF_STALL [`SZ_STALL-1:0]

  val SZ_VIU_FN  = 11
  val SZ_VAU0_FN = 3
  val SZ_VAU1_FN = 6
  val SZ_VAU2_FN = 7
/*
   val RG_VIU_T  10:7
   val RG_VIU_T0 10:9
   val RG_VIU_T1 8:7
   val RG_VIU_DW 6
   val RG_VIU_FP 5
   val RG_VIU_FN 4:0

val RG_VAU1_FP 5
val RG_VAU1_RM 4:3
val RG_VAU1_FN 2:0
val RG_VAU2_FP 6
val RG_VAU2_RM 5:4
val RG_VAU2_FN 3:0
* */

  val RG_VIU_T0 = (in: UFix) => in(10,9).toBits
  val RG_VIU_T1 = (in: UFix) => in(8,7).toBits
  val RG_VIU_DW = (in: UFix) => in(6).toBits;
  val RG_VIU_FN = (in: UFix) => in(4, 0);
  val RG_VIU_FP = (in: UFix) => in(5).toBits;
  
  val DEF_VIU_FN  = SZ_VIU_FN
  val DEF_VAU0_FN = SZ_VAU0_FN
  val DEF_VAU1_FN = SZ_VAU1_FN
  val DEF_VAU2_FN = SZ_VAU2_FN

  val SZ_VLEN = 11
  val SZ_REGLEN = 6
  val SZ_REGCNT = 6

  val DEF_VLEN   = SZ_VLEN
  val DEF_REGLEN = SZ_REGLEN
  val DEF_REGCNT = SZ_REGCNT

  val SZ_BANK = 8
  val SZ_LGBANK = 3
  val SZ_LGBANK1 = 4
  val SZ_BVLEN = 3
  val SZ_BREGLEN = 8
  val SZ_BOPL = 2
  val SZ_BRPORT = SZ_BANK
  val SZ_BWPORT = 3

  val DEF_BANK    = SZ_BANK
  val DEF_BPTR    = SZ_LGBANK
  val DEF_BPTR1   = SZ_LGBANK
  val DEF_BPTR2   = SZ_LGBANK+1
  val DEF_BCNT    = SZ_LGBANK
  val DEF_BVLEN   = SZ_BVLEN
  val DEF_BREGLEN = SZ_BREGLEN
  val DEF_BOPL    = SZ_BOPL
  val DEF_BRPORT  = SZ_BRPORT
  val DEF_BWPORT  = SZ_BWPORT

}
}

package hwacha

import Chisel._
import Node._
import Constants._
import rocket.ALU._
import uncore.constants.MemoryOpConstants._
import ScalarFPUDecode._

object VTDecodeTable
{
  import HwachaElementInstructions._
                //   vd vs vt vr i      VIUfn     DW     alu1    alu2     fpu_fn    scalar mem cmd    mt    stop
                //   |  |  |  |  |      |         |      |       |        |         |      |   |      |
  val default = List(R_,R_,R_,R_,IMM_X, N,FN_X,   DW_X,  A1_X,   A2_X,  N,FX,       N,     N,  M_X,   MT_X, N)
  val table = Array(
    //Arith ops
    //VEIDX->    List(RV,R_,R_,R_,IMM_X,T,   N),

    VADD->       List(RX,RX,RX,R_,IMM_X,Y,FN_ADD ,DW_XPR,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VSUB->       List(RX,RX,RX,R_,IMM_X,Y,FN_SUB ,DW_XPR,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VSLL->       List(RX,RX,RX,R_,IMM_X,Y,FN_SL  ,DW_XPR,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VSLT->       List(RX,RX,RX,R_,IMM_X,Y,FN_SLT ,DW_XPR,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VSLTU->      List(RX,RX,RX,R_,IMM_X,Y,FN_SLTU,DW_XPR,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VXOR->       List(RX,RX,RX,R_,IMM_X,Y,FN_XOR ,DW_XPR,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VSRL->       List(RX,RX,RX,R_,IMM_X,Y,FN_SR  ,DW_XPR,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VSRA->       List(RX,RX,RX,R_,IMM_X,Y,FN_SRA ,DW_XPR,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VOR->        List(RX,RX,RX,R_,IMM_X,Y,FN_OR  ,DW_XPR,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VAND->       List(RX,RX,RX,R_,IMM_X,Y,FN_AND ,DW_XPR,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),

    VLUI->       List(RS,RS,R_,R_,IMM_U,Y,FN_ADD ,DW_XPR,A1_ZERO,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),
    VADDI->      List(RS,RS,R_,R_,IMM_I,Y,FN_ADD ,DW_XPR,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),
    VSLLI->      List(RS,RS,R_,R_,IMM_I,Y,FN_SL  ,DW_XPR,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),
    VSLTI->      List(RS,RS,R_,R_,IMM_I,Y,FN_SLT ,DW_XPR,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),
    VSLTIU->     List(RS,RS,R_,R_,IMM_I,Y,FN_SLTU,DW_XPR,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),
    VXORI->      List(RS,RS,R_,R_,IMM_I,Y,FN_XOR ,DW_XPR,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),
    VSRLI->      List(RS,RS,R_,R_,IMM_I,Y,FN_SR  ,DW_XPR,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),
    VSRAI->      List(RS,RS,R_,R_,IMM_I,Y,FN_SRA ,DW_XPR,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),
    VORI->       List(RS,RS,R_,R_,IMM_I,Y,FN_OR  ,DW_XPR,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),
    VANDI->      List(RS,RS,R_,R_,IMM_I,Y,FN_AND ,DW_XPR,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),

    VADDW->      List(RX,RX,RX,R_,IMM_X,Y,FN_ADD ,DW_32 ,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VSUBW->      List(RX,RX,RX,R_,IMM_X,Y,FN_SUB ,DW_32 ,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VSLLW->      List(RX,RX,RX,R_,IMM_X,Y,FN_SL  ,DW_32 ,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VSRLW->      List(RX,RX,RX,R_,IMM_X,Y,FN_SR  ,DW_32 ,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),
    VSRAW->      List(RX,RX,RX,R_,IMM_X,Y,FN_SRA ,DW_32 ,A1_RS1 ,A2_RS2,N,FX,       N,     N,  M_X,   MT_X, N),

    VADDIW->     List(RS,RS,R_,R_,IMM_I,Y,FN_ADD ,DW_32 ,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),
    VSLLIW->     List(RS,RS,R_,R_,IMM_I,Y,FN_SL  ,DW_32 ,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),
    VSRLIW->     List(RS,RS,R_,R_,IMM_I,Y,FN_SR  ,DW_32 ,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),
    VSRAIW->     List(RS,RS,R_,R_,IMM_I,Y,FN_SRA ,DW_32 ,A1_RS1 ,A2_IMM,N,FX,       Y,     N,  M_X,   MT_X, N),

    VSSSEGD ->   List(RS,RS,RS,R_,IMM_X,Y,FN_ADD, DW_XPR,A1_RS1, A2_RS2,N,FX,       Y,     Y,  M_XWR, MT_D, N),
    VLSSEGD ->   List(RS,RS,RS,R_,IMM_X,Y,FN_ADD, DW_XPR,A1_RS1, A2_RS2,N,FX,       Y,     Y,  M_XRD, MT_D, N),

    VFCVT_S_W -> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_S_W ,N,     N,  M_X,   MT_X, N),
    VFCVT_S_WU-> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_S_WU,N,     N,  M_X,   MT_X, N),
    VFCVT_S_L -> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_S_L ,N,     N,  M_X,   MT_X, N),
    VFCVT_S_LU-> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_S_LU,N,     N,  M_X,   MT_X, N),
    VFCVT_D_W -> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_D_W ,N,     N,  M_X,   MT_X, N),
    VFCVT_D_WU-> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_D_WU,N,     N,  M_X,   MT_X, N),
    VFCVT_D_L -> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_D_L ,N,     N,  M_X,   MT_X, N),
    VFCVT_D_LU-> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_D_LU,N,     N,  M_X,   MT_X, N),
    VFCLASS_S -> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCLASS_S ,N,     N,  M_X,   MT_X, N),
    VFCLASS_D -> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCLASS_D ,N,     N,  M_X,   MT_X, N),
    VFCVT_W_S -> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_W_S ,N,     N,  M_X,   MT_X, N),
    VFCVT_WU_S-> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_WU_S,N,     N,  M_X,   MT_X, N),
    VFCVT_L_S -> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_L_S ,N,     N,  M_X,   MT_X, N),
    VFCVT_LU_S-> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_LU_S,N,     N,  M_X,   MT_X, N),
    VFCVT_W_D -> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_W_D ,N,     N,  M_X,   MT_X, N),
    VFCVT_WU_D-> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_WU_D,N,     N,  M_X,   MT_X, N),
    VFCVT_L_D -> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_L_D ,N,     N,  M_X,   MT_X, N),
    VFCVT_LU_D-> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_LU_D,N,     N,  M_X,   MT_X, N),
    VFCVT_S_D -> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_S_D ,N,     N,  M_X,   MT_X, N),
    VFCVT_D_S -> List(RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FCVT_D_S ,N,     N,  M_X,   MT_X, N),
    VCMPFEQ_S -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FEQ_S    ,N,     N,  M_X,   MT_X, N),
    VCMPFLT_S -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FLT_S    ,N,     N,  M_X,   MT_X, N),
    VCMPFLE_S -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FLE_S    ,N,     N,  M_X,   MT_X, N),
    VCMPFEQ_D -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FEQ_D    ,N,     N,  M_X,   MT_X, N),
    VCMPFLT_D -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FLT_D    ,N,     N,  M_X,   MT_X, N),
    VCMPFLE_D -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FLE_D    ,N,     N,  M_X,   MT_X, N),
    VFSGNJ_S  -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FSGNJ_S  ,N,     N,  M_X,   MT_X, N),
    VFSGNJN_S -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FSGNJN_S ,N,     N,  M_X,   MT_X, N),
    VFSGNJX_S -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FSGNJX_S ,N,     N,  M_X,   MT_X, N),
    VFSGNJ_D  -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FSGNJ_D  ,N,     N,  M_X,   MT_X, N),
    VFSGNJN_D -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FSGNJN_D ,N,     N,  M_X,   MT_X, N),
    VFSGNJX_D -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FSGNJX_D ,N,     N,  M_X,   MT_X, N),
    VFMIN_S   -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FMIN_S   ,N,     N,  M_X,   MT_X, N),
    VFMAX_S   -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FMAX_S   ,N,     N,  M_X,   MT_X, N),
    VFMIN_D   -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FMIN_D   ,N,     N,  M_X,   MT_X, N),
    VFMAX_D   -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FMAX_D   ,N,     N,  M_X,   MT_X, N),
    VFADD_S   -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FADD_S   ,N,     N,  M_X,   MT_X, N),
    VFSUB_S   -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FSUB_S   ,N,     N,  M_X,   MT_X, N),
    VFMUL_S   -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FMUL_S   ,N,     N,  M_X,   MT_X, N),
    VFADD_D   -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FADD_D   ,N,     N,  M_X,   MT_X, N),
    VFSUB_D   -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FSUB_D   ,N,     N,  M_X,   MT_X, N),
    VFMUL_D   -> List(RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FMUL_D   ,N,     N,  M_X,   MT_X, N),
    VFMADD_S  -> List(RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FMADD_S  ,N,     N,  M_X,   MT_X, N),
    VFMSUB_S  -> List(RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FMSUB_S  ,N,     N,  M_X,   MT_X, N),
    VFNMADD_S -> List(RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FNMADD_S ,N,     N,  M_X,   MT_X, N),
    VFNMSUB_S -> List(RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FNMSUB_S ,N,     N,  M_X,   MT_X, N),
    VFMADD_D  -> List(RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FMADD_D  ,N,     N,  M_X,   MT_X, N),
    VFMSUB_D  -> List(RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FMSUB_D  ,N,     N,  M_X,   MT_X, N),
    VFNMADD_D -> List(RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FNMADD_D ,N,     N,  M_X,   MT_X, N),
    VFNMSUB_D -> List(RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  Y,FNMSUB_D ,N,     N,  M_X,   MT_X, N),

    VSTOP->      List(R_,R_,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,  N,FX,       N,     N,  M_X,   MT_X, Y)
    /*
    VMUL->       List(RX,RX,RX,R_,IMM_X,F,I_X,   F),
    VMULH->      List(RX,RX,RX,R_,IMM_X,F,I_X,   F),
    VMULHU->     List(RX,RX,RX,R_,IMM_X,F,I_X,   F),
    VMULHSU->    List(RX,RX,RX,R_,IMM_X,F,I_X,   F),
    VMULW->      List(RX,RX,RX,R_,IMM_X,F,I_X,   F),

    VFADD_H->    List(RF,RF,RF,R_,IMM_X,F,I_X,   F),
    VFSUB_H->    List(RF,RF,RF,R_,IMM_X,F,I_X,   F),
    VFMUL_H->    List(RF,RF,RF,R_,IMM_X,F,I_X,   F),

    VLSEGXB->    List(RX,RX,R_,R_,IMM_I,F,I_X,   F),
    VLSEGXH->    List(RX,RX,R_,R_,IMM_I,F,I_X,   F),
    VLSEGXW->    List(RX,RX,R_,R_,IMM_I,F,I_X,   F),
    VLSEGXD->    List(RX,RX,R_,R_,IMM_I,F,I_X,   F),
    VLSEGXBU->   List(RX,RX,R_,R_,IMM_I,F,I_X,   F),
    VLSEGXHU->   List(RX,RX,R_,R_,IMM_I,F,I_X,   F),
    VLSEGXWU->   List(RX,RX,R_,R_,IMM_I,F,I_X,   F),
    VSSEGXB->    List(R_,RX,RX,R_,IMM_S,F,I_X,   F),
    VSSEGXH->    List(R_,RX,RX,R_,IMM_S,F,I_X,   F),
    VSSEGXW->    List(R_,RX,RX,R_,IMM_S,F,I_X,   F),
    VSSEGXD->    List(R_,RX,RX,R_,IMM_S,F,I_X,   F),
    VAMOADD_W->  List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOXOR_W->  List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOOR_W->   List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOAND_W->  List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOMIN_W->  List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOMAX_W->  List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOMINU_W-> List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOMAXU_W-> List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOSWAP_W-> List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOADD_D->  List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOXOR_D->  List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOOR_D->   List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOAND_D->  List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOMIN_D->  List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOMAX_D->  List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOMINU_D-> List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOMAXU_D-> List(RX,RX,RX,R_,IMM_0,F,I_X,   F),
    VAMOSWAP_D-> List(RX,RX,RX,R_,IMM_0,F,I_X,   F),

    VSTOP->      List(R_,R_,R_,R_,IMM_X,F,I_X,   T)
    */
  )
}

class IntCtrlSigs extends Bundle
{
  val vdi = Bits(width = RX.getWidth)
  val vri = Bits(width = RX.getWidth)
  val vsi = Bits(width = RX.getWidth)
  val vti = Bits(width = RX.getWidth)
  val sel_imm = Bits(width = IMM_X.getWidth)
  val viu_val = Bool()
  val alu_fn = Bits(width = FN_X.getWidth)
  val alu_dw = Bool()
  val sel_alu2 = Bits(width = A2_X.getWidth)
  val sel_alu1 = Bits(width = A1_X.getWidth)
  val fpu_val = Bool()
  val fpu_fn = Bits(width = FX.getWidth)
  val decode_scalar = Bool()
  val vmu_val = Bool()
  val vmu_cmd = Bits(width = M_X.getWidth)
  val vmu_type = Bits(width = MT_X.getWidth)
  val decode_stop = Bool()

  def decode(inst: UInt, table: Iterable[(UInt, List[UInt])]) = {
    val decoder = rocket.DecodeLogic(inst, VTDecodeTable.default, table)
    Vec(vdi, vri, vsi, vti, sel_imm, viu_val, alu_fn, alu_dw, sel_alu1,
        sel_alu2, fpu_val, fpu_fn, decode_scalar, vmu_val, vmu_cmd, vmu_type, decode_stop) := decoder
    this
  }
}

class CtrlDpathIO extends HwachaBundle
{
  val inst    = Bits(INPUT, 64)
  val ex_inst    = Bits(INPUT, 64)
  val wb_inst    = Bits(INPUT, 64)
  val killd   = Bool(OUTPUT)
  val ren     = Vec.fill(3)(Bool(OUTPUT))
  val ex_ctrl = new IntCtrlSigs().asOutput()
  val ex_valid = Bool(OUTPUT)
  val ex_waddr = Bits(INPUT, log2Up(nsregs))
  val wb_ctrl = new IntCtrlSigs().asOutput()
  val wb_valid = Bool(OUTPUT)
  val wb_waddr = Bits(INPUT, log2Up(nsregs))
  val wb_wen   = Bool(OUTPUT)
  val retire   = Bool(OUTPUT)
  val bypass = Vec.fill(3)(Bool(OUTPUT))
  val bypass_src = Vec.fill(3)(Bits(OUTPUT, SZ_BYP))
  val pending_mem_reg = UInt(INPUT)
  val fire_fpu = Bool(OUTPUT)
  val pending_fpu = Bool(INPUT)
  val pending_fpu_reg = UInt(INPUT)
  val swrite  = Valid(new Write().asOutput())
  val awrite  = Valid(new Write().asOutput())
  val wb_vf_active = Bool(OUTPUT)
}

class ScalarCtrl(resetSignal: Bool = null) extends HwachaModule(_reset = resetSignal)
{
  import Commands._

  val io = new Bundle {
    val cmdq = new CMDQIO().flip

    val dpath = new CtrlDpathIO
    val fpu = new Bundle {
      val req = Decoupled(new FPInput())
      val resp = Decoupled(new FPResult()).flip
    }

    val vmu = new ScalarMemIO

    val imem = new FrontendIO

    val vf_active = Bool(OUTPUT)
    val pending_seq = Bool(OUTPUT)
    val pending_memop = Bool(OUTPUT)
  }

  class Scoreboard(n: Int)
  {
    def set(en: Bool, addr: UInt): Unit = update(en, _next | mask(en, addr))
    def clear(en: Bool, addr: UInt): Unit = update(en, _next & ~mask(en, addr))
    def read(addr: UInt): Bool = r(addr)
    def readBypassed(addr: UInt): Bool = _next(addr)

    private val r = Reg(init=Bits(0, n))
    private var _next = r
    private var ens = Bool(false)
    private def mask(en: Bool, addr: UInt) = Mux(en, UInt(1) << addr, UInt(0))
    private def update(en: Bool, update: UInt) = {
      _next = update
      ens = ens || en
      when (ens) { r := _next }
    }
  }
  val sboard = new Scoreboard(nsregs)
  val pending_mem = Reg(init=Bool(false))//scalar memop in flight

  val vf_active     = Reg(init=Bool(false))
  val ex_vf_active  = Reg(init=Bool(false))
  val wb_vf_active  = Reg(init=Bool(false))
  val vf_pc         = Reg(UInt())

  val pending_seq   = Reg(init=Bool(false))
  val pending_memop = Reg(init=Bool(false))

  io.vf_active    := vf_active
  io.pending_seq   := pending_seq
  io.pending_memop := pending_memop

  val decode_vmss = io.cmdq.cmd.valid && io.cmdq.cmd.bits === CMD_VMSS
  val decode_vmsa = io.cmdq.cmd.valid && io.cmdq.cmd.bits === CMD_VMSA
  val decode_vf   = io.cmdq.cmd.valid && io.cmdq.cmd.bits === CMD_VF
  val decode_vft  = io.cmdq.cmd.valid && io.cmdq.cmd.bits === CMD_VFT

  io.dpath.swrite.valid    := decode_vmss && io.cmdq.rd.valid
  io.dpath.swrite.bits.rd  := io.cmdq.rd.bits
  io.dpath.swrite.bits.imm := io.cmdq.imm.bits

  io.dpath.awrite.valid    := decode_vmsa && io.cmdq.rd.valid
  io.dpath.awrite.bits.rd  := io.cmdq.rd.bits
  io.dpath.awrite.bits.imm := io.cmdq.imm.bits

  io.cmdq.cmd.ready := !vf_active 
  io.cmdq.imm.ready := !vf_active
  io.cmdq.rd.ready  := !vf_active && (decode_vmss || decode_vmsa)

  //default values
  io.imem.req.valid := vf_active
  io.imem.req.bits.pc := vf_pc
  io.imem.req.bits.npc := vf_pc + UInt(8)
  io.imem.req.bits.nnpc := vf_pc + UInt(2*8)
  io.imem.invalidate := Bool(false)
  io.imem.resp.ready := vf_active

  //decode
  val id_ctrl = new IntCtrlSigs().decode(io.dpath.inst, VTDecodeTable.table)
  val ex_ctrl = Reg(new IntCtrlSigs)
  val wb_ctrl = Reg(new IntCtrlSigs)
  io.dpath.ex_ctrl := ex_ctrl
  io.dpath.wb_ctrl := wb_ctrl

  val ex_reg_valid = Reg(Bool())
  val wb_reg_valid = Reg(Bool())
  val wb_reg_replay = Reg(Bool())

  val ctrl_killd = Bool()
  val ctrl_killx = Bool()
  val ctrl_killm = Bool()

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Array(
      vf_active,
      io.cmdq.cmd.valid, io.cmdq.imm.valid)
    rvs.filter(_ != exclude).reduce(_&&_) && (Bool(true) :: include.toList).reduce(_&&_)
  }

  //start and stop vf block and i$ fetching
  when(fire(vf_active,decode_vf))
  {
    vf_active := Bool(true)
    vf_pc := io.cmdq.imm.bits
  }
  when(id_ctrl.decode_stop && vf_active)
  {
    vf_active := Bool(false)
  }
  io.dpath.wb_vf_active    := wb_vf_active

  val vd_val :: vd_scalar :: vd_sp :: vd_dyn :: Nil = parse_rinfo(id_ctrl.vdi)
  val vr_val :: vr_scalar :: vr_sp :: vr_dyn :: Nil = parse_rinfo(id_ctrl.vri)
  val vs_val :: vs_scalar :: vs_sp :: vs_dyn :: Nil = parse_rinfo(id_ctrl.vsi)
  val vt_val :: vt_scalar :: vt_sp :: vt_dyn :: Nil = parse_rinfo(id_ctrl.vti)
  io.dpath.ren(0) := vr_val && vr_scalar
  io.dpath.ren(1) := vs_val && vs_scalar
  io.dpath.ren(2) := vt_val && vt_scalar

  val ex_vd_val :: ex_vd_scalar :: ex_vd_sp :: ex_vd_dyn :: Nil = parse_rinfo(ex_ctrl.vdi)

  val wb_vd_val :: wb_vd_scalar :: wb_vd_sp :: wb_vd_dyn :: Nil = parse_rinfo(wb_ctrl.vdi)

  val id_scalar_dest = !id_ctrl.fpu_val && (id_ctrl.decode_scalar || (vd_val && vd_scalar) || (vd_dyn && io.dpath.inst(OPC_VD)))
  val id_scalar_src1 = id_ctrl.decode_scalar || (vr_val && vr_scalar) || (vr_dyn && io.dpath.inst(OPC_VS1))
  val id_scalar_src2 = id_ctrl.decode_scalar || (vs_val && vs_scalar) || (vs_dyn && io.dpath.inst(OPC_VS2))
  val id_scalar_src3 = id_ctrl.decode_scalar || (vt_val && vt_scalar) || (vt_dyn && io.dpath.inst(OPC_VS3))

  //COLIN FIXME: only send over dynamic bits from ex/wb_inst ala rockets ex_waddr
  val ex_scalar_dest = !ex_ctrl.fpu_val && (ex_ctrl.decode_scalar || (ex_vd_val && ex_vd_scalar) || (ex_vd_dyn && io.dpath.ex_inst(OPC_VD)))

  val wb_scalar_dest = !wb_ctrl.fpu_val && (wb_ctrl.decode_scalar || (wb_vd_val && wb_vd_scalar) || (wb_vd_dyn && io.dpath.wb_inst(OPC_VD)))

  val id_waddr = io.dpath.inst(23,16)
  val id_raddrs1 = io.dpath.inst(31,24)
  val id_raddrs2 = io.dpath.inst(40,33)
  val id_raddrs3 = io.dpath.inst(48,41)

  val ex_waddr   = io.dpath.ex_waddr

  val wb_waddr   = io.dpath.wb_waddr

  //COLIN FIXME: do we have any scalar instructions with no destination?
  val id_scalar_inst = id_scalar_dest &&
                       (id_scalar_src1 && vr_val)
                       (id_scalar_src2 && vs_val)
                       (id_scalar_src3 && vt_val)
  //assuming all scalar instructions have a dest

  val bypassDst = Array(id_raddrs1, id_raddrs2, id_raddrs3)
  val bypassSrc = Array.fill(NBYP)((Bool(true), UInt(0)))
  bypassSrc(BYP_EX) = (ex_reg_valid && ex_scalar_dest, ex_waddr)

  val doBypass = bypassDst.map(d => bypassSrc.map(s => s._1 && s._2 === d))
  for (i <- 0 until io.dpath.bypass.size) {
    io.dpath.bypass(i) := doBypass(i).reduce(_||_)
    io.dpath.bypass_src(i) := PriorityEncoder(doBypass(i))
  }

  val id_ctrl_wen_not0 = vd_val && vd_scalar && vd_sp && id_waddr != UInt(0)
  val id_ctrl_rens1_not0 = vr_val && vr_scalar && vr_sp && id_raddrs1 != UInt(0)
  val id_ctrl_rens2_not0 = vs_val && vs_scalar && vs_sp && id_raddrs2 != UInt(0)
  val id_ctrl_rens3_not0 = vt_val && vt_scalar && vt_sp && id_raddrs3 != UInt(0)

  // stall for RAW/WAW hazards on loads, AMOs, and mul/div in execute stage.
  val ex_cannot_bypass = ex_ctrl.vmu_val
  val data_hazard_ex = ex_scalar_dest &&
    (id_ctrl_rens1_not0 && id_raddrs1 === ex_waddr ||
     id_ctrl_rens2_not0 && id_raddrs2 === ex_waddr ||
     id_ctrl_wen_not0   && id_waddr  === ex_waddr)

  val id_ex_hazard = ex_reg_valid && (data_hazard_ex && ex_cannot_bypass)

  val wb_set_sboard = wb_ctrl.vmu_val || wb_ctrl.fpu_val
  // stall for RAW/WAW hazards on load/AMO misses and mul/div in writeback.
  val data_hazard_wb = wb_scalar_dest &&
     (id_ctrl_rens1_not0 && id_raddrs1 === wb_waddr ||
      id_ctrl_rens2_not0 && id_raddrs2 === wb_waddr ||
      id_ctrl_wen_not0   && id_waddr  === wb_waddr)
  val id_wb_hazard = wb_reg_valid && (data_hazard_wb && wb_set_sboard)

  //Stall second load/store on decode
  val id_second_mem = id_ctrl.vmu_val && pending_mem
  //Stall second fpu on decode
  val fpu_hazard = id_ctrl.fpu_val && io.dpath.pending_fpu

  //stall on RAW/WAW hazards on loads until data returns
  //stall on WAW hazards on stores until translation succeeds
  //COLIN FIXME: The current logic doesn't check that the sboard is set
  //because of a load rather than another int op we can bypass from
  val mem_hazard = pending_mem && (
                   id_ctrl_rens1_not0 && sboard.read(id_raddrs1) ||
                   id_ctrl_rens2_not0 && sboard.read(id_raddrs2) ||
                   id_ctrl_rens3_not0 && sboard.read(id_raddrs3) ||
                   id_ctrl_wen_not0 && sboard.read(id_waddr) )

  val id_sboard_hazard = 
                   (id_ctrl_rens1_not0 && sboard.readBypassed(id_raddrs1) ||
                   id_ctrl_rens2_not0 && sboard.readBypassed(id_raddrs2) ||
                   id_ctrl_rens1_not0 && sboard.readBypassed(id_waddr))

  sboard.set(wb_set_sboard && io.dpath.wb_wen, wb_waddr)

  //Only one outstanding scalar load/store
  assert(!(io.vmu.loadData.valid && io.vmu.storeAck), "only one scalar store/load should be active")
  //on loadData.valid or store_ack we clear the sboard for its addr
  //on fpu resp valid we clear the sboard for its addr
  val clear_mem = io.vmu.loadData.valid || io.vmu.storeAck 
  val clear_fpu = io.fpu.resp.valid
  val sboard_clear_addr = Mux(clear_mem, io.dpath.pending_mem_reg, io.dpath.pending_fpu_reg)
  sboard.clear(clear_mem || clear_fpu, sboard_clear_addr)
 
  val ctrl_stalld = id_ex_hazard || id_wb_hazard || 
                    id_sboard_hazard || id_second_mem || mem_hazard || fpu_hazard

  io.dpath.killd := !vf_active || ctrl_stalld

  //excute
  ctrl_killd := !vf_active || !io.imem.resp.valid || ctrl_stalld 
  ex_reg_valid := !ctrl_killd
  io.dpath.ex_valid := ex_reg_valid

  when (!ctrl_killd) {
    vf_pc := vf_pc + UInt(8)
    ex_ctrl := id_ctrl
  }
  when (!ctrl_stalld) { ex_vf_active := vf_active }

  // replay inst in ex stage
  val replay_ex_structural = ex_ctrl.vmu_val && pending_mem ||
                             ex_ctrl.fpu_val && !io.fpu.req.ready
  //if either the vmu or fpu needs to writeback we need to replay
  val vmu_kill_ex = ex_reg_valid && ex_scalar_dest && io.vmu.loadData.valid
  val fpu_kill_ex = ex_reg_valid && ex_scalar_dest && io.fpu.resp.valid
  val replay_ex = ex_reg_valid && (replay_ex_structural || vmu_kill_ex || fpu_kill_ex)

  //fpu
  val fpu_fn = new FPUFn().fromBits(ex_ctrl.fpu_fn)
  io.fpu.req.bits.cmd     := fpu_fn.cmd     
  io.fpu.req.bits.ldst    := fpu_fn.ldst    
  io.fpu.req.bits.wen     := fpu_fn.wen     
  io.fpu.req.bits.ren1    := fpu_fn.ren1    
  io.fpu.req.bits.ren2    := fpu_fn.ren2    
  io.fpu.req.bits.ren3    := fpu_fn.ren3    
  io.fpu.req.bits.swap23  := fpu_fn.swap23  
  io.fpu.req.bits.single  := fpu_fn.single  
  io.fpu.req.bits.fromint := fpu_fn.fromint 
  io.fpu.req.bits.toint   := fpu_fn.toint   
  io.fpu.req.bits.fastpipe:= fpu_fn.fastpipe
  io.fpu.req.bits.fma     := fpu_fn.fma     
  io.fpu.req.bits.round   := fpu_fn.round   
  io.fpu.req.valid := ex_reg_valid && ex_ctrl.fpu_val
  io.dpath.fire_fpu := Bool(false)
  when(io.fpu.req.fire()){
    io.dpath.fire_fpu := Bool(true)
  }

  //memory
  io.vmu.op.valid := ex_reg_valid && ex_ctrl.vmu_val
  io.vmu.op.bits.fn.cmd := ex_ctrl.vmu_cmd
  io.vmu.op.bits.fn.mt := ex_ctrl.vmu_type
  when(io.vmu.op.fire()){
    pending_mem := Bool(true)
  }
  when(clear_mem){
    pending_mem := Bool(false)
  }
 
  //writeback
  ctrl_killx := replay_ex || !ex_reg_valid 
  wb_reg_valid := !ctrl_killx
  wb_reg_replay := replay_ex
  io.dpath.wb_valid := wb_reg_valid

  val replay_wb = wb_reg_replay

  when (!ctrl_killx) {
    wb_ctrl := ex_ctrl
  }
  when (!ctrl_stalld) { wb_vf_active := ex_vf_active }

  assert(!(io.vmu.loadData.valid && wb_scalar_dest), "load result and scalar wb conflict")

  io.dpath.retire := wb_reg_valid && !replay_wb
  io.dpath.wb_wen := wb_vf_active && wb_reg_valid && (io.vmu.loadData.valid || io.fpu.resp.valid || wb_scalar_dest)
  /*
  
  //COLIN FIXME: only recode when sending to shared rocket fpu
  val encode_sp = hardfloat.floatNToRecodedFloatN(io.vcmdq.imm1.bits, 23, 9)
  val encode_dp = hardfloat.floatNToRecodedFloatN(io.vcmdq.imm1.bits, 52, 12)

  */
}

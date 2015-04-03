package hwacha

import Chisel._
import Node._
import Constants._
import HwachaElementInstructions._
import rocket.ALU._
import uncore.constants.MemoryOpConstants._
import ScalarFPUDecode._

class IntCtrlSigs extends Bundle
{
  val ival = Bool()
  val decode_scalar = Bool()
  val vdi = Bits(width = RX.getWidth)
  val vs1i = Bits(width = RX.getWidth)
  val vs2i = Bits(width = RX.getWidth)
  val vs3i = Bits(width = RX.getWidth)
  val sel_imm = Bits(width = IMM_X.getWidth)
  val alu_fn = Bits(width = FN_X.getWidth)
  val alu_dw = Bool()
  val alu_sel2 = Bits(width = A2_X.getWidth)
  val alu_sel1 = Bits(width = A1_X.getWidth)
  val fpu_val = Bool()
  val fpu_fp = Bits(width = FP_.getWidth)
  val fpu_fn = Bits(width = FX.getWidth)
  val mem_val = Bool()
  val mem_cmd = Bits(width = M_X.getWidth)
  val mem_type = Bits(width = MT_X.getWidth)
  val decode_stop = Bool()
  val viu_val = Bool()
  val viu_fn = Bits(width = I_X.getWidth)
  val vimu_val = Bool()
  val vimu_fn = Bits(width = IM_X.getWidth)
  val vidu_val = Bool()
  val vidu_fn = Bits(width = ID_X.getWidth)
  val vfmu_val = Bool()
  val vfmu_fn = Bits(width = FM_X.getWidth)
  val vfdu_val = Bool()
  val vfdu_fn = Bits(width = FD_X.getWidth)
  val vfcu_val = Bool()
  val vfcu_fn = Bits(width = FC_X.getWidth)
  val vfvu_val = Bool()
  val vfvu_fn = Bits(width = FV_X.getWidth)
  val vmu_val = Bool()
  val vmu_fn = Bits(width = VM_X.getWidth)

  def decode(inst: UInt, table: Iterable[(UInt, List[UInt])]) = {
    val decoder = rocket.DecodeLogic(inst, ScalarDecode.default, table)
    Vec(ival, decode_scalar, decode_stop,
        vdi, vs1i, vs2i, vs3i, sel_imm,
        alu_fn, alu_dw, alu_sel1, alu_sel2,
        fpu_val, fpu_fp, fpu_fn, mem_val,
        mem_cmd, mem_type,
        viu_val, viu_fn,
        vimu_val, vimu_fn,
        vidu_val, vidu_fn,
        vfmu_val, vfmu_fn,
        vfdu_val, vfdu_fn,
        vfcu_val, vfcu_fn,
        vfvu_val, vfvu_fn,
        vmu_val, vmu_fn) := decoder
    this
  }
}

abstract trait VFDecodeTable
{

  //             scalar? stop?                                              fpu?            mem?          viu?     vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?      vmu?
  //              val? | | d  s1 s2 s3 imm   alufn   dw     sel1    sel2    | fp  fn        | cmd   mt    | fn     | fn      | fn      | fn       | fn      | fn       | fn       | fn
  //                 | | | |  |  |  |  |     |       |      |       |       | |   |         | |     |     | |      | |       | |       | |        | |       | |        | |        | |
  val default = List(N,N,N,R_,R_,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X)

  val table: Array[(UInt, List[UInt])]
}

object ScalarDecode extends VFDecodeTable
{
  val table = Array(
  //             scalar? stop?                                              fpu?            mem?          viu?     vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?      vmu?
  //              val? | | d  s1 s2 s3 imm   alufn   dw     sel1    sel2    | fp  fn        | cmd   mt    | fn     | fn      | fn      | fn       | fn      | fn       | fn       | fn
  //                 | | | |  |  |  |  |     |       |      |       |       | |   |         | |     |     | |      | |       | |       | |        | |       | |        | |        | |
    VSSSEGD   ->List(Y,Y,N,R_,RS,RS,R_,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,FX,       Y,M_XWR,MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VLSSEGD   ->List(Y,Y,N,RS,RS,R_,R_,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,FX,       Y,M_XRD,MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),

    VLUI      ->List(Y,Y,N,RS,RS,R_,R_,IMM_U,FN_ADD, DW_XPR,A1_ZERO,A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VADDI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_ADD, DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSLLI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SL,  DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSLTI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SLT, DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSLTIU    ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SLTU,DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VXORI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_XOR, DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSRLI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SR,  DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSRAI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SRA, DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VORI      ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_OR,  DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VANDI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_AND, DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VADDIW    ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_ADD, DW_32, A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSLLIW    ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SL,  DW_32, A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSRLIW    ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SR,  DW_32, A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSRAIW    ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SRA, DW_32, A1_RS1, A2_IMM, N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),

    VSTOP     ->List(Y,Y,Y,R_,R_,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X))
}

object VectorMemoryDecode extends VFDecodeTable
{
  val table = Array(
  //             scalar? stop?                                              fpu?            mem?          viu?     vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?      vmu?
  //              val? | | d  s1 s2 s3 imm   alufn   dw     sel1    sel2    | fp  fn        | cmd   mt    | fn     | fn      | fn      | fn       | fn      | fn       | fn       | fn
  //                 | | | |  |  |  |  |     |       |      |       |       | |   |         | |     |     | |      | |       | |       | |        | |       | |        | |        | |
    VLSEGB    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VLSEGH    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VLSEGW    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VLSEGD    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VLSEGBU   ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_BU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VLSEGHU   ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_HU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VLSEGWU   ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_WU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VSSEGB    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VST),
    VSSEGH    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VST),
    VSSEGW    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VST),
    VSSEGD    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VST),

    VLSEGSTB  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VLSEGSTH  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VLSEGSTW  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VLSEGSTD  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VLSEGSTBU ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_BU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VLSEGSTHU ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_HU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VLSEGSTWU ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_WU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLD),
    VSSEGSTB  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VST),
    VSSEGSTH  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VST),
    VSSEGSTW  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VST),
    VSSEGSTD  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VST),

    VLSEGXB   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLDX),
    VLSEGXH   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLDX),
    VLSEGXW   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLDX),
    VLSEGXD   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLDX),
    VLSEGXBU  ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_BU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLDX),
    VLSEGXHU  ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_HU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLDX),
    VLSEGXWU  ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_WU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VLDX),
    VSSEGXB   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VSTX),
    VSSEGXH   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VSTX),
    VSSEGXW   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VSTX),
    VSSEGXD   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_VSTX),

    VAMOSWAP_W->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_SWAP),
    VAMOADD_W ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_ADD),
    VAMOXOR_W ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_XOR),
    VAMOAND_W ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_AND),
    VAMOOR_W  ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_OR),
    VAMOMIN_W ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_MIN),
    VAMOMAX_W ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_MAX),
    VAMOMINU_W->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_MINU),
    VAMOMAXU_W->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_MAXU),
    VAMOSWAP_D->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_SWAP),
    VAMOADD_D ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_ADD),
    VAMOXOR_D ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_XOR),
    VAMOAND_D ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_AND),
    VAMOOR_D  ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_OR),
    VAMOMIN_D ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_MIN),
    VAMOMAX_D ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_MAX),
    VAMOMINU_D->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_MINU),
    VAMOMAXU_D->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,VM_AMO_MAX))
}

object VectorArithmeticDecode extends VFDecodeTable
{
  val table = Array(
  //             scalar? stop?                                              fpu?            mem?          viu?     vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?      vmu?
  //              val? | | d  s1 s2 s3 imm   alufn   dw     sel1    sel2    | fp  fn        | cmd   mt    | fn     | fn      | fn      | fn       | fn      | fn       | fn       | fn
  //                 | | | |  |  |  |  |     |       |      |       |       | |   |         | |     |     | |      | |       | |       | |        | |       | |        | |        | |
    VEIDX     ->List(Y,N,N,RV,R_,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, Y,I_IDX, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VADD      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_ADD, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_ADD, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VADDU     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_ADD, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_ADDU,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSUB      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SUB, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_SUB, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSLL      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SL,  DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_SLL, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSLT      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SLT, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_SLT, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSLTU     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SLTU,DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_SLTU,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VXOR      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_XOR, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_XOR, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSRL      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SR,  DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_SRL, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSRA      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SRA, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_SRA, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VOR       ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_OR,  DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_OR,  N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VAND      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_AND, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_AND, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    // FIXME START
    VMUL      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   Y,IM_M,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VMULH     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   Y,IM_MH,  N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VMULHSU   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   Y,IM_MHSU,N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VMULHU    ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   Y,IM_MHU, N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VDIV      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   Y,ID_DIV, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VDIVU     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   Y,ID_DIVU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VREM      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   Y,ID_REM, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VREMU     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   Y,ID_REMU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    // FIXME END
    VADDW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_ADD, DW_32, A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_ADD, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSUBW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SUB, DW_32, A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_SUB, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSLLW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SL,  DW_32, A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_SLL, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSRLW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SR,  DW_32, A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_SRL, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VSRAW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SRA, DW_32, A1_RS1, A2_RS2, N,FP_,FX,       N,M_X,  MT_X, Y,I_SRA, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    // FIXME START
    VMULW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   Y,IM_M,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VDIVW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   Y,ID_DIV, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VDIVUW    ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   Y,ID_DIVU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VREMW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   Y,ID_REM, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VREMUW    ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   Y,ID_REMU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    // FIXME END

    VFMADD_D  ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FMADD_D,  N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFMSUB_D  ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FMSUB_D,  N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFNMADD_D ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FNMADD_D, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFNMSUB_D ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FNMSUB_D, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFADD_D   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FADD_D,   N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFSUB_D   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FSUB_D,   N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFMUL_D   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FMUL_D,   N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    // FIXME START
    VFDIV_D   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X,    N,VM_X),
    VFSQRT_D  ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X,    N,VM_X),
    // FIXME END
    VFSGNJ_D  ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FSGNJ_D,  N,M_X,  MT_X, Y,I_FSJ, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFSGNJN_D ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FSGNJN_D, N,M_X,  MT_X, Y,I_FSJN,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFSGNJX_D ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FSGNJX_D, N,M_X,  MT_X, Y,I_FSJX,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFMIN_D   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FMIN_D,   N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X,    N,VM_X),
    VFMAX_D   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FMAX_D,   N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X,    N,VM_X),
    VFCVT_D_S ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_D_S, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CSTD, N,VM_X),
    // FIXME START
    VFCVT_D_H ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CHTD, N,VM_X),
    // FIXME END
    VFCLASS_D ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCLASS_D, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X,    N,VM_X),

    VFMADD_S  ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FMADD_S,  N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFMSUB_S  ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FMSUB_S,  N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFNMADD_S ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FNMADD_S, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFNMSUB_S ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FNMSUB_S, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFADD_S   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FADD_S,   N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFSUB_S   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FSUB_S,   N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFMUL_S   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FMUL_S,   N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    // FIXME START
    VFDIV_S   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X,    N,VM_X),
    VFSQRT_S  ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X,    N,VM_X),
    // FIXME END
    VFSGNJ_S  ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FSGNJ_S,  N,M_X,  MT_X, Y,I_FSJ, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFSGNJN_S ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FSGNJN_S, N,M_X,  MT_X, Y,I_FSJN,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFSGNJX_S ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FSGNJX_S, N,M_X,  MT_X, Y,I_FSJX,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFMIN_S   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FMIN_S,   N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X,    N,VM_X),
    VFMAX_S   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FMAX_S,   N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X,    N,VM_X),
    VFCVT_S_D ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_S_D, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CDTS, N,VM_X),
    // FIXME START
    VFCVT_S_H ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CHTS, N,VM_X),
    // FIXME END
    VFCLASS_S ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCLASS_S, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X,    N,VM_X),

    // FIXME START
    VFMADD_H  ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFMSUB_H  ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFNMADD_H ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFNMSUB_H ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFADD_H   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFSUB_H   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFMUL_H   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFDIV_H   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X,    N,VM_X),
    VFSQRT_H  ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X,    N,VM_X),
    VFSGNJ_H  ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, Y,I_FSJ, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFSGNJN_H ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, Y,I_FSJN,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFSGNJX_H ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, Y,I_FSJX,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VFMIN_H   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X,    N,VM_X),
    VFMAX_H   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X,    N,VM_X),
    VFCVT_H_D ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CDTH, N,VM_X),
    VFCVT_H_S ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CSTH, N,VM_X),
    VFCLASS_H ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X,    N,VM_X),
    // FIXME END

    VFCVT_W_D ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_W_D, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW, N,VM_X),
    VFCVT_WU_D->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_WU_D,N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU,N,VM_X),
    VFCVT_L_D ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_L_D, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL, N,VM_X),
    VFCVT_LU_D->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_LU_D,N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU,N,VM_X),
    VFCVT_D_W ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_D_W, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF, N,VM_X),
    VFCVT_D_WU->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_D_WU,N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF,N,VM_X),
    VFCVT_D_L ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_D_L, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF, N,VM_X),
    VFCVT_D_LU->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_D_LU,N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF,N,VM_X),
    VFCVT_W_S ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_W_S, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW, N,VM_X),
    VFCVT_WU_S->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_WU_S,N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU,N,VM_X),
    VFCVT_L_S ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_L_S, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL, N,VM_X),
    VFCVT_LU_S->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_LU_S,N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU,N,VM_X),
    VFCVT_S_W ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_S_W, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF, N,VM_X),
    VFCVT_S_WU->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_S_WU,N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF,N,VM_X),
    VFCVT_S_L ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_S_L, N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF, N,VM_X),
    VFCVT_S_LU->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_S_LU,N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF,N,VM_X),
    // FIXME START
    VFCVT_W_H ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW, N,VM_X),
    VFCVT_WU_H->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU,N,VM_X),
    VFCVT_L_H ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL, N,VM_X),
    VFCVT_LU_H->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU,N,VM_X),
    VFCVT_H_W ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF, N,VM_X),
    VFCVT_H_WU->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF,N,VM_X),
    VFCVT_H_L ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF, N,VM_X),
    VFCVT_H_LU->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF,N,VM_X),
    // FIXME END

    // FIXME START
    VCMPEQ    ->List(Y,N,N,RP,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VCMPLT    ->List(Y,N,N,RP,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    VCMPLTU   ->List(Y,N,N,RP,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,VM_X),
    // FIXME END
    VCMPFEQ_D ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FEQ_D,    N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X,    N,VM_X),
    VCMPFLT_D ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FLT_D,    N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X,    N,VM_X),
    VCMPFLE_D ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FLE_D,    N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X,    N,VM_X),
    VCMPFEQ_S ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FEQ_S,    N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X,    N,VM_X),
    VCMPFLT_S ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FLT_S,    N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X,    N,VM_X),
    VCMPFLE_S ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FLE_S,    N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X,    N,VM_X),
    // FIXME START
    VCMPFEQ_H ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X,    N,VM_X),
    VCMPFLT_H ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X,    N,VM_X),
    VCMPFLE_H ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,M_X,  MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X,    N,VM_X))
    // FIXME END
}

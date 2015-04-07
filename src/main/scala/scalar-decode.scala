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
  val decode_stop = Bool()
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
  val vmu_val = Bool()
  val vmu_mode = Bits(width = MM_X.getWidth)
  val vmu_cmd = Bits(width = M_X.getWidth)
  val vmu_mt = Bits(width = MT_X.getWidth)
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

  def decode(inst: UInt, table: Iterable[(UInt, List[UInt])]) = {
    val decoder = rocket.DecodeLogic(inst, ScalarDecode.default, table)
    Vec(ival, decode_scalar, decode_stop,
        vdi, vs1i, vs2i, vs3i, sel_imm,
        alu_fn, alu_dw, alu_sel1, alu_sel2,
        fpu_val, fpu_fp, fpu_fn,
        vmu_val, vmu_mode, vmu_cmd, vmu_mt,
        viu_val, viu_fn,
        vimu_val, vimu_fn,
        vidu_val, vidu_fn,
        vfmu_val, vfmu_fn,
        vfdu_val, vfdu_fn,
        vfcu_val, vfcu_fn,
        vfvu_val, vfvu_fn) := decoder
    this
  }

  def active_vint(d: Int = 0) = viu_val
  def active_vimul(d: Int = 0) = vimu_val
  def active_vidiv(d: Int = 0) = vidu_val
  def active_vfma(d: Int = 0) = vfmu_val
  def active_vfdiv(d: Int = 0) = vfdu_val
  def active_vfcmp(d: Int = 0) = vfcu_val
  def active_vfconv(d: Int = 0) = vfvu_val
  def active_vamo(d: Int = 0) = vmu_val && isAMO(vmu_cmd)
  def active_vldx(d: Int = 0) = vmu_val && is_indexed(vmu_mode) && vmu_cmd === M_XRD
  def active_vstx(d: Int = 0) = vmu_val && is_indexed(vmu_mode) && vmu_cmd === M_XWR
  def active_vld(d: Int = 0) = vmu_val && !is_indexed(vmu_mode) && vmu_cmd === M_XRD
  def active_vst(d: Int = 0) = vmu_val && !is_indexed(vmu_mode) && vmu_cmd === M_XWR

  def fn_viu(d: Int = 0) = new VIUFn().fromBits(Cat(alu_dw, fpu_fp, viu_fn))
  def fn_vimu(d: Int = 0) = new VIMUFn().fromBits(Cat(alu_dw, vimu_fn))
  def fn_vidu(d: Int = 0) = new VIDUFn().fromBits(Cat(alu_dw, vidu_fn))
  def fn_vfmu(rm: Bits) = new VFMUFn().fromBits(Cat(fpu_fp, rm, vfmu_fn))
  def fn_vfdu(rm: Bits) = new VFDUFn().fromBits(Cat(fpu_fp, rm, vfdu_fn))
  def fn_vfcu(rm: Bits) = new VFCUFn().fromBits(Cat(fpu_fp, rm, vfcu_fn))
  def fn_vfvu(rm: Bits) = new VFVUFn().fromBits(Cat(fpu_fp, rm, vfvu_fn))
  def fn_vmu(d: Int = 0) = new VMUFn().fromBits(Cat(vmu_mode, vmu_cmd, vmu_mt))
}

abstract trait VFDecodeTable
{

  //             scalar? stop?                                              fpu?            vmu?                    viu?     vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?
  //              val? | | d  s1 s2 s3 imm   alufn   dw     sel1    sel2    | fp  fn        | mode  cmd       mt    | fn     | fn      | fn      | fn       | fn      | fn       | fn
  //                 | | | |  |  |  |  |     |       |      |       |       | |   |         | |     |         |     | |      | |       | |       | |        | |       | |        | |
  val default = List(N,N,N,R_,R_,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X)

  val table: Array[(UInt, List[UInt])]
}

object ScalarDecode extends VFDecodeTable
{
  val table = Array(
  //             scalar? stop?                                              fpu?            vmu?                    viu?     vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?
  //              val? | | d  s1 s2 s3 imm   alufn   dw     sel1    sel2    | fp  fn        | mode  cmd       mt    | fn     | fn      | fn      | fn       | fn      | fn       | fn
  //                 | | | |  |  |  |  |     |       |      |       |       | |   |         | |     |         |     | |      | |       | |       | |        | |       | |        | |
    VSSSEGD   ->List(Y,Y,N,R_,RS,RS,R_,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,FX,       Y,MM_S, M_XWR,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSSEGD   ->List(Y,Y,N,RS,RS,R_,R_,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,FX,       Y,MM_S, M_XRD,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),

    VLUI      ->List(Y,Y,N,RS,RS,R_,R_,IMM_U,FN_ADD, DW_XPR,A1_ZERO,A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VADDI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_ADD, DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSLLI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SL,  DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSLTI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SLT, DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSLTIU    ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SLTU,DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VXORI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_XOR, DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSRLI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SR,  DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSRAI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SRA, DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VORI      ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_OR,  DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VANDI     ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_AND, DW_XPR,A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VADDIW    ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_ADD, DW_32, A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSLLIW    ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SL,  DW_32, A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSRLIW    ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SR,  DW_32, A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSRAIW    ->List(Y,Y,N,RS,RS,R_,R_,IMM_I,FN_SRA, DW_32, A1_RS1, A2_IMM, N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),

    VSTOP     ->List(Y,Y,Y,R_,R_,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X))
}

object VectorMemoryDecode extends VFDecodeTable
{
  val table = Array(
  //             scalar? stop?                                              fpu?            vmu?                    viu?     vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?
  //              val? | | d  s1 s2 s3 imm   alufn   dw     sel1    sel2    | fp  fn        | mode  cmd       mt    | fn     | fn      | fn      | fn       | fn      | fn       | fn
  //                 | | | |  |  |  |  |     |       |      |       |       | |   |         | |     |         |     | |      | |       | |       | |        | |       | |        | |
    VLSEGB    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGH    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGW    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGD    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGBU   ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_BU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGHU   ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_HU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGWU   ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_WU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSSEGB    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XWR,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSSEGH    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XWR,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSSEGW    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XWR,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSSEGD    ->List(Y,N,N,RV,RA,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XWR,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),

    VLSEGSTB  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGSTH  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGSTW  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGSTD  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGSTBU ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_BU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGSTHU ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_HU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGSTWU ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XRD,    MT_WU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSSEGSTB  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XWR,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSSEGSTH  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XWR,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSSEGSTW  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XWR,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSSEGSTD  ->List(Y,N,N,RV,RA,RA,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VS,M_XWR,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),

    VLSEGXB   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XRD,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGXH   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XRD,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGXW   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XRD,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGXD   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XRD,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGXBU  ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XRD,    MT_BU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGXHU  ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XRD,    MT_HU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VLSEGXWU  ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XRD,    MT_WU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSSEGXB   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XWR,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSSEGXH   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XWR,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSSEGXW   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XWR,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSSEGXD   ->List(Y,N,N,RV,RS,RV,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XWR,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),

    VAMOSWAP_W->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_SWAP,MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOADD_W ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_ADD, MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOXOR_W ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_XOR, MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOAND_W ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_AND, MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOOR_W  ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_OR,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOMIN_W ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_MIN, MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOMAX_W ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_MAX, MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOMINU_W->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_MINU,MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOMAXU_W->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_MAXU,MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOSWAP_D->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_SWAP,MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOADD_D ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_ADD, MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOXOR_D ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_XOR, MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOAND_D ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_AND, MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOOR_D  ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_OR,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOMIN_D ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_MIN, MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOMAX_D ->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_MAX, MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOMINU_D->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_MINU,MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAMOMAXU_D->List(Y,N,N,RV,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       Y,MM_VX,M_XA_MAXU,MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X))
}

object VectorArithmeticDecode extends VFDecodeTable
{
  val table = Array(
  //             scalar? stop?                                              fpu?            vmu?                    viu?     vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?
  //              val? | | d  s1 s2 s3 imm   alufn   dw     sel1    sel2    | fp  fn        | mode  cmd       mt    | fn     | fn      | fn      | fn       | fn      | fn       | fn
  //                 | | | |  |  |  |  |     |       |      |       |       | |   |         | |     |         |     | |      | |       | |       | |        | |       | |        | |
    VEIDX     ->List(Y,N,N,RV,R_,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_IDX, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VADD      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_ADD, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_ADD, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VADDU     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_ADD, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_ADDU,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSUB      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SUB, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_SUB, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSLL      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SL,  DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_SLL, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSLT      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SLT, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_SLT, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSLTU     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SLTU,DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_SLTU,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VXOR      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_XOR, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_XOR, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSRL      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SR,  DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_SRL, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSRA      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SRA, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_SRA, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VOR       ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_OR,  DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_OR,  N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VAND      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_AND, DW_X,  A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_AND, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    // FIXME START
    VMUL      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   Y,IM_M,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VMULH     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   Y,IM_MH,  N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VMULHSU   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   Y,IM_MHSU,N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VMULHU    ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   Y,IM_MHU, N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VDIV      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_DIV, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VDIVU     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_DIVU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VREM      ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_REM, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VREMU     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_REMU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    // FIXME END
    VADDW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_ADD, DW_32, A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_ADD, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSUBW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SUB, DW_32, A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_SUB, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSLLW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SL,  DW_32, A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_SLL, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSRLW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SR,  DW_32, A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_SRL, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VSRAW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_SRA, DW_32, A1_RS1, A2_RS2, N,FP_,FX,       N,MM_X, M_X,      MT_X, Y,I_SRA, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    // FIXME START
    VMULW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   Y,IM_M,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VDIVW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_DIV, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VDIVUW    ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_DIVU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VREMW     ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_REM, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VREMUW    ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_REMU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    // FIXME END

    VFMADD_D  ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FMADD_D,  N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X),
    VFMSUB_D  ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FMSUB_D,  N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X),
    VFNMADD_D ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FNMADD_D, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X),
    VFNMSUB_D ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FNMSUB_D, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X),
    VFADD_D   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FADD_D,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X),
    VFSUB_D   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FSUB_D,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X),
    VFMUL_D   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FMUL_D,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X),
    // FIXME START
    VFDIV_D   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X),
    VFSQRT_D  ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X),
    // FIXME END
    VFSGNJ_D  ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FSGNJ_D,  N,MM_X, M_X,      MT_X, Y,I_FSJ, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VFSGNJN_D ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FSGNJN_D, N,MM_X, M_X,      MT_X, Y,I_FSJN,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VFSGNJX_D ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FSGNJX_D, N,MM_X, M_X,      MT_X, Y,I_FSJX,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VFMIN_D   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FMIN_D,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X),
    VFMAX_D   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FMAX_D,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X),
    VFCVT_D_S ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_D_S, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CSTD),
    // FIXME START
    VFCVT_D_H ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CHTD),
    // FIXME END
    VFCLASS_D ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCLASS_D, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X),

    VFMADD_S  ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FMADD_S,  N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X),
    VFMSUB_S  ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FMSUB_S,  N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X),
    VFNMADD_S ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FNMADD_S, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X),
    VFNMSUB_S ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FNMSUB_S, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X),
    VFADD_S   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FADD_S,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X),
    VFSUB_S   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FSUB_S,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X),
    VFMUL_S   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FMUL_S,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X),
    // FIXME START
    VFDIV_S   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X),
    VFSQRT_S  ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X),
    // FIXME END
    VFSGNJ_S  ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FSGNJ_S,  N,MM_X, M_X,      MT_X, Y,I_FSJ, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VFSGNJN_S ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FSGNJN_S, N,MM_X, M_X,      MT_X, Y,I_FSJN,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VFSGNJX_S ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FSGNJX_S, N,MM_X, M_X,      MT_X, Y,I_FSJX,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VFMIN_S   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FMIN_S,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X),
    VFMAX_S   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FMAX_S,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X),
    VFCVT_S_D ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_S_D, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CDTS),
    // FIXME START
    VFCVT_S_H ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CHTS),
    // FIXME END
    VFCLASS_S ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCLASS_S, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X),

    // FIXME START
    VFMADD_H  ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X),
    VFMSUB_H  ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X),
    VFNMADD_H ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X),
    VFNMSUB_H ->List(Y,N,N,RX,RX,RX,RX,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X),
    VFADD_H   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X),
    VFSUB_H   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X),
    VFMUL_H   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X),
    VFDIV_H   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X),
    VFSQRT_H  ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X),
    VFSGNJ_H  ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, Y,I_FSJ, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VFSGNJN_H ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, Y,I_FSJN,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VFSGNJX_H ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, Y,I_FSJX,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VFMIN_H   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X),
    VFMAX_H   ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X),
    VFCVT_H_D ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CDTH),
    VFCVT_H_S ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CSTH),
    VFCLASS_H ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X),
    // FIXME END

    VFCVT_W_D ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_W_D, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW),
    VFCVT_WU_D->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_WU_D,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU),
    VFCVT_L_D ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_L_D, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL),
    VFCVT_LU_D->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_LU_D,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU),
    VFCVT_D_W ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_D_W, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF),
    VFCVT_D_WU->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_D_WU,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF),
    VFCVT_D_L ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_D_L, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF),
    VFCVT_D_LU->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FCVT_D_LU,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF),
    VFCVT_W_S ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_W_S, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW),
    VFCVT_WU_S->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_WU_S,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU),
    VFCVT_L_S ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_L_S, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL),
    VFCVT_LU_S->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_LU_S,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU),
    VFCVT_S_W ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_S_W, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF),
    VFCVT_S_WU->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_S_WU,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF),
    VFCVT_S_L ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_S_L, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF),
    VFCVT_S_LU->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FCVT_S_LU,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF),
    // FIXME START
    VFCVT_W_H ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW),
    VFCVT_WU_H->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU),
    VFCVT_L_H ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL),
    VFCVT_LU_H->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU),
    VFCVT_H_W ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF),
    VFCVT_H_WU->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF),
    VFCVT_H_L ->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF),
    VFCVT_H_LU->List(Y,N,N,RX,RX,R_,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF),
    // FIXME END

    // FIXME START
    VCMPEQ    ->List(Y,N,N,RP,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VCMPLT    ->List(Y,N,N,RP,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    VCMPLTU   ->List(Y,N,N,RP,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X),
    // FIXME END
    VCMPFEQ_D ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FEQ_D,    N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X),
    VCMPFLT_D ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FLT_D,    N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X),
    VCMPFLE_D ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,FLE_D,    N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X),
    VCMPFEQ_S ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FEQ_S,    N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X),
    VCMPFLT_S ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FLT_S,    N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X),
    VCMPFLE_S ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,FLE_S,    N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X),
    // FIXME START
    VCMPFEQ_H ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X),
    VCMPFLT_H ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X),
    VCMPFLE_H ->List(Y,N,N,RX,RX,RX,R_,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,FX,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X))
    // FIXME END
}

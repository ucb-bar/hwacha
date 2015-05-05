package hwacha

import Chisel._
import HwachaElementInstructions._
import rocket.ALU._
import ScalarFPUDecode._

class IntCtrlSigs extends Bundle {
  val ival = Bool()
  val decode_scalar = Bool()
  val decode_stop = Bool()
  val vd_val = Bool()
  val vd_t = Bits(width = RV.getWidth)
  val vd_dyn = Bool()
  val vd_type = Bits(width = REG_VEC.getWidth)
  val vs1_val = Bool()
  val vs1_t = Bits(width = RV.getWidth)
  val vs1_dyn = Bool()
  val vs1_type = Bits(width = REG_VEC.getWidth)
  val vs2_val = Bool()
  val vs2_t = Bits(width = RV.getWidth)
  val vs2_dyn = Bool()
  val vs2_type = Bits(width = REG_VEC.getWidth)
  val vs3_val = Bool()
  val vs3_t = Bits(width = RV.getWidth)
  val vs3_dyn = Bool()
  val vs3_type = Bits(width = REG_VEC.getWidth)
  val sel_imm = Bits(width = IMM_X.getWidth)
  val alu_fn = Bits(width = FN_X.getWidth)
  val alu_dw = Bool()
  val alu_sel2 = Bits(width = A2_X.getWidth)
  val alu_sel1 = Bits(width = A1_X.getWidth)
  val fpu_val = Bool()
  val fpu_fp = Bits(width = FP_.getWidth)
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
  val fpu_fn = new rocket.FPUCtrlSigs

  def decode(inst: UInt, table: Iterable[(UInt, List[UInt])]) = {
    val decoder = rocket.DecodeLogic(inst, ScalarDecode.default, table)
    Vec(ival, decode_scalar, decode_stop,
        vd_val, vd_t, vd_dyn, vs1_val, vs1_t, vs1_dyn, 
        vs2_val, vs2_t, vs2_dyn, vs3_val, vs3_t, vs3_dyn, 
        sel_imm,
        alu_fn, alu_dw, alu_sel1, alu_sel2,
        fpu_val, fpu_fp,
        vmu_val, vmu_mode, vmu_cmd, vmu_mt,
        viu_val, viu_fn,
        vimu_val, vimu_fn,
        vidu_val, vidu_fn,
        vfmu_val, vfmu_fn,
        vfdu_val, vfdu_fn,
        vfcu_val, vfcu_fn,
        vfvu_val, vfvu_fn,
        fpu_fn.cmd, fpu_fn.ldst, fpu_fn.wen, fpu_fn.ren1, fpu_fn.ren2, fpu_fn.ren3, fpu_fn.swap12, fpu_fn.swap23, fpu_fn.single, fpu_fn.fromint,
        fpu_fn.toint, fpu_fn.fastpipe, fpu_fn.fma, fpu_fn.div, fpu_fn.sqrt, fpu_fn.round, fpu_fn.wflags) := decoder
    vd_type := reg_type(vd_t, vd_dyn, inst(OPC_VD))
    vs1_type := reg_type(vs1_t, vs1_dyn, inst(OPC_VS1))
    vs2_type := reg_type(vs2_t, vs2_dyn, inst(OPC_VS2))
    vs3_type := reg_type(vs3_t, vs3_dyn, inst(OPC_VS3))
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

abstract trait VFDecodeTable {

  //             scalar? stop?                                                              fpu?            vmu?                    viu?     vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?
  //              val? | | dv t d 1v t d 2v t d 3v t d imm   alufn   dw     sel1    sel2    | fp  fn        | mode  cmd       mt    | fn     | fn      | fn      | fn       | fn      | fn       | fn
  //                 | | | |  | | |  | | |  | | |  | | |     |       |      |       |       | |   |         | |     |         |     | |      | |       | |       | |        | |       | |        | |
  val default = List(N,N,N,N,RX,N,N,RX,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX

  val table: Array[(UInt, List[UInt])]
}

object ScalarDecode extends VFDecodeTable {
  val table = Array(
  //             scalar? stop?                                                              fpu?            vmu?                    viu?     vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?
  //              val? | | dv t d 1v t d 2v t d 3v t d imm   alufn   dw     sel1    sel2    | fp  fn        | mode  cmd       mt    | fn     | fn      | fn      | fn       | fn      | fn       | fn
  //                 | | | |  | | |  | | |  | | |  | | |     |       |      |       |       | |   |         | |     |         |     | |      | |       | |       | |        | |       | |        | |
    VLSB   ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLSH   ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLSW   ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLSD   ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLSBU  ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_BU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLSHU  ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_HU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLSWU  ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_WU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSSB   ->(List(Y,Y,N,N,RX,N,Y,RS,N,Y,RS,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XWR,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSSH   ->(List(Y,Y,N,N,RX,N,Y,RS,N,Y,RS,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XWR,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSSW   ->(List(Y,Y,N,N,RX,N,Y,RS,N,Y,RS,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XWR,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSSD   ->(List(Y,Y,N,N,RX,N,Y,RS,N,Y,RS,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XWR,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLAB   ->(List(Y,Y,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLAH   ->(List(Y,Y,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLAW   ->(List(Y,Y,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLAD   ->(List(Y,Y,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLABU  ->(List(Y,Y,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_BU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLAHU  ->(List(Y,Y,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_HU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLAWU  ->(List(Y,Y,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XRD,    MT_WU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSAB   ->(List(Y,Y,N,N,RX,N,Y,RA,N,Y,RS,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XWR,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSAH   ->(List(Y,Y,N,N,RX,N,Y,RA,N,Y,RS,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XWR,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSAW   ->(List(Y,Y,N,N,RX,N,Y,RA,N,Y,RS,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XWR,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSAD   ->(List(Y,Y,N,N,RX,N,Y,RA,N,Y,RS,N,N,RX,N,IMM_X,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FP_,       Y,MM_S, M_XWR,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),

    VLUI      ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_U,FN_ADD, DW_XPR,A1_ZERO,A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VADDI     ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_ADD, DW_XPR,A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSLLI     ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_SL,  DW_XPR,A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSLTI     ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_SLT, DW_XPR,A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSLTIU    ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_SLTU,DW_XPR,A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VXORI     ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_XOR, DW_XPR,A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSRLI     ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_SR,  DW_XPR,A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSRAI     ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_SRA, DW_XPR,A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VORI      ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_OR,  DW_XPR,A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VANDI     ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_AND, DW_XPR,A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VADDIW    ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_ADD, DW_32, A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSLLIW    ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_SL,  DW_32, A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSRLIW    ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_SR,  DW_32, A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSRAIW    ->(List(Y,Y,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,IMM_I,FN_SRA, DW_32, A1_RS1, A2_IMM, N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),

    VSTOP     ->(List(Y,Y,Y,N,RX,N,N,RX,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX))
}

object VectorMemoryDecode extends VFDecodeTable {
  val table = Array(
  //             scalar? stop?                                                              fpu?            vmu?                    viu?     vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?
  //              val? | | dv t d 1v t d 2v t d 3v t d imm   alufn   dw     sel1    sel2    | fp  fn        | mode  cmd       mt    | fn     | fn      | fn      | fn       | fn      | fn       | fn
  //                 | | | |  | | |  | | |  | | |  | | |     |       |      |       |       | |   |         | |     |         |     | |      | |       | |       | |        | |       | |        | |
    VLB    ->(List(Y,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLH    ->(List(Y,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLW    ->(List(Y,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLD    ->(List(Y,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLBU   ->(List(Y,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_BU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLHU   ->(List(Y,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_HU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLWU   ->(List(Y,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_WU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSB    ->(List(Y,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XWR,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSH    ->(List(Y,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XWR,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSW    ->(List(Y,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XWR,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSD    ->(List(Y,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XWR,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),

    VLSTB  ->(List(Y,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLSTH  ->(List(Y,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLSTW  ->(List(Y,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLSTD  ->(List(Y,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLSTBU ->(List(Y,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_BU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLSTHU ->(List(Y,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_HU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLSTWU ->(List(Y,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XRD,    MT_WU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSSTB  ->(List(Y,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XWR,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSSTH  ->(List(Y,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XWR,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSSTW  ->(List(Y,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XWR,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSSTD  ->(List(Y,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VS,M_XWR,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),

    VLXB   ->(List(Y,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XRD,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLXH   ->(List(Y,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XRD,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLXW   ->(List(Y,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XRD,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLXD   ->(List(Y,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XRD,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLXBU  ->(List(Y,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XRD,    MT_BU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLXHU  ->(List(Y,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XRD,    MT_HU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VLXWU  ->(List(Y,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XRD,    MT_WU,N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSXB   ->(List(Y,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XWR,    MT_B, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSXH   ->(List(Y,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XWR,    MT_H, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSXW   ->(List(Y,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XWR,    MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSXD   ->(List(Y,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XWR,    MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),

    VAMOSWAP_W->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_SWAP,MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOADD_W ->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_ADD, MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOXOR_W ->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_XOR, MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOAND_W ->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_AND, MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOOR_W  ->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_OR,  MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOMIN_W ->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_MIN, MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOMAX_W ->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_MAX, MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOMINU_W->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_MINU,MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOMAXU_W->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_MAXU,MT_W, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOSWAP_D->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_SWAP,MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOADD_D ->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_ADD, MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOXOR_D ->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_XOR, MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOAND_D ->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_AND, MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOOR_D  ->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_OR,  MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOMIN_D ->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_MIN, MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOMAX_D ->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_MAX, MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOMINU_D->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_MINU,MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAMOMAXU_D->(List(Y,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       Y,MM_VX,M_XA_MAXU,MT_D, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX))
}

object VectorArithmeticDecode extends VFDecodeTable {
  val table = Array(
  //             scalar? stop?                                                              fpu?            vmu?                    viu?     vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?
  //              val? | | dv t d 1v t d 2v t d 3v t d imm   alufn   dw     sel1    sel2    | fp  fn        | mode  cmd       mt    | fn     | fn      | fn      | fn       | fn      | fn       | fn
  //                 | | | |  | | |  | | |  | | |  | | |     |       |      |       |       | |   |         | |     |         |     | |      | |       | |       | |        | |       | |        | |
    VEIDX     ->(List(Y,N,N,Y,RV,N,N,RX,N,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, Y,I_IDX, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VADD      ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_ADD, DW_X,  A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_ADD, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VADDU     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_ADD, DW_X,  A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_ADDU,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSUB      ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_SUB, DW_X,  A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_SUB, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSLL      ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_SL,  DW_X,  A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_SLL, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSLT      ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_SLT, DW_X,  A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_SLT, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSLTU     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_SLTU,DW_X,  A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_SLTU,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VXOR      ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_XOR, DW_X,  A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_XOR, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSRL      ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_SR,  DW_X,  A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_SRL, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSRA      ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_SRA, DW_X,  A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_SRA, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VOR       ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_OR,  DW_X,  A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_OR,  N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VAND      ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_AND, DW_X,  A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_AND, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    // FIXME START
    VMUL      ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   Y,IM_M,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VMULH     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   Y,IM_MH,  N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VMULHSU   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   Y,IM_MHSU,N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VMULHU    ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   Y,IM_MHU, N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VDIV      ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_DIV, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VDIVU     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_DIVU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VREM      ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_REM, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VREMU     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_REMU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    // FIXME END
    VADDW     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_ADD, DW_32, A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_ADD, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSUBW     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_SUB, DW_32, A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_SUB, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSLLW     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_SL,  DW_32, A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_SLL, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSRLW     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_SR,  DW_32, A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_SRL, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VSRAW     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_SRA, DW_32, A1_RS1, A2_RS2, N,FP_,       N,MM_X, M_X,      MT_X, Y,I_SRA, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    // FIXME START
    VMULW     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   Y,IM_M,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VDIVW     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_DIV, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VDIVUW    ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_DIVU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VREMW     ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_REM, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VREMUW    ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_32, A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   Y,ID_REMU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    // FIXME END

    VFMADD_D  ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,  N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X) ++ FMADD_D),
    VFMSUB_D  ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,  N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X) ++ FMSUB_D),
    VFNMADD_D ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X) ++ FNMADD_D),
    VFNMSUB_D ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X) ++ FNMSUB_D),
    VFADD_D   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X) ++ FADD_D),
    VFSUB_D   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X) ++ FSUB_D),
    VFMUL_D   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X) ++ FMUL_D),
    // FIXME START
    VFDIV_D   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X) ++ FDIV_D),
    VFSQRT_D  ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,  N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X) ++ FSQRT_D),
    // FIXME END
    VFSGNJ_D  ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,  N,MM_X, M_X,      MT_X, Y,I_FSJ, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FSGNJ_D),
    VFSGNJN_D ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD, N,MM_X, M_X,      MT_X, Y,I_FSJN,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FSGNJN_D),
    VFSGNJX_D ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD, N,MM_X, M_X,      MT_X, Y,I_FSJX,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FSGNJX_D),
    VFMIN_D   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X) ++ FMIN_D),
    VFMAX_D   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X) ++ FMAX_D),
    VFCVT_D_S ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CSTD) ++ FCVT_D_S),
    // FIXME START
    VFCVT_D_H ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CHTD) ++ FX),
    // FIXME END
    VFCLASS_D ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X) ++ FCLASS_D),

    VFMADD_S  ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,  N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X) ++ FMADD_S),
    VFMSUB_S  ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,  N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X) ++ FMSUB_S),
    VFNMADD_S ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X) ++ FNMADD_S),
    VFNMSUB_S ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X) ++ FNMSUB_S),
    VFADD_S   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X) ++ FADD_S),
    VFSUB_S   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X) ++ FSUB_S),
    VFMUL_S   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X) ++ FMUL_S),
    // FIXME START
    VFDIV_S   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X) ++ FDIV_S),
    VFSQRT_S  ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,  N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X) ++ FSQRT_S),
    // FIXME END
    VFSGNJ_S  ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,  N,MM_X, M_X,      MT_X, Y,I_FSJ, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FSGNJ_S),
    VFSGNJN_S ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS, N,MM_X, M_X,      MT_X, Y,I_FSJN,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FSGNJN_S),
    VFSGNJX_S ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS, N,MM_X, M_X,      MT_X, Y,I_FSJX,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FSGNJX_S),
    VFMIN_S   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X) ++ FMIN_S),
    VFMAX_S   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,   N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X) ++ FMAX_S),
    VFCVT_S_D ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CDTS) ++ FCVT_S_D),
    // FIXME START
    VFCVT_S_H ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CHTS) ++ FX),
    // FIXME END
    VFCLASS_S ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X) ++ FCLASS_S),

    // FIXME START
    VFMADD_H  ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VFMSUB_H  ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VFNMADD_H ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VFNMSUB_H ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VFADD_H   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VFSUB_H   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VFMUL_H   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VFDIV_H   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X) ++ FX),
    VFSQRT_H  ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X) ++ FX),
    VFSGNJ_H  ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, Y,I_FSJ, N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VFSGNJN_H ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, Y,I_FSJN,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VFSGNJX_H ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, Y,I_FSJX,N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VFMIN_H   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X) ++ FX),
    VFMAX_H   ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X) ++ FX),
    VFCVT_H_D ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CDTH) ++ FX),
    VFCVT_H_S ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CSTH) ++ FX),
    VFCLASS_H ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X) ++ FX),
    // FIXME END

    VFCVT_W_D ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW) ++ FCVT_W_D),
    VFCVT_WU_D->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU) ++ FCVT_WU_D),
    VFCVT_L_D ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL) ++ FCVT_L_D),
    VFCVT_LU_D->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU) ++ FCVT_LU_D),
    VFCVT_D_W ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF) ++ FCVT_D_W),
    VFCVT_D_WU->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF) ++ FCVT_D_WU),
    VFCVT_D_L ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF) ++ FCVT_D_L),
    VFCVT_D_LU->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF) ++ FCVT_D_LU),
    VFCVT_W_S ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW) ++ FCVT_W_S),
    VFCVT_WU_S->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU) ++ FCVT_WU_S),
    VFCVT_L_S ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL) ++ FCVT_L_S),
    VFCVT_LU_S->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU) ++ FCVT_LU_S),
    VFCVT_S_W ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF) ++ FCVT_S_W),
    VFCVT_S_WU->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF) ++ FCVT_S_WU),
    VFCVT_S_L ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS, N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF) ++ FCVT_S_L),
    VFCVT_S_LU->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF) ++ FCVT_S_LU),
    // FIXME START
    VFCVT_W_H ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW) ++ FX),
    VFCVT_WU_H->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU) ++ FX),
    VFCVT_L_H ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL) ++ FX),
    VFCVT_LU_H->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU) ++ FX),
    VFCVT_H_W ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF) ++ FX),
    VFCVT_H_WU->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF) ++ FX),
    VFCVT_H_L ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF) ++ FX),
    VFCVT_H_LU->(List(Y,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF) ++ FX),
    // FIXME END

    // FIXME START
    VCMPEQ    ->(List(Y,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VCMPLT    ->(List(Y,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    VCMPLTU   ->(List(Y,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   N,FP_,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X) ++ FX),
    // FIXME END
    VCMPFEQ_D ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,    N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X) ++ FEQ_D),
    VCMPFLT_D ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,    N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X) ++ FLT_D),
    VCMPFLE_D ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPD,    N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X) ++ FLE_D),
    VCMPFEQ_S ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,    N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X) ++ FEQ_S),
    VCMPFLT_S ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,    N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X) ++ FLT_S),
    VCMPFLE_S ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPS,    N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X) ++ FLE_S),
    // FIXME START
    VCMPFEQ_H ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X) ++ FX),
    VCMPFLT_H ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X) ++ FX),
    VCMPFLE_H ->(List(Y,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,IMM_X,FN_X,   DW_X,  A1_X,   A2_X,   Y,FPH,       N,MM_X, M_X,      MT_X, N,I_X,   N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X) ++ FX))
    // FIXME END
}

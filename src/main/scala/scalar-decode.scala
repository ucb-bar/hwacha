package hwacha

import Chisel._
import freechips.rocketchip.config._
import HwachaElementInstructions._
import freechips.rocketchip.rocket.ALU._
import freechips.rocketchip.util._
import ScalarFPUDecode._

class IntCtrlSigs(implicit p: Parameters) extends HwachaBundle()(p) {
  val ival = Bool()
  val decode_scalar = Bool()
  val decode_fence = Bool()
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
  val vp_val = Bool()
  val vp_neg = Bool()
  val sel_imm = Bits(width = IMM_X.getWidth)
  val alu_fn = Bits(width = FN_X.getWidth)
  val alu_dw = Bool()
  val alu_sel2 = Bits(width = A2_X.getWidth)
  val alu_sel1 = Bits(width = A1_X.getWidth)
  val fpu_val = Bool()
  val fpu_fp = Bits(width = FP_.getWidth)
  val vmu_val = Bool()
  val vmu_mode = Bits(width = VM_X.getWidth)
  val vmu_cmd = Bits(width = M_X.getWidth)
  val smu_val = Bool()
  val smu_cmd = Bits(width = SM_X.getWidth)
  val mt = Bits(width = MT_X.getWidth)
  val viu_val = Bool()
  val viu_fn = Bits(width = I_X.getWidth)
  val vipu_val = Bool()
  val vipu_fn = Bits(width = SZ_VIPU_OP)
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
  val vrpu_val = Bool()
  val vrpu_fn = Bits(width = FR_X.getWidth)
  val vrfu_val = Bool()
  val fpu_fn = new freechips.rocketchip.tile.FPUCtrlSigs

  val vp = UInt(width = bPRegs)
  val vs1 = UInt(width = bRegs)
  val vs2 = UInt(width = bRegs)
  val vs3 = UInt(width = bRegs)
  val vd = UInt(width = bRegs)
  val rm = UInt(width = 3)
  val in_fmt = UInt(width = 2)
  val out_fmt = UInt(width = 2)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, ScalarDecode.default, table)
    val sigs = Seq(ival, decode_scalar, decode_fence, decode_stop,
        vd_val, vd_t, vd_dyn, vs1_val, vs1_t, vs1_dyn,
        vs2_val, vs2_t, vs2_dyn, vs3_val, vs3_t, vs3_dyn,
        vp_val,
        sel_imm,
        alu_fn, alu_dw, alu_sel1, alu_sel2,
        fpu_val, fpu_fp,
        vmu_val, vmu_mode, vmu_cmd,
        smu_val, smu_cmd,
        mt,
        viu_val, viu_fn,
        vipu_val,
        vimu_val, vimu_fn,
        vidu_val, vidu_fn,
        vfmu_val, vfmu_fn,
        vfdu_val, vfdu_fn,
        vfcu_val, vfcu_fn,
        vfvu_val, vfvu_fn,
        vrpu_val,
        vrfu_val,
        fpu_fn.ldst, fpu_fn.wen, fpu_fn.ren1, fpu_fn.ren2, fpu_fn.ren3, fpu_fn.swap12,
          fpu_fn.swap23, fpu_fn.typeTagIn, fpu_fn.typeTagOut, fpu_fn.fromint, fpu_fn.toint,
          fpu_fn.fastpipe, fpu_fn.fma, fpu_fn.div, fpu_fn.sqrt, fpu_fn.wflags)
    sigs zip decoder map {case(s,d) => s := d}
    vd_type := reg_type(vd_t, vd_dyn, inst(OPC_VD))
    vs1_type := reg_type(vs1_t, vs1_dyn, inst(OPC_VS1))
    vs2_type := reg_type(vs2_t, vs2_dyn, inst(OPC_VS2))
    vs3_type := reg_type(vs3_t, vs3_dyn, inst(OPC_VS3))
    vipu_fn := inst(57,50)
    vrpu_fn := inst(34,33)
    vp_neg := inst(OPC_NEG)

    out_fmt := inst(56,55)
    in_fmt := inst(54,53)
    rm := inst(52,50)
    vs3 := inst(48,41)
    vs2 := inst(40,33)
    vs1 := inst(31,24)
    vd := inst(23,16)
    vp := inst(15,12)

    this
  }

  def active_vint(d: Int = 0) = viu_val
  def active_vipred(d: Int = 0) = vipu_val
  def active_vimul(d: Int = 0) = vimu_val
  def active_vidiv(d: Int = 0) = vidu_val
  def active_vfma(d: Int = 0) = vfmu_val
  def active_vfdiv(d: Int = 0) = vfdu_val
  def active_vfcmp(d: Int = 0) = vfcu_val
  def active_vfconv(d: Int = 0) = vfvu_val
  def active_vrpred(d: Int = 0) = vrpu_val
  def active_vrfirst(d: Int = 0) = vrfu_val
  def active_vamo(d: Int = 0) = vmu_val && isAMO(vmu_cmd)
  def active_vldx(d: Int = 0) = vmu_val && vmu_indexed(vmu_mode) && (vmu_cmd === M_XRD)
  def active_vstx(d: Int = 0) = vmu_val && vmu_indexed(vmu_mode) && (vmu_cmd === M_XWR)
  def active_vld(d: Int = 0) = vmu_val && !vmu_indexed(vmu_mode) && (vmu_cmd === M_XRD)
  def active_vst(d: Int = 0) = vmu_val && !vmu_indexed(vmu_mode) && (vmu_cmd === M_XWR)

  def fn_viu(d: Int = 0) = new VIUFn().fromBits(Cat(alu_dw, fpu_fp, viu_fn))
  def fn_vipu(d: Int = 0) = new VIPUFn().fromBits(vipu_fn)
  def fn_vimu(d: Int = 0) = new VIMUFn().fromBits(Cat(alu_dw, vimu_fn))
  def fn_vidu(d: Int = 0) = new VIDUFn().fromBits(Cat(alu_dw, vidu_fn))
  def fn_vfmu(rm: Bits) = new VFMUFn().fromBits(Cat(fpu_fp, rm, vfmu_fn))
  def fn_vfdu(rm: Bits) = new VFDUFn().fromBits(Cat(fpu_fp, rm, vfdu_fn))
  def fn_vfcu(rm: Bits) = new VFCUFn().fromBits(Cat(fpu_fp, rm, vfcu_fn))
  def fn_vfvu(rm: Bits) = new VFVUFn().fromBits(Cat(fpu_fp, rm, vfvu_fn))
  def fn_vrpu(d: Int = 0) = new VRPUFn().fromBits(vrpu_fn)
  def fn_vrfu(d: Int = 0) = new VRFUFn().fromBits(vd)
  def fn_vmu(d: Int = 0) = new VMUFn().fromBits(Cat(vmu_mode, vmu_cmd, mt))
  def fn_smu(d: Int = 0) = new SMUFn().fromBits(Cat(smu_cmd, mt))

  def as1(d: Int = 0) = vs1(4,0)
  def as2(d: Int = 0) = vs2(4,0)
}

abstract trait VFDecodeTable {

  val default: List[BitPat] =
  //                         fence? stop?                                                              fpu?  vmu?             smu?         viu?     vipu? vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?       vrfu?
  //                      scalar? | | dv t d 1v t d 2v t d 3v t d p imm   alufn   dw   sel1    sel2    | fp  | mode cmd       | cmd  mt    | fn     |     | fn      | fn      | fn       | fn      | fn       | fn   vrpu?|     fpfn
  //                       val? | | | |  | | |  | | |  | | |  | | | |     |       |    |       |       | |   | |    |         | |    |     | |      |     | |       | |       | |        | |       | |        | |       | |     |
                         List(N,N,N,N,N,RX,N,N,RX,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX

  val table: Array[(BitPat, List[BitPat])]
}

object ScalarDecode extends VFDecodeTable {
  val table: Array[(BitPat, List[BitPat])] = Array(
  //                         fence? stop?                                                              fpu?  vmu?             smu?         viu?     vipu? vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?       vrfu?
  //                      scalar? | | dv t d 1v t d 2v t d 3v t d p imm   alufn   dw   sel1    sel2    | fp  | mode cmd       | cmd  mt    | fn     |     | fn      | fn      | fn       | fn      | fn       | fn   vrpu?|     fpfn
  //                       val? | | | |  | | |  | | |  | | |  | | | |     |       |    |       |       | |   | |    |         | |    |     | |      |     | |       | |       | |        | |       | |        | |       | |     |
    VLSB      ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLSH      ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLSW      ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLSD      ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLSBU     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_BU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLSHU     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_HU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLSWU     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_WU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSSB      ->(List[BitPat](Y,Y,N,N,N,RX,N,Y,RS,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSSH      ->(List[BitPat](Y,Y,N,N,N,RX,N,Y,RS,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSSW      ->(List[BitPat](Y,Y,N,N,N,RX,N,Y,RS,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSSD      ->(List[BitPat](Y,Y,N,N,N,RX,N,Y,RS,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLAB      ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLAH      ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLAW      ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLAD      ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLABU     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_BU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLAHU     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_HU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLAWU     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_WU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSAB      ->(List[BitPat](Y,Y,N,N,N,RX,N,Y,RA,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSAH      ->(List[BitPat](Y,Y,N,N,N,RX,N,Y,RA,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSAW      ->(List[BitPat](Y,Y,N,N,N,RX,N,Y,RA,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSAD      ->(List[BitPat](Y,Y,N,N,N,RX,N,Y,RA,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),

    VLUI      ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_L,FN_ADD, DW64,A1_ZERO,A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAUIPC    ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_L,FN_ADD, DW64,A1_PC,  A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VADDI     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_ADD, DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSLLI     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SL,  DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSLTI     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SLT, DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSLTIU    ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SLTU,DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VXORI     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_XOR, DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSRLI     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SR,  DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSRAI     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SRA, DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VORI      ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_OR,  DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VANDI     ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_AND, DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VADDIW    ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_ADD, DW32,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSLLIW    ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SL,  DW32,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSRLIW    ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SR,  DW32,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSRAIW    ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SRA, DW32,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),

    VCJAL     ->(List[BitPat](Y,Y,N,N,Y,RS,N,N,RX,N,N,RX,N,N,RX,N,Y,IMM_U,FN_ADD, DW64,A1_PC,  A2_8,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,N) ++ FX),
    VCJALR    ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,Y,IMM_U,FN_ADD, DW64,A1_PC,  A2_8,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    Y,N) ++ FX),
    VFIRST    ->(List[BitPat](Y,Y,N,N,Y,RS,N,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,Y) ++ FX),
    VFENCE    ->(List[BitPat](Y,Y,Y,N,N,RX,N,N,RX,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSTOP     ->(List[BitPat](Y,Y,N,Y,N,RX,N,N,RX,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX))
}

object VectorMemoryDecode extends VFDecodeTable {
  val table: Array[(BitPat, List[BitPat])] = Array(
  //                         fence? stop?                                                              fpu?  vmu?             smu?         viu?     vipu? vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?       vrfu?
  //                      scalar? | | dv t d 1v t d 2v t d 3v t d p imm   alufn   dw   sel1    sel2    | fp  | mode cmd       | cmd  mt    | fn     |     | fn      | fn      | fn       | fn      | fn       | fn   vrpu?|     fpfn
  //                       val? | | | |  | | |  | | |  | | |  | | | |     |       |    |       |       | |   | |    |         | |    |     | |      |     | |       | |       | |        | |       | |        | |       | |     |
    VLB       ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLH       ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLW       ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLD       ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLBU      ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_BU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLHU      ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_HU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLWU      ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_WU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSB       ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XWR,    N,SM_X,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSH       ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XWR,    N,SM_X,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSW       ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XWR,    N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSD       ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XWR,    N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),

    VLSTB     ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLSTH     ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLSTW     ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLSTD     ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLSTBU    ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_BU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLSTHU    ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_HU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLSTWU    ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_WU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSSTB     ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XWR,    N,SM_X,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSSTH     ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XWR,    N,SM_X,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSSTW     ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XWR,    N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSSTD     ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XWR,    N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),

    VLXB      ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLXH      ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLXW      ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLXD      ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLXBU     ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_BU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLXHU     ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_HU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VLXWU     ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_WU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSXB      ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XWR,    N,SM_X,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSXH      ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XWR,    N,SM_X,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSXW      ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XWR,    N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSXD      ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XWR,    N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),

    VAMOSWAP_W->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_SWAP,N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOADD_W ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_ADD, N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOXOR_W ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_XOR, N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOAND_W ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_AND, N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOOR_W  ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_OR,  N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOMIN_W ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MIN, N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOMAX_W ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MAX, N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOMINU_W->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MINU,N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOMAXU_W->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MAXU,N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOSWAP_D->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_SWAP,N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOADD_D ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_ADD, N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOXOR_D ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_XOR, N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOAND_D ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_AND, N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOOR_D  ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_OR,  N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOMIN_D ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MIN, N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOMAX_D ->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MAX, N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOMINU_D->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MINU,N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAMOMAXU_D->(List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MAXU,N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX))
}

object VectorArithmeticDecode extends VFDecodeTable {
  val table: Array[(BitPat, List[BitPat])] = Array(
  //                         fence? stop?                                                              fpu?  vmu?             smu?         viu?     vipu? vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?       vrfu?
  //                      scalar? | | dv t d 1v t d 2v t d 3v t d p imm   alufn   dw   sel1    sel2    | fp  | mode cmd       | cmd  mt    | fn     |     | fn      | fn      | fn       | fn      | fn       | fn   vrpu?|     fpfn
  //                       val? | | | |  | | |  | | |  | | |  | | | |     |       |    |       |       | |   | |    |         | |    |     | |      |     | |       | |       | |        | |       | |        | |       | |     |
    VEIDX     ->(List[BitPat](Y,N,N,N,Y,RV,N,N,RX,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_IDX, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VADD      ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_ADD, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_ADD, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VADDU     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_ADD, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_ADDU,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSUB      ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SUB, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SUB, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSLL      ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SL,  DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SLL, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSLT      ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SLT, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SLT, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSLTU     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SLTU,DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SLTU,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VXOR      ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_XOR, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_XOR, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSRL      ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SR,  DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SRL, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSRA      ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SRA, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SRA, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VOR       ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_OR,  DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_OR,  N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VAND      ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_AND, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_AND, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VMUL      ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    Y,IM_M,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VMULH     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    Y,IM_MH,  N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VMULHSU   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    Y,IM_MHSU,N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VMULHU    ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    Y,IM_MHU, N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VDIV      ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_DIV, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VDIVU     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_DIVU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VREM      ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_REM, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VREMU     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_REMU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VADDW     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_ADD, DW32,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_ADD, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSUBW     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SUB, DW32,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SUB, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSLLW     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SL,  DW32,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SLL, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSRLW     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SR,  DW32,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SRL, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VSRAW     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SRA, DW32,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SRA, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VMULW     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW32,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    Y,IM_M,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VDIVW     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW32,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_DIV, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VDIVUW    ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW32,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_DIVU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VREMW     ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW32,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_REM, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VREMUW    ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW32,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_REMU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),

    VFMADD_D  ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FMADD_D),
    VFMSUB_D  ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FMSUB_D),
    VFNMADD_D ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FNMADD_D),
    VFNMSUB_D ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FNMSUB_D),
    VFADD_D   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FADD_D),
    VFSUB_D   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FSUB_D),
    VFMUL_D   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FMUL_D),
    // FIXME START
    VFDIV_D   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X,    N,N) ++ FDIV_D),
    VFSQRT_D  ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X,    N,N) ++ FSQRT_D),
    // FIXME END
    VFSGNJ_D  ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJ, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FSGNJ_D),
    VFSGNJN_D ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJN,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FSGNJN_D),
    VFSGNJX_D ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJX,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FSGNJX_D),
    VFMIN_D   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X,    N,N) ++ FMIN_D),
    VFMAX_D   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X,    N,N) ++ FMAX_D),
    VFCVT_D_S ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CSTD, N,N) ++ FCVT_D_S),
    VFCVT_D_H ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CHTD, N,N) ++ FCVT_D_S),
    VFCLASS_D ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X,    N,N) ++ FCLASS_D),

  //                         fence? stop?                                                              fpu?  vmu?             smu?         viu?     vipu? vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?       vrfu?
  //                      scalar? | | dv t d 1v t d 2v t d 3v t d p imm   alufn   dw   sel1    sel2    | fp  | mode cmd       | cmd  mt    | fn     |     | fn      | fn      | fn       | fn      | fn       | fn   vrpu?|     fpfn
  //                       val? | | | |  | | |  | | |  | | |  | | | |     |       |    |       |       | |   | |    |         | |    |     | |      |     | |       | |       | |        | |       | |        | |       | |     |
    VFMADD_S  ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FMADD_S),
    VFMSUB_S  ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FMSUB_S),
    VFNMADD_S ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FNMADD_S),
    VFNMSUB_S ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FNMSUB_S),
    VFADD_S   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FADD_S),
    VFSUB_S   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FSUB_S),
    VFMUL_S   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FMUL_S),
    // FIXME START
    VFDIV_S   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X,    N,N) ++ FDIV_S),
    VFSQRT_S  ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X,    N,N) ++ FSQRT_S),
    // FIXME END
    VFSGNJ_S  ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJ, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FSGNJ_S),
    VFSGNJN_S ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJN,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FSGNJN_S),
    VFSGNJX_S ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJX,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FSGNJX_S),
    VFMIN_S   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X,    N,N) ++ FMIN_S),
    VFMAX_S   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X,    N,N) ++ FMAX_S),
    VFCVT_S_D ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CDTS, N,N) ++ FCVT_S_D),
    VFCVT_S_H ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CHTS, N,N) ++ FCVT_S_S),
    VFCLASS_S ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X,    N,N) ++ FCLASS_S),

    VFMADD_H  ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FMADD_S),
    VFMSUB_H  ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FMSUB_S),
    VFNMADD_H ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FNMADD_S),
    VFNMSUB_H ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FNMSUB_S),
    VFADD_H   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FADD_S),
    VFSUB_H   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FSUB_S),
    VFMUL_H   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FMUL_S),
    // FIXME START
    VFDIV_H   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X,    N,N) ++ FDIV_S),
    VFSQRT_H  ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X,    N,N) ++ FSQRT_S),
    // FIXME END
    VFSGNJ_H  ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJ, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FSGNJ_S),
    VFSGNJN_H ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJN,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FSGNJN_S),
    VFSGNJX_H ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJX,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FSGNJX_S),
    VFMIN_H   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X,    N,N) ++ FMIN_S),
    VFMAX_H   ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X,    N,N) ++ FMAX_S),
    VFCVT_H_D ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CDTH, N,N) ++ FCVT_S_D),
    VFCVT_H_S ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CSTH, N,N) ++ FCVT_S_S),
    VFCLASS_H ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X,    N,N) ++ FCLASS_S),

  //                         fence? stop?                                                              fpu?  vmu?             smu?         viu?     vipu? vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?       vrfu?
  //                      scalar? | | dv t d 1v t d 2v t d 3v t d p imm   alufn   dw   sel1    sel2    | fp  | mode cmd       | cmd  mt    | fn     |     | fn      | fn      | fn       | fn      | fn       | fn   vrpu?|     fpfn
  //                       val? | | | |  | | |  | | |  | | |  | | | |     |       |    |       |       | |   | |    |         | |    |     | |      |     | |       | |       | |        | |       | |        | |       | |     |
    VFCVT_W_D ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW, N,N) ++ FCVT_W_D),
    VFCVT_WU_D->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU,N,N) ++ FCVT_WU_D),
    VFCVT_L_D ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL, N,N) ++ FCVT_L_D),
    VFCVT_LU_D->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU,N,N) ++ FCVT_LU_D),
    VFCVT_D_W ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF, N,N) ++ FCVT_D_W),
    VFCVT_D_WU->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF,N,N) ++ FCVT_D_WU),
    VFCVT_D_L ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF, N,N) ++ FCVT_D_L),
    VFCVT_D_LU->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF,N,N) ++ FCVT_D_LU),
    VFCVT_W_S ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW, N,N) ++ FCVT_W_S),
    VFCVT_WU_S->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU,N,N) ++ FCVT_WU_S),
    VFCVT_L_S ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL, N,N) ++ FCVT_L_S),
    VFCVT_LU_S->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU,N,N) ++ FCVT_LU_S),
    VFCVT_S_W ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF, N,N) ++ FCVT_S_W),
    VFCVT_S_WU->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF,N,N) ++ FCVT_S_WU),
    VFCVT_S_L ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF, N,N) ++ FCVT_S_L),
    VFCVT_S_LU->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF,N,N) ++ FCVT_S_LU),
    VFCVT_W_H ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW, N,N) ++ FCVT_W_S),
    VFCVT_WU_H->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU,N,N) ++ FCVT_WU_S),
    VFCVT_L_H ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL, N,N) ++ FCVT_L_S),
    VFCVT_LU_H->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU,N,N) ++ FCVT_LU_S),
    VFCVT_H_W ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF, N,N) ++ FCVT_S_W),
    VFCVT_H_WU->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF,N,N) ++ FCVT_S_WU),
    VFCVT_H_L ->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF, N,N) ++ FCVT_S_L),
    VFCVT_H_LU->(List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF,N,N) ++ FCVT_S_LU),

    VCMPEQ    ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_CEQ, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VCMPLT    ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_CLT, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VCMPLTU   ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_CLTU,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX),
    VCMPFEQ_D ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X,    N,N) ++ FEQ_D),
    VCMPFLT_D ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X,    N,N) ++ FLT_D),
    VCMPFLE_D ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X,    N,N) ++ FLE_D),
    VCMPFEQ_S ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X,    N,N) ++ FEQ_S),
    VCMPFLT_S ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X,    N,N) ++ FLT_S),
    VCMPFLE_S ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X,    N,N) ++ FLE_S),
    VCMPFEQ_H ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X,    N,N) ++ FEQ_S),
    VCMPFLT_H ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X,    N,N) ++ FLT_S),
    VCMPFLE_H ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X,    N,N) ++ FLE_S),

    VPOP      ->(List[BitPat](Y,N,N,N,Y,RP,N,Y,RP,N,Y,RP,N,Y,RP,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   Y,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N) ++ FX))
}

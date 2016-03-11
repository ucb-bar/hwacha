package hwacha

import Chisel._
import cde.Parameters
import HwachaElementInstructions._
import rocket.ALU._
import rocket.FPConstants._
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

  val vp = UInt(width = bPRegs)
  val vs1 = UInt(width = bRegs)
  val vs2 = UInt(width = bRegs)
  val vs3 = UInt(width = bRegs)
  val vd = UInt(width = bRegs)
  val rm = UInt(width = 3)
  val in_fmt = UInt(width = 2)
  val out_fmt = UInt(width = 2)
  val fop = UInt(width = OP_SZ)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = rocket.DecodeLogic(inst, ScalarDecode.default, table)
    Vec(ival, decode_scalar, decode_fence, decode_stop,
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
        fop) := decoder

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
  //                      scalar? | | dv t d 1v t d 2v t d 3v t d p imm   alufn   dw   sel1    sel2    | fp  | mode cmd       | cmd  mt    | fn     |     | fn      | fn      | fn       | fn      | fn       | fn   vrpu?|     fop
  //                       val? | | | |  | | |  | | |  | | |  | | | |     |       |    |       |       | |   | |    |         | |    |     | |      |     | |       | |       | |        | |       | |        | |       | |     |
                         List(N,N,N,N,N,RX,N,N,RX,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X)

  val table: Array[(BitPat, List[BitPat])]
}

object ScalarDecode extends VFDecodeTable {
  val table: Array[(BitPat, List[BitPat])] = Array(
  //                         fence? stop?                                                              fpu?  vmu?             smu?         viu?     vipu? vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?       vrfu?
  //                      scalar? | | dv t d 1v t d 2v t d 3v t d p imm   alufn   dw   sel1    sel2    | fp  | mode cmd       | cmd  mt    | fn     |     | fn      | fn      | fn       | fn      | fn       | fn   vrpu?|     fop
  //                       val? | | | |  | | |  | | |  | | |  | | | |     |       |    |       |       | |   | |    |         | |    |     | |      |     | |       | |       | |        | |       | |        | |       | |     |
    VLSB      -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLSH      -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLSW      -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLSD      -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLSBU     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_BU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLSHU     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_HU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLSWU     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_WU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSSB      -> List[BitPat](Y,Y,N,N,N,RX,N,Y,RS,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSSH      -> List[BitPat](Y,Y,N,N,N,RX,N,Y,RS,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSSW      -> List[BitPat](Y,Y,N,N,N,RX,N,Y,RS,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSSD      -> List[BitPat](Y,Y,N,N,N,RX,N,Y,RS,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLAB      -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLAH      -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLAW      -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLAD      -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLABU     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_BU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLAHU     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_HU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLAWU     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RA,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_L,MT_WU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSAB      -> List[BitPat](Y,Y,N,N,N,RX,N,Y,RA,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSAH      -> List[BitPat](Y,Y,N,N,N,RX,N,Y,RA,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSAW      -> List[BitPat](Y,Y,N,N,N,RX,N,Y,RA,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSAD      -> List[BitPat](Y,Y,N,N,N,RX,N,Y,RA,N,Y,RS,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      Y,SM_S,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),

    VLUI      -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_L,FN_ADD, DW64,A1_ZERO,A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAUIPC    -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_L,FN_ADD, DW64,A1_PC,  A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VADDI     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_ADD, DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSLLI     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SL,  DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSLTI     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SLT, DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSLTIU    -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SLTU,DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VXORI     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_XOR, DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSRLI     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SR,  DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSRAI     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SRA, DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VORI      -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_OR,  DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VANDI     -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_AND, DW64,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VADDIW    -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_ADD, DW32,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSLLIW    -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SL,  DW32,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSRLIW    -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SR,  DW32,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSRAIW    -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,N,IMM_I,FN_SRA, DW32,A1_RS1, A2_IMM, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),

    VCJAL     -> List[BitPat](Y,Y,N,N,Y,RS,N,N,RX,N,N,RX,N,N,RX,N,Y,IMM_U,FN_ADD, DW64,A1_PC,  A2_8,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   Y,N,    OP_X),
    VCJALR    -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RS,N,N,RX,N,N,RX,N,Y,IMM_U,FN_ADD, DW64,A1_PC,  A2_8,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   Y,N,    OP_X),
    VFIRST    -> List[BitPat](Y,Y,N,N,Y,RS,N,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,Y,    OP_X),
    VFENCE    -> List[BitPat](Y,Y,Y,N,N,RX,N,N,RX,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSTOP     -> List[BitPat](Y,Y,N,Y,N,RX,N,N,RX,N,N,RX,N,N,RX,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X))
}

object VectorMemoryDecode extends VFDecodeTable {
  val table: Array[(BitPat, List[BitPat])] = Array(
  //                         fence? stop?                                                              fpu?  vmu?             smu?         viu?     vipu? vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?       vrfu?
  //                      scalar? | | dv t d 1v t d 2v t d 3v t d p imm   alufn   dw   sel1    sel2    | fp  | mode cmd       | cmd  mt    | fn     |     | fn      | fn      | fn       | fn      | fn       | fn   vrpu?|     fop
  //                       val? | | | |  | | |  | | |  | | |  | | | |     |       |    |       |       | |   | |    |         | |    |     | |      |     | |       | |       | |        | |       | |        | |       | |     |
    VLB       -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLH       -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLW       -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLD       -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLBU      -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_BU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLHU      -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_HU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLWU      -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XRD,    N,SM_X,MT_WU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSB       -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XWR,    N,SM_X,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSH       -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XWR,    N,SM_X,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSW       -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XWR,    N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSD       -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_U,M_XWR,    N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),

    VLSTB     -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLSTH     -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLSTW     -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLSTD     -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLSTBU    -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_BU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLSTHU    -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_HU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLSTWU    -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XRD,    N,SM_X,MT_WU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSSTB     -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XWR,    N,SM_X,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSSTH     -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XWR,    N,SM_X,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSSTW     -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XWR,    N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSSTD     -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RA,N,Y,RA,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_S,M_XWR,    N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),

    VLXB      -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLXH      -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLXW      -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLXD      -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLXBU     -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_BU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLXHU     -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_HU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VLXWU     -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XRD,    N,SM_X,MT_WU,N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSXB      -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XWR,    N,SM_X,MT_B, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSXH      -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XWR,    N,SM_X,MT_H, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSXW      -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XWR,    N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VSXD      -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RS,N,Y,RV,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XWR,    N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),

    VAMOSWAP_W-> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_SWAP,N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOADD_W -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_ADD, N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOXOR_W -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_XOR, N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOAND_W -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_AND, N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOOR_W  -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_OR,  N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOMIN_W -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MIN, N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOMAX_W -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MAX, N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOMINU_W-> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MINU,N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOMAXU_W-> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MAXU,N,SM_X,MT_W, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOSWAP_D-> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_SWAP,N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOADD_D -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_ADD, N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOXOR_D -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_XOR, N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOAND_D -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_AND, N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOOR_D  -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_OR,  N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOMIN_D -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MIN, N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOMAX_D -> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MAX, N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOMINU_D-> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MINU,N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X),
    VAMOMAXU_D-> List[BitPat](Y,N,N,N,Y,RV,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,Y,VM_I,M_XA_MAXU,N,SM_X,MT_D, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,   N,N,    OP_X))
}

object VectorArithmeticDecode extends VFDecodeTable {
  val table: Array[(BitPat, List[BitPat])] = Array(
  //                         fence? stop?                                                              fpu?  vmu?             smu?         viu?     vipu? vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?       vrfu?
  //                      scalar? | | dv t d 1v t d 2v t d 3v t d p imm   alufn   dw   sel1    sel2    | fp  | mode cmd       | cmd  mt    | fn     |     | fn      | fn      | fn       | fn      | fn       | fn    vrpu?|    fop
  //                       val? | | | |  | | |  | | |  | | |  | | | |     |       |    |       |       | |   | |    |         | |    |     | |      |     | |       | |       | |        | |       | |        | |        | |    |
    VEIDX     -> List[BitPat](Y,N,N,N,Y,RV,N,N,RX,N,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_IDX, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VADD      -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_ADD, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_ADD, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VADDU     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_ADD, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_ADDU,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VSUB      -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SUB, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SUB, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VSLL      -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SL,  DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SLL, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VSLT      -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SLT, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SLT, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VSLTU     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SLTU,DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SLTU,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VXOR      -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_XOR, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_XOR, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VSRL      -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SR,  DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SRL, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VSRA      -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SRA, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SRA, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VOR       -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_OR,  DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_OR,  N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VAND      -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_AND, DW64,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_AND, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VMUL      -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    Y,IM_M,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VMULH     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    Y,IM_MH,  N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VMULHSU   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    Y,IM_MHSU,N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VMULHU    -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    Y,IM_MHU, N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VDIV      -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_DIV, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VDIVU     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_DIVU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VREM      -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_REM, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VREMU     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW64,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_REMU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VADDW     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_ADD, DW32,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_ADD, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VSUBW     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SUB, DW32,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SUB, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VSLLW     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SL,  DW32,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SLL, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VSRLW     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SR,  DW32,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SRL, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VSRAW     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_SRA, DW32,A1_RS1, A2_RS2, N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_SRA, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VMULW     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW32,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    Y,IM_M,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VDIVW     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW32,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_DIV, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VDIVUW    -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW32,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_DIVU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VREMW     -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW32,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_REM, N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VREMUW    -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW32,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   Y,ID_REMU,N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),

    VFMADD_D  -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FMADD),
    VFMSUB_D  -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FMSUB),
    VFNMADD_D -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FNMADD),
    VFNMSUB_D -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FNMSUB),
    VFADD_D   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FADD),
    VFSUB_D   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FSUB),
    VFMUL_D   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FMUL),
    // FIXME START
    VFDIV_D   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X,    N,N,   OP_FDIV),
    VFSQRT_D  -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X,    N,N,   OP_FSQRT),
    // FIXME END
    VFSGNJ_D  -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJ, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FSGNJ),
    VFSGNJN_D -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJN,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FSGNJ),
    VFSGNJX_D -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJX,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FSGNJ),
    VFMIN_D   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X,    N,N,   OP_FMINMAX),
    VFMAX_D   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X,    N,N,   OP_FMINMAX),
    VFCVT_D_S -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CSTD, N,N,   OP_FCVT_FF),
    VFCVT_D_H -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CHTD, N,N,   OP_FCVT_FF),
    VFCLASS_D -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X,    N,N,   OP_FCLASS),

  //                         fence? stop?                                                              fpu?  vmu?             smu?         viu?     vipu? vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?       vrfu?
  //                      scalar? | | dv t d 1v t d 2v t d 3v t d p imm   alufn   dw   sel1    sel2    | fp  | mode cmd       | cmd  mt    | fn     |     | fn      | fn      | fn       | fn      | fn       | fn   vrpu?|     fop
  //                       val? | | | |  | | |  | | |  | | |  | | | |     |       |    |       |       | |   | |    |         | |    |     | |      |     | |       | |       | |        | |       | |        | |       | |     |
    VFMADD_S  -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FMADD),
    VFMSUB_S  -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FMSUB),
    VFNMADD_S -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FNMADD),
    VFNMSUB_S -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FNMSUB),
    VFADD_S   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FADD),
    VFSUB_S   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FSUB),
    VFMUL_S   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FMUL),
    // FIXME START
    VFDIV_S   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X,    N,N,   OP_FDIV),
    VFSQRT_S  -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X,    N,N,   OP_FSQRT),
    // FIXME END
    VFSGNJ_S  -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJ, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FSGNJ),
    VFSGNJN_S -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJN,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FSGNJ),
    VFSGNJX_S -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJX,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FSGNJ),
    VFMIN_S   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X,    N,N,   OP_FMINMAX),
    VFMAX_S   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X,    N,N,   OP_FMINMAX),
    VFCVT_S_D -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CDTS, N,N,   OP_FCVT_FF),
    VFCVT_S_H -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CHTS, N,N,   OP_FCVT_FF),
    VFCLASS_S -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X,    N,N,   OP_FCLASS),

    VFMADD_H  -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MADD, N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FMADD),
    VFMSUB_H  -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MSUB, N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FMSUB),
    VFNMADD_H -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_NMADD,N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FNMADD),
    VFNMSUB_H -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,RX,Y,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_NMSUB,N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FNMSUB),
    VFADD_H   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_ADD,  N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FADD),
    VFSUB_H   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_SUB,  N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FSUB),
    VFMUL_H   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   Y,FM_MUL,  N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FMUL),
    // FIXME START
    VFDIV_H   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_DIV, N,FC_X,    N,FV_X,    N,N,   OP_FDIV),
    VFSQRT_H  -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    Y,FD_SQRT,N,FC_X,    N,FV_X,    N,N,   OP_FSQRT),
    // FIXME END
    VFSGNJ_H  -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJ, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FSGNJ),
    VFSGNJN_H -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJN,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FSGNJ),
    VFSGNJX_H -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_FSJX,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_FSGNJ),
    VFMIN_H   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MIN,  N,FV_X,    N,N,   OP_FMINMAX),
    VFMAX_H   -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_MAX,  N,FV_X,    N,N,   OP_FMINMAX),
    VFCVT_H_D -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CDTH, N,N,   OP_FCVT_FF),
    VFCVT_H_S -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CSTH, N,N,   OP_FCVT_FF),
    VFCLASS_H -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLASS,N,FV_X,    N,N,   OP_FCLASS),

  //                         fence? stop?                                                              fpu?  vmu?             smu?         viu?     vipu? vimu?     vidu?     vfmu?      vfdu?     vfcu?      vfvu?       vrfu?
  //                      scalar? | | dv t d 1v t d 2v t d 3v t d p imm   alufn   dw   sel1    sel2    | fp  | mode cmd       | cmd  mt    | fn     |     | fn      | fn      | fn       | fn      | fn       | fn   vrpu?|     fpfn
  //                       val? | | | |  | | |  | | |  | | |  | | | |     |       |    |       |       | |   | |    |         | |    |     | |      |     | |       | |       | |        | |       | |        | |       | |     |
    VFCVT_W_D -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW, N,N,   OP_FCVT_IF),
    VFCVT_WU_D-> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU,N,N,   OP_FCVT_IF),
    VFCVT_L_D -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL, N,N,   OP_FCVT_IF),
    VFCVT_LU_D-> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU,N,N,   OP_FCVT_IF),
    VFCVT_D_W -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF, N,N,   OP_FCVT_FI),
    VFCVT_D_WU-> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF,N,N,   OP_FCVT_FI),
    VFCVT_D_L -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF, N,N,   OP_FCVT_FI),
    VFCVT_D_LU-> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF,N,N,   OP_FCVT_FI),
    VFCVT_W_S -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW, N,N,   OP_FCVT_IF),
    VFCVT_WU_S-> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU,N,N,   OP_FCVT_IF),
    VFCVT_L_S -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL, N,N,   OP_FCVT_IF),
    VFCVT_LU_S-> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU,N,N,   OP_FCVT_IF),
    VFCVT_S_W -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF, N,N,   OP_FCVT_FI),
    VFCVT_S_WU-> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF,N,N,   OP_FCVT_FI),
    VFCVT_S_L -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF, N,N,   OP_FCVT_FI),
    VFCVT_S_LU-> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF,N,N,   OP_FCVT_FI),
    VFCVT_W_H -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTW, N,N,   OP_FCVT_IF),
    VFCVT_WU_H-> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTWU,N,N,   OP_FCVT_IF),
    VFCVT_L_H -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTL, N,N,   OP_FCVT_IF),
    VFCVT_LU_H-> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CFTLU,N,N,   OP_FCVT_IF),
    VFCVT_H_W -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWTF, N,N,   OP_FCVT_FI),
    VFCVT_H_WU-> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CWUTF,N,N,   OP_FCVT_FI),
    VFCVT_H_L -> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLTF, N,N,   OP_FCVT_FI),
    VFCVT_H_LU-> List[BitPat](Y,N,N,N,Y,RX,Y,Y,RX,Y,N,RX,N,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    Y,FV_CLUTF,N,N,   OP_FCVT_FI),

    VCMPEQ    -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_CEQ, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VCMPLT    -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_CLT, N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VCMPLTU   -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, Y,I_CLTU,N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X),
    VCMPFEQ_D -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X,    N,N,   OP_FCMP),
    VCMPFLT_D -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X,    N,N,   OP_FCMP),
    VCMPFLE_D -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPD,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X,    N,N,   OP_FCMP),
    VCMPFEQ_S -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X,    N,N,   OP_FCMP),
    VCMPFLT_S -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X,    N,N,   OP_FCMP),
    VCMPFLE_S -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPS,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X,    N,N,   OP_FCMP),
    VCMPFEQ_H -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CEQ,  N,FV_X,    N,N,   OP_FCMP),
    VCMPFLT_H -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLT,  N,FV_X,    N,N,   OP_FCMP),
    VCMPFLE_H -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RX,Y,Y,RX,Y,N,RX,N,Y,IMM_X,FN_X,   DW__,A1_X,   A2_X,   Y,FPH,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   N,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   Y,FC_CLE,  N,FV_X,    N,N,   OP_FCMP),

    VPOP      -> List[BitPat](Y,N,N,N,Y,RP,N,Y,RP,N,Y,RP,N,Y,RP,N,N,IMM_X,FN_X,   DW__,A1_X,   A2_X,   N,FP_,N,VM_X,M_X,      N,SM_X,MT_X, N,I_X,   Y,    N,IM_X,   N,ID_X,   N,FM_X,    N,FD_X,   N,FC_X,    N,FV_X,    N,N,   OP_X))
}

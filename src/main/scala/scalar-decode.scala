package hwacha

import Chisel._
import Node._
import Constants._
import rocket.ALU._
import uncore.constants.MemoryOpConstants._
import ScalarFPUDecode._

class IntCtrlSigs extends Bundle
{
  val ival = Bool()
  val vdi = Bits(width = RX.getWidth)
  val vri = Bits(width = RX.getWidth)
  val vsi = Bits(width = RX.getWidth)
  val vti = Bits(width = RX.getWidth)
  val sel_imm = Bits(width = IMM_X.getWidth)
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
    val decoder = rocket.DecodeLogic(inst, HwachaVFDecodeTable.default, table)
    Vec(ival, vdi, vri, vsi, vti, sel_imm, decode_scalar, alu_fn, alu_dw, sel_alu1,
        sel_alu2, fpu_val, fpu_fn, vmu_val, vmu_cmd, vmu_type, decode_stop) := decoder
    this
  }
}

object HwachaVFDecodeTable
{
  import HwachaElementInstructions._

                // val vd vs vt vr i scalar VIUfn  DW  alu1    alu2 fpu_val fpu_fn  mem cmd  mt stop
                //   | |  |  |  |  |     |  |      |      |       |       | |         | |     |    |
  val default = List(N,R_,R_,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N)
  val table = Array(                        
    // Hwacha Scalar Load/Store Instructions
    VSSSEGD   ->List(Y,R_,RS,RS,R_,IMM_X,Y,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FX,       Y,M_XWR,MT_D,N),
    VLSSEGD   ->List(Y,RS,RS,R_,R_,IMM_X,Y,FN_ADD, DW_XPR,A1_RS1, A2_ZERO,N,FX,       Y,M_XRD,MT_D,N),

    // Hwacha Scalar Integer Arithmetic Instructions
    VLUI      ->List(Y,RS,RS,R_,R_,IMM_U,Y,FN_ADD, DW_XPR,A1_ZERO,A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VADDI     ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_ADD, DW_XPR,A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VSLLI     ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_SL,  DW_XPR,A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VSLTI     ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_SLT, DW_XPR,A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VSLTIU    ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_SLTU,DW_XPR,A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VXORI     ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_XOR, DW_XPR,A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VSRLI     ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_SR,  DW_XPR,A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VSRAI     ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_SRA, DW_XPR,A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VORI      ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_OR,  DW_XPR,A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VANDI     ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_AND, DW_XPR,A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VADDIW    ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_ADD, DW_32, A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VSLLIW    ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_SL,  DW_32, A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VSRLIW    ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_SR,  DW_32, A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),
    VSRAIW    ->List(Y,RS,RS,R_,R_,IMM_I,Y,FN_SRA, DW_32, A1_RS1, A2_IMM, N,FX,       N,M_X,  MT_X,N),

    // Hwacha Vector Load/Store Instructions (Unit-Stride-Segmented)
    VLSEGB    ->List(Y,RV,RA,R_,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGH    ->List(Y,RV,RA,R_,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGW    ->List(Y,RV,RA,R_,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGD    ->List(Y,RV,RA,R_,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGBU   ->List(Y,RV,RA,R_,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGHU   ->List(Y,RV,RA,R_,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGWU   ->List(Y,RV,RA,R_,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VSSEGB    ->List(Y,RV,RA,R_,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VSSEGH    ->List(Y,RV,RA,R_,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VSSEGW    ->List(Y,RV,RA,R_,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VSSEGD    ->List(Y,RV,RA,R_,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),

    // Hwacha Vector Load/Store Instructions (Constant-Stride-Segmented)
    VLSEGSTB  ->List(Y,RV,RA,RA,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGSTH  ->List(Y,RV,RA,RA,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGSTW  ->List(Y,RV,RA,RA,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGSTD  ->List(Y,RV,RA,RA,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGSTBU ->List(Y,RV,RA,RA,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGSTHU ->List(Y,RV,RA,RA,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGSTWU ->List(Y,RV,RA,RA,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VSSEGSTB  ->List(Y,RV,RA,RA,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VSSEGSTH  ->List(Y,RV,RA,RA,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VSSEGSTW  ->List(Y,RV,RA,RA,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VSSEGSTD  ->List(Y,RV,RA,RA,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),

    // Hwacha Vector Indexed Load/Store Instructions
    VLSEGXB   ->List(Y,RV,RS,RV,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGXH   ->List(Y,RV,RS,RV,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGXW   ->List(Y,RV,RS,RV,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGXD   ->List(Y,RV,RS,RV,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGXBU  ->List(Y,RV,RS,RV,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGXHU  ->List(Y,RV,RS,RV,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VLSEGXWU  ->List(Y,RV,RS,RV,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VSSEGXB   ->List(Y,RV,RS,RV,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VSSEGXH   ->List(Y,RV,RS,RV,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VSSEGXW   ->List(Y,RV,RS,RV,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VSSEGXD   ->List(Y,RV,RS,RV,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),

    // Hwacha Vector Atomic Memory Instructions
    VAMOSWAP_W->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOADD_W ->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOXOR_W ->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOAND_W ->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOOR_W  ->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOMIN_W ->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOMAX_W ->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOMINU_W->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOMAXU_W->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOSWAP_D->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOADD_D ->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOXOR_D ->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOAND_D ->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOOR_D  ->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOMIN_D ->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOMAX_D ->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOMINU_D->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VAMOMAXU_D->List(Y,RV,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),

    // Hwacha Vector Integer Arithmetic Instructions
    VEIDX     ->List(Y,RV,R_,R_,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VADD      ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_ADD, DW_XPR,A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    // FIXME START
    VADDU     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_ADD, DW_XPR,A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    // FIXME END
    VSUB      ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_SUB, DW_XPR,A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    VSLL      ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_SL,  DW_XPR,A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    VSLT      ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_SLT, DW_XPR,A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    VSLTU     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_SLTU,DW_XPR,A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    VXOR      ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_XOR, DW_XPR,A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    VSRL      ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_SR,  DW_XPR,A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    VSRA      ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_SRA, DW_XPR,A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    VOR       ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_OR,  DW_XPR,A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    VAND      ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_AND, DW_XPR,A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    // FIXME START
    VMUL      ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VMULH     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VMULHSU   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VMULHU    ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VDIV      ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VDIVU     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VREM      ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VREMU     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_XPR,A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    // FIXME END
    VADDW     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_ADD, DW_32, A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    VSUBW     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_SUB, DW_32, A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    VSLLW     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_SL,  DW_32, A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    VSRLW     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_SR,  DW_32, A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    VSRAW     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_SRA, DW_32, A1_RS1, A2_RS2, N,FX,       N,M_X,  MT_X,N),
    // FIXME START
    VMULW     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_32, A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VDIVW     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_32, A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VDIVUW    ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_32, A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VREMW     ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_32, A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VREMUW    ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_32, A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    // FIXME END

    // Hwacha Vector Floating-Point Arithmetic Instructions (Double-Precision)
    VFMADD_D  ->List(Y,RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FMADD_D,  N,M_X,  MT_X,N),
    VFMSUB_D  ->List(Y,RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FMSUB_D,  N,M_X,  MT_X,N),
    VFNMADD_D ->List(Y,RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FNMADD_D, N,M_X,  MT_X,N),
    VFNMSUB_D ->List(Y,RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FNMSUB_D, N,M_X,  MT_X,N),
    VFADD_D   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FADD_D,   N,M_X,  MT_X,N),
    VFSUB_D   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FSUB_D,   N,M_X,  MT_X,N),
    VFMUL_D   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FMUL_D,   N,M_X,  MT_X,N),
    // FIXME START
    VFDIV_D   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFSQRT_D  ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    // FIXME END
    VFSGNJ_D  ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FSGNJ_D,  N,M_X,  MT_X,N),
    VFSGNJN_D ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FSGNJN_D, N,M_X,  MT_X,N),
    VFSGNJX_D ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FSGNJX_D, N,M_X,  MT_X,N),
    VFMIN_D   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FMIN_D,   N,M_X,  MT_X,N),
    VFMAX_D   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FMAX_D,   N,M_X,  MT_X,N),
    VFCVT_D_S ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_D_S, N,M_X,  MT_X,N),
    // FIXME START
    VFCVT_D_H ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    // FIXME END
    VFCLASS_D ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCLASS_D, N,M_X,  MT_X,N),

    // Hwacha Vector Floating-Point Arithmetic Instructions (Single-Precision)
    VFMADD_S  ->List(Y,RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FMADD_S,  N,M_X,  MT_X,N),
    VFMSUB_S  ->List(Y,RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FMSUB_S,  N,M_X,  MT_X,N),
    VFNMADD_S ->List(Y,RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FNMADD_S, N,M_X,  MT_X,N),
    VFNMSUB_S ->List(Y,RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FNMSUB_S, N,M_X,  MT_X,N),
    VFADD_S   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FADD_S,   N,M_X,  MT_X,N),
    VFSUB_S   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FSUB_S,   N,M_X,  MT_X,N),
    VFMUL_S   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FMUL_S,   N,M_X,  MT_X,N),
    // FIXME START
    VFDIV_S   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFSQRT_S  ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    // FIXME END
    VFSGNJ_S  ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FSGNJ_S,  N,M_X,  MT_X,N),
    VFSGNJN_S ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FSGNJN_S, N,M_X,  MT_X,N),
    VFSGNJX_S ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FSGNJX_S, N,M_X,  MT_X,N),
    VFMIN_S   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FMIN_S,   N,M_X,  MT_X,N),
    VFMAX_S   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FMAX_S,   N,M_X,  MT_X,N),
    VFCVT_S_D ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_S_D, N,M_X,  MT_X,N),
    // FIXME START
    VFCVT_S_H ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    // FIXME END
    VFCLASS_S ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCLASS_S, N,M_X,  MT_X,N),

    // Hwacha Vector Floating-Point Arithmetic Instructions (Half-Precision)
    // FIXME START
    VFMADD_H  ->List(Y,RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFMSUB_H  ->List(Y,RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFNMADD_H ->List(Y,RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFNMSUB_H ->List(Y,RX,RX,RX,RX,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFADD_H   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFSUB_H   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFMUL_H   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFDIV_H   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFSQRT_H  ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFSGNJ_H  ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFSGNJN_H ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFSGNJX_H ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFMIN_H   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFMAX_H   ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFCVT_H_D ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFCVT_H_S ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFCLASS_H ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    // FIXME END

    // Hwacha Vector Floating-Point to/from Integer Conversion Instructions
    VFCVT_W_D ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_W_D, N,M_X,  MT_X,N),
    VFCVT_WU_D->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_WU_D,N,M_X,  MT_X,N),
    VFCVT_L_D ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_L_D, N,M_X,  MT_X,N),
    VFCVT_LU_D->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_LU_D,N,M_X,  MT_X,N),
    VFCVT_D_W ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_D_W, N,M_X,  MT_X,N),
    VFCVT_D_WU->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_D_WU,N,M_X,  MT_X,N),
    VFCVT_D_L ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_D_L, N,M_X,  MT_X,N),
    VFCVT_D_LU->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_D_LU,N,M_X,  MT_X,N),
    VFCVT_W_S ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_W_S, N,M_X,  MT_X,N),
    VFCVT_WU_S->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_WU_S,N,M_X,  MT_X,N),
    VFCVT_L_S ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_L_S, N,M_X,  MT_X,N),
    VFCVT_LU_S->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_LU_S,N,M_X,  MT_X,N),
    VFCVT_S_W ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_S_W, N,M_X,  MT_X,N),
    VFCVT_S_WU->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_S_WU,N,M_X,  MT_X,N),
    VFCVT_S_L ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_S_L, N,M_X,  MT_X,N),
    VFCVT_S_LU->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FCVT_S_LU,N,M_X,  MT_X,N),
    // FIXME START
    VFCVT_W_H ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFCVT_WU_H->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFCVT_L_H ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFCVT_LU_H->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFCVT_H_W ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFCVT_H_WU->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFCVT_H_L ->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    VFCVT_H_LU->List(Y,RX,RX,R_,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FX,       N,M_X,  MT_X,N),
    // FIXME END

    // Hwacha Vector Compare Instructions
    // FIXME START
    VCMPEQ    ->List(Y,RP,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VCMPLT    ->List(Y,RP,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    VCMPLTU   ->List(Y,RP,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,N),
    // FIXME END
    VCMPFEQ_D ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FEQ_D,    N,M_X,  MT_X,N),
    VCMPFLT_D ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FLT_D,    N,M_X,  MT_X,N),
    VCMPFLE_D ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FLE_D,    N,M_X,  MT_X,N),
    VCMPFEQ_S ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FEQ_S,    N,M_X,  MT_X,N),
    VCMPFLT_S ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FLT_S,    N,M_X,  MT_X,N),
    VCMPFLE_S ->List(Y,RX,RX,RX,R_,IMM_X,N,FN_X,   DW_X,  A1_X,   A2_X,   Y,FLE_S,    N,M_X,  MT_X,N),

    // Hwacha Control Flow Instructions
    VSTOP     ->List(Y,R_,R_,R_,R_,IMM_X,Y,FN_X,   DW_X,  A1_X,   A2_X,   N,FX,       N,M_X,  MT_X,Y)
  )
}

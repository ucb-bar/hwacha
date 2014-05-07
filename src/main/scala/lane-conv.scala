package hwacha

import Chisel._
import Node._
import Constants._
import Compaction._

class LaneConv extends HwachaModule 
{
  val io = new Bundle {
    val valid = Bool(INPUT)
    val fn = new VAU2Fn().asInput
    val in = Bits(INPUT, SZ_DATA)
    val out = Bits(OUTPUT, SZ_DATA)
    val exc = Bits(OUTPUT, rocket.FPConstants.FLAGS_SZ)
  }

  def OP(ops: Bits*) = ops.toList.map(x => {io.fn.op === x}).reduceLeft( _ || _ )
  def FP(fp: Bits) = io.fn.fp === fp

  val op_int2float = MuxCase(
    Bits(0,2), Array(
      OP(A2_CLTF)  -> hardfloat.consts.type_int64,
      OP(A2_CLUTF) -> hardfloat.consts.type_uint64,
      OP(A2_CWTF)  -> hardfloat.consts.type_int32,
      OP(A2_CWUTF) -> hardfloat.consts.type_uint32
    ))

  val op_float2int = MuxCase(
    Bits(0,2), Array(
      OP(A2_CFTL)  -> hardfloat.consts.type_int64,
      OP(A2_CFTLU) -> hardfloat.consts.type_uint64,
      OP(A2_CFTW)  -> hardfloat.consts.type_int32,
      OP(A2_CFTWU) -> hardfloat.consts.type_uint32
    ))

  val val_dhp   = io.valid & FP(FPH) & OP(A2_MFTX)
  val val_ehp   = io.valid & FP(FPH) & OP(A2_MXTF)
  val val_ifsp  = io.valid & FP(FPS) & OP(A2_CLTF,A2_CLUTF,A2_CWTF,A2_CWUTF)
  val val_fisp  = io.valid & FP(FPS) & OP(A2_CFTL,A2_CFTLU,A2_CFTW,A2_CFTWU)
  val val_dsp   = io.valid & FP(FPS) & OP(A2_MFTX)
  val val_esp   = io.valid & FP(FPS) & OP(A2_MXTF)
  val val_dp2sp = io.valid & OP(A2_CDTS)
  val val_ifdp  = io.valid & FP(FPD) & OP(A2_CLTF,A2_CLUTF,A2_CWTF,A2_CWUTF)
  val val_fidp  = io.valid & FP(FPD) & OP(A2_CFTL,A2_CFTLU,A2_CFTW,A2_CFTWU)
  val val_ddp   = io.valid & FP(FPD) & OP(A2_MFTX)
  val val_edp   = io.valid & FP(FPD) & OP(A2_MXTF)
  val val_sp2dp = io.valid & OP(A2_CSTD)

  val dhp_in = Fill(16,val_dhp) & unpack_h(io.in, 0)
  val result_dhp = dhp_in

  val ehp_in = Fill(16,val_ehp) & io.in(15,0)
  val result_ehp = repack_h(ehp_in)

  val ifsp_in = Fill(64,val_ifsp) & io.in(63,0)
  val ifsp_rm = Fill(3,val_ifsp) & io.fn.rm
  val ifsp_to = Fill(2,val_ifsp) & op_int2float
  val ifsp_u = hardfloat.anyToRecodedFloatN(ifsp_in, ifsp_rm, ifsp_to, 23, 9, 64)
  val result_ifsp = repack_w(ifsp_u._1)
  val exc_ifsp = ifsp_u._2

  val fisp_in = Fill(33,val_fisp) & unpack_w(io.in, 0)
  val fisp_rm = Fill(3,val_fisp) & io.fn.rm
  val fisp_to = Fill(2,val_fisp) & op_float2int
  val fisp_u = hardfloat.recodedFloatNToAny(fisp_in, fisp_rm, fisp_to, 23, 9, 64)
  val result_fisp = fisp_u._1
  val exc_fisp = fisp_u._2

  val dsp_in = Fill(33,val_dsp) & unpack_w(io.in, 0)
  val result_dsp = hardfloat.recodedFloatNToFloatN(dsp_in, 23, 9)

  val esp_in = Fill(32,val_esp) & io.in(31,0)
  val result_esp = repack_w(hardfloat.floatNToRecodedFloatN(esp_in, 23, 9))

  val dp2sp_in = Fill(65,val_dp2sp) & unpack_d(io.in, 0)
  val dp2sp_rm = Fill(3,val_dp2sp) & io.fn.rm
  val dp2sp_u = hardfloat.recodedFloatNToRecodedFloatM(dp2sp_in, dp2sp_rm, 52, 12, 23, 9)
  val result_float2float_sp = repack_w(dp2sp_u._1)
  val exc_float2float_sp = dp2sp_u._2

  val ifdp_in = Fill(64,val_ifdp) & io.in(63,0)
  val ifdp_rm = Fill(3,val_ifdp) & io.fn.rm
  val ifdp_to = Fill(2,val_ifdp) & op_int2float
  val ifdp_u = hardfloat.anyToRecodedFloatN(ifdp_in, ifdp_rm, ifdp_to, 52, 12, 64)
  val result_ifdp = repack_d(ifdp_u._1)
  val exc_ifdp = ifdp_u._2

  val fidp_in = Fill(65,val_fidp) & unpack_d(io.in, 0)
  val fidp_rm = Fill(3,val_fidp) & io.fn.rm
  val fidp_to = Fill(2,val_fidp) & op_float2int
  val fidp_u = hardfloat.recodedFloatNToAny(fidp_in, fidp_rm, fidp_to, 52, 12, 64)
  val result_fidp = fidp_u._1
  val exc_fidp = fidp_u._2

  val ddp_in = Fill(65,val_ddp) & unpack_d(io.in, 0)
  val result_ddp = hardfloat.recodedFloatNToFloatN(ddp_in, 52, 12)

  val edp_in = Fill(64,val_edp) & io.in(63,0)
  val result_edp = repack_d(hardfloat.floatNToRecodedFloatN(edp_in, 52, 12))

  val sp2dp_in = Fill(33,val_sp2dp) & unpack_w(io.in, 0)
  val sp2dp_rm = Fill(3,val_sp2dp) & io.fn.rm
  val sp2dp_u = hardfloat.recodedFloatNToRecodedFloatM(sp2dp_in, sp2dp_rm, 23, 9, 52, 12)
  val result_float2float_dp = repack_d(sp2dp_u._1)
  val exc_float2float_dp = sp2dp_u._2

  val next_result_hp = MuxCase(
    Bits(0, SZ_DATA), Array(
      OP(A2_MXTF) -> result_ehp,
      OP(A2_MFTX) -> Cat(Bits(0,2),Fill(48,result_dhp(15)),result_dhp(15,0))
  ))

  val next_result_sp = MuxCase(
    Bits(0, SZ_DATA), Array(
      OP(A2_CLTF,A2_CLUTF,A2_CWTF,A2_CWUTF) -> result_ifsp,
      OP(A2_MXTF) -> result_esp,
      OP(A2_CFTL,A2_CFTLU) -> Cat(Bits(0,1),result_fisp(63,0)),
      OP(A2_CFTW,A2_CFTWU) -> Cat(Bits(0,1),Fill(32,result_fisp(31)),result_fisp(31,0)),
      OP(A2_MFTX) -> Cat(Bits(0,1),Fill(32,result_dsp(31)),result_dsp(31,0)),
      OP(A2_CDTS) -> result_float2float_sp
    ))

  val next_result_dp = MuxCase(
    Bits(0, SZ_DATA), Array(
      OP(A2_CLTF,A2_CLUTF,A2_CWTF,A2_CWUTF) -> result_ifdp,
      OP(A2_MXTF) -> result_edp,
      OP(A2_CFTL,A2_CFTLU) -> Cat(Bits(0,1),result_fidp(63,0)),
      OP(A2_CFTW,A2_CFTWU) -> Cat(Bits(0,1),Fill(32,result_fidp(31)),result_fidp(31,0)),
      OP(A2_MFTX) -> Cat(Bits(0,1),result_ddp(63,0)),
      OP(A2_CSTD) -> result_float2float_dp
    ))

  val next_exc_sp = MuxCase(
    Bits(0), Array(
      OP(A2_CLTF,A2_CLUTF,A2_CWTF,A2_CWUTF) -> exc_ifsp,
      OP(A2_CFTL,A2_CFTLU,A2_CFTW,A2_CFTWU) -> exc_fisp,
      OP(A2_CDTS) -> exc_float2float_sp
    ))

  val next_exc_dp = MuxCase(
    Bits(0), Array(
      OP(A2_CLTF,A2_CLUTF,A2_CWTF,A2_CWUTF) -> exc_ifdp,
      OP(A2_CFTL,A2_CFTLU,A2_CFTW,A2_CFTWU) -> exc_fidp,
      OP(A2_CSTD) -> exc_float2float_dp
    ))

  val result = MuxCase(Cat(next_exc_dp, next_result_dp), Array(
    FP(FPD) -> Cat(next_exc_dp, next_result_dp),
    FP(FPS) -> Cat(next_exc_sp, next_result_sp),
    FP(FPH) -> Cat(Bits(0), next_result_hp)
  ))

  val pipereg = ShiftRegister(result, fconv_stages, io.valid)

  Match(pipereg, io.exc, io.out)
}

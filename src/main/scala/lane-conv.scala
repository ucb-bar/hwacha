package hwacha

import Chisel._
import Node._
import Constants._
import Compaction._

class LaneConv(implicit conf: HwachaConfiguration) extends Module 
{
  val io = new Bundle {
    val valid = Bool(INPUT)
    val fn = new VAU2Fn().asInput
    val ut = UInt(INPUT, 2)
    val in = Bits(INPUT, SZ_DATA)
    val out = Bits(OUTPUT, SZ_DATA)
    val exc = Bits(OUTPUT, rocket.FPConstants.FLAGS_SZ)
  }

  def OP(ops: Bits*) = ops.toList.map(x => {io.fn.op === x}).reduceLeft( _ || _ )
  def FP(fp: Bits) = io.fn.fp === fp

  val op_int2float = MuxCase(
    Bits(0,2), Array(
      OP(A2_CLTF)  -> hardfloat.fpu_recoded.type_int64,
      OP(A2_CLUTF) -> hardfloat.fpu_recoded.type_uint64,
      OP(A2_CWTF)  -> hardfloat.fpu_recoded.type_int32,
      OP(A2_CWUTF) -> hardfloat.fpu_recoded.type_uint32
    ))

  val op_float2int = MuxCase(
    Bits(0,2), Array(
      OP(A2_CFTL)  -> hardfloat.fpu_recoded.type_int64,
      OP(A2_CFTLU) -> hardfloat.fpu_recoded.type_uint64,
      OP(A2_CFTW)  -> hardfloat.fpu_recoded.type_int32,
      OP(A2_CFTWU) -> hardfloat.fpu_recoded.type_uint32
    ))

  val val_decode_hp    = io.valid & FP(FPH) & OP(A2_MFTX)
  val val_encode_hp    = io.valid & FP(FPH) & OP(A2_MXTF)
  val val_int2float_sp = io.valid & FP(FPS) & OP(A2_CLTF,A2_CLUTF,A2_CWTF,A2_CWUTF)
  val val_float2int_sp = io.valid & FP(FPS) & OP(A2_CFTL,A2_CFTLU,A2_CFTW,A2_CFTWU)
  val val_decode_sp    = io.valid & FP(FPS) & OP(A2_MFTX)
  val val_encode_sp    = io.valid & FP(FPS) & OP(A2_MXTF)
  val val_dp2sp        = io.valid & OP(A2_CDTS)
  val val_int2float_dp = io.valid & FP(FPD) & OP(A2_CLTF,A2_CLUTF,A2_CWTF,A2_CWUTF)
  val val_float2int_dp = io.valid & FP(FPD) & OP(A2_CFTL,A2_CFTLU,A2_CFTW,A2_CFTWU)
  val val_decode_dp    = io.valid & FP(FPD) & OP(A2_MFTX)
  val val_encode_dp    = io.valid & FP(FPD) & OP(A2_MXTF)
  val val_sp2dp        = io.valid & OP(A2_CSTD)

  val sel = MuxLookup(io.fn.prec.vs, UInt(0), Array(
    PREC_DOUBLE -> UInt(0),
    PREC_SINGLE -> (io.ut(0) << UInt(1)),
      PREC_HALF -> io.ut
  ))

  val hps = Vec((0 until N_XH).map(i => (io.in >> UInt(i * SZ_XH))))
  val result_decode_hp = Fill(16,val_decode_hp) & unpack_h(hps(sel), 0)

  val result_encode_hp = repack_h(Fill(16,val_encode_hp) & io.in(15,0))

  val int2float_sp = Module(new hardfloat.anyToRecodedFloat32)
  int2float_sp.io.in := Fill(64,val_int2float_sp) & io.in(63,0)
  int2float_sp.io.roundingMode := Fill(3,val_int2float_sp) & io.fn.rm
  int2float_sp.io.typeOp := Fill(2,val_int2float_sp) & op_int2float
  val result_int2float_sp = repack_w(int2float_sp.io.out)
  val exc_int2float_sp = int2float_sp.io.exceptionFlags

  val float2int_sp = Module(new hardfloat.recodedFloat32ToAny)
  float2int_sp.io.in := Fill(33,val_float2int_sp) & unpack_w(io.in, 0)
  float2int_sp.io.roundingMode := Fill(3,val_float2int_sp) & io.fn.rm
  float2int_sp.io.typeOp := Fill(2,val_float2int_sp) & op_float2int
  val result_float2int_sp = float2int_sp.io.out
  val exc_float2int_sp = float2int_sp.io.exceptionFlags

  val decode_sp = Module(new hardfloat.recodedFloat32ToFloat32)
  decode_sp.io.in := Fill(33,val_decode_sp) & unpack_w(io.in, 0)
  val result_decode_sp = decode_sp.io.out

  val encode_sp = Module(new hardfloat.float32ToRecodedFloat32)
  encode_sp.io.in := Fill(32,val_encode_sp) & io.in(31,0)
  val result_encode_sp = repack_w(encode_sp.io.out)

  val dp2sp = Module(new hardfloat.recodedFloat64ToRecodedFloat32)
  dp2sp.io.in := Fill(65,val_dp2sp) & unpack_d(io.in, 0)
  dp2sp.io.roundingMode := Fill(3,val_dp2sp) & io.fn.rm
  val result_float2float_sp = repack_w(dp2sp.io.out)
  val exc_float2float_sp = dp2sp.io.exceptionFlags

  val int2float_dp = Module(new hardfloat.anyToRecodedFloat64) 
  int2float_dp.io.in := Fill(64,val_int2float_dp) & io.in(63,0)
  int2float_dp.io.roundingMode := Fill(3,val_int2float_dp) & io.fn.rm
  int2float_dp.io.typeOp := Fill(2,val_int2float_dp) & op_int2float
  val result_int2float_dp = repack_d(int2float_dp.io.out)
  val exc_int2float_dp = int2float_dp.io.exceptionFlags

  val float2int_dp = Module(new hardfloat.recodedFloat64ToAny)
  float2int_dp.io.in := Fill(65,val_float2int_dp) & unpack_d(io.in, 0)
  float2int_dp.io.roundingMode := Fill(3,val_float2int_dp) & io.fn.rm
  float2int_dp.io.typeOp := Fill(2,val_float2int_dp) & op_float2int
  val result_float2int_dp = float2int_dp.io.out
  val exc_float2int_dp = float2int_dp.io.exceptionFlags

  val decode_dp = Module(new hardfloat.recodedFloat64ToFloat64)
  decode_dp.io.in := Fill(65,val_decode_dp) & unpack_d(io.in, 0)
  val result_decode_dp = decode_dp.io.out

  val encode_dp = Module(new hardfloat.float64ToRecodedFloat64)
  encode_dp.io.in := Fill(64,val_encode_dp) & io.in(63,0)
  val result_encode_dp = repack_d(encode_dp.io.out)

  val sp2dp = Module(new hardfloat.rF32_rF64)
  sp2dp.io.in := Fill(33,val_sp2dp) & unpack_w(io.in, 0)
  val result_float2float_dp = repack_d(sp2dp.io.out)
  val exc_float2float_dp = sp2dp.io.exception_flags

  val next_result_hp = MuxCase(
    Bits(0, SZ_DATA), Array(
      OP(A2_MXTF) -> result_encode_hp,
      OP(A2_MFTX) -> Cat(Bits(0,2), Fill(48, result_decode_hp(15)), result_decode_hp(15,0))
  ))

  val next_result_sp = MuxCase(
    Bits(0, SZ_DATA), Array(
      OP(A2_CLTF,A2_CLUTF,A2_CWTF,A2_CWUTF) -> result_int2float_sp,
      OP(A2_MXTF) -> result_encode_sp,
      OP(A2_CFTL,A2_CFTLU) -> Cat(Bits(0,1),result_float2int_sp(63,0)),
      OP(A2_CFTW,A2_CFTWU) -> Cat(Bits(0,1),Fill(32,result_float2int_sp(31)),result_float2int_sp(31,0)),
      OP(A2_MFTX) -> Cat(Bits(0,1),Fill(32,result_decode_sp(31)),result_decode_sp(31,0)),
      OP(A2_CDTS) -> result_float2float_sp
    ))

  val next_result_dp = MuxCase(
    Bits(0, SZ_DATA), Array(
      OP(A2_CLTF,A2_CLUTF,A2_CWTF,A2_CWUTF) -> result_int2float_dp,
      OP(A2_MXTF) -> result_encode_dp,
      OP(A2_CFTL,A2_CFTLU) -> Cat(Bits(0,1),result_float2int_dp(63,0)),
      OP(A2_CFTW,A2_CFTWU) -> Cat(Bits(0,1),Fill(32,result_float2int_dp(31)),result_float2int_dp(31,0)),
      OP(A2_MFTX) -> Cat(Bits(0,1),result_decode_dp(63,0)),
      OP(A2_CSTD) -> result_float2float_dp
    ))

  val next_exc_sp = MuxCase(
    Bits(0), Array(
      OP(A2_CLTF,A2_CLUTF,A2_CWTF,A2_CWUTF) -> exc_int2float_sp,
      OP(A2_CFTL,A2_CFTLU,A2_CFTW,A2_CFTWU) -> exc_float2int_sp,
      OP(A2_CDTS) -> exc_float2float_sp
    ))

  val next_exc_dp = MuxCase(
    Bits(0), Array(
      OP(A2_CLTF,A2_CLUTF,A2_CWTF,A2_CWUTF) -> exc_int2float_dp,
      OP(A2_CFTL,A2_CFTLU,A2_CFTW,A2_CFTWU) -> exc_float2int_dp,
      OP(A2_CSTD) -> exc_float2float_dp
    ))

  val result = MuxCase(Cat(next_exc_dp, next_result_dp), Array(
    FP(FPD) -> Cat(next_exc_dp, next_result_dp),
    FP(FPS) -> Cat(next_exc_sp, next_result_sp),
    FP(FPH) -> Cat(Bits(0), next_result_hp)
  ))

  val pipereg = ShiftRegister(result, conf.fconv_stages, io.valid)

  Match(pipereg, io.exc, io.out)
}

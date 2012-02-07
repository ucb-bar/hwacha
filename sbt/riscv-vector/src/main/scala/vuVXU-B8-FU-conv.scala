package riscvVector

import Chisel._
import Node._
import Fpu._
import Config._
import fpu_recoded._

class vuVXU_Banked8_FU_conv extends Component 
{
  val io = new Bundle
  {
    val valid = Bool(INPUT);
    val fn = Bits(DEF_VAU2_FN, INPUT);
    val in = Bits(DEF_DATA, INPUT);
    val exc = Bits(DEF_EXC, OUTPUT);
    val out = Bits(DEF_DATA, OUTPUT);
  }

  def VAU2_FN(ins: Bits*) = ins.toList.map(x => {io.fn(RG_VAU2_FN) === x}).reduceLeft( _ || _ );
  def VAU2_FP(fp: Bits) = io.fn(RG_VAU2_FP) === fp

  val op_int2float = MuxCase(
    Bits(0,2), Array(
      VAU2_FN(VAU2_CLTF)  -> type_int64,
      VAU2_FN(VAU2_CLUTF) -> type_uint64,
      VAU2_FN(VAU2_CWTF)  -> type_int32,
      VAU2_FN(VAU2_CWUTF) -> type_uint32
    ));

  val op_float2int = MuxCase(
    Bits(0,2), Array(
      VAU2_FN(VAU2_CFTL)  -> type_int64,
      VAU2_FN(VAU2_CFTLU) -> type_uint64,
      VAU2_FN(VAU2_CFTW)  -> type_int32,
      VAU2_FN(VAU2_CFTWU) -> type_uint32
    ));

  val val_int2float_sp = io.valid & VAU2_FP(FPS) & VAU2_FN(VAU2_CLTF,VAU2_CLUTF,VAU2_CWTF,VAU2_CWUTF);
  val val_float2int_sp = io.valid & VAU2_FP(FPS) & VAU2_FN(VAU2_CFTL,VAU2_CFTLU,VAU2_CFTW,VAU2_CFTWU);
  val val_decode_sp    = io.valid & VAU2_FP(FPS) & VAU2_FN(VAU2_MFTX);
  val val_encode_sp    = io.valid & VAU2_FP(FPS) & VAU2_FN(VAU2_MXTF);
  val val_dp2sp        = io.valid & VAU2_FN(VAU2_CDTS);
  val val_int2float_dp = io.valid & VAU2_FP(FPD) & VAU2_FN(VAU2_CLTF,VAU2_CLUTF,VAU2_CWTF,VAU2_CWUTF);
  val val_float2int_dp = io.valid & VAU2_FP(FPD) & VAU2_FN(VAU2_CFTL,VAU2_CFTLU,VAU2_CFTW,VAU2_CFTWU);
  val val_decode_dp    = io.valid & VAU2_FP(FPD) & VAU2_FN(VAU2_MFTX);
  val val_encode_dp    = io.valid & VAU2_FP(FPD) & VAU2_FN(VAU2_MXTF);
  val val_sp2dp        = io.valid & VAU2_FN(VAU2_CSTD);

  val int2float_sp = new anyToRecodedFloat32();
  int2float_sp.io.in := Fill(64,val_int2float_sp) & io.in(63,0);
  int2float_sp.io.roundingMode := Fill(2,val_int2float_sp) & io.fn(RG_VAU2_RM);
  int2float_sp.io.typeOp := Fill(2,val_int2float_sp) & op_int2float;
  val result_int2float_sp = int2float_sp.io.out;
  val exc_int2float_sp = int2float_sp.io.exceptionFlags;

  val float2int_sp = new recodedFloat32ToAny();
  float2int_sp.io.in := Fill(33,val_float2int_sp) & io.in(32,0);
  float2int_sp.io.roundingMode := Fill(2,val_float2int_sp) & io.fn(RG_VAU2_RM);
  float2int_sp.io.typeOp := Fill(2,val_float2int_sp) & op_float2int;
  val result_float2int_sp = float2int_sp.io.out;
  val exc_float2int_sp = float2int_sp.io.exceptionFlags;

  val decode_sp = new recodedFloat32ToFloat32();
  decode_sp.io.in := Fill(33,val_decode_sp) & io.in(32,0);
  val result_decode_sp = decode_sp.io.out;

  val encode_sp = new float32ToRecodedFloat32();
  encode_sp.io.in := Fill(32,val_encode_sp) & io.in(31,0);
  val result_encode_sp = encode_sp.io.out;

  val dp2sp = new recodedFloat64ToRecodedFloat32();
  dp2sp.io.in := Fill(65,val_dp2sp) & io.in;
  dp2sp.io.roundingMode := Fill(2,val_dp2sp) & io.fn(RG_VAU2_RM);
  val result_float2float_sp = dp2sp.io.out;
  val exc_float2float_sp = dp2sp.io.exceptionFlags;

  val int2float_dp = new anyToRecodedFloat64(); 
  int2float_dp.io.in := Fill(64,val_int2float_dp) & io.in(63,0);
  int2float_dp.io.roundingMode := Fill(2,val_int2float_dp) & io.fn(RG_VAU2_RM);
  int2float_dp.io.typeOp := Fill(2,val_int2float_dp) & op_int2float;
  val result_int2float_dp = int2float_dp.io.out;
  val exc_int2float_dp = int2float_dp.io.exceptionFlags;

  val float2int_dp = new recodedFloat64ToAny();
  float2int_dp.io.in := Fill(65,val_float2int_dp) & io.in;
  float2int_dp.io.roundingMode := Fill(2,val_float2int_dp) & io.fn(RG_VAU2_RM);
  float2int_dp.io.typeOp := Fill(2,val_float2int_dp) & op_float2int;
  val result_float2int_dp = float2int_dp.io.out;
  val exc_float2int_dp = float2int_dp.io.exceptionFlags;

  val decode_dp = new recodedFloat64ToFloat64();
  decode_dp.io.in := Fill(65,val_decode_dp) & io.in;
  val result_decode_dp = decode_dp.io.out;

  val encode_dp = new float64ToRecodedFloat64();
  encode_dp.io.in := Fill(64,val_encode_dp) & io.in(63,0);
  val result_encode_dp = encode_dp.io.out;

  val sp2dp = new rF32_rF64();
  sp2dp.io.in := Fill(33,val_sp2dp) & io.in(32,0);
  val result_float2float_dp = sp2dp.io.out;
  val exc_float2float_dp = sp2dp.io.exception_flags;

  val next_result_sp = MuxCase(
    Bits(0, SZ_DATA), Array(
      VAU2_FN(VAU2_CLTF,VAU2_CLUTF,VAU2_CWTF,VAU2_CWUTF) -> Cat(Bits("hFFFF_FFFF",32), result_int2float_sp(32,0)),
      VAU2_FN(VAU2_MXTF) -> Cat(Bits("hFFFF_FFFF",32), result_encode_sp(32,0)),
      VAU2_FN(VAU2_CFTL,VAU2_CFTLU) -> Cat(Bits(0,1),result_float2int_sp(63,0)),
      VAU2_FN(VAU2_CFTW,VAU2_CFTWU) -> Cat(Bits(0,1),Fill(32,result_float2int_sp(31)),result_float2int_sp(31,0)),
      VAU2_FN(VAU2_MFTX) -> Cat(Bits(0,1),Fill(32,result_decode_sp(31)),result_decode_sp(31,0)),
      VAU2_FN(VAU2_CDTS) -> Cat(Bits("hFFFF_FFFF",32), result_float2float_sp(32,0))
    ));

  val next_result_dp = MuxCase(
    Bits(0, SZ_DATA), Array(
      VAU2_FN(VAU2_CLTF,VAU2_CLUTF,VAU2_CWTF,VAU2_CWUTF) -> result_int2float_dp,
      VAU2_FN(VAU2_MXTF) -> result_encode_dp,
      VAU2_FN(VAU2_CFTL,VAU2_CFTLU) -> Cat(Bits(0,1),result_float2int_dp(63,0)),
      VAU2_FN(VAU2_CFTW,VAU2_CFTWU) -> Cat(Bits(0,1),Fill(32,result_float2int_dp(31)),result_float2int_dp(31,0)),
      VAU2_FN(VAU2_MFTX) -> Cat(Bits(0,1),result_decode_dp(63,0)),
      VAU2_FN(VAU2_CSTD) -> result_float2float_dp
    ));

  val next_exc_sp = MuxCase(
    Bits(0, SZ_EXC), Array(
      VAU2_FN(VAU2_CLTF,VAU2_CLUTF,VAU2_CWTF,VAU2_CWUTF) -> exc_int2float_sp,
      VAU2_FN(VAU2_CFTL,VAU2_CFTLU,VAU2_CFTW,VAU2_CFTWU) -> exc_float2int_sp,
      VAU2_FN(VAU2_CDTS) -> exc_float2float_sp
    ));

  val next_exc_dp = MuxCase(
    Bits(0, SZ_EXC), Array(
      VAU2_FN(VAU2_CLTF,VAU2_CLUTF,VAU2_CWTF,VAU2_CWUTF) -> exc_int2float_dp,
      VAU2_FN(VAU2_CFTL,VAU2_CFTLU,VAU2_CFTW,VAU2_CFTWU) -> exc_float2int_dp,
      VAU2_FN(VAU2_CSTD) -> exc_float2float_dp
    ));

  val result = Mux(
    VAU2_FP(FPD), Cat(next_exc_dp, next_result_dp),
    Cat(next_exc_sp, next_result_sp));

  val pipereg = ShiftRegister(FCONV_STAGES-1, DEF_DATA+DEF_EXC, io.valid, result);

  Match(pipereg, io.exc, io.out);
}

`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"
`include "fpu_recoded.vh"

module vuVXU_Banked8_FU_conv
(
  input clk,
  input reset,
  input val,

  input `DEF_VAU2_FN fn,
  input `DEF_DATA in,
  output `DEF_EXC exc,
  output `DEF_DATA out
);

  `define VAU2_FN1(fn0_) (fn[`RG_VAU2_FN] == fn0_)
  `define VAU2_FN2(fn0_, fn1_) ((fn[`RG_VAU2_FN] == fn0_) || (fn[`RG_VAU2_FN] == fn1_))
  `define VAU2_FN4(fn0_, fn1_, fn2_, fn3_) ((fn[`RG_VAU2_FN] == fn0_) || (fn[`RG_VAU2_FN] == fn1_) || (fn[`RG_VAU2_FN] == fn2_) || (fn[`RG_VAU2_FN] == fn3_))
  `define VAU2_FP(fp_) (fn[`RG_VAU2_FP] == fp_)

  wire [1:0] op_int2float
    = `VAU2_FN1(`VAU2_CLTF) ? `type_int64
    : `VAU2_FN1(`VAU2_CLUTF) ? `type_uint64
    : `VAU2_FN1(`VAU2_CWTF) ? `type_int32
    : `VAU2_FN1(`VAU2_CWUTF) ? `type_uint32
    : 2'bx;

  wire [1:0] op_float2int
    = `VAU2_FN1(`VAU2_CFTL) ? `type_int64
    : `VAU2_FN1(`VAU2_CFTLU) ? `type_uint64
    : `VAU2_FN1(`VAU2_CFTW) ? `type_int32
    : `VAU2_FN1(`VAU2_CFTWU) ? `type_uint32
    : 2'bx;

  wire `DEF_DATA result_int2float_sp;
  wire `DEF_DATA result_float2int_sp;
  wire `DEF_DATA result_decode_sp;
  wire `DEF_DATA result_encode_sp;
  wire `DEF_DATA result_float2float_sp;

  wire `DEF_EXC exc_int2float_sp;
  wire `DEF_EXC exc_float2int_sp;
  wire `DEF_EXC exc_float2float_sp;

  wire `DEF_DATA result_int2float_dp;
  wire `DEF_DATA result_float2int_dp;
  wire `DEF_DATA result_decode_dp;
  wire `DEF_DATA result_encode_dp;
  wire `DEF_DATA result_float2float_dp;

  wire `DEF_EXC exc_int2float_dp;
  wire `DEF_EXC exc_float2int_dp;
  wire `DEF_EXC exc_float2float_dp;

  wire val_int2float_sp = val & `VAU2_FP(`FPS) & `VAU2_FN4(`VAU2_CLTF,`VAU2_CLUTF,`VAU2_CWTF,`VAU2_CWUTF);
  wire val_float2int_sp = val & `VAU2_FP(`FPS) & `VAU2_FN4(`VAU2_CFTL,`VAU2_CFTLU,`VAU2_CFTW,`VAU2_CFTWU);
  wire val_decode_sp = val & `VAU2_FP(`FPS) & `VAU2_FN1(`VAU2_MFTX);
  wire val_encode_sp = val & `VAU2_FP(`FPS) & `VAU2_FN1(`VAU2_MXTF);
  wire val_dp2sp = val & `VAU2_FN1(`VAU2_CDTS);
  wire val_int2float_dp = val & `VAU2_FP(`FPD) & `VAU2_FN4(`VAU2_CLTF,`VAU2_CLUTF,`VAU2_CWTF,`VAU2_CWUTF);
  wire val_float2int_dp = val & `VAU2_FP(`FPD) & `VAU2_FN4(`VAU2_CFTL,`VAU2_CFTLU,`VAU2_CFTW,`VAU2_CFTWU);
  wire val_decode_dp = val & `VAU2_FP(`FPD) & `VAU2_FN1(`VAU2_MFTX);
  wire val_encode_dp = val & `VAU2_FP(`FPD) & `VAU2_FN1(`VAU2_MXTF);
  wire val_sp2dp = val & `VAU2_FN1(`VAU2_CSTD);

  anyToRecodedFloat32 int2float_sp
  (
    .in({64{val_int2float_sp}} & in[63:0]),
    .roundingMode({2{val_int2float_sp}} & fn[`RG_VAU2_RM]),
    .typeOp({2{val_int2float_sp}} & op_int2float),
    .out(result_int2float_sp[32:0]),
    .exceptionFlags(exc_int2float_sp)
  );

  recodedFloat32ToAny float2int_sp
  (
    .in({33{val_float2int_sp}} & in[32:0]),
    .roundingMode({2{val_float2int_sp}} & fn[`RG_VAU2_RM]),
    .typeOp({2{val_float2int_sp}} & op_float2int),
    .out(result_float2int_sp[63:0]),
    .exceptionFlags(exc_float2int_sp)
  );

  recodedFloatNToFloatN#(8,24) decode_sp
  (
    .in({33{val_decode_sp}} & in[32:0]),
    .out(result_decode_sp[31:0])
  );

  floatNToRecodedFloatN#(8,24) encode_sp
  (
    .in({32{val_encode_sp}} & in[31:0]),
    .out(result_encode_sp[32:0])
  );

  recodedFloat64ToRecodedFloat32 dp2sp
  (
    .in({65{val_dp2sp}} & in),
    .roundingMode({2{val_dp2sp}} & fn[`RG_VAU2_RM]),
    .out(result_float2float_sp[32:0]),
    .exceptionFlags(exc_float2float_sp)
  );

  anyToRecodedFloat64 int2float_dp
  (
    .in({64{val_int2float_dp}} & in[63:0]),
    .roundingMode({2{val_int2float_dp}} & fn[`RG_VAU2_RM]),
    .typeOp({2{val_int2float_dp}} & op_int2float),
    .out(result_int2float_dp),
    .exceptionFlags(exc_int2float_dp)
  );

  recodedFloat64ToAny float2int_dp
  (
    .in({65{val_float2int_dp}} & in),
    .roundingMode({2{val_float2int_dp}} & fn[`RG_VAU2_RM]),
    .typeOp({2{val_float2int_dp}} & op_float2int),
    .out(result_float2int_dp[63:0]),
    .exceptionFlags(exc_float2int_dp)
  );

  recodedFloatNToFloatN#(11,53) decode_dp
  (
    .in({65{val_decode_dp}} & in),
    .out(result_decode_dp[63:0])
  );

  floatNToRecodedFloatN#(11,53) encode_dp
  (
    .in({64{val_encode_dp}} & in[63:0]),
    .out(result_encode_dp)
  );

  recodedFloat32ToRecodedFloat64 sp2dp
  (
    .in({33{val_sp2dp}} & in[32:0]),
    .out(result_float2float_dp),
    .exceptionFlags(exc_float2float_dp)
  );

  wire `DEF_DATA next_result_sp
    = `VAU2_FN4(`VAU2_CLTF,`VAU2_CLUTF,`VAU2_CWTF,`VAU2_CWUTF) ? {32'hFFFF_FFFF, result_int2float_sp[32:0]}
    : `VAU2_FN1(`VAU2_MXTF) ? {32'hFFFF_FFFF, result_encode_sp[32:0]}
    : `VAU2_FN2(`VAU2_CFTL,`VAU2_CFTLU) ? {1'd0,result_float2int_sp[63:0]}
    : `VAU2_FN2(`VAU2_CFTW,`VAU2_CFTWU) ? {1'd0,{32{result_float2int_sp[31]}},result_float2int_sp[31:0]}
    : `VAU2_FN1(`VAU2_MFTX) ? {1'd0,{32{result_decode_sp[31]}},result_decode_sp[31:0]}
    : `VAU2_FN1(`VAU2_CDTS) ? {32'hFFFF_FFFF, result_float2float_sp[32:0]}
    : `SZ_DATA'bx;

  wire `DEF_DATA next_result_dp
    = `VAU2_FN4(`VAU2_CLTF,`VAU2_CLUTF,`VAU2_CWTF,`VAU2_CWUTF) ? result_int2float_dp
    : `VAU2_FN1(`VAU2_MXTF) ? result_encode_dp
    : `VAU2_FN2(`VAU2_CFTL,`VAU2_CFTLU) ? {1'd0,result_float2int_dp[63:0]}
    : `VAU2_FN2(`VAU2_CFTW,`VAU2_CFTWU) ? {1'd0,{32{result_float2int_dp[31]}},result_float2int_dp[31:0]}
    : `VAU2_FN1(`VAU2_MFTX) ? {1'd0,result_decode_dp[63:0]}
    : `VAU2_FN1(`VAU2_CSTD) ? result_float2float_dp
    : `SZ_DATA'bx;

  wire `DEF_EXC next_exc_sp
    = `VAU2_FN4(`VAU2_CLTF,`VAU2_CLUTF,`VAU2_CWTF,`VAU2_CWUTF) ? exc_int2float_sp
    : `VAU2_FN4(`VAU2_CFTL,`VAU2_CFTLU,`VAU2_CFTW,`VAU2_CFTWU) ? exc_float2int_sp
    : `VAU2_FN1(`VAU2_CDTS) ? exc_float2float_sp
    : `SZ_EXC'bx;

  wire `DEF_EXC next_exc_dp
    = `VAU2_FN4(`VAU2_CLTF,`VAU2_CLUTF,`VAU2_CWTF,`VAU2_CWUTF) ? exc_int2float_dp
    : `VAU2_FN4(`VAU2_CFTL,`VAU2_CFTLU,`VAU2_CFTW,`VAU2_CFTWU) ? exc_float2int_dp
    : `VAU2_FN1(`VAU2_CSTD) ? exc_float2float_dp
    : `SZ_EXC'bx;

  // pipeline registers

  reg `DEF_DATA pipereg_result [`FCONV_STAGES-1:0];
  reg `DEF_EXC pipereg_exc [`FCONV_STAGES-1:0];

  integer i;

  always @(posedge clk)
  begin
    if (val)
    begin
      pipereg_result[0] <= `VAU2_FP(`FPD) ? next_result_dp : next_result_sp;
      pipereg_exc[0] <= `VAU2_FP(`FPD) ? next_exc_dp : next_exc_sp;
    end
    for (i=1; i<`FCONV_STAGES; i=i+1)
    begin
      pipereg_result[i] <= pipereg_result[i-1];
      pipereg_exc[i] <= pipereg_result[i-1];
    end
  end

  assign out = pipereg_result[`FCONV_STAGES-1];
  assign exc = pipereg_exc[`FCONV_STAGES-1];

endmodule

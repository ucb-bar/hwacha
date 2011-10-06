`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Banked8_FU_fma
(
  input clk,
  input reset,
  input val,

  input `DEF_VAU1_FN fn,
  input `DEF_DATA in0,
  input `DEF_DATA in1,
  input `DEF_DATA in2,
  output `DEF_DATA out,
  output `DEF_EXC exc
);

  // use in0 & in2 for a two operand flop (add,sub,mul)
  // use in0, in1, & in2 otherwise

  wire [1:0] fma_op
    = fn[`RG_VAU1_FN] == `VAU1_SUB || fn[`RG_VAU1_FN] == `VAU1_MSUB ? 2'b01
    : fn[`RG_VAU1_FN] == `VAU1_NMSUB ? 2'b10
    : fn[`RG_VAU1_FN] == `VAU1_NMADD ? 2'b11
    : 2'b00;

  wire [64:0] one_dp = 65'h8000000000000000;
  wire [64:0] one_sp = 65'h80000000;
  wire [64:0] fma_multiplicand = in0;
  wire [64:0] fma_multiplier
    = (fn[`RG_VAU1_FP] == 1'b1) && (fn[`RG_VAU1_FN] == `VAU1_ADD || fn[`RG_VAU1_FN] == `VAU1_SUB) ? one_dp
    : (fn[`RG_VAU1_FP] == 1'b0) && (fn[`RG_VAU1_FN] == `VAU1_ADD || fn[`RG_VAU1_FN] == `VAU1_SUB) ? one_sp
    : (fn[`RG_VAU1_FN] == `VAU1_MUL) ? in2
    : in1;
  wire [64:0] fma_addend
    = fn[`RG_VAU1_FN] == `VAU1_MUL ? 65'd0
    : in2;

  wire [69:0] result_dp;
  wire [37:0] result_sp;

  wire val_fma_dp = val & (fn[`RG_VAU1_FP] == 1'b1);
  wire val_fma_sp = val & (fn[`RG_VAU1_FP] == 1'b0);

  mulAddSubRecodedFloatN #(11, 53) fma_dp
  (
    .op({2{val_fma_dp}} & fma_op),
    .a({65{val_fma_dp}} & fma_multiplicand),
    .b({65{val_fma_dp}} & fma_multiplier),
    .c({65{val_fma_dp}} & fma_addend),
    .roundingMode({2{val_fma_dp}} & fn[`RG_VAU1_RM]),
    .out(result_dp[64:0]),
    .exceptionFlags(result_dp[69:65])
  );

  mulAddSubRecodedFloatN #(8, 24) fma_sp
  (
    .op({2{val_fma_sp}} & fma_op),
    .a({33{val_fma_sp}} & fma_multiplicand[32:0]),
    .b({33{val_fma_sp}} & fma_multiplier[32:0]),
    .c({33{val_fma_sp}} & fma_addend[32:0]),
    .roundingMode({2{val_fma_sp}} & fn[`RG_VAU1_RM]),
    .out(result_sp[32:0]),
    .exceptionFlags(result_sp[37:33])
  );

  reg [69:0] pipereg [`FMA_STAGES-1:0];

  integer i;

  always @(posedge clk) begin : foo
    if (val)
      pipereg[0] <= fn[`RG_VAU1_FP] ? result_dp : {result_sp[37:33], 32'hFFFF_FFFF, result_sp[32:0]};
    for(i=1; i<`FMA_STAGES; i=i+1)
      pipereg[i] <= pipereg[i-1];
  end

  assign {exc,out} = pipereg[`FMA_STAGES-1];

endmodule

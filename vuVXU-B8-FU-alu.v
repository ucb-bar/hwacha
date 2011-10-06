`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module shifter
(
  input `DEF_VIU_FN fn,
  input [5:0] shamt,
  input [63:0] in,
  output [63:0] out
);

  wire left
    = fn[`RG_VIU_FN] == `VIU_SLL ? 1'b1
    : fn[`RG_VIU_FN] == `VIU_SRL ? 1'b0
    : fn[`RG_VIU_FN] == `VIU_SRA ? 1'b0
    : 1'bx;

  wire arith
    = fn[`RG_VIU_FN] == `VIU_SLL ? 1'b0
    : fn[`RG_VIU_FN] == `VIU_SRL ? 1'b0
    : fn[`RG_VIU_FN] == `VIU_SRA ? 1'b1
    : 1'bx;

  wire trunc
    = fn[`RG_VIU_FN] == `VIU_SRL && fn[`RG_VIU_DW] == `DW32 ? 1'b1
    : fn[`RG_VIU_FN] == `VIU_SRL && fn[`RG_VIU_DW] == `DW64 ? 1'b0
    : fn[`RG_VIU_FN] == `VIU_SRA ? 1'b0
    : 1'bx;

  wire tmp;
  wire [63:0] in_reversed, shift_out_reversed, shift_out;
  generate
    genvar i;
    for(i = 0; i < 64; i=i+1)
    begin : reverse
      assign in_reversed[i] = in[63-i];
      assign shift_out_reversed[i] = shift_out[63-i];
    end
  endgenerate

  wire [63:0] shift_in = left ? in_reversed : {{32{~trunc}}&in[63:32],in[31:0]};
  assign {tmp, shift_out} = $signed({arith & shift_in[63], shift_in}) >>> shamt;
  assign out = left ? shift_out_reversed : shift_out;

endmodule

module vuVXU_Banked8_FU_alu
(
  input clk,
  input reset,

  input val,
  input wen,
  output wen_masked,
  input `DEF_VIU_FN fn,
  input `DEF_VLEN utidx,
  input `DEF_DATA in0,
  input `DEF_DATA in1,
  output `DEF_DATA out
);

  `define VIU_FN1(fn0_) (reg_fn[`RG_VIU_FN] == fn0_)
  `define VIU_FN2(fn0_, fn1_) ((reg_fn[`RG_VIU_FN] == fn0_) || (reg_fn[`RG_VIU_FN] == fn1_))
  `define VIU_FN3(fn0_, fn1_, fn2_) ((reg_fn[`RG_VIU_FN] == fn0_) || (reg_fn[`RG_VIU_FN] == fn1_) || (reg_fn[`RG_VIU_FN] == fn2_))
  `define VIU_FN4(fn0_, fn1_, fn2_, fn3_) ((reg_fn[`RG_VIU_FN] == fn0_) || (reg_fn[`RG_VIU_FN] == fn1_) || (reg_fn[`RG_VIU_FN] == fn2_) || (reg_fn[`RG_VIU_FN] == fn3_))
  `define VIU_FN5(fn0_, fn1_, fn2_, fn3_, fn4_) ((reg_fn[`RG_VIU_FN] == fn0_) || (reg_fn[`RG_VIU_FN] == fn1_) || (reg_fn[`RG_VIU_FN] == fn2_) || (reg_fn[`RG_VIU_FN] == fn3_) || (reg_fn[`RG_VIU_FN] == fn4_))
  `define VIU_FP(fp_) (reg_fn[`RG_VIU_FP] == fp_)
  `define VIU_DW(dw_) (reg_fn[`RG_VIU_DW] == dw_)

  reg `DEF_VIU_FN reg_fn;
  reg `DEF_VLEN   reg_utidx;
  reg `DEF_DATA   reg_in0;
  reg `DEF_DATA   reg_in1;
  reg             reg_mask;
  reg `DEF_DATA   reg_result;

  wire sub
    = `VIU_FN1(`VIU_ADD) ? 1'b0
    : `VIU_FN1(`VIU_SUB) ? 1'b1
    : 1'bx;

  wire [63:0] adder_out;
  wire [63:0] shift_out;
  wire tmp;

  assign {adder_out, tmp} = {reg_in0[63:0], sub} + {reg_in1[63:0] ^ {64{sub}}, sub};

  wire [5:0] shamt
    = `VIU_DW(`DW64) ? reg_in1[5:0]
    : `VIU_DW(`DW32) ? {1'b0, reg_in1[4:0]}
    : 6'bx;

  shifter shifter
  (
    .fn(reg_fn),
    .shamt(shamt),
    .in(reg_in0[63:0]),
    .out(shift_out)
  );

  wire ltu = (reg_in0 < reg_in1);
  wire lt = (reg_in0[63] == reg_in1[63]) & ltu | reg_in0[63] & ~reg_in1[63];

  wire less_sp;
  wire equal_sp;
  wire unordered_sp;

  wire less_dp;
  wire equal_dp;
  wire unordered_dp;

  compareRecodedFloatN#(8,24) comp_sp
  (
    .a(reg_in0[32:0]),
    .b(reg_in1[32:0]),
    .less(less_sp),
    .equal(equal_sp),
    .unordered(unordered_sp),
    .exceptionFlags()
  );

  compareRecodedFloatN#(11,53) comp_dp
  (
    .a(reg_in0),
    .b(reg_in1),
    .less(less_dp),
    .equal(equal_dp),
    .unordered(unordered_dp),
    .exceptionFlags()
  );

  wire comp
    = `VIU_FN1(`VIU_SLT) & lt
    | `VIU_FN1(`VIU_SLTU) & ltu
    | `VIU_FN1(`VIU_FEQ) & (`VIU_FP(`FPS) & equal_sp | `VIU_FP(`FPD) & equal_dp)
    | `VIU_FN1(`VIU_FLE) & (`VIU_FP(`FPS) & (equal_sp | less_sp) | `VIU_FP(`FPD) & (equal_dp | less_dp))
    | `VIU_FN1(`VIU_FLT) & (`VIU_FP(`FPS) & less_sp | `VIU_FP(`FPD) & less_dp);

  wire sj_sp
    = `VIU_FN1(`VIU_FSJ) & reg_in1[32]
    | `VIU_FN1(`VIU_FSJN) & ~reg_in1[32]
    | `VIU_FN1(`VIU_FSJX) & (reg_in1[32] ^ reg_in0[32]);

  wire sj_dp
    = `VIU_FN1(`VIU_FSJ) & reg_in1[64]
    | `VIU_FN1(`VIU_FSJN) & ~reg_in1[64]
    | `VIU_FN1(`VIU_FSJX) & (reg_in1[64] ^ reg_in0[64]);

  wire is_in0_nan
    = `VIU_FP(`FPS) & (reg_in0[31:29] == 3'b111)
    | `VIU_FP(`FPD) & (reg_in0[63:61] == 3'b111);

  wire is_in1_nan
    = `VIU_FP(`FPS) & (reg_in1[31:29] == 3'b111)
    | `VIU_FP(`FPD) & (reg_in1[63:61] == 3'b111);

  wire want_min
    = `VIU_FN1(`VIU_FMIN) ? 1'b1
    : `VIU_FN1(`VIU_FMAX) ? 1'b0
    : 1'bx;

  wire less_fp
    = `VIU_FP(`FPS) & less_sp
    | `VIU_FP(`FPD) & less_dp;

  wire `DEF_DATA fminmax
    = is_in1_nan || ~is_in0_nan && (want_min == less_fp) ? reg_in0
    : reg_in1;

  wire next_mask
    = `VIU_FN1(`VIU_MOVZ) ? ~reg_in0[0]
    : `VIU_FN1(`VIU_MOVN) ? reg_in0[0]
    : 1'b1;

  wire `DEF_DATA next_result64
    = `VIU_FN1(`VIU_IDX) ? {{`SZ_DATA-`SZ_VLEN{1'b0}}, reg_utidx}
    : `VIU_FN3(`VIU_MOV,`VIU_MOVZ,`VIU_MOVN) ? reg_in1
    : `VIU_FN2(`VIU_ADD,`VIU_SUB) ? {1'b0, adder_out}
    : `VIU_FN3(`VIU_SLL,`VIU_SRL,`VIU_SRA) ? {1'b0, shift_out}
    : `VIU_FN5(`VIU_SLT,`VIU_SLTU,`VIU_FEQ,`VIU_FLE,`VIU_FLT) ? {64'd0, comp}
    : `VIU_FN1(`VIU_AND) ? reg_in0 & reg_in1
    : `VIU_FN1(`VIU_OR) ? reg_in0 | reg_in1
    : `VIU_FN1(`VIU_XOR) ? reg_in0 ^ reg_in1
    : `VIU_FN3(`VIU_FSJ,`VIU_FSJN,`VIU_FSJX) && `VIU_FP(`FPS) ? {32'hFFFF_FFFF, sj_sp, reg_in0[31:0]}
    : `VIU_FN3(`VIU_FSJ,`VIU_FSJN,`VIU_FSJX) && `VIU_FP(`FPD) ? {sj_dp, reg_in0[63:0]}
    : `VIU_FN2(`VIU_FMIN,`VIU_FMAX) ? fminmax
    : `SZ_DATA'bx;

  wire `DEF_DATA next_result
    = `VIU_DW(`DW64) ? next_result64
    : `VIU_DW(`DW32) ? {1'b0, {32{next_result64[31]}}, next_result64[31:0]}
    : `SZ_DATA'bx;

  always @(posedge clk)
  begin
    reg_fn <= fn;
    reg_utidx <= utidx;
    reg_in0 <= in0;
    reg_in1 <= in1;
    reg_mask <= next_mask;
    reg_result <= next_result;
  end

  assign wen_masked = wen & reg_mask;
  assign out = reg_result;

endmodule

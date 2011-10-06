`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Banked8_FU_imul
(
  input clk,
  input reset,
  input val,
  input `DEF_VAU0_FN fn,
  input `DEF_DATA in0,
  input `DEF_DATA in1,
  output `DEF_DATA out
);

  wire sxl64 = fn == `VAU0_64H  | fn == `VAU0_64HSU;
  wire sxr64 = fn == `VAU0_64H;
  wire zxl32 = fn == `VAU0_32HU;
  wire zxr32 = fn == `VAU0_32HU | fn == `VAU0_32HSU;
  wire sxl32 = fn == `VAU0_32H | fn == `VAU0_32HSU;
  wire sxr32 = fn == `VAU0_32H;

  wire [64:0] lhs = {in0[63] & sxl64,
                     {32{~zxl32}}&in0[63:32] | {32{sxl32&in0[31]}},
                     in0[31:0]};
  wire [64:0] rhs = {in1[63] & sxr64,
                     {32{~zxr32}}&in1[63:32] | {32{sxr32&in1[31]}},
                     in1[31:0]};

  wire [129:0] mul_result = $signed(lhs)*$signed(rhs);
  reg [63:0] reg_result [0:`IMUL_STAGES-1];

  wire [63:0] mul_output_mux
    = fn == `VAU0_64    ? mul_result[63:0]
    : fn == `VAU0_64H   ? mul_result[127:64]
    : fn == `VAU0_64HU  ? mul_result[127:64]
    : fn == `VAU0_64HSU ? mul_result[127:64]
    : fn == `VAU0_32    ? {{32{mul_result[31]}}, mul_result[31:0]}
    : fn == `VAU0_32H   ? {{32{mul_result[63]}}, mul_result[63:32]}
    : fn == `VAU0_32HU  ? {{32{mul_result[63]}}, mul_result[63:32]}
    : fn == `VAU0_32HSU ? {{32{mul_result[63]}}, mul_result[63:32]}
    : 64'bx;

  integer i;

  always @(posedge clk)
  begin: pipe_regs
    if (val)
      reg_result[0] <= mul_output_mux;
    for (i=1;i<`IMUL_STAGES;i=i+1)
      reg_result[i] <= reg_result[i-1];
  end

  assign out = reg_result[`IMUL_STAGES-1];

endmodule

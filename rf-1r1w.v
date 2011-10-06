`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module rf_1r1w
#(
  parameter WIDTH=65,
  parameter LG_DEPTH=8
)
(
  input clk,
  input ren,
  input [LG_DEPTH-1:0] raddr,
  output [WIDTH-1:0] rdata,
  input wen,
  input [LG_DEPTH-1:0] waddr,
  input [WIDTH-1:0] wdata
);

  sram_1r1w #(WIDTH, LG_DEPTH, WIDTH) sram
  (
    .CE1(clk),
    .OEB1(1'b0),
    .CSB1(~ren),
    .A1(raddr),
    .O1(rdata),
    .CE2(clk),
    .WEB2(~wen),
    .CSB2(1'b0),
    .A2(waddr),
    .BM2(1'b1),
    .I2(wdata)
  );

endmodule

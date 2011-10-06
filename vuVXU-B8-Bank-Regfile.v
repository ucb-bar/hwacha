`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Banked8_Bank_Regfile
(
  input clk,
  input reset,

  input              ren,
  input `DEF_BREGLEN raddr,
  input `DEF_BOPL    roplen,

  input              wen,
  input `DEF_BREGLEN waddr,
  input `DEF_BWPORT  wsel,

  output `DEF_DATA rdata,
  output reg `DEF_DATA ropl0,
  output reg `DEF_DATA ropl1,

  input `DEF_DATA wbl0,
  input `DEF_DATA wbl1,
  input `DEF_DATA wbl2,
  input `DEF_DATA wbl3,

  output `DEF_DATA viu_rdata,
  output `DEF_DATA viu_ropl,
  input  `DEF_DATA viu_wdata
);

  wire `DEF_DATA rdata_rf;
  wire `DEF_DATA wdata;

  assign wdata
    = wsel == `SZ_BWPORT'd0 ? wbl0
    : wsel == `SZ_BWPORT'd1 ? wbl1
    : wsel == `SZ_BWPORT'd2 ? wbl2
    : wsel == `SZ_BWPORT'd3 ? wbl3
    : wsel == `SZ_BWPORT'd4 ? viu_wdata
    : `SZ_DATA'd0;

`ifdef ASIC
  `ifndef ASIC_SMALL
  rf_1r1w#(65,6) rf
  (
    .clk(clk),
    .ren(ren),
    .raddr(raddr[5:0]),
    .rdata(rdata_rf),
    .wen(wen),
    .waddr(waddr[5:0]),
    .wdata(wdata)
  );
  `else
  SRAM8T_65x256 rf
  (
    .clk(clk),
    .read(ren),
    .raddr(raddr),
    .dout(rdata_rf),
    .write(wen),
    .waddr(waddr),
    .din(wdata)
  );
  `endif
`else
  rf_1r1w rf
  (
    .clk(clk),
    .ren(ren),
    .raddr(raddr),
    .rdata(rdata_rf),
    .wen(wen),
    .waddr(waddr),
    .wdata(wdata)
  );
`endif

  assign rdata = rdata_rf;

  always @(posedge clk)
  begin
    if (roplen[0])
      ropl0 <= rdata_rf;
    if (roplen[1])
      ropl1 <= rdata_rf;
  end

  assign viu_rdata = rdata_rf;
  assign viu_ropl = ropl0;

endmodule

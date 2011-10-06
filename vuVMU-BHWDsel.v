`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

// byte/halfword/word/doubleword select + sign extension logic

module vuVMU_BHWD_sel
(
  input  [1:0]   bhwd_sel,
  input          signext,
  input  [3:0]   addr_lsb,
  input  [127:0] din,
  output [63:0]  dout
);

  wire [7:0]    byte_sel;
  wire [63:0]   byte_sel_ext;

  wire [15:0]   hw_sel;
  wire [63:0]   hw_sel_ext;
  
  wire [31:0]   word_sel;
  wire [63:0]   word_sel_ext;
  
  wire [63:0]   dword_sel;
  
  assign dword_sel    = addr_lsb[3] ? din[127:64] : din[63:0];

  assign word_sel     = addr_lsb[2] ? dword_sel[63:32] : dword_sel[31:0];
  assign word_sel_ext = signext ? {32'd0, word_sel} :  {{32{word_sel[31]}}, word_sel};
  
  assign hw_sel       = addr_lsb[1] ? word_sel[31:16] : word_sel[15:0];
  assign hw_sel_ext   = signext ? {48'd0, hw_sel} : {{48{hw_sel[15]}}, hw_sel};

  assign byte_sel     = addr_lsb[0] ? hw_sel[15:8] : hw_sel[7:0];
  assign byte_sel_ext = signext ? {56'd0, byte_sel} :  {{56{byte_sel[7]}}, byte_sel};

  assign dout        = (bhwd_sel == 2'b11)  ? dword_sel : 
                       (bhwd_sel == 2'b10)  ? word_sel :
                       (bhwd_sel == 2'b01)  ? hw_sel_ext :
                       (bhwd_sel == 2'b00)  ? byte_sel_ext : 64'dx;


endmodule

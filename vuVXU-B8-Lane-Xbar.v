`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Banked8_Lane_Xbar_Mux8
(
  input  `DEF_BRPORT rblen [0:7],
  input  integer     port,
  input  `DEF_DATA   rdata [0:7],
  output `DEF_DATA   rbl
);

  assign rbl
    = {`SZ_DATA{rblen[0][port]}} & rdata[0]
    | {`SZ_DATA{rblen[1][port]}} & rdata[1]
    | {`SZ_DATA{rblen[2][port]}} & rdata[2]
    | {`SZ_DATA{rblen[3][port]}} & rdata[3]
    | {`SZ_DATA{rblen[4][port]}} & rdata[4]
    | {`SZ_DATA{rblen[5][port]}} & rdata[5]
    | {`SZ_DATA{rblen[6][port]}} & rdata[6]
    | {`SZ_DATA{rblen[7][port]}} & rdata[7];

endmodule

module vuVXU_Banked8_Lane_Xbar
(
  input `DEF_BRPORT rblen [0:7],
  input `DEF_DATA   rdata [0:7],
  input `DEF_DATA   ropl0 [0:7],
  input `DEF_DATA   ropl1 [0:7],

  output `DEF_DATA rbl0,
  output `DEF_DATA rbl1,
  output `DEF_DATA rbl2,
  output `DEF_DATA rbl3,
  output `DEF_DATA rbl4,
  output `DEF_DATA rbl5,
  output `DEF_DATA rbl6,
  output `DEF_DATA rbl7
);

  vuVXU_Banked8_Lane_Xbar_Mux8 mux0 (rblen, 0, ropl0, rbl0);
  vuVXU_Banked8_Lane_Xbar_Mux8 mux1 (rblen, 1, rdata, rbl1);
  vuVXU_Banked8_Lane_Xbar_Mux8 mux2 (rblen, 2, ropl1, rbl2);
  vuVXU_Banked8_Lane_Xbar_Mux8 mux3 (rblen, 3, ropl0, rbl3);
  vuVXU_Banked8_Lane_Xbar_Mux8 mux4 (rblen, 4, rdata, rbl4);
  vuVXU_Banked8_Lane_Xbar_Mux8 mux5 (rblen, 5, rdata, rbl5);
  vuVXU_Banked8_Lane_Xbar_Mux8 mux6 (rblen, 6, rdata, rbl6);
  vuVXU_Banked8_Lane_Xbar_Mux8 mux7 (rblen, 7, rdata, rbl7);

endmodule

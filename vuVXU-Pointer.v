`include "vuVXU-B8-Config.vh"

module vuVXU_Pointer
(
  input      `DEF_BPTR  ptr,
  input      `DEF_BPTR1 incr,
  input      `DEF_BCNT  bcnt,
  output reg `DEF_BPTR  nptr
);

  wire `DEF_BPTR2 add = ptr + incr;

  // the following lookup table should implement:
  // wire `DEF_BPTR2 mod = add % bcnt;
  // assign nptr = mod[`SZ_LGBANK-1:0];

  // maximum: max bptr + max pipestage + max read port + 2
  //          7 + 5 + 3 + 2 = 17

  always @(*)
  begin
    case ({add, bcnt})
      {5'd0,4'd3}: nptr = 3'd0;
      {5'd0,4'd4}: nptr = 3'd0;
      {5'd0,4'd5}: nptr = 3'd0;
      {5'd0,4'd6}: nptr = 3'd0;
      {5'd0,4'd7}: nptr = 3'd0;
      {5'd0,4'd8}: nptr = 3'd0;
      {5'd1,4'd3}: nptr = 3'd1;
      {5'd1,4'd4}: nptr = 3'd1;
      {5'd1,4'd5}: nptr = 3'd1;
      {5'd1,4'd6}: nptr = 3'd1;
      {5'd1,4'd7}: nptr = 3'd1;
      {5'd1,4'd8}: nptr = 3'd1;
      {5'd2,4'd3}: nptr = 3'd2;
      {5'd2,4'd4}: nptr = 3'd2;
      {5'd2,4'd5}: nptr = 3'd2;
      {5'd2,4'd6}: nptr = 3'd2;
      {5'd2,4'd7}: nptr = 3'd2;
      {5'd2,4'd8}: nptr = 3'd2;
      {5'd3,4'd3}: nptr = 3'd0;
      {5'd3,4'd4}: nptr = 3'd3;
      {5'd3,4'd5}: nptr = 3'd3;
      {5'd3,4'd6}: nptr = 3'd3;
      {5'd3,4'd7}: nptr = 3'd3;
      {5'd3,4'd8}: nptr = 3'd3;
      {5'd4,4'd3}: nptr = 3'd1;
      {5'd4,4'd4}: nptr = 3'd0;
      {5'd4,4'd5}: nptr = 3'd4;
      {5'd4,4'd6}: nptr = 3'd4;
      {5'd4,4'd7}: nptr = 3'd4;
      {5'd4,4'd8}: nptr = 3'd4;
      {5'd5,4'd3}: nptr = 3'd2;
      {5'd5,4'd4}: nptr = 3'd1;
      {5'd5,4'd5}: nptr = 3'd0;
      {5'd5,4'd6}: nptr = 3'd5;
      {5'd5,4'd7}: nptr = 3'd5;
      {5'd5,4'd8}: nptr = 3'd5;
      {5'd6,4'd3}: nptr = 3'd0;
      {5'd6,4'd4}: nptr = 3'd2;
      {5'd6,4'd5}: nptr = 3'd1;
      {5'd6,4'd6}: nptr = 3'd0;
      {5'd6,4'd7}: nptr = 3'd6;
      {5'd6,4'd8}: nptr = 3'd6;
      {5'd7,4'd3}: nptr = 3'd1;
      {5'd7,4'd4}: nptr = 3'd3;
      {5'd7,4'd5}: nptr = 3'd2;
      {5'd7,4'd6}: nptr = 3'd1;
      {5'd7,4'd7}: nptr = 3'd0;
      {5'd7,4'd8}: nptr = 3'd7;
      {5'd8,4'd3}: nptr = 3'd2;
      {5'd8,4'd4}: nptr = 3'd0;
      {5'd8,4'd5}: nptr = 3'd3;
      {5'd8,4'd6}: nptr = 3'd2;
      {5'd8,4'd7}: nptr = 3'd1;
      {5'd8,4'd8}: nptr = 3'd0;
      {5'd9,4'd3}: nptr = 3'd0;
      {5'd9,4'd4}: nptr = 3'd1;
      {5'd9,4'd5}: nptr = 3'd4;
      {5'd9,4'd6}: nptr = 3'd3;
      {5'd9,4'd7}: nptr = 3'd2;
      {5'd9,4'd8}: nptr = 3'd1;
      {5'd10,4'd3}: nptr = 3'd1;
      {5'd10,4'd4}: nptr = 3'd2;
      {5'd10,4'd5}: nptr = 3'd0;
      {5'd10,4'd6}: nptr = 3'd4;
      {5'd10,4'd7}: nptr = 3'd3;
      {5'd10,4'd8}: nptr = 3'd2;
      {5'd11,4'd3}: nptr = 3'd2;
      {5'd11,4'd4}: nptr = 3'd3;
      {5'd11,4'd5}: nptr = 3'd1;
      {5'd11,4'd6}: nptr = 3'd5;
      {5'd11,4'd7}: nptr = 3'd4;
      {5'd11,4'd8}: nptr = 3'd3;
      {5'd12,4'd3}: nptr = 3'd0;
      {5'd12,4'd4}: nptr = 3'd0;
      {5'd12,4'd5}: nptr = 3'd2;
      {5'd12,4'd6}: nptr = 3'd0;
      {5'd12,4'd7}: nptr = 3'd5;
      {5'd12,4'd8}: nptr = 3'd4;
      {5'd13,4'd3}: nptr = 3'd1;
      {5'd13,4'd4}: nptr = 3'd1;
      {5'd13,4'd5}: nptr = 3'd3;
      {5'd13,4'd6}: nptr = 3'd1;
      {5'd13,4'd7}: nptr = 3'd6;
      {5'd13,4'd8}: nptr = 3'd5;
      {5'd14,4'd3}: nptr = 3'd2;
      {5'd14,4'd4}: nptr = 3'd2;
      {5'd14,4'd5}: nptr = 3'd4;
      {5'd14,4'd6}: nptr = 3'd2;
      {5'd14,4'd7}: nptr = 3'd0;
      {5'd14,4'd8}: nptr = 3'd6;
      {5'd15,4'd3}: nptr = 3'd0;
      {5'd15,4'd4}: nptr = 3'd3;
      {5'd15,4'd5}: nptr = 3'd0;
      {5'd15,4'd6}: nptr = 3'd3;
      {5'd15,4'd7}: nptr = 3'd1;
      {5'd15,4'd8}: nptr = 3'd7;
      {5'd16,4'd3}: nptr = 3'd1;
      {5'd16,4'd4}: nptr = 3'd0;
      {5'd16,4'd5}: nptr = 3'd1;
      {5'd16,4'd6}: nptr = 3'd4;
      {5'd16,4'd7}: nptr = 3'd2;
      {5'd16,4'd8}: nptr = 3'd0;
      {5'd17,4'd3}: nptr = 3'd2;
      {5'd17,4'd4}: nptr = 3'd1;
      {5'd17,4'd5}: nptr = 3'd2;
      {5'd17,4'd6}: nptr = 3'd5;
      {5'd17,4'd7}: nptr = 3'd3;
      {5'd17,4'd8}: nptr = 3'd1;
      //{5'd18,4'd3}: nptr = 3'd0;
      //{5'd18,4'd4}: nptr = 3'd2;
      //{5'd18,4'd5}: nptr = 3'd3;
      //{5'd18,4'd6}: nptr = 3'd0;
      //{5'd18,4'd7}: nptr = 3'd4;
      //{5'd18,4'd8}: nptr = 3'd2;
      //{5'd19,4'd3}: nptr = 3'd1;
      //{5'd19,4'd4}: nptr = 3'd3;
      //{5'd19,4'd5}: nptr = 3'd4;
      //{5'd19,4'd6}: nptr = 3'd1;
      //{5'd19,4'd7}: nptr = 3'd5;
      //{5'd19,4'd8}: nptr = 3'd3;
      //{5'd20,4'd3}: nptr = 3'd2;
      //{5'd20,4'd4}: nptr = 3'd0;
      //{5'd20,4'd5}: nptr = 3'd0;
      //{5'd20,4'd6}: nptr = 3'd2;
      //{5'd20,4'd7}: nptr = 3'd6;
      //{5'd20,4'd8}: nptr = 3'd4;
      //{5'd21,4'd3}: nptr = 3'd0;
      //{5'd21,4'd4}: nptr = 3'd1;
      //{5'd21,4'd5}: nptr = 3'd1;
      //{5'd21,4'd6}: nptr = 3'd3;
      //{5'd21,4'd7}: nptr = 3'd0;
      //{5'd21,4'd8}: nptr = 3'd5;
      default: nptr = 3'bx;
    endcase 
  end

endmodule

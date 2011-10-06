`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Banked8_Bank
(
  input clk,
  input reset,

  input active,

  input               in_ren,
  input               in_rlast,
  input  `DEF_BVLEN   in_rcnt,
  input  `DEF_BREGLEN in_raddr,
  input  `DEF_BOPL    in_roplen,
  input  `DEF_BRPORT  in_rblen,

  input               in_wen,
  input               in_wlast,
  input  `DEF_BVLEN   in_wcnt,
  input  `DEF_BREGLEN in_waddr,
  input  `DEF_BWPORT  in_wsel,

  input               in_viu_val,
  input  `DEF_VIU_FN  in_viu_fn,
  input  `DEF_VLEN    in_viu_utidx,
  input  `DEF_DATA    in_viu_imm,

  output              out_ren,
  output              out_rlast,
  output `DEF_BVLEN   out_rcnt,
  output `DEF_BREGLEN out_raddr,
  output `DEF_BOPL    out_roplen,
  output `DEF_BRPORT  out_rblen,

  output              out_wen,
  output              out_wlast,
  output `DEF_BVLEN   out_wcnt,
  output `DEF_BREGLEN out_waddr,
  output `DEF_BWPORT  out_wsel,

  output              out_viu_val,
  output `DEF_VIU_FN  out_viu_fn,
  output `DEF_VLEN    out_viu_utidx,
  output `DEF_DATA    out_viu_imm,

  output `DEF_BRPORT rblen,
  output `DEF_DATA   rdata,
  output `DEF_DATA   ropl0,
  output `DEF_DATA   ropl1,

  input `DEF_DATA wbl0,
  input `DEF_DATA wbl1,
  input `DEF_DATA wbl2,
  input `DEF_DATA wbl3
);

  reg              reg_ren;
  reg              reg_rlast;
  reg `DEF_BVLEN   reg_rcnt;
  reg `DEF_BREGLEN reg_raddr;
  reg `DEF_BOPL    reg_roplen;
  reg `DEF_BRPORT  reg_rblen;

  reg              reg_wen;
  reg              reg_wlast;
  reg `DEF_BVLEN   reg_wcnt;
  reg `DEF_BREGLEN reg_waddr;
  reg `DEF_BWPORT  reg_wsel;

  reg              reg_viu_val;
  reg `DEF_VIU_FN  reg_viu_fn;
  reg `DEF_VLEN    reg_viu_utidx;
  reg `DEF_DATA    reg_viu_imm;

  // every signal related to the read port is delayed by one cycle 
  // because of the register file is an sram

  wire `DEF_BOPL   delay_roplen = reg_roplen & {`SZ_BOPL{active}};
  wire `DEF_BRPORT delay_rblen = reg_rblen & {`SZ_BRPORT{active}};

  reg              delay_viu_val;
  wire `DEF_VIU_FN delay_viu_fn = reg_viu_fn;
  reg  `DEF_VLEN   delay_viu_utidx;
  wire `DEF_DATA   delay_viu_imm = reg_viu_imm;

  wire in_wen_masked;

  always @(posedge clk)
  begin
    delay_viu_val <= in_viu_val & active;
    delay_viu_utidx <= in_viu_utidx;
  end

  wire `DEF_DATA viu_rdata;
  wire `DEF_DATA viu_ropl;
  wire `DEF_DATA viu_wdata;

  assign rblen = delay_rblen;

  vuVXU_Banked8_Bank_Regfile rfile
  (
    .clk(clk),
    .reset(reset),

    .ren(in_ren & active),
    .raddr(in_raddr),
    .roplen(delay_roplen),

    .wen(in_wen_masked & active),
    .waddr(in_waddr),
    .wsel(in_wsel),

    .rdata(rdata),
    .ropl0(ropl0),
    .ropl1(ropl1),

    .wbl0(wbl0),
    .wbl1(wbl1),
    .wbl2(wbl2),
    .wbl3(wbl3),

    .viu_rdata(viu_rdata),
    .viu_ropl(viu_ropl),
    .viu_wdata(viu_wdata)
  );

  wire `DEF_DATA viu_in0
    = (delay_viu_fn[`RG_VIU_T0] == `M0) ? `SZ_DATA'd0
    : (delay_viu_fn[`RG_VIU_T0] == `ML) ? viu_ropl
    : (delay_viu_fn[`RG_VIU_T0] == `MR) ? viu_rdata
    : `SZ_DATA'bx;

  wire `DEF_DATA viu_in1
    = (delay_viu_fn[`RG_VIU_T1] == `M0) ? `SZ_DATA'd0
    : (delay_viu_fn[`RG_VIU_T1] == `MR) ? viu_rdata
    : (delay_viu_fn[`RG_VIU_T1] == `MI) ? delay_viu_imm
    : `SZ_DATA'bx;

  vuVXU_Banked8_FU_alu alu
  (
    .clk(clk),
    .reset(reset),

    .val(delay_viu_val),
    .wen(in_wen),
    .wen_masked(in_wen_masked),
    .fn(delay_viu_fn),
    .utidx(delay_viu_utidx),
    .in0(viu_in0),
    .in1(viu_in1),
    .out(viu_wdata)
  );

  wire rpass = (|in_rcnt);
  wire wpass = (|in_wcnt);

  always @(posedge clk)
  begin
    reg_ren <= rpass & in_ren;
    reg_rlast <= in_rlast;
    reg_rcnt <= rpass ? in_rcnt - 1'b1 : `SZ_LGBANK'd0;
    reg_raddr <= in_raddr;
    reg_roplen <= in_roplen;
    reg_rblen <= in_rblen;

    reg_wen <= wpass & in_wen;
    reg_wlast <= in_wlast;
    reg_wcnt <= wpass ? in_wcnt - 1'b1 : `SZ_LGBANK'd0;
    reg_waddr <= in_waddr;
    reg_wsel <= in_wsel;

    reg_viu_val <= rpass & in_viu_val;
    reg_viu_fn <= in_viu_fn;
    reg_viu_utidx <= in_viu_utidx + 1'b1;
    reg_viu_imm <= in_viu_imm;
  end

  assign out_ren = active ? reg_ren : in_ren;
  assign out_rlast = active ? reg_rlast : in_rlast;
  assign out_rcnt = active ? reg_rcnt : in_rcnt;
  assign out_raddr = active ? reg_raddr : in_raddr;
  assign out_roplen = active ? reg_roplen : in_roplen;
  assign out_rblen = active ? reg_rblen : in_rblen;

  assign out_wen = active ? reg_wen : in_wen;
  assign out_wlast = active ? reg_wlast : in_wlast;
  assign out_wcnt = active ? reg_wcnt : in_wcnt;
  assign out_waddr = active ? reg_waddr : in_waddr;
  assign out_wsel = active ? reg_wsel : in_wsel;

  assign out_viu_val = active ? reg_viu_val : in_viu_val;
  assign out_viu_fn = active ? reg_viu_fn : in_viu_fn;
  assign out_viu_utidx = active ? reg_viu_utidx : in_viu_utidx;
  assign out_viu_imm = active ? reg_viu_imm : in_viu_imm;

endmodule

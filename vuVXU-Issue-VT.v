`include "riscvInst.vh"
`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Issue_VT
(
  input clk,
  input reset,

  output illegal,

  output `DEF_ADDR imem_req_addr,
  output           imem_req_val,
  input            imem_req_rdy,
  input  `DEF_INST imem_resp_data,
  input            imem_resp_val,

  output `DEF_VMU_UTCMDQ vmu_utcmdq_bits,
  output                 vmu_utcmdq_val,
  input                  vmu_utcmdq_rdy,

  output `DEF_VMU_UTIMMQ vmu_utimmq_bits,
  output                 vmu_utimmq_val,
  input                  vmu_utimmq_rdy,

  input              vt_active,
  input              vt_fire,
  output             vt_stop,
  input  `DEF_ADDR   vt_pc,

  output issue_val_viu,
  output issue_val_vau0,
  output issue_val_vau1,
  output issue_val_vau2,
  output issue_val_vgslu,
  output issue_val_vglu,
  output issue_val_vgsu,

  input issue_rdy,

  output reg issue_dhazard_vs,
  output reg issue_dhazard_vt,
  output reg issue_dhazard_vr,
  output reg issue_dhazard_vd,

  output reg issue_shazard_vau0,
  output reg issue_shazard_vau1,
  output reg issue_shazard_vau2,
  output reg issue_shazard_vgu,
  output reg issue_shazard_vlu,
  output reg issue_shazard_vsu,

  output reg issue_bhazard_r1w1,
  output reg issue_bhazard_r2w1,
  output reg issue_bhazard_r3w1,
  output reg issue_bhazard_vgslu,
  output reg issue_bhazard_vglu,
  output reg issue_bhazard_vgsu,

  output reg `DEF_VIU_FN  issue_fn_viu,
  output reg `DEF_VAU0_FN issue_fn_vau0,
  output reg `DEF_VAU1_FN issue_fn_vau1,
  output reg `DEF_VAU2_FN issue_fn_vau2,

  output `DEF_REGLEN issue_vs_decoded,
  output `DEF_REGLEN issue_vt_decoded,
  output `DEF_REGLEN issue_vr_decoded,
  output `DEF_REGLEN issue_vd_decoded,
  output             issue_vd_val,
  output `DEF_DATA   issue_imm
);

  wire stallf;
  wire stalld;
  wire killf;

  assign stallf = ~imem_req_rdy | ~imem_resp_val | stalld;
  assign stalld = ~issue_rdy;
  assign killf = ~imem_resp_val;

  reg `DEF_ADDR if_next_pc;
  reg `DEF_ADDR if_reg_pc;

  always @(posedge clk)
  begin
    if (reset)
      if_reg_pc <= `SZ_ADDR'd0;
    else if  (vt_fire)
      if_reg_pc <= vt_pc;
    else if (!stallf)
      if_reg_pc <= if_next_pc;
  end

  assign if_next_pc = if_reg_pc + `SZ_ADDR'd4;

  assign imem_req_addr = stallf ? if_reg_pc : if_next_pc;
  assign imem_req_val = vt_active;

  reg `DEF_INST id_reg_inst;

  always @(posedge clk)
  begin
    if (reset || vt_fire)
    begin
      id_reg_inst <= `NOP;
    end
    else if (!stalld)
    begin
      if (killf)
        id_reg_inst <= `NOP;
      else
        id_reg_inst <= imem_resp_data;
    end
  end

  reg unmasked_val_viu;
  reg unmasked_val_vau0;
  reg unmasked_val_vau1;
  reg unmasked_val_vau2;
  reg unmasked_val_vgslu;
  reg unmasked_val_vglu;
  reg unmasked_val_vgsu;

  `define ISSUE_VAL unmasked_val_viu,unmasked_val_vau0,unmasked_val_vau1,unmasked_val_vau2,unmasked_val_vgslu,unmasked_val_vglu,unmasked_val_vgsu
  `define ISSUE_DHAZARD issue_dhazard_vs,issue_dhazard_vt,issue_dhazard_vr,issue_dhazard_vd
  `define ISSUE_SHAZARD issue_shazard_vau0,issue_shazard_vau1,issue_shazard_vau2,issue_shazard_vgu,issue_shazard_vlu,issue_shazard_vsu
  `define ISSUE_BHAZARD issue_bhazard_r1w1,issue_bhazard_r2w1,issue_bhazard_r3w1,issue_bhazard_vgslu,issue_bhazard_vglu,issue_bhazard_vgsu
  `define ISSUE_FN issue_fn_viu,issue_fn_vau0,issue_fn_vau1[`RG_VAU1_FP],issue_fn_vau1[`RG_VAU1_FN],issue_fn_vau2[`RG_VAU2_FP],issue_fn_vau2[`RG_VAU2_FN]

  reg rtype_vs;
  reg rtype_vt;
  reg rtype_vr;
  reg rtype_vd;
  reg [1:0] itype;
  reg vd_val;
  reg decode_stop;
  reg enq_vmu_utcmdq;
  reg enq_vmu_utimmq;
  reg [7:0] utcmd;

  `define VTCS {`ISSUE_VAL,`ISSUE_DHAZARD,`ISSUE_SHAZARD,`ISSUE_BHAZARD,`ISSUE_FN,rtype_vs,rtype_vt,rtype_vr,rtype_vd,itype,vd_val,decode_stop,enq_vmu_utcmdq,enq_vmu_utimmq,utcmd}

  localparam y = 1'b1;
  localparam n = 1'b0;

  always @(*)
  begin
    casez (id_reg_inst)
                     //                                                                                                                                                 vd_val
                     //                                                                                                                                                 | decode_stop
                     //                                                                                                                                                 | | enq_vmu_utcmdq
                     //                                                                                                                                                 | | | enq_vmu_utimmq
                     // val          dhazard  shazard    bhazard    viufn                        vau0fn           vau1fn           vau2fn           vs  vt  vr  vd  i   | | | | utcmd
                     // |            |        |          |          |                            |                |                |                |   |   |   |   |   | | | | |
    `LB:       `VTCS = {7'b0_000_010,4'b100_1,6'b000_110,6'b000_010,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,y,y,`CMD_VLXB};
    `LH:       `VTCS = {7'b0_000_010,4'b100_1,6'b000_110,6'b000_010,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,y,y,`CMD_VLXH};
    `LW:       `VTCS = {7'b0_000_010,4'b100_1,6'b000_110,6'b000_010,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,y,y,`CMD_VLXW};
    `LD:       `VTCS = {7'b0_000_010,4'b100_1,6'b000_110,6'b000_010,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,y,y,`CMD_VLXD};
    `LBU:      `VTCS = {7'b0_000_010,4'b100_1,6'b000_110,6'b000_010,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,y,y,`CMD_VLXBU};
    `LHU:      `VTCS = {7'b0_000_010,4'b100_1,6'b000_110,6'b000_010,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,y,y,`CMD_VLXHU};
    `LWU:      `VTCS = {7'b0_000_010,4'b100_1,6'b000_110,6'b000_010,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,y,y,`CMD_VLXWU};
    `SB:       `VTCS = {7'b0_000_001,4'b110_0,6'b000_101,6'b000_001,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`R_,`IB,n,n,y,y,`CMD_VSXB};
    `SH:       `VTCS = {7'b0_000_001,4'b110_0,6'b000_101,6'b000_001,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`R_,`IB,n,n,y,y,`CMD_VSXH};
    `SW:       `VTCS = {7'b0_000_001,4'b110_0,6'b000_101,6'b000_001,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`R_,`IB,n,n,y,y,`CMD_VSXW};
    `SD:       `VTCS = {7'b0_000_001,4'b110_0,6'b000_101,6'b000_001,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`R_,`IB,n,n,y,y,`CMD_VSXD};
    `AMOADD_W: `VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOADDW};
    `AMOSWAP_W:`VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOSWAPW};
    `AMOAND_W: `VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOANDW};
    `AMOOR_W:  `VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOORW};
    `AMOMIN_W: `VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOMINW};
    `AMOMAX_W: `VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOMAXW};
    `AMOMINU_W:`VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOMINUW};
    `AMOMAXU_W:`VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOMAXUW};
    `AMOADD_D: `VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOADDD};
    `AMOSWAP_D:`VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOSWAPD};
    `AMOAND_D: `VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOANDD};
    `AMOOR_D:  `VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOORD};
    `AMOMIN_D: `VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOMIND};
    `AMOMAX_D: `VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOMAXD};
    `AMOMINU_D:`VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOMINUD};
    `AMOMAXU_D:`VTCS = {7'b0_000_100,4'b110_1,6'b000_111,6'b000_100,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,y,n,`CMD_VAMOMAXUD};
    `FLW:      `VTCS = {7'b0_000_010,4'b100_1,6'b000_110,6'b000_010,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RF,`II,y,n,y,y,`CMD_VFLXW};
    `FLD:      `VTCS = {7'b0_000_010,4'b100_1,6'b000_110,6'b000_010,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RF,`II,y,n,y,y,`CMD_VFLXD};
    `FSW:      `VTCS = {7'b0_000_001,4'b110_0,6'b000_101,6'b000_001,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RF,`R_,`R_,`IB,n,n,y,y,`CMD_VFSXW};
    `FSD:      `VTCS = {7'b0_000_001,4'b110_0,6'b000_101,6'b000_001,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RF,`R_,`R_,`IB,n,n,y,y,`CMD_VFSXD};

    `UTIDX:    `VTCS = {7'b1_000_000,4'b000_1,6'b000_000,6'b100_000,`M0,`M0,`DW64,`FP_,`VIU_IDX, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `R_,`R_,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `MOVZ:     `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_MOVZ,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `MOVN:     `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_MOVN,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FMOVZ:    `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_MOVZ,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FMOVN:    `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_MOVN,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};

    `LUI:      `VTCS = {7'b1_000_000,4'b000_1,6'b000_000,6'b100_000,`M0,`MI,`DW64,`FP_,`VIU_MOV, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `R_,`R_,`R_,`RX,`IL,y,n,n,n,`CMD_X};
    `ADDI:     `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW64,`FP_,`VIU_ADD, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};
    `SLLI:     `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW64,`FP_,`VIU_SLL, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};
    `SLTI:     `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW64,`FP_,`VIU_SLT, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};
    `SLTIU:    `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW64,`FP_,`VIU_SLTU,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};
    `XORI:     `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW64,`FP_,`VIU_XOR, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};
    `SRLI:     `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW64,`FP_,`VIU_SRL, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};
    `SRAI:     `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW64,`FP_,`VIU_SRA, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};
    `ORI:      `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW64,`FP_,`VIU_OR,  `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};
    `ANDI:     `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW64,`FP_,`VIU_AND, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};

    `ADD:      `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_ADD, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `SUB:      `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_SUB, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `SLL:      `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_SLL, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `SLT:      `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_SLT, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `SLTU:     `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_SLTU,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `XOR:      `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_XOR, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `SRL:      `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_SRL, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `SRA:      `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_SRA, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `OR:       `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_OR,  `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `AND:      `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FP_,`VIU_AND, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};

    `ADDIW:    `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW32,`FP_,`VIU_ADD, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};
    `SLLIW:    `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW32,`FP_,`VIU_SLL, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};
    `SRLIW:    `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW32,`FP_,`VIU_SRL, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};
    `SRAIW:    `VTCS = {7'b1_000_000,4'b100_1,6'b000_000,6'b100_000,`MR,`MI,`DW32,`FP_,`VIU_SRA, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`R_,`R_,`RX,`II,y,n,n,n,`CMD_X};

    `ADDW:     `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW32,`FP_,`VIU_ADD, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `SUBW:     `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW32,`FP_,`VIU_SUB, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `SLLW:     `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW32,`FP_,`VIU_SLL, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `SRLW:     `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW32,`FP_,`VIU_SRL, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `SRAW:     `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW32,`FP_,`VIU_SRA, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};

    `FSGNJ_S:  `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPS,`VIU_FSJ, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FSGNJN_S: `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPS,`VIU_FSJN,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FSGNJX_S: `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPS,`VIU_FSJX,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FEQ_S:    `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPS,`VIU_FEQ, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FLT_S:    `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPS,`VIU_FLT, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FLE_S:    `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPS,`VIU_FLE, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FMIN_S:   `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPS,`VIU_FMIN,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FMAX_S:   `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPS,`VIU_FMAX,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FSGNJ_D:  `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPD,`VIU_FSJ, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FSGNJN_D: `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPD,`VIU_FSJN,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FSGNJX_D: `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPD,`VIU_FSJX,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FEQ_D:    `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPD,`VIU_FEQ, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FLT_D:    `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPD,`VIU_FLT, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FLE_D:    `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPD,`VIU_FLE, `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FMIN_D:   `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPD,`VIU_FMIN,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FMAX_D:   `VTCS = {7'b1_000_000,4'b110_1,6'b000_000,6'b010_000,`ML,`MR,`DW64,`FPD,`VIU_FMAX,`DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};

    `MUL:      `VTCS = {7'b0_100_000,4'b110_1,6'b100_000,6'b010_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW64,`VAU0_M,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `MULH:     `VTCS = {7'b0_100_000,4'b110_1,6'b100_000,6'b010_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW64,`VAU0_MH,  `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `MULHU:    `VTCS = {7'b0_100_000,4'b110_1,6'b100_000,6'b010_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW64,`VAU0_MHU, `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `MULHSU:   `VTCS = {7'b0_100_000,4'b110_1,6'b100_000,6'b010_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW64,`VAU0_MHSU,`FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `MULW:     `VTCS = {7'b0_100_000,4'b110_1,6'b100_000,6'b010_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW32,`VAU0_M,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `RX,`RX,`R_,`RX,`I_,y,n,n,n,`CMD_X};

    `FADD_S:   `VTCS = {7'b0_010_000,4'b110_1,6'b010_000,6'b010_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPS,`VAU1_ADD,  `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FSUB_S:   `VTCS = {7'b0_010_000,4'b110_1,6'b010_000,6'b010_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPS,`VAU1_SUB,  `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FMUL_S:   `VTCS = {7'b0_010_000,4'b110_1,6'b010_000,6'b010_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPS,`VAU1_MUL,  `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FMADD_S:  `VTCS = {7'b0_010_000,4'b111_1,6'b010_000,6'b001_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPS,`VAU1_MADD, `FP_,`VAU2_X,    `RF,`RF,`RF,`RF,`I_,y,n,n,n,`CMD_X};
    `FMSUB_S:  `VTCS = {7'b0_010_000,4'b111_1,6'b010_000,6'b001_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPS,`VAU1_MSUB, `FP_,`VAU2_X,    `RF,`RF,`RF,`RF,`I_,y,n,n,n,`CMD_X};
    `FNMSUB_S: `VTCS = {7'b0_010_000,4'b111_1,6'b010_000,6'b001_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPS,`VAU1_NMSUB,`FP_,`VAU2_X,    `RF,`RF,`RF,`RF,`I_,y,n,n,n,`CMD_X};
    `FNMADD_S: `VTCS = {7'b0_010_000,4'b111_1,6'b010_000,6'b001_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPS,`VAU1_NMADD,`FP_,`VAU2_X,    `RF,`RF,`RF,`RF,`I_,y,n,n,n,`CMD_X};
    `FADD_D:   `VTCS = {7'b0_010_000,4'b110_1,6'b010_000,6'b010_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPD,`VAU1_ADD,  `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FSUB_D:   `VTCS = {7'b0_010_000,4'b110_1,6'b010_000,6'b010_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPD,`VAU1_SUB,  `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FMUL_D:   `VTCS = {7'b0_010_000,4'b110_1,6'b010_000,6'b010_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPD,`VAU1_MUL,  `FP_,`VAU2_X,    `RF,`RF,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FMADD_D:  `VTCS = {7'b0_010_000,4'b111_1,6'b010_000,6'b001_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPD,`VAU1_MADD, `FP_,`VAU2_X,    `RF,`RF,`RF,`RF,`I_,y,n,n,n,`CMD_X};
    `FMSUB_D:  `VTCS = {7'b0_010_000,4'b111_1,6'b010_000,6'b001_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPD,`VAU1_MSUB, `FP_,`VAU2_X,    `RF,`RF,`RF,`RF,`I_,y,n,n,n,`CMD_X};
    `FNMSUB_D: `VTCS = {7'b0_010_000,4'b111_1,6'b010_000,6'b001_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPD,`VAU1_NMSUB,`FP_,`VAU2_X,    `RF,`RF,`RF,`RF,`I_,y,n,n,n,`CMD_X};
    `FNMADD_D: `VTCS = {7'b0_010_000,4'b111_1,6'b010_000,6'b001_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FPD,`VAU1_NMADD,`FP_,`VAU2_X,    `RF,`RF,`RF,`RF,`I_,y,n,n,n,`CMD_X};

    `FCVT_S_D: `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPS,`VAU2_CDTS, `RF,`R_,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FCVT_D_S: `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPD,`VAU2_CSTD, `RF,`R_,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FCVT_L_S: `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPS,`VAU2_CFTL, `RF,`R_,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FCVT_LU_S:`VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPS,`VAU2_CFTLU,`RF,`R_,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FCVT_W_S: `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPS,`VAU2_CFTW, `RF,`R_,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FCVT_WU_S:`VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPS,`VAU2_CFTWU,`RF,`R_,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FCVT_S_L: `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPS,`VAU2_CLTF, `RX,`R_,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FCVT_S_LU:`VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPS,`VAU2_CLUTF,`RX,`R_,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FCVT_S_W: `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPS,`VAU2_CWTF, `RX,`R_,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FCVT_S_WU:`VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPS,`VAU2_CWUTF,`RX,`R_,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `MXTF_S:   `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPS,`VAU2_MXTF, `RX,`R_,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `MFTX_S:   `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPS,`VAU2_MFTX, `RF,`R_,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FCVT_L_D: `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPD,`VAU2_CFTL, `RF,`R_,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FCVT_LU_D:`VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPD,`VAU2_CFTLU,`RF,`R_,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FCVT_W_D: `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPD,`VAU2_CFTW, `RF,`R_,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FCVT_WU_D:`VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPD,`VAU2_CFTWU,`RF,`R_,`R_,`RX,`I_,y,n,n,n,`CMD_X};
    `FCVT_D_L: `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPD,`VAU2_CLTF, `RX,`R_,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FCVT_D_LU:`VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPD,`VAU2_CLUTF,`RX,`R_,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FCVT_D_W: `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPD,`VAU2_CWTF, `RX,`R_,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `FCVT_D_WU:`VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPD,`VAU2_CWUTF,`RX,`R_,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `MXTF_D:   `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPD,`VAU2_MXTF, `RX,`R_,`R_,`RF,`I_,y,n,n,n,`CMD_X};
    `MFTX_D:   `VTCS = {7'b0_001_000,4'b100_1,6'b001_000,6'b100_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FPD,`VAU2_MFTX, `RF,`R_,`R_,`RX,`I_,y,n,n,n,`CMD_X};

    `STOP:     `VTCS = {7'b0_000_000,4'b000_0,6'b000_000,6'b000_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `R_,`R_,`R_,`R_,`I_,n,y,n,n,`CMD_X};
    default:   `VTCS = {7'b0_000_000,4'b000_0,6'b000_000,6'b000_000,`M0,`M0,`DW__,`FP_,`VIU_X,   `DW__,`VAU0_X,   `FP_,`VAU1_X,    `FP_,`VAU2_X,    `R_,`R_,`R_,`R_,`I_,n,n,n,n,`CMD_X};
    endcase

    if (id_reg_inst[11:9] == 3'b111)
    begin
      issue_fn_vau1[`RG_VAU1_RM] = 2'd0;
      issue_fn_vau2[`RG_VAU2_RM] = 2'd0;
    end
    else
    begin
      issue_fn_vau1[`RG_VAU1_RM] = id_reg_inst[10:9];
      issue_fn_vau2[`RG_VAU2_RM] = id_reg_inst[10:9];
    end
  end

  wire unmasked_val
    = unmasked_val_viu
    | unmasked_val_vau0 | unmasked_val_vau1 | unmasked_val_vau2
    | unmasked_val_vgslu | unmasked_val_vglu | unmasked_val_vgsu;

  assign vt_stop = decode_stop;

  wire mask_issue_rdy = issue_rdy; // always issue for memory ops
  wire mask_vmu_utcmdq_rdy = ~enq_vmu_utcmdq | vmu_utcmdq_rdy;
  wire mask_vmu_utimmq_rdy = ~enq_vmu_utimmq | vmu_utimmq_rdy;

  wire `DEF_DATA imm
    = (itype == `II) ? {1'd0, {52{id_reg_inst[21]}}, id_reg_inst[21:10]}
    : (itype == `IB) ? {1'd0, {52{id_reg_inst[31]}}, id_reg_inst[31:27], id_reg_inst[16:10]}
    : (itype == `IL) ? {1'd0, {32{id_reg_inst[26]}}, id_reg_inst[26:7], 12'd0}
    : `SZ_DATA'bx;

  assign vmu_utcmdq_bits = {utcmd, `UTMCMD_VLEN_SZ'd0}; // vlen is filled by upper level
  assign vmu_utimmq_bits = imm[31:0];

  assign vmu_utcmdq_val = vt_active & ~(vd_val & ~rtype_vd & id_reg_inst[31:27] == 5'd0) & mask_issue_rdy & enq_vmu_utcmdq & mask_vmu_utimmq_rdy;
  assign vmu_utimmq_val = vt_active & ~(vd_val & ~rtype_vd & id_reg_inst[31:27] == 5'd0) & mask_issue_rdy & mask_vmu_utcmdq_rdy & enq_vmu_utimmq;

  wire issue_val_common = vt_active & mask_vmu_utcmdq_rdy & mask_vmu_utimmq_rdy;

  assign issue_val_viu = issue_val_common & unmasked_val_viu;
  assign issue_val_vau0 = issue_val_common & unmasked_val_vau0;
  assign issue_val_vau1 = issue_val_common & unmasked_val_vau1;
  assign issue_val_vau2 = issue_val_common & unmasked_val_vau2;
  assign issue_val_vgslu = issue_val_common & unmasked_val_vgslu;
  assign issue_val_vglu = issue_val_common & unmasked_val_vglu;
  assign issue_val_vgsu = issue_val_common & unmasked_val_vgsu;

  assign issue_vs_decoded = {rtype_vs, id_reg_inst[26:22]};
  assign issue_vt_decoded = {rtype_vt, id_reg_inst[21:17]};
  assign issue_vr_decoded = {rtype_vr, id_reg_inst[16:12]};
  assign issue_vd_decoded = {rtype_vd, id_reg_inst[31:27]};
  assign issue_vd_val = vd_val;
  assign issue_imm = imm;

  reg reg_illegal;

  always @(posedge clk)
  begin
    if (reset)
      reg_illegal <= 1'b0;
    else
      reg_illegal <= vt_active & (~unmasked_val & ~decode_stop);
  end

  assign illegal = reg_illegal;

`ifndef SYNTHESIS
  disasmInst dasm_vt_f(imem_resp_data);
  disasmInst dasm_vt_d(id_reg_inst);
`endif

endmodule

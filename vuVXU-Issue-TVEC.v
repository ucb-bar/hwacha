`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Issue_TVEC
(
  input clk,
  input reset,

  output `DEF_VLEN   vlen,
  output `DEF_REGLEN stride,
  output `DEF_REGCNT nxregs,
  output `DEF_BANK   bactive,
  output `DEF_BCNT   bcnt,

  output             vt_active,
  output             vt_fire,
  input              vt_stop,
  output `DEF_ADDR   vt_pc,

  input  `DEF_VXU_CMDQ vxu_cmdq_bits,
  input                vxu_cmdq_val,
  output               vxu_cmdq_rdy,

  input  `DEF_VXU_IMMQ vxu_immq_bits,
  input                vxu_immq_val,
  output               vxu_immq_rdy,

  output `DEF_VMU_UTCMDQ vmu_utcmdq_bits,
  output                 vmu_utcmdq_val,
  input                  vmu_utcmdq_rdy,

  output issue_val_viu,
  output issue_val_vlu,
  output issue_val_vsu,

  input issue_rdy,

  output issue_dhazard_vt,
  output issue_dhazard_vd,

  output issue_shazard_vlu,
  output issue_shazard_vsu,

  output issue_bhazard_r1w1,
  output issue_bhazard_vlu,
  output issue_bhazard_vsu,

  output `DEF_VIU_FN  issue_fn_viu,

  output `DEF_REGLEN  issue_vt_decoded,
  output `DEF_REGLEN  issue_vd_decoded,
  output reg          issue_vd_val,
  output `DEF_DATA    issue_imm
);


/////////////////////////////////////////////////////////////////////////////
// STATE
/////////////////////////////////////////////////////////////////////////////

  localparam ISSUE_TVEC = 1'b0;
  localparam ISSUE_VT = 1'b1;

  reg next_state;
  reg reg_state;

  always @(posedge clk)
  begin
    if (reset)
      reg_state <= ISSUE_TVEC;
    else
      reg_state <= next_state;
  end

  wire tvec_active = (reg_state == ISSUE_TVEC);


/////////////////////////////////////////////////////////////////////////////
// DECODE
/////////////////////////////////////////////////////////////////////////////

  wire [`XCMD_CMD_SZ-1:0] cmd = vxu_cmdq_bits[`XCMD_CMCODE];
  wire [`XCMD_VD_SZ-1:0] vd = vxu_cmdq_bits[`XCMD_VD];
  wire [`XCMD_VS_SZ-1:0] vt = vxu_cmdq_bits[`XCMD_VS];
  wire [`XIMM_SZ-1:0] imm = vxu_immq_bits;

  localparam cs_sz = 15;

  localparam y = 1'b1;
  localparam n = 1'b0;

  reg [2:0] issue_val;
  reg [1:0] dhazard;
  reg [1:0] shazard;
  reg [2:0] bhazard;
  reg [1:0] vmsrc;
  reg decode_vcfg;
  reg decode_setvl;
  reg decode_vf;
  reg deq_vxu_immq;
  reg enq_vmu_utcmdq;

  `define TVCS {issue_val,dhazard,shazard,bhazard,vmsrc,issue_vd_val,decode_vcfg,decode_setvl,decode_vf,deq_vxu_immq,enq_vmu_utcmdq}

  always @(*)
  begin
    casez (cmd)
                      //                                    vd_val
                      //                                    | decode_vcfg
                      //                                    | | decode_setvl
                      //                                    | | | decode_vf
                      //                                    | | | | deq_vxu_immq
                      //      issue_val                     | | | | | enq_vmu_utcmdq
                      //      |      dhaz   shaz   bhaz msrc| | | | | |
                      //      |      |      |       |   |   | | | | | |
    `CMD_VVCFGIVL  : `TVCS = {3'b000,2'b00,2'b00,3'b000,`M0,n,y,y,n,y,n};
    `CMD_VSETVL    : `TVCS = {3'b000,2'b00,2'b00,3'b000,`M0,n,n,y,n,y,n};
    `CMD_VF        : `TVCS = {3'b000,2'b00,2'b00,3'b000,`M0,n,n,n,y,y,n};

    `CMD_FENCE_L_V : `TVCS = {3'b000,2'b00,2'b00,3'b000,`M0,n,n,n,n,n,y};
    `CMD_FENCE_G_V : `TVCS = {3'b000,2'b00,2'b00,3'b000,`M0,n,n,n,n,n,y};
    `CMD_FENCE_L_CV: `TVCS = {3'b000,2'b00,2'b00,3'b000,`M0,n,n,n,n,n,y};
    `CMD_FENCE_G_CV: `TVCS = {3'b000,2'b00,2'b00,3'b000,`M0,n,n,n,n,n,y};

    `CMD_VMVV      : `TVCS = {3'b001,2'b11,2'b00,3'b001,`MR,y,n,n,n,n,n};
    `CMD_VMSV      : `TVCS = {3'b001,2'b10,2'b00,3'b001,`MI,y,n,n,n,y,n};
    `CMD_VFMVV     : `TVCS = {3'b001,2'b11,2'b00,3'b001,`MR,y,n,n,n,n,n};

    `CMD_LDWB      : `TVCS = {3'b010,2'b10,2'b01,3'b010,`M0,y,n,n,n,n,n};
    `CMD_STAC      : `TVCS = {3'b100,2'b01,2'b10,3'b100,`M0,n,n,n,n,n,n};

    default        : `TVCS = {3'b000,2'b00,2'b00,3'b000,`M0,n,n,n,n,n,n};
    endcase
  end

  wire fire_vcfg  = tvec_active & vxu_cmdq_val & decode_vcfg;
  wire fire_setvl = tvec_active & vxu_cmdq_val & decode_setvl;
  wire fire_vf    = tvec_active & vxu_cmdq_val & decode_vf;


/////////////////////////////////////////////////////////////////////////////
// REGISTERS
/////////////////////////////////////////////////////////////////////////////

  reg `DEF_VLEN   next_vlen;
  reg `DEF_REGCNT next_nxregs;
  reg `DEF_REGCNT next_nfregs;
  reg `DEF_BANK   next_bactive;
  reg `DEF_BCNT   next_bcnt;
  reg `DEF_REGLEN next_stride;

  reg `DEF_VLEN   reg_vlen;
  reg `DEF_REGCNT reg_nxregs;
  reg `DEF_REGCNT reg_nfregs;
  reg `DEF_BANK   reg_bactive;
  reg `DEF_BCNT   reg_bcnt;
  reg `DEF_REGLEN reg_stride;

  always @(posedge clk)
  begin
    if (reset)
    begin
      reg_vlen <= `SZ_VLEN'd0; // this is really not a zero, since vlen is -1
      reg_nxregs <= `SZ_REGCNT'd32;
      reg_nfregs <= `SZ_REGCNT'd32;
      reg_bactive <= `SZ_BANK'b1111_1111;
      reg_bcnt <= `SZ_LGBANK1'd8;
      reg_stride <= `SZ_REGLEN'd63;
    end
    else
    begin
      reg_vlen <= next_vlen;
      reg_nxregs <= next_nxregs;
      reg_nfregs <= next_nfregs;
      reg_bactive <= next_bactive;
      reg_bcnt <= next_bcnt;
      reg_stride <= next_stride;
    end
  end

  always @(*)
  begin
    next_state = reg_state;
    next_vlen = reg_vlen;
    next_nxregs = reg_nxregs;
    next_nfregs = reg_nfregs;
    next_bactive = reg_bactive;
    next_bcnt = reg_bcnt;
    next_stride = reg_stride;

    if (fire_vcfg)
    begin
      next_vlen = vxu_immq_bits[10:0];
      next_nxregs = vxu_immq_bits[16:11];
      next_nfregs = vxu_immq_bits[22:17];
      next_bactive = vxu_immq_bits[30:23];
      next_bcnt = vxu_immq_bits[34:31];
      next_stride = next_nxregs + next_nfregs - 2'd1;
    end

    else if (fire_setvl)
    begin
      next_vlen = vxu_immq_bits[10:0];
    end

    else if (fire_vf)
      next_state = ISSUE_VT;

    else if (vt_stop)
      next_state = ISSUE_TVEC;
  end


/////////////////////////////////////////////////////////////////////////////
// SIGNALS
/////////////////////////////////////////////////////////////////////////////

  assign vlen = reg_vlen;
  assign stride = reg_stride;
  assign nxregs = reg_nxregs;
  assign bactive = reg_bactive;
  assign bcnt = reg_bcnt;

  assign vt_active = (reg_state == ISSUE_VT);
  assign vt_fire = fire_vf;
  assign vt_pc = vxu_immq_bits[31:0];

  wire mask_vxu_immq_val = ~deq_vxu_immq | vxu_immq_val;
  wire mask_issue_rdy = ~(|issue_val) | issue_rdy;
  wire mask_vmu_utcmdq_rdy = ~enq_vmu_utcmdq | vmu_utcmdq_rdy;

  assign vxu_cmdq_rdy =
    tvec_active &&
    1'b1 && mask_vxu_immq_val &&
    mask_issue_rdy &&
    mask_vmu_utcmdq_rdy;

  assign vxu_immq_rdy =
    tvec_active &&
    vxu_cmdq_val && deq_vxu_immq &&
    mask_issue_rdy &&
    mask_vmu_utcmdq_rdy;

  assign vmu_utcmdq_val =
    tvec_active &&
    vxu_cmdq_val && mask_vxu_immq_val &&
    mask_issue_rdy &&
    enq_vmu_utcmdq;

  assign issue_val_viu = issue_val[0] & tvec_active & vxu_cmdq_val & mask_vxu_immq_val & mask_vmu_utcmdq_rdy;
  assign issue_val_vlu = issue_val[1] & tvec_active & vxu_cmdq_val & mask_vxu_immq_val & mask_vmu_utcmdq_rdy;
  assign issue_val_vsu = issue_val[2] & tvec_active & vxu_cmdq_val & mask_vxu_immq_val & mask_vmu_utcmdq_rdy;

  assign issue_dhazard_vt = dhazard[0];
  assign issue_dhazard_vd = dhazard[1];

  assign issue_shazard_vlu = shazard[0];
  assign issue_shazard_vsu = shazard[1];

  assign issue_bhazard_r1w1 = bhazard[0];
  assign issue_bhazard_vlu = bhazard[1];
  assign issue_bhazard_vsu = bhazard[2];

  assign issue_fn_viu = {`M0,vmsrc,`DW64,`FP_,`VIU_MOV};

  assign issue_vt_decoded = vt;
  assign issue_vd_decoded = vd;
  assign issue_imm = imm;

  assign vmu_utcmdq_bits = {cmd, `UTMCMD_VLEN_SZ'd0};

`ifndef SYNTHESIS
  disasm_cmd tvec_cmd (cmd);
`endif

endmodule

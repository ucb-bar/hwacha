`include "defCommon.vh"
`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVCU
(
  input clk,
  input reset,

  input [`VCMD_SZ-1:0] vec_cmdq_bits,
  input                vec_cmdq_val,
  output               vec_cmdq_rdy,

  input [`VIMM_SZ-1:0] vec_ximm1q_bits,
  input                vec_ximm1q_val,
  output               vec_ximm1q_rdy,

  input [`VSTRIDE_SZ-1:0] vec_ximm2q_bits,
  input                   vec_ximm2q_val,
  output                  vec_ximm2q_rdy,

  output [`XCMD_SZ-1:0] vxu_cmdq_bits,
  output                vxu_cmdq_val,
  input                 vxu_cmdq_rdy,

  output [`XIMM_SZ-1:0] vxu_immq_bits,
  output                vxu_immq_val,
  input                 vxu_immq_rdy,

  output [`VMCMD_SZ-1:0] vmu_vcmdq_bits,
  output                 vmu_vcmdq_val,
  input                  vmu_vcmdq_rdy,

  output [`VMIMM_SZ-1:0] vmu_vbaseq_bits,
  output                 vmu_vbaseq_val,
  input                  vmu_vbaseq_rdy,

  output [`VMSTRIDE_SZ-1:0] vmu_vstrideq_bits,
  output                    vmu_vstrideq_val,
  input                     vmu_vstrideq_rdy,

  output reg [`VRESP_SZ-1:0] vec_ackq_bits,
  output reg                 vec_ackq_val,
  input                     vec_ackq_rdy,

  input [`XRESP_SZ-1:0] vxu_ackq_bits,
  input                 vxu_ackq_val,
  output reg            vxu_ackq_rdy,

  input [`VMRESP_SZ-1:0] vmu_vackq_bits,
  input                  vmu_vackq_val,
  output reg             vmu_vackq_rdy
);


/////////////////////////////////////////////////////////////////////////////
// DECODE
/////////////////////////////////////////////////////////////////////////////

  wire [`VCMD_CMD_SZ-1:0] cmd = vec_cmdq_bits[`VCMD_CMCODE];

  localparam y = 1'b1;
  localparam n = 1'b0;

  reg decode_fence_cv;
  reg decode_fence_v;
  reg decode_setvl;
  reg deq_vec_ximm1q;
  reg deq_vec_ximm2q;
  reg enq_vxu_cmdq;
  reg enq_vxu_immq;
  reg enq_vmu_vcmdq;
  reg enq_vmu_vbaseq;
  reg enq_vmu_vstrideq;
  reg [1:0] sel_vxu_cmdq;

  `define CS {decode_fence_cv,decode_fence_v,decode_setvl,deq_vec_ximm1q,deq_vec_ximm2q,enq_vxu_cmdq,enq_vxu_immq,enq_vmu_vcmdq,enq_vmu_vbaseq,enq_vmu_vstrideq,sel_vxu_cmdq}

  localparam FWD = 2'd0;
  localparam LDWB = 2'd1;
  localparam STAC = 2'd2;

  always @(*)
  begin
    casez (cmd)
                         // decode_fence_cv
                         // |  decode_fence_v
                         // |  |  decode_setvl
                         // |  |  |  deq_vec_ximm1q
                         // |  |  |  |  deq_vec_ximm2q
                         // |  |  |  |  |  enq_vxu_cmdq
                         // |  |  |  |  |  |  enq_vxu_immq
                         // |  |  |  |  |  |  |  enq_vmu_vcmdq
                         // |  |  |  |  |  |  |  |  enq_vmu_vbaseq
                         // |  |  |  |  |  |  |  |  |  enq_vmu_vstrideq
                         // |  |  |  |  |  |  |  |  |  |  sel_vxu_cmdq
                         // |  |  |  |  |  |  |  |  |  |  |
    `CMD_VVCFGIVL  : `CS = {n, n, y, y, n, y, y, n, n, n, FWD};
    `CMD_VSETVL    : `CS = {n, n, y, y, n, y, y, n, n, n, FWD};
    `CMD_VF        : `CS = {n, n, n, y, n, y, y, n, n, n, FWD};

    `CMD_FENCE_L_V : `CS = {n, y, n, n, n, y, n, y, n, n, FWD};
    `CMD_FENCE_G_V : `CS = {n, y, n, n, n, y, n, y, n, n, FWD};
    `CMD_FENCE_L_CV: `CS = {y, n, n, n, n, y, n, y, n, n, FWD};
    `CMD_FENCE_G_CV: `CS = {y, n, n, n, n, y, n, y, n, n, FWD};

    `CMD_VMVV      : `CS = {n, n, n, n, n, y, n, n, n, n, FWD};
    `CMD_VMSV      : `CS = {n, n, n, y, n, y, y, n, n, n, FWD};
    `CMD_VFMVV     : `CS = {n, n, n, n, n, y, n, n, n, n, FWD};

    `CMD_VLD       : `CS = {n, n, n, y, n, y, n, y, y, n, LDWB};
    `CMD_VLW       : `CS = {n, n, n, y, n, y, n, y, y, n, LDWB};
    `CMD_VLWU      : `CS = {n, n, n, y, n, y, n, y, y, n, LDWB};
    `CMD_VLH       : `CS = {n, n, n, y, n, y, n, y, y, n, LDWB};
    `CMD_VLHU      : `CS = {n, n, n, y, n, y, n, y, y, n, LDWB};
    `CMD_VLB       : `CS = {n, n, n, y, n, y, n, y, y, n, LDWB};
    `CMD_VLBU      : `CS = {n, n, n, y, n, y, n, y, y, n, LDWB};
    `CMD_VSD       : `CS = {n, n, n, y, n, y, n, y, y, n, STAC};
    `CMD_VSW       : `CS = {n, n, n, y, n, y, n, y, y, n, STAC};
    `CMD_VSH       : `CS = {n, n, n, y, n, y, n, y, y, n, STAC};
    `CMD_VSB       : `CS = {n, n, n, y, n, y, n, y, y, n, STAC};

    `CMD_VFLD      : `CS = {n, n, n, y, n, y, n, y, y, n, LDWB};
    `CMD_VFLW      : `CS = {n, n, n, y, n, y, n, y, y, n, LDWB};
    `CMD_VFSD      : `CS = {n, n, n, y, n, y, n, y, y, n, STAC};
    `CMD_VFSW      : `CS = {n, n, n, y, n, y, n, y, y, n, STAC};

    `CMD_VLSTD     : `CS = {n, n, n, y, y, y, n, y, y, y, LDWB};
    `CMD_VLSTW     : `CS = {n, n, n, y, y, y, n, y, y, y, LDWB};
    `CMD_VLSTWU    : `CS = {n, n, n, y, y, y, n, y, y, y, LDWB};
    `CMD_VLSTH     : `CS = {n, n, n, y, y, y, n, y, y, y, LDWB};
    `CMD_VLSTHU    : `CS = {n, n, n, y, y, y, n, y, y, y, LDWB};
    `CMD_VLSTB     : `CS = {n, n, n, y, y, y, n, y, y, y, LDWB};
    `CMD_VLSTBU    : `CS = {n, n, n, y, y, y, n, y, y, y, LDWB};
    `CMD_VSSTD     : `CS = {n, n, n, y, y, y, n, y, y, y, STAC};
    `CMD_VSSTW     : `CS = {n, n, n, y, y, y, n, y, y, y, STAC};
    `CMD_VSSTH     : `CS = {n, n, n, y, y, y, n, y, y, y, STAC};
    `CMD_VSSTB     : `CS = {n, n, n, y, y, y, n, y, y, y, STAC};

    `CMD_VFLSTD    : `CS = {n, n, n, y, y, y, n, y, y, y, LDWB};
    `CMD_VFLSTW    : `CS = {n, n, n, y, y, y, n, y, y, y, LDWB};
    `CMD_VFSSTD    : `CS = {n, n, n, y, y, y, n, y, y, y, STAC};
    `CMD_VFSSTW    : `CS = {n, n, n, y, y, y, n, y, y, y, STAC};

    default        : `CS = {n, n, n, n, n, y, n, n, n, n, FWD};
    endcase

`ifndef SYNTHESIS
  `RTL_PROPAGATE_X(cmd, decode_fence_cv);
  `RTL_PROPAGATE_X(cmd, decode_fence_v);
  `RTL_PROPAGATE_X(cmd, decode_setvl);
  `RTL_PROPAGATE_X(cmd, deq_vec_ximm1q);
  `RTL_PROPAGATE_X(cmd, deq_vec_ximm2q);
  `RTL_PROPAGATE_X(cmd, enq_vxu_cmdq);
  `RTL_PROPAGATE_X(cmd, enq_vxu_immq);
  `RTL_PROPAGATE_X(cmd, enq_vmu_vcmdq);
  `RTL_PROPAGATE_X(cmd, enq_vmu_vbaseq);
  `RTL_PROPAGATE_X(cmd, enq_vmu_vstrideq);
`endif
  end

  wire mask_vec_ximm1q_val = ~deq_vec_ximm1q | vec_ximm1q_val;
  wire mask_vec_ximm2q_val = ~deq_vec_ximm2q | vec_ximm2q_val;
  wire mask_vxu_cmdq_rdy = ~enq_vxu_cmdq | vxu_cmdq_rdy;
  wire mask_vxu_immq_rdy = ~enq_vxu_immq | vxu_immq_rdy;
  wire mask_vmu_vcmdq_rdy = ~enq_vmu_vcmdq | vmu_vcmdq_rdy;
  wire mask_vmu_vbaseq_rdy = ~enq_vmu_vbaseq | vmu_vbaseq_rdy;
  wire mask_vmu_vstrideq_rdy = ~enq_vmu_vstrideq | vmu_vstrideq_rdy;

  wire fire_fence_cv =
    decode_fence_cv &
    vec_cmdq_val & mask_vec_ximm1q_val & mask_vec_ximm2q_val &
    mask_vxu_cmdq_rdy & mask_vxu_immq_rdy &
    mask_vmu_vcmdq_rdy & mask_vmu_vbaseq_rdy & mask_vmu_vstrideq_rdy;

  wire fire_fence_v =
    decode_fence_v &
    vec_cmdq_val & mask_vec_ximm1q_val & mask_vec_ximm2q_val &
    mask_vxu_cmdq_rdy & mask_vxu_immq_rdy &
    mask_vmu_vcmdq_rdy & mask_vmu_vbaseq_rdy & mask_vmu_vstrideq_rdy;

  wire fire_setvl =
    decode_setvl &
    vec_cmdq_val & mask_vec_ximm1q_val & mask_vec_ximm2q_val &
    mask_vxu_cmdq_rdy & mask_vxu_immq_rdy &
    mask_vmu_vcmdq_rdy & mask_vmu_vbaseq_rdy & mask_vmu_vstrideq_rdy;


/////////////////////////////////////////////////////////////////////////////
// REGISTERS
/////////////////////////////////////////////////////////////////////////////

  localparam VCU_FORWARD = 2'd0;
  localparam VCU_FENCE_CV = 2'd1;
  localparam VCU_FENCE_V = 2'd2;

  reg [1:0] state_next;
  reg [1:0] state_reg;

  always @(posedge clk)
  begin
    if (reset)
      state_reg <= VCU_FORWARD;
    else
      state_reg <= state_next;
  end

  always @(*)
  begin
    state_next = state_reg;

    case (state_reg)
    VCU_FORWARD:
      begin
        if (fire_fence_cv)
          state_next = VCU_FENCE_CV;
        if (fire_fence_v)
          state_next = VCU_FENCE_V;
      end
    VCU_FENCE_CV:
      if (vmu_vackq_val && vxu_ackq_val && vec_ackq_rdy)
        state_next = VCU_FORWARD;
    VCU_FENCE_V:
      if (vmu_vackq_val && vxu_ackq_val)
        state_next = VCU_FORWARD;
    endcase

`ifndef SYNTHESIS
  `RTL_PROPAGATE_X(fire_fence_cv, state_next);
  `RTL_PROPAGATE_X(fire_fence_v, state_next);
  `RTL_PROPAGATE_X(vmu_vackq_val && vxu_ackq_val && vec_ackq_rdy, state_next);
  `RTL_PROPAGATE_X(vmu_vackq_val && vxu_ackq_val, state_next);
`endif
  end

  reg [`VLENMAX_SZ-1:0] vlen_next;
  reg [`VLENMAX_SZ-1:0] vlen_reg;

  always @(posedge clk)
  begin
    if (reset)
      vlen_reg <= `VLENMAX_SZ'd0; // this is really not a zero, since vlen is -1
    else
      vlen_reg <= vlen_next;
  end

  always @(*)
  begin
    vlen_next = vlen_reg;

    if (fire_setvl)
      vlen_next = vec_ximm1q_bits[`VLENMAX_SZ-1:0];

`ifndef SYNTHESIS
  `RTL_PROPAGATE_X(fire_setvl, vlen_next);
`endif
  end


/////////////////////////////////////////////////////////////////////////////
// SIGNALS
/////////////////////////////////////////////////////////////////////////////

  wire forward = (state_reg == VCU_FORWARD);

  assign vec_cmdq_rdy =
    forward &&
    1'b1 && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

  assign vec_ximm1q_rdy =
    forward &&
    vec_cmdq_val && deq_vec_ximm1q && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

  assign vec_ximm2q_rdy =
    forward &&
    vec_cmdq_val && mask_vec_ximm1q_val && deq_vec_ximm2q &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

  assign vxu_cmdq_val =
    forward &&
    vec_cmdq_val && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    enq_vxu_cmdq && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

  assign vxu_immq_val =
    forward &&
    vec_cmdq_val && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && enq_vxu_immq &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

  assign vmu_vcmdq_val =
    forward &&
    vec_cmdq_val && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    enq_vmu_vcmdq && mask_vmu_vbaseq_rdy && mask_vmu_vstrideq_rdy;

  assign vmu_vbaseq_val =
    forward &&
    vec_cmdq_val && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && enq_vmu_vbaseq && mask_vmu_vstrideq_rdy;

  assign vmu_vstrideq_val =
    forward &&
    vec_cmdq_val && mask_vec_ximm1q_val && mask_vec_ximm2q_val &&
    mask_vxu_cmdq_rdy && mask_vxu_immq_rdy &&
    mask_vmu_vcmdq_rdy && mask_vmu_vbaseq_rdy && enq_vmu_vstrideq;

  assign vxu_cmdq_bits
    = sel_vxu_cmdq == LDWB ? {`CMD_LDWB, vec_cmdq_bits[11:0]}
    : sel_vxu_cmdq == STAC ? {`CMD_STAC, vec_cmdq_bits[11:0]}
    : vec_cmdq_bits;

  assign vxu_immq_bits = vec_ximm1q_bits;
  assign vmu_vcmdq_bits = {cmd, vlen_reg};
  assign vmu_vbaseq_bits = vec_ximm1q_bits[31:0];
  assign vmu_vstrideq_bits = vec_ximm2q_bits;

  // response logic
  always @(*)
  begin
    vec_ackq_bits = 32'd0;
    vec_ackq_val = 1'b0;
    vxu_ackq_rdy = 1'b0;
    vmu_vackq_rdy = 1'b0;

    case (state_reg)
    VCU_FORWARD: ;
    VCU_FENCE_CV:
      begin
        if (vxu_ackq_val && vmu_vackq_val)
        begin
          vec_ackq_bits = 32'd1;
          vec_ackq_val = 1'b1;
        end

        vxu_ackq_rdy = vmu_vackq_val;
        vmu_vackq_rdy = vxu_ackq_val;
      end
    VCU_FENCE_V:
      begin
        vxu_ackq_rdy = vmu_vackq_val;
        vmu_vackq_rdy = vxu_ackq_val;
      end
    endcase

`ifndef SYNTHESIS
  `RTL_PROPAGATE_X(state_reg, vec_ackq_bits);
  `RTL_PROPAGATE_X(state_reg, vec_ackq_val);
  `RTL_PROPAGATE_X(state_reg, vxu_ackq_rdy);
  `RTL_PROPAGATE_X(state_reg, vmu_vackq_rdy);
`endif
  end

  // assert logic
`ifndef SYNTHESIS
  disasm_cmd asm(cmd);
`endif

endmodule

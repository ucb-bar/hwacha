`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"
`include "riscvConst.vh"

module vuVMU_Ctrl_vec_load_wb
(
  input clk,
  input reset,

  output reg vec_done,
  input      ldq_deq,

  input [`VM_WBCMD_SZ-1:0] wbcmdq_deq_bits,
  input                    wbcmdq_deq_val,
  output reg               wbcmdq_deq_rdy,

  // interface to reorder queue
  input [127:0]            roq_deq_bits,
  input                    roq_deq_val,
  output reg               roq_deq_rdy,

  // interface to load data queue
  output [64:0]            ldq_enq_bits,
  output                   ldq_enq_val,
  input                    ldq_enq_rdy

);

  localparam VMU_Ctrl_Idle          = 2'd0;
  localparam VMU_Ctrl_Writeback     = 2'd1;
  localparam VMU_Ctrl_WritebackDone = 2'd2;

  wire [`VMCMD_VLEN_SZ-1:0] vlen     = wbcmdq_deq_bits[`VMCMD_VLEN_SZ-1:0];
  wire [`VMIMM_SZ-1:0]      addr     = wbcmdq_deq_bits[`VMIMM_SZ+`VMCMD_VLEN_SZ-1:`VMCMD_VLEN_SZ];
  wire [`VMSTRIDE_SZ-1:0]   stride   = wbcmdq_deq_bits[`VMSTRIDE_SZ+`VMIMM_SZ+`VMCMD_VLEN_SZ-1:`VMIMM_SZ+`VMCMD_VLEN_SZ];
  wire [3:0]                cmd_type = wbcmdq_deq_bits[`VM_WBCMD_SZ-1:`VM_WBCMD_SZ-4];

  reg [3:0] cmd_type_reg, cmd_type_next;
  reg [`VMIMM_SZ-1:0] addr_reg, addr_next;
  reg [`VMSTRIDE_SZ-1:0] stride_reg, stride_next;
  reg [`VMCMD_VLEN_SZ-1:0] vlen_reg, vlen_next;
  reg [`VMCMD_VLEN_SZ-1:0] vlen_cnt_reg, vlen_cnt_next;
  
  reg [1:0] state, nstate;

  reg         buf_ldq_enq_val;
  wire        buf_ldq_enq_rdy;

  wire [127:0] delay_roq_deq_bits;
  wire [3:0]   delay_cmd_type;
  wire [3:0]   delay_addr_lsb;

  `VC_PIPE1_QUEUE(128+4+4) skidbuf
  (
    .clk(clk),
    .reset(reset),

    .enq_bits({cmd_type_reg,addr_reg[3:0],roq_deq_bits}),
    .enq_val(buf_ldq_enq_val),
    .enq_rdy(buf_ldq_enq_rdy),

    .deq_bits({delay_cmd_type,delay_addr_lsb,delay_roq_deq_bits}),
    .deq_val(ldq_enq_val),
    .deq_rdy(ldq_enq_rdy)
  );
  
  wire [63:0] ldq_bits;
  wire [32:0] ldq_sp_bits;
  wire [64:0] ldq_dp_bits;
  wire fp_cmd = delay_cmd_type[3];
  
  assign ldq_enq_bits
    = fp_cmd && (delay_cmd_type[1:0] == 2'b11) ? ldq_dp_bits
    : fp_cmd && (delay_cmd_type[1:0] == 2'b10) ? {32'hFFFF_FFFF, ldq_sp_bits}
    : { 1'b0, ldq_bits };

  floatNToRecodedFloatN #(8,24) recode_sp ( ldq_bits[31:0], ldq_sp_bits );
  floatNToRecodedFloatN #(11,53) recode_dp ( ldq_bits, ldq_dp_bits );
  
  vuVMU_BHWD_sel bhwd_sel
  (
    .bhwd_sel           (delay_cmd_type[1:0]),
    .signext            (delay_cmd_type[2]),
    .addr_lsb           (delay_addr_lsb),
    .din                (delay_roq_deq_bits),
    .dout               (ldq_bits)
  );
  
  always @(posedge clk)
  begin
    if (reset)
    begin
      state <= VMU_Ctrl_Idle;
      addr_reg <= 0;
      vlen_reg <= 0;
      vlen_cnt_reg <= 0;
      stride_reg <= 0;
      cmd_type_reg <= 0;
    end
    else
    begin
      state <= nstate;
      addr_reg <= addr_next;
      vlen_reg <= vlen_next;
      vlen_cnt_reg <= vlen_cnt_next;
      stride_reg <= stride_next;
      cmd_type_reg <= cmd_type_next;
    end
  end

  always @(*)
  begin
    addr_next = addr_reg;
    vlen_next = vlen_reg;
    vlen_cnt_next = vlen_cnt_reg;
    stride_next = stride_reg;
    cmd_type_next = cmd_type_reg;
    wbcmdq_deq_rdy = 1'b0;
    roq_deq_rdy = 1'b0;
    buf_ldq_enq_val = 1'b0;
    vec_done = 1'b0;

    nstate = state;
    case (state)
      VMU_Ctrl_Idle:
      begin
        wbcmdq_deq_rdy = 1'b1;
        if (wbcmdq_deq_val)
        begin
          addr_next = addr;
          stride_next = stride;
          vlen_next = vlen;
          vlen_cnt_next = vlen;
          cmd_type_next = cmd_type;
          nstate = VMU_Ctrl_Writeback;
        end
      end
      VMU_Ctrl_Writeback:
      begin
        if (roq_deq_val)
        begin
          buf_ldq_enq_val = 1'b1;
          if (buf_ldq_enq_rdy)
          begin
            addr_next = addr_reg + stride_reg;
            vlen_next = vlen_reg - 1'b1;
            if (vlen_reg == 0)
            begin
              nstate = VMU_Ctrl_WritebackDone;
              roq_deq_rdy = 1'b1;
            end
            else if (addr_next[31:4] != addr_reg[31:4])
              roq_deq_rdy = 1'b1;
          end
        end
        if (ldq_deq)
        begin
          vlen_cnt_next = vlen_cnt_reg - 1'b1;
        end
      end
      VMU_Ctrl_WritebackDone:
      begin
        vec_done = 1'b1;
        if (ldq_deq)
        begin
          if (vlen_cnt_reg == 0)
          begin
            nstate = VMU_Ctrl_Idle;
          end
          else
          begin
            vlen_cnt_next = vlen_cnt_reg - 1'b1;
          end
        end
      end
      default:;
    endcase
  end

endmodule

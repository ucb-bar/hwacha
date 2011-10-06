`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVMU_Ctrl_vec_load_issue
(
  input clk,
  input reset,

  input [`VM_ISCMD_SZ-1:0]             iscmdq_deq_bits,
  input                    iscmdq_deq_val,
  output reg               iscmdq_deq_rdy,

  // interface to load request queue
  // 28 bits for address, 8 bits for tag
  output [35:0]            lrq_enq_bits,
  input                    lrq_enq_rdy,
  output reg               lrq_enq_val,

  // interface to reorder queue (256 entries)
  input [7:0]              roq_deq_tag_bits,
  input                    roq_deq_tag_val,
  output reg               roq_deq_tag_rdy

);

  localparam VMU_Ctrl_Idle           = 2'd0;
  localparam VMU_Ctrl_IssueShort     = 2'd1;
  localparam VMU_Ctrl_IssueLong      = 2'd2;

  reg [31:0] addr_reg, addr_next;
  reg [31:0] last_addr_reg, last_addr_next;
  reg [31:0] stride_reg, stride_next;

  wire [`VMCMD_VLEN_SZ-1:0]  vlen   = iscmdq_deq_bits[`VMCMD_VLEN_SZ-1:0];
  wire [`VMIMM_SZ-1:0] addr   = iscmdq_deq_bits[`VMIMM_SZ+`VMCMD_VLEN_SZ-1:`VMCMD_VLEN_SZ];
  wire [`VMSTRIDE_SZ-1:0] stride = iscmdq_deq_bits[`VMSTRIDE_SZ+`VMIMM_SZ+`VMCMD_VLEN_SZ-1:`VMIMM_SZ+`VMCMD_VLEN_SZ];
  wire [31:0] addr_cacheline = {addr_reg[31:4], 4'b0000};

  reg [1:0] state, nstate;

  assign lrq_enq_bits = {addr_reg[31:4], roq_deq_tag_bits};

  always @(posedge clk)
  begin
    if (reset)
    begin
      state <= VMU_Ctrl_Idle;
      addr_reg <= 0;
      last_addr_reg <= 0;
      stride_reg <= 0;
    end
    else
    begin
      state <= nstate;
      addr_reg <= addr_next;
      last_addr_reg <= last_addr_next;
      stride_reg <= stride_next;
    end
  end

  always @(*)
  begin
    addr_next = addr_reg;
    last_addr_next = last_addr_reg;
    stride_next = stride_reg;
    lrq_enq_val = 1'b0;
    iscmdq_deq_rdy = 1'b0;
    roq_deq_tag_rdy = 1'b0;

    nstate = state;
    case (state)
      VMU_Ctrl_Idle:
      begin
        iscmdq_deq_rdy = 1'b1;
        if (iscmdq_deq_val)
        begin
          addr_next = addr;
          last_addr_next = addr + (vlen * stride);
          stride_next = stride;
          if (stride < 16)
            nstate = VMU_Ctrl_IssueShort;
          else
            nstate = VMU_Ctrl_IssueLong;
        end
      end
      VMU_Ctrl_IssueShort:
      begin
        if (addr_cacheline > last_addr_reg)
          nstate = VMU_Ctrl_Idle;
        else
        begin
          roq_deq_tag_rdy = lrq_enq_rdy;
          lrq_enq_val     = roq_deq_tag_val;
          if (roq_deq_tag_val & lrq_enq_rdy)
          begin
            addr_next = addr_reg + 16;
          end
        end
      end
      VMU_Ctrl_IssueLong:
      begin
        if (addr_cacheline > last_addr_reg)
          nstate = VMU_Ctrl_Idle;
        else
        begin
          roq_deq_tag_rdy = lrq_enq_rdy;
          lrq_enq_val     = roq_deq_tag_val;
          if (roq_deq_tag_val & lrq_enq_rdy)
          begin
              addr_next = addr_reg + stride_reg;
          end
        end
      end
      default:;
    endcase
  end

endmodule

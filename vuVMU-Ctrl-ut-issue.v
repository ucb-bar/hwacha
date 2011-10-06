`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVMU_Ctrl_ut_issue
(
  input clk,
  input reset,

  input [`UT_ISCMD_SZ-1:0] iscmdq_deq_bits,
  input                    iscmdq_deq_val,
  output reg               iscmdq_deq_rdy,

  input [31:0]             utaq_deq_bits,
  input                    utaq_deq_val,
  output reg               utaq_deq_rdy,

  // interface to load request queue
  output [29:0]            lrq_enq_addr_bits,
  output [11:0]            lrq_enq_tag_bits,
  input                    lrq_enq_rdy,
  output reg               lrq_enq_val,

  // interface to reorder queue
  input [7:0]              roq_deq_tag_bits,
  input                    roq_deq_tag_val,
  output reg               roq_deq_tag_rdy,

  output                   issue_busy

);

  localparam VMU_Ctrl_Idle          = 1'b0;
  localparam VMU_Ctrl_Issue         = 1'b1;

  reg [31:0] addr_reg, addr_next;
  reg [`UTMCMD_VLEN_SZ-1:0] vlen_reg, vlen_next;

  wire [`UTMCMD_VLEN_SZ-1:0] vlen = iscmdq_deq_bits[`UTMCMD_VLEN_SZ-1:0];
  wire [31:0] addr     = iscmdq_deq_bits[`UTMIMM_SZ+`UTMCMD_VLEN_SZ-1:`UTMCMD_VLEN_SZ];
  wire [31:0] req_addr = addr_reg + utaq_deq_bits;

  reg state, nstate;

  assign issue_busy = (state == VMU_Ctrl_Issue) | iscmdq_deq_val;
  assign lrq_enq_addr_bits = req_addr[31:2];
  assign lrq_enq_tag_bits  = {1'b0, req_addr[2:0], roq_deq_tag_bits};

  always @(posedge clk)
  begin
    if (reset)
    begin
      state <= VMU_Ctrl_Idle;
      addr_reg <= 0;
      vlen_reg <= 0;
    end
    else
    begin
      state <= nstate;
      addr_reg <= addr_next;
      vlen_reg <= vlen_next;
    end
  end

  always @(*)
  begin
    addr_next = addr_reg;
    vlen_next = vlen_reg;

    lrq_enq_val = 1'b0;
    iscmdq_deq_rdy = 1'b0;
    roq_deq_tag_rdy = 1'b0;
    utaq_deq_rdy = 1'b0;

    nstate = state;
    case (state)
      VMU_Ctrl_Idle:
      begin
        iscmdq_deq_rdy = 1'b1;
        if (iscmdq_deq_val)
        begin
          addr_next = addr;
          vlen_next = vlen;
          nstate = VMU_Ctrl_Issue;
        end
      end
      VMU_Ctrl_Issue:
      begin
        lrq_enq_val     = roq_deq_tag_val & utaq_deq_val;
        roq_deq_tag_rdy = lrq_enq_rdy     & utaq_deq_val;
        utaq_deq_rdy    = roq_deq_tag_val & lrq_enq_rdy;
        if (lrq_enq_rdy & roq_deq_tag_val & utaq_deq_val)
        begin
          if (vlen_reg == 0)
            nstate = VMU_Ctrl_Idle;
          else
            vlen_next = vlen_reg - 1'b1;
        end
      end
      default:;
    endcase
  end

endmodule

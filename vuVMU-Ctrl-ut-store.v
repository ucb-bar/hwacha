`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

`define CP_MREQ_X       3'bxxx
`define CP_MREQ_R       3'd0
`define CP_MREQ_W       3'd1
`define CP_MREQ_OR      3'd2
`define CP_MREQ_AND     3'd3
`define CP_MREQ_ADD     3'd4

module vuVMU_Ctrl_ut_store
(
  input clk,
  input reset,

  input [`UT_STCMD_SZ-1:0] stcmdq_deq_bits,
  input                     stcmdq_deq_val,
  output reg                stcmdq_deq_rdy,

  input [64:0]              sdq_deq_bits,
  input                     sdq_deq_val,
  output reg                sdq_deq_rdy,

  input [31:0]              utaq_deq_bits,
  input                     utaq_deq_val,
  output reg                utaq_deq_rdy,

  // interface to reorder queue
  input [7:0]              roq_deq_tag_bits,
  input                    roq_deq_tag_val,
  output reg               roq_deq_tag_rdy,

  output [29:0]            srq_enq_addr_bits,
  output [11:0]            srq_enq_tag_bits,
  output [3:0]             srq_enq_op_bits,
  output [63:0]            srq_enq_data_bits,
  output [7:0]             srq_enq_wmask_bits,
  output reg               srq_enq_val,
  input                    srq_enq_rdy,

  output                    store_busy
);

  localparam VMU_Ctrl_Idle          = 2'd0;
  localparam VMU_Ctrl_Store         = 2'd1;
  localparam VMU_Ctrl_AMO           = 2'd2;

  wire [`UTMCMD_VLEN_SZ-1:0] vlen = stcmdq_deq_bits[`UTMCMD_VLEN_SZ-1:0];
  wire [31:0] addr        = stcmdq_deq_bits[`UTMIMM_SZ+`UTMCMD_VLEN_SZ-1:`UTMCMD_VLEN_SZ];
  wire [4:0]  cmd_type    = stcmdq_deq_bits[`UT_STCMD_SZ-2:`UT_STCMD_SZ-6];
  wire        cmd_type_amo = stcmdq_deq_bits[`UT_STCMD_SZ-1];

  reg [4:0]  cmd_type_reg, cmd_type_next;
  reg [31:0] addr_reg, addr_next;
  reg [`UTMCMD_VLEN_SZ-1:0]  vlen_reg, vlen_next;
  reg [1:0]  state, nstate;

  reg  [7:0] store_data_wmask;
  wire [31:0] req_addr = addr_reg + utaq_deq_bits;
  wire [63:0] sdq_deq_dp_bits;
  wire [31:0] sdq_deq_sp_bits;

//  wire fp_cmd = cmd_type_reg[3];
  wire cmd_type_fp = (state == VMU_Ctrl_AMO) ? 1'b0 : cmd_type_reg[3];

  assign store_busy = (state != VMU_Ctrl_Idle) | stcmdq_deq_val;

  assign srq_enq_data_bits = cmd_type_fp && (cmd_type_reg[1:0] == 2'b11) ? sdq_deq_dp_bits : 
                             cmd_type_fp && (cmd_type_reg[1:0] == 2'b10) ? {2{sdq_deq_sp_bits}} :
                      (cmd_type_reg[1:0] == 2'b11) ? {sdq_deq_bits[63:0]} :
                      (cmd_type_reg[1:0] == 2'b10) ? {2{sdq_deq_bits[31:0]}} :
                      (cmd_type_reg[1:0] == 2'b01) ? {4{sdq_deq_bits[15:0]}} :
                      (cmd_type_reg[1:0] == 2'b00) ? {8{sdq_deq_bits[7:0]}} : 64'd0;

  assign srq_enq_addr_bits  = req_addr[31:2];

  assign srq_enq_tag_bits = (state == VMU_Ctrl_Store) ? 12'h800 :
                            (state == VMU_Ctrl_AMO)   ? {4'h8, roq_deq_tag_bits} : 12'd0;

  assign srq_enq_op_bits = (state == VMU_Ctrl_Store) ? 4'b0001 :
                           (state == VMU_Ctrl_AMO)   ? {1'b1, cmd_type_reg[4:2]} : 4'd0;

  assign srq_enq_wmask_bits = store_data_wmask;

  recodedFloatNToFloatN #(8,24) decoder_sp ( sdq_deq_bits[32:0], sdq_deq_sp_bits );
  recodedFloatNToFloatN #(11,53) decoder_dp ( sdq_deq_bits, sdq_deq_dp_bits );

  always @(posedge clk)
  begin
    if (reset)
    begin
      state <= VMU_Ctrl_Idle;
      addr_reg <= 0;
      vlen_reg <= 0;
      cmd_type_reg <= 0;
    end
    else
    begin
      state <= nstate;
      addr_reg <= addr_next;
      vlen_reg <= vlen_next;
      cmd_type_reg <= cmd_type_next;
    end
  end

  always @(*)
  begin
    addr_next = addr_reg;
    vlen_next = vlen_reg;
    cmd_type_next = cmd_type_reg;

    stcmdq_deq_rdy = 1'b0;
    sdq_deq_rdy = 1'b0;
    utaq_deq_rdy = 1'b0;
    srq_enq_val = 1'b0;
    roq_deq_tag_rdy = 1'b0;

    nstate = state;
    
    case (cmd_type_reg[1:0])
      2'b11: store_data_wmask = 8'b11111111;
      2'b10: store_data_wmask = req_addr[2] ? 8'b11110000 : 8'b00001111;
      2'b01: case (req_addr[2:1])
               2'b00 : store_data_wmask = 8'b00000011;
               2'b01 : store_data_wmask = 8'b00001100;
               2'b10 : store_data_wmask = 8'b00110000;
               2'b11 : store_data_wmask = 8'b11000000;
             endcase
      2'b00: case (req_addr[2:0])
               3'b000: store_data_wmask = 8'b00000001;
               3'b001: store_data_wmask = 8'b00000010;
               3'b010: store_data_wmask = 8'b00000100;
               3'b011: store_data_wmask = 8'b00001000;
               3'b100: store_data_wmask = 8'b00010000;
               3'b101: store_data_wmask = 8'b00100000;
               3'b110: store_data_wmask = 8'b01000000;
               3'b111: store_data_wmask = 8'b10000000;
             endcase
      default: store_data_wmask = 8'd0;
    endcase
    
    case (state)
      VMU_Ctrl_Idle:
      begin
        stcmdq_deq_rdy = 1'b1;
        if (stcmdq_deq_val)
        begin
          vlen_next = vlen;
          cmd_type_next = cmd_type;
          if (cmd_type_amo)
          begin
            addr_next = 0;
            nstate = VMU_Ctrl_AMO;
          end
          else
          begin
            addr_next = addr;
            nstate = VMU_Ctrl_Store;
          end
        end
      end
      VMU_Ctrl_Store:
      begin
        sdq_deq_rdy     = utaq_deq_val & srq_enq_rdy;
        utaq_deq_rdy    = sdq_deq_val  & srq_enq_rdy;
        srq_enq_val     = sdq_deq_val  & utaq_deq_val;
        if (sdq_deq_val & utaq_deq_val & srq_enq_rdy)
        begin
          if (vlen_reg == 0)
            nstate = VMU_Ctrl_Idle;
          else
            vlen_next = vlen_reg - 1'b1;
        end
      end
      VMU_Ctrl_AMO:
      begin
        sdq_deq_rdy     = utaq_deq_val & srq_enq_rdy  & roq_deq_tag_val;
        utaq_deq_rdy    = sdq_deq_val  & srq_enq_rdy  & roq_deq_tag_val;
        srq_enq_val     = sdq_deq_val  & utaq_deq_val & roq_deq_tag_val;
        roq_deq_tag_rdy = utaq_deq_val & srq_enq_rdy  & sdq_deq_val;
        if (sdq_deq_val & utaq_deq_val & roq_deq_tag_val & srq_enq_rdy)
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

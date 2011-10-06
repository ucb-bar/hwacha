`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"
`include "riscvConst.vh"

module vuVMU_Ctrl_ut_wb
(
  input clk,
  input reset,
  
  output reg               ldq_wb_done,
  input                    ldq_deq_rdy,

  input [`UT_WBCMD_SZ-1:0] wbcmdq_deq_bits,
  input                    wbcmdq_deq_val,
  output reg               wbcmdq_deq_rdy,

  // interface to reorder queue
  input [63:0]             roq_deq_bits,
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

  wire [`UTMCMD_VLEN_SZ-1:0]  vlen     = wbcmdq_deq_bits[`UTMCMD_VLEN_SZ-1:0];
  wire [3:0]                  cmd_type = wbcmdq_deq_bits[`UT_WBCMD_SZ-2:`UTMCMD_VLEN_SZ];
  wire                        cmd_type_amo = wbcmdq_deq_bits[`UT_WBCMD_SZ-1];
  reg  [`UTMCMD_VLEN_SZ-1:0]  vlen_reg, vlen_next;
  reg  [`UTMCMD_VLEN_SZ-1:0]  vlen_cnt_reg, vlen_cnt_next;
  reg  [3:0]                  cmd_type_reg, cmd_type_next;
  reg                         cmd_type_amo_reg, cmd_type_amo_next;

  reg [1:0] state, nstate;

  reg         buf_ldq_enq_val;
  wire        buf_ldq_enq_rdy;

  wire [63:0] delay_roq_deq_bits;
  wire [3:0]  delay_cmd_type;
  wire        delay_cmd_type_amo;

  `VC_PIPE1_QUEUE(64+4+1) skidbuf
  (
    .clk(clk),
    .reset(reset),

    .enq_bits({cmd_type_reg,cmd_type_amo_reg,roq_deq_bits}),
    .enq_val(buf_ldq_enq_val),
    .enq_rdy(buf_ldq_enq_rdy),

    .deq_bits({delay_cmd_type,delay_cmd_type_amo,delay_roq_deq_bits}),
    .deq_val(ldq_enq_val),
    .deq_rdy(ldq_enq_rdy)
  );
  
  wire          fp_cmd = delay_cmd_type_amo ? 1'b0 : delay_cmd_type[3];
  wire          signext = delay_cmd_type_amo ? 1'b0 : delay_cmd_type[2];
  wire [1:0]    bhwd_sel = delay_cmd_type[1:0];
  wire [63:0]   byte_sel_ext, hw_sel_ext, word_sel_ext;
  wire [64:0]   roq_deq_dp_bits;
  wire [32:0]   roq_deq_sp_bits;
    
  assign word_sel_ext = signext ? {32'd0, delay_roq_deq_bits[31:0]} : {{32{delay_roq_deq_bits[31]}}, delay_roq_deq_bits[31:0]};
  assign hw_sel_ext   = signext ? {48'd0, delay_roq_deq_bits[15:0]} : {{48{delay_roq_deq_bits[15]}}, delay_roq_deq_bits[15:0]};
  assign byte_sel_ext = signext ? {56'd0, delay_roq_deq_bits[7:0]}  : {{56{delay_roq_deq_bits[7]}}, delay_roq_deq_bits[7:0]};

  assign ldq_enq_bits = fp_cmd && (bhwd_sel == 2'b11) ? roq_deq_dp_bits :
                        fp_cmd && (bhwd_sel == 2'b10) ? {32'hFFFF_FFFF, roq_deq_sp_bits } :
                        (bhwd_sel == 2'b11)  ? {1'b0, delay_roq_deq_bits} : 
                        (bhwd_sel == 2'b10)  ? {1'b0, word_sel_ext} :
                        (bhwd_sel == 2'b01)  ? {1'b0, hw_sel_ext} :
                        (bhwd_sel == 2'b00)  ? {1'b0, byte_sel_ext} : 64'dx;

  floatNToRecodedFloatN #(8,24) recode_sp ( delay_roq_deq_bits[31:0], roq_deq_sp_bits );
  floatNToRecodedFloatN #(11,53) recode_dp ( delay_roq_deq_bits, roq_deq_dp_bits );

  always @(posedge clk)
  begin
    if (reset)
    begin
      state <= VMU_Ctrl_Idle;
      vlen_reg <= 0;
      vlen_cnt_reg <= 0;
      cmd_type_reg <= 0;
      cmd_type_amo_reg <= 0;
    end
    else
    begin
      state <= nstate;
      vlen_reg <= vlen_next;
      vlen_cnt_reg <= vlen_cnt_next;
      cmd_type_reg <= cmd_type_next;
      cmd_type_amo_reg <= cmd_type_amo_next;
    end
  end

  always @(*)
  begin
    vlen_next = vlen_reg;
    vlen_cnt_next = vlen_cnt_reg;
    cmd_type_next = cmd_type_reg;
    cmd_type_amo_next = cmd_type_amo_reg;
    wbcmdq_deq_rdy = 1'b0;
    roq_deq_rdy = 1'b0;
    buf_ldq_enq_val = 1'b0;
    ldq_wb_done = 1'b0;

    nstate = state;
    case (state)
      VMU_Ctrl_Idle:
      begin
        wbcmdq_deq_rdy = 1'b1;
        if (wbcmdq_deq_val)
        begin
          vlen_next = vlen;
          vlen_cnt_next = vlen;
          cmd_type_next = cmd_type;
          cmd_type_amo_next = cmd_type_amo;
          nstate = VMU_Ctrl_Writeback;
        end
      end
      VMU_Ctrl_Writeback:
      begin
        roq_deq_rdy = buf_ldq_enq_rdy;
        buf_ldq_enq_val = roq_deq_val;
        if (roq_deq_val & buf_ldq_enq_rdy)
        begin
          if (vlen_reg == 0)
            nstate = VMU_Ctrl_WritebackDone;
          else
            vlen_next = vlen_reg - 1'b1;
        end
        if (ldq_deq_rdy)
        begin
          vlen_cnt_next = vlen_cnt_reg - 1'b1;
        end
      end
      VMU_Ctrl_WritebackDone:
      begin
        ldq_wb_done = 1'b1;
        if (ldq_deq_rdy)
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

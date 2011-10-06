`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

`define CP_LDST_X         2'bxx
`define CP_LDST_W         2'd0
`define CP_LDST_B         2'd1
`define CP_LDST_HW        2'd2

module vuVMU_Ctrl_ut_top
(
  input clk,
  input reset,

  input  [`UTMCMD_SZ-1:0] utmcmdq_bits,
  input                   utmcmdq_val,
  output reg              utmcmdq_rdy,

  input  [`UTMIMM_SZ-1:0] utmimmq_bits,
  input                   utmimmq_val,
  output reg              utmimmq_rdy,

  output reg [`UTMRESP_SZ-1:0] utmrespq_bits,
  output reg             utmrespq_val,
  input                  utmrespq_rdy,

  output [`UT_ISCMD_SZ-1:0] iscmdq_enq_bits,
  output reg                  iscmdq_enq_val,
  input                       iscmdq_enq_rdy,

  output [`UT_WBCMD_SZ-1:0] wbcmdq_enq_bits,
  output reg                  wbcmdq_enq_val,
  input                       wbcmdq_enq_rdy,

  output [`UT_STCMD_SZ-1:0] stcmdq_enq_bits,
  output reg                  stcmdq_enq_val,
  input                       stcmdq_enq_rdy,

  input                     issue_busy,
  input                     store_busy
);

  localparam VMU_Ctrl_Idle          = 3'd0;
  localparam VMU_Ctrl_Load          = 3'd1;
  localparam VMU_Ctrl_Store         = 3'd2;
  localparam VMU_Ctrl_AMO           = 3'd3;
  localparam VMU_Ctrl_Sync          = 3'd4;
  localparam VMU_Ctrl_SyncWait      = 3'd5;
  localparam VMU_Ctrl_Invalid       = 3'd6;

  reg [2:0]     state, nstate;
  reg           error;
  reg           cmd_amo;

  wire [`UTMCMD_CMD_SZ-1:0]  cmd  = utmcmdq_bits[`UTMCMD_CMDCODE];
  wire [`UTMCMD_VLEN_SZ-1:0] vlen = utmcmdq_bits[`UTMCMD_VLEN_M1];
  wire [`UTMIMM_SZ-1:0]      addr = utmimmq_bits;

  assign iscmdq_enq_bits = {addr, vlen};
  assign wbcmdq_enq_bits = {cmd_amo, cmd[3:0], vlen};
  assign stcmdq_enq_bits = {cmd_amo, cmd[4:0], addr, vlen};

  always @(posedge clk)
  begin
    if (reset)
      state <= VMU_Ctrl_Idle;
    else
      state <= nstate;

`ifndef SYNTHESIS
    if (error)
      $display("ERROR in VMU_Ctrl_UT - Invalid command received!");

/*    if (state == VMU_Ctrl_Load)
      $display("Issued UT load : vlen == %d", vlen);
    if (state == VMU_Ctrl_Store)
      $display("Issued UT store : vlen == %d", vlen);
    if (state == VMU_Ctrl_AMO)
      $display("Issued UT AMO : vlen == %d", vlen);     */

`endif
  end

  always @(*)
  begin
    // set default values for all output signals
    utmcmdq_rdy          = 1'b0;
    utmimmq_rdy          = 1'b0;
    utmrespq_bits        = {`UTMRESP_SZ{1'b0}};
    utmrespq_val         = 1'b0;
    iscmdq_enq_val       = 1'b0;
    wbcmdq_enq_val       = 1'b0;
    stcmdq_enq_val       = 1'b0;
    cmd_amo              = 1'b0;
    error                = 1'b0;

    nstate = state;
    case (state)
      VMU_Ctrl_Idle:
        begin
          if (utmcmdq_val)
          begin
            case (cmd[7:4])
              4'b0_000:
                if (cmd[3:0] == 4'b1100 || cmd[3:0] == 4'b1101 || cmd[3:0] == 4'b1110 || cmd[3:0] == 4'b1111)
                  nstate = VMU_Ctrl_Sync;
                else
                  nstate = VMU_Ctrl_Invalid;
              4'b1_10_0: // UT load
                if (utmimmq_val & iscmdq_enq_rdy & wbcmdq_enq_rdy & ~store_busy)
                  nstate = VMU_Ctrl_Load;
              4'b1_10_1: // UT store
                if (utmimmq_val & stcmdq_enq_rdy & ~issue_busy)
                  nstate = VMU_Ctrl_Store;
              4'b1_11_0, 4'b1_11_1: // UT AMO
                if (stcmdq_enq_rdy & wbcmdq_enq_rdy & ~issue_busy)
                  nstate = VMU_Ctrl_AMO;
              default: nstate = VMU_Ctrl_Invalid;
            endcase
          end
        end
      VMU_Ctrl_Load:
        begin
          utmcmdq_rdy = 1'b1;
          utmimmq_rdy = 1'b1;
          iscmdq_enq_val = 1'b1;
          wbcmdq_enq_val = 1'b1;
          nstate = VMU_Ctrl_Idle;
        end
      VMU_Ctrl_Store:
        begin
          utmcmdq_rdy = 1'b1;
          utmimmq_rdy = 1'b1;
          stcmdq_enq_val = 1'b1;
          nstate = VMU_Ctrl_Idle;
        end
      VMU_Ctrl_AMO:
        begin
          utmcmdq_rdy = 1'b1;
//           utmimmq_rdy = 1'b1;
          stcmdq_enq_val = 1'b1;
          wbcmdq_enq_val = 1'b1;
          cmd_amo    = 1'b1;
          nstate = VMU_Ctrl_Idle;
        end
      VMU_Ctrl_Sync:
        begin
          if (~store_busy)
          begin
            utmcmdq_rdy = 1'b1;
            utmrespq_val = 1'b1;
            utmrespq_bits = 1;
            if (utmrespq_rdy)
              nstate = VMU_Ctrl_Idle;
            else
              nstate = VMU_Ctrl_SyncWait;
          end
        end
      VMU_Ctrl_SyncWait:
        begin
          utmrespq_val = 1'b1;
          utmrespq_bits = 1;
          if (utmrespq_rdy)
            nstate = VMU_Ctrl_Idle;
          else
            nstate = VMU_Ctrl_SyncWait;
        end
      VMU_Ctrl_Invalid:
        begin
          utmcmdq_rdy = 1'b1;
          error = 1'b1;
          nstate = VMU_Ctrl_Idle;
        end
//      default: nstate = VMU_Ctrl_Idle;
    endcase
  end

endmodule

`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVMU_Ctrl_vec_top
(
  input clk,
  input reset,

  input  [`VMCMD_SZ-1:0] vmcmdq_bits,
  input                  vmcmdq_val,
  output reg             vmcmdq_rdy,

  input  [`VMIMM_SZ-1:0] vmimmq_bits,
  input                  vmimmq_val,
  output reg             vmimmq_rdy,

  input  [`VMSTRIDE_SZ-1:0] vmstrideq_bits,
  input                  vmstrideq_val,
  output reg             vmstrideq_rdy,

  output reg [`VMRESP_SZ-1:0] vmrespq_bits,
  output reg             vmrespq_val,
  input                  vmrespq_rdy,

  output [`VM_ISCMD_SZ-1:0] iscmdq_bits,
  output reg                iscmdq_val,
  input                     iscmdq_rdy,

  output [`VM_WBCMD_SZ-1:0] wbcmdq_bits,
  output reg                wbcmdq_val,
  input                     wbcmdq_rdy,

  output [`VM_STCMD_SZ-1:0] stcmdq_bits,
  output reg                stcmdq_val,
  input                     stcmdq_rdy,

  input                     store_busy
);

  localparam VMU_Ctrl_Idle          = 3'd0;
  localparam VMU_Ctrl_Load          = 3'd1;
  localparam VMU_Ctrl_LoadStride    = 3'd2;
  localparam VMU_Ctrl_Store         = 3'd3;
  localparam VMU_Ctrl_StoreStride   = 3'd4;
  localparam VMU_Ctrl_Sync          = 3'd5;
  localparam VMU_Ctrl_SyncWait      = 3'd6;
  localparam VMU_Ctrl_Invalid       = 3'd7;

  reg [2:0]     state, nstate;
  reg           error;

  wire [`VMCMD_CMD_SZ-1:0]  cmd  = vmcmdq_bits[`VMCMD_CMDCODE];

  wire [`VMCMD_VLEN_SZ-1:0] vlen = vmcmdq_bits[`VMCMD_VLEN_M1];
  wire [`VMIMM_SZ-1:0]      addr = vmimmq_bits;
  reg  [`VMSTRIDE_SZ-1:0]   stride;

  assign wbcmdq_bits = {cmd[3:0], stride, addr, vlen};
  assign iscmdq_bits = {stride, addr, vlen};
  assign stcmdq_bits = {cmd[3:0], stride, addr, vlen};

  always @(posedge clk)
  begin
    if (reset)
      state <= VMU_Ctrl_Idle;
    else
      state <= nstate;

`ifndef SYNTHESIS
    if (error)
      $display("ERROR in VMU_Ctrl_Vec - Invalid command received!");

/*    if (state == VMU_Ctrl_Load)
      $display("Issued unit stride vector load : vlen == %d", vlen);
    if (state == VMU_Ctrl_LoadStride)
      $display("Issued strided vector load : vlen == %d", vlen);
    if (state == VMU_Ctrl_Store)
      $display("Issued unit stride vector store : vlen == %d", vlen);
    if (state == VMU_Ctrl_StoreStride)
      $display("Issued strided vector store : vlen == %d", vlen);     */

`endif
  end

  always @(*)
  begin
    // set default values for all output signals
    vmcmdq_rdy          = 1'b0;
    vmimmq_rdy          = 1'b0;
    vmstrideq_rdy       = 1'b0;
    vmrespq_bits        = {`VMRESP_SZ{1'b0}};
    vmrespq_val         = 1'b0;
    iscmdq_val          = 1'b0;
    wbcmdq_val          = 1'b0;
    stcmdq_val          = 1'b0;
    stride              = 32'd0;
    error               = 1'b0;

    nstate = state;
    case (state)
      VMU_Ctrl_Idle:
        begin
          if (vmcmdq_val)
          begin
            case (cmd[7:4])
              4'b0_000:
                if (cmd[3:0] == 4'b1100 || cmd[3:0] == 4'b1101 || cmd[3:0] == 4'b1110 || cmd[3:0] == 4'b1111)
                  nstate = VMU_Ctrl_Sync;
                else
                  nstate = VMU_Ctrl_Invalid;
              4'b1_00_0: // standard vector load
                if (vmimmq_val & iscmdq_rdy & wbcmdq_rdy)
                  nstate = VMU_Ctrl_Load;
              4'b1_00_1: // standard vector store
                if (vmimmq_val & stcmdq_rdy)
                  nstate = VMU_Ctrl_Store;
              4'b1_01_0: // strided vector load
                if (vmimmq_val & vmstrideq_val & iscmdq_rdy & wbcmdq_rdy)
                  nstate = VMU_Ctrl_LoadStride;
              4'b1_01_1: // strided vector store
                if (vmimmq_val & vmstrideq_val & stcmdq_rdy)
                  nstate = VMU_Ctrl_StoreStride;
              default: nstate = VMU_Ctrl_Invalid;
            endcase
          end
        end
      VMU_Ctrl_Load:
        begin
          vmcmdq_rdy = 1'b1;
          vmimmq_rdy = 1'b1;
          iscmdq_val = 1'b1;
          wbcmdq_val = 1'b1;
          case (cmd[1:0])
            2'b11 : stride = 32'd8;
            2'b10 : stride = 32'd4;
            2'b01 : stride = 32'd2;
            2'b00 : stride = 32'd1;
            default : stride = 32'd0;
          endcase
          nstate = VMU_Ctrl_Idle;
        end
      VMU_Ctrl_Store:
        begin
          vmcmdq_rdy = 1'b1;
          vmimmq_rdy = 1'b1;
          stcmdq_val = 1'b1;
          case (cmd[1:0])
            2'b11 : stride = 32'd8;
            2'b10 : stride = 32'd4;
            2'b01 : stride = 32'd2;
            2'b00 : stride = 32'd1;
            default : stride = 32'd0;
          endcase
          nstate = VMU_Ctrl_Idle;
        end
      VMU_Ctrl_LoadStride:
        begin
          vmcmdq_rdy   = 1'b1;
          vmimmq_rdy   = 1'b1;
          vmstrideq_rdy = 1'b1;
          iscmdq_val = 1'b1;
          wbcmdq_val = 1'b1;
          stride = vmstrideq_bits;
          nstate = VMU_Ctrl_Idle;
        end
      VMU_Ctrl_StoreStride:
        begin
          vmcmdq_rdy   = 1'b1;
          vmimmq_rdy   = 1'b1;
          vmstrideq_rdy = 1'b1;
          stcmdq_val   = 1'b1;
          stride = vmstrideq_bits;
          nstate = VMU_Ctrl_Idle;
        end
      VMU_Ctrl_Sync:
        begin
          if (~store_busy)
          begin
            vmcmdq_rdy = 1'b1;
            vmrespq_val = 1'b1;
            vmrespq_bits = 1;
            if (vmrespq_rdy)
              nstate = VMU_Ctrl_Idle;
            else
              nstate = VMU_Ctrl_SyncWait;
          end
        end
      VMU_Ctrl_SyncWait:
        begin
          vmrespq_val = 1'b1;
          vmrespq_bits = 1;
          if (vmrespq_rdy)
            nstate = VMU_Ctrl_Idle;
          else
            nstate = VMU_Ctrl_SyncWait;
        end
      VMU_Ctrl_Invalid:
        begin
          vmcmdq_rdy = 1'b1;
          error = 1'b1;
          nstate = VMU_Ctrl_Idle;
        end
//      default: nstate = VMU_Ctrl_Idle;
    endcase
  end

endmodule


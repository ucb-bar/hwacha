`include "macros.vh"
`include "riscvConst.vh"
`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVMU_Ctrl_vec_store
(
  input clk,
  input reset,

  input [`VM_STCMD_SZ-1:0] stcmdq_deq_bits,
  input                     stcmdq_deq_val,
  output reg                stcmdq_deq_rdy,

  input [64:0]              sdq_deq_bits,
  input                     sdq_deq_val,
  output reg                sdq_deq_rdy,

  output [27:0]             srq_enq_addr_bits,
  output [127:0]            srq_enq_data_bits,
  output [15:0]             srq_enq_wmask_bits,
  output reg                srq_enq_val,
  input                     srq_enq_rdy,

  output                    store_busy
);

  localparam VMU_Ctrl_Idle          = 2'd0;
  localparam VMU_Ctrl_Store         = 2'd1;
  localparam VMU_Ctrl_StoreWait     = 2'd2;

  wire [`VMCMD_VLEN_SZ-1:0] vlen     = stcmdq_deq_bits[`VMCMD_VLEN_SZ-1:0];
  wire [`VMIMM_SZ-1:0]      addr     = stcmdq_deq_bits[`VMIMM_SZ+`VMCMD_VLEN_SZ-1:`VMCMD_VLEN_SZ];
  wire [`VMSTRIDE_SZ-1:0]   stride   = stcmdq_deq_bits[`VMSTRIDE_SZ+`VMIMM_SZ+`VMCMD_VLEN_SZ-1:`VMIMM_SZ+`VMCMD_VLEN_SZ];
  wire [3:0]                cmd_type = stcmdq_deq_bits[`VM_STCMD_SZ-1:`VM_STCMD_SZ-4];

  reg [3:0] cmd_type_reg, cmd_type_next;
  reg [31:0] addr_reg, addr_next, stride_reg, stride_next;
  wire [31:0] addr_incr;
  reg [`VMCMD_VLEN_SZ-1:0]  vlen_reg, vlen_next;
  reg [1:0] state, nstate;

  reg                           sbuf_enq_val;
  wire [15:0]                   sbuf_byte_enq_val; //, sbuf_byte_deq_val;
  wire [63:0]                   store_data;
  wire [63:0]                   sdq_deq_dp;
  wire [31:0]                   sdq_deq_sp;
  reg  [7:0]                    store_data_wmask;

  wire fp_cmd = cmd_type_reg[3];

  recodedFloatNToFloatN #(8,24) decoder_sp ( sdq_deq_bits[32:0], sdq_deq_sp );
  recodedFloatNToFloatN #(11,53) decoder_dp ( sdq_deq_bits, sdq_deq_dp );

  assign store_busy = ~((state == VMU_Ctrl_Idle) & ~stcmdq_deq_val);

  assign store_data = fp_cmd && (cmd_type_reg[1:0] == 2'b11) ? sdq_deq_dp :
                      fp_cmd && (cmd_type_reg[1:0] == 2'b10) ? {2{sdq_deq_sp}} :
                      (cmd_type_reg[1:0] == 2'b11) ? {sdq_deq_bits[63:0]} :
                      (cmd_type_reg[1:0] == 2'b10) ? {2{sdq_deq_bits[31:0]}} :
                      (cmd_type_reg[1:0] == 2'b01) ? {4{sdq_deq_bits[15:0]}} :
                      (cmd_type_reg[1:0] == 2'b00) ? {8{sdq_deq_bits[7:0]}} : 64'd0;

  assign srq_enq_addr_bits = addr_reg[31:4];
//  assign srq_enq_wmask_bits = sbuf_byte_deq_val;

  always @(posedge clk)
  begin
    if (reset)
    begin
      state <= VMU_Ctrl_Idle;
      addr_reg <= 0;
      vlen_reg <= 0;
      stride_reg <= 0;
      cmd_type_reg <= 0;
    end
    else
    begin
      state <= nstate;
      addr_reg <= addr_next;
      vlen_reg <= vlen_next;
      stride_reg <= stride_next;
      cmd_type_reg <= cmd_type_next;
    end


  end

// `ifndef SYNTHESIS
//   always @(posedge clk)
//   begin
//     if (state == VMU_Ctrl_StoreWait)
//       $display("Writing value %f to address %08x", store_data, addr_reg);
//   end
// `endif
//

  assign addr_incr = addr_reg + stride_reg;

  always @(*)
  begin
    addr_next = addr_reg;
    vlen_next = vlen_reg;
    stride_next = stride_reg;
    cmd_type_next = cmd_type_reg;
    stcmdq_deq_rdy = 1'b0;
    sdq_deq_rdy = 1'b0;
    srq_enq_val = 1'b0;
    sbuf_enq_val = 1'b0;

    nstate = state;

    case (cmd_type_reg[1:0])
      2'b11: store_data_wmask = 8'b11111111;
      2'b10: store_data_wmask = addr_reg[2] ? 8'b11110000 : 8'b00001111;
      2'b01: case (addr_reg[2:1])
               2'b00 : store_data_wmask = 8'b00000011;
               2'b01 : store_data_wmask = 8'b00001100;
               2'b10 : store_data_wmask = 8'b00110000;
               2'b11 : store_data_wmask = 8'b11000000;
             endcase
      2'b00: case (addr_reg[2:0])
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
          addr_next = addr;
          stride_next = stride;
          vlen_next = vlen;
          cmd_type_next = cmd_type;
          nstate = VMU_Ctrl_Store;
        end
      end
      VMU_Ctrl_Store:
      begin
        sdq_deq_rdy = 1'b1;
        if (sdq_deq_val)
        begin
          sbuf_enq_val = 1'b1;
          if (vlen_reg == 0)
          begin
            srq_enq_val = 1'b1;
            if (srq_enq_rdy)
              nstate = VMU_Ctrl_Idle;
            else
              nstate = VMU_Ctrl_StoreWait;
          end
          else if (addr_incr[31:4] != addr_reg[31:4])
          begin
            srq_enq_val = 1'b1;
            if (srq_enq_rdy)
            begin
              addr_next = addr_reg + stride_reg;
              vlen_next = vlen_reg - 1'b1;
            end
            else
              nstate = VMU_Ctrl_StoreWait;
          end
          else
          begin
            addr_next = addr_incr;
            vlen_next = vlen_reg - 1'b1;
          end
        end
      end
      VMU_Ctrl_StoreWait:
      begin
        srq_enq_val = 1'b1;
        if (srq_enq_rdy)
        begin
          if (vlen_reg == 0)
            nstate = VMU_Ctrl_Idle;
          else
          begin
            addr_next = addr_incr;
            vlen_next = vlen_reg - 1'b1;
            nstate = VMU_Ctrl_Store;
          end
        end
      end
      default:;
    endcase
  end

  assign sbuf_byte_enq_val = sbuf_enq_val ? 
                             (addr_reg[3] ? {store_data_wmask, 8'h0} : {8'h0, store_data_wmask}) : 16'd0;  
                             
  genvar i,j;
  generate
    for (i=0;i<2;i=i+1)
    begin : vmu_vec_sbuf
      for (j=0;j<8;j=j+1)
      begin : sbuf64
        `VC_FLOW_QUEUE(8,2) sbuf
        (
          .clk            (clk),
          .reset          (reset),
          .enq_bits       (store_data[((j+1)*8)-1:(j*8)]),
          .enq_val        (sbuf_byte_enq_val[i*8+j]),
          .enq_rdy        (),
          .deq_bits       (srq_enq_data_bits[((j+1)*8+i*64)-1:(j*8+i*64)]),
          .deq_val        (srq_enq_wmask_bits[i*8+j]),
          .deq_rdy        (srq_enq_val & srq_enq_rdy)
        );
      end
    end
  endgenerate

endmodule

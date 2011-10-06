`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVMU_ROQ_tag #(
    parameter ROQ_DATA_SIZE   = 128,
    parameter ROQ_TAG_ENTRIES = 8,
    parameter ROQ_TAG_SIZE    = 3
)
(
 input clk,
 input reset,

 // interface to responses from D$
 input [ROQ_DATA_SIZE-1:0]  roq_enq_data_bits,
 input [ROQ_TAG_SIZE-1:0]   roq_enq_tag_bits,
 input                      roq_enq_val,

 // interface to writeback control
 output [ROQ_DATA_SIZE-1:0] roq_deq_data_bits,
 output reg                 roq_deq_data_val,
 input                      roq_deq_data_rdy

);

  reg  [ROQ_TAG_SIZE-1:0]    read_ptr; //, roq_enq_tag_bits_reg;
  wire [ROQ_TAG_SIZE-1:0]    read_ptr_next;
  wire                       roq_data_deq;
  reg  [ROQ_TAG_ENTRIES-1:0] vb_array; // valid bit array

  assign read_ptr_next    = read_ptr + 1'b1;
  assign roq_data_deq     = roq_deq_data_rdy & roq_deq_data_val;

  //generate
  //  begin : roq_data_array_32
  //    SRAM_32x32_2P data_array
  //    (
  //      .CE1            (clk),
  //      .OEB1           (~roq_deq_data_val),
  //      .CSB1           (1'b0),
  //      .A1             (roq_data_deq ? read_ptr_next : read_ptr),
  //      .O1             (roq_deq_data_bits),
  //      .CE2            (clk),
  //      .WEB2           (~roq_enq_val),
  //      .CSB2           (1'b0),
  //      .A2             (roq_enq_tag_bits),
  //      .I2             (roq_enq_data_bits)
  //    );
  //  end
  //endgenerate

  reg [ROQ_DATA_SIZE-1:0] data [0:ROQ_TAG_ENTRIES-1];
  wire roq_deq_data_val_pre;

  assign roq_deq_data_bits = data[read_ptr];
  //assign roq_deq_data_val = 1'b1;
  assign roq_deq_data_val_pre  = reset ? 1'b0 : (roq_data_deq ? vb_array[read_ptr_next] : vb_array[read_ptr]);

  always @(posedge clk)
  begin
    if (reset)
    begin
      read_ptr  <= 0;
      vb_array  <= 0;
      roq_deq_data_val <= 0;
    end
    else
    begin
      roq_deq_data_val <= roq_deq_data_val_pre;
      if (roq_data_deq)
      begin
        vb_array[read_ptr] <= 1'b0;
        read_ptr  <= read_ptr_next;
      end
      if (roq_enq_val)
      begin
        vb_array[roq_enq_tag_bits] <= 1'b1;
        data[roq_enq_tag_bits] <= roq_enq_data_bits;
      end
    end
  end

endmodule


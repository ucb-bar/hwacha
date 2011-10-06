`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVMU_ROQ #(
    parameter ROQ_DATA_SIZE   = 128,
    parameter ROQ_TAG_ENTRIES = 8,
    parameter ROQ_TAG_SIZE    = 3
)
(
 input clk,
 input reset,

 // interface to issue control
 output [ROQ_TAG_SIZE-1:0]   roq_deq_tag_bits,
 output                       roq_deq_tag_val,
 input                        roq_deq_tag_rdy,

 // interface to responses from D$
 input [ROQ_DATA_SIZE-1:0]    roq_enq_data_bits,
 input [ROQ_TAG_SIZE-1:0]     roq_enq_tag_bits,
 input                        roq_enq_val,

 // interface to writeback control
 output [ROQ_DATA_SIZE-1:0]   roq_deq_data_bits,
 output reg                   roq_deq_data_val,
 input                        roq_deq_data_rdy

);

  reg [ROQ_TAG_SIZE-1:0]     read_ptr, write_ptr;
  wire [ROQ_TAG_SIZE-1:0]    read_ptr_next, write_ptr_next;
  wire                       full;
  reg [ROQ_TAG_ENTRIES-1:0]  vb_array; // valid bit array
  wire                       roq_data_deq, roq_tag_deq;
  wire                       roq_deq_data_val_pre;

  assign full              = (write_ptr_next == read_ptr);
  assign roq_deq_tag_val   = reset ? 1'b0 : ~full;
  assign roq_deq_tag_bits  = write_ptr;
  assign roq_deq_data_val_pre  = reset ? 1'b0 : (roq_data_deq ? vb_array[read_ptr_next] : vb_array[read_ptr]);
  assign read_ptr_next  = read_ptr + 1'b1;
  assign write_ptr_next = write_ptr + 1'b1;
  assign roq_data_deq   = roq_deq_data_rdy & roq_deq_data_val;
  assign roq_tag_deq    = roq_deq_tag_rdy & roq_deq_tag_val;

`ifdef ASIC 
  generate
    if (ROQ_DATA_SIZE == 65 && ROQ_TAG_ENTRIES == 256)
    begin
      SRAM8T_65x256 data_array
      (
        .clk(clk),
        .read(roq_deq_data_val_pre),
        .raddr(roq_data_deq ? read_ptr_next : read_ptr),
        .dout(roq_deq_data_bits),
        .write(roq_enq_val),
        .waddr(roq_enq_tag_bits),
        .din(roq_enq_data_bits)
      );
    end
    else if (ROQ_DATA_SIZE == 130 && ROQ_TAG_ENTRIES == 256)
    begin
      SRAM8T_65x256 data_array1
      (
        .clk(clk),
        .read(roq_deq_data_val_pre),
        .raddr(roq_data_deq ? read_ptr_next : read_ptr),
        .dout(roq_deq_data_bits[64:0]),
        .write(roq_enq_val),
        .waddr(roq_enq_tag_bits),
        .din(roq_enq_data_bits[64:0])
      );

      SRAM8T_65x256 data_array2
      (
        .clk(clk),
        .read(roq_deq_data_val_pre),
        .raddr(roq_data_deq ? read_ptr_next : read_ptr),
        .dout(roq_deq_data_bits[129:65]),
        .write(roq_enq_val),
        .waddr(roq_enq_tag_bits),
        .din(roq_enq_data_bits[129:65])
      );
    end
    else
    begin
      sram_1r1w#(ROQ_DATA_SIZE,ROQ_TAG_SIZE,ROQ_DATA_SIZE) data_array
      (
        .CE1            (clk),
        .OEB1           (~roq_deq_data_val),
        .CSB1           (1'b0),
        .A1             (roq_data_deq ? read_ptr_next : read_ptr),
        .O1             (roq_deq_data_bits),
        .CE2            (clk),
        .WEB2           (~roq_enq_val),
        .CSB2           (1'b0),
        .A2             (roq_enq_tag_bits),
        .BM2            (1'b1),
        .I2             (roq_enq_data_bits)
      );
    end
  endgenerate
`else
  sram_1r1w#(ROQ_DATA_SIZE,ROQ_TAG_SIZE,ROQ_DATA_SIZE) data_array
  (
    .CE1            (clk),
    .OEB1           (~roq_deq_data_val),
    .CSB1           (1'b0),
    .A1             (roq_data_deq ? read_ptr_next : read_ptr),
    .O1             (roq_deq_data_bits),
    .CE2            (clk),
    .WEB2           (~roq_enq_val),
    .CSB2           (1'b0),
    .A2             (roq_enq_tag_bits),
    .BM2            (1'b1),
    .I2             (roq_enq_data_bits)
  );
`endif
  always @(posedge clk)
  begin
    if (reset)
    begin
      read_ptr  <= 0;
      write_ptr <= 0;
      vb_array  <= 0;
      roq_deq_data_val <= 0;
    end
    else
    begin
      roq_deq_data_val <= roq_deq_data_val_pre;
      // read tag
      if (roq_tag_deq)
      begin
        write_ptr <= write_ptr_next;
      end

      // read data
      if (roq_data_deq)
      begin
        vb_array[read_ptr] <= 1'b0;
        read_ptr  <= read_ptr_next;
      end

      // write data
      if (roq_enq_val)
      begin
        vb_array[roq_enq_tag_bits] <= 1'b1;
      end
    end
  end

endmodule


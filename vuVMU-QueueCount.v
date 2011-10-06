`include "macros.vh"
`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVMU_QueueCount
#(
  parameter reset_cnt = 0,
  parameter ready_cnt = 8,
  parameter max_cnt = 12
)
(
  input clk,
  input reset,
  
  input enq,
  input deq,
  
  output ready
);
  
  reg [`ceilLog2(max_cnt):0] count;
  
  assign ready = (count >= ready_cnt);
    
  always @(posedge clk)
  begin
    if (reset)
      count <= reset_cnt;
    else
    begin
      if (enq ^ deq)
      begin
        if (enq)
          count <= count + 1;
        else
          count <= count - 1;
      end
    end
  end
  
`ifndef SYNTHESIS
  always @(posedge clk)
  begin
    if (~reset)
    begin
      if (enq ^ deq)
      begin
        if (enq & (count == max_cnt))
          $display("%t : vuVMU_QueueCount : Error - attempting to enqueue when queue is full!", $time);
        if (deq & (count == 0))
          $display("%t : vuVMU_QueueCount : Error - attempting to dequeue when queue is empty!", $time);
      end
    end
  end
`endif
  
endmodule

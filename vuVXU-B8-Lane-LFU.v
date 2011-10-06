`include "interface.vh"
`include "vuVXU-B8-Config.vh"
`include "vuVXU-Opcode.vh"

module vuVXU_Banked8_Lane_LFU
(
  input clk,
  input reset,

  input  `DEF_BVLEN   expand_rcnt,
  input  `DEF_BVLEN   expand_wcnt,

  input               expand_vau0,
  input  `DEF_VAU0_FN expand_vau0_fn,
  input               expand_vau1,
  input  `DEF_VAU1_FN expand_vau1_fn,
  input               expand_vau2,
  input  `DEF_VAU2_FN expand_vau2_fn,
  input               expand_vldq,
  input               expand_vsdq,
  input               expand_utaq,
  input               expand_utldq,
  input               expand_utsdq,

  output              vau0_val,
  output `DEF_VAU0_FN vau0_fn,
  output              vau1_val,
  output `DEF_VAU1_FN vau1_fn,
  output              vau2_val,
  output `DEF_VAU2_FN vau2_fn,
  output              vldq_rdy,
  output              vsdq_val,
  output              utaq_val,
  output              utldq_rdy,
  output              utsdq_val
);

  reg `DEF_BVLEN next_vau0_cnt;
  reg `DEF_BVLEN next_vau1_cnt;
  reg `DEF_BVLEN next_vau2_cnt;
  reg `DEF_BVLEN next_vgu_cnt;
  reg `DEF_BVLEN next_vlu_cnt;
  reg `DEF_BVLEN next_vsu_cnt;

  reg `DEF_BVLEN reg_vau0_cnt;
  reg `DEF_BVLEN reg_vau1_cnt;
  reg `DEF_BVLEN reg_vau2_cnt;
  reg `DEF_BVLEN reg_vgu_cnt;
  reg `DEF_BVLEN reg_vlu_cnt;
  reg `DEF_BVLEN reg_vsu_cnt;

  always @(posedge clk)
  begin
    if (reset)
    begin
      reg_vau0_cnt <= `SZ_BVLEN'd0;
      reg_vau1_cnt <= `SZ_BVLEN'd0;
      reg_vau2_cnt <= `SZ_BVLEN'd0;
      reg_vgu_cnt <= `SZ_BVLEN'd0;
      reg_vlu_cnt <= `SZ_BVLEN'd0;
      reg_vsu_cnt <= `SZ_BVLEN'd0;
    end
    else
    begin
      reg_vau0_cnt <= next_vau0_cnt;
      reg_vau1_cnt <= next_vau1_cnt;
      reg_vau2_cnt <= next_vau2_cnt;
      reg_vgu_cnt <= next_vgu_cnt;
      reg_vlu_cnt <= next_vlu_cnt;
      reg_vsu_cnt <= next_vsu_cnt;
    end
  end

  always @(*)
  begin
    next_vau0_cnt = `SZ_BVLEN'd0;
    next_vau1_cnt = `SZ_BVLEN'd0;
    next_vau2_cnt = `SZ_BVLEN'd0;
    next_vgu_cnt = `SZ_BVLEN'd0;
    next_vlu_cnt = `SZ_BVLEN'd0;
    next_vsu_cnt = `SZ_BVLEN'd0;

    if (expand_vau0) next_vau0_cnt = expand_rcnt;
    if (expand_vau1) next_vau1_cnt = expand_rcnt;
    if (expand_vau2) next_vau2_cnt = expand_rcnt;
    if (expand_utaq) next_vgu_cnt = expand_rcnt;
    if (expand_vldq || expand_utldq) next_vlu_cnt = expand_wcnt;
    if (expand_vsdq || expand_utsdq) next_vsu_cnt = expand_rcnt;

    if (|reg_vau0_cnt) next_vau0_cnt = reg_vau0_cnt - 1'b1;
    if (|reg_vau1_cnt) next_vau1_cnt = reg_vau1_cnt - 1'b1;
    if (|reg_vau2_cnt) next_vau2_cnt = reg_vau2_cnt - 1'b1;
    if (|reg_vgu_cnt) next_vgu_cnt = reg_vgu_cnt - 1'b1;
    if (|reg_vlu_cnt) next_vlu_cnt = reg_vlu_cnt - 1'b1;
    if (|reg_vsu_cnt) next_vsu_cnt = reg_vsu_cnt - 1'b1;
  end

  reg              reg_vau0;
  reg `DEF_VAU0_FN reg_vau0_fn;
  reg              reg_vau1;
  reg `DEF_VAU1_FN reg_vau1_fn;
  reg              reg_vau2;
  reg `DEF_VAU2_FN reg_vau2_fn;
  reg              reg_vldq;
  reg              reg_vsdq;
  reg              reg_utaq;
  reg              reg_utldq;
  reg              reg_utsdq;

  always @(posedge clk)
  begin
    if (reset)
    begin
      reg_vau0 <= 1'b0;
      reg_vau1 <= 1'b0;
      reg_vau2 <= 1'b0;
      reg_utaq <= 1'b0;
      reg_vldq <= 1'b0;
      reg_vsdq <= 1'b0;
      reg_utldq <= 1'b0;
      reg_utsdq <= 1'b0;
    end
    else
    begin
      if (expand_vau0)
      begin
        reg_vau0 <= 1'b1;
        reg_vau0_fn <= expand_vau0_fn;
      end
      else if (~(|reg_vau0_cnt))
        reg_vau0 <= 1'b0;

      if (expand_vau1)
      begin
        reg_vau1 <= 1'b1;
        reg_vau1_fn <= expand_vau1_fn;
      end
      else if (~(|reg_vau1_cnt))
        reg_vau1 <= 1'b0;

      if (expand_vau2)
      begin
        reg_vau2 <= 1'b1;
        reg_vau2_fn <= expand_vau2_fn;
      end
      else if (~(|reg_vau2_cnt))
        reg_vau2 <= 1'b0;

      if (expand_utaq)
        reg_utaq <= 1'b1;
      else if (~(|reg_vgu_cnt))
        reg_utaq <= 1'b0;

      if ((expand_vldq || expand_utldq) && (|expand_wcnt))
      begin
        reg_vldq <= expand_vldq;
        reg_utldq <= expand_utldq;
      end
      else if (~(|next_vlu_cnt))
      begin
        reg_vldq <= 1'b0;
        reg_utldq <= 1'b0;
      end

      if (expand_vsdq || expand_utsdq)
      begin
        reg_vsdq <= expand_vsdq;
        reg_utsdq <= expand_utsdq;
      end
      else if (~(|reg_vsu_cnt))
      begin
        reg_vsdq <= 1'b0;
        reg_utsdq <= 1'b0;
      end
    end
  end

  // every signal related to the read port is delayed by one cycle 
  // because of the register file is an sram
  // for this reason vldq_rdy, utldq_rdy needs to be bypassed
  // and count down using the next_vlu_cnt signal

  assign vau0_val = reg_vau0;
  assign vau0_fn = reg_vau0_fn;
  assign vau1_val = reg_vau1;
  assign vau1_fn = reg_vau1_fn;
  assign vau2_val = reg_vau2;
  assign vau2_fn = reg_vau2_fn;
  assign utaq_val = reg_utaq;
  assign vldq_rdy = expand_vldq | reg_vldq;
  assign utldq_rdy = expand_utldq | reg_utldq;
  assign vsdq_val = reg_vsdq;
  assign utsdq_val = reg_utsdq;

endmodule

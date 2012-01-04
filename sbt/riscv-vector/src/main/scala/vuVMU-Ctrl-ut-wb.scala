package riscvVector {
  import Chisel._
  import Node._
  import Interface._
  import Fpu._
  import queues._

  class vuVMU_Ctrl_ut_wbIO extends Bundle
  {
    // interface to load data queue
    val ldq = new utldqIO();

    val wbcmdq_deq_bits = Bits(UT_WBCMD_SZ, 'input);
    val wbcmdq_deq_val  = Bool('input);
    val wbcmdq_deq_rdy  = Bool('output);

    // interface to reorder queue
    val roq_deq_bits    = Bits(64, 'input);
    val roq_deq_val     = Bool('input);
    val roq_deq_rdy     = Bool('output);
  }

  class vuVMU_Ctrl_ut_wb extends Component
  {
    val io = new vuVMU_Ctrl_ut_wbIO();

    val VMU_Ctrl_Idle           = Bits(0,2);
    val VMU_Ctrl_Writeback      = Bits(1,2);
    val VMU_Ctrl_WritebackDone  = Bits(2,2);

    val vlen             = io.wbcmdq_deq_bits(UTMCMD_VLEN_SZ-1, 0).toUFix;
    val cmd_type         = io.wbcmdq_deq_bits(UT_WBCMD_SZ-2, UTMCMD_VLEN_SZ);
    val cmd_type_amo     = io.wbcmdq_deq_bits(UT_WBCMD_SZ-1).toBool;
    val vlen_reg         = Reg(resetVal = UFix(0, UTMCMD_VLEN_SZ));
    val vlen_cnt_reg     = Reg(resetVal = UFix(0, UTMCMD_VLEN_SZ));
    val cmd_type_reg     = Reg(resetVal = Bits(0, 4));
    val cmd_type_amo_reg = Reg(resetVal = Bool(false));

    val state = Reg(resetVal = VMU_Ctrl_Idle);

    val buf_ldq_enq_val     = Wire(){Bool()};
    val buf_ldq_enq_rdy     = Wire(){Bool()};
    val delay_roq_deq_bits  = Wire(){Bits(width=64)};
    val delay_cmd_type      = Wire(){Bits(width=4)};
    val delay_cmd_type_amo  = Wire(){Bool()};
    
    val skidbuf = new queuePipe1PF(64+4+1);
    skidbuf.io.enq_bits := Cat(cmd_type_reg,cmd_type_amo_reg,io.roq_deq_bits);
    skidbuf.io.enq_val  := buf_ldq_enq_val;
    buf_ldq_enq_rdy     := skidbuf.io.enq_rdy;
    delay_cmd_type      := skidbuf.io.deq_bits(68,65);
    delay_cmd_type_amo  := skidbuf.io.deq_bits(64).toBool;
    delay_roq_deq_bits  := skidbuf.io.deq_bits(63,0);
    io.ldq.enq_val      := skidbuf.io.deq_val;
    skidbuf.io.deq_rdy  := io.ldq.enq_rdy;

    val fp_cmd = Mux(delay_cmd_type_amo, Bool(false), delay_cmd_type(3).toBool);
    val signext = Mux(delay_cmd_type_amo, Bool(false), delay_cmd_type(2).toBool);
    val bhwd_sel = delay_cmd_type(1,0);
    val roq_deq_dp_bits = Wire(){Bits(width=65)};
    val roq_deq_sp_bits = Wire(){Bits(width=33)};

    val word_sel_ext = Mux(signext,
                          Cat(Bits(0,32), delay_roq_deq_bits(31,0)),
                          Cat(Fill(32,delay_roq_deq_bits(31)), delay_roq_deq_bits(31,0)));
    val hw_sel_ext = Mux(signext,
                          Cat(Bits(0,48), delay_roq_deq_bits(15,0)),
                          Cat(Fill(48,delay_roq_deq_bits(15)), delay_roq_deq_bits(15,0)));
    val byte_sel_ext = Mux(signext,
                          Cat(Bits(0,56), delay_roq_deq_bits(7,0)),
                          Cat(Fill(56,delay_roq_deq_bits(7)), delay_roq_deq_bits(7,0)));

    io.ldq.enq_bits := MuxCase(
      Bits(0,64), Array(
        (fp_cmd && bhwd_sel === Bits("b11",2)) -> roq_deq_dp_bits,
        (fp_cmd && bhwd_sel === Bits("b10",2)) -> Cat(Bits("hFFFF_FFFF",32), roq_deq_sp_bits),
        (bhwd_sel === Bits("b11")) -> Cat(Bits(0,1), delay_roq_deq_bits),
        (bhwd_sel === Bits("b10")) -> Cat(Bits(0,1), word_sel_ext),
        (bhwd_sel === Bits("b01")) -> Cat(Bits(0,1), hw_sel_ext),
        (bhwd_sel === Bits("b00")) -> Cat(Bits(0,1), byte_sel_ext)
      ));

    val recode_sp = new float32ToRecodedFloat32();
    recode_sp.io.in := delay_roq_deq_bits(31,0);
    roq_deq_sp_bits := recode_sp.io.out;
    val recode_dp = new float64ToRecodedFloat64();
    recode_dp.io.in := delay_roq_deq_bits;
    roq_deq_dp_bits := recode_dp.io.out;

    switch(state)
    {
      is(VMU_Ctrl_Idle)
      {
        io.wbcmdq_deq_rdy <== Bool(true);
        when(io.wbcmdq_deq_val)
        {
          vlen_reg <== vlen;
          vlen_cnt_reg <== vlen;
          cmd_type_reg <== cmd_type;
          cmd_type_amo_reg <== cmd_type_amo;
          state <== VMU_Ctrl_Writeback;
        }
      }
      is(VMU_Ctrl_Writeback)
      {
        io.roq_deq_rdy <== buf_ldq_enq_rdy;
        buf_ldq_enq_val <== io.roq_deq_val;
        when(io.roq_deq_val && buf_ldq_enq_rdy)
        {
          when(vlen_reg === UFix(0))
          {
            state <== VMU_Ctrl_WritebackDone;
          }
          when(vlen_reg != UFix(0))
          {
            vlen_reg <== vlen_reg - UFix(1);
          }
          when(io.ldq.deq_rdy)
          {
            vlen_cnt_reg <== vlen_cnt_reg - UFix(1);
          }
        }
      }
      is(VMU_Ctrl_WritebackDone)
      {
        io.ldq.wb_done <== Bool(true);
        when(io.ldq.deq_rdy)
        {
          when(vlen_cnt_reg === UFix(0))
          {
            state <== VMU_Ctrl_Idle;
          }
          when(vlen_cnt_reg != UFix(0))
          {
            vlen_cnt_reg <== vlen_cnt_reg - UFix(1);
          }
        }
      }
      otherwise
      {
        io.wbcmdq_deq_rdy <== Bool(false);
        io.roq_deq_rdy <== Bool(false);
        buf_ldq_enq_val <== Bool(false);
        io.ldq.wb_done <== Bool(false);
      }
    }

  }

}

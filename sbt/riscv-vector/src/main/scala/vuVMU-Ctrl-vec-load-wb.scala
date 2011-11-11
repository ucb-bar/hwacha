package riscvVector {
  import Chisel._
  import Node._
  import Interface._
  import Fpu._

  class vuVMU_Ctrl_vec_load_wbIO extends Bundle
  {
    val vec_done		= Bool('output);
    val ldq_deq		  = Bool('input);
    
    val wbcmdq_deq_bits		= Bits(VM_WBCMD_SZ, 'input);
    val wbcmdq_deq_val		= Bool('input);
    val wbcmdq_deq_rdy		= Bool('output);
    // interface to reorder queue
    val roq_deq_bits	= Bits(128, 'input);
    val roq_deq_val		= Bool('input);
    val roq_deq_rdy		= Bool('output);
    // interface to load data queue
    val ldq_enq_bits	= Bits(65, 'output);
    val ldq_enq_val		= Bool('output);
    val ldq_enq_rdy		= Bool('input);
  }

  class vuVMU_Ctrl_vec_load_wb extends Component 
  {
    val io = new vuVMU_Ctrl_vec_load_wbIO();

    val VMU_Ctrl_Idle          = Bits(0,2);
    val VMU_Ctrl_Writeback     = Bits(1,2);
    val VMU_Ctrl_WritebackDone = Bits(2,2);

    val vlen = io.wbcmdq_deq_bits(VMCMD_VLEN_SZ-1, 0);
    val addr = io.wbcmdq_deq_bits(VMIMM_SZ+VMCMD_VLEN_SZ-1, VMCMD_VLEN_SZ);
    val stride = io.wbcmdq_deq_bits(VMSTRIDE_SZ+VMIMM_SZ+VMCMD_VLEN_SZ-1,VMIMM_SZ+VMCMD_VLEN_SZ);
    val cmd_type = io.wbcmdq_deq_bits(VM_WBCMD_SZ-1, VM_WBCMD_SZ-4);

    val cmd_type_reg = Reg(resetVal = Bits(0,4));
    val addr_reg = Reg(resetVal = UFix(0,VMIMM_SZ));
    val stride_reg = Reg(resetVal = UFix(0,VMSTRIDE_SZ));
    val vlen_reg = Reg(resetVal = UFix(0,VMCMD_VLEN_SZ));
    val vlen_cnt_reg = Reg(resetVal = UFix(0,VMCMD_VLEN_SZ));

    val state = Reg(resetVal = VMU_Ctrl_Idle);

    val buf_ldq_enq_val = Wire(){Bool()};
    val buf_ldq_enq_rdy = Wire(){Bool()};

    val delay_roq_deq_bits = Wire(){Bits(128)};
    val delay_cmd_type = Wire(){Bits(4)};
    val delay_addr_lsb = Wire(){Bits(4)};

    //insert queue

    val ldq_bits = Wire(){Bits()};
    val ldq_sp_bits = Wire(){Bits()};
    val ldq_dp_bits = Wire(){Bits()};
    val fp_cmd = delay_cmd_type(3).toBool;

    when(fp_cmd)
    {
      switch(delay_cmd_type(1,0))
      {
        is(Bits("b11")) {io.ldq_enq_bits <== ldq_dp_bits;}
        is(Bits("b10")) {io.ldq_enq_bits <== Cat(Bits("hFFFF_FFFF"), ldq_sp_bits);}
      }
    }
    otherwise
    {
      io.ldq_enq_bits <== Cat(Bits("b0", 1), ldq_bits);
    }

    val rf32f32  = new recodedFloat32ToFloat32();
    rf32f32.io.in := ldq_bits(31,0);
    rf32f32.io.out := ldq_sp_bits;
    val rf64f64  = new recodedFloat64ToFloat64();
    rf64f64.io.in := ldq_bits;
    rf64f64.io.out := ldq_dp_bits;

    val bhwd_sel = new vuVMU_BHWD_sel();
    bhwd_sel.io.bhwd_sel := delay_cmd_type(1,0);
    bhwd_sel.io.signext := delay_cmd_type(2);
    bhwd_sel.io.addr_lsb := delay_addr_lsb;
    bhwd_sel.io.din := delay_roq_deq_bits;
    bhwd_sel.io.dout := ldq_bits;

    switch(state)
    {
      is(VMU_Ctrl_Idle)
      {
        io.wbcmdq_deq_rdy <== Bool(true);
        when(io.wbcmdq_deq_val)
        {
          addr_reg <== addr;
          stride_reg <== stride;
          vlen_reg <== vlen;
          vlen_cnt_reg <== vlen;
          cmd_type_reg <== cmd_type;
          state <== VMU_Ctrl_Writeback;
        }
      }
      is(VMU_Ctrl_Writeback)
      {
        when(io.roq_deq_val)
        {
          buf_ldq_enq_val <== Bool(true);
          when(buf_ldq_enq_rdy)
          {
            addr_reg <== addr_reg + stride_reg;
            vlen_reg <== vlen_reg - UFix(1);
            // Should double check to see
            // what is used in this comparison
            when( vlen_reg === UFix(0) ) 
            {
              state <== VMU_Ctrl_WritebackDone;
              io.roq_deq_rdy <== Bool(true);
            }
            // Verilog was as follows
            // addr_next = addr_reg + stride_reg;
            // else if (addr_next[31:4] != addr_reg[31:4])
            when( stride_reg != Bits(0) )
            {
              io.roq_deq_rdy <== Bool(true);
            }
          }
        }
        when(io.ldq_deq)
        {
          vlen_cnt_reg <== vlen_cnt_reg - UFix(1);
        }
      }
      is(VMU_Ctrl_WritebackDone)
      {
        io.vec_done <== Bool(true);
        when(io.ldq_deq)
        {
          when(vlen_cnt_reg === UFix(0) )
          {
            state <== VMU_Ctrl_Idle;
          }
          otherwise
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
        io.vec_done <== Bool(false);
      }
    }
  }
}


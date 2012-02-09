package riscvVector {
  import Chisel._
  import Node._
  import Interface._
  import Fpu._
  import queues._

  class vuVMU_Ctrl_vec_load_wbIO extends Bundle
  {
    val wbcmdq_deq_bits		= Bits(VM_WBCMD_SZ, INPUT);
    val wbcmdq_deq_val		= Bool(INPUT);
    val wbcmdq_deq_rdy		= Bool(OUTPUT);
    // interface to reorder queue
    val roq_deq_bits	= Bits(128, INPUT);
    val roq_deq_val		= Bool(INPUT);
    val roq_deq_rdy		= Bool(OUTPUT);
    // interface to load data queue
    val ldq          = new vldqIO();
  }

  class vuVMU_Ctrl_vec_load_wb extends Component 
  {
    val io = new vuVMU_Ctrl_vec_load_wbIO();

    val vec_done		      = Wire(){Bool()};
    val ldq_deq		        = Wire(){Bool()};

    val buf_ldq_enq_val = Wire(){Bool()};
    val buf_ldq_enq_rdy = Wire(){Bool()};

    val delay_cmd_type = Wire(){Bits(width=4)};
    val delay_addr_lsb = Wire(){Bits(width=4)};
    val delay_roq_deq_bits = Wire(){Bits(width=128)};

    val ldq_bits = Wire(){Bits(width=64)};
    val ldq_sp_bits = Wire(){Bits(width=33)};
    val ldq_dp_bits = Wire(){Bits(width=65)};
    val fp_cmd = delay_cmd_type(3).toBool;

    io.ldq.wb_done   := vec_done;
    ldq_deq          := io.ldq.deq_rdy;

    val ldq_enq_bits_int = MuxCase(
      Cat(Bits("b0", 1), ldq_bits), Array(
        (fp_cmd && delay_cmd_type(1,0) === Bits("b11",2)) -> ldq_dp_bits,
        (fp_cmd && delay_cmd_type(1,0) === Bits("b10",2)) -> Cat(Bits("hFFFF_FFFF", 32), ldq_sp_bits)
      ));
    
    io.ldq.enq_bits  := ldq_enq_bits_int;

    val VMU_Ctrl_Idle          = Bits(0,2);
    val VMU_Ctrl_Writeback     = Bits(1,2);
    val VMU_Ctrl_WritebackDone = Bits(2,2);

    val vlen = io.wbcmdq_deq_bits(VMCMD_VLEN_SZ-1, 0).toUFix;
    val addr = io.wbcmdq_deq_bits(VMIMM_SZ+VMCMD_VLEN_SZ-1, VMCMD_VLEN_SZ).toUFix;
    val stride = io.wbcmdq_deq_bits(VMSTRIDE_SZ+VMIMM_SZ+VMCMD_VLEN_SZ-1,VMIMM_SZ+VMCMD_VLEN_SZ).toUFix;
    val cmd_type = io.wbcmdq_deq_bits(VM_WBCMD_SZ-1, VM_WBCMD_SZ-4);

    val cmd_type_reg = Reg(resetVal = Bits(0,4));
    val addr_reg = Reg(resetVal = UFix(0,VMIMM_SZ));
    val stride_reg = Reg(resetVal = UFix(0,VMSTRIDE_SZ));
    val vlen_reg = Reg(resetVal = UFix(0,VMCMD_VLEN_SZ));
    val vlen_cnt_reg = Reg(resetVal = UFix(0,VMCMD_VLEN_SZ));

    val state = Reg(resetVal = VMU_Ctrl_Idle);

    val skidbuf = new queuePipe1PF(128+4+4);
    skidbuf.io.enq_bits := Cat(cmd_type_reg, addr_reg(3,0), io.roq_deq_bits);
    skidbuf.io.enq_val := buf_ldq_enq_val;
    buf_ldq_enq_rdy := skidbuf.io.enq_rdy;
    delay_cmd_type := skidbuf.io.deq_bits(135,132);
    delay_addr_lsb := skidbuf.io.deq_bits(131,128);
    delay_roq_deq_bits := skidbuf.io.deq_bits(127,0);
    skidbuf.io.deq_val ^^ io.ldq.enq_val;
    skidbuf.io.deq_rdy ^^ io.ldq.enq_rdy;
    
    val recode_sp     = new float32ToRecodedFloat32();
    recode_sp.io.in   := ldq_bits(31,0);
    ldq_sp_bits       := recode_sp.io.out;
    
    val recode_dp     = new float64ToRecodedFloat64();
    recode_dp.io.in   := ldq_bits;
    ldq_dp_bits       := recode_dp.io.out;

    val bhwd_sel = new vuVMU_BHWD_sel();
    bhwd_sel.io.bhwd_sel  := delay_cmd_type(1,0);
    bhwd_sel.io.signext   := delay_cmd_type(2).toBool;
    bhwd_sel.io.addr_lsb  := delay_addr_lsb;
    bhwd_sel.io.din       := delay_roq_deq_bits;
    ldq_bits              := bhwd_sel.io.dout;

    io.wbcmdq_deq_rdy <== Bool(false);
    io.roq_deq_rdy <== Bool(false);
    buf_ldq_enq_val <== Bool(false);
    vec_done <== Bool(false);

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
            when( vlen_reg != UFix(0) &&
                  addr_reg(31,4) != (addr_reg + stride_reg)(31,4) )
            {
              io.roq_deq_rdy <== Bool(true);
            }
          }
        }
        when(ldq_deq)
        {
          vlen_cnt_reg <== vlen_cnt_reg - UFix(1);
        }
      }
      is(VMU_Ctrl_WritebackDone)
      {
        vec_done <== Bool(true);
        when(ldq_deq)
        {
          when(vlen_cnt_reg === UFix(0) )
          {
            state <== VMU_Ctrl_Idle;
          }
          when(vlen_cnt_reg != UFix(0) ) // otherwise
          {
            vlen_cnt_reg <== vlen_cnt_reg - UFix(1);
          }
          // Alternatively:
          // when(vlen_cnt_reg === UFix(0) )
          // {
          //   state <== VMU_Ctrl_Idle;
          //   vlen_cnt_reg <== vlen_cnt_reg; // to counter otherwise assignment
          // }
          // otherwise
          // {
          //   vlen_cnt_reg <== vlen_cnt_reg - UFix(1);
          // }

        }
      }
    }
  }
}


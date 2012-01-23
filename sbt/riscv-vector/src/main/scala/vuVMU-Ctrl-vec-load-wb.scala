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

    val vmldq             = new vmldqIO().flip;

    // interface to load data queue
    val vldq               = new vldqIO();
  }

  class vuVMU_Ctrl_vec_load_wb extends Component 
  {
    val io = new vuVMU_Ctrl_vec_load_wbIO();

    val vlen = io.wbcmdq_deq_bits(VMCMD_VLEN_SZ-1, 0).toUFix;
    val addr = io.wbcmdq_deq_bits(VMIMM_SZ+VMCMD_VLEN_SZ-1, VMCMD_VLEN_SZ).toUFix;
    val stride = io.wbcmdq_deq_bits(VMSTRIDE_SZ+VMIMM_SZ+VMCMD_VLEN_SZ-1,VMIMM_SZ+VMCMD_VLEN_SZ).toUFix;
    val cmd_type = io.wbcmdq_deq_bits(VM_WBCMD_SZ-1, VM_WBCMD_SZ-4);

    val vec_done		      = Wire(){Bool()};
    val vldq_deq		      = Wire(){Bool()};

    val vldq_bits         = Wire(){Bits(width=64)};
    val vldq_sp_bits      = Wire(){Bits(width=33)};
    val vldq_dp_bits      = Wire(){Bits(width=65)};
    val fp_cmd            = cmd_type(3).toBool;

    io.vldq.wb_done        := vec_done;
    vldq_deq              := io.vldq.deq_rdy;

    val vldq_enq_bits_int = MuxCase(
      Cat(Bits("b0", 1), vldq_bits), Array(
        (fp_cmd && cmd_type(1,0) === Bits("b11",2)) -> vldq_dp_bits,
        (fp_cmd && cmd_type(1,0) === Bits("b10",2)) -> Cat(Bits("hFFFF_FFFF", 32), vldq_sp_bits)
      ));
    
    io.vldq.enq_bits := vldq_enq_bits_int;

    val VMU_Ctrl_Idle          = Bits(0,2);
    val VMU_Ctrl_Writeback     = Bits(1,2);
    val VMU_Ctrl_WritebackDone = Bits(2,2);

    val cmd_type_reg  = Reg(resetVal = Bits(0,4));
    val addr_reg      = Reg(resetVal = UFix(0,VMIMM_SZ));
    val stride_reg    = Reg(resetVal = UFix(0,VMSTRIDE_SZ));
    val vlen_reg      = Reg(resetVal = UFix(0,VMCMD_VLEN_SZ));
    val vlen_cnt_reg  = Reg(resetVal = UFix(0,VMCMD_VLEN_SZ));

    val state = Reg(resetVal = VMU_Ctrl_Idle);

    val recode_sp     = new float32ToRecodedFloat32();
    recode_sp.io.in   := vldq_bits(31,0);
    vldq_sp_bits       := recode_sp.io.out;
    
    val recode_dp     = new float64ToRecodedFloat64();
    recode_dp.io.in   := vldq_bits;
    vldq_dp_bits       := recode_dp.io.out;

    val bhwd_sel          = new vuVMU_BHWD_sel();
    bhwd_sel.io.bhwd_sel  := cmd_type(1,0);
    bhwd_sel.io.signext   := cmd_type(2).toBool;
    bhwd_sel.io.addr_lsb  := addr_reg(3,0);
    bhwd_sel.io.din       := io.vmldq.bits;
    vldq_bits             := bhwd_sel.io.dout;

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
        when(io.vmldq.valid)
        {
          addr_reg <== addr_reg + stride_reg;
          vlen_reg <== vlen_reg - UFix(1);
          // Should double check to see
          // what is used in this comparison
          when( vlen_reg === UFix(0) ) 
          {
            state <== VMU_Ctrl_WritebackDone;
            io.vmldq.rdy <== Bool(true);
          }
          // Verilog was as follows
          // addr_next = addr_reg + stride_reg;
          // else if (addr_next[31:4] != addr_reg[31:4])
          when( vlen_reg != UFix(0) &&
            addr_reg(31,4) != (addr_reg + stride_reg)(31,4) )
          {
            io.vmldq.rdy <== Bool(true);
          }
        }
        when(vldq_deq)
        {
          vlen_cnt_reg <== vlen_cnt_reg - UFix(1);
        }
      }
      is(VMU_Ctrl_WritebackDone)
      {
        vec_done <== Bool(true);
        when(vldq_deq)
        {
          when(vlen_cnt_reg === UFix(0) )
          {
            state <== VMU_Ctrl_Idle;
          }
          when(vlen_cnt_reg != UFix(0) ) // otherwise
          {
            vlen_cnt_reg <== vlen_cnt_reg - UFix(1);
          }
        }
      }
      otherwise
      {
        io.wbcmdq_deq_rdy <== Bool(false);
        io.vmldq.rdy <== Bool(false);
        vec_done <== Bool(false);
      }
    }
  }
}

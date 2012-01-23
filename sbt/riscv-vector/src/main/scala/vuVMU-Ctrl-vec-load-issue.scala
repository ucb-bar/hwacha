package riscvVector {
  import Chisel._
  import Node._
  import Interface._

  class vuVMU_Ctrl_vec_load_issueIO extends Bundle {
    val iscmdq_deq_bits		= UFix(VM_ISCMD_SZ, INPUT);
    val iscmdq_deq_val		= Bool(INPUT);
    val iscmdq_deq_rdy		= Bool(OUTPUT);
    
    // to load request queue
    // 28 bits for address
    val vlrq            = new vlrqIO().flip;
  }

  class vuVMU_Ctrl_vec_load_issue extends Component {
    val io = new vuVMU_Ctrl_vec_load_issueIO();

    val VMU_Ctrl_Idle       = Bits(0, 2);
    val VMU_Ctrl_IssueShort = Bits(1, 2);
    val VMU_Ctrl_IssueLong  = Bits(2, 2);

    val state         = Reg(resetVal = VMU_Ctrl_Idle);
    val addr_reg      = Reg(resetVal = UFix(0, 32));
    val last_addr_reg = Reg(resetVal = UFix(0, 32));
    val stride_reg    = Reg(resetVal = UFix(0, 32));
    
    val vlen    = io.iscmdq_deq_bits(VMCMD_VLEN_SZ-1, 0).toUFix(); 
    val addr    = io.iscmdq_deq_bits(VMIMM_SZ+VMCMD_VLEN_SZ-1, VMCMD_VLEN_SZ).toUFix();
    val stride  = io.iscmdq_deq_bits(VMSTRIDE_SZ+VMIMM_SZ+VMCMD_VLEN_SZ-1, VMIMM_SZ+VMCMD_VLEN_SZ).toUFix();
    val addr_cacheline = Cat(addr_reg(31,4), UFix(0,4)).toUFix;

    io.vlrq.bits := addr_reg(31,4);

    switch(state)
    {
      is(VMU_Ctrl_Idle)
      {
        io.iscmdq_deq_rdy <== Bool(true);
        when( io.iscmdq_deq_val )
        {
          addr_reg <== addr;
          last_addr_reg <== addr + (vlen * stride);
          stride_reg <== stride;
          when( stride < UFix(16) )
          {
            state <== VMU_Ctrl_IssueShort;
          }
          otherwise
          {
            state <== VMU_Ctrl_IssueLong;
          }
        }
      }
      is(VMU_Ctrl_IssueShort)
      {
        when( addr_cacheline > last_addr_reg )
        {
          state <== VMU_Ctrl_Idle;
        }
        when( !(addr_cacheline > last_addr_reg) )
        {
          io.vlrq.valid <== Bool(true);
          when( io.vlrq.rdy )
          {
            addr_reg <== addr_reg + UFix(16);
          }
        }
      }
      is(VMU_Ctrl_IssueLong)
      {
        when( addr_cacheline > last_addr_reg )
        {
          state <== VMU_Ctrl_Idle;
        }
        when( !(addr_cacheline > last_addr_reg) )
        {
          io.vlrq.valid <== Bool(true);
          when( io.vlrq.rdy )
          {
            addr_reg <== addr_reg + stride_reg;
          }
        }
      }
      otherwise
      {
        io.vlrq.valid <== Bool(false);
        io.iscmdq_deq_rdy <== Bool(false);
      }
    }
  }
}

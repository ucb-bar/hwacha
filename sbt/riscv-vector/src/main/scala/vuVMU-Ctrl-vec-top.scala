package riscvVector {
  import Chisel._
  import Node._
  import Interface._

  class vuVMU_Ctrl_vec_topIO extends Bundle {
    val vmcmdq      = new vmcmdqIO();
    val vmimmq      = new vmimmqIO();
    val vmstrideq   = new vmstrideqIO();
    val vmrespq     = new vmrespqIO();
    val iscmdq      = new iscmdqIO();
    val wbcmdq      = new wbcmdqIO();
    val stcmdq      = new stcmdqIO();
    val store_busy		= Bool(INPUT);
  }
  
  class vuVMU_Ctrl_vec_top extends Component {
    val io = new vuVMU_Ctrl_vec_topIO();
    
    val VMU_Ctrl_Idle          = Bits(0,3);
    val VMU_Ctrl_Load          = Bits(1,3);
    val VMU_Ctrl_LoadStride    = Bits(2,3);
    val VMU_Ctrl_Store         = Bits(3,3);
    val VMU_Ctrl_StoreStride   = Bits(4,3);
    val VMU_Ctrl_Sync          = Bits(5,3);
    val VMU_Ctrl_SyncWait      = Bits(6,3);
    val VMU_Ctrl_Invalid       = Bits(7,3);

    val state = Reg(resetVal = VMU_Ctrl_Idle);
    
    val cmd = io.vmcmdq.bits(VMCMD_CMDCODE);
    val vlen = io.vmcmdq.bits(VMCMD_VLEN_M1);
    val addr = io.vmimmq.bits;
    val stride = Wire() {Bits(width=VMSTRIDE_SZ)};

    io.wbcmdq.bits := Cat(cmd(3,0), stride.toBits, addr, vlen);
    io.iscmdq.bits := Cat(stride.toBits, addr, vlen);
    io.stcmdq.bits := Cat(cmd(3,0), stride.toBits, addr, vlen);

    io.vmcmdq.rdy          <== Bool(false);
    io.vmimmq.rdy          <== Bool(false);
    io.vmstrideq.rdy       <== Bool(false);
    io.vmrespq.bits        <== Bits(0);
    io.vmrespq.valid         <== Bool(false);
    io.iscmdq.valid          <== Bool(false);
    io.wbcmdq.valid          <== Bool(false);
    io.stcmdq.valid          <== Bool(false);
    stride                 <== UFix(0);

    switch(state)
    {
      // Need default values

      is(VMU_Ctrl_Idle)
      {
        when(io.vmcmdq.valid)
        {
          switch(cmd(7,4))
          {
            state <== VMU_Ctrl_Invalid;
	    is(Bits("b0000",4))
            {
              when(cmd(3,0) === Bits("b1100") || cmd(3,0) === Bits("b1101") || cmd(3,0) === Bits("b1110") || cmd(3,0) === Bits("b1111"))
              {
                state <== VMU_Ctrl_Sync;
              }
	      when( !(cmd(3,0) === Bits("b1100") || cmd(3,0) === Bits("b1101") || cmd(3,0) === Bits("b1110") || cmd(3,0) === Bits("b1111")) )
              {
                state <== VMU_Ctrl_Invalid;
              }
            }
            is(Bits("b1000",4)) // standard vector load
            {
              when( io.vmimmq.valid && io.iscmdq.rdy && io.wbcmdq.rdy )
              {
                state <== VMU_Ctrl_Load;
              }
            }
            is(Bits("b1001",4)) // standard vector store
            {
              when( io.vmimmq.valid && io.stcmdq.rdy )
              {
                state <== VMU_Ctrl_Store;
              }
            }
            is(Bits("b1010",4)) // strided vector load
            {
              when( io.vmimmq.valid && io.vmstrideq.valid && io.iscmdq.rdy && io.wbcmdq.rdy ) 
              {
                state <== VMU_Ctrl_LoadStride;
              }
            }
            is(Bits("b1011",4)) // strided vector load
            {
              when( io.vmimmq.valid && io.vmstrideq.valid && io.stcmdq.rdy ) 
              {
                state <== VMU_Ctrl_StoreStride;
              }
            }
          }
        }
      }
      is(VMU_Ctrl_Load)
      {
        io.vmcmdq.rdy <== Bool(true);
        io.vmimmq.rdy <== Bool(true);
        io.iscmdq.valid <== Bool(true);
        io.wbcmdq.valid <== Bool(true);
        switch(cmd(1,0))
        {
	  stride <== UFix(0);
          is(Bits("b11")) { stride <== UFix(8); }
          is(Bits("b10")) { stride <== UFix(4); }
          is(Bits("b01")) { stride <== UFix(2); }
          is(Bits("b00")) { stride <== UFix(1); }
        }
        state <== VMU_Ctrl_Idle;
      }
      is(VMU_Ctrl_Store)
      {
        io.vmcmdq.rdy <== Bool(true);
        io.vmimmq.rdy <== Bool(true);
        io.stcmdq.valid <== Bool(true);
        switch(cmd(1,0))
        {
	  stride <== UFix(0);
          is(Bits("b11")) { stride <== UFix(8); }
          is(Bits("b10")) { stride <== UFix(4); }
          is(Bits("b01")) { stride <== UFix(2); }
          is(Bits("b00")) { stride <== UFix(1); }
        }
        state <== VMU_Ctrl_Idle;
      }
      is(VMU_Ctrl_LoadStride)
      {
        io.vmcmdq.rdy <== Bool(true);
        io.vmimmq.rdy <== Bool(true);
        io.vmstrideq.rdy <== Bool(true);
        io.iscmdq.valid <== Bool(true);
        io.wbcmdq.valid <== Bool(true);
        stride <== io.vmstrideq.bits;
        state <== VMU_Ctrl_Idle;
      }
      is(VMU_Ctrl_StoreStride)
      {
        io.vmcmdq.rdy <== Bool(true);
        io.vmimmq.rdy <== Bool(true);
        io.vmstrideq.rdy <== Bool(true);
        io.stcmdq.valid <== Bool(true);
        stride <== io.vmstrideq.bits;
        state <== VMU_Ctrl_Idle;
      }
      is(VMU_Ctrl_Sync)
      {
        when(!io.store_busy)
        {
          io.vmcmdq.rdy <== Bool(true);
          io.vmrespq.valid <== Bool(true);
          io.vmrespq.bits <== Bits(1);
          when(io.vmrespq.rdy)
          {
            state <== VMU_Ctrl_Idle;
          }
          when(!io.vmrespq.rdy)	  
          {
            state <== VMU_Ctrl_SyncWait;
          }
        }
      }
      is(VMU_Ctrl_SyncWait)
      {
        io.vmrespq.valid <== Bool(true);
        io.vmrespq.bits <== Bits(1);
        when(io.vmrespq.rdy)
        {
          state <== VMU_Ctrl_Idle;
        }
        when(!io.vmrespq.rdy)
        {
          state <== VMU_Ctrl_SyncWait;
        }
      }
      is(VMU_Ctrl_Invalid)
      {
        io.vmcmdq.rdy <== Bool(true);
        // error?
        state <== VMU_Ctrl_Idle;
      }
    }
  }
}

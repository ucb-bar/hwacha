package riscvVector {
  import Chisel._
  import Node._
  import interface._

  class vuVMU_Ctrl_vec_topIO extends Bundle {
    val vmcmdq_bits		= Bits(VMCMD_SZ, 'input);
    val vmcmdq_val		= Bool('input);
    val vmcmdq_rdy		= Bool('output);

    val vmimmq_bits		= Bits(VMIMM_SZ, 'input);
    val vmimmq_val		= Bool('input);
    val vmimmq_rdy		= Bool('output);

    val vmstrideq_bits		= Bits(VMSTRIDE_SZ, 'input);
    val vmstrideq_val		= Bool('input);
    val vmstrideq_rdy		= Bool('output);

    val vmrespq_bits		= Bits(VMRESP_SZ, 'output);
    val vmrespq_val		= Bool('output);
    val vmrespq_rdy		= Bool('input);

    val iscmdq_bits		= Bits(VM_ISCMD_SZ, 'output);
    val iscmdq_val		= Bool('output);
    val iscmdq_rdy		= Bool('input);

    val wbcmdq_bits		= Bits(VM_WBCMD_SZ, 'output);
    val wbcmdq_val		= Bool('output);
    val wbcmdq_rdy		= Bool('input);

    val stcmdq_bits		= Bits(VM_STCMD_SZ, 'output);
    val stcmdq_val		= Bool('output);
    val stcmdq_rdy		= Bool('input);
    val store_busy		= Bool('input);
  }
  
  class vuVMU_Ctrl_vec_top extends Component {
    val VMU_Ctrl_Idle          = UFix(0,3);
    val VMU_Ctrl_Load          = UFix(1,3);
    val VMU_Ctrl_LoadStride    = UFix(2,3);
    val VMU_Ctrl_Store         = UFix(3,3);
    val VMU_Ctrl_StoreStride   = UFix(4,3);
    val VMU_Ctrl_Sync          = UFix(5,3);
    val VMU_Ctrl_SyncWait      = UFix(6,3);
    val VMU_Ctrl_Invalid       = UFix(7,3);

    val state = Reg(resetVal = VMU_Ctrl_Idle);
    
    val cmd = VMCMD_CMDCODE(io.vmcmdq_bits);
    val vlen = VMCMD_VLEN_M1(io.vmcmdq_bits);
    val addr = io.vmimmq_bits;
    val stride = Wire() {Bits(VMSTRIDE_SZ)};

    io.wbcmdq_bits := Cat(cmd(3,0), stride.toBits, addr, vlen);
    io.iscmdq_bits := Cat(stride.toBits, addr, vlen);
    io.stcmdq_bits := Cat(cmd(3,0), stride.toBits, addr, vlen);

    switch(state)
    {
      // Need default values

      is(VMU_Ctrl_Idle)
      {
        when(vmcmdq_val)
        {
          switch(cmd(7,4))
          {
            is(Bits("b0000",4))
            {
              when(cmd(3,0) === Bits("1100") || cmd(3,0) === Bits("1101") || cmd(3,0) === Bits("1110") || cmd(3,0) === Bits("1111"))
              {
                state <== VMU_Ctrl_Sync;
              }
              otherwise
              {
                state <== VMU_Ctrl_Invalid;
              }
            }
            is(Bits("b1000",4)) // standard vector load
            {
              when( io.vmimmq_val && io.iscmdq_rdy && io.wbcmdq_rdy )
              {
                state <== VMU_Ctrl_Load;
              }
            }
            is(Bits("b1001",4)) // standard vector store
            {
              when( io.vmimmq_val && io.stcmdq_rdy )
              {
                state <== VMU_Ctrl_Store;
              }
            }
            is(Bits("b1010",4)) // strided vector load
            {
              when( io.vmimmq_val && io.vmstrideq_val && io.iscmdq_rdy && io.wbcmdq_rdy ) 
              {
                state <== VMU_Ctrl_LoadStride;
              }
            }
            is(Bits("b1011",4)) // strided vector load
            {
              when( io.vmimmq_val && io.vmstrideq_val && io.stcmdq_rdy ) 
              {
                state <== VMU_Ctrl_StoreStride;
              }
            }
            otherwise
            {
              state <== VMU_Ctrl_Invalid;
            }
          }
        }
      }
      is(VMU_Ctrl_Load)
      {
        io.vmcmdq_rdy <== Bool(true);
        io.vmimmq_rdy <== Bool(true);
        io.iscmdq_rdy <== Bool(true);
        io.wbcmdq_rdy <== Bool(true);
        switch(cmd(1,0))
        {
          is(Bits("b11"))
          {
            stride <== UFix(8);
          }
          is(Bits("b10"))
          {
            stride <== UFix(4);
          }
          is(Bits("b01"))
          {
            stride <== UFix(2);
          }
          is(Bits("b00"))
          {
            stride <== UFix(1);
          }
          otherwise
          {
            stride <== UFix(0);
          }
        }
        state <== VMU_Ctrl_Idle;
      }
      is(VMU_Ctrl_Store)
      {
        io.vmcmdq_rdy <== Bool(true);
        io.vmimmq_rdy <== Bool(true);
        io.stcmdq_rdy <== Bool(true);
        switch(cmd(1,0))
        {
          is(Bits("b11"))
          {
            stride <== UFix(8);
          }
          is(Bits("b10"))
          {
            stride <== UFix(4);
          }
          is(Bits("b01"))
          {
            stride <== UFix(2);
          }
          is(Bits("b00"))
          {
            stride <== UFix(1);
          }
          otherwise
          {
            stride <== UFix(0);
          }
        }
        state <== VMU_Ctrl_Idle;
      }
      is(VMU_Ctrl_LoadStride)
      {
        io.vmcmdq_rdy <== Bool(true);
        io.vmimmq_rdy <== Bool(true);
        io.vmstrideq_rdy <== Bool(true);
        io.iscmdq_val <== Bool(true);
        io.wbcmdq_val <== Bool(true);
        stride <== io.vmstrideq_bits;
        state <== VMU_Ctrl_Idle;
      }
      is(VMU_Ctrl_StoreStride)
      {
        io.vmcmdq_rdy <== Bool(true);
        io.vmimmq_rdy <== Bool(true);
        io.vmstrideq_rdy <== Bool(true);
        io.stcmdq_val <== Bool(true);
        stride <== io.vmstrideq_bits;
        state <== VMU_Ctrl_Idle;
      }
      is(VMU_Ctrl_Sync)
      {
        when(!io.store_busy)
        {
          io.vmcmdq_rdy <== Bool(true);
          io.vmrespq_val <== Bool(true);
          io.vmrespq_bits <== Bits(1);
          when(io.vmrespq_rdy)
          {
            state <== VMU_Ctrl_Idle;
          }
          otherwise
          {
            state <== VMU_Ctrl_SyncWait;
          }
        }
      }
      is(VMU_Ctrl_SyncWait)
      {
        io.vmrespq_val <== Bool(true);
        io.vmrespq_bits <== Bits(1);
        when(io.vmcmdq_rdy)
        {
          state <== VMU_Ctrl_Idle;
        }
        otherwise
        {
          state <== VMU_Ctrl_SyncWait;
        }
      }
      is(VMU_Ctrl_Invalid)
      {
        io.vmcmdq_rdy <== Bool(true);
        // error?
        state <== VMU_Ctrl_Idle;
      }
      otherwise
      {
        vmcmdq_rdy          = Bool(false);
        vmimmq_rdy          = Bool(false);
        vmstrideq_rdy       = Bool(false);
        vmrespq_bits        = Bits(0);
        vmrespq_val         = Bool(false);
        iscmdq_val          = Bool(false);
        wbcmdq_val          = Bool(false);
        stcmdq_val          = Bool(false);
        stride              = UFix(0);
      }
    }
  }

package riscvVector {
  import Chisel._
  import Node._
  import Interface._
  import Fpu._

  class vuVMU_Ctrl_ut_storeIO extends Bundle
  {
    val stcmdq_deq_bits		= UFix(UT_STCMD_SZ, 'input);
    val stcmdq_deq_val		= Bool('input);
    val stcmdq_deq_rdy		= Bool('output);
 
    val sdq_deq       = new utsdq_deqIO();
   
    val utaq_deq_bits		= UFix(32, 'input);
    val utaq_deq_val		= Bool('input);
    val utaq_deq_rdy		= Bool('output);
   
    // to reorder queue
    val roq_deq_tag_bits		= UFix(8, 'input);
    val roq_deq_tag_val		= Bool('input);
    val roq_deq_tag_rdy		= Bool('output);
   
    val srq_enq_addr_bits		= UFix(30, 'output);
    val srq_enq_tag_bits		= Bits(12, 'output);
    val srq_enq_op_bits		= Bits(4, 'output);
    val srq_enq_data_bits		= Bits(64, 'output);
    val srq_enq_wmask_bits		= Bits(8, 'output);
    val srq_enq_val		= Bool('output);
    val srq_enq_rdy		= Bool('input);
    
    val store_busy		= Bool('output);
  }

  class vuVMU_Ctrl_ut_store extends Component
  {
    val io = new vuVMU_Ctrl_ut_storeIO();

    val VMU_Ctrl_Idle   = UFix(0, 2);
    val VMU_Ctrl_Store  = UFix(1, 2);
    val VMU_Ctrl_AMO    = UFix(2, 2);

    val vlen          = io.stcmdq_deq_bits(UTMCMD_VLEN_SZ-1,0);
    val addr          = io.stcmdq_deq_bits(UTMIMM_SZ+UTMCMD_VLEN_SZ-1, UTMCMD_VLEN_SZ);
    val cmd_type      = io.stcmdq_deq_bits(UT_STCMD_SZ-2, UT_STCMD_SZ-6);
    val cmd_type_amo  = io.stcmdq_deq_bits(UT_STCMD_SZ);

    val cmd_type_reg = Reg(resetVal = Bits(0, 5));
    val addr_reg = Reg(resetVal = UFix(0, 32));
    val vlen_reg = Reg(resetVal = UFix(0, UTMCMD_VLEN_SZ));
    val state = Reg(resetVal = VMU_Ctrl_Idle);

    val req_addr = addr_reg + io.utaq_deq_bits;
    val sdq_deq_dp_bits = Wire() {Bits(width=32)};
    val sdq_deq_sp_bits = Wire() {Bits(width=64)};

    val cmd_type_fp = Mux(state === VMU_Ctrl_AMO, Bool(false), cmd_type_reg(3).toBool());

    io.store_busy := (state != VMU_Ctrl_Idle) || io.stcmdq_deq_val;


    val srq_enq_data_bits = Wire(){Bits()};
    when(cmd_type_fp)
    {
      switch(cmd_type_reg(1,0))
      {
        is(Bits("b11")) {srq_enq_data_bits <== sdq_deq_dp_bits;}
        is(Bits("b10")) {srq_enq_data_bits <== Fill(2, sdq_deq_sp_bits);}
      }
    }
    switch(cmd_type_reg(1,0))
    {
      is(Bits("b11")) {srq_enq_data_bits <== io.sdq_deq.bits(63,0);}
      is(Bits("b10")) {srq_enq_data_bits <== Fill(2, io.sdq_deq.bits(63,0));}
      is(Bits("b01")) {srq_enq_data_bits <== Fill(4, io.sdq_deq.bits(63,0));}
      is(Bits("b00")) {srq_enq_data_bits <== Fill(8, io.sdq_deq.bits(63,0));}
      otherwise {srq_enq_data_bits <== Bits(0, 64);}
    }

    val srq_enq_tag_bits = Wire(){Bits()};
    val srq_enq_op_bits = Wire(){Bits()};

    when(state === VMU_Ctrl_Store)
    {
      srq_enq_tag_bits <== Bits("h800", 12);
      srq_enq_op_bits <== UFix(1, 4);
    }
    when(state === VMU_Ctrl_AMO)
    {
      srq_enq_tag_bits <== Cat(Bits(8, 4), io.roq_deq_tag_bits);
      srq_enq_op_bits <== Cat(Bits(1,1), cmd_type_reg(4,2), Bits(0,4));
    }
    otherwise
    {
      srq_enq_tag_bits <== UFix(0, 12);
    }

    val store_data_wmask = Wire() {Bits()};

    io.srq_enq_data_bits  := srq_enq_data_bits;
    io.srq_enq_tag_bits   := srq_enq_tag_bits;
    io.srq_enq_op_bits    := srq_enq_op_bits;
    io.srq_enq_wmask_bits := store_data_wmask;
    io.srq_enq_addr_bits  := req_addr(31,2);

    val decoder_sp = new recodedFloat32ToFloat32();
    decoder_sp.io.in := io.sdq_deq.bits(32,0);
    sdq_deq_sp_bits := decoder_sp.io.out;
    val decoder_dp = new recodedFloat64ToFloat64();
    decoder_dp.io.in := io.sdq_deq.bits;
    sdq_deq_dp_bits := decoder_dp.io.out;

    switch(cmd_type_reg(1,0))
    {
      is(UFix(3,2))
      {
        store_data_wmask <== Bits("hFF", 8);
      }
      is(UFix(2,2))
      {
        when(req_addr(2).toBool())
        {
          store_data_wmask <== Bits("hF0", 8);
        }
        otherwise
        {
          store_data_wmask <== Bits("h0F", 8);
        }
      }
      is(UFix(1,2))
      {
        switch(req_addr(2,1))
        {
          is(UFix(0,2)) { store_data_wmask <== Bits("h03", 8); }
          is(UFix(1,2)) { store_data_wmask <== Bits("h0C", 8); }
          is(UFix(2,2)) { store_data_wmask <== Bits("h30", 8); }
          is(UFix(3,2)) { store_data_wmask <== Bits("hC0", 8); }
        }
      }
      is(UFix(0,2)) { 
        switch(req_addr(2,0)) {
          is(UFix(0,3)) { store_data_wmask <== Bits("h01", 8); }
          is(UFix(1,3)) { store_data_wmask <== Bits("h02", 8); }
          is(UFix(2,3)) { store_data_wmask <== Bits("h04", 8); }
          is(UFix(3,3)) { store_data_wmask <== Bits("h08", 8); }
          is(UFix(4,3)) { store_data_wmask <== Bits("h10", 8); }
          is(UFix(5,3)) { store_data_wmask <== Bits("h20", 8); }
          is(UFix(6,3)) { store_data_wmask <== Bits("h40", 8); }
          is(UFix(7,3)) { store_data_wmask <== Bits("h80", 8); }
        }
      }
    }

    switch(state) {
      is(VMU_Ctrl_Idle) {
        io.stcmdq_deq_rdy <== Bool(true);
        when(io.stcmdq_deq_val) {
          vlen_reg <== vlen;
          cmd_type_reg <== cmd_type;
          when(cmd_type_amo.toBool()) {
            addr_reg <== UFix(0, 32);
            state <== VMU_Ctrl_AMO;
          }
          otherwise {
            addr_reg <== addr;
            state <== VMU_Ctrl_Store;
          }
        }
      }
      is(VMU_Ctrl_Store) {
        io.sdq_deq.rdy <== io.utaq_deq_val && io.srq_enq_rdy;
        io.utaq_deq_rdy <== io.sdq_deq.valid && io.srq_enq_rdy;
        io.srq_enq_val <== io.sdq_deq.valid && io.utaq_deq_val;
        when( io.sdq_deq.valid && io.utaq_deq_val && io.srq_enq_rdy ) {
          when( vlen_reg === UFix(0,UTMCMD_VLEN_SZ) ) {
            state <== VMU_Ctrl_Idle;
          }
          otherwise {
            vlen_reg <== vlen_reg - UFix(1,UTMCMD_VLEN_SZ);
          }
        }
      }
      is(VMU_Ctrl_AMO) {
        io.sdq_deq.rdy      <== io.utaq_deq_val && io.srq_enq_rdy && io.roq_deq_tag_val;
        io.utaq_deq_rdy     <== io.sdq_deq.valid && io.srq_enq_rdy && io.roq_deq_tag_val;
        io.srq_enq_val      <== io.sdq_deq.valid && io.utaq_deq_val && io.roq_deq_tag_val;
        io.roq_deq_tag_rdy  <== io.utaq_deq_val && io.srq_enq_rdy && io.sdq_deq.valid;
        when( io.sdq_deq.valid && io.utaq_deq_val && io.roq_deq_tag_val && io.srq_enq_rdy ) {
          when( vlen_reg === UFix(0,UTMCMD_VLEN_SZ) ) {
            state <== VMU_Ctrl_Idle;
          }
          otherwise {
            vlen_reg <== vlen_reg - UFix(1,UTMCMD_VLEN_SZ);
          }
        }
      }
    }
  }
}

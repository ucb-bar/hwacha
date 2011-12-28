package riscvVector
{
  import Chisel._
  import Node._
  import Interface._
  import Fpu._
  import queues._
  import scala.collection.mutable.Queue

  object flatten 
  {
    def apply(x: Queue[Bits]): Bits =
      {
	var res: Bits = null;
	while(!x.isEmpty){
	  res = Cat(x.dequeue, res);
	}
	res
      }
  }

  class vuVMU_Ctrl_vec_storeIO extends Bundle
  {
    val stcmdq_deq_bits		= Bits(VM_STCMD_SZ, 'input);
    val stcmdq_deq_val		= Bool('input);
    val stcmdq_deq_rdy		= Bool('output);

    val sdq_deq           = new vsdq_deqIO();

    val srq_enq_addr_bits		= Bits(28, 'output);
    val srq_enq_data_bits		= Bits(128, 'output);
    val srq_enq_wmask_bits		= Bits(16, 'output);
    val srq_enq_val		= Bool('output);
    val srq_enq_rdy		= Bool('input);

    val store_busy		= Bool('output);
  }

  class vuVMU_Ctrl_vec_store extends Component
  {
    val io = new vuVMU_Ctrl_vec_storeIO();

    val VMU_Ctrl_Idle          = Bits(0,2);
    val VMU_Ctrl_Store         = Bits(1,2);
    val VMU_Ctrl_StoreWait     = Bits(2,2);

    val vlen = io.stcmdq_deq_bits(VMCMD_VLEN_SZ-1, 0).toUFix;
    val addr = io.stcmdq_deq_bits(VMIMM_SZ+VMCMD_VLEN_SZ-1, VMCMD_VLEN_SZ).toUFix;
    val stride = io.stcmdq_deq_bits(VMSTRIDE_SZ+VMIMM_SZ+VMCMD_VLEN_SZ-1,VMIMM_SZ+VMCMD_VLEN_SZ).toUFix;
    val cmd_type = io.stcmdq_deq_bits(VM_STCMD_SZ-1, VM_STCMD_SZ-4);

    val cmd_type_reg = Reg(resetVal = Bits(0,4));
    val addr_reg = Reg(resetVal = UFix(0,VMIMM_SZ));
    val stride_reg = Reg(resetVal = UFix(0,VMSTRIDE_SZ));
    val vlen_reg = Reg(resetVal = UFix(0,VMCMD_VLEN_SZ));
    val state = Reg(resetVal = VMU_Ctrl_Idle);

    val sbuf_enq_val = Wire(){Bool()};
    val store_data_wmask = Wire(){Bits()};

    val fp_cmd = cmd_type_reg(3).toBool;

    val rf32f32  = new recodedFloat32ToFloat32();
    rf32f32.io.in := io.sdq_deq.bits(32,0);
    val sdq_deq_sp = rf32f32.io.out;

    val rf64f64  = new recodedFloat64ToFloat64();
    rf64f64.io.in := io.sdq_deq.bits;
    val sdq_deq_dp = rf64f64.io.out

    io.store_busy := ~((state === VMU_Ctrl_Idle) && ~io.stcmdq_deq_val);

    val store_data = MuxCase(
      Bits(0,64), Array(	
	(fp_cmd && (cmd_type_reg(1, 0) === Bits("b11", 2))) -> sdq_deq_dp,
        (fp_cmd && (cmd_type_reg(1, 0) === Bits("b10", 2))) -> Fill(2, sdq_deq_sp),
        (cmd_type_reg(1, 0) === Bits("b11", 2)) -> io.sdq_deq.bits(63, 0),
        (cmd_type_reg(1, 0) === Bits("b10", 2)) -> Fill(2, io.sdq_deq.bits(31, 0)),
        (cmd_type_reg(1, 0) === Bits("b01", 2)) -> Fill(4, io.sdq_deq.bits(15, 0)),
        (cmd_type_reg(1, 0) === Bits("b00", 2)) -> Fill(8, io.sdq_deq.bits(7, 0))
      ));
      
    io.srq_enq_addr_bits := addr_reg(31,4);

    val addr_incr = addr_reg + stride_reg;

    switch(cmd_type_reg(1,0))
    {
      is(Bits("b11"))
      {
        store_data_wmask <== Bits("b11111111");
      }
      is(Bits("b10"))
      {
        switch(addr_reg(2))
        {
          is(Bits(0)) {store_data_wmask <== Bits("b00001111");}
          is(Bits(1)) {store_data_wmask <== Bits("b11110000");}
        }
      }
      is(Bits("b01"))
      {
        switch(addr_reg(2,1))
        {
          is(Bits("b00")) {store_data_wmask <== Bits("b00000011");}
          is(Bits("b01")) {store_data_wmask <== Bits("b00001100");}
          is(Bits("b10")) {store_data_wmask <== Bits("b00110000");}
          is(Bits("b11")) {store_data_wmask <== Bits("b11000000");}
        }
      }
      is(Bits("b00"))
      {
        switch(addr_reg(2,0))
        {
          is(Bits("b000")) {store_data_wmask <== Bits("b00000001");}
          is(Bits("b001")) {store_data_wmask <== Bits("b00000010");}
          is(Bits("b010")) {store_data_wmask <== Bits("b00000100");}
          is(Bits("b011")) {store_data_wmask <== Bits("b00001000");}
          is(Bits("b100")) {store_data_wmask <== Bits("b00010000");}
          is(Bits("b101")) {store_data_wmask <== Bits("b00100000");}
          is(Bits("b110")) {store_data_wmask <== Bits("b01000000");}
          is(Bits("b111")) {store_data_wmask <== Bits("b10000000");}
        }
      }
      otherwise
      {
        store_data_wmask <== Bits(0,8);
      }
    }

    switch(state)
    {
      is(VMU_Ctrl_Idle)
      {
        io.stcmdq_deq_rdy <== Bool(true);
        when(io.stcmdq_deq_val)
        {
          addr_reg <== addr;
          stride_reg <== stride;
          vlen_reg <== vlen;
          cmd_type_reg <== cmd_type;
          state <== VMU_Ctrl_Store;
        }
      }
      is(VMU_Ctrl_Store)
      {
        io.sdq_deq.rdy <== Bool(true);
        when(io.sdq_deq.valid)
        {
          sbuf_enq_val <== Bool(true);
          when(vlen_reg === UFix(0))
          {
            io.srq_enq_val <== Bool(true);
            when(io.srq_enq_rdy)
            {
              state <== VMU_Ctrl_Idle;
            }
            otherwise
            {
              state <== VMU_Ctrl_StoreWait;
            }
          }
          when(addr_incr(31,4) != addr_reg(31,4))
          {
            io.srq_enq_val <== Bool(true);
            when(io.srq_enq_rdy)
            {
              addr_reg <== addr_reg + stride_reg;
              vlen_reg <== vlen_reg - UFix(1);
            }
            when(!io.srq_enq_rdy)
            {
              state <== VMU_Ctrl_StoreWait;
            }
          }
          when(addr_incr(31,4) === addr_reg(31,4))
          {
            addr_reg <== addr_incr;
            vlen_reg <== vlen_reg - UFix(1);
          }
        }
      }
      is(VMU_Ctrl_StoreWait)
      {
        io.srq_enq_val <== Bool(true);
        when(io.srq_enq_rdy)
        {
          when(vlen_reg === UFix(0))
          {
            state <== VMU_Ctrl_Idle;
          }
          when(vlen_reg != UFix(0))
          {
            addr_reg <== addr_incr;
            vlen_reg <== vlen_reg - UFix(1);
            state <== VMU_Ctrl_Store;
          }
        }
      }
      otherwise
      {
        io.stcmdq_deq_rdy <== Bool(false);
        io.sdq_deq.rdy <== Bool(false);
        io.srq_enq_val <== Bool(false);
        sbuf_enq_val <== Bool(false);
      }
    }

    val sbuf_byte_enq_val = Mux(sbuf_enq_val,
				Mux(addr_reg(3), 
				     Cat(store_data_wmask, Bits(0,8)), 
				     Cat(Bits(0,8), store_data_wmask)), 
				Bits(0,16));

    var srq_enq_wmask_bits_array = new Queue[Bits];
    var srq_enq_data_bits_array = new Queue[Bits];
    var first = true;


    for( i <- 0 until 2 )
    {
      for( j <- 0 until 8 )
      {
	val sbuf = new queueFlowPF(8,2,1);
	sbuf.io.enq_bits := store_data(((j+1)*8)-1,(j*8));
	sbuf.io.enq_val  := sbuf_byte_enq_val(i*8+j).toBool;

	//io.srq_enq_data_bits(((j+1)*8+i*64)-1,j*8+i*64) := sbuf.io.deq_bits;
	srq_enq_data_bits_array += sbuf.io.deq_bits;
	//io.srq_enq_wmask_bits(i*8+j) := sbuf.io.deq_val;
	srq_enq_wmask_bits_array += sbuf.io.deq_val;

	sbuf.io.deq_rdy := io.srq_enq_val & io.srq_enq_rdy;
      }
    }

    io.srq_enq_data_bits := flatten(srq_enq_data_bits_array);
    io.srq_enq_wmask_bits := flatten(srq_enq_wmask_bits_array);
    
  }
}

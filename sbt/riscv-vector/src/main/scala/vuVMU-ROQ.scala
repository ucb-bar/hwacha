package riscvVector {
  import Chisel._
  import Node._
  import Interface._

  class vuVMU_ROQIO(ROQ_DATA_SIZE :Int = 128, ROQ_TAG_SIZE :Int = 3) extends Bundle
  {
    // interface to issue control
    val roq_deq_tag_bits  = Bits(ROQ_TAG_SIZE, 'output);
    val roq_deq_tag_val		= Bool('output);
    val roq_deq_tag_rdy		= Bool('input);
    // interface to responses from D$
    val roq_enq_data_bits   = Bits(ROQ_DATA_SIZE, 'input);
    val roq_enq_tag_bits    = Bits(ROQ_TAG_SIZE, 'input);
    val roq_enq_val		      = Bool('input);
    // interface to writeback control
    val roq_deq_data_bits   = Bits(ROQ_DATA_SIZE, 'output);
    val roq_deq_data_val		= Bool('output);
    val roq_deq_data_rdy		= Bool('input);
  }

  class vuVMU_ROQ(ROQ_DATA_SIZE :Int = 128, ROQ_TAG_ENTRIES :Int = 8, ROQ_TAG_SIZE :Int = 3) extends Component
  {
    val io = new vuVMU_ROQIO(ROQ_DATA_SIZE, ROQ_TAG_SIZE);
    
    val read_ptr          = Reg(resetVal = UFix(0, ROQ_TAG_SIZE));
    val write_ptr         = Reg(resetVal = UFix(0, ROQ_TAG_SIZE));
    val read_ptr_next     = read_ptr + UFix(1);
    val write_ptr_next    = write_ptr + UFix(1);

    val full              = (write_ptr_next === read_ptr);
    val vb_array          = Reg(resetVal=Bits(0, ROQ_TAG_ENTRIES));
    val roq_data_deq      = io.roq_deq_data_rdy && io.roq_deq_data_val;
    val roq_tag_deq       = io.roq_deq_tag_rdy && io.roq_deq_tag_val;
    val roq_deq_data_val  = Reg(resetVal = Bool(false));
    io.roq_deq_data_val   := roq_deq_data_val;

    val vb_update_mask = MuxCase(
      Bits(0,ROQ_TAG_ENTRIES), Array(
        roq_data_deq -> ~(Bits(1,ROQ_TAG_ENTRIES) << read_ptr),
        io.roq_enq_val -> (Bits(1,ROQ_TAG_ENTRIES) << read_ptr_next)
      ));
    
    // Mem4 <-- Read tutorial
    // compare with trainwreck/caches/sram.v
    val data_array = Mem4(ROQ_TAG_ENTRIES, io.roq_enq_val, io.roq_enq_tag_bits.toUFix, io.roq_enq_data_bits, w_mask = Bits("b11111111",8));
    io.roq_deq_data_bits := data_array(io.roq_enq_tag_bits.toUFix, oe = !io.roq_enq_val, cs = Bool(true));

    // read tag
    when(roq_tag_deq)
    {
      write_ptr <== write_ptr_next;
    }
    // read data
    when(roq_data_deq)
    {
      // vb_array[read_ptr] <= 1'b0
      vb_array <== vb_array & vb_update_mask;
      /*MuxCase(
        vb_array, Array(
          (read_ptr.toInt == ROQ_TAG_ENTRIES-1) -> Cat(Bits("b0",0), vb_array(ROQ_TAG_ENTRIES-2,0)),
          (read_ptr.toInt == 0) -> Cat(vb_array(ROQ_TAG_ENTRIES-1,1),Bits("b0",0)),
          (true) -> Cat(vb_array(ROQ_TAG_ENTIRES-1,read_ptr.toInt+1),Bits("b0",0),vb_array(read_ptr.toInt-1,0))
      ));*/
      roq_deq_data_val <== vb_array(read_ptr_next).toBool;
      read_ptr <== read_ptr_next;
    }
    // write data
    when(io.roq_enq_val)
    {
      // vb_array[roq_enq_tag_bits] <= 1'b1
      vb_array <== vb_array | vb_update_mask;
      /*MuxCase(
        vb_array, Array(
          (io.roq_enq_tag_bits.toInt == ROQ_TAG_ENTRIES-1) -> Cat(Bits("b1",0), vb_array(ROQ_TAG_ENTRIES-2,0)),
          (io.roq_enq_tag_bits.toInt == 0) -> Cat(vb_array(ROQ_TAG_ENTRIES-1,1),Bits("b1",0)),
          (true) -> Cat(vb_array(ROQ_TAG_ENTIRES-1,io.roq_enq_tag_bits.toInt+1),Bits("b1",0),vb_array(io.roq_enq_tag_bits.toInt-1,0))
        ));*/
    }
    otherwise
    {
      roq_deq_data_val <== vb_array(read_ptr).toBool;
    }
  }
}

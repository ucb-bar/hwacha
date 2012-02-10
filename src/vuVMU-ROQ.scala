package hwacha {
  import Chisel._
  import Node._
  import Interface._

  class vuVMU_ROQIO(ROQ_DATA_SIZE :Int, ROQ_TAG_SIZE :Int) extends Bundle
  {
    // interface to issue control
    val roq_deq_tag_bits  = Bits(ROQ_TAG_SIZE, OUTPUT);
    val roq_deq_tag_val		= Bool(OUTPUT);
    val roq_deq_tag_rdy		= Bool(INPUT);
    // interface to responses from D$
    val roq_enq_data_bits   = Bits(ROQ_DATA_SIZE, INPUT);
    val roq_enq_tag_bits    = UFix(ROQ_TAG_SIZE, INPUT);
    val roq_enq_val		      = Bool(INPUT);
    // interface to writeback control
    val roq_deq_data_bits   = Bits(ROQ_DATA_SIZE, OUTPUT);
    val roq_deq_data_val		= Bool(OUTPUT);
    val roq_deq_data_rdy		= Bool(INPUT);
  }

  class vuVMU_ROQ(ROQ_DATA_SIZE :Int, ROQ_TAG_ENTRIES :Int, ROQ_TAG_SIZE :Int) extends Component
  {
    val io = new vuVMU_ROQIO(ROQ_DATA_SIZE, ROQ_TAG_SIZE);
    
    val read_ptr              = Reg(resetVal = UFix(0, ROQ_TAG_SIZE));
    val write_ptr             = Reg(resetVal = UFix(0, ROQ_TAG_SIZE));
    val read_ptr_next         = read_ptr + UFix(1);
    val write_ptr_next        = write_ptr + UFix(1);

    val full                  = (write_ptr_next === read_ptr);
    io.roq_deq_tag_val        := !full;
    io.roq_deq_tag_bits       := write_ptr.toBits;
    val vb_array              = Reg(resetVal=Bits(0, ROQ_TAG_ENTRIES));
    val roq_data_deq          = io.roq_deq_data_rdy && io.roq_deq_data_val;
    val roq_tag_deq           = io.roq_deq_tag_rdy && io.roq_deq_tag_val;
    val roq_deq_data_val_int  = Reg(resetVal = Bool(false));
    io.roq_deq_data_val       := roq_deq_data_val_int;
    val roq_enq_tag_bits_int  = io.roq_enq_tag_bits;

    val vb_update_read = Mux(roq_data_deq, ~(Bits(1) << read_ptr), Fill(ROQ_TAG_ENTRIES,Bits(1)));
    val vb_update_write = Mux(io.roq_enq_val, (Bits(1) << roq_enq_tag_bits_int), Bits(0,ROQ_TAG_ENTRIES));
    
    // Mem4 <-- Read tutorial
    // compare with trainwreck/caches/sram.v
    // depth: Int, wrEnable: Bool, wrAddr: UFix, wrData: Data
    Mem4.setDefaultReadLatency(1);
    val data_array = Mem4(ROQ_TAG_ENTRIES, io.roq_enq_val, roq_enq_tag_bits_int, io.roq_enq_data_bits) //, w_mask = Bits("b11111111",8));
    // readAddr: UFix, oe = output enable, cs = chip select
    io.roq_deq_data_bits := data_array(Mux(roq_data_deq, read_ptr_next, read_ptr), oe = Bool(true), cs = Bool(true));

    // vb_array[read_ptr] <= 1'b0
    // vb_array[roq_enq_tag_bits] <= 1'b1
    vb_array <== (vb_array & vb_update_read) | vb_update_write;
    roq_deq_data_val_int <== vb_array(read_ptr).toBool;
    
    // read tag
    when(roq_tag_deq)
    {
      write_ptr <== write_ptr_next;
    }
    // read data
    when(roq_data_deq)
    {
      roq_deq_data_val_int <== vb_array(read_ptr_next).toBool;
      read_ptr <== read_ptr_next;
    }
  }
}

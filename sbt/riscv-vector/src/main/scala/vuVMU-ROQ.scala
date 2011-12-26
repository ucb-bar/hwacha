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
    val read_ptr = Reg(resetVal = UFix(0, ROQ_TAG_SIZE));
    val write_ptr = Reg(resetVal = UFix(0, ROQ_TAG_SIZE));
    val read_ptr_next = read_ptr + UFix(1);
    val write_ptr_next = write_ptr + UFix(1);
    val full = (write_ptr_next === read_ptr);
    val vb_array = Reg(resetVal = Bits(0, ROQ_TAG_ENTRIES));
    val roq_data_deq = io.roq_deq_data_rdy && io.roq_deq_data_val;
    val roq_tag_deq = io.roq_deq_tag_rdy && io.roq_deq_tag_val;
    val roq_deq_data_val = Reg(resetVal = Bool(false));
    io.roq_deq_data_val := roq_deq_data_val;


    // val data_array = Mem(ROQ_TAG_ENTRIES, <write enable>, wrAddr: Num, wrData: T, wrMask: Bits = null)
    // Mem4 <-- Read tutorial
    // compare with trainwreck/caches/sram.v

    // read tag
    when(roq_tag_deq)
    {
      write_ptr <== write_ptr_next;
    }
    // read data
    when(roq_data_deq)
    {
      vb_array(io.roq_enq_tag_bits.toUFix) <== Bits("b0");
      roq_deq_data_val <== vb_array(read_ptr_next);
      read_ptr <== read_ptr_next;
    }
    // write data
    when(io.roq_enq_val)
    {
      vb_array(io.roq_enq_tag_bits.toUFix) <== Bits("b1");
    }
    otherwise
    {
      roq_deq_data_val <== vb_array(read_ptr);
    }
  }
}

package riscvVector {
  import Chisel._
  import Node._
  import Interface._
  import scala.collection.mutable.{ArrayBuffer}

  class vuVMU_ROQ_tagIO(ROQ_DATA_SIZE:Int = 128, ROQ_TAG_SIZE:Int = 3) extends Bundle
  {
    // interface to responses from D$
    val roq_enq_data_bits = Bits(ROQ_DATA_SIZE, INPUT);
    val roq_enq_tag_bits = Bits(ROQ_TAG_SIZE, INPUT);
    val roq_enq_val		= Bool(INPUT);
    // interface to writeback control
    val roq_deq_data_bits = Bits(ROQ_DATA_SIZE, OUTPUT);
    val roq_deq_data_val		= Bool(OUTPUT);
    val roq_deq_data_rdy		= Bool(INPUT);
  }

  class vuVMU_ROQ_tag(ROQ_DATA_SIZE:Int = 128, ROQ_TAG_ENTRIES:Int = 8, ROQ_TAG_SIZE:Int = 3) extends Component
  {
    val io = new vuVMU_ROQ_tagIO(ROQ_DATA_SIZE, ROQ_TAG_SIZE);
    val read_ptr = Reg(resetVal = UFix(0, ROQ_TAG_SIZE));
    val roq_data_deq = io.roq_deq_data_rdy && io.roq_deq_data_val;
    val vb_array = Reg(resetVal = Bits(0, ROQ_TAG_ENTRIES));

    val data = Reg(resetVal = Bits(0, ROQ_DATA_SIZE));

    io.roq_deq_data_bits := data(read_ptr);
    val roq_deq_data_val = Reg(resetVal = Bits(0, 1));
    io.roq_deq_data_val := roq_deq_data_val;

    when(roq_data_deq)
    {
      vb_array(read_ptr) <== Bits("b0");
      // Non-blocking in Verilog
      read_ptr <== read_ptr + UFix(1);
    }
    when(io.roq_enq_val)
    {
      vb_array(io.roq_enq_tag_bits.toUFix) <== Bits("b1");
      data(io.roq_enq_tag_bits.toUFix) <== io.roq_enq_data_bits;
    }
    otherwise
    {
      roq_deq_data_val <== vb_array(read_ptr);
    }
  }
}

package hwacha {
  import Chisel._
  import Node._
  import Interface._
  import scala.math.{log, ceil}

  class vuVMU_Ctrl_vec_sequencerIO extends Bundle 
  {
    val vlrq_val      = Bool(INPUT);
    val vlrq_rdy      = Bool(INPUT);
    val vmldq_val     = Bool(INPUT);
    val vmldq_rdy     = Bool(INPUT);
    val vsrq_val      = Bool(INPUT);
    val vsrq_rdy      = Bool(INPUT);
    val vsackq_val    = Bool(INPUT);
    val vsackq_rdy    = Bool(INPUT);
    val vlr_fulfilled = Bool(OUTPUT);
    val vsr_fulfilled = Bool(OUTPUT);
  }

  class vuVMU_Ctrl_vec_sequencer(SRQ_SIZE :Int, LRQ_SIZE :Int) extends Component 
  {
    def ceilLog2(x : Int) = ceil(log(x)/log(2.0)).toInt;
    val io = new vuVMU_Ctrl_vec_sequencerIO();

    val vlr_inflight = Reg(resetVal = UFix(0, ceilLog2(LRQ_SIZE)));
    val vsr_inflight = Reg(resetVal = UFix(0, ceilLog2(SRQ_SIZE)));

    io.vlr_fulfilled := (vlr_inflight === UFix(0));
    io.vsr_fulfilled := (vsr_inflight === UFix(0));

    when( io.vlrq_val && io.vlrq_rdy )
    {
      vlr_inflight <== vlr_inflight + UFix(1);
    }
    when( io.vmldq_val && io.vmldq_rdy && vlr_inflight > UFix(0) )
    {
      vlr_inflight <== vlr_inflight - UFix(1);
    }
    when( io.vlrq_val && io.vlrq_rdy && io.vmldq_val && io.vmldq_rdy )
    {
      vlr_inflight <== vlr_inflight;
    }
    // bottom when takes priority

    when( io.vsrq_val && io.vsrq_rdy )
    {
      vsr_inflight <== vsr_inflight + UFix(1);
    }
    when( io.vsackq_val && io.vsackq_rdy && vsr_inflight > UFix(0) )
    {
      vsr_inflight <== vsr_inflight - UFix(1);
    }
    when( io.vsrq_val && io.vsrq_rdy && io.vsackq_val && io.vsackq_rdy )
    {
      vsr_inflight <== vsr_inflight;
    }
  }
}


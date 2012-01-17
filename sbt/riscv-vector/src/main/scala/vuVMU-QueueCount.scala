package riscvVector {
  import Chisel._
  import Node._
  import scala.math.{log, ceil}

  class vuVMU_QueueCountIO extends Bundle
  {
    val enq		= Bool('input);
    val deq		= Bool('input);
    val ready		= Bool('output);
  }


  class vuVMU_QueueCount(reset_cnt : Int, ready_cnt : Int, max_cnt : Int) extends Component
  {
    def ceilLog2(x : Int)=ceil(log(x)/log(2.0)).toInt;

    val io = new vuVMU_QueueCountIO();
    val count = Reg(resetVal = UFix(reset_cnt, ceilLog2(max_cnt)+1));
    io.ready := count >= UFix(ready_cnt);
    
    when(io.enq ^ io.deq)
    {
      when(io.enq) {count <== count + UFix(1);}
      otherwise {count <== count - UFix(1);}
    }
  }
}

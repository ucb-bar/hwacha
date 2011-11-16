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


  class vuVMU_QueueCount(reset_cnt : Int = 0, ready_cnt : Int = 8, max_cnt : Int = 12) extends Component
  {
    def ceilLog2(x : Int)=ceil(log(x)).toInt;

    val io = new vuVMU_QueueCountIO();
    val count = Reg(resetVal = UFix(reset_cnt, ceilLog2(max_cnt)));
    io.ready := count >= UFix(ready_cnt);
    
    when(io.enq ^ io.deq)
    {
      when(io.enq) {count <== count + UFix(1);}
      otherwise {count <== count - UFix(1);}
    }
  }
}

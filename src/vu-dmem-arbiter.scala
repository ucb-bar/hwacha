package hwacha {
  import Chisel._
  import Node._
  import Interface._
  import queues._
  import scala.math.{log}

  class vmldqIO extends Bundle
  {
    val bits    = Bits(128, OUTPUT);
    val valid   = Bool(OUTPUT);
    val rdy     = Bool(INPUT);
  }

  class vlrqIO extends Bundle
  {
    // 28 bits for address
    val bits    = Bits(28, INPUT);
    val valid   = Bool(INPUT);
    val rdy     = Bool(OUTPUT);
  }

  class vsrqIO extends Bundle
  {
    // 28 bits for address + 128 bits for data + 16 bytes for wmask
    val bits    = Bits(28+16+128, INPUT);
    val valid   = Bool(INPUT);
    val rdy     = Bool(OUTPUT);
  }

  class vu_dmem_arbiterIO extends Bundle
  {
    val vlrq        = new vlrqIO();
    val vsrq        = new vsrqIO();
    val vmldq       = new vmldqIO();
    val dcachereq   = new vec_dcachereqIO();
    val dcacheresp  = new vec_dcacherespIO();
  }
  
  class vu_dmem_arbiter extends Component
  {
    def log2(x : Double) = (log(x)/log(2)).toInt;
    
    val io    = new vu_dmem_arbiterIO();
    
    val VMU_QUEUE_ENTRIES = 16;
    val VMU_QUEUE_LEVEL   = 8;

    val roq     = new vuVMU_ROQ(130, 256, 8);
    val vlrq    = new queueSimplePF(28, VMU_QUEUE_ENTRIES, log2(VMU_QUEUE_ENTRIES)); 
    val vsrq    = new queueSimplePF(28+128+16, VMU_QUEUE_ENTRIES, log2(VMU_QUEUE_ENTRIES));
    val vmldq   = new queueSimplePF(128, VMU_QUEUE_ENTRIES, log2(VMU_QUEUE_ENTRIES));

    vlrq.io.enq_bits  := io.vlrq.bits;
    vlrq.io.enq_val   := io.vlrq.valid;
    io.vlrq.rdy       := vlrq.io.enq_rdy;

    vsrq.io.enq_bits  := io.vsrq.bits;
    vsrq.io.enq_val   := io.vsrq.valid;
    io.vsrq.rdy       := vsrq.io.enq_rdy;

    // D$ to ROQ
    roq.io.roq_enq_data_bits  := io.dcacheresp.data;
    roq.io.roq_enq_tag_bits   := io.dcacheresp.tag(7,0);
    roq.io.roq_enq_val        := io.dcacheresp.valid && !io.dcacheresp.tag(11).toBool;

    // ROQ to vmldq
    vmldq.io.enq_bits       := roq.io.roq_deq_data_bits;
    vmldq.io.enq_val        := roq.io.roq_deq_data_val;
    roq.io.roq_deq_data_rdy := vmldq.io.enq_rdy;

    // vmldq to load-wb
    io.vmldq.bits    := vmldq.io.deq_bits;
    io.vmldq.valid   := vmldq.io.deq_val;
    vmldq.io.deq_rdy := io.vmldq.rdy;

    // stores are given priority over loads
    io.dcachereq.valid  := vlrq.io.deq_val || vsrq.io.deq_val;
    vlrq.io.deq_rdy     := Mux(vsrq.io.deq_val, Bool(false), io.dcachereq.rdy);
    vsrq.io.deq_rdy     := io.dcachereq.rdy;
    io.dcachereq.data   := vsrq.io.deq_bits(127, 0);
    io.dcachereq.wmask  := vsrq.io.deq_bits(143, 128);
    io.dcachereq.addr   := Mux(vsrq.io.deq_val, vsrq.io.deq_bits(171, 144), vlrq.io.deq_bits);
    io.dcachereq.tag    := Mux(vsrq.io.deq_val, Bits("h800", 12), Cat(Bits("b0", 4), roq.io.roq_deq_tag_bits));
    io.dcachereq.op     := Mux(vsrq.io.deq_val, Bits("b0001", 4), Bits("b0000", 4));
  }
}

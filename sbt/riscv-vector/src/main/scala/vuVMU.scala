package riscvVector {
  import Chisel._
  import Node._
  import Interface._
  import queues._
  import scala.math.{log}
  
  class vuVMUIO extends Bundle
  {
    val vmu_vcmdq     = new vmcmdqIO();
    val vmu_vbaseq    = new vmimmqIO();
    val vmu_vstrideq  = new vmstrideqIO();
    val vmu_vackq     = new vmrespqIO();
    val vmu_utcmdq    = new utmcmdqIO();
    val vmu_utimmq    = new utmimmqIO();
    val vmu_utackq    = new utmrespqIO();

    val lane_vldq_deq_bits	= Bits(65, OUTPUT);
    val lane_vldq_deq_val	= Bool(OUTPUT);
    val lane_vldq_deq_rdy	= Bool(INPUT);

    val lane_vsdq_enq_bits	= Bits(65, INPUT);
    val lane_vsdq_enq_val	= Bool(INPUT);
    val lane_vsdq_enq_rdy	= Bool(OUTPUT);

    val lane_utaq_enq_bits	= Bits(32, INPUT);
    val lane_utaq_enq_val	= Bool(INPUT);
    val lane_utaq_enq_rdy	= Bool(OUTPUT);

    val lane_utldq_deq_bits	= Bits(65, OUTPUT);
    val lane_utldq_deq_val	= Bool(OUTPUT);
    val lane_utldq_deq_rdy	= Bool(INPUT);

    val lane_utsdq_enq_bits	= Bits(65, INPUT);
    val lane_utsdq_enq_val	= Bool(INPUT);
    val lane_utsdq_enq_rdy	= Bool(OUTPUT);

    val dmem_req_ut         = new ut_dcachereqIO();
    val dmem_resp_ut        = new ut_dcacherespIO(); 

    val dmem_req_vec        = new vec_dcachereqIO();
    val dmem_resp_vec       = new vec_dcacherespIO();
  }

  
  class vuVMU extends Component
  {
    val io = new vuVMUIO();
  
    val VMU_QUEUE_ENTRIES = 16;
    val VMU_QUEUE_LEVEL   = 8;

    def log2(x : Double) = (log(x)/log(2)).toInt;

    val ctrl_vec    = new vuVMU_Ctrl_vec();
    val ctrl_ut     = new vuVMU_Ctrl_ut();
    // queues
    // vldq queue and capacity counter
    val vldq_count = new vuVMU_QueueCount(0, VMU_QUEUE_LEVEL+1, VMU_QUEUE_ENTRIES);
    val vldq = new queueSimplePF(65, VMU_QUEUE_ENTRIES, log2(VMU_QUEUE_ENTRIES));
    // vsdq queue and capacity counter
    val vsdq_count = new vuVMU_QueueCount(VMU_QUEUE_ENTRIES, VMU_QUEUE_LEVEL+1, VMU_QUEUE_ENTRIES);
    val vsdq = new queueSimplePF(65, VMU_QUEUE_ENTRIES, log2(VMU_QUEUE_ENTRIES));
    // utaq queue and capacity counter
    val utaq_count = new vuVMU_QueueCount(VMU_QUEUE_ENTRIES, VMU_QUEUE_LEVEL+1, VMU_QUEUE_ENTRIES);
    val utaq = new queueSimplePF(32, VMU_QUEUE_ENTRIES, log2(VMU_QUEUE_ENTRIES));
    // utldq queue and capacity counter
    val utldq_count = new vuVMU_QueueCount(0, VMU_QUEUE_LEVEL+1, VMU_QUEUE_ENTRIES);
    val utldq = new queueSimplePF(65, VMU_QUEUE_ENTRIES, log2(VMU_QUEUE_ENTRIES));
    // utsdq queue and capacity counter
    val utsdq_count = new vuVMU_QueueCount(VMU_QUEUE_ENTRIES, VMU_QUEUE_LEVEL+1, VMU_QUEUE_ENTRIES);
    val utsdq = new queueSimplePF(65, VMU_QUEUE_ENTRIES, log2(VMU_QUEUE_ENTRIES));

    // ctrl_vec
    ctrl_vec.io.vmcmdq      ^^ io.vmu_vcmdq;
    ctrl_vec.io.vmimmq      ^^ io.vmu_vbaseq;
    ctrl_vec.io.vmstrideq   ^^ io.vmu_vstrideq;
    ctrl_vec.io.vmrespq     ^^ io.vmu_vackq;
    ctrl_vec.io.dcachereq   ^^ io.dmem_req_vec; 
    ctrl_vec.io.dcacheresp  ^^ io.dmem_resp_vec; 

    ctrl_vec.io.vldq.enq_rdy    := vldq.io.enq_rdy;
    vldq.io.enq_val             := ctrl_vec.io.vldq.enq_val;
    vldq.io.enq_bits            := ctrl_vec.io.vldq.enq_bits;
   
    ctrl_vec.io.vsdq_deq.valid  := vsdq.io.deq_val;
    ctrl_vec.io.vsdq_deq.bits   := vsdq.io.deq_bits;
    vsdq.io.deq_rdy             := ctrl_vec.io.vsdq_deq.rdy;

    // ctrl_ut
    ctrl_ut.io.utmcmdq    ^^ io.vmu_utcmdq;
    ctrl_ut.io.utmimmq    ^^ io.vmu_utimmq;
    ctrl_ut.io.utmrespq   ^^ io.vmu_utackq;
    ctrl_ut.io.dcachereq  ^^ io.dmem_req_ut;
    ctrl_ut.io.dcacheresp ^^ io.dmem_resp_ut;

    ctrl_ut.io.utaq_deq.valid   := utaq.io.deq_val;
    ctrl_ut.io.utaq_deq.bits    := utaq.io.deq_bits;
    utaq.io.deq_rdy             := ctrl_ut.io.utaq_deq.rdy;

    utldq.io.enq_val            := ctrl_ut.io.utldq.enq_val;
    utldq.io.enq_bits           := ctrl_ut.io.utldq.enq_bits;
    ctrl_ut.io.utldq.enq_rdy    := utldq.io.enq_rdy;

    ctrl_ut.io.utsdq_deq.valid   := utsdq.io.deq_val;
    ctrl_ut.io.utsdq_deq.bits    := utsdq.io.deq_bits;
    utsdq.io.deq_rdy             := ctrl_ut.io.utsdq_deq.rdy;
    
    // lane i/o
    io.lane_vldq_deq_bits     := vldq.io.deq_bits;
    io.lane_vldq_deq_val      := vldq_count.io.ready || ctrl_vec.io.vldq.wb_done;
    vldq_count.io.deq         := io.lane_vldq_deq_rdy;
    vldq.io.deq_rdy           := io.lane_vldq_deq_rdy;
    ctrl_vec.io.vldq.deq_rdy  := io.lane_vldq_deq_rdy;
   
    vsdq.io.enq_bits          := io.lane_vsdq_enq_bits;
    io.lane_vsdq_enq_rdy      := vsdq_count.io.ready;
    vsdq.io.enq_val           := io.lane_vsdq_enq_val;
    vsdq_count.io.deq         := io.lane_vsdq_enq_val;
    
    io.lane_utaq_enq_rdy      := utaq_count.io.ready || ctrl_ut.io.utldq.wb_done;
    utaq.io.enq_bits          := io.lane_utaq_enq_bits;
    utaq.io.enq_val           := io.lane_utaq_enq_val;
    utaq_count.io.deq         := io.lane_utaq_enq_val;

    io.lane_utldq_deq_bits    := utldq.io.deq_bits;
    io.lane_utldq_deq_val     := utldq_count.io.ready || ctrl_ut.io.utldq.wb_done;
    utldq_count.io.deq        := io.lane_utldq_deq_rdy;
    utldq.io.deq_rdy          := io.lane_utldq_deq_rdy;
    ctrl_ut.io.utldq.deq_rdy  := io.lane_utldq_deq_rdy;

    io.lane_utsdq_enq_rdy     := utsdq_count.io.ready;
    utsdq.io.enq_bits         := io.lane_utsdq_enq_bits;
    utsdq.io.enq_val          := io.lane_utsdq_enq_val;
    utsdq_count.io.deq        := io.lane_utsdq_enq_val;

    // count enq
    vldq_count.io.enq   := vldq.io.enq_val && ctrl_vec.io.vldq.enq_rdy;
    vsdq_count.io.enq   := vsdq.io.deq_val && ctrl_vec.io.vsdq_deq.rdy;
    utaq_count.io.enq   := utaq.io.deq_val && ctrl_ut.io.utaq_deq.rdy;
    utldq_count.io.enq  := utldq.io.enq_val && ctrl_ut.io.utldq.enq_rdy;
    utsdq_count.io.enq  := utsdq.io.deq_val && ctrl_ut.io.utsdq_deq.rdy;

  }
}

package hwacha {
  import Chisel._
  import Node._
  import Interface._
  import queues._
  import scala.math.{log}
  
  class vuVMUIO extends Bundle
  {
    val vxu_to_vmu    = new io_vxu_to_vmu().asInput();

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
    val vldq_count = new vuVMU_QueueCount(0, VMU_QUEUE_LEVEL+1, VMU_QUEUE_ENTRIES, true);
    val vldq = new queueSimplePF(65, VMU_QUEUE_ENTRIES);
    // vsdq queue and capacity counter
    val vsdq_count = new vuVMU_QueueCount(VMU_QUEUE_ENTRIES, VMU_QUEUE_LEVEL+1, VMU_QUEUE_ENTRIES, true);
    val vsdq = new queueSimplePF(65, VMU_QUEUE_ENTRIES);
    // utaq queue and capacity counter
    val utaq_count = new vuVMU_QueueCount(VMU_QUEUE_ENTRIES, VMU_QUEUE_LEVEL+1, VMU_QUEUE_ENTRIES, true);
    val utaq = new queueSimplePF(32, VMU_QUEUE_ENTRIES);
    // utldq queue and capacity counter
    val utldq_count = new vuVMU_QueueCount(0, VMU_QUEUE_LEVEL+1, VMU_QUEUE_ENTRIES, true);
    val utldq = new queueSimplePF(65, VMU_QUEUE_ENTRIES);
    // utsdq queue and capacity counter
    val utsdq_count = new vuVMU_QueueCount(VMU_QUEUE_ENTRIES, VMU_QUEUE_LEVEL+1, VMU_QUEUE_ENTRIES, true);
    val utsdq = new queueSimplePF(65, VMU_QUEUE_ENTRIES);

    // ctrl_vec
    ctrl_vec.io.vmcmdq      <> io.vmu_vcmdq;
    ctrl_vec.io.vmimmq      <> io.vmu_vbaseq;
    ctrl_vec.io.vmstrideq   <> io.vmu_vstrideq;
    ctrl_vec.io.vmrespq     <> io.vmu_vackq;
    ctrl_vec.io.dcachereq   <> io.dmem_req_vec; 
    ctrl_vec.io.dcacheresp  <> io.dmem_resp_vec; 

    ctrl_vec.io.vldq.enq_rdy    := vldq.io.enq.ready;
    vldq.io.enq.valid             := ctrl_vec.io.vldq.enq_val;
    vldq.io.enq.bits            := ctrl_vec.io.vldq.enq_bits;
   
    ctrl_vec.io.vsdq_deq.valid  := vsdq.io.deq.valid;
    ctrl_vec.io.vsdq_deq.bits   := vsdq.io.deq.bits;
    vsdq.io.deq.ready             := ctrl_vec.io.vsdq_deq.rdy;

    // ctrl_ut
    ctrl_ut.io.utmcmdq    <> io.vmu_utcmdq;
    ctrl_ut.io.utmimmq    <> io.vmu_utimmq;
    ctrl_ut.io.utmrespq   <> io.vmu_utackq;
    ctrl_ut.io.dcachereq  <> io.dmem_req_ut;
    ctrl_ut.io.dcacheresp <> io.dmem_resp_ut;

    ctrl_ut.io.utaq_deq.valid   := utaq.io.deq.valid;
    ctrl_ut.io.utaq_deq.bits    := utaq.io.deq.bits;
    utaq.io.deq.ready             := ctrl_ut.io.utaq_deq.rdy;

    utldq.io.enq.valid            := ctrl_ut.io.utldq.enq_val;
    utldq.io.enq.bits           := ctrl_ut.io.utldq.enq_bits;
    ctrl_ut.io.utldq.enq_rdy    := utldq.io.enq.ready;

    ctrl_ut.io.utsdq_deq.valid   := utsdq.io.deq.valid;
    ctrl_ut.io.utsdq_deq.bits    := utsdq.io.deq.bits;
    utsdq.io.deq.ready             := ctrl_ut.io.utsdq_deq.rdy;
    
    // lane i/o
    io.lane_vldq_deq_bits     := vldq.io.deq.bits;
    io.lane_vldq_deq_val      := vldq_count.io.ready 
    vldq_count.io.qcnt        := io.vxu_to_vmu.qcnt;
    vldq_count.io.dec         := io.lane_vldq_deq_rdy;
    vldq.io.deq.ready           := io.lane_vldq_deq_rdy;
    ctrl_vec.io.vldq.deq_rdy  := io.lane_vldq_deq_rdy;
   
    vsdq.io.enq.bits          := io.lane_vsdq_enq_bits;
    io.lane_vsdq_enq_rdy      := vsdq_count.io.ready;
    vsdq.io.enq.valid           := io.lane_vsdq_enq_val;
    vsdq_count.io.qcnt        := io.vxu_to_vmu.qcnt;
    vsdq_count.io.dec         := io.lane_vsdq_enq_val;
    
    io.lane_utaq_enq_rdy      := utaq_count.io.ready
    utaq.io.enq.bits          := io.lane_utaq_enq_bits;
    utaq.io.enq.valid           := io.lane_utaq_enq_val;
    utaq_count.io.qcnt        := io.vxu_to_vmu.qcnt;
    utaq_count.io.dec         := io.lane_utaq_enq_val;

    io.lane_utldq_deq_bits    := utldq.io.deq.bits;
    io.lane_utldq_deq_val     := utldq_count.io.ready;
    utldq_count.io.qcnt       := io.vxu_to_vmu.qcnt;
    utldq_count.io.dec        := io.lane_utldq_deq_rdy;
    utldq.io.deq.ready          := io.lane_utldq_deq_rdy;
    ctrl_ut.io.utldq.deq_rdy  := io.lane_utldq_deq_rdy;

    io.lane_utsdq_enq_rdy     := utsdq_count.io.ready;
    utsdq.io.enq.bits         := io.lane_utsdq_enq_bits;
    utsdq.io.enq.valid          := io.lane_utsdq_enq_val;
    utsdq_count.io.qcnt       := io.vxu_to_vmu.qcnt;
    utsdq_count.io.dec        := io.lane_utsdq_enq_val;

    // count enq
    vldq_count.io.inc   := vldq.io.enq.valid && ctrl_vec.io.vldq.enq_rdy;
    vsdq_count.io.inc   := vsdq.io.deq.valid && ctrl_vec.io.vsdq_deq.rdy;
    utaq_count.io.inc   := utaq.io.deq.valid && ctrl_ut.io.utaq_deq.rdy;
    utldq_count.io.inc  := utldq.io.enq.valid && ctrl_ut.io.utldq.enq_rdy;
    utsdq_count.io.inc  := utsdq.io.deq.valid && ctrl_ut.io.utsdq_deq.rdy;

  }
}

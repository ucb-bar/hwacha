package hwacha {
  import Chisel._
  import Node._
  import Interface._
  import queues._
  import scala.math.{log}
  
  class vuVMUIO extends Bundle
  {
    val vmu_vcmdq     = new vmcmdqIO()
    val vmu_vbaseq    = new vmimmqIO()
    val vmu_vstrideq  = new vmstrideqIO()
    val vmu_vackq     = new vmrespqIO()
    val vmu_utcmdq    = new utmcmdqIO()
    val vmu_utimmq    = new utmimmqIO()
    val vmu_utackq    = new utmrespqIO()

    val vldq_enq_bits = Bits(65, OUTPUT)
    val vldq_enq_valid = Bool(OUTPUT)
    val vldq_enq_ready = Bool(INPUT)

    val vsdq_deq_bits = Bits(65, INPUT)
    val vsdq_deq_valid = Bool(INPUT)
    val vsdq_deq_ready = Bool(OUTPUT)

    val utaq_deq_bits = Bits(32, INPUT)
    val utaq_deq_valid = Bool(INPUT)
    val utaq_deq_ready = Bool(OUTPUT)

    val utldq_enq_bits = Bits(65, OUTPUT)
    val utldq_enq_valid = Bool(OUTPUT)
    val utldq_enq_ready = Bool(INPUT)

    val utsdq_deq_bits = Bits(65, INPUT)
    val utsdq_deq_valid = Bool(INPUT)
    val utsdq_deq_ready = Bool(OUTPUT)

    val dmem_req_ut = new ut_dcachereqIO()
    val dmem_resp_ut = new ut_dcacherespIO() 

    val dmem_req_vec = new vec_dcachereqIO()
    val dmem_resp_vec = new vec_dcacherespIO()
  }

  
  class vuVMU extends Component
  {
    val io = new vuVMUIO()
  
    val VMU_QUEUE_ENTRIES = 16
    val VMU_QUEUE_LEVEL   = 8

    def log2(x : Double) = (log(x)/log(2)).toInt

    val ctrl_vec    = new vuVMU_Ctrl_vec()
    val ctrl_ut     = new vuVMU_Ctrl_ut()

    // ctrl_vec
    ctrl_vec.io.vmcmdq      <> io.vmu_vcmdq
    ctrl_vec.io.vmimmq      <> io.vmu_vbaseq
    ctrl_vec.io.vmstrideq   <> io.vmu_vstrideq
    ctrl_vec.io.vmrespq     <> io.vmu_vackq
    ctrl_vec.io.dcachereq   <> io.dmem_req_vec 
    ctrl_vec.io.dcacheresp  <> io.dmem_resp_vec 

    ctrl_vec.io.vldq.enq_rdy    := io.vldq_enq_ready
    io.vldq_enq_valid           := ctrl_vec.io.vldq.enq_val
    io.vldq_enq_bits            := ctrl_vec.io.vldq.enq_bits
   
    ctrl_vec.io.vsdq_deq.valid  := io.vsdq_deq_valid
    ctrl_vec.io.vsdq_deq.bits   := io.vsdq_deq_bits
    io.vsdq_deq_ready           := ctrl_vec.io.vsdq_deq.rdy

    // ctrl_ut
    ctrl_ut.io.utmcmdq    <> io.vmu_utcmdq
    ctrl_ut.io.utmimmq    <> io.vmu_utimmq
    ctrl_ut.io.utmrespq   <> io.vmu_utackq
    ctrl_ut.io.dcachereq  <> io.dmem_req_ut
    ctrl_ut.io.dcacheresp <> io.dmem_resp_ut

    ctrl_ut.io.utaq_deq.valid   := io.utaq_deq_valid
    ctrl_ut.io.utaq_deq.bits    := io.utaq_deq_bits
    io.utaq_deq_ready           := ctrl_ut.io.utaq_deq.rdy

    io.utldq_enq_valid          := ctrl_ut.io.utldq.enq_val
    io.utldq_enq_bits           := ctrl_ut.io.utldq.enq_bits
    ctrl_ut.io.utldq.enq_rdy    := io.utldq_enq_ready

    ctrl_ut.io.utsdq_deq.valid  := io.utsdq_deq_valid
    ctrl_ut.io.utsdq_deq.bits   := io.utsdq_deq_bits
    io.utsdq_deq_ready          := ctrl_ut.io.utsdq_deq.rdy
  }
}

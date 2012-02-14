package hwacha

import Chisel._
import Node._
import Config._
import Interface._

class vu extends Component
{
  val io = new io_vu()

  val vxu = new vuVXU()

  val vmu_vcmdq = VC_SIMPLE_QUEUE(VMCMD_SZ, 16)
  val vmu_vbaseq = VC_SIMPLE_QUEUE(VMIMM_SZ, 16)
  val vmu_vstrideq = VC_SIMPLE_QUEUE(VMSTRIDE_SZ, 16)
  val vaq_count = new vuVMU_QueueCount(16, 9, 16, true)

  val vldq = VC_SIMPLE_QUEUE(SZ_DATA, 16)
  val vldq_count = new vuVMU_QueueCount(0, 9, 16, true)

  val vsdq = VC_SIMPLE_QUEUE(SZ_DATA, 16)
  val vsdq_count = new vuVMU_QueueCount(16, 9, 16, true)

  val vmu_utcmdq = VC_SIMPLE_QUEUE(UTMCMD_SZ, 16)
  val vmu_utimmq = VC_SIMPLE_QUEUE(UTMIMM_SZ, 16)
  val utaq = VC_SIMPLE_QUEUE(SZ_ADDR, 16)
  val utaq_count = new vuVMU_QueueCount(16, 9, 16, true)

  val utldq = VC_SIMPLE_QUEUE(SZ_DATA, 16)
  val utldq_count = new vuVMU_QueueCount(0, 9, 16, true)

  val utsdq = VC_SIMPLE_QUEUE(SZ_DATA, 16)
  val utsdq_count = new vuVMU_QueueCount(16, 9, 16, true)

  val vmu = new vuVMU()

  // vxu
  io.illegal <> vxu.io.illegal

  vxu.io.vxu_cmdq <> io.vec_cmdq
  vxu.io.vxu_immq <> io.vec_ximm1q
  vxu.io.vxu_imm2q <> io.vec_ximm2q

  vxu.io.vmu_vcmdq <> vmu_vcmdq.io.enq
  vxu.io.vmu_vbaseq <> vmu_vbaseq.io.enq
  vxu.io.vmu_vstrideq <> vmu_vstrideq.io.enq

  vxu.io.vmu_utcmdq <> vmu_utcmdq.io.enq
  vxu.io.vmu_utimmq <> vmu_utimmq.io.enq

  vxu.io.vec_ackq <> io.vec_ackq

  vxu.io.cp_imul_req <> io.cp_imul_req
  vxu.io.cp_imul_resp <> io.cp_imul_resp

  vxu.io.cp_fma_req <> io.cp_fma_req
  vxu.io.cp_fma_resp <> io.cp_fma_resp

  vxu.io.imem_req <> io.imem_req
  vxu.io.imem_resp <> io.imem_resp


  // vaq
  vxu.io.lane_vaq.ready := vaq_count.io.watermark
  vaq_count.io.qcnt := vxu.io.vxu_to_vmu.qcnt
  vaq_count.io.inc := vmu_vbaseq.io.deq.valid && vmu_vbaseq.io.deq.ready
  vaq_count.io.dec := vxu.io.vmu_vbaseq.valid


  // vldq
  vmu.io.vldq_enq_ready := vldq.io.enq.ready
  vldq.io.enq.valid := vmu.io.vldq_enq_valid
  vldq.io.enq.bits := vmu.io.vldq_enq_bits

  vldq.io.deq.ready := vxu.io.lane_vldq.ready
  vxu.io.lane_vldq.valid := vldq_count.io.watermark // vldq.deq.valid
  vxu.io.lane_vldq.bits := vldq.io.deq.bits

  vldq_count.io.qcnt := vxu.io.vxu_to_vmu.qcnt
  vldq_count.io.inc := vldq.io.enq.ready && vmu.io.vldq_enq_valid
  vldq_count.io.dec := vxu.io.lane_vldq.ready


  // vsdq
  vxu.io.lane_vsdq.ready := vsdq_count.io.watermark // vsdq.io.enq.ready
  vsdq.io.enq.valid := vxu.io.lane_vsdq.valid
  vsdq.io.enq.bits := vxu.io.lane_vsdq.bits

  vsdq.io.deq.ready := vmu.io.vsdq_deq_ready
  vmu.io.vsdq_deq_valid := vsdq.io.deq.valid
  vmu.io.vsdq_deq_bits := vsdq.io.deq.bits

  vsdq_count.io.qcnt := vxu.io.vxu_to_vmu.qcnt
  vsdq_count.io.inc := vmu.io.vsdq_deq_ready && vsdq.io.deq.valid
  vsdq_count.io.dec := vxu.io.lane_vsdq.valid


  // utaq
  vxu.io.lane_utaq.ready := utaq_count.io.watermark // utaq.io.enq.ready
  utaq.io.enq.valid := vxu.io.lane_utaq.valid
  utaq.io.enq.bits := vxu.io.lane_utaq.bits

  utaq.io.deq.ready := vmu.io.utaq_deq_ready
  vmu.io.utaq_deq_valid := utaq.io.deq.valid
  vmu.io.utaq_deq_bits := utaq.io.deq.bits

  utaq_count.io.qcnt := vxu.io.vxu_to_vmu.qcnt
  utaq_count.io.inc := vmu.io.utaq_deq_ready && utaq.io.deq.valid
  utaq_count.io.dec := vxu.io.lane_utaq.valid


  // utldq
  vmu.io.utldq_enq_ready := utldq.io.enq.ready
  utldq.io.enq.valid := vmu.io.utldq_enq_valid
  utldq.io.enq.bits := vmu.io.utldq_enq_bits

  utldq.io.deq.ready := vxu.io.lane_utldq.ready
  vxu.io.lane_utldq.valid := utldq_count.io.watermark // utldq.deq.valid
  vxu.io.lane_utldq.bits := utldq.io.deq.bits

  utldq_count.io.qcnt := vxu.io.vxu_to_vmu.qcnt
  utldq_count.io.inc := utldq.io.enq.ready && vmu.io.utldq_enq_valid
  utldq_count.io.dec := vxu.io.lane_utldq.ready


  // utsdq
  vxu.io.lane_utsdq.ready := utsdq_count.io.watermark // utsdq.io.enq.ready
  utsdq.io.enq.valid := vxu.io.lane_utsdq.valid
  utsdq.io.enq.bits := vxu.io.lane_utsdq.bits

  utsdq.io.deq.ready := vmu.io.utsdq_deq_ready
  vmu.io.utsdq_deq_valid := utsdq.io.deq.valid
  vmu.io.utsdq_deq_bits := utsdq.io.deq.bits

  utsdq_count.io.qcnt := vxu.io.vxu_to_vmu.qcnt
  utsdq_count.io.inc := vmu.io.utsdq_deq_ready && utsdq.io.deq.valid
  utsdq_count.io.dec := vxu.io.lane_utsdq.valid


  // vmu
  vmu.io.vmu_vcmdq.bits <> vmu_vcmdq.io.deq.bits
  vmu.io.vmu_vcmdq.valid <> vmu_vcmdq.io.deq.valid
  vmu.io.vmu_vcmdq.rdy <> vmu_vcmdq.io.deq.ready

  vmu.io.vmu_vbaseq.bits <> vmu_vbaseq.io.deq.bits
  vmu.io.vmu_vbaseq.valid <> vmu_vbaseq.io.deq.valid
  vmu.io.vmu_vbaseq.rdy <> vmu_vbaseq.io.deq.ready
  
  vmu.io.vmu_vstrideq.bits <> vmu_vstrideq.io.deq.bits
  vmu.io.vmu_vstrideq.valid <> vmu_vstrideq.io.deq.valid
  vmu.io.vmu_vstrideq.rdy <> vmu_vstrideq.io.deq.ready

  vmu.io.vmu_vackq.bits <> vxu.io.vmu_vackq.bits
  vmu.io.vmu_vackq.valid <> vxu.io.vmu_vackq.valid
  vmu.io.vmu_vackq.rdy <> vxu.io.vmu_vackq.ready
  
  vmu.io.vmu_utcmdq.bits <> vmu_utcmdq.io.deq.bits
  vmu.io.vmu_utcmdq.valid <> vmu_utcmdq.io.deq.valid
  vmu.io.vmu_utcmdq.rdy <> vmu_utcmdq.io.deq.ready

  vmu.io.vmu_utimmq.bits <> vmu_utimmq.io.deq.bits
  vmu.io.vmu_utimmq.valid <> vmu_utimmq.io.deq.valid
  vmu.io.vmu_utimmq.rdy <> vmu_utimmq.io.deq.ready

  vmu.io.vmu_utackq.bits <> vxu.io.vmu_utackq.bits
  vmu.io.vmu_utackq.valid <> vxu.io.vmu_utackq.valid
  vmu.io.vmu_utackq.rdy <> vxu.io.vmu_utackq.ready

  vmu.io.dmem_req_vec.addr <> io.dmem_req_vec.bits.addr
  vmu.io.dmem_req_vec.op <> io.dmem_req_vec.bits.op
  vmu.io.dmem_req_vec.data <> io.dmem_req_vec.bits.data
  vmu.io.dmem_req_vec.wmask <> io.dmem_req_vec.bits.wmask
  vmu.io.dmem_req_vec.tag <> io.dmem_req_vec.bits.tag
  vmu.io.dmem_req_vec.valid <> io.dmem_req_vec.valid
  vmu.io.dmem_req_vec.rdy <> io.dmem_req_vec.ready
  vmu.io.dmem_resp_vec.valid <> io.dmem_resp_vec.valid

  vmu.io.dmem_req_ut.addr <> io.dmem_req_ut.bits.addr
  vmu.io.dmem_req_ut.op <> io.dmem_req_ut.bits.op
  vmu.io.dmem_req_ut.data <> io.dmem_req_ut.bits.data
  vmu.io.dmem_req_ut.wmask <> io.dmem_req_ut.bits.wmask
  vmu.io.dmem_req_ut.tag <> io.dmem_req_ut.bits.tag
  vmu.io.dmem_req_ut.valid <> io.dmem_req_ut.valid
  vmu.io.dmem_req_ut.rdy <> io.dmem_req_ut.ready
  vmu.io.dmem_resp_ut.valid <> io.dmem_resp_ut.valid

  vmu.io.dmem_resp_ut.tag <> io.dmem_resp_ut.bits.tag
  vmu.io.dmem_resp_ut.data <> io.dmem_resp_ut.bits.data
  vmu.io.dmem_resp_vec.tag <> io.dmem_resp_vec.bits.tag
  vmu.io.dmem_resp_vec.data <> io.dmem_resp_vec.bits.data
}

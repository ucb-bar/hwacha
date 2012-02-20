package hwacha

import Chisel._
import Node._
import Config._
import Interface._
import queues._

class vu extends Component
{
  val io = new io_vu()

  val vcmdq = VC_SIMPLE_QUEUE(VCMD_SZ, 16)
  val vximm1q = VC_SIMPLE_QUEUE(VIMM_SZ, 16)
  val vximm2q = VC_SIMPLE_QUEUE(VSTRIDE_SZ, 16)

  vcmdq.io.enq <> io.vec_cmdq
  vximm1q.io.enq <> io.vec_ximm1q
  vximm2q.io.enq <> io.vec_ximm2q

  val vxu = new vuVXU()

  val irb = new vuIRB()

  val vaq = new queue_spec(16)({ new io_vaq_bundle() })
  val vaq_count = new vuVMU_QueueCount(16, 9, 16, true)

  val vldq = new queue_reorder_qcnt(65, 256, 9)
  val vldq_count = new vuVMU_QueueCount(0, 9, 16, true)

  val vsdq = new queue_spec(16)({ Bits(width = 65) })
  val vsdq_count = new vuVMU_QueueCount(16, 9, 16, true)

  val vsackcnt = new sackcnt()

  val vmu_utcmdq = VC_SIMPLE_QUEUE(UTMCMD_SZ, 16)
  val vmu_utimmq = VC_SIMPLE_QUEUE(UTMIMM_SZ, 16)
  val utaq = VC_SIMPLE_QUEUE(SZ_ADDR, 16)
  val utaq_count = new vuVMU_QueueCount(16, 9, 16, true)

  val utldq = VC_SIMPLE_QUEUE(SZ_DATA, 16)
  val utldq_count = new vuVMU_QueueCount(0, 9, 16, true)

  val utsdq = VC_SIMPLE_QUEUE(SZ_DATA, 16)
  val utsdq_count = new vuVMU_QueueCount(16, 9, 16, true)

  // vxu
  io.illegal <> vxu.io.illegal

  vxu.io.vxu_cmdq <> vcmdq.io.deq
  vxu.io.vxu_immq <> vximm1q.io.deq
  vxu.io.vxu_imm2q <> vximm2q.io.deq

  vxu.io.vec_ackq <> io.vec_ackq

  vxu.io.cp_imul_req <> io.cp_imul_req
  vxu.io.cp_imul_resp <> io.cp_imul_resp

  vxu.io.cp_fma_req <> io.cp_fma_req
  vxu.io.cp_fma_resp <> io.cp_fma_resp

  vxu.io.imem_req <> io.imem_req
  vxu.io.imem_resp <> io.imem_resp

  val memif = new vuMemIF()

  io.dmem_req <> memif.io.mem_req
  memif.io.mem_resp <> io.dmem_resp

  // irb
  irb.io.irb_cmdb <> vxu.io.irb_cmdb
  irb.io.irb_imm1b <> vxu.io.irb_imm1b
  irb.io.irb_imm2b <> vxu.io.irb_imm2b
  irb.io.irb_cntb <> vxu.io.irb_cntb

  irb.io.issue_to_irb <> vxu.io.issue_to_irb
  irb.io.irb_to_issue <> vxu.io.irb_to_issue

  irb.io.seq_to_irb <> vxu.io.seq_to_irb

  // vaq
  vxu.io.lane_vaq.ready := vaq_count.io.watermark // vaq.io.enq.ready
  vaq.io.enq.valid := vxu.io.lane_vaq.valid
  vaq.io.enq.bits := vxu.io.lane_vaq.bits

  memif.io.vaq_deq <> vaq.io.deq

  vaq.io.ack := memif.io.vaq_ack
  vaq.io.nack := memif.io.vaq_nack

  vaq_count.io.qcnt := vxu.io.qcnt
  vaq_count.io.inc := memif.io.vaq_ack
  vaq_count.io.dec := vxu.io.lane_vaq.valid


  // vldq
  vldq.io.enq <> memif.io.vldq_enq

  memif.io.vldq_deq_rtag <> vldq.io.deq_rtag

  vldq.io.deq_data.ready := vxu.io.lane_vldq.ready
  vxu.io.lane_vldq.valid := vldq.io.watermark // vldq.deq_data.valid
  vxu.io.lane_vldq.bits := vldq.io.deq_data.bits

  vldq.io.qcnt := vxu.io.qcnt
  vldq.io.ack := memif.io.vldq_ack
  vldq.io.nack := memif.io.vldq_nack


  // vsdq
  vxu.io.lane_vsdq.ready := vsdq_count.io.watermark && vsackcnt.io.watermark// vsdq.io.enq.ready
  vsdq.io.enq.valid := vxu.io.lane_vsdq.valid
  vsdq.io.enq.bits := Mux(Bool(true), vxu.io.lane_vsdq.bits, irb.io.mem.bits)

  memif.io.vsdq_deq <> vsdq.io.deq

  vsdq.io.ack := memif.io.vsdq_ack
  vsdq.io.nack := memif.io.vsdq_nack

  vsdq_count.io.qcnt := vxu.io.qcnt
  vsdq_count.io.inc := memif.io.vsdq_ack
  vsdq_count.io.dec := vxu.io.lane_vsdq.valid

  // vsack
  vsackcnt.io.vsdq_ack := memif.io.vsdq_ack
  vsackcnt.io.vsdq_enq_valid := vxu.io.lane_vsdq.valid
  vsackcnt.io.vsdq_enq_ready := vsdq.io.enq.ready
  vsackcnt.io.qcnt := vxu.io.qcnt
  vxu.io.store_zero := vsackcnt.io.zero

  //val vmu = new vuVMU()

  //// utaq
  //vxu.io.lane_utaq.ready := utaq_count.io.watermark // utaq.io.enq.ready
  //utaq.io.enq.valid := vxu.io.lane_utaq.valid
  //utaq.io.enq.bits := vxu.io.lane_utaq.bits

  //utaq.io.deq.ready := vmu.io.utaq_deq_ready
  //vmu.io.utaq_deq_valid := utaq.io.deq.valid
  //vmu.io.utaq_deq_bits := utaq.io.deq.bits

  //utaq_count.io.qcnt := vxu.io.qcnt
  //utaq_count.io.inc := vmu.io.utaq_deq_ready && utaq.io.deq.valid
  //utaq_count.io.dec := vxu.io.lane_utaq.valid


  //// utldq
  //vmu.io.utldq_enq_ready := utldq.io.enq.ready
  //utldq.io.enq.valid := vmu.io.utldq_enq_valid
  //utldq.io.enq.bits := vmu.io.utldq_enq_bits

  //utldq.io.deq.ready := vxu.io.lane_utldq.ready
  //vxu.io.lane_utldq.valid := utldq_count.io.watermark // utldq.deq.valid
  //vxu.io.lane_utldq.bits := utldq.io.deq.bits

  //utldq_count.io.qcnt := vxu.io.qcnt
  //utldq_count.io.inc := utldq.io.enq.ready && vmu.io.utldq_enq_valid
  //utldq_count.io.dec := vxu.io.lane_utldq.ready


  //// utsdq
  //vxu.io.lane_utsdq.ready := utsdq_count.io.watermark // utsdq.io.enq.ready
  //utsdq.io.enq.valid := vxu.io.lane_utsdq.valid
  //utsdq.io.enq.bits := vxu.io.lane_utsdq.bits

  //utsdq.io.deq.ready := vmu.io.utsdq_deq_ready
  //vmu.io.utsdq_deq_valid := utsdq.io.deq.valid
  //vmu.io.utsdq_deq_bits := utsdq.io.deq.bits

  //utsdq_count.io.qcnt := vxu.io.qcnt
  //utsdq_count.io.inc := vmu.io.utsdq_deq_ready && utsdq.io.deq.valid
  //utsdq_count.io.dec := vxu.io.lane_utsdq.valid


  //// vmu
  //vmu.io.vmu_vcmdq.bits <> vmu_vcmdq.io.deq.bits
  //vmu.io.vmu_vcmdq.valid <> vmu_vcmdq.io.deq.valid
  //vmu.io.vmu_vcmdq.rdy <> vmu_vcmdq.io.deq.ready

  //vmu.io.vmu_vbaseq.bits <> vmu_vbaseq.io.deq.bits
  //vmu.io.vmu_vbaseq.valid <> vmu_vbaseq.io.deq.valid
  //vmu.io.vmu_vbaseq.rdy <> vmu_vbaseq.io.deq.ready
  //
  //vmu.io.vmu_vstrideq.bits <> vmu_vstrideq.io.deq.bits
  //vmu.io.vmu_vstrideq.valid <> vmu_vstrideq.io.deq.valid
  //vmu.io.vmu_vstrideq.rdy <> vmu_vstrideq.io.deq.ready

  //vmu.io.vmu_utcmdq.bits <> vmu_utcmdq.io.deq.bits
  //vmu.io.vmu_utcmdq.valid <> vmu_utcmdq.io.deq.valid
  //vmu.io.vmu_utcmdq.rdy <> vmu_utcmdq.io.deq.ready

  //vmu.io.vmu_utimmq.bits <> vmu_utimmq.io.deq.bits
  //vmu.io.vmu_utimmq.valid <> vmu_utimmq.io.deq.valid
  //vmu.io.vmu_utimmq.rdy <> vmu_utimmq.io.deq.ready

  //vmu.io.dmem_req_vec.addr <> io.dmem_req_vec.bits.addr
  //vmu.io.dmem_req_vec.op <> io.dmem_req_vec.bits.op
  //vmu.io.dmem_req_vec.data <> io.dmem_req_vec.bits.data
  //vmu.io.dmem_req_vec.wmask <> io.dmem_req_vec.bits.wmask
  //vmu.io.dmem_req_vec.tag <> io.dmem_req_vec.bits.tag
  //vmu.io.dmem_req_vec.valid <> io.dmem_req_vec.valid
  //vmu.io.dmem_req_vec.rdy <> io.dmem_req_vec.ready
  //vmu.io.dmem_resp_vec.valid <> io.dmem_resp_vec.valid

  //vmu.io.dmem_req_ut.addr <> io.dmem_req_ut.bits.addr
  //vmu.io.dmem_req_ut.op <> io.dmem_req_ut.bits.op
  //vmu.io.dmem_req_ut.data <> io.dmem_req_ut.bits.data
  //vmu.io.dmem_req_ut.wmask <> io.dmem_req_ut.bits.wmask
  //vmu.io.dmem_req_ut.tag <> io.dmem_req_ut.bits.tag
  //vmu.io.dmem_req_ut.valid <> io.dmem_req_ut.valid
  //vmu.io.dmem_req_ut.rdy <> io.dmem_req_ut.ready
  //vmu.io.dmem_resp_ut.valid <> io.dmem_resp_ut.valid

  //vmu.io.dmem_resp_ut.tag <> io.dmem_resp_ut.bits.tag
  //vmu.io.dmem_resp_ut.data <> io.dmem_resp_ut.bits.data
  //vmu.io.dmem_resp_vec.tag <> io.dmem_resp_vec.bits.tag
  //vmu.io.dmem_resp_vec.data <> io.dmem_resp_vec.bits.data
}

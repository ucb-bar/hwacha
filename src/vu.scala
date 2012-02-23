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

  // needs to make sure log2up(vldq_entries)+1 <= CPU_TAG_BITS-1
  val vldq = new queue_reorder_qcnt(65, 128, 9)

  val vsdq = new queue_spec(16)({ Bits(width = 65) })
  val vsdq_count = new vuVMU_QueueCount(16, 9, 16, true)

  val vsackcnt = new sackcnt()

  val vmu_utcmdq = VC_SIMPLE_QUEUE(UTMCMD_SZ, 16)
  val vmu_utimmq = VC_SIMPLE_QUEUE(UTMIMM_SZ, 16)

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
  vaq_count.io.dec := vxu.io.lane_vaq_dec


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
  vsdq_count.io.dec := vxu.io.lane_vsdq_dec

  // vsack
  vsackcnt.io.inc := memif.io.vsdq_ack
  vsackcnt.io.dec := vxu.io.lane_vsdq.valid && vsdq.io.enq.ready
  vsackcnt.io.qcnt := vxu.io.qcnt
  vxu.io.pending_store := !vsackcnt.io.zero
}

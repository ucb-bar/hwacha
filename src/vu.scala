package hwacha

import Chisel._
import Node._
import Constants._
import queues._

class vu extends Component
{
  val io = new io_vu()

  val vcmdq = new queueSimplePF(16)({Bits(width=SZ_VCMD)})
  val vximm1q = new queueSimplePF(16)({Bits(width=SZ_VIMM)})
  val vximm2q = new queueSimplePF(16)({Bits(width=SZ_VSTRIDE)})
  val vxcntq = new queueSimplePF(16)( Bits(width=SZ_VLEN) )

  vcmdq.io.enq <> io.vec_cmdq
  vximm1q.io.enq <> io.vec_ximm1q
  vximm2q.io.enq <> io.vec_ximm2q
  vxcntq.io.enq <> io.vec_cntq
  
  val vxu = new vuVXU()
  val irb = new vuIRB()
  val evac = new vuEvac()

  val vaq = new queue_spec(16)({ new io_vaq_bundle() })
  val vaq_count = new queuecnt(16,9,16,true)

  // needs to make sure log2up(vldq_entries)+1 <= CPU_TAG_BITS-1
  val vldq = new queue_reorder_qcnt(65,128,9)

  val vsdq = new queue_spec(16)({ Bits(width = 65) })
  val vsdq_count = new queuecnt(16,9,16,true)

  val vsack_count = new sackcnt()

  // vxu
  io.illegal <> vxu.io.illegal

  vxu.io.vxu_cmdq.bits := vcmdq.io.deq.bits
  vxu.io.vxu_cmdq.valid := vcmdq.io.deq.valid
  vcmdq.io.deq.ready := vxu.io.vxu_cmdq.ready || evac.io.vcmdq.ready

  vxu.io.vxu_immq.bits := vximm1q.io.deq.bits
  vxu.io.vxu_immq.valid := vximm1q.io.deq.valid
  vximm1q.io.deq.ready := vxu.io.vxu_immq.ready || evac.io.vimm1q.ready

  vxu.io.vxu_imm2q.bits := vximm2q.io.deq.bits
  vxu.io.vxu_imm2q.valid := vximm2q.io.deq.valid
  vximm2q.io.deq.ready := vxu.io.vxu_imm2q.ready || evac.io.vimm2q.ready

  vxu.io.vxu_cntq.bits := vxcntq.io.deq.bits
  vxu.io.vxu_cntq.valid := vxcntq.io.deq.valid
  vxcntq.io.deq.ready := vxu.io.vxu_cntq.ready || evac.io.vcntq.ready

  vxu.io.vec_ackq <> io.vec_ackq

  vxu.io.cp_imul_req <> io.cp_imul_req
  vxu.io.cp_imul_resp <> io.cp_imul_resp

  vxu.io.imem_req <> io.imem_req
  vxu.io.imem_resp <> io.imem_resp

  vxu.io.cpu_exception <> io.cpu_exception

  val memif = new vuMemIF()

  io.dmem_req <> memif.io.mem_req
  memif.io.mem_resp <> io.dmem_resp

  // irb
  irb.io.irb_enq_cmdb <> vxu.io.irb_cmdb
  irb.io.irb_enq_imm1b <> vxu.io.irb_imm1b
  irb.io.irb_enq_imm2b <> vxu.io.irb_imm2b
  irb.io.irb_enq_cntb <> vxu.io.irb_cntb

  irb.io.issue_to_irb <> vxu.io.issue_to_irb
  irb.io.irb_to_issue <> vxu.io.irb_to_issue

  irb.io.seq_to_irb <> vxu.io.seq_to_irb

  // evac
  evac.io.cpu_exception <> io.cpu_exception

  evac.io.irb_cmdb <> irb.io.irb_deq_cmdb
  evac.io.irb_imm1b <> irb.io.irb_deq_imm1b
  evac.io.irb_imm2b <> irb.io.irb_deq_imm2b
  evac.io.irb_cntb <> irb.io.irb_deq_cntb
  evac.io.irb_cntb_last <> irb.io.irb_deq_cntb_last

  evac.io.vcmdq.bits := vcmdq.io.deq.bits
  evac.io.vcmdq.valid := vcmdq.io.deq.valid

  evac.io.vimm1q.bits := vximm1q.io.deq.bits
  evac.io.vimm1q.valid := vximm1q.io.deq.valid
  
  evac.io.vimm2q.bits := vximm2q.io.deq.bits
  evac.io.vimm2q.valid := vximm2q.io.deq.valid

  evac.io.vcntq.bits := vxcntq.io.deq.bits
  evac.io.vcntq.valid := vxcntq.io.deq.valid
  
  // vldq
  vldq.io.enq <> memif.io.vldq_enq

  memif.io.vldq_deq_rtag <> vldq.io.deq_rtag

  vldq.io.deq_data.ready := vxu.io.lane_vldq.ready
  vxu.io.lane_vldq.valid := vldq.io.watermark // vldq.deq_data.valid
  vxu.io.lane_vldq.bits := vldq.io.deq_data.bits

  vldq.io.qcnt := vxu.io.qcnt
  vldq.io.ack := memif.io.vldq_ack
  vldq.io.nack := memif.io.vldq_nack

  // vsdq arbiter
  val vsdq_arb = new hArbiter(2)( new io_vsdq() )
  vsdq_arb.io.in(0).valid := vxu.io.lane_vsdq.valid
  vsdq_arb.io.in(0).bits := vxu.io.lane_vsdq.bits
  vxu.io.lane_vsdq.ready := vsdq_arb.io.in(0).ready
  
  vsdq_arb.io.in(1).valid := evac.io.vsdq.valid
  vsdq_arb.io.in(1).bits := evac.io.vsdq.bits
  evac.io.vsdq.ready := vsdq_arb.io.in(1).ready

  // vsdq
  vsdq_arb.io.out.ready := vsdq_count.io.watermark && vsack_count.io.watermark// vsdq.io.enq.ready
  vsdq.io.enq.valid := vsdq_arb.io.out.valid
  vsdq.io.enq.bits := vsdq_arb.io.out.bits

  memif.io.vsdq_deq <> vsdq.io.deq

  vsdq.io.ack := memif.io.vsdq_ack
  vsdq.io.nack := memif.io.vsdq_nack

  vsdq_count.io.qcnt := vxu.io.qcnt
  vsdq_count.io.inc := memif.io.vsdq_ack
  vsdq_count.io.dec := vxu.io.lane_vsdq_dec

  // vsack
  vsack_count.io.inc := memif.io.vsdq_ack
  vsack_count.io.dec := vxu.io.lane_vsdq.valid && vsdq.io.enq.ready
  vsack_count.io.qcnt := vxu.io.qcnt
  vxu.io.pending_store := !vsack_count.io.zero

  // prefetch

  val vru = new vuVRU()
  val vpfaq = new queue_spec(16)({ new io_vaq_bundle() })
  val vaq_arb = new hArbiter(2)({ new io_lane_vaq() })

  val reg_chosen = Reg(vaq_arb.io.chosen)
  val reg_chosen2 = Reg(reg_chosen)

  val vpfcmdq = new queueSimplePF(16)({Bits(width=SZ_VCMD)})
  val vpfximm1q = new queueSimplePF(16)({Bits(width=SZ_VIMM)})
  val vpfximm2q = new queueSimplePF(16)({Bits(width=SZ_VSTRIDE)})

  vpfcmdq.io.enq <> io.vec_pfcmdq
  vpfximm1q.io.enq <> io.vec_pfximm1q
  vpfximm2q.io.enq <> io.vec_pfximm2q

  vru.io.vec_pfcmdq <> vpfcmdq.io.deq
  vru.io.vec_pfximm1q <> vpfximm1q.io.deq
  vru.io.vec_pfximm2q <> vpfximm2q.io.deq
  
  vpfaq.io.enq <> vru.io.vpfaq

  // vaq arbiter
  val vaq_evac_lane_arb = new hArbiter(2)( new io_lane_vaq() )
  
  vaq_evac_lane_arb.io.in(0).valid := vxu.io.lane_vaq.valid
  vaq_evac_lane_arb.io.in(0).bits := vxu.io.lane_vaq.bits
  vxu.io.lane_vaq.ready := vaq_evac_lane_arb.io.in(0).ready

  vaq_evac_lane_arb.io.in(1).valid := evac.io.vaq.valid
  vaq_evac_lane_arb.io.in(1).bits := evac.io.vaq.bits
  evac.io.vaq.ready := vaq_evac_lane_arb.io.in(1).ready

  // vaq
  vaq_evac_lane_arb.io.out.ready := vaq_count.io.watermark // vaq.io.enq.ready
  vaq.io.enq.valid := vaq_evac_lane_arb.io.out.valid
  vaq.io.enq.bits := vaq_evac_lane_arb.io.out.bits

  vaq_arb.io.in(0) <> vpfaq.io.deq
  vaq_arb.io.in(1) <> vaq.io.deq

  memif.io.vaq_deq <> vaq_arb.io.out

  val vaqack = Mux(reg_chosen2 === Bits(1), memif.io.vaq_ack, Bool(false))
  val vaqnack = memif.io.vaq_nack
  
  val vpfaqack = Mux(reg_chosen2 === Bits(0), memif.io.vaq_ack, Bool(false))
  val vpfaqnack = memif.io.vaq_nack
  
  vaq.io.ack := vaqack
  vaq.io.nack := vaqnack

  vpfaq.io.ack := vpfaqack
  vpfaq.io.nack := vpfaqnack
  
  vaq_count.io.qcnt := vxu.io.qcnt
  vaq_count.io.inc := vaqack
  vaq_count.io.dec := vxu.io.lane_vaq_dec

  // tlb
  // temporary
  io.vec_tlb_req.valid := Bool(false)
  io.vec_pftlb_req.valid := Bool(false)
}

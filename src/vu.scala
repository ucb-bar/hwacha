package hwacha

import Chisel._
import Node._
import Constants._

class io_xcpt extends Bundle 
{
  val exception = Bool(OUTPUT)
  val evac_addr = UFix(OUTPUT, SZ_ADDR)
  val evac = Bool(OUTPUT)
  val hold = Bool(OUTPUT)
  val kill = Bool(OUTPUT)
}

class io_vu extends Bundle 
{
  val irq = Bool(OUTPUT)
  val irq_cause = UFix(OUTPUT, 5)
  val irq_aux = Bits(OUTPUT, 64)

  val vec_cmdq = new io_vec_cmdq().flip
  val vec_ximm1q = new io_vec_ximm1q().flip
  val vec_ximm2q = new io_vec_ximm2q().flip
  val vec_cntq = new io_vec_cntq().flip

  val vec_cmdq_user_ready = Bool(OUTPUT)
  val vec_ximm1q_user_ready = Bool(OUTPUT)
  val vec_ximm2q_user_ready = Bool(OUTPUT)
  val vec_fence_ready = Bool(OUTPUT)

  val vec_pfcmdq = new io_vec_cmdq().flip
  val vec_pfximm1q = new io_vec_ximm1q().flip
  val vec_pfximm2q = new io_vec_ximm2q().flip
  val vec_pfcntq = new io_vec_cntq().flip

  val cp_imul_req = new io_imul_req().flip
  val cp_imul_resp = Bits(OUTPUT, SZ_XLEN)
  val cp_dfma = new io_cp_dfma()
  val cp_sfma = new io_cp_sfma()

  val imem_req = new io_imem_req()
  val imem_resp = new io_imem_resp().flip

  val dmem_req = new io_dmem_req()
  val dmem_resp = new io_dmem_resp().flip

  val vec_tlb = new io_tlb
  val vec_pftlb = new io_tlb

  val xcpt = new io_xcpt().flip()
}

class vu extends Component
{
  val io = new io_vu()

  val xcpt = new vuXCPTHandler()

  val flush_kill = reset || xcpt.io.xcpt_to_vu.flush_kill
  val flush_irq = reset || xcpt.io.xcpt_to_vu.flush_irq
  val flush_aiw = reset || xcpt.io.xcpt_to_vu.flush_aiw
  val flush_vru = reset || xcpt.io.xcpt_to_vu.flush_vru
  val flush_vmu = reset || xcpt.io.xcpt_to_vu.flush_vmu

  val vcmdq = new Queue(19, resetSignal = flush_kill)(Bits(width = SZ_VCMD))
  val vximm1q = new Queue(19, resetSignal = flush_kill)(Bits(width = SZ_VIMM))
  val vximm2q = new Queue(17, resetSignal = flush_kill)(Bits(width = SZ_VSTRIDE))
  val vxcntq = new Queue(8, resetSignal = flush_kill)(Bits(width = SZ_VLEN+1))

  vcmdq.io.enq <> MaskStall(io.vec_cmdq, xcpt.io.xcpt_to_vu.busy)
  vximm1q.io.enq <> MaskStall(io.vec_ximm1q, xcpt.io.xcpt_to_vu.busy)
  vximm2q.io.enq <> MaskStall(io.vec_ximm2q, xcpt.io.xcpt_to_vu.busy)
  vxcntq.io.enq <> MaskStall(io.vec_cntq, xcpt.io.xcpt_to_vu.busy)

  val vxu = new vuVXU()
  val vmu = new vuVMU(resetSignal = flush_vmu)
  val irq = new vuIRQHandler(resetSignal = flush_irq)
  val aiw = new vuAIW(resetSignal = flush_aiw)
  val evac = new vuEvac()

  // counters
  val vcmdq_count = new qcnt(19, 19, resetSignal = flush_kill)
  val vximm1q_count = new qcnt(19, 19, resetSignal = flush_kill)
  val vximm2q_count = new qcnt(17, 17, resetSignal = flush_kill)

  if (HAVE_VRU)
  {
    val vru = new vuVRU(resetSignal = flush_vru)

    val vpfcmdq = new Queue(19, resetSignal = flush_kill)(Bits(width=SZ_VCMD))
    val vpfximm1q = new Queue(19, resetSignal = flush_kill)(Bits(width=SZ_VIMM))
    val vpfximm2q = new Queue(17, resetSignal = flush_kill)(Bits(width=SZ_VSTRIDE))
    val vpfcntq = new Queue(8, resetSignal = flush_kill)(Bits(width=SZ_VLEN))

    vpfcmdq.io.enq <> MaskStall(io.vec_pfcmdq, xcpt.io.xcpt_to_vu.busy)
    vpfximm1q.io.enq <> MaskStall(io.vec_pfximm1q, xcpt.io.xcpt_to_vu.busy)
    vpfximm2q.io.enq <> MaskStall(io.vec_pfximm2q, xcpt.io.xcpt_to_vu.busy)
    vpfcntq.io.enq <> MaskStall(io.vec_pfcntq, xcpt.io.xcpt_to_vu.busy)

    vru.io.vec_pfcmdq <> vpfcmdq.io.deq
    vru.io.vec_pfximm1q <> vpfximm1q.io.deq
    vru.io.vec_pfximm2q <> vpfximm2q.io.deq
    vru.io.vec_pfcntq <> vpfcntq.io.deq

    vmu.io.pf_vvaq <> vru.io.vpfvaq
    vmu.io.vec_pftlb <> io.vec_pftlb
  }
  else
  {
    io.vec_pfcmdq.ready := Bool(true)
    io.vec_pfximm1q.ready := Bool(true)
    io.vec_pfximm2q.ready := Bool(true)
    io.vec_pfcntq.ready := Bool(true)
  }

  vcmdq_count.io.dec := vcmdq.io.enq.ready && io.vec_cmdq.valid
  vcmdq_count.io.inc := (vxu.io.vxu_cmdq.ready || evac.io.vcmdq.ready) && vcmdq.io.deq.valid
  vximm1q_count.io.dec := vximm1q.io.enq.ready && io.vec_ximm1q.valid
  vximm1q_count.io.inc := (vxu.io.vxu_immq.ready || evac.io.vimm1q.ready) && vximm1q.io.deq.valid
  vximm2q_count.io.dec := vximm2q.io.enq.ready && io.vec_ximm2q.valid
  vximm2q_count.io.inc := (vxu.io.vxu_imm2q.ready || evac.io.vimm2q.ready) && vximm2q.io.deq.valid

  vcmdq_count.io.qcnt := UFix(11)
  vximm1q_count.io.qcnt := UFix(11)
  vximm2q_count.io.qcnt := UFix(9)
  io.vec_cmdq_user_ready := vcmdq_count.io.watermark && !xcpt.io.xcpt_to_vu.busy
  io.vec_ximm1q_user_ready := vximm1q_count.io.watermark && !xcpt.io.xcpt_to_vu.busy
  io.vec_ximm2q_user_ready := vximm2q_count.io.watermark && !xcpt.io.xcpt_to_vu.busy

  // fence
  io.vec_fence_ready := !vcmdq.io.deq.valid && !vxu.io.pending_vf && !vxu.io.pending_memop && !vmu.io.pending_store

  io.irq := irq.io.irq
  io.irq_cause := irq.io.irq_cause
  io.irq_aux := irq.io.irq_aux

  // xcpt
  xcpt.io.xcpt <> io.xcpt

  // vxu
  vxu.io.vxu_cmdq.bits := vcmdq.io.deq.bits
  vxu.io.vxu_cmdq.valid := vcmdq.io.deq.valid
  vcmdq.io.deq.ready := vxu.io.vxu_cmdq.ready || evac.io.vcmdq.ready

  vxu.io.vxu_immq.bits := vximm1q.io.deq.bits
  vxu.io.vxu_immq.valid := vximm1q.io.deq.valid
  vximm1q.io.deq.ready := vxu.io.vxu_immq.ready || evac.io.vimm1q.ready

  vxu.io.vxu_imm2q.bits := vximm2q.io.deq.bits
  vxu.io.vxu_imm2q.valid := vximm2q.io.deq.valid
  vximm2q.io.deq.ready := vxu.io.vxu_imm2q.ready || evac.io.vimm2q.ready

  vxu.io.vxu_cntq.bits := vxcntq.io.deq.bits(10,0)
  vxu.io.vxu_cntq.valid := vxcntq.io.deq.valid
  vxcntq.io.deq.ready := vxu.io.vxu_cntq.ready || evac.io.vcntq.ready

  vxu.io.cp_imul_req <> io.cp_imul_req
  vxu.io.cp_imul_resp <> io.cp_imul_resp
  io.cp_dfma <> vxu.io.cp_dfma
  io.cp_sfma <> vxu.io.cp_sfma

  vxu.io.imem_req <> io.imem_req
  vxu.io.imem_resp <> io.imem_resp

  vxu.io.xcpt_to_vxu <> xcpt.io.xcpt_to_vxu
  vxu.io.vxu_to_xcpt <> xcpt.io.vxu_to_xcpt

  irq.io.issue_to_irq <> vxu.io.irq

  // vmu
  vmu.io.lane_vvaq <> vxu.io.lane_vaq
  vmu.io.evac_vvaq <> evac.io.vaq

  vmu.io.lane_vsdq <> vxu.io.lane_vsdq
  vmu.io.evac_vsdq <> evac.io.vsdq

  vxu.io.lane_vldq <> vmu.io.lane_vldq

  vmu.io.lane_vaq_dec := vxu.io.lane_vaq_dec
  vmu.io.lane_vsdq_dec := vxu.io.lane_vsdq_dec

  vmu.io.qcntp1 := vxu.io.qcntp1
  vmu.io.qcntp2 := vxu.io.qcntp2

  vxu.io.pending_store := vmu.io.pending_store

  vmu.io.dmem_req <> io.dmem_req
  vmu.io.dmem_resp <> io.dmem_resp

  vmu.io.vec_tlb <> io.vec_tlb

  vmu.io.xcpt_to_vmu <> xcpt.io.xcpt_to_vmu
  vmu.io.evac_to_vmu <> evac.io.evac_to_vmu
  vmu.io.vmu_to_xcpt <> xcpt.io.vmu_to_xcpt

  vmu.io.irq <> irq.io.vmu_to_irq

  // aiw
  aiw.io.aiw_enq_cmdb <> vxu.io.aiw_cmdb
  aiw.io.aiw_enq_imm1b <> vxu.io.aiw_imm1b
  aiw.io.aiw_enq_imm2b <> vxu.io.aiw_imm2b
  aiw.io.aiw_enq_cntb <> vxu.io.aiw_cntb
  aiw.io.aiw_enq_numCntB <> vxu.io.aiw_numCntB

  aiw.io.issue_to_aiw <> vxu.io.issue_to_aiw
  aiw.io.aiw_to_issue <> vxu.io.aiw_to_issue
  aiw.io.seq_to_aiw <> vxu.io.seq_to_aiw

  // evac
  evac.io.aiw_cmdb <> aiw.io.aiw_deq_cmdb
  evac.io.aiw_imm1b <> aiw.io.aiw_deq_imm1b
  evac.io.aiw_imm2b <> aiw.io.aiw_deq_imm2b
  evac.io.aiw_cntb <> aiw.io.aiw_deq_cntb
  evac.io.aiw_numCntB <> aiw.io.aiw_deq_numCntB
  evac.io.aiw_numCntB_last <> aiw.io.aiw_deq_numCntB_last

  evac.io.evac_to_aiw <> aiw.io.evac_to_aiw

  evac.io.vcmdq.bits := vcmdq.io.deq.bits
  evac.io.vcmdq.valid := vcmdq.io.deq.valid

  evac.io.vimm1q.bits := vximm1q.io.deq.bits
  evac.io.vimm1q.valid := vximm1q.io.deq.valid

  evac.io.vimm2q.bits := vximm2q.io.deq.bits
  evac.io.vimm2q.valid := vximm2q.io.deq.valid

  evac.io.vcntq.bits := vxcntq.io.deq.bits
  evac.io.vcntq.valid := vxcntq.io.deq.valid

  evac.io.xcpt_to_evac <> xcpt.io.xcpt_to_evac
  evac.io.evac_to_xcpt <> xcpt.io.evac_to_xcpt
}

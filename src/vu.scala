package hwacha

import Chisel._
import Node._
import Constants._

class io_xcpt extends Bundle 
{
  val exception = Bool(OUTPUT)
  val backup_addr = UFix(SZ_ADDR, OUTPUT)
  val backup = Bool(OUTPUT)
  val exception_ack_valid = Bool(INPUT)
  val exception_ack_ready = Bool(OUTPUT)
  val hold = Bool(OUTPUT)
  val kill = Bool(OUTPUT)
}

class io_vu extends Bundle 
{
  val illegal = Bool(OUTPUT)

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
  val cp_imul_resp = Bits(SZ_XLEN, OUTPUT)
  val cp_dfma = new io_cp_dfma()
  val cp_sfma = new io_cp_sfma()
  
  val imem_req = new io_imem_req()
  val imem_resp = new io_imem_resp().flip

  val dmem_req = new io_dmem_req()
  val dmem_resp = new io_dmem_resp().flip

  val xcpt = new io_xcpt().flip()

  val vec_tlb_req = new ioDTLB_CPU_req()
  val vec_tlb_resp = new ioDTLB_CPU_resp().flip

  val vec_pftlb_req = new ioDTLB_CPU_req()
  val vec_pftlb_resp = new ioDTLB_CPU_resp().flip
}

class vu extends Component
{
  val io = new io_vu()

  val vcmdq = new queueSimplePF(32, flushable = true)({Bits(width=SZ_VCMD)})
  val vximm1q = new queueSimplePF(32, flushable = true)({Bits(width=SZ_VIMM)})
  val vximm2q = new queueSimplePF(32, flushable = true)({Bits(width=SZ_VSTRIDE)})
  val vxcntq = new queueSimplePF(8, flushable = true)( Bits(width=SZ_VLEN) )

  vcmdq.io.enq <> io.vec_cmdq
  vximm1q.io.enq <> io.vec_ximm1q
  vximm2q.io.enq <> io.vec_ximm2q
  vxcntq.io.enq <> io.vec_cntq
  
  val vpfcmdq = new queueSimplePF(32, flushable = true)({Bits(width=SZ_VCMD)})
  val vpfximm1q = new queueSimplePF(32, flushable = true)({Bits(width=SZ_VIMM)})
  val vpfximm2q = new queueSimplePF(32, flushable = true)({Bits(width=SZ_VSTRIDE)})
  val vpfcntq = new queueSimplePF(8, flushable = true)({Bits(width=SZ_VLEN)})

  vpfcmdq.io.enq <> io.vec_pfcmdq
  vpfximm1q.io.enq <> io.vec_pfximm1q
  vpfximm2q.io.enq <> io.vec_pfximm2q
  vpfcntq.io.enq <> io.vec_pfcntq

  val vru = new vuVRU()
  val vxu = new vuVXU()
  val vmu = new vuVMU()
  val evac = new vuEvac()
  val xcpt = new vuXCPTHandler()

  // counters
  val vcmdq_count = new qcnt(32, 32, flushable = true)
  val vximm1q_count = new qcnt(32, 32, flushable = true)
  val vximm2q_count = new qcnt(32, 32, flushable = true)

  vcmdq_count.io.dec := vcmdq.io.enq.ready && io.vec_cmdq.valid
  vcmdq_count.io.inc := (vxu.io.vxu_cmdq.ready || evac.io.vcmdq.ready) && vcmdq.io.deq.valid
  vximm1q_count.io.dec := vximm1q.io.enq.ready && io.vec_ximm1q.valid
  vximm1q_count.io.inc := (vxu.io.vxu_immq.ready || evac.io.vimm1q.ready) && vximm1q.io.deq.valid
  vximm2q_count.io.dec := vximm2q.io.enq.ready && io.vec_ximm2q.valid
  vximm2q_count.io.inc := (vxu.io.vxu_imm2q.ready || evac.io.vimm2q.ready) && vximm2q.io.deq.valid

  vcmdq_count.io.qcnt := UFix(9)
  vximm1q_count.io.qcnt := UFix(9)
  vximm2q_count.io.qcnt := UFix(9)
  io.vec_cmdq_user_ready := vcmdq_count.io.watermark
  io.vec_ximm1q_user_ready := vximm1q_count.io.watermark
  io.vec_ximm2q_user_ready := vximm2q_count.io.watermark

  // fence
  io.vec_fence_ready := !vcmdq.io.deq.valid && !vxu.io.pending_memop && !vmu.io.pending_store

  // xcpt
  xcpt.io.xcpt <> io.xcpt

  vcmdq.io.flush := xcpt.io.xcpt_to_vu.flush
  vximm1q.io.flush := xcpt.io.xcpt_to_vu.flush
  vximm2q.io.flush := xcpt.io.xcpt_to_vu.flush
  vxcntq.io.flush := xcpt.io.xcpt_to_vu.flush

  vcmdq_count.io.flush := xcpt.io.xcpt_to_vu.flush
  vximm1q_count.io.flush := xcpt.io.xcpt_to_vu.flush
  vximm2q_count.io.flush := xcpt.io.xcpt_to_vu.flush

  vpfcmdq.io.flush := xcpt.io.xcpt_to_vu.flush
  vpfximm1q.io.flush := xcpt.io.xcpt_to_vu.flush
  vpfximm2q.io.flush := xcpt.io.xcpt_to_vu.flush
  vpfcntq.io.flush := xcpt.io.xcpt_to_vu.flush

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

  vxu.io.cp_imul_req <> io.cp_imul_req
  vxu.io.cp_imul_resp <> io.cp_imul_resp
  io.cp_dfma <> vxu.io.cp_dfma
  io.cp_sfma <> vxu.io.cp_sfma

  vxu.io.imem_req <> io.imem_req
  vxu.io.imem_resp <> io.imem_resp

  vxu.io.xcpt_to_vxu <> xcpt.io.xcpt_to_vxu
  vxu.io.vxu_to_xcpt <> xcpt.io.vxu_to_xcpt

  // vru
  vru.io.vec_pfcmdq <> vpfcmdq.io.deq
  vru.io.vec_pfximm1q <> vpfximm1q.io.deq
  vru.io.vec_pfximm2q <> vpfximm2q.io.deq
  vru.io.vec_pfcntq <> vpfcntq.io.deq

  vru.io.xcpt_to_vru <> xcpt.io.xcpt_to_vru

  // vmu
  vmu.io.pf_vvaq <> vru.io.vpfvaq

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

  vmu.io.vec_tlb_req <> io.vec_tlb_req
  vmu.io.vec_tlb_resp <> io.vec_tlb_resp

  vmu.io.vec_pftlb_req <> io.vec_pftlb_req
  vmu.io.vec_pftlb_resp <> io.vec_pftlb_resp

  vmu.io.xcpt_to_vmu <> xcpt.io.xcpt_to_vmu
  vmu.io.evac_to_vmu <> evac.io.evac_to_vmu
  vmu.io.vmu_to_xcpt <> xcpt.io.vmu_to_xcpt

  val irb = new vuIRB()

  // irb
  irb.io.irb_enq_cmdb <> vxu.io.irb_cmdb
  irb.io.irb_enq_imm1b <> vxu.io.irb_imm1b
  irb.io.irb_enq_imm2b <> vxu.io.irb_imm2b
  irb.io.irb_enq_cntb <> vxu.io.irb_cntb
  irb.io.irb_enq_numCntB <> vxu.io.irb_numCntB

  irb.io.issue_to_irb <> vxu.io.issue_to_irb
  irb.io.irb_to_issue <> vxu.io.irb_to_issue
  irb.io.seq_to_irb <> vxu.io.seq_to_irb

  irb.io.xcpt_to_aiw <> xcpt.io.xcpt_to_aiw

  // evac
  evac.io.irb_cmdb <> irb.io.irb_deq_cmdb
  evac.io.irb_imm1b <> irb.io.irb_deq_imm1b
  evac.io.irb_imm2b <> irb.io.irb_deq_imm2b
  evac.io.irb_cntb <> irb.io.irb_deq_cntb
  evac.io.irb_numCntB <> irb.io.irb_deq_numCntB
  evac.io.irb_numCntB_last <> irb.io.irb_deq_numCntB_last

  evac.io.evac_to_irb <> irb.io.evac_to_irb

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

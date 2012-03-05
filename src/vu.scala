package hwacha

import Chisel._
import Node._
import Constants._

class io_vu extends Bundle 
{
  val illegal = Bool(OUTPUT)

  val vec_cmdq = new io_vec_cmdq().flip
  val vec_ximm1q = new io_vec_ximm1q().flip
  val vec_ximm2q = new io_vec_ximm2q().flip
  val vec_cntq = new io_vec_cntq().flip
  val vec_ackq = new io_vec_ackq()

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

  val cpu_exception = new io_cpu_exception().flip

  val exception_ack_valid = Bool(OUTPUT)
  val exception_ack_ready = Bool(INPUT)

  val kill_ack_valid = Bool(OUTPUT)
  val kill_ack_ready = Bool(INPUT)

  val vec_tlb_req = new ioDTLB_CPU_req()
  val vec_tlb_resp = new ioDTLB_CPU_resp().flip

  val vec_pftlb_req = new ioDTLB_CPU_req()
  val vec_pftlb_resp = new ioDTLB_CPU_resp().flip
}

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
  
  val vpfcmdq = new queueSimplePF(16)({Bits(width=SZ_VCMD)})
  val vpfximm1q = new queueSimplePF(16)({Bits(width=SZ_VIMM)})
  val vpfximm2q = new queueSimplePF(16)({Bits(width=SZ_VSTRIDE)})
  val vpfcntq = new queueSimplePF(16)({Bits(width=SZ_VLEN)})

  vpfcmdq.io.enq <> io.vec_pfcmdq
  vpfximm1q.io.enq <> io.vec_pfximm1q
  vpfximm2q.io.enq <> io.vec_pfximm2q
  vpfcntq.io.enq <> io.vec_pfcntq

  val vru = new vuVRU()
  val vxu = new vuVXU()
  val vmu = new vuVMU()
  val evac = new vuEvac()

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
  io.cp_dfma <> vxu.io.cp_dfma
  io.cp_sfma <> vxu.io.cp_sfma

  vxu.io.imem_req <> io.imem_req
  vxu.io.imem_resp <> io.imem_resp

  vxu.io.cpu_exception <> io.cpu_exception

  // vru
  vru.io.vec_pfcmdq <> vpfcmdq.io.deq
  vru.io.vec_pfximm1q <> vpfximm1q.io.deq
  vru.io.vec_pfximm2q <> vpfximm2q.io.deq
  vru.io.vec_pfcntq <> vpfcntq.io.deq

  // vmu
  vmu.io.pf_vvaq <> vru.io.vpfvaq

  vmu.io.lane_vvaq <> vxu.io.lane_vaq
  vmu.io.evac_vvaq <> evac.io.vaq

  vmu.io.lane_vsdq <> vxu.io.lane_vsdq
  vmu.io.evac_vsdq <> evac.io.vsdq

  vxu.io.lane_vldq <> vmu.io.lane_vldq

  vmu.io.lane_vaq_dec := vxu.io.lane_vaq_dec
  vmu.io.lane_vsdq_dec := vxu.io.lane_vsdq_dec

  vmu.io.qcnt := vxu.io.qcnt

  vxu.io.pending_store := vmu.io.pending_store

  vmu.io.dmem_req <> io.dmem_req
  vmu.io.dmem_resp <> io.dmem_resp

  vmu.io.vec_tlb_req <> io.vec_tlb_req
  vmu.io.vec_tlb_resp <> io.vec_tlb_resp

  vmu.io.vec_pftlb_req <> io.vec_pftlb_req
  vmu.io.vec_pftlb_resp <> io.vec_pftlb_resp

  val irb = new vuIRB()

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

  evac.io.evac_to_seq <> vxu.io.evac_to_seq

  io.exception_ack_valid := evac.io.done
  io.kill_ack_valid := Bool(false)

}

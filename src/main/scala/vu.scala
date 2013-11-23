package hwacha

import Chisel._
import Node._
import Constants._

class io_xcpt extends Bundle 
{
  val exception = Bool(OUTPUT)
  val evac_addr = UInt(OUTPUT, SZ_ADDR)
  val evac = Bool(OUTPUT)
  val hold = Bool(OUTPUT)
  val kill = Bool(OUTPUT)
}

class vu(resetSignal: Bool = null)(implicit conf: HwachaConfiguration) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val irq = Bool(OUTPUT)
    val irq_cause = UInt(OUTPUT, 5)
    val irq_aux = Bits(OUTPUT, 64)

    val vcmdq = new VCMDQIO().flip
    val vpfcmdq = new VCMDQIO().flip

    val vcmdq_user_ready = Bool(OUTPUT)
    val vimm1q_user_ready = Bool(OUTPUT)
    val vimm2q_user_ready = Bool(OUTPUT)
    val vfence_ready = Bool(OUTPUT)

    val cp_dfma = new rocket.ioFMA(65).flip
    val cp_sfma = new rocket.ioFMA(33).flip

    val imem_req = new io_imem_req()
    val imem_resp = new io_imem_resp().flip

    val dmem_req = new io_dmem_req()
    val dmem_resp = new io_dmem_resp().flip

    val vtlb = new io_tlb
    val vpftlb = new io_tlb

    val xcpt = new io_xcpt().flip
  }

  val xcpt = Module(new XCPT())

  val flush_kill = this.reset || xcpt.io.xcpt_to_vu.flush_kill
  val flush_irq = this.reset || xcpt.io.xcpt_to_vu.flush_irq
  val flush_aiw = this.reset || xcpt.io.xcpt_to_vu.flush_aiw
  val flush_vru = this.reset || xcpt.io.xcpt_to_vu.flush_vru
  val flush_vmu = this.reset || xcpt.io.xcpt_to_vu.flush_vmu

  val vcmdq = new {
    val cmd = Module(new Queue(Bits(width = SZ_VCMD), conf.vcmdq.ncmd, _reset = flush_kill))
    val imm1 = Module(new Queue(Bits(width = SZ_VIMM), conf.vcmdq.nimm1, _reset = flush_kill))
    val imm2 = Module(new Queue(Bits(width = SZ_VSTRIDE), conf.vcmdq.nimm2, _reset = flush_kill))
    val cnt = Module(new Queue(Bits(width = SZ_VLEN+1), conf.vcmdq.ncnt, _reset = flush_kill))
  }

  vcmdq.cmd.io.enq <> MaskStall(io.vcmdq.cmd, xcpt.io.xcpt_to_vu.busy)
  vcmdq.imm1.io.enq <> MaskStall(io.vcmdq.imm1, xcpt.io.xcpt_to_vu.busy)
  vcmdq.imm2.io.enq <> MaskStall(io.vcmdq.imm2, xcpt.io.xcpt_to_vu.busy)
  vcmdq.cnt.io.enq <> MaskStall(io.vcmdq.cnt, xcpt.io.xcpt_to_vu.busy)

  val vxu = Module(new VXU)
  val vmu = Module(new VMU(resetSignal = flush_vmu))
  val irq = Module(new IRQ(resetSignal = flush_irq))
  val aiw = Module(new AIW(resetSignal = flush_aiw))
  val evac = Module(new Evac)

  // counters
  val vcmdqcnt = new {
    val cmd = Module(new qcnt(conf.vcmdq.ncmd, conf.vcmdq.ncmd, resetSignal = flush_kill))
    val imm1 = Module(new qcnt(conf.vcmdq.nimm1, conf.vcmdq.nimm1, resetSignal = flush_kill))
    val imm2 = Module(new qcnt(conf.vcmdq.nimm2, conf.vcmdq.nimm2, resetSignal = flush_kill))
  }

  if (conf.vru)
  {
    val vru = Module(new VRU(resetSignal = flush_vru))
    val vpfcmdq = new {
      val cmd = Module(new Queue(Bits(width=SZ_VCMD), conf.vcmdq.ncmd, _reset = flush_kill))
      val imm1 = Module(new Queue(Bits(width=SZ_VIMM), conf.vcmdq.nimm1, _reset = flush_kill))
      val imm2 = Module(new Queue(Bits(width=SZ_VSTRIDE), conf.vcmdq.nimm2, _reset = flush_kill))
      val cnt = Module(new Queue(Bits(width=SZ_VLEN), conf.vcmdq.ncnt, _reset = flush_kill))
    }

    vpfcmdq.cmd.io.enq <> MaskStall(io.vpfcmdq.cmd, xcpt.io.xcpt_to_vu.busy)
    vpfcmdq.imm1.io.enq <> MaskStall(io.vpfcmdq.imm1, xcpt.io.xcpt_to_vu.busy)
    vpfcmdq.imm2.io.enq <> MaskStall(io.vpfcmdq.imm2, xcpt.io.xcpt_to_vu.busy)
    vpfcmdq.cnt.io.enq <> MaskStall(io.vpfcmdq.cnt, xcpt.io.xcpt_to_vu.busy)

    vru.io.vcmdq.cmd <> vpfcmdq.cmd.io.deq
    vru.io.vcmdq.imm1 <> vpfcmdq.imm1.io.deq
    vru.io.vcmdq.imm2 <> vpfcmdq.imm2.io.deq
    vru.io.vcmdq.cnt <> vpfcmdq.cnt.io.deq

    vmu.io.pf_vvaq <> vru.io.vvaq
    vmu.io.vpftlb <> io.vpftlb
  }
  else
  {
    io.vpfcmdq.cmd.ready := Bool(true)
    io.vpfcmdq.imm1.ready := Bool(true)
    io.vpfcmdq.imm2.ready := Bool(true)
    io.vpfcmdq.cnt.ready := Bool(true)
  }

  vcmdqcnt.cmd.io.dec := vcmdq.cmd.io.enq.ready && io.vcmdq.cmd.valid
  vcmdqcnt.cmd.io.inc := (vxu.io.vxu_cmdq.ready || evac.io.vcmdq.cmd.ready) && vcmdq.cmd.io.deq.valid
  vcmdqcnt.imm1.io.dec := vcmdq.imm1.io.enq.ready && io.vcmdq.imm1.valid
  vcmdqcnt.imm1.io.inc := (vxu.io.vxu_immq.ready || evac.io.vcmdq.imm1.ready) && vcmdq.imm1.io.deq.valid
  vcmdqcnt.imm2.io.dec := vcmdq.imm2.io.enq.ready && io.vcmdq.imm2.valid
  vcmdqcnt.imm2.io.inc := (vxu.io.vxu_imm2q.ready || evac.io.vcmdq.imm2.ready) && vcmdq.imm2.io.deq.valid

  vcmdqcnt.cmd.io.qcnt := UInt(11)
  vcmdqcnt.imm1.io.qcnt := UInt(11)
  vcmdqcnt.imm2.io.qcnt := UInt(9)
  io.vcmdq_user_ready := vcmdqcnt.cmd.io.watermark && !xcpt.io.xcpt_to_vu.busy
  io.vimm1q_user_ready := vcmdqcnt.imm1.io.watermark && !xcpt.io.xcpt_to_vu.busy
  io.vimm2q_user_ready := vcmdqcnt.imm2.io.watermark && !xcpt.io.xcpt_to_vu.busy

  // fence
  io.vfence_ready := !vcmdq.cmd.io.deq.valid && !vxu.io.pending_vf && !vxu.io.pending_memop && !vmu.io.pending_store

  io.irq := irq.io.irq
  io.irq_cause := irq.io.irq_cause
  io.irq_aux := irq.io.irq_aux

  // xcpt
  xcpt.io.xcpt <> io.xcpt

  // vxu
  vxu.io.vxu_cmdq.bits := vcmdq.cmd.io.deq.bits
  vxu.io.vxu_cmdq.valid := vcmdq.cmd.io.deq.valid
  vcmdq.cmd.io.deq.ready := vxu.io.vxu_cmdq.ready || evac.io.vcmdq.cmd.ready

  vxu.io.vxu_immq.bits := vcmdq.imm1.io.deq.bits
  vxu.io.vxu_immq.valid := vcmdq.imm1.io.deq.valid
  vcmdq.imm1.io.deq.ready := vxu.io.vxu_immq.ready || evac.io.vcmdq.imm1.ready

  vxu.io.vxu_imm2q.bits := vcmdq.imm2.io.deq.bits
  vxu.io.vxu_imm2q.valid := vcmdq.imm2.io.deq.valid
  vcmdq.imm2.io.deq.ready := vxu.io.vxu_imm2q.ready || evac.io.vcmdq.imm2.ready

  vxu.io.vxu_cntq.bits := vcmdq.cnt.io.deq.bits(10,0)
  vxu.io.vxu_cntq.valid := vcmdq.cnt.io.deq.valid
  vcmdq.cnt.io.deq.ready := vxu.io.vxu_cntq.ready || evac.io.vcmdq.cnt.ready

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

  vmu.io.vtlb <> io.vtlb

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

  evac.io.vcmdq.cmd.bits := vcmdq.cmd.io.deq.bits
  evac.io.vcmdq.cmd.valid := vcmdq.cmd.io.deq.valid

  evac.io.vcmdq.imm1.bits := vcmdq.imm1.io.deq.bits
  evac.io.vcmdq.imm1.valid := vcmdq.imm1.io.deq.valid

  evac.io.vcmdq.imm2.bits := vcmdq.imm2.io.deq.bits
  evac.io.vcmdq.imm2.valid := vcmdq.imm2.io.deq.valid

  evac.io.vcmdq.cnt.bits := vcmdq.cnt.io.deq.bits
  evac.io.vcmdq.cnt.valid := vcmdq.cnt.io.deq.valid

  evac.io.xcpt_to_evac <> xcpt.io.xcpt_to_evac
  evac.io.evac_to_xcpt <> xcpt.io.evac_to_xcpt
}

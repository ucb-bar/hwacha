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

class VCMDQIO extends Bundle
{
  val cmd = Decoupled(Bits(width = SZ_XCMD))
  val imm1 = Decoupled(Bits(width = SZ_XIMM))
  val imm2 = Decoupled(Bits(width = SZ_XIMM2))
  val cnt = Decoupled(Bits(width = SZ_VLEN+1))
}

class AIWVCMDQIO extends VCMDQIO
{
  val numcnt = Decoupled(Bits(width = 1))
}

class VCMDQ(resetSignal: Bool = null)(implicit conf: HwachaConfiguration) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val enq = new VCMDQIO().flip
    val deq = new VCMDQIO()
  }

  io.deq.cmd <> Queue(io.enq.cmd, conf.vcmdq.ncmd)
  io.deq.imm1 <> Queue(io.enq.imm1, conf.vcmdq.nimm1)
  io.deq.imm2 <> Queue(io.enq.imm2, conf.vcmdq.nimm2)
  io.deq.cnt <> Queue(io.enq.cnt, conf.vcmdq.ncnt)
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

    val busy = Bool(OUTPUT)

    val imem = new rocket.CPUFrontendIO()(conf.icache)

    val dmem_req = new io_dmem_req()
    val dmem_resp = new io_dmem_resp().flip

    val vtlb = new TLBIO
    val vpftlb = new TLBIO

    val xcpt = new io_xcpt().flip
  }

  val xcpt = Module(new XCPT())

  val flush_kill = this.reset || xcpt.io.xcpt_to_vu.flush_kill
  val flush_irq = this.reset || xcpt.io.xcpt_to_vu.flush_irq
  val flush_aiw = this.reset || xcpt.io.xcpt_to_vu.flush_aiw
  val flush_vru = this.reset || xcpt.io.xcpt_to_vu.flush_vru
  val flush_vmu = this.reset || xcpt.io.xcpt_to_vu.flush_vmu

  val vcmdq = Module(new VCMDQ(resetSignal = flush_kill))

  vcmdq.io.enq.cmd <> MaskStall(io.vcmdq.cmd, xcpt.io.xcpt_to_vu.busy)
  vcmdq.io.enq.imm1 <> MaskStall(io.vcmdq.imm1, xcpt.io.xcpt_to_vu.busy)
  vcmdq.io.enq.imm2 <> MaskStall(io.vcmdq.imm2, xcpt.io.xcpt_to_vu.busy)
  vcmdq.io.enq.cnt <> MaskStall(io.vcmdq.cnt, xcpt.io.xcpt_to_vu.busy)

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
    val vpfcmdq = Module(new VCMDQ(resetSignal = flush_kill))

    vpfcmdq.io.enq.cmd <> MaskStall(io.vpfcmdq.cmd, xcpt.io.xcpt_to_vu.busy)
    vpfcmdq.io.enq.imm1 <> MaskStall(io.vpfcmdq.imm1, xcpt.io.xcpt_to_vu.busy)
    vpfcmdq.io.enq.imm2 <> MaskStall(io.vpfcmdq.imm2, xcpt.io.xcpt_to_vu.busy)
    vpfcmdq.io.enq.cnt <> MaskStall(io.vpfcmdq.cnt, xcpt.io.xcpt_to_vu.busy)

    vru.io.vcmdq <> vpfcmdq.io.deq

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

  vcmdqcnt.cmd.io.dec := vcmdq.io.enq.cmd.ready && io.vcmdq.cmd.valid
  vcmdqcnt.cmd.io.inc := (vxu.io.vcmdq.cmd.ready || evac.io.vcmdq.cmd.ready) && vcmdq.io.deq.cmd.valid
  vcmdqcnt.imm1.io.dec := vcmdq.io.enq.imm1.ready && io.vcmdq.imm1.valid
  vcmdqcnt.imm1.io.inc := (vxu.io.vcmdq.imm1.ready || evac.io.vcmdq.imm1.ready) && vcmdq.io.deq.imm1.valid
  vcmdqcnt.imm2.io.dec := vcmdq.io.enq.imm2.ready && io.vcmdq.imm2.valid
  vcmdqcnt.imm2.io.inc := (vxu.io.vcmdq.imm2.ready || evac.io.vcmdq.imm2.ready) && vcmdq.io.deq.imm2.valid

  vcmdqcnt.cmd.io.qcnt := UInt(11)
  vcmdqcnt.imm1.io.qcnt := UInt(11)
  vcmdqcnt.imm2.io.qcnt := UInt(9)
  io.vcmdq_user_ready := vcmdqcnt.cmd.io.watermark && !xcpt.io.xcpt_to_vu.busy
  io.vimm1q_user_ready := vcmdqcnt.imm1.io.watermark && !xcpt.io.xcpt_to_vu.busy
  io.vimm2q_user_ready := vcmdqcnt.imm2.io.watermark && !xcpt.io.xcpt_to_vu.busy

  // fence
  io.busy := vcmdq.io.deq.cmd.valid || vxu.io.pending_vf || vxu.io.pending_memop || vmu.io.pending_store

  io.irq := irq.io.irq
  io.irq_cause := irq.io.irq_cause
  io.irq_aux := irq.io.irq_aux

  // xcpt
  xcpt.io.xcpt <> io.xcpt

  // vxu
  vcmdq.io.deq.cmd.ready := (vxu.io.vcmdq.cmd.ready || evac.io.vcmdq.cmd.ready)
  vcmdq.io.deq.imm1.ready := (vxu.io.vcmdq.imm1.ready || evac.io.vcmdq.imm1.ready)
  vcmdq.io.deq.imm2.ready := (vxu.io.vcmdq.imm2.ready || evac.io.vcmdq.imm2.ready)
  vcmdq.io.deq.cnt.ready := (vxu.io.vcmdq.cnt.ready || evac.io.vcmdq.cnt.ready)

  vxu.io.vcmdq.cmd.valid := vcmdq.io.deq.cmd.valid
  vxu.io.vcmdq.cmd.bits := vcmdq.io.deq.cmd.bits
  vxu.io.vcmdq.imm1.valid := vcmdq.io.deq.imm1.valid
  vxu.io.vcmdq.imm1.bits := vcmdq.io.deq.imm1.bits
  vxu.io.vcmdq.imm2.valid := vcmdq.io.deq.imm2.valid
  vxu.io.vcmdq.imm2.bits := vcmdq.io.deq.imm2.bits
  vxu.io.vcmdq.cnt.valid := vcmdq.io.deq.cnt.valid
  vxu.io.vcmdq.cnt.bits := vcmdq.io.deq.cnt.bits

  evac.io.vcmdq.cmd.valid := vcmdq.io.deq.cmd.valid
  evac.io.vcmdq.cmd.bits := vcmdq.io.deq.cmd.bits
  evac.io.vcmdq.imm1.valid := vcmdq.io.deq.imm1.valid
  evac.io.vcmdq.imm1.bits := vcmdq.io.deq.imm1.bits
  evac.io.vcmdq.imm2.valid := vcmdq.io.deq.imm2.valid
  evac.io.vcmdq.imm2.bits := vcmdq.io.deq.imm2.bits
  evac.io.vcmdq.cnt.valid := vcmdq.io.deq.cnt.valid
  evac.io.vcmdq.cnt.bits := vcmdq.io.deq.cnt.bits

  io.imem <> vxu.io.imem

  vxu.io.xcpt_to_vxu <> xcpt.io.xcpt_to_vxu
  vxu.io.vxu_to_xcpt <> xcpt.io.vxu_to_xcpt

  irq.io.issue_to_irq <> vxu.io.irq

  // vmu
  vmu.io.lane_vvaq <> vxu.io.vaq
  vmu.io.evac_vvaq <> evac.io.vaq

  vmu.io.lane_vsdq <> vxu.io.vsdq
  vmu.io.evac_vsdq <> evac.io.vsdq

  vxu.io.vldq <> vmu.io.lane_vldq

  vmu.io.vxu_to_vmu <> vxu.io.vxu_to_vmu

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

  vmu.io.prec := vxu.io.prec

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

  evac.io.vcmdq.cmd.bits := vcmdq.io.deq.cmd.bits
  evac.io.vcmdq.cmd.valid := vcmdq.io.deq.cmd.valid

  evac.io.vcmdq.imm1.bits := vcmdq.io.deq.imm1.bits
  evac.io.vcmdq.imm1.valid := vcmdq.io.deq.imm1.valid

  evac.io.vcmdq.imm2.bits := vcmdq.io.deq.imm2.bits
  evac.io.vcmdq.imm2.valid := vcmdq.io.deq.imm2.valid

  evac.io.vcmdq.cnt.bits := vcmdq.io.deq.cnt.bits
  evac.io.vcmdq.cnt.valid := vcmdq.io.deq.cnt.valid

  evac.io.xcpt_to_evac <> xcpt.io.xcpt_to_evac
  evac.io.evac_to_xcpt <> xcpt.io.evac_to_xcpt
}

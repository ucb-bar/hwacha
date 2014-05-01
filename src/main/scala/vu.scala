package hwacha

import Chisel._
import Node._
import Constants._

class VCMDQIO extends Bundle
{
  val cmd = Decoupled(new HwachaCommand)
  val imm1 = Decoupled(Bits(width = SZ_VIMM))
  val imm2 = Decoupled(Bits(width = SZ_VSTRIDE))
  val cnt = Decoupled(new HwachaCnt)
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

class TLBIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val req = Decoupled(new rocket.TLBReq()(conf.as))
  val resp = new rocket.TLBResp(1)(conf.as).flip // we don't use hit_idx
}

class VU(resetSignal: Bool = null)(implicit conf: HwachaConfiguration) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val irq = new IRQIO
    val xcpt = new XCPTIO().flip

    val vcmdq = new VCMDQIO().flip
    val vpfcmdq = new VCMDQIO().flip
    val vcmdq_user_ready = Bool(OUTPUT)
    val vimm1q_user_ready = Bool(OUTPUT)
    val vimm2q_user_ready = Bool(OUTPUT)

    val imem = new rocket.CPUFrontendIO()(conf.vicache)
    val dmem = new rocket.HellaCacheIO()(conf.dcache)
    val vtlb = new TLBIO
    val vpftlb = new TLBIO

    val keepcfg = Bool(OUTPUT)
    val busy = Bool(OUTPUT)
  }

  val flush_kill = this.reset || io.xcpt.prop.vu.flush_kill
  val flush_aiw = this.reset || io.xcpt.prop.vu.flush_aiw
  val flush_vru = this.reset || io.xcpt.prop.vu.flush_vru
  val flush_vmu = this.reset || io.xcpt.prop.vu.flush_vmu

  val vcmdq = Module(new VCMDQ(resetSignal = flush_kill))

  vcmdq.io.enq.cmd <> MaskStall(io.vcmdq.cmd, io.xcpt.prop.vu.busy)
  vcmdq.io.enq.imm1 <> MaskStall(io.vcmdq.imm1, io.xcpt.prop.vu.busy)
  vcmdq.io.enq.imm2 <> MaskStall(io.vcmdq.imm2, io.xcpt.prop.vu.busy)
  vcmdq.io.enq.cnt <> MaskStall(io.vcmdq.cnt, io.xcpt.prop.vu.busy)

  val vxu = Module(new VXU)
  val vmu = Module(new VMU(resetSignal = flush_vmu))
  val aiw = Module(new AIW(resetSignal = flush_aiw))
  val evac = Module(new Evac)
  val mrt = Module(new MRT)

  // counters
  val vcmdqcnt = new {
    val cmd = Module(new QCounter(conf.vcmdq.ncmd, conf.vcmdq.ncmd, resetSignal = flush_kill))
    val imm1 = Module(new QCounter(conf.vcmdq.nimm1, conf.vcmdq.nimm1, resetSignal = flush_kill))
    val imm2 = Module(new QCounter(conf.vcmdq.nimm2, conf.vcmdq.nimm2, resetSignal = flush_kill))
  }

  if (conf.vru)
  {
    val vru = Module(new VRU(resetSignal = flush_vru))
    val vpfcmdq = Module(new VCMDQ(resetSignal = flush_kill))

    vpfcmdq.io.enq.cmd <> MaskStall(io.vpfcmdq.cmd, io.xcpt.prop.vu.busy)
    vpfcmdq.io.enq.imm1 <> MaskStall(io.vpfcmdq.imm1, io.xcpt.prop.vu.busy)
    vpfcmdq.io.enq.imm2 <> MaskStall(io.vpfcmdq.imm2, io.xcpt.prop.vu.busy)
    vpfcmdq.io.enq.cnt <> MaskStall(io.vpfcmdq.cnt, io.xcpt.prop.vu.busy)

    vru.io.vcmdq <> vpfcmdq.io.deq

    vmu.io.pf.vaq <> vru.io.vaq
    vmu.io.vpftlb <> io.vpftlb
  }
  else
  {
    io.vpfcmdq.cmd.ready := Bool(true)
    io.vpfcmdq.imm1.ready := Bool(true)
    io.vpfcmdq.imm2.ready := Bool(true)
    io.vpfcmdq.cnt.ready := Bool(true)
    io.vpftlb.req.valid := Bool(false)
  }

  vcmdqcnt.cmd.io.dec := vcmdq.io.enq.cmd.ready && io.vcmdq.cmd.valid
  vcmdqcnt.cmd.io.inc := (vxu.io.vcmdq.cmd.ready || evac.io.vcmdq.cmd.ready) && vcmdq.io.deq.cmd.valid
  vcmdqcnt.imm1.io.dec := vcmdq.io.enq.imm1.ready && io.vcmdq.imm1.valid
  vcmdqcnt.imm1.io.inc := (vxu.io.vcmdq.imm1.ready || evac.io.vcmdq.imm1.ready) && vcmdq.io.deq.imm1.valid
  vcmdqcnt.imm2.io.dec := vcmdq.io.enq.imm2.ready && io.vcmdq.imm2.valid
  vcmdqcnt.imm2.io.inc := (vxu.io.vcmdq.imm2.ready || evac.io.vcmdq.imm2.ready) && vcmdq.io.deq.imm2.valid

  vcmdqcnt.cmd.io.qcnt := UInt(conf.vcmdq.ncmd - conf.nbanks)
  vcmdqcnt.imm1.io.qcnt := UInt(conf.vcmdq.nimm1 - conf.nbanks)
  vcmdqcnt.imm2.io.qcnt := UInt(conf.vcmdq.nimm2 - conf.nbanks)
  io.vcmdq_user_ready := vcmdqcnt.cmd.io.watermark && !io.xcpt.prop.vu.busy
  io.vimm1q_user_ready := vcmdqcnt.imm1.io.watermark && !io.xcpt.prop.vu.busy
  io.vimm2q_user_ready := vcmdqcnt.imm2.io.watermark && !io.xcpt.prop.vu.busy

  // fence
  mrt.io.xcpt <> io.xcpt
  mrt.io.lreq <> vxu.io.lreq
  mrt.io.sreq.vxu <> vxu.io.sreq
  //FIXME Chisel
  //mrt.io.sreq.evac := evac.io.vaq.fire()
  mrt.io.sreq.evac := evac.io.vaq.valid && vmu.io.evac.vaq.ready
  mrt.io.lret <> vxu.io.lret
  mrt.io.sret.update := io.dmem.resp.valid && is_mcmd_store(io.dmem.resp.bits.cmd)

  io.keepcfg :=
    vcmdq.io.deq.cmd.valid ||
    vxu.io.pending_vf || vxu.io.pending_seq

  io.busy :=
    vcmdq.io.deq.cmd.valid ||
    vxu.io.pending_vf || vxu.io.pending_memop ||
    mrt.io.pending_memop

  io.imem <> vxu.io.imem
  io.irq <> vxu.io.irq
  io.irq <> vmu.io.irq

  // vxu
  vxu.io.xcpt <> io.xcpt

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

  // vmu
  vmu.io.xcpt <> io.xcpt

  vmu.io.lane <> vxu.io.vmu
  vmu.io.evac.vaq <> evac.io.vaq
  vmu.io.evac.vsdq <> evac.io.vsdq
  vmu.io.dmem <> io.dmem
  vmu.io.vtlb <> io.vtlb

  // evac
  evac.io.xcpt <> io.xcpt

  evac.io.vcmdq.cmd.bits := vcmdq.io.deq.cmd.bits
  evac.io.vcmdq.cmd.valid := vcmdq.io.deq.cmd.valid
  evac.io.vcmdq.imm1.bits := vcmdq.io.deq.imm1.bits
  evac.io.vcmdq.imm1.valid := vcmdq.io.deq.imm1.valid
  evac.io.vcmdq.imm2.bits := vcmdq.io.deq.imm2.bits
  evac.io.vcmdq.imm2.valid := vcmdq.io.deq.imm2.valid
  evac.io.vcmdq.cnt.bits := vcmdq.io.deq.cnt.bits
  evac.io.vcmdq.cnt.valid := vcmdq.io.deq.cnt.valid

  // aiw
  aiw.io.vxu <> vxu.io.aiw
  aiw.io.evac <> evac.io.aiw
}

package hwacha

import Chisel._
import Node._
import Constants._

class VVAQIO extends DecoupledIO(new VVAQEntry)
class VPAQIO extends DecoupledIO(new VPAQEntry)

class VAQLaneIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val q = new VVAQIO
  val vala = new LookAheadPortIO(log2Down(conf.nvvaq)+1)
  val pala = new LookAheadPortIO(log2Down(conf.nvpaq)+1)
}

class VLDQIO extends DecoupledIO(Bits(width = SZ_DATA))
class VLDQLaneIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val q = new VLDQIO().flip
  val la = new LookAheadPortIO(log2Down(conf.nvldq)+1)
}
class VLDQMemIfIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val update = Valid(new VLDQEnqOp(66, log2Up(conf.nvldq))).flip
  val rtag = Decoupled(Bits(width = log2Up(conf.nvldq)))
}

class VSDQIO extends DecoupledIO(Bits(width = SZ_DATA))
class VSDQLaneIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val q = new VSDQIO
  val la = new LookAheadPortIO(log2Down(conf.nvsdq)+1)
}

class VMUIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val addr = new VAQLaneIO
  val ldata = new VLDQLaneIO
  val sdata = new VSDQLaneIO
}

class VMU(resetSignal: Bool = null)(implicit conf: HwachaConfiguration) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val irq = new IRQVMUIO
    val xcpt = new XCPTVMUIO().flip

    val lane = new VMUIO().flip
    val pf = new Bundle {
      val vaq = new VVAQIO().flip
    }
    val evac = new Bundle {
      val vaq = new VVAQIO().flip
      val vsdq = new VSDQIO().flip
    }

    val dmem = new rocket.HellaCacheIO()(conf.dcache)

    val vtlb = new TLBIO
    val vpftlb = new TLBIO

    val evac_to_vmu = new io_evac_to_vmu().flip
  }

  val addr = Module(new VMUAddress)
  val ldata = Module(new VMULoadData)
  val sdata = Module(new VMUStoreData)
  val memif = Module(new MemIF)

  addr.io.irq <> io.irq
  addr.io.stall := io.xcpt.tlb.stall
  addr.io.pf <> io.pf.vaq
  addr.io.lane <> io.lane.addr
  addr.io.evac <> io.evac.vaq
  addr.io.evac_to_vmu <> io.evac_to_vmu

  ldata.io.lane <> io.lane.ldata

  sdata.io.lane <> io.lane.sdata
  sdata.io.evac <> io.evac.vsdq
  sdata.io.evac_to_vmu <> io.evac_to_vmu

  memif.io.vaq <> addr.io.memif
  memif.io.vldq <> ldata.io.memif
  memif.io.vsdq <> sdata.io.memif

  io.vtlb <> addr.io.vtlb
  io.vpftlb <> addr.io.vpftlb
  io.dmem <> memif.io.dmem
}

class AddressTLB extends Module
{
  val io = new Bundle {
    val vvaq = new VVAQIO().flip
    val vpaq = new VPAQIO
    val tlb = new TLBIO
    val ack = Bool(OUTPUT)
    val stall = Bool(INPUT)

    val irq = new IRQVMUIO
  }

  val sticky_stall = Reg(init=Bool(false))
  val stall = io.stall || sticky_stall

  io.tlb.req.valid := !stall && io.vvaq.valid && io.vpaq.ready
  io.tlb.req.bits.asid := UInt(0)
  io.tlb.req.bits.vpn := io.vvaq.bits.vpn.toUInt
  io.tlb.req.bits.passthrough := Bool(false)
  io.tlb.req.bits.instruction := Bool(false)

  val mcmd_load = is_mcmd_load(io.vvaq.bits.cmd)
  val mcmd_store = is_mcmd_store(io.vvaq.bits.cmd)
  val mcmd_amo = is_mcmd_amo(io.vvaq.bits.cmd)
  val mcmd_pfr = is_mcmd_pfr(io.vvaq.bits.cmd)
  val mcmd_pfw = is_mcmd_pfw(io.vvaq.bits.cmd)

  val ma_half = is_mtype_halfword(io.vvaq.bits.typ) && io.vvaq.bits.idx(0) != UInt(0)
  val ma_word = is_mtype_word(io.vvaq.bits.typ) && io.vvaq.bits.idx(1,0) != UInt(0)
  val ma_double = is_mtype_doubleword(io.vvaq.bits.typ) && io.vvaq.bits.idx(2,0) != UInt(0)
  val ma_addr = ma_half || ma_word || ma_double
  val ma_ld = ma_addr && (mcmd_load || mcmd_amo)
  val ma_st = ma_addr && (mcmd_store || mcmd_amo)

  val xcpt_ld = io.tlb.resp.xcpt_ld && (mcmd_load || mcmd_amo)
  val xcpt_st = io.tlb.resp.xcpt_st && (mcmd_store || mcmd_amo)
  val xcpt_pf = io.tlb.resp.xcpt_ld && mcmd_pfr || io.tlb.resp.xcpt_st && mcmd_pfw
  val xcpt_stall = ma_addr || xcpt_ld || xcpt_st
  val xcpt = xcpt_stall || xcpt_pf

  io.ack := io.tlb.req.fire() && !io.tlb.resp.miss && !xcpt

  io.vvaq.ready := !stall && io.vpaq.ready && io.tlb.req.ready && !io.tlb.resp.miss && !xcpt_stall

  io.vpaq.valid := io.ack
  io.vpaq.bits := io.vvaq.bits
  io.vpaq.bits.addr := Cat(io.tlb.resp.ppn, io.vvaq.bits.idx)

  when (io.tlb.req.fire() && xcpt_stall) { sticky_stall := Bool(true) }

  io.irq.ma_ld := io.tlb.req.fire() && ma_ld
  io.irq.ma_st := io.tlb.req.fire() && ma_st
  io.irq.faulted_ld := io.tlb.req.fire() && xcpt_ld
  io.irq.faulted_st := io.tlb.req.fire() && xcpt_st
  io.irq.mem_xcpt_addr := Cat(io.vvaq.bits.vpn, io.vvaq.bits.idx)
}

class VPAQThrottle(implicit conf: HwachaConfiguration) extends Module
{
  val sz = log2Down(conf.nvpaq)+1
  val io = new Bundle {
    val original = new VPAQIO().flip
    val la = new LookAheadPortIO(sz).flip
    val masked = new VPAQIO
  }

  val reg_count = Reg(init = UInt(0, sz))

  when (io.la.reserve) {
    reg_count := reg_count + io.la.cnt
    when (io.masked.fire()) { reg_count := reg_count + io.la.cnt - UInt(1) }
  }
  .otherwise {
    when (io.masked.fire()) { reg_count := reg_count - UInt(1) }
  }

  val stall = reg_count === UInt(0)

  io.masked.valid := io.original.valid && !stall
  io.masked.bits := io.original.bits
  io.original.ready := io.masked.ready && !stall
}

class VMUAddress(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val irq = new IRQVMUIO
    val stall = Bool(INPUT)

    val pf = new VVAQIO().flip
    val lane = new VAQLaneIO().flip
    val evac = new VVAQIO().flip
    val memif = new VPAQIO

    val vtlb = new TLBIO
    val vpftlb = new TLBIO

    val evac_to_vmu = new io_evac_to_vmu().flip
  }

  // when the lane is valid it should be able to enq
  assert(!io.lane.q.valid || io.lane.q.ready,
    "vaq invariant not met, probably a counter logic problem")

  // VVAQ
  val vvaq_arb = Module(new Arbiter(new VVAQEntry, 2))
  val vvaq = Module(new Queue(new VVAQEntry, conf.nvvaq))
  val vvaq_lacntr = Module(new LookAheadCounter(conf.nvvaq, conf.nvvaq))
  val vvaq_tlb = Module(new AddressTLB)
  val vpaq = Module(new Queue(new VPAQEntry, conf.nvpaq))
  val vpaq_lacntr = Module(new LookAheadCounter(0, conf.nvpaq))
  val vpaq_throttle = Module(new VPAQThrottle)

  vvaq_tlb.io.irq <> io.irq

  // vvaq hookup
  vvaq_arb.io.in(0) <> io.lane.q
  vvaq_arb.io.in(1) <> io.evac
  vvaq.io.enq <> vvaq_arb.io.out

  vvaq_lacntr.io.la <> io.lane.vala
  //FIXME Chisel
  //vvaq_lacntr.io.inc := vvaq.io.deq.fire()
  vvaq_lacntr.io.inc := vvaq.io.deq.valid && vvaq_tlb.io.vvaq.ready
  vvaq_lacntr.io.dec := io.evac_to_vmu.evac_mode && io.evac.fire()

  // vvaq address translation
  vvaq_tlb.io.vvaq <> vvaq.io.deq
  vpaq.io.enq <> vvaq_tlb.io.vpaq
  io.vtlb <> vvaq_tlb.io.tlb
  vvaq_tlb.io.stall := io.stall

  vpaq_lacntr.io.la <> io.lane.pala
  //FIXME Chisel
  //vpaq_lacntr.io.inc := vpaq.io.enq.fire()
  vpaq_lacntr.io.inc := vvaq_tlb.io.vpaq.valid && vpaq.io.enq.ready
  vpaq_lacntr.io.dec := Bool(false)

  vpaq_throttle.io.original <> vpaq.io.deq
  vpaq_throttle.io.la <> io.lane.pala

  if (conf.vru) {
    val vpfvaq = Module(new Queue(new VVAQEntry, conf.nvpfvaq))
    val vpfvaq_tlb = Module(new AddressTLB)
    val vpfpaq = Module(new Queue(new VPAQEntry, conf.nvpfpaq))

    vpfvaq.io.enq <> io.pf

    vpfvaq_tlb.io.vvaq <> vpfvaq.io.deq
    vpfpaq.io.enq <> vpfvaq_tlb.io.vpaq
    io.vpftlb <> vpfvaq_tlb.io.tlb
    vpfvaq_tlb.io.stall := io.stall

    val vpaq_arb = Module(new RRArbiter(new VPAQEntry, 2))

    vpaq_arb.io.in(0) <> vpaq_throttle.io.masked
    vpaq_arb.io.in(1) <> MaskStall(vpfpaq.io.deq, io.stall)
    io.memif <> vpaq_arb.io.out
  }
  else {
    io.memif <> vpaq_throttle.io.masked
  }
}

class VMULoadData(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val lane = new VLDQLaneIO().flip
    val memif = new VLDQMemIfIO
  }

  // when the lane is ready it should be able to deq
  assert(!io.lane.q.ready || io.lane.q.valid,
    "vldq invariant not met, probably a counter logic problem")

  val vldq = Module(new VLDQ(66, conf.nvldq, 10))

  vldq.io.enq <> io.memif.update
  io.lane.q <> vldq.io.deq_data
  io.memif.rtag <> vldq.io.deq_rtag
  vldq.io.la <> io.lane.la
}

class VMUStoreData(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val lane = new VSDQLaneIO().flip
    val evac = new VSDQIO().flip
    val memif = new VSDQIO

    val evac_to_vmu = new io_evac_to_vmu().flip
  }

  // when the lane is valid it should be able to enq
  assert(!io.lane.q.valid || io.lane.q.ready,
    "vsdq invariant not met, probably a counter logic problem")

  val arb = Module(new Arbiter(Bits(width = 65), 2))
  val vsdq = Module(new Queue(Bits(width = 65), conf.nvsdq))
  val lacntr = Module(new LookAheadCounter(conf.nvsdq, conf.nvsdq))

  arb.io.in(0) <> io.lane.q
  arb.io.in(1) <> io.evac
  vsdq.io.enq <> arb.io.out
  io.memif <> vsdq.io.deq

  lacntr.io.la <> io.lane.la
  lacntr.io.inc := vsdq.io.deq.fire()
  lacntr.io.dec := io.evac_to_vmu.evac_mode && io.evac.fire()
}

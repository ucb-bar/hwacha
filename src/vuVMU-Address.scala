package hwacha

import Chisel._
import Node._
import Constants._

class io_vmu_to_irq_handler extends Bundle
{
  val ma_ld = Bool(OUTPUT)
  val ma_st = Bool(OUTPUT)
  val faulted_ld = Bool(OUTPUT)
  val faulted_st = Bool(OUTPUT)
  val mem_xcpt_addr = Bits(OUTPUT, SZ_ADDR)
}

class io_vmu_address_tlb extends Bundle
{
  val vvaq = new io_vvaq().flip
  val vpaq = new io_vpaq()
  val tlb = new io_tlb
  val ack = Bool(OUTPUT)
  val stall = Bool(INPUT)

  val irq = new io_vmu_to_irq_handler()
}

class vuVMU_AddressTLB(resetSignal: Bool = null) extends Component(resetSignal)
{
  val io = new io_vmu_address_tlb()

  val sticky_stall = Reg(resetVal = Bool(false))
  val stall = io.stall || sticky_stall

  io.tlb.req.valid := !stall && io.vvaq.valid && io.vpaq.ready
  io.tlb.req.bits.asid := UFix(0)
  io.tlb.req.bits.vpn := io.vvaq.bits.vpn.toUFix
  io.tlb.req.bits.passthrough := Bool(false)
  io.tlb.req.bits.instruction := Bool(false)

  val mcmd_load = is_mcmd_load(io.vvaq.bits.cmd)
  val mcmd_store = is_mcmd_store(io.vvaq.bits.cmd)
  val mcmd_amo = is_mcmd_amo(io.vvaq.bits.cmd)
  val mcmd_pfr = is_mcmd_pfr(io.vvaq.bits.cmd)
  val mcmd_pfw = is_mcmd_pfw(io.vvaq.bits.cmd)

  val ma_half = is_mtype_halfword(io.vvaq.bits.typ) && io.vvaq.bits.idx(0) != UFix(0)
  val ma_word = is_mtype_word(io.vvaq.bits.typ) && io.vvaq.bits.idx(1,0) != UFix(0)
  val ma_double = is_mtype_doubleword(io.vvaq.bits.typ) && io.vvaq.bits.idx(2,0) != UFix(0)
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

class checkcnt extends Component
{
  val io = new Bundle()
  {
    val input = new io_vpaq().flip
    val output = new io_vpaq()
    val qcnt = UFix(OUTPUT, SZ_QCNT)
    val watermark = Bool(INPUT)
  }

  io.qcnt := io.input.bits.cnt.toUFix
  io.output.valid := io.input.valid && (!io.input.bits.checkcnt || io.watermark)
  io.output.bits := io.input.bits
  io.input.ready := io.output.ready && (!io.input.bits.checkcnt || io.watermark)
}

object CheckCnt
{
  def apply(deq: FIFOIO[io_vpaq_bundle], qcnt: UFix, watermark: Bool) =
  {
    val cc = new checkcnt
    cc.io.input <> deq
    qcnt := cc.io.qcnt
    cc.io.watermark := watermark
    cc.io.output
  }
}

class io_vpaq_to_xcpt_handler extends Bundle 
{
  val vpaq_valid = Bool(OUTPUT)
}

class io_vmu_address extends Bundle
{
  val vvaq_pf = new io_vvaq().flip

  val vvaq_lane = new io_vvaq().flip
  val vvaq_evac = new io_vvaq().flip

  val vtlb = new io_tlb
  val vpftlb = new io_tlb

  val vaq = new io_vpaq()

  val vvaq_lane_dec = Bool(INPUT)

  val vvaq_do_enq = Bool(OUTPUT)
  val vvaq_do_deq = Bool(OUTPUT)
  val vpaq_do_enq = Bool(OUTPUT)
  val vpaq_do_deq = Bool(OUTPUT)
  val vpaq_do_enq_vsdq = Bool(OUTPUT)
  val vpaq_qcnt = UFix(OUTPUT, SZ_QCNT)
  val vvaq_watermark = Bool(INPUT)
  val vpaq_watermark = Bool(INPUT)
  val vsreq_watermark = Bool(INPUT)
  val vlreq_watermark = Bool(INPUT)

  val vpaq_to_xcpt = new io_vpaq_to_xcpt_handler()
  val evac_to_vmu = new io_evac_to_vmu().flip

  val stall = Bool(INPUT)

  val irq = new io_vmu_to_irq_handler()
}

class vuVMU_Address(resetSignal: Bool = null) extends Component(resetSignal)
{
  val io = new io_vmu_address()

  // VVAQ
  val vvaq_arb = (new Arbiter(2)){ new io_vvaq_bundle() }
  val vvaq = new Queue(ENTRIES_VVAQ)(new io_vvaq_bundle())
  val vvaq_tlb = new vuVMU_AddressTLB()
  val vpaq = new Queue(ENTRIES_VPAQ)(new io_vpaq_bundle())

  vvaq_tlb.io.irq <> io.irq

  // vvaq hookup
  vvaq_arb.io.in(VVAQARB_LANE) <> io.vvaq_lane
  vvaq_arb.io.in(VVAQARB_EVAC) <> io.vvaq_evac
  // ready signal a little bit conservative, since checking space for both
  // vsreq and vlreq, not looking at the request type
  // however, this is okay since normally you don't hit this limit
  vvaq_arb.io.out.ready :=
    Mux(io.evac_to_vmu.evac_mode, vvaq.io.enq.ready,
        io.vvaq_watermark && io.vsreq_watermark && io.vlreq_watermark)
  vvaq.io.enq.valid := vvaq_arb.io.out.valid
  vvaq.io.enq.bits := vvaq_arb.io.out.bits

  // vvaq address translation
  vvaq_tlb.io.vvaq <> vvaq.io.deq
  vvaq_tlb.io.vpaq <> vpaq.io.enq
  vvaq_tlb.io.tlb <> io.vtlb
  vvaq_tlb.io.stall := io.stall

  io.vvaq_do_enq :=
    Mux(io.evac_to_vmu.evac_mode, vvaq.io.enq.ready && io.vvaq_evac.valid,
        io.vvaq_lane_dec)
  io.vvaq_do_deq := vvaq_tlb.io.vvaq.ready && vvaq.io.deq.valid

  if (HAVE_VRU)
  {
    // VPFVAQ
    val vpfvaq = new Queue(ENTRIES_VPFVAQ)(new io_vvaq_bundle())
    val vpfvaq_tlb = new vuVMU_AddressTLB()
    val vpfpaq = new Queue(ENTRIES_VPFPAQ)(new io_vpaq_bundle())

    // vpfvaq hookup
    vpfvaq.io.enq <> io.vvaq_pf

    // vpfvaq address translation
    vpfvaq_tlb.io.vvaq <> vpfvaq.io.deq
    vpfvaq_tlb.io.vpaq <> vpfpaq.io.enq
    vpfvaq_tlb.io.tlb <> io.vpftlb
    vpfvaq_tlb.io.stall := io.stall

    // VPAQ and VPFPAQ arbiter
    val vpaq_arb = (new RRArbiter(2)){ new io_vpaq_bundle() }

    val vpaq_check_cnt = CheckCnt(vpaq.io.deq, io.vpaq_qcnt, io.vpaq_watermark)
    io.vpaq_to_xcpt.vpaq_valid := vpaq_check_cnt.valid

    vpaq_arb.io.in(VPAQARB_VPAQ) <> vpaq_check_cnt
    vpaq_arb.io.in(VPAQARB_VPFPAQ) <> MaskStall(vpfpaq.io.deq, io.stall)
    vpaq_arb.io.out <> io.vaq

    io.vpaq_do_enq := vvaq_tlb.io.ack
    io.vpaq_do_deq := vpaq_check_cnt.valid && vpaq_arb.io.in(VPAQARB_VPAQ).ready
    io.vpaq_do_enq_vsdq :=
      vvaq_tlb.io.ack &&
      (is_mcmd_store(vvaq_tlb.io.vpaq.bits.cmd) || is_mcmd_amo(vvaq_tlb.io.vpaq.bits.cmd))
  }
  else
  {
    val vpaq_check_cnt = CheckCnt(vpaq.io.deq, io.vpaq_qcnt, io.vpaq_watermark)
    io.vpaq_to_xcpt.vpaq_valid := vpaq_check_cnt.valid

    io.vaq <> vpaq_check_cnt
    io.vpaq_do_enq := vvaq_tlb.io.ack
    io.vpaq_do_deq := vpaq_check_cnt.valid && io.vaq.ready
    io.vpaq_do_enq_vsdq :=
      vvaq_tlb.io.ack &&
      (is_mcmd_store(vvaq_tlb.io.vpaq.bits.cmd) || is_mcmd_amo(vvaq_tlb.io.vpaq.bits.cmd))

  }
}

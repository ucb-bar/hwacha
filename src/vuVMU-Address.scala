package hwacha

import Chisel._
import Node._
import Constants._

class io_vmu_addr_tlb_irq extends Bundle {
  val ma_ld = Bool(OUTPUT)
  val ma_st = Bool(OUTPUT)
  val faulted_ld = Bool(OUTPUT)
  val faulted_st = Bool(OUTPUT)
  val mem_xcpt_addr = Bits(SZ_ADDR, OUTPUT)
}

class io_vmu_address_tlb extends Bundle
{
  val vvaq = new io_vvaq().flip
  val vpaq = new io_vpaq()
  val tlb_req = new ioDTLB_CPU_req()
  val tlb_resp = new ioDTLB_CPU_resp().flip
  val ack = Bool(OUTPUT)
  val flush = Bool(INPUT)
  val stall = Bool(INPUT)

  val irq = new io_vmu_addr_tlb_irq()
}

class vuVMU_AddressTLB(late_tlb_miss: Boolean = false) extends Component
{
  val io = new io_vmu_address_tlb()

  val vvaq_skid = SkidBuffer(io.vvaq, late_tlb_miss, flushable = true)

  // check if address misaligned
  val mem_cmd = vvaq_skid.io.pipereg.bits.cmd
  val mem_type = vvaq_skid.io.pipereg.bits.typ
  val mem_idx = vvaq_skid.io.pipereg.bits.idx
  val mem_vpn = vvaq_skid.io.pipereg.bits.vpn
  val ma_half = mem_type === mtyp_H && mem_idx(0) != UFix(0)
  val ma_word = mem_type === mtyp_W && mem_idx(1,0) != UFix(0)
  val ma_double = mem_type === mtyp_D && mem_idx(2,0) != UFix(0)
  val ma_addr = ma_half || ma_word || ma_double
  val ma_ld = ma_addr && (is_mcmd_load(mem_cmd) || is_mcmd_amo(mem_cmd))
  val ma_st = ma_addr && (is_mcmd_store(mem_cmd) || is_mcmd_amo(mem_cmd)) // TODO: VALID SIGNAL!

  // sticky stall
  val sticky_stall = Reg(resetVal = Bool(false))
  val xcpt_ready = !vvaq_skid.io.kill && vvaq_skid.io.pipereg.valid
  val xcpt = (ma_addr || io.tlb_resp.xcpt_ld || io.tlb_resp.xcpt_st) && xcpt_ready
  val stall = xcpt || sticky_stall || io.stall

  when (xcpt) { sticky_stall := Bool(true) }
  when (io.flush) { sticky_stall := Bool(false) }

  // irq stuff
  io.irq.ma_ld := ma_ld && xcpt_ready
  io.irq.ma_st := ma_st && xcpt_ready
  io.irq.faulted_ld := io.tlb_resp.xcpt_ld && xcpt_ready
  io.irq.faulted_st := io.tlb_resp.xcpt_st && xcpt_ready
  io.irq.mem_xcpt_addr := Cat(mem_vpn, mem_idx)

  // tlb signals
  val tlb_ready = io.tlb_req.ready && !stall
  var tlb_vec_valid = vvaq_skid.io.deq.valid
  if (late_tlb_miss) tlb_vec_valid = vvaq_skid.io.deq.valid && io.vpaq.ready
  val tlb_vec_requested = Reg(tlb_vec_valid && tlb_ready) && !vvaq_skid.io.kill && !stall
  val tlb_vec_hit = tlb_vec_requested && !io.tlb_resp.miss
  val tlb_vec_miss = tlb_vec_requested && io.tlb_resp.miss

  // ack
  io.ack := tlb_vec_hit && io.vpaq.ready

  // skid control
  vvaq_skid.io.deq.ready := tlb_ready
  vvaq_skid.io.nack := tlb_vec_miss || !io.vpaq.ready || stall

  // tlb hookup
  io.tlb_req.valid := tlb_vec_valid
  io.tlb_req.bits.kill := vvaq_skid.io.kill
  io.tlb_req.bits.cmd := vvaq_skid.io.deq.bits.cmd
  io.tlb_req.bits.vpn := vvaq_skid.io.deq.bits.vpn
  io.tlb_req.bits.asid := Bits(0)

  // enqueue everything but the page number from virtual queue
  io.vpaq.valid := tlb_vec_hit
  io.vpaq.bits.checkcnt := Reg(vvaq_skid.io.deq.bits.checkcnt)
  io.vpaq.bits.cnt := Reg(vvaq_skid.io.deq.bits.cnt)
  io.vpaq.bits.cmd := Reg(vvaq_skid.io.deq.bits.cmd)
  io.vpaq.bits.typ := Reg(vvaq_skid.io.deq.bits.typ)
  io.vpaq.bits.typ_float := Reg(vvaq_skid.io.deq.bits.typ_float)
  io.vpaq.bits.idx := Reg(vvaq_skid.io.deq.bits.idx)
  io.vpaq.bits.ppn := io.tlb_resp.ppn

  // exception handler
  vvaq_skid.io.flush := io.flush
}

class checkcnt extends Component
{
  val io = new Bundle()
  {
    val input = new io_vpaq().flip
    val output = new io_vpaq()
    val qcnt = UFix(SZ_QCNT, OUTPUT)
    val watermark = Bool(INPUT)
  }

  io.qcnt := io.input.bits.cnt.toUFix
  io.output.valid := io.input.valid && (!io.input.bits.checkcnt || io.watermark)
  io.output.bits := io.input.bits
  io.input.ready := io.output.ready && (!io.input.bits.checkcnt || io.watermark)
}

object CheckCnt
{
  def apply(deq: ioDecoupled[io_vpaq_bundle], qcnt: UFix, watermark: Bool) =
  {
    val cc = new checkcnt
    cc.io.input <> deq
    qcnt := cc.io.qcnt
    cc.io.watermark := watermark
    cc.io.output
  }
}

class maskstall extends Component
{
  val io = new Bundle()
  {
    val input = new io_vpaq().flip
    val output = new io_vpaq()
    val stall = Bool(INPUT)
  }

  io.output.valid := io.input.valid && !io.stall
  io.output.bits := io.input.bits
  io.input.ready := io.output.ready && !io.stall
}

object MaskStall
{
  def apply(deq: ioDecoupled[io_vpaq_bundle], stall: Bool) =
  {
    val ms = new maskstall
    ms.io.input <> deq
    ms.io.stall := stall
    ms.io.output
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

  val vec_tlb_req = new ioDTLB_CPU_req()
  val vec_tlb_resp = new ioDTLB_CPU_resp().flip

  val vec_pftlb_req = new ioDTLB_CPU_req()
  val vec_pftlb_resp = new ioDTLB_CPU_resp().flip

  val vaq = new io_vpaq()

  val vvaq_lane_dec = Bool(INPUT)

  val vvaq_do_enq = Bool(OUTPUT)
  val vvaq_do_deq = Bool(OUTPUT)
  val vpaq_do_enq = Bool(OUTPUT)
  val vpaq_do_deq = Bool(OUTPUT)
  val vpaq_do_enq_vsdq = Bool(OUTPUT)
  val vpaq_qcnt = UFix(SZ_QCNT, OUTPUT)
  val vvaq_watermark = Bool(INPUT)
  val vpaq_watermark = Bool(INPUT)
  val vsreq_watermark = Bool(INPUT)
  val vlreq_watermark = Bool(INPUT)

  val vpaq_to_xcpt = new io_vpaq_to_xcpt_handler()
  val evac_to_vmu = new io_evac_to_vmu().flip

  val flush = Bool(INPUT)
  val stall = Bool(INPUT)

  val irq = new io_vmu_addr_tlb_irq()
}

class vuVMU_Address extends Component
{
  val io = new io_vmu_address()

  // VVAQ
  val vvaq_arb = (new Arbiter(2)){ new io_vvaq() }
  val vvaq = (new queueSimplePF(ENTRIES_VVAQ, flushable = true)){ new io_vvaq_bundle() }
  val vvaq_tlb = new vuVMU_AddressTLB(LATE_TLB_MISS)
  val vpaq = (new queueSimplePF(ENTRIES_VPAQ, flushable = true)){ new io_vpaq_bundle() }

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
  vpaq.io.enq <> vvaq_tlb.io.vpaq
  io.vec_tlb_req <> vvaq_tlb.io.tlb_req
  vvaq_tlb.io.tlb_resp <> io.vec_tlb_resp

  io.vvaq_do_enq :=
    Mux(io.evac_to_vmu.evac_mode, vvaq.io.enq.ready && io.vvaq_evac.valid,
        io.vvaq_lane_dec)
  io.vvaq_do_deq := vvaq_tlb.io.vvaq.ready && vvaq.io.deq.valid

  if (HAVE_VRU)
  {
    // VPFVAQ
    val vpfvaq = (new queueSimplePF(ENTRIES_VPFVAQ, flushable = true)){ new io_vvaq_bundle() }
    val vpfvaq_tlb = new vuVMU_AddressTLB(LATE_TLB_MISS)
    val vpfpaq = (new queueSimplePF(ENTRIES_VPFPAQ, flushable = true)){ new io_vpaq_bundle() }

    // vpfvaq hookup
    vpfvaq.io.enq <> io.vvaq_pf

    // vpfvaq address translation
    vpfvaq_tlb.io.vvaq <> vpfvaq.io.deq
    vpfpaq.io.enq <> vpfvaq_tlb.io.vpaq
    io.vec_pftlb_req <> vpfvaq_tlb.io.tlb_req
    vpfvaq_tlb.io.tlb_resp <> io.vec_pftlb_resp

    vpfvaq.io.flush := io.flush
    vpfvaq_tlb.io.flush := io.flush
    vpfvaq_tlb.io.stall := io.stall

    vpfpaq.io.flush := io.flush
    // VPAQ and VPFPAQ arbiter
    val vpaq_arb = (new RoundRobinArbiter(2)){ new io_vpaq() }

    val vpaq_check_cnt = CheckCnt(vpaq.io.deq, io.vpaq_qcnt, io.vpaq_watermark)
    io.vpaq_to_xcpt.vpaq_valid := vpaq_check_cnt.valid

    vpaq_arb.io.in(VPAQARB_VPAQ) <> vpaq_check_cnt
    vpaq_arb.io.in(VPAQARB_VPFPAQ) <> MaskStall(vpfpaq.io.deq, io.stall)
    io.vaq <> vpaq_arb.io.out

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

  // exception handler
  vvaq.io.flush := io.flush
  vvaq_tlb.io.flush := io.flush
  vvaq_tlb.io.stall := io.stall

  vpaq.io.flush := io.flush
}

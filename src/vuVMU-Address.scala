package hwacha

import Chisel._
import Node._
import Constants._

class io_vmu_address_tlb extends Bundle
{
  val vvaq = new io_vvaq().flip
  val vpaq = new io_vpaq()
  val tlb_req = new ioDTLB_CPU_req()
  val tlb_resp = new ioDTLB_CPU_resp().flip
  val ack = Bool(OUTPUT)
}

class vuVMU_AddressTLB(late_tlb_miss: Boolean = false) extends Component
{
  val io = new io_vmu_address_tlb()

  val vvaq_skid = SkidBuffer(io.vvaq, late_tlb_miss)

  // tlb signals
  var tlb_vec_valid = vvaq_skid.io.deq.valid
  if (late_tlb_miss) tlb_vec_valid = vvaq_skid.io.deq.valid && io.vpaq.ready
  val tlb_vec_requested = Reg(tlb_vec_valid && io.tlb_req.ready) && !vvaq_skid.io.kill
  val tlb_vec_hit = tlb_vec_requested && !io.tlb_resp.miss
  val tlb_vec_miss = tlb_vec_requested && io.tlb_resp.miss

  // ack
  io.ack := tlb_vec_hit && io.vpaq.ready

  // skid control
  vvaq_skid.io.deq.ready := io.tlb_req.ready
  vvaq_skid.io.nack := tlb_vec_miss || !io.vpaq.ready

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
}

class checkcnt extends Component
{
  val io = new Bundle()
  {
    val input = new io_vpaq().flip
    val output = new io_vpaq()
    val qcnt = UFix(5, OUTPUT)
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

class io_vmu_address_arbiter extends Bundle
{
  val vpaq = new io_vpaq().flip
  val vpfpaq = new io_vpaq().flip
  val qcnt = UFix(5, OUTPUT)
  val watermark = Bool(INPUT)
  val vaq = new io_vpaq()
  val ack = Bool(INPUT)
  val nack = Bool(INPUT)
  val vpaq_ack = Bool(OUTPUT)
  val vpfpaq_ack = Bool(OUTPUT)
}

class vuVMU_AddressArbiter(late_nack: Boolean = false) extends Component
{
  val io = new io_vmu_address_arbiter()

  val vpaq_skid = SkidBuffer(io.vpaq, late_nack)
  val vpfpaq_skid = SkidBuffer(io.vpfpaq, late_nack)

  val vpaq_arb = new Arbiter(2)( new io_vpaq() )

  val VPAQARB_VPAQ = 0
  val VPAQARB_VPFPAQ = 1

  vpaq_arb.io.in(VPAQARB_VPAQ) <> CheckCnt(vpaq_skid.io.deq, io.qcnt, io.watermark)
  vpaq_arb.io.in(VPAQARB_VPFPAQ) <> vpfpaq_skid.io.deq
  io.vaq <> vpaq_arb.io.out
  val reg_vpaq_arb_chosen = Reg(vpaq_arb.io.chosen)

  io.vpaq_ack := io.ack && reg_vpaq_arb_chosen === Bits(VPAQARB_VPAQ)
  io.vpfpaq_ack := io.ack && reg_vpaq_arb_chosen === Bits(VPAQARB_VPFPAQ)

  vpaq_skid.io.nack := io.nack
  vpfpaq_skid.io.nack := io.nack
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
  val vaq_ack = Bool(INPUT)
  val vaq_nack = Bool(INPUT)

  val vvaq_lane_dec = Bool(INPUT)

  val vvaq_inc = Bool(OUTPUT)
  val vvaq_dec = Bool(OUTPUT)
  val vpaq_inc = Bool(OUTPUT)
  val vpaq_dec = Bool(OUTPUT)
  val vpaq_qcnt = UFix(5, OUTPUT)
  val vvaq_watermark = Bool(INPUT)
  val vpaq_watermark = Bool(INPUT)
  val vsreq_watermark = Bool(INPUT)
  val vlreq_watermark = Bool(INPUT)
}

class vuVMU_Address extends Component
{
  val io = new io_vmu_address()

  // VVAQ
  val vvaq_arb = new Arbiter(2)( new io_vvaq() )
  val VVAQARB_LANE = 0
  val VVAQARB_EVAC = 1

  val vvaq = (new queueSimplePF(16)){ new io_vvaq_bundle() }
  val vvaq_tlb = new vuVMU_AddressTLB(late_tlb_miss = true)
  val vpaq = (new queueSimplePF(16)){ new io_vpaq_bundle() }

  // vvaq arbiter, port 0: lane vaq
  vvaq_arb.io.in(VVAQARB_LANE) <> io.vvaq_lane
  // vvaq arbiter, port 1: evac
  vvaq_arb.io.in(VVAQARB_EVAC) <> io.vvaq_evac
  // vvaq arbiter, output
  // ready signal a little bit conservative, since checking space for both
  // vsreq and vlreq, not looking at the request type
  // however, this is okay since normally you don't hit this limit
  vvaq_arb.io.out.ready := io.vvaq_watermark && io.vsreq_watermark && io.vlreq_watermark // vaq.io.enq.ready
  vvaq.io.enq.valid := vvaq_arb.io.out.valid
  vvaq.io.enq.bits := vvaq_arb.io.out.bits

  // vvaq address translation
  vvaq_tlb.io.vvaq <> vvaq.io.deq
  vpaq.io.enq <> vvaq_tlb.io.vpaq
  io.vec_tlb_req <> vvaq_tlb.io.tlb_req
  vvaq_tlb.io.tlb_resp <> io.vec_tlb_resp

  // vvaq counts available space
  // vvaq frees an entry, when vvaq kicks out an entry to the skid buffer
  io.vvaq_inc := vvaq_tlb.io.vvaq.ready && vvaq.io.deq.valid
  // vvaq occupies an entry, when the lane kicks out an entry
  io.vvaq_dec := io.vvaq_lane_dec

  // VPFVAQ
  val vpfvaq = (new queueSimplePF(16)){ new io_vvaq_bundle() }
  val vpfvaq_tlb = new vuVMU_AddressTLB(late_tlb_miss = true)
  val vpfpaq = (new queueSimplePF(16)){ new io_vpaq_bundle() }

  // vpfvaq hookup
  vpfvaq.io.enq <> io.vvaq_pf

  // vpfvaq address translation
  vpfvaq_tlb.io.vvaq <> vpfvaq.io.deq
  vpfpaq.io.enq <> vpfvaq_tlb.io.vpaq
  io.vec_pftlb_req <> vpfvaq_tlb.io.tlb_req
  vpfvaq_tlb.io.tlb_resp <> io.vec_pftlb_resp

  // VPAQ and VPFPAQ arbiter
  val vpaq_arb = new vuVMU_AddressArbiter(late_nack = true)

  vpaq_arb.io.vpaq <> vpaq.io.deq
  vpaq_arb.io.vpfpaq <> vpfpaq.io.deq
  io.vpaq_qcnt := vpaq_arb.io.qcnt
  vpaq_arb.io.watermark := io.vpaq_watermark
  io.vaq <> vpaq_arb.io.vaq
  vpaq_arb.io.ack := io.vaq_ack
  vpaq_arb.io.nack := io.vaq_nack

  // vpaq counts occupied space
  // vpaq occupies an entry, when it accepts an entry from vvaq
  io.vpaq_inc := vvaq_tlb.io.ack
  // vpaq frees an entry, when the memory system drains it
  io.vpaq_dec := vpaq_arb.io.vpaq_ack
}

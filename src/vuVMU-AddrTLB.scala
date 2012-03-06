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

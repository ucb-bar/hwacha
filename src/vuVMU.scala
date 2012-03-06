package hwacha

import Chisel._
import Node._
import Constants._

class io_vmu extends Bundle
{
  val pf_vvaq = new io_vvaq().flip

  val lane_vvaq = new io_vvaq().flip
  val evac_vvaq = new io_vvaq().flip

  val lane_vsdq = new io_vsdq().flip
  val evac_vsdq = new io_vsdq().flip

  val lane_vldq = new io_vldq()

  val lane_vaq_dec = Bool(INPUT)
  val lane_vsdq_dec = Bool(INPUT)

  val qcnt = UFix(5, INPUT)

  val pending_store = Bool(OUTPUT)

  val dmem_req = new io_dmem_req()
  val dmem_resp = new io_dmem_resp().flip

  val vec_tlb_req = new ioDTLB_CPU_req()
  val vec_tlb_resp = new ioDTLB_CPU_resp().flip

  val vec_pftlb_req = new ioDTLB_CPU_req()
  val vec_pftlb_resp = new ioDTLB_CPU_resp().flip
}

class vuVMU extends Component
{
  val io = new io_vmu()

  val memif = new vuMemIF()

  val vvaq_count = new qcnt(16,16)
  val vpaq_count = new qcnt(0,16)
  val vsdq_count = new qcnt(16,16)
  val vsreq_count = new qcnt(31,31) // vector stores in flight
  val vlreq_count = new qcnt(128,128) // vector loads in flight

  // VVAQ
  val vvaq_arb = new Arbiter(2)( new io_vvaq() )
  val VVAQARB_LANE = 0
  val VVAQARB_EVAC = 1

  val vvaq = (new queueSimplePF(16)){ new io_vvaq_bundle() }
  val vvaq_tlb = new vuVMU_AddressTLB(late_tlb_miss = true)
  val vpaq = (new queueSimplePF(16)){ new io_vpaq_bundle() }

  // vvaq arbiter, port 0: lane vaq
  vvaq_arb.io.in(VVAQARB_LANE) <> io.lane_vvaq
  // vvaq arbiter, port 1: evac
  vvaq_arb.io.in(VVAQARB_EVAC) <> io.evac_vvaq
  // vvaq arbiter, output
  // ready signal a little bit conservative, since checking space for both
  // vsreq and vlreq, not looking at the request type
  // however, this is okay since normally you don't hit this limit
  vvaq_arb.io.out.ready :=
    vvaq_count.io.watermark && vsreq_count.io.watermark && vlreq_count.io.watermark // vaq.io.enq.ready
  vvaq.io.enq.valid := vvaq_arb.io.out.valid
  vvaq.io.enq.bits := vvaq_arb.io.out.bits

  // vvaq address translation
  vvaq_tlb.io.vvaq <> vvaq.io.deq
  vpaq.io.enq <> vvaq_tlb.io.vpaq
  io.vec_tlb_req <> vvaq_tlb.io.tlb_req
  vvaq_tlb.io.tlb_resp <> io.vec_tlb_resp

  // vvaq counts available space
  vvaq_count.io.qcnt := io.qcnt
  // vvaq frees an entry, when vvaq kicks out an entry to the skid buffer
  vvaq_count.io.inc := vvaq_tlb.io.vvaq.ready && vvaq.io.deq.valid
  // vvaq occupies an entry, when the lane kicks out an entry
  vvaq_count.io.dec := io.lane_vaq_dec

  // VPFVAQ
  val vpfvaq = (new queueSimplePF(16)){ new io_vvaq_bundle() }
  val vpfvaq_tlb = new vuVMU_AddressTLB(late_tlb_miss = true)
  val vpfpaq = (new queueSimplePF(16)){ new io_vpaq_bundle() }

  // vpfvaq hookup
  vpfvaq.io.enq <> io.pf_vvaq

  // vpfvaq address translation
  vpfvaq_tlb.io.vvaq <> vpfvaq.io.deq
  vpfpaq.io.enq <> vpfvaq_tlb.io.vpaq
  io.vec_pftlb_req <> vpfvaq_tlb.io.tlb_req
  vpfvaq_tlb.io.tlb_resp <> io.vec_pftlb_resp

  // VPAQ and VPFPAQ arbiter
  val vpaq_arb = new vuVMU_AddressArbiter(late_nack = true)

  vpaq_arb.io.vpaq <> vpaq.io.deq
  vpaq_arb.io.vpfpaq <> vpfpaq.io.deq
  vpaq_count.io.qcnt2 := vpaq_arb.io.qcnt
  vpaq_arb.io.watermark := vpaq_count.io.watermark2
  memif.io.vaq_deq <> vpaq_arb.io.vaq
  vpaq_arb.io.ack := memif.io.vaq_ack
  vpaq_arb.io.nack := memif.io.vaq_nack

  // vpaq counts occupied space
  vpaq_count.io.qcnt := io.qcnt
  // vpaq occupies an entry, when it accepts an entry from vvaq
  vpaq_count.io.inc := vvaq_tlb.io.ack
  // vpaq frees an entry, when the memory system drains it
  vpaq_count.io.dec := vpaq_arb.io.vpaq_ack


  // vector load data queue and counter
  val vldq = new queue_reorder_qcnt(65,128,9) // needs to make sure log2up(vldq_entries)+1 <= CPU_TAG_BITS-1
  val vldq_skid = SkidBuffer(vldq.io.deq_rtag, late_nack = true)

  vldq.io.deq_data.ready := io.lane_vldq.ready
  io.lane_vldq.valid := vldq.io.watermark // vldq.deq_data.valid
  io.lane_vldq.bits := vldq.io.deq_data.bits

  vldq.io.enq <> memif.io.vldq_enq
  memif.io.vldq_deq_rtag <> vldq_skid.io.deq

  vldq_skid.io.nack := memif.io.vldq_nack

  // vldq has an embedded counter
  // vldq counts occupied space
  // vldq occupies an entry, when it accepts an entry from the memory system
  // vldq frees an entry, when the lane consumes it
  vldq.io.qcnt := io.qcnt

  // vlreq counts available space
  vlreq_count.io.qcnt := io.qcnt
  // vlreq frees an entry, when the vector load data queue consumes an entry
  vlreq_count.io.inc := io.lane_vldq.ready
  // vlreq occupies an entry, when the memory system kicks out an entry
  vlreq_count.io.dec := memif.io.vldq_ack


  // vector store data queue and counter
  val vsdq_arb = new Arbiter(2)( new io_vsdq() )
  val vsdq = new queueSimplePF(16)({ Bits(width = 65) })

  val VSDQARB_LANE = 0
  val VSDQARB_EVAC = 1

  // vsdq arbiter, port 0: lane vsdq
  vsdq_arb.io.in(VSDQARB_LANE) <> io.lane_vsdq
  // vsdq arbiter, port 1: evac
  vsdq_arb.io.in(VSDQARB_EVAC) <> io.evac_vsdq
  // vsdq arbiter, output
  vsdq_arb.io.out.ready := vsdq_count.io.watermark && vpaq_count.io.watermark // vsdq.io.enq.ready
  vsdq.io.enq.valid := vsdq_arb.io.out.valid
  vsdq.io.enq.bits := vsdq_arb.io.out.bits

  val vsdq_skid = SkidBuffer(vsdq.io.deq, late_nack = true)

  memif.io.vsdq_deq <> vsdq_skid.io.deq
  vsdq_skid.io.nack := memif.io.vsdq_nack

  // vsdq counts available space
  vsdq_count.io.qcnt := io.qcnt
  // vsdq frees an entry, when the memory system drains it
  vsdq_count.io.inc := vsdq_skid.io.enq.ready && vsdq.io.deq.valid
  // vsdq occupies an entry, when the lane kicks out an entry
  vsdq_count.io.dec := io.lane_vsdq_dec

  // vsreq counts available space
  vsreq_count.io.qcnt := io.qcnt
  // vsreq frees an entry, when the memory system acks the store
  vsreq_count.io.inc := memif.io.vsdq_ack
  // vsreq occupies an entry, when the lane kicks out an entry
  vsreq_count.io.dec := io.lane_vsdq.valid && vsdq.io.enq.ready
  // there is no stores in flight, when the counter is full
  io.pending_store := !vsreq_count.io.full


  // memif interface
  io.dmem_req <> memif.io.mem_req
  memif.io.mem_resp <> io.dmem_resp
}

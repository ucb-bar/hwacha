package hwacha

import Chisel._
import Node._
import Constants._

class io_vmu_to_xcpt_handler extends Bundle 
{
  val no_pending_load_store = Bool(OUTPUT)
}

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

  val qcntp1 = UFix(SZ_QCNT, INPUT)
  val qcntp2 = UFix(SZ_QCNT, INPUT)

  val pending_store = Bool(OUTPUT)

  val dmem_req = new io_dmem_req()
  val dmem_resp = new io_dmem_resp().flip

  val vec_tlb_req = new ioDTLB_CPU_req()
  val vec_tlb_resp = new ioDTLB_CPU_resp().flip

  val vec_pftlb_req = new ioDTLB_CPU_req()
  val vec_pftlb_resp = new ioDTLB_CPU_resp().flip

  val vmu_to_xcpt  = new io_vmu_to_xcpt_handler()
}

class vuVMU extends Component
{
  val io = new io_vmu()

  val addr = new vuVMU_Address()
  val ldata = new vuVMU_LoadData()
  val sdata = new vuVMU_StoreData()
  val memif = new vuVMU_MemIF()
  val counters = new vuVMU_Counters()

  // address unit
  addr.io.vvaq_pf <> io.pf_vvaq
  addr.io.vvaq_lane <> io.lane_vvaq
  addr.io.vvaq_evac <> io.evac_vvaq

  io.vec_tlb_req <> addr.io.vec_tlb_req
  addr.io.vec_tlb_resp <> io.vec_tlb_resp
  io.vec_pftlb_req <> addr.io.vec_pftlb_req
  addr.io.vec_pftlb_resp <> io.vec_pftlb_resp

  memif.io.vaq <> addr.io.vaq
  addr.io.vaq_ack := memif.io.vaq_ack
  addr.io.vaq_nack := memif.io.vaq_nack

  addr.io.vvaq_lane_dec := io.lane_vaq_dec

  counters.io.vvaq_inc := addr.io.vvaq_inc
  counters.io.vvaq_dec := addr.io.vvaq_dec
  counters.io.vpaq_inc := addr.io.vpaq_inc
  counters.io.vpaq_dec := addr.io.vpaq_dec
  counters.io.vpaq_qcnt2 := addr.io.vpaq_qcnt
  addr.io.vvaq_watermark := counters.io.vvaq_watermark
  addr.io.vpaq_watermark := counters.io.vpaq_watermark2
  addr.io.vsreq_watermark := counters.io.vsreq_watermark
  addr.io.vlreq_watermark := counters.io.vlreq_watermark

  // load data unit
  io.lane_vldq <> ldata.io.vldq_lane

  ldata.io.vldq <> memif.io.vldq
  memif.io.vldq_rtag <> ldata.io.vldq_rtag
  ldata.io.vldq_ack := memif.io.vldq_ack
  ldata.io.vldq_nack := memif.io.vldq_nack

  ldata.io.qcnt := io.qcntp1
  counters.io.vlreq_inc := ldata.io.vlreq_inc
  counters.io.vlreq_dec := ldata.io.vlreq_dec

  // store data unit
  sdata.io.vsdq_lane <> io.lane_vsdq
  sdata.io.vsdq_evac <> io.evac_vsdq

  memif.io.vsdq <> sdata.io.vsdq
  sdata.io.vsdq_ack := memif.io.vsdq_ack
  sdata.io.vsdq_nack := memif.io.vsdq_nack

  sdata.io.vsdq_lane_dec := io.lane_vsdq_dec

  counters.io.vsdq_inc := sdata.io.vsdq_inc
  counters.io.vsdq_dec := sdata.io.vsdq_dec
  counters.io.vsreq_inc := sdata.io.vsreq_inc
  counters.io.vsreq_dec := sdata.io.vsreq_dec
  sdata.io.vpaq_watermark := counters.io.vpaq_watermark
  sdata.io.vsdq_watermark := counters.io.vsdq_watermark

  // counters
  counters.io.qcnt := io.qcntp2
  io.pending_store := counters.io.pending_store

  // memif interface
  io.dmem_req <> memif.io.mem_req
  memif.io.mem_resp <> io.dmem_resp

  io.vmu_to_xcpt.no_pending_load_store := !counters.io.pending_load && !counters.io.pending_store
}

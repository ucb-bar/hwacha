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

  val qcntp1 = UFix(INPUT, SZ_QCNT)
  val qcntp2 = UFix(INPUT, SZ_QCNT)

  val pending_store = Bool(OUTPUT)

  val dmem_req = new io_dmem_req()
  val dmem_resp = new io_dmem_resp().flip

  val vtlb = new io_tlb
  val vpftlb = new io_tlb

  val xcpt_to_vmu = new io_xcpt_handler_to_vmu().flip()
  val evac_to_vmu = new io_evac_to_vmu().flip
  val vmu_to_xcpt  = new io_vmu_to_xcpt_handler()

  val irq = new io_vmu_to_irq_handler()
}

class vuVMU(resetSignal: Bool = null) extends Component(resetSignal)
{
  val io = new io_vmu()

  val addr = new vuVMU_Address()
  val ldata = new vuVMU_LoadData()
  val sdata = new vuVMU_StoreData()
  val counters = new vuVMU_Counters()
  val memif = new vuVMU_MemIF()


  // address unit
  addr.io.vvaq_pf <> io.pf_vvaq
  addr.io.vvaq_lane <> io.lane_vvaq
  addr.io.vvaq_evac <> io.evac_vvaq

  io.vtlb <> addr.io.vtlb
  io.vpftlb <> addr.io.vpftlb

  memif.io.vaq <> addr.io.vaq

  addr.io.irq <> io.irq

  addr.io.vvaq_lane_dec := io.lane_vaq_dec
  // vvaq counts available space
  counters.io.vvaq_dec := addr.io.vvaq_do_enq
  counters.io.vvaq_inc := addr.io.vvaq_do_deq
  // vpaq counts occupied space
  counters.io.vpaq_inc := addr.io.vpaq_do_enq
  counters.io.vpaq_dec := addr.io.vpaq_do_deq
  // vpasdq counts occupied space
  counters.io.vpasdq_inc := addr.io.vpaq_do_enq_vsdq
  counters.io.vpaq_qcnt := addr.io.vpaq_qcnt
  addr.io.vvaq_watermark := counters.io.vvaq_watermark
  addr.io.vpaq_watermark := counters.io.vpaq_watermark
  addr.io.vsreq_watermark := counters.io.vsreq_watermark
  addr.io.vlreq_watermark := counters.io.vlreq_watermark


  // load data unit
  io.lane_vldq <> ldata.io.vldq_lane

  ldata.io.vldq <> memif.io.vldq
  memif.io.vldq_rtag <> ldata.io.vldq_rtag

  ldata.io.qcnt := io.qcntp1
  // vlreq counts available space
  counters.io.vlreq_inc := ldata.io.vldq_rtag_do_deq
  counters.io.vlreq_dec := ldata.io.vldq_rtag_do_enq


  // store data unit
  sdata.io.vsdq_lane <> io.lane_vsdq
  sdata.io.vsdq_evac <> io.evac_vsdq

  memif.io.vsdq <> sdata.io.vsdq

  sdata.io.vsdq_lane_dec := io.lane_vsdq_dec
  // vsdq counts available space
  counters.io.vsdq_inc := sdata.io.vsdq_do_deq
  counters.io.vsdq_dec := sdata.io.vsdq_do_enq
  // vsreq counts available space
  counters.io.vsreq_inc := sdata.io.vsdq_do_deq
  counters.io.vsreq_dec := sdata.io.vsdq_do_enq
  // vpasdq counts occupied space
  counters.io.vpasdq_dec := sdata.io.vsdq_do_enq
  sdata.io.vpasdq_watermark := counters.io.vpasdq_watermark
  sdata.io.vsdq_watermark := counters.io.vsdq_watermark


  // counters
  counters.io.qcnt := io.qcntp2
  io.pending_store := counters.io.pending_store || memif.io.pending_replayq


  // memif interface
  io.dmem_req <> memif.io.mem_req
  memif.io.mem_resp <> io.dmem_resp


  // exception handler
  addr.io.evac_to_vmu <> io.evac_to_vmu
  addr.io.stall := io.xcpt_to_vmu.tlb.stall
  sdata.io.evac_to_vmu <> io.evac_to_vmu

  io.vmu_to_xcpt.no_pending_load_store :=
    !counters.io.pending_load && !counters.io.pending_store &&
    !memif.io.pending_replayq && !addr.io.vpaq_to_xcpt.vpaq_valid
}

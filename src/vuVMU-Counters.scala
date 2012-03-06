package hwacha

import Chisel._
import Node._
import Constants._

class io_vmu_counters extends Bundle
{
  val vvaq_inc = Bool(INPUT)
  val vvaq_dec = Bool(INPUT)
  val vpaq_inc = Bool(INPUT)
  val vpaq_dec = Bool(INPUT)
  val vsdq_inc = Bool(INPUT)
  val vsdq_dec = Bool(INPUT)
  val vlreq_inc = Bool(INPUT)
  val vlreq_dec = Bool(INPUT)
  val vsreq_inc = Bool(INPUT)
  val vsreq_dec = Bool(INPUT)

  val qcnt = UFix(5, INPUT)
  val vvaq_watermark = Bool(OUTPUT)
  val vpaq_watermark = Bool(OUTPUT)
  val vsdq_watermark = Bool(OUTPUT)
  val vlreq_watermark = Bool(OUTPUT)
  val vsreq_watermark = Bool(OUTPUT)

  val vpaq_qcnt2 = UFix(5, INPUT)
  val vpaq_watermark2 = Bool(OUTPUT)

  val pending_store = Bool(OUTPUT)
}

class vuVMU_Counters extends Component
{
  val io = new io_vmu_counters()

  val vvaq_count = new qcnt(16,16)
  val vpaq_count = new qcnt(0,16)
  val vsdq_count = new qcnt(16,16)
  val vsreq_count = new qcnt(31,31) // vector stores in flight
  val vlreq_count = new qcnt(128,128) // vector loads in flight

  // vvaq counts available space
  vvaq_count.io.inc := io.vvaq_inc
  vvaq_count.io.dec := io.vvaq_dec
  vvaq_count.io.qcnt := io.qcnt
  io.vvaq_watermark := vvaq_count.io.watermark

  // vpaq counts occupied space
  vpaq_count.io.inc := io.vpaq_inc
  vpaq_count.io.dec := io.vpaq_dec
  vpaq_count.io.qcnt := io.qcnt
  io.vpaq_watermark := vpaq_count.io.watermark
  vpaq_count.io.qcnt2 := io.vpaq_qcnt2
  io.vpaq_watermark2 := vpaq_count.io.watermark2

  // vsdq counts available space
  vsdq_count.io.inc := io.vsdq_inc
  vsdq_count.io.dec := io.vsdq_dec
  vsdq_count.io.qcnt := io.qcnt
  io.vsdq_watermark := vsdq_count.io.watermark

  // vlreq counts available space
  vlreq_count.io.inc := io.vlreq_inc
  vlreq_count.io.dec := io.vlreq_dec
  vlreq_count.io.qcnt := io.qcnt
  io.vlreq_watermark := vlreq_count.io.watermark

  // vsreq counts available space
  vsreq_count.io.inc := io.vsreq_inc
  vsreq_count.io.dec := io.vsreq_dec
  vsreq_count.io.qcnt := io.qcnt
  io.vsreq_watermark := vsreq_count.io.watermark

  // there is no stores in flight, when the counter is full
  io.pending_store := !vsreq_count.io.full
}

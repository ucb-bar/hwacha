package hwacha

import Chisel._
import Node._
import Constants._

class io_vmu_store_data extends Bundle
{
  val vsdq_lane = new io_vsdq().flip
  val vsdq_evac = new io_vsdq().flip

  val vsdq = new io_vsdq()
  val vsdq_ack = Bool(INPUT)
  val vsdq_nack = Bool(INPUT)

  val vsdq_lane_dec = Bool(INPUT)

  val vsdq_inc = Bool(OUTPUT)
  val vsdq_dec = Bool(OUTPUT)
  val vsreq_inc = Bool(OUTPUT)
  val vsreq_dec = Bool(OUTPUT)
  val vpaq_watermark = Bool(INPUT)
  val vsdq_watermark = Bool(INPUT)

  val evac_to_vmu = new io_evac_to_vmu().flip
  val flush = Bool(INPUT)
}

class vuVMU_StoreData extends Component
{
  val io = new io_vmu_store_data()

  val vsdq_arb = new Arbiter(2)( new io_vsdq() )
  val vsdq = new queueSimplePF(ENTRIES_VSDQ, flushable = true)({ Bits(width = 65) })

  // vsdq arbiter, port 0: lane vsdq
  vsdq_arb.io.in(VSDQARB_LANE) <> io.vsdq_lane
  // vsdq arbiter, port 1: evac
  vsdq_arb.io.in(VSDQARB_EVAC) <> io.vsdq_evac
  // vsdq arbiter, output
  vsdq_arb.io.out.ready :=
    Mux(io.evac_to_vmu.bypass_watermark, vsdq.io.enq.ready,
        io.vsdq_watermark && io.vpaq_watermark)
  vsdq.io.enq.valid := vsdq_arb.io.out.valid
  vsdq.io.enq.bits := vsdq_arb.io.out.bits

  val vsdq_skid = SkidBuffer(vsdq.io.deq, LATE_DMEM_NACK, flushable = true)

  io.vsdq <> vsdq_skid.io.deq
  vsdq_skid.io.nack := io.vsdq_nack

  // vsdq counts available space
  // vsdq frees an entry, when the memory system drains it
  io.vsdq_inc := vsdq_skid.io.enq.ready && vsdq.io.deq.valid
  // vsdq occupies an entry, when the lane kicks out an entry
  io.vsdq_dec := io.vsdq_lane_dec

  // vsreq counts available space
  // vsreq frees an entry, when the memory system acks the store
  io.vsreq_inc := io.vsdq_ack
  // vsreq occupies an entry, when the lane kicks out an entry
  io.vsreq_dec := io.vsdq_lane_dec

  // exception handler
  vsdq.io.flush := io.flush
  vsdq_skid.io.flush := io.flush
}

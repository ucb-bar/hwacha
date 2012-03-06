package hwacha

import Chisel._
import Node._
import Constants._

class io_vmu_load_data extends Bundle
{
  val vldq_lane = new io_vldq()

  val vldq = new ioDecoupled()({ new io_queue_reorder_qcnt_enq_bundle(65, LG_ENTRIES_VLDQ) }).flip
  val vldq_rtag = (new ioDecoupled()){ Bits(width = LG_ENTRIES_VLDQ) }
  val vldq_ack = Bool(INPUT)
  val vldq_nack = Bool(INPUT)

  val qcnt = UFix(SZ_QCNT, INPUT)
  val vlreq_inc = Bool(OUTPUT)
  val vlreq_dec = Bool(OUTPUT)
}

class vuVMU_LoadData extends Component
{
  val io = new io_vmu_load_data()

  // needs to make sure log2up(vldq_entries)+1 <= CPU_TAG_BITS-1
  val vldq = new queue_reorder_qcnt(65,ENTRIES_VLDQ,9)
  val vldq_skid = SkidBuffer(vldq.io.deq_rtag, LATE_DMEM_NACK)

  vldq.io.deq_data.ready := io.vldq_lane.ready
  io.vldq_lane.valid := vldq.io.watermark // vldq.deq_data.valid
  io.vldq_lane.bits := vldq.io.deq_data.bits

  vldq.io.enq <> io.vldq
  io.vldq_rtag <> vldq_skid.io.deq

  vldq_skid.io.nack := io.vldq_nack

  // vldq has an embedded counter
  // vldq counts occupied space
  // vldq occupies an entry, when it accepts an entry from the memory system
  // vldq frees an entry, when the lane consumes it
  vldq.io.qcnt := io.qcnt

  // vlreq counts available space
  // vlreq frees an entry, when the vector load data queue consumes an entry
  io.vlreq_inc := io.vldq_lane.ready
  // vlreq occupies an entry, when the memory system kicks out an entry
  io.vlreq_dec := io.vldq_ack
}

package hwacha

import Chisel._
import Node._
import Constants._

class io_vmu_load_data extends Bundle
{
  val vldq_lane = new io_vldq()

  val vldq = new PipeIO()(new io_queue_reorder_qcnt_enq_bundle(65, LG_ENTRIES_VLDQ)).flip
  val vldq_rtag = new FIFOIO()(Bits(width = LG_ENTRIES_VLDQ))

  val qcnt = UFix(INPUT, SZ_QCNT)
  val vldq_rtag_do_enq = Bool(OUTPUT)
  val vldq_rtag_do_deq = Bool(OUTPUT)
}

class vuVMU_LoadData(resetSignal: Bool = null) extends Component(resetSignal)
{
  val io = new io_vmu_load_data()

  // needs to make sure log2Up(vldq_entries)+1 <= CPU_TAG_BITS-1
  val vldq = new queue_reorder_qcnt(65, ENTRIES_VLDQ, 9)

  vldq.io.deq_data.ready := io.vldq_lane.ready
  io.vldq_lane.valid := vldq.io.watermark // vldq.deq_data.valid
  io.vldq_lane.bits := vldq.io.deq_data.bits

  vldq.io.enq <> io.vldq
  io.vldq_rtag <> vldq.io.deq_rtag

  // vldq has an embedded counter
  // vldq counts occupied space
  // vldq occupies an entry, when it accepts an entry from the memory system
  // vldq frees an entry, when the lane consumes it
  vldq.io.qcnt := io.qcnt

  io.vldq_rtag_do_enq := io.vldq_lane.ready
  io.vldq_rtag_do_deq := io.vldq_rtag.ready && vldq.io.deq_rtag.valid
}

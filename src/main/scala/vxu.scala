package hwacha

import Chisel._
import Node._

class VXU extends HwachaModule
{
  val io = new Bundle {
    val seqop = new SequencerOpIO().flip
    val vmu = new VMUIO
  }

  val seq = Module(new Sequencer)
  val lane = Module(new Lane)
  val dcc = Module(new DecoupledCluster)

  seq.io.seqop <> io.seqop
  seq.io.laneack <> lane.io.ack

  lane.io.op <> seq.io.laneop

  lane.io.brqs <> dcc.io.mem.brqs
  lane.io.bwqs <> dcc.io.mem.bwqs

  io.vmu <> lane.io.vmu
  io.vmu <> dcc.io.mem.vmu

  // TODO: this is here to make sure things get instantiated
  io.vmu.issue.cmd.valid := io.seqop.valid
  io.vmu.issue.cmd.bits.fn := io.seqop.bits.inst
  io.vmu.issue.cmd.bits.mt := io.seqop.bits.inst
  io.vmu.issue.cmd.bits.vlen := io.seqop.bits.inst

  io.vmu.issue.addr.valid := io.seqop.valid
  io.vmu.issue.addr.bits.base := io.seqop.bits.inst
  io.vmu.issue.addr.bits.stride := io.seqop.bits.inst

  // FIXME
  dcc.io.mem.op.valid := Bool(false)
  dcc.io.mem.spred.valid := Bool(false)
  dcc.io.mem.sla.reserve := Bool(false)
  dcc.io.xcpt.prop.vmu.stall := Bool(false)
}

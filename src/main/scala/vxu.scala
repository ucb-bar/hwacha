package hwacha

import Chisel._
import Node._

class VXUIssueOpIO extends DecoupledIO(new IssueOp)

class VXU extends HwachaModule
{
  val io = new Bundle {
    val issue = new VXUIssueOpIO().flip
    val vmu = new VMUIO
  }

  val ibox = Module(new Issue)
  val seq = Module(new Sequencer)
  val lane = Module(new Lane)
  val dcc = Module(new DecoupledCluster)

  ibox.io.issue <> io.issue

  seq.io.op <> ibox.io.seq
  seq.io.ack <> lane.io.ack

  lane.io.op <> seq.io.lane

  lane.io.brqs <> dcc.io.mem.brqs
  lane.io.bwqs <> dcc.io.mem.bwqs

  io.vmu <> lane.io.vmu
  io.vmu <> dcc.io.mem.vmu

  // FIXME
  dcc.io.op <> ibox.io.dcc

  dcc.io.mem.spred.valid := Bool(false)
  dcc.io.mem.sla.reserve := Bool(false)
  dcc.io.xcpt.prop.vmu.stall := Bool(false)
}

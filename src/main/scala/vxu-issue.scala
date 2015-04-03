package hwacha

import Chisel._
import Constants._

class Issue extends HwachaModule
{
  val io = new Bundle {
    val issue = new VXUIssueOpIO().flip
    val seq = new SequencerOpIO
    val dcc = new DecoupledClusterIO
  }

  io.issue.ready := Bool(true)

  io.seq.valid := io.issue.valid
  io.seq.bits.inst := io.issue.bits.inst

  io.dcc.valid := Bool(false)
}

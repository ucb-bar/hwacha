package hwacha

import Chisel._
import Node._

class VXUIssueOpIO extends DecoupledIO(new IssueOp)

class VXU extends HwachaModule
{
  val io = new Bundle {
    val issue = new VXUIssueOpIO().flip
    val cfg = new HwachaConfigIO().flip
    val vmu = new LaneMemIO
    val pending_seq = Bool(OUTPUT)
  }

  val seq = Module(new Sequencer)
  val lane = Module(new Lane)
  val dcc = Module(new DecoupledCluster)

  val enq_dcc = io.issue.bits.enq_dcc()
  val mask_dcc_ready = !enq_dcc || dcc.io.op.ready

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      io.issue.valid,
      seq.io.op.ready, mask_dcc_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.issue.ready := fire(io.issue.valid)
  seq.io.op.valid := fire(seq.io.op.ready)
  dcc.io.op.valid := fire(mask_dcc_ready, enq_dcc)

  seq.io.op.bits := io.issue.bits
  dcc.io.op.bits.vlen := io.issue.bits.vlen
  dcc.io.op.bits.fn := io.issue.bits.fn.vmu()
  dcc.io.op.bits.vd := io.issue.bits.reg.vd

  seq.io.ack <> lane.io.ack

  lane.io.op <> seq.io.lane

  dcc.io.mem.lla <> seq.io.lla
  dcc.io.mem.sla <> seq.io.sla

  lane.io.brqs <> dcc.io.mem.brqs
  lane.io.bwqs <> dcc.io.mem.bwqs

  io.vmu <> seq.io.vmu
  io.vmu <> lane.io.vmu
  io.vmu <> dcc.io.mem.vmu

  dcc.io.cfg <> io.cfg
  io.pending_seq := seq.io.pending

  // FIXME
  dcc.io.mem.spred.valid := Bool(false)
  dcc.io.xcpt.prop.vmu.stall := Bool(false)
}

package hwacha

import Chisel._

class VXUIssueOpIO(implicit p: Parameters) extends DecoupledIO(new IssueOp()(p))

class VXU(implicit p: Parameters) extends HwachaModule()(p) {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val issue = new VXUIssueOpIO().flip
    val vmu = new LaneMemIO
    val mrt = new LaneMRTIO
    val pending = Bool(OUTPUT)
  }

  val seq = Module(new Sequencer)
  val exp = Module(new Expander)
  val lane = Module(new Lane)
  val dcc = Module(new DecoupledCluster)

  seq.io.cfg <> io.cfg
  dcc.io.cfg <> io.cfg

  val enq_dcc = io.issue.bits.active.enq_dcc()
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
  dcc.io.op.bits.active := io.issue.bits.active
  dcc.io.op.bits.fn := io.issue.bits.fn
  dcc.io.op.bits.vd := io.issue.bits.reg.vd

  exp.io.seq <> seq.io.seq
  lane.io.op <> exp.io.lane

  seq.io.ticker <> exp.io.ticker
  seq.io.lack <> lane.io.ack
  seq.io.dack <> dcc.io.ack

  dcc.io.dpla <> seq.io.dpla
  dcc.io.dqla <> seq.io.dqla
  dcc.io.dila <> seq.io.dila
  dcc.io.dfla <> seq.io.dfla
  dcc.io.gpla <> seq.io.gpla
  dcc.io.gqla <> seq.io.gqla
  dcc.io.pla <> seq.io.pla
  dcc.io.lla <> seq.io.lla
  dcc.io.sla <> seq.io.sla

  dcc.io.lpqs <> lane.io.lpqs
  dcc.io.lrqs <> lane.io.lrqs
  dcc.io.bpqs <> lane.io.bpqs
  dcc.io.brqs <> lane.io.brqs
  lane.io.bwqs <> dcc.io.bwqs

  io.vmu <> seq.io.vmu
  io.vmu <> dcc.io.vmu

  io.pending := seq.io.pending

  io.mrt.lreq <> seq.io.lreq
  io.mrt.sreq <> seq.io.sreq
  io.mrt.lret.cnt := dcc.io.lla.cnt
  io.mrt.lret.update := dcc.io.lla.reserve
}

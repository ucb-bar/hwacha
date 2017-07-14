package hwacha

import Chisel._
import freechips.rocketchip.config._

class VXU(implicit p: Parameters) extends VXUModule()(p) {
  val io = new Bundle {
    val id = UInt(INPUT)
    val cfg = new HwachaConfigIO().flip
    val issue = Decoupled(new IssueOp).flip
    val mseq = new MasterSequencerIO().flip
    val mocheck = Vec(nSeq, new MOCheck).asInput
    val red = new ReduceResultIO
    val vmu = new VMUIO
    val mrt = new LaneMRTIO
  }

  val seq = Module(new LaneSequencer)
  seq.suggestName("seqInst")
  val exp = Module(new Expander)
  exp.suggestName("expInst")
  val lane = Module(new Lane)
  lane.suggestName("laneInst")
  val dcc = Module(new DecoupledCluster)
  dcc.suggestName("dccInst")

  seq.io.cfg <> io.cfg
  seq.io.lid := io.id
  exp.io.cfg <> io.cfg
  lane.io.cfg <> io.cfg
  lane.io.id := io.id
  dcc.io.cfg <> io.cfg

  val enq_dcc = io.issue.bits.active.enq_dcc()
  val mask_dcc_ready = !enq_dcc || dcc.io.op.ready

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(io.issue.valid, mask_dcc_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.issue.ready := fire(io.issue.valid)
  seq.io.op.valid := fire(null)
  dcc.io.op.valid := fire(mask_dcc_ready, enq_dcc)

  seq.io.op.bits := io.issue.bits
  dcc.io.op.bits.vlen := io.issue.bits.vlen
  dcc.io.op.bits.active := io.issue.bits.active
  dcc.io.op.bits.fn := io.issue.bits.fn
  dcc.io.op.bits.vd := io.issue.bits.base.vd
  dcc.io.op.bits.vd.id := io.issue.bits.reg.vd.id

  seq.io.master <> io.mseq
  seq.io.mocheck <> io.mocheck

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
  lane.io.bwqs.mem <> dcc.io.bwqs.mem
  lane.io.bwqs.fu <> dcc.io.bwqs.fu
  //lane.io.bwqs <> dcc.io.bwqs
  io.red <> dcc.io.red

  io.vmu <> dcc.io.vmu
  io.vmu.pala <> seq.io.vmu.pala

  io.mrt.lreq <> seq.io.lreq
  io.mrt.sreq <> seq.io.sreq
  io.mrt.areq <> seq.io.areq
  io.mrt.lret.cnt := dcc.io.lla.cnt
  io.mrt.lret.update := dcc.io.lla.reserve
}

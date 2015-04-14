package hwacha

import Chisel._
import Constants._

class DCCAckIO extends HwachaBundle
{
  val vidu = Valid(new VIDUAck)
  val vfdu = Valid(new VFDUAck)
}

class DecoupledCluster extends HwachaModule with LaneParameters with VMUParameters {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val op = Decoupled(new DCCOp).flip
    val ack = new DCCAckIO
    val lrqs = Vec.fill(nLRQOperands){new LRQIO}.flip
    val brqs = Vec.fill(nbanks)(new BRQIO).flip
    val bwqs = new Bundle {
      val mem = Vec.fill(nbanks)(new BWQIO)
      val fu = Vec.fill(nbanks)(new BWQIO)
    }
    val dqla = new CounterLookAheadIO().flip
    val dila = new CounterLookAheadIO().flip
    val dfla = new CounterLookAheadIO().flip
    val lla = new CounterLookAheadIO().flip
    val sla = new BRQLookAheadIO().flip
    val spred = Decoupled(Bits(width = nPredSet)).flip
    val vmu = new LaneMemIO
    val xcpt = new XCPTIO().flip
  }

  val vdu = Module(new VDU)
  val vlu = Module(new VLU)
  val vsu = Module(new VSU)

  val mask_vdu_ready = !io.op.bits.active.enq_vdu() || vdu.io.op.ready
  val mask_vlu_ready = !io.op.bits.active.enq_vlu() || vlu.io.op.ready
  val mask_vsu_ready = !io.op.bits.active.enq_vsu() || vsu.io.op.ready

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      io.op.valid, 
      mask_vdu_ready, mask_vlu_ready, mask_vsu_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }
  
  io.op.ready := fire(io.op.valid)

  vdu.io.cfg <> io.cfg
  vdu.io.op.valid := fire(mask_vdu_ready, io.op.bits.active.enq_vdu())
  vdu.io.op.bits := io.op.bits
  vdu.io.qla <> io.dqla
  vdu.io.ila <> io.dila
  vdu.io.fla <> io.dfla
  vdu.io.lrqs <> io.lrqs
  io.ack <> vdu.io.ack
  io.bwqs.fu <> vdu.io.bwqs

  vlu.io.cfg <> io.cfg
  vlu.io.op.valid := fire(mask_vlu_ready, io.op.bits.active.enq_vlu())
  vlu.io.op.bits := io.op.bits
  vlu.io.la <> io.lla
  vlu.io.vldq <> io.vmu.vldq
  io.bwqs.mem <> vlu.io.bwqs

  vsu.io.op.valid := fire(mask_vsu_ready, io.op.bits.active.enq_vsu())
  vsu.io.op.bits := io.op.bits
  vsu.io.brqs <> io.brqs
  vsu.io.la <> io.sla
  vsu.io.pred <> io.spred
  vsu.io.xcpt <> io.xcpt
  io.vmu.vsdq <> vsu.io.vsdq
}

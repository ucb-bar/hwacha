package hwacha

import Chisel._
import Constants._

class DecoupledClusterIO extends DecoupledIO(new DCCMemOp)

class DecoupledCluster extends HwachaModule with VMUParameters {
  val io = new Bundle {
    val op = new DecoupledClusterIO().flip
    val mem = new Bundle {
      val brqs = Vec.fill(nbanks)(new BRQIO).flip
      val bwqs = Vec.fill(nbanks)(new BWQIO)

      val spred = Decoupled(Bits(width = nPredSet)).flip
      val sla = new BRQLookAheadIO().flip
      val vmu = new LaneMemIO
    }
    val xcpt = new XCPTIO().flip
  }

  val vsu = Module(new VSU)
  vsu.io.op <> io.op // FIXME
  vsu.io.brqs <> io.mem.brqs
  vsu.io.la <> io.mem.sla
  vsu.io.pred <> io.mem.spred
  vsu.io.vsdq <> io.mem.vmu.vsdq

  // FIXME
  io.mem.bwqs.foreach { _.valid := Bool(false) }
  io.mem.vmu.vldq.ready := Bool(false)
}

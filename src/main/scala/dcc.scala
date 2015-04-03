package hwacha

import Chisel._
import Constants._

class DecoupledCluster extends HwachaModule with VMUParameters {
  val io = new Bundle {
    val mem = new Bundle {
      val op = Decoupled(new DCCMemOp).flip
      val brqs = Vec.fill(nbanks)(new BRQIO).flip
      val bwqs = Vec.fill(nbanks)(new BWQIO)

      val spred = Decoupled(Bits(width = nPredSet)).flip
      val sla = new BRQLookAheadIO().flip
      val vmu = new VMUIO
    }
    val xcpt = new XCPTIO().flip
  }

  val vsu = Module(new VSU)
  vsu.io.op <> io.mem.op // FIXME
  vsu.io.brqs <> io.mem.brqs
  vsu.io.la <> io.mem.sla
  vsu.io.pred <> io.mem.spred
  vsu.io.vsdq <> io.mem.vmu.vsdq

  // FIXME
  io.mem.bwqs.foreach { _.valid := Bool(false) }
  io.mem.vmu.vldq.ready := Bool(false)
}

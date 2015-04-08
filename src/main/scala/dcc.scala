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
      val lla = new CounterLookAheadIO().flip
      val vmu = new LaneMemIO
    }
    val xcpt = new XCPTIO().flip
  }

  val vlu = Module(new VLU)
  val vsu = Module(new VSU)

  val cmd = DecodedMemCommand(io.op.bits.fn.cmd)
  io.op.ready := (!cmd.read || vlu.io.op.ready) && (!cmd.write || vsu.io.op.ready)

  vlu.io.op.valid := io.op.valid && cmd.read
  vlu.io.op.bits := io.op.bits
  vlu.io.bwqs <> io.mem.bwqs
  vlu.io.la <> io.mem.lla
  vlu.io.vldq <> io.mem.vmu.vldq

  vsu.io.op.valid := io.op.valid && cmd.write
  vsu.io.op.bits := io.op.bits
  vsu.io.brqs <> io.mem.brqs
  vsu.io.la <> io.mem.sla
  vsu.io.pred <> io.mem.spred
  vsu.io.vsdq <> io.mem.vmu.vsdq
  vsu.io.xcpt <> io.xcpt
}

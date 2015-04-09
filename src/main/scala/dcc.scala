package hwacha

import Chisel._
import Constants._

class DecoupledCluster extends HwachaModule with VMUParameters {
  val io = new Bundle {
    val op = Decoupled(new DCCMemOp).flip
    val cfg = new HwachaConfigIO().flip
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

  val memopq = Module(new Queue(new DCCMemOp, confvmu.ncmdq))
  val vlu = Module(new VLU)
  val vsu = Module(new VSU)

  memopq.io.enq <> io.op

  val memop = memopq.io.deq
  val cmd = DecodedMemCommand(memop.bits.fn.cmd)
  memop.ready := (!cmd.read || vlu.io.op.ready) && (!cmd.write || vsu.io.op.ready)

  vlu.io.op.valid := memop.valid && cmd.read
  vlu.io.op.bits := memop.bits
  vlu.io.bwqs <> io.mem.bwqs
  vlu.io.la <> io.mem.lla
  vlu.io.vldq <> io.mem.vmu.vldq
  vlu.io.cfg <> io.cfg

  vsu.io.op.valid := memop.valid && cmd.write
  vsu.io.op.bits := memop.bits
  vsu.io.brqs <> io.mem.brqs
  vsu.io.la <> io.mem.sla
  vsu.io.pred <> io.mem.spred
  vsu.io.vsdq <> io.mem.vmu.vsdq
  vsu.io.xcpt <> io.xcpt
}

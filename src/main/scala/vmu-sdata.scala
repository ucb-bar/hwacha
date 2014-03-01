package hwacha

import Chisel._
import Constants._

class StoreDataUnit(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val ctrl = new VMUBackendIO().flip
    val lane = new VSDQIO().flip
    val evac = new VSDQIO().flip
    val memif = new VSDQIO
  }

  val arb = Module(new Arbiter(Bits(width = SZ_VMU_DATA), 2))
  val vsdq = Module(new Queue(Bits(width = SZ_VMU_DATA), conf.vmu.nvsdq))

  arb.io.in(0) <> io.lane
  arb.io.in(1) <> io.evac
  vsdq.io.enq <> arb.io.out
  io.memif <> vsdq.io.deq

  io.ctrl.busy := Bool(false)
}

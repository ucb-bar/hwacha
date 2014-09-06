package hwacha

import Chisel._
import Node._
import Constants._

class MRTLoadRetireIO(sz: Int) extends CounterPortIO(sz)

class MRTStoreRetireIO extends Bundle {
  val update = Bool(OUTPUT)
}

// Memory Request Tracker:
// Counts memory operations in transit
class MRT extends HwachaModule
{
  val io = new Bundle {
    val xcpt = new XCPTIO().flip
    val lreq = new LookAheadPortIO(log2Down(nvlreq)+1).flip
    val sreq = new Bundle {
      val vxu = new LookAheadPortIO(log2Down(nvsreq)+1).flip
      val evac = Bool(INPUT)
    }
    val lret = new MRTLoadRetireIO(log2Down(nvlreq)+1).flip
    val sret = new MRTStoreRetireIO().flip
    val pending_memop = Bool(OUTPUT)
  }

  val lcnt = Module(new LookAheadCounter(nvlreq, nvlreq))
  val scnt = Module(new LookAheadCounter(nvsreq, nvsreq))

  lcnt.io.la <> io.lreq
  lcnt.io.inc <> io.lret
  lcnt.io.dec.update := Bool(false)

  scnt.io.la <> io.sreq.vxu
  scnt.io.inc.cnt := UInt(1)
  scnt.io.inc.update := io.sret.update
  scnt.io.dec.cnt := UInt(1)
  scnt.io.dec.update := io.sreq.evac

  io.pending_memop := !lcnt.io.full || !scnt.io.full
  io.xcpt.report.mrt.pending := io.pending_memop
}

package hwacha

import Chisel._
import cde.Parameters

class LaneMRTIO(implicit p: Parameters) extends HwachaBundle()(p) with SeqParameters {
  val lreq = new CounterLookAheadIO
  val sreq = new CounterLookAheadIO
  val lret = new CounterUpdateIO(bLookAhead)
}

class MRTIO(implicit p: Parameters) extends LaneMRTIO()(p) with VMUParameters {
  val sret = new CounterUpdateIO(bSRet)
  val pending = Bool(INPUT)
}

class MemTracker(implicit p: Parameters) extends HwachaModule()(p) {
  val io = new MRTIO().flip

  val lcnt = Module(new LookAheadCounter(nvlreq, nvlreq))
  val scnt = Module(new LookAheadCounter(nvsreq, nvsreq))

  lcnt.io.dec <> io.lreq
  lcnt.io.inc <> io.lret

  scnt.io.dec <> io.sreq
  scnt.io.inc <> io.sret

  io.pending := !lcnt.io.full || !scnt.io.full
}

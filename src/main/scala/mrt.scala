package hwacha

import Chisel._

class LaneMRTIO extends Bundle with SeqParameters {
  val lreq = new CounterLookAheadIO
  val sreq = new CounterLookAheadIO
  val lret = new CounterUpdateIO(bLookAhead)
}

class MRTIO extends LaneMRTIO with VMUParameters {
  val sret = new CounterUpdateIO(sretBits)
  val pending = Bool(INPUT)
}

class MemTracker extends HwachaModule {
  val io = new MRTIO().flip

  val lcnt = Module(new LookAheadCounter(nvlreq, nvlreq))
  val scnt = Module(new LookAheadCounter(nvsreq, nvsreq))

  lcnt.io.dec <> io.lreq
  lcnt.io.inc <> io.lret

  scnt.io.dec <> io.sreq
  scnt.io.inc <> io.sret

  io.pending := !lcnt.io.full || !scnt.io.full
}

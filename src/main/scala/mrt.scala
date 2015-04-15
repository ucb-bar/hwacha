package hwacha

import Chisel._

class LaneMRTIO extends Bundle with LaneParameters {
    val lreq = new CounterLookAheadIO
    val sreq = new CounterLookAheadIO
    val lret = new CounterUpdateIO(lookAheadBits)
}

class MRTIO extends LaneMRTIO with VMUParameters {
    val sret = new CounterUpdateIO(sretBits)
    val pending_memop = Bool(INPUT)
}

class MemTracker extends HwachaModule {
  val io = new MRTIO().flip

  val lcnt = Module(new LookAheadCounter(nvlreq, nvlreq))
  val scnt = Module(new LookAheadCounter(nvsreq, nvsreq))

  lcnt.io.dec <> io.lreq
  lcnt.io.inc <> io.lret

  scnt.io.dec <> io.sreq
  scnt.io.inc <> io.sret

  io.pending_memop := !lcnt.io.full || !scnt.io.full
}

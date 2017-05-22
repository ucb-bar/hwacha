package hwacha

import Chisel._
import config._

class LaneMRTIO(implicit p: Parameters) extends HwachaBundle()(p) with SeqParameters {
  val lreq = new CounterLookAheadIO
  val sreq = new CounterLookAheadIO
  val lret = new CounterUpdateIO(bLookAhead)
}

class MRTIO(implicit p: Parameters) extends LaneMRTIO()(p) with VMUParameters {
  val sret = new CounterUpdateIO(bSRet)
  val pending = new MRTPending().asInput
}

class MRTPending(implicit p: Parameters) extends HwachaBundle()(p) {
  val load = Bool()
  val store = Bool()
  val all = Bool()
}

class MemTracker(nlreq: Int, nsreq: Int)(implicit p: Parameters) extends HwachaModule()(p) {
  val io = new MRTIO().flip

  val lcnt = Module(new LookAheadCounter(nlreq, nlreq))
  lcnt.suggestName("lcntInst")
  val scnt = Module(new LookAheadCounter(nsreq, nsreq))
  scnt.suggestName("scntInst")

  lcnt.io.dec <> io.lreq
  lcnt.io.inc <> io.lret

  scnt.io.dec <> io.sreq
  scnt.io.inc <> io.sret

  io.pending.load := !lcnt.io.full
  io.pending.store := !scnt.io.full
  io.pending.all := io.pending.load || io.pending.store
}

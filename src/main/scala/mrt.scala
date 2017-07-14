package hwacha

import Chisel._
import freechips.rocketchip.config._

class MRTAddrIO(implicit p: Parameters) extends HwachaBundle()(p) with SeqParameters {
  val valid = Bool()
  val bits = UInt(width = log2Up(nSeq))
}

class LaneMRTIO(implicit p: Parameters) extends HwachaBundle()(p) with SeqParameters {
  val lreq = new CounterLookAheadIO
  val sreq = new CounterLookAheadIO
  val lret = new CounterUpdateIO(bLookAhead)
  val areq = new MRTAddrIO
}

class MRTIO(implicit p: Parameters) extends LaneMRTIO()(p) with VMUParameters {
  val sret = new CounterUpdateIO(bSRet)
  val aret = Bool()
  val pending = new MRTPending().asInput
}

class MRTPending(implicit p: Parameters) extends HwachaBundle()(p) {
  val load = Bool()
  val store = Bool()
  val addr = new MRTAddrIO
  val all = Bool()
}

class MemTracker(nlreq: Int, nsreq: Int)(implicit p: Parameters) extends HwachaModule()(p) with SeqParameters with VMUParameters {
  val io = new MRTIO().flip

  val lcnt = Module(new LookAheadCounter(nlreq, nlreq))
  lcnt.suggestName("lcntInst")
  val scnt = Module(new LookAheadCounter(nsreq, nsreq))
  scnt.suggestName("scntInst")
  val addr = Module(new Queue(UInt(width = log2Up(nSeq)), nVMUQ + nVMUIQ))

  lcnt.io.dec <> io.lreq
  lcnt.io.inc <> io.lret

  scnt.io.dec <> io.sreq
  scnt.io.inc <> io.sret

  io.pending.load := !lcnt.io.full
  io.pending.store := !scnt.io.full
  io.pending.all := io.pending.load || io.pending.store

  addr.io.enq := io.areq
  assert(!io.areq.valid || addr.io.enq.ready, "MRT addr queue full!")
  addr.io.deq.ready := io.aret
  io.pending.addr := addr.io.deq
}

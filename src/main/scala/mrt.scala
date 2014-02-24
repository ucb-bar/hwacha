package hwacha

import Chisel._
import Node._
import Constants._

class MRTLoadRequestIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val update = Bool(OUTPUT)
  val cnt = UInt(OUTPUT, log2Down(conf.nvlreq)+1)
}

class MRTStoreRequestIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val update = Bool(OUTPUT)
  val cnt = UInt(OUTPUT, log2Down(conf.nvsreq)+1)
}

class MRTLoadRetireIO extends Bundle
{
  val update = Bool(OUTPUT)
}

class MRTStoreRetireIO extends Bundle
{
  val update = Bool(OUTPUT)
}

class MRT(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val lreq = new MRTLoadRequestIO().flip
    val sreq = new MRTStoreRequestIO().flip
    val lret = new MRTLoadRetireIO().flip
    val sret = new MRTStoreRetireIO().flip
    val pending_memreq = Bool(OUTPUT)
  }

  val lcnt = Module(new LookAheadCounter(conf.nvlreq, conf.nvlreq))
  val scnt = Module(new LookAheadCounter(conf.nvsreq, conf.nvsreq))

  lcnt.io.la.reserve.valid := io.lreq.update
  lcnt.io.la.reserve.cnt := io.lreq.cnt
  lcnt.io.inc := io.lret.update
  lcnt.io.dec := Bool(false)
  lcnt.io.la.available.cnt := UInt(conf.nvlreq)

  scnt.io.la.reserve.valid := io.sreq.update
  scnt.io.la.reserve.cnt := io.sreq.cnt
  scnt.io.inc := io.sret.update
  scnt.io.dec := Bool(false)
  scnt.io.la.available.cnt := UInt(conf.nvsreq)

  io.pending_memreq := !lcnt.io.la.available.check || !scnt.io.la.available.check
}

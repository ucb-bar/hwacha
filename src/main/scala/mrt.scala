package hwacha

import Chisel._
import Node._
import Constants._

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
    val lreq = new LookAheadPortIO(log2Down(conf.nvlreq)+1).flip
    val sreq = new LookAheadPortIO(log2Down(conf.nvsreq)+1).flip
    val lret = new MRTLoadRetireIO().flip
    val sret = new MRTStoreRetireIO().flip
    val pending_memreq = Bool(OUTPUT)
  }

  val lcnt = Module(new LookAheadCounter(conf.nvlreq, conf.nvlreq))
  val scnt = Module(new LookAheadCounter(conf.nvsreq, conf.nvsreq))

  lcnt.io.la <> io.lreq
  lcnt.io.inc := io.lret.update
  lcnt.io.dec := Bool(false)

  scnt.io.la <> io.sreq
  scnt.io.inc := io.sret.update
  scnt.io.dec := Bool(false)

  io.pending_memreq := !lcnt.io.full || !scnt.io.full
}

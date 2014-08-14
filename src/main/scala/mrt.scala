package hwacha

import Chisel._
import Node._
import Constants._

class MRTLoadRetireIO extends HwachaBundle
{
  val update = Bool(OUTPUT)
  val cnt = UInt(OUTPUT, log2Down(nvlreq)+1)
}

class MRTStoreRetireIO extends Bundle
{
  val update = Bool(OUTPUT)
}

// TODO: Eliminate by further genericizing LookAheadCounter
class MRTLoadRetireCounter extends HwachaModule
{
  val sz = log2Down(nvlreq) + 1
  val io = new Bundle {
    val la = new LookAheadPortIO(sz).flip
    val ret = new MRTLoadRetireIO().flip
    val full = Bool(OUTPUT)
  }

  val count = Reg(init = UInt(nvlreq, sz))
  io.full := (count === UInt(nvlreq))

  when (io.la.reserve) {
    count := count - io.la.cnt
    when (io.ret.update) { count := count - io.la.cnt + io.ret.cnt }
  }
  .otherwise {
    when (io.ret.update) { count := count + io.ret.cnt }
  }
  io.la.available := (count >= io.la.cnt)
}

class MRT extends HwachaModule
{
  val io = new Bundle {
    val xcpt = new XCPTIO().flip
    val lreq = new LookAheadPortIO(log2Down(nvlreq)+1).flip
    val sreq = new Bundle {
      val vxu = new LookAheadPortIO(log2Down(nvsreq)+1).flip
      val evac = Bool(INPUT)
    }
    val lret = new MRTLoadRetireIO().flip
    val sret = new MRTStoreRetireIO().flip
    val pending_memop = Bool(OUTPUT)
  }

  val lcnt = Module(new MRTLoadRetireCounter)
  val scnt = Module(new LookAheadCounter(nvsreq, nvsreq))

  lcnt.io.la <> io.lreq
  lcnt.io.ret <> io.lret

  scnt.io.la <> io.sreq.vxu
  scnt.io.inc := io.sret.update
  scnt.io.dec := io.sreq.evac

  io.pending_memop := !lcnt.io.full || !scnt.io.full
  io.xcpt.report.mrt.pending := io.pending_memop
}

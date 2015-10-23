package hwacha

import Chisel._
import cde.Parameters

class Quad(implicit p: Parameters) extends HwachaModule()(p) {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val issue = new Bundle {
      val vxu = new VXUIssueOpIO().flip
      val vmu = Decoupled(new VMUOp).flip
      val scalar = new ScalarMemIO
    }
    val dtlb = new RTLBIO
    val ptlb = new RTLBIO
    val dmem = new uncore.ClientUncachedTileLinkIO
    val pending = new Bundle {
      val seq = Bool(OUTPUT)
      val mem = Bool(OUTPUT)
    }
  }

  val vxu = Module(new VXU)
  val vmu = Module(new VMU)
  val memif = Module(new VMUTileLink)
  val mrt = Module(new MemTracker)

  vxu.io.cfg <> io.cfg
  vxu.io.issue <> io.issue.vxu
  vmu.io.op <> io.issue.vmu
  vmu.io.scalar <> io.issue.scalar

  vmu.io.lane <> vxu.io.vmu
  memif.io.vmu <> vmu.io.memif 

  mrt.io.lreq <> vxu.io.mrt.lreq
  mrt.io.lret <> vxu.io.mrt.lret
  mrt.io.sreq <> vxu.io.mrt.sreq
  mrt.io.sret <> vmu.io.sret

  io.dtlb <> vmu.io.dtlb
  io.ptlb <> vmu.io.ptlb
  io.dmem <> memif.io.dmem
  io.pending.seq := vxu.io.pending
  io.pending.mem := mrt.io.pending

//vmu.io.pf.vaq.valid := Bool(false)
  vmu.io.xcpt.prop.vmu.stall := Bool(false)
  vmu.io.xcpt.prop.vmu.drain := Bool(false)
  vmu.io.xcpt.prop.top.stall := Bool(false)
}

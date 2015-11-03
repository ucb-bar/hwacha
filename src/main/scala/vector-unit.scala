package hwacha

import Chisel._
import cde.Parameters

class VectorUnit(id: Int)(implicit p: Parameters) extends HwachaModule()(p) with SeqParameters {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val issue = new Bundle {
      val vxu = Decoupled(new IssueOp).flip
      val vmu = Decoupled(new VMUOp).flip
    }
    val mseq = new MasterSequencerIO().flip
    val mocheck = Vec.fill(nSeq){new MOCheck}.asInput
    val tlb = new RTLBIO
    val dmem = new uncore.ClientUncachedTileLinkIO
    val pending = new MRTPending().asOutput
  }

  val vxu = Module(new VXU(id))
  val vmu = Module(new VMU(id))
  val memif = Module(new VMUTileLink)
  val mrt = Module(new MemTracker(nvlreq, nvsreq))

  vxu.io.cfg <> io.cfg
  vxu.io.issue <> io.issue.vxu
  vxu.io.mseq <> io.mseq
  vxu.io.mocheck <> io.mocheck
  vmu.io.op <> io.issue.vmu

  vmu.io.cfg <> io.cfg
  vmu.io.lane <> vxu.io.vmu
  memif.io.vmu <> vmu.io.memif 

  mrt.io.lreq <> vxu.io.mrt.lreq
  mrt.io.lret <> vxu.io.mrt.lret
  mrt.io.sreq <> vxu.io.mrt.sreq
  mrt.io.sret <> vmu.io.sret

  io.tlb <> vmu.io.tlb
  io.dmem <> memif.io.dmem
  io.pending <> mrt.io.pending

  vmu.io.xcpt.prop.vmu.stall := Bool(false)
  vmu.io.xcpt.prop.vmu.drain := Bool(false)
  vmu.io.xcpt.prop.top.stall := Bool(false)
}

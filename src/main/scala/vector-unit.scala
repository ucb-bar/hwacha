package hwacha

import Chisel._
import config._
import diplomacy._
import uncore.tilelink2._

class VectorUnit(implicit p: Parameters) extends LazyModule {
  lazy val module = new VectorUnitModule(this)
  val masterNode = TLClientNode(
    TLClientParameters(name = "HwachaVMU", sourceId = IdRange(0, 2*p(HwachaNVLTEntries))))
}

class VectorUnitModule(outer: VectorUnit)(implicit p: Parameters) extends LazyModuleImp(outer) with SeqParameters {
  val io = new Bundle {
    val id = UInt(INPUT)
    val cfg = new HwachaConfigIO().flip
    val issue = new Bundle {
      val vxu = Decoupled(new IssueOp).flip
      val vmu = Decoupled(new VMUOp).flip
    }
    val mseq = new MasterSequencerIO().flip
    val mocheck = Vec(nSeq, new MOCheck).asInput
    val red = new ReduceResultIO
    val tlb = new RTLBIO
    val dmem = outer.masterNode.bundleOut
    val pending = new MRTPending().asOutput

    val complete_memop = Bool(OUTPUT)
  }
  val edge = outer.masterNode.edgesOut.head

  val vxu = Module(new VXU)
  vxu.suggestName("vxuInst")
  val vmu = Module(new VMU)
  vmu.suggestName("vmuInst")
  val memif = Module(new VMUTileLink(edge))
  memif.suggestName("memifInst")
  val mrt = Module(new MemTracker(nvlreq, nvsreq))
  mrt.suggestName("mrtInst")

  vxu.io.id := io.id
  vxu.io.cfg <> io.cfg
  vxu.io.issue <> io.issue.vxu
  vxu.io.mseq <> io.mseq
  vxu.io.mocheck <> io.mocheck
  vmu.io.op <> io.issue.vmu

  vmu.io.id := io.id
  vmu.io.cfg <> io.cfg
  vmu.io.lane <> vxu.io.vmu
  memif.io.vmu <> vmu.io.memif

  io.complete_memop := vmu.io.memif.resp.ready && vmu.io.memif.resp.valid

  mrt.io.lreq <> vxu.io.mrt.lreq
  mrt.io.lret <> vxu.io.mrt.lret
  mrt.io.sreq <> vxu.io.mrt.sreq
  mrt.io.areq <> vxu.io.mrt.areq
  mrt.io.sret <> vmu.io.sret
  mrt.io.aret <> vmu.io.aret

  io.red <> vxu.io.red
  io.tlb <> vmu.io.tlb
  io.dmem.head <> memif.io.dmem
  io.pending <> mrt.io.pending

  vmu.io.xcpt.prop.vmu.stall := Bool(false)
  vmu.io.xcpt.prop.vmu.drain := Bool(false)
  vmu.io.xcpt.prop.top.stall := Bool(false)
}

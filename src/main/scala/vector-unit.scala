package hwacha

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket.{TLBPTWIO, TLBConfig}

class VectorUnit(implicit p: Parameters) extends LazyModule {
  lazy val module = new VectorUnitModule(this)
  val masterNode = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(name = "HwachaVMU", sourceId = IdRange(0, p(HwachaNVMTEntries)))))))
}

class VectorUnitModule(outer: VectorUnit)(implicit p: Parameters) extends LazyModuleImp(outer) with SeqParameters {
  val io = IO(new Bundle {
    val id = Input(UInt())
    val cfg = Flipped(new HwachaConfigIO())
    val issue = new Bundle {
      val vxu = Flipped(Decoupled(new IssueOp))
      val vmu = Flipped(Decoupled(new VMUOp))
    }
    val mseq = Flipped(new MasterSequencerIO())
    val mocheck = Input(Vec(nSeq, new MOCheck))
    val red = new ReduceResultIO
    val ptw = new TLBPTWIO
    val pending = Output(new MRTPending())

    val complete_memop = Output(Bool())
  })
  val (dmem, edge) = outer.masterNode.out.head

  val vxu = Module(new VXU)
  vxu.suggestName("vxuInst")
  val vmu = Module(new VMU)
  vmu.suggestName("vmuInst")
  val memif = Module(new VMUTileLink(edge))
  memif.suggestName("memifInst")
  val mrt = Module(new MemTracker(nvlreq, nvsreq))
  mrt.suggestName("mrtInst")
  val dtlb = Module(new freechips.rocketchip.rocket.TLB(instruction = false, lgMaxSize = log2Ceil(regBytes), TLBConfig(nSets=1, nWays=ndtlb))(edge, p))

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

  dtlb.io.req <> vmu.io.tlb.req
  vmu.io.tlb.resp <> dtlb.io.resp
  io.ptw <> dtlb.io.ptw
  dtlb.io.ptw.status := vmu.io.tlb.status
  dtlb.io.sfence.valid := false.B

  io.red <> vxu.io.red
  dmem <> memif.io.dmem
  io.pending <> mrt.io.pending

  vmu.io.xcpt.prop.vmu.stall := false.B
  vmu.io.xcpt.prop.vmu.drain := false.B
  vmu.io.xcpt.prop.top.stall := false.B
}

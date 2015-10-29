package hwacha

import Chisel._
import cde.{Parameters, Field}

case object HwachaNVVAQEntries extends Field[Int]
case object HwachaNVPAQEntries extends Field[Int]
case object HwachaNVPFQEntries extends Field[Int]
case object HwachaNVSDQEntries extends Field[Int]
case object HwachaNVLDQEntries extends Field[Int]
case object HwachaNVLTEntries extends Field[Int]

trait MemParameters extends UsesHwachaParameters
  with uncore.HasTileLinkParameters {
  val bVAddr = p(junctions.VAddrBits)
  val bPAddr = p(junctions.PAddrBits)
  val bVAddrExtended = bVAddr + (if (bVAddr < regLen) 1 else 0)

  val bVPN = p(junctions.VPNBits)
  val bPPN = p(junctions.PPNBits)
  val bPgIdx = p(junctions.PgIdxBits)
  val pgSize = 1 << bPgIdx

  require((tlDataBits & (regLen - 1)) == 0)
}

trait VMUParameters extends MemParameters {
  val nVMUQ = 2
  val nVVAQ = p(HwachaNVVAQEntries)
  val nVPAQ = p(HwachaNVPAQEntries)
  val nVPFQ = p(HwachaNVPFQEntries)
  val nVSDQ = p(HwachaNVSDQEntries)
  val nVLDQ = p(HwachaNVLDQEntries)
  val nVMUPredQ = 4

  val nVLT = p(HwachaNVLTEntries)
  val bVMUTag = log2Up(nVLT)
  require(tlClientXactIdBits >= bVMUTag)

  /* Maximum of two ongoing operations in the VMU */
  val maxVCU = maxVLen << 1
  val bVCU = bVLen + 1

  val bSRet = log2Down(tlDataBytes) + 1
}

class VMUIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val vaq = new VVAQIO
  val vsdq = new VSDQIO
  val vldq = new VLDQIO().flip

  val pred = Decoupled(Bits(width = nPredSet))
  val pala = new CounterLookAheadIO
}

class VMUDecodedOp(implicit p: Parameters) extends VMUOp()(p) {
  val mode = new Bundle {
    val unit = Bool()
    val indexed = Bool()
  }
  val cmd = new DecodedMemCommand
  val mt = new DecodedMemType
}

object VMUDecodedOp extends HwachaConstants {
  def apply(op: VMUOp)(implicit p: Parameters): VMUDecodedOp = {
    val dec = new VMUDecodedOp
    dec.fn := op.fn
    dec.vlen := op.vlen
    dec.base := op.base
    dec.stride := op.stride

    dec.mode.unit := vmu_unit(op.fn.mode)
    dec.mode.indexed := vmu_indexed(op.fn.mode)

    dec.cmd := DecodedMemCommand(op.fn.cmd)
    dec.mt := DecodedMemType(op.fn.mt)
    dec
  }
}

class VMUIssueIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val op = Decoupled(new VMUDecodedOp()(p)).flip
}

class IBox(implicit p: Parameters) extends VMUModule()(p) {
  val io = new Bundle {
    val op = Decoupled(new VMUOp).flip
    val abox = Vec.fill(3)(Decoupled(new VMUDecodedOp))
    val pbox = Vec.fill(2)(Decoupled(new VMUDecodedOp))
    val mbar = Decoupled(new VMUDecodedOp)
  }

  val opq = Module(new Queue(io.op.bits, nVMUQ))
  opq.io.enq <> io.op
  opq.io.deq.ready := Bool(false)

  val op = VMUDecodedOp(opq.io.deq.bits)
  private val issue = io.abox ++ io.pbox :+ io.mbar

  val mask = Reg(init = Bits(0, issue.size))

  issue.zipWithIndex.foreach { case (box, i) =>
    val _mask = mask(i)
    box.bits := op
    box.valid := opq.io.deq.valid && !_mask
    val fire = opq.io.deq.valid && box.ready
    _mask := _mask || fire
  }

  when (mask.andR) {
    opq.io.deq.ready := Bool(true)
    mask := Bits(0)
  }
}


class VMU(resetSignal: Bool = null)(implicit p: Parameters)
  extends VMUModule(_reset = resetSignal)(p) {
  val io = new Bundle {
    val op = Decoupled(new VMUOp).flip
    val lane = new VMUIO().flip
    val tlb = new RTLBIO
    val memif = new VMUMemIO

    val sret = new CounterUpdateIO(bSRet)
    val irq = new IRQIO
    val xcpt = new XCPTIO().flip
  }

  val ibox = Module(new IBox)
  val pbox = Module(new PBox)
  val abox = Module(new ABox)
  val tbox = Module(new TBox(1))
  val sbox = Module(new SBox)
  val lbox = Module(new LBox)
  val mbox = Module(new MBox)
  val mbar = Module(new MBar)

  ibox.io.op <> io.op

  pbox.io.op <> ibox.io.pbox
  pbox.io.pred.bits.pred := io.lane.pred.bits
  pbox.io.pred <> io.lane.pred

  abox.io.op <> ibox.io.abox
  abox.io.mask <> pbox.io.mask
  abox.io.lane <> io.lane.vaq
  abox.io.xcpt <> io.xcpt
  abox.io.la <> io.lane.pala

  tbox.io.inner(0) <> abox.io.tlb
  io.tlb <> tbox.io.outer

  io.irq <> tbox.io.irq

  sbox.io.ctrl <> abox.io.store
  sbox.io.lane <> io.lane.vsdq
  lbox.io.lane <> io.lane.vldq

  mbox.io.inner.abox <> abox.io.mem
  mbox.io.inner.sbox <> sbox.io.mem
  mbox.io.inner.lbox <> lbox.io.mem
  io.sret <> mbox.io.sret

  mbar.io.op <> ibox.io.mbar
  mbar.io.inner <> mbox.io.outer
  io.memif <> mbar.io.outer
}

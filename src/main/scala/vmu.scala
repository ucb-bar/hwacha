package hwacha

import Chisel._
import Constants._
import uncore._

trait VMUParameters extends UsesHwachaParameters {
  val tagBits = log2Up(params(HwachaNVectorLoadMetaBufferEntries))

  val vaddrBits = params(VAddrBits)
  val paddrBits = params(PAddrBits)
  val pgIdxBits = params(PgIdxBits)
  val pgSzBytes = 1 << pgIdxBits
  val vpnBits = params(VPNBits)
  val ppnBits = params(PPNBits)
  val maxAddrBits = math.max(vaddrBits, paddrBits) + 1

  val tlBlockAddrBits = params(TLBlockAddrBits)
  val tlBeatAddrBits = log2Up(params(TLDataBeats))
  val tlDataBits = params(TLDataBits)
  val tlDataBytes = tlDataBits >> 3
  val tlByteAddrBits = log2Up(tlDataBytes)

  val sretBits = log2Down(tlDataBytes) + 1

  val tlDataHalves = tlDataBits >> 4
  val tlDataWords = tlDataBits >> 5
  val tlDataDoubles = tlDataBits >> 6
  require((tlDataBits & (SZ_D-1)) == 0)

  val nPredSet = tlDataBits // TODO: rename
}

class LaneMemIO extends Bundle {
  val vaq = new VVAQIO
  val vsdq = new VSDQIO
  val vldq = new VLDQIO().flip
  val la = new VMULookAheadIO
}

class PrefetchMemIO extends Bundle {
  val vaq = new VVAPFQIO
}

class VMUDecodedOp extends VMUOpBase {
  val mode = new Bundle {
    val indexed = Bool()
    val scalar = Bool()
  }
  val cmd = new DecodedMemCommand
  val mt = new DecodedMemType

  val aux = new Bundle {
    val v = new VMUAuxVector
    val s = new VMUAuxScalar
  }

  val unit = Bool()
}

object VMUDecodedOp {
  def apply(src: VMUOp): VMUDecodedOp = {
    val op = new VMUDecodedOp
    op.fn := src.fn
    op.vlen := src.vlen
    op.base := src.base

    op.mode.indexed := is_indexed(op.fn.mode)
    op.mode.scalar := is_scalar(op.fn.mode)
    op.cmd := DecodedMemCommand(op.fn.cmd)
    op.mt := DecodedMemType(op.fn.mt)

    op.aux.v := src.aux.vector()
    op.aux.s := src.aux.scalar()

    op.unit :=
      Seq(op.mt.b, op.mt.h, op.mt.w, op.mt.d).zipWithIndex.map(i =>
        i._1 && (op.aux.v.stride === UInt(1 << i._2))).reduce(_||_)
    op
  }
}

class VMUIssueOpIO extends Bundle {
  val op = new VMUDecodedOp().asOutput
  val fire = Bool(OUTPUT)
  val busy = Bool(INPUT)
}

class IBox extends VMUModule {
  val io = new Bundle {
    val op = Decoupled(new VMUOp).flip
    val abox = new VMUIssueOpIO
    val sbox = new VMUIssueOpIO
    val smu = new VMUIssueOpIO
    val quiescent = Bool(INPUT)
  }

  val issueq = Module(new Queue(new VMUOp, confvmu.ncmdq))
  issueq.io.enq <> io.op

  val op = VMUDecodedOp(issueq.io.deq.bits)
  private val backends = Seq(io.abox, io.sbox, io.smu)
  for (box <- backends) {
    box.op := op
    box.fire := Bool(false)
  }

  issueq.io.deq.ready := Bool(false)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      when (issueq.io.deq.valid) {
        state := s_busy
        when (op.mode.scalar) {
          io.smu.fire := Bool(true)
        } .otherwise {
          io.abox.fire := Bool(true)
          io.sbox.fire := op.cmd.store || op.cmd.amo
        }
      }
    }

    is (s_busy) {
      when (!backends.map(_.busy).reduce(_||_) && io.quiescent) {
        state := s_idle
        issueq.io.deq.ready := Bool(true)
      }
    }
  }
}

class VMU(resetSignal: Bool = null) extends VMUModule(_reset = resetSignal) {
  val io = new Bundle {
    val op = Decoupled(new VMUOp).flip

    val lane = new LaneMemIO().flip
    val scalar = new ScalarMemIO
    val pf = new PrefetchMemIO().flip

    val memif = new VMUMemIO
    val sret = Valid(UInt(width = sretBits))

    val dtlb = new TLBIO
    val ptlb = new TLBIO

    val irq = new IRQIO
    val xcpt = new XCPTIO().flip
  }

  val ibox = Module(new IBox)
  val abox = Module(new ABox)
  val tbox = Module(new TBox(2))
  val sbox = Module(new SBox)
  val lbox = Module(new LBox)
  val mbox = Module(new MBox)

  val smu = Module(new SMU)
  val arb = Module(new VMUMemArb(2))
  val mon = Module(new VMUMemMonitor)

  ibox.io.op <> io.op
  val sel = ibox.io.smu.op.mode.scalar

  abox.io.issue <> ibox.io.abox
  abox.io.xcpt <> io.xcpt
  abox.io.lane <> io.lane.vaq
  abox.io.pf <> io.pf.vaq
  abox.io.la <> io.lane.la

  tbox.io.inner(0) <> abox.io.tlb.lane
  tbox.io.inner(1) <> smu.io.tlb
  tbox.io.sel := sel

  io.dtlb <> tbox.io.outer
  io.ptlb <> abox.io.tlb.pf

  sbox.io.issue <> ibox.io.sbox
  sbox.io.lane <> io.lane.vsdq

  lbox.io.lane <> io.lane.vldq

  mbox.io.inner.abox <> abox.io.mbox
  mbox.io.inner.sbox <> sbox.io.mbox
  mbox.io.inner.lbox <> lbox.io.mbox
  mbox.io.inner.sret <> io.sret

  smu.io.issue <> ibox.io.smu
  smu.io.xcpt <> io.xcpt
  smu.io.scalar <> io.scalar

  arb.io.inner(0) <> mbox.io.outer
  arb.io.inner(1) <> smu.io.outer
  arb.io.sel := sel
  io.memif <> arb.io.outer

  mon.io.req := io.memif.req.fire()
  mon.io.resp := io.memif.resp.fire()
  ibox.io.quiescent := mon.io.quiescent

  val irqs = Seq(abox.io.irq, smu.io.irq)
  io.irq.vmu.ma_ld := irqs.map(_.vmu.ma_ld).reduce(_ || _)
  io.irq.vmu.ma_st := irqs.map(_.vmu.ma_st).reduce(_ || _)
  io.irq.vmu.faulted_ld := irqs.map(_.vmu.faulted_ld).reduce(_ || _)
  io.irq.vmu.faulted_st := irqs.map(_.vmu.faulted_st).reduce(_ || _)
  io.irq.vmu.aux := Vec(irqs.map(_.vmu.aux))(sel)
}

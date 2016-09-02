package hwacha

import Chisel._
import cde.{Parameters, Field}

case object HwachaNVVAQEntries extends Field[Int]
case object HwachaNVPAQEntries extends Field[Int]
case object HwachaNVSDQEntries extends Field[Int]
case object HwachaNVLDQEntries extends Field[Int]
case object HwachaNVLTEntries extends Field[Int]

trait MemParameters extends UsesHwachaParameters
  with uncore.tilelink.HasTileLinkParameters {
  val bVAddr = vaddrBits
  val bPAddr = paddrBits
  val bVAddrExtended = bVAddr + (if (bVAddr < regLen) 1 else 0)

  val bVPN = vpnBits
  val bPPN = ppnBits
  val bPgIdx = pgIdxBits
  val pgSize = 1 << bPgIdx

  require((tlDataBits & (regLen - 1)) == 0)
}

trait VMUParameters extends MemParameters {
  val nVMUQ = 2
  val nVVAQ = p(HwachaNVVAQEntries)
  val nVPAQ = p(HwachaNVPAQEntries)
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

  val pred = Decoupled(new PredEntry)
  val pala = new CounterLookAheadIO
  val vlu = new VLUSelectIO
}

class VMUDecodedOp(implicit p: Parameters) extends VMUOp()(p) with VMUMetaIndex {
  val mode = new Bundle {
    val unit = Bool()
    val indexed = Bool()
  }
  val cmd = new DecodedMemCommand
  val mt = new DecodedMemType

  val first = Bool()
}

object VMUDecodedOp extends HwachaConstants {
  def apply(op: VMUOp)(implicit p: Parameters): VMUDecodedOp = {
    val dec = Wire(new VMUDecodedOp)
    dec.fn := op.fn
    dec.vlen := op.vlen
    dec.base := op.base
    dec.stride := op.stride
    dec.status := op.status

    dec.mode.unit := vmu_unit(op.fn.mode)
    dec.mode.indexed := vmu_indexed(op.fn.mode)

    dec.cmd := DecodedMemCommand(op.fn.cmd)
    dec.mt := DecodedMemType(op.fn.mt)

    dec.eidx := UInt(0)
    dec.first := Bool(true)
    dec
  }
}

class VMUIssueIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val op = Decoupled(new VMUDecodedOp).flip
}
class IBoxIO(implicit p: Parameters) extends VMUIssueIO()(p) {
  val issue = Vec(4, Decoupled(new VMUDecodedOp))

  def span(sink: DecoupledIO[VMUDecodedOp]*) = {
    val src = Wire(Decoupled(new VMUDecodedOp))
    val rvs = (src.ready, src.valid) +: sink.map(x => (x.valid, x.ready))
    rvs.foreach { case (x, y) =>
      x := rvs.map(_._2).filter(_ ne y).reduce(_ && _)
    }
    sink.foreach { case x => x.bits := src.bits }
    src
  }
}

class IBox(id: Int)(implicit p: Parameters) extends VMUModule()(p) {
  val io = new Bundle {
    val op = Decoupled(new VMUOp).flip
    val cfg = new HwachaConfigIO().flip
    val agu = new AGUIO
    val abox = Vec(3, Decoupled(new VMUDecodedOp))
    val pbox = Vec(2, Decoupled(new VMUDecodedOp))
  }

  val opq = Module(new Queue(io.op.bits, nVMUQ))
  opq.io.enq <> io.op
  val op = VMUDecodedOp(opq.io.deq.bits)

  val agent = if (nLanes > 1) {
      val _agent = Module(new IBoxML(id))
      _agent.io.cfg <> io.cfg
      io.agu <> _agent.io.agu
      _agent
  } else Module(new IBoxSL)

  agent.io.op.bits := op
  agent.io.op.valid := opq.io.deq.valid
  opq.io.deq.ready := agent.io.op.ready

  val issue = Seq(io.abox(0), io.pbox(0),
    agent.io.span(io.abox(1), io.pbox(1)), io.abox(2))
  issue zip agent.io.issue map {case(s,d) => s <> d}
}

class IBoxSL(implicit p: Parameters) extends VMUModule()(p) {
  val io = new IBoxIO

  val mask = Reg(init = Vec.fill(io.issue.size){Bool(false)})
  io.issue.zipWithIndex.map { case (box, i) =>
    val _mask = mask(i)
    box.bits := io.op.bits
    box.valid := io.op.valid && !_mask
    val fire = io.op.valid && box.ready
    _mask := _mask || fire
  }

  io.op.ready := mask.asUInt.andR
  when (io.op.ready) {
    mask.map(_ := Bool(false))
  }
}

class IBoxML(id: Int)(implicit p: Parameters) extends VMUModule()(p) {
  val io = new IBoxIO {
    val cfg = new HwachaConfigIO().flip
    val agu = new AGUIO
  }

  val op = Reg(new VMUDecodedOp)
  val indexed = io.op.bits.mode.indexed

  val (lut, setup) = if (id != 0) {
    val _lut = (0 to log2Up(id)).filter(i => (id & (1 << i)) != 0)
    val _setup = Reg(UInt(width = log2Up(_lut.size)))
    (Vec(_lut.map(UInt(_))), _setup)
  } else (null, null)

  val shift = Wire(UInt(width = io.agu.in.bits.shift.getWidth))
  shift := UInt(bLanes)

  io.agu.in.valid := Bool(false)
  io.agu.in.bits.base := op.base
  io.agu.in.bits.offset := Cat(io.op.bits.stride, UInt(0, bStrip))
  io.agu.in.bits.shift := io.cfg.lstride + shift

  val ecnt_max = io.cfg.lstrip
  val eidx_next = op.eidx + ecnt_max
  val vlen_next = op.vlen.zext - ecnt_max.zext
  val vlen_end = (vlen_next <= SInt(0))
  val ecnt = Mux(vlen_end, op.vlen(bfLStrip-1, 0), ecnt_max)

  val enq = io.span(io.issue.map { case deq =>
    val q = Module(new Queue(new VMUDecodedOp, 2))
    deq <> q.io.deq
    q.io.enq
  }:_*)
  enq.bits := io.op.bits
  enq.bits.vlen := ecnt
  enq.bits.base := op.base
  enq.bits.eidx := op.eidx
  enq.bits.first := op.first
  enq.bits.status := op.status

  io.op.ready := Bool(false)
  enq.valid := Bool(false)

  val s_idle :: s_busy :: s_setup :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      when (io.op.valid) {
        op := io.op.bits
        if (id != 0) {
          setup := UInt(lut.size - 1)
          state := Mux(indexed, s_busy, s_setup)
        } else {
          state := s_busy
        }
      }
    }

    is (s_busy) {
      io.agu.in.valid := !indexed
      enq.valid := indexed || io.agu.out.valid

      when (enq.fire()) {
        unless (indexed) {
          op.base := io.agu.out.bits.addr
        }
        op.vlen := vlen_next.asUInt()
        op.eidx := eidx_next
        op.first := Bool(false)
        when (vlen_end) {
          state := s_idle
          io.op.ready := Bool(true)
        }
      }
    }

    is (s_setup) {
      if (id != 0) {
        shift := lut(setup)
        io.agu.in.valid := Bool(true)
        when (io.agu.out.valid) {
          op.base := io.agu.out.bits.addr
          setup := setup - UInt(1)
          when (setup === UInt(0)) {
            state := s_busy
          }
        }
      }
    }
  }
}

class VMU(id: Int, resetSignal: Bool = null)(implicit p: Parameters)
  extends VMUModule(_reset = resetSignal)(p) {
  val io = new Bundle {
    val op = Decoupled(new VMUOp).flip
    val cfg = new HwachaConfigIO().flip
    val lane = new VMUIO().flip
    val tlb = new RTLBIO
    val memif = new VMUMemIO

    val sret = new CounterUpdateIO(bSRet)
    val irq = new IRQIO
    val xcpt = new XCPTIO().flip
  }

  private val confml = (nLanes > 1)
  val ibox = Module(new IBox(id))
  val pbox = Module(new PBox)
  val abox = Module(new ABox)
  val tbox = Module(new TBox(1))
  val sbox = Module(new SBox)
  val lbox = Module(new LBox)
  val mbox = Module(new MBox)
  val agu = Module(new AGU(if (confml) 2 else 1))

  ibox.io.op <> io.op
  ibox.io.cfg <> io.cfg
  if (confml) agu.io.ports(1) <> ibox.io.agu

  pbox.io.op <> ibox.io.pbox
  pbox.io.pred <> io.lane.pred

  abox.io.op <> ibox.io.abox
  abox.io.mask <> pbox.io.mask
  abox.io.lane <> io.lane.vaq
  abox.io.xcpt <> io.xcpt
  abox.io.la <> io.lane.pala
  abox.io.load <> io.lane.vlu
  agu.io.ports(0) <> abox.io.agu

  tbox.io.inner(0) <> abox.io.tlb
  io.tlb <> tbox.io.outer

  io.irq <> tbox.io.irq

  sbox.io.ctrl <> abox.io.store
  sbox.io.lane <> io.lane.vsdq
  io.lane.vldq <> lbox.io.lane

  mbox.io.inner.abox <> abox.io.mem
  mbox.io.inner.sbox <> sbox.io.mem
  lbox.io.mem <> mbox.io.inner.lbox
  io.sret <> mbox.io.sret

  io.memif <> mbox.io.outer
}

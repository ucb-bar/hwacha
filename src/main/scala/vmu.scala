package hwacha

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.tilelink.TLEdgeOut
import freechips.rocketchip.tile.TileVisibilityNodeKey

case object HwachaNVVAQEntries extends Field[Int]
case object HwachaNVPAQEntries extends Field[Int]
case object HwachaNVSDQEntries extends Field[Int]
case object HwachaNVLDQEntries extends Field[Int]
case object HwachaNVMTEntries extends Field[Int]

trait MemParameters extends UsesHwachaParameters
  with freechips.rocketchip.tile.HasCoreParameters {
  val bVAddr = vaddrBits
  val bPAddr = paddrBits
  val bVAddrExtended = bVAddr + (if (bVAddr < regLen) 1 else 0)

  val bVPN = vpnBits
  val bPPN = ppnBits
  val bPgIdx = pgIdxBits
  val pgSize = 1 << bPgIdx

  val tlDataBytes = p(TileVisibilityNodeKey).edges.out.head.manager.beatBytes
  val tlByteAddrBits = log2Up(tlDataBytes)
  val tlDataBits = tlDataBytes*8


  require(((tlDataBytes*8) & (regLen - 1)) == 0)
}

trait VMUParameters extends MemParameters {
  val nVMUQ = 2
  val nVMUIQ = 2
  val nVVAQ = p(HwachaNVVAQEntries)
  val nVPAQ = p(HwachaNVPAQEntries)
  val nVSDQ = p(HwachaNVSDQEntries)
  val nVLDQ = p(HwachaNVLDQEntries)
  val nVMUPredQ = 4

  val nVMT = p(HwachaNVMTEntries)
  val bVMUTag = log2Up(nVMT)

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
    dec.suggestName("decWire")
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
  val aret = Bool()

  def span(sink: DecoupledIO[VMUDecodedOp]*) = {
    val src = Wire(Decoupled(new VMUDecodedOp)).suggestName("srcWire")
    val rvs = (src.ready, src.valid) +: sink.map(x => (x.valid, x.ready))
    rvs.foreach { case (x, y) =>
    x := rvs.map(_._2).filter(_ ne y).reduce((a,b) => (a && b).suggestName("andWire"))
      x.suggestName("andRvs")
    }
    sink.foreach { case x => x.bits := src.bits }
    src
  }
}

class IBox(implicit p: Parameters) extends VMUModule()(p) {
  val io = new Bundle {
    val id = UInt(INPUT)
    val op = Decoupled(new VMUOp).flip
    val cfg = new HwachaConfigIO().flip
    val agu = new AGUIO
    val abox = Vec(3, Decoupled(new VMUDecodedOp))
    val pbox = Vec(2, Decoupled(new VMUDecodedOp))
    val aret = Bool()
  }

  val opq = Module(new Queue(io.op.bits, nVMUQ))
  opq.suggestName("opqInst")
  opq.io.enq <> io.op
  val op = VMUDecodedOp(opq.io.deq.bits)

  val agent = if (nLanes > 1) {
      val _agent = Module(new IBoxML)
      _agent.suggestName("_agentInst")
      _agent.io.id := io.id
      _agent.io.cfg <> io.cfg
      io.agu <> _agent.io.agu
      _agent.io
  } else (Module(new IBoxSL)).io

  agent.op.bits := op
  agent.op.valid := opq.io.deq.valid
  opq.io.deq.ready := agent.op.ready
  io.aret := agent.aret

  val issue = Seq(io.abox(0), io.pbox(0),
    agent.span(io.abox(1), io.pbox(1)), io.abox(2))
  issue zip agent.issue map {case(s,d) => s <> d}
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
    io.aret := true.B
  }
}

class IBoxML(implicit p: Parameters) extends VMUModule()(p) {
  val io = new IBoxIO {
    val id = UInt(INPUT, width = bLanes)
    val cfg = new HwachaConfigIO().flip
    val agu = new AGUIO
  }

  val op = Reg(new VMUDecodedOp)
  val indexed = io.op.bits.mode.indexed

  val idMask = Reg(UInt(width = bLanes))

  // Each bit position of id is put into priority mux with priority of its bit position
  // whether its requested in the queue is dependent on the value at that bit position
  // we keep track of which was the most recent position issued and mask any higher bit positions off
  val pMux = PriorityMux(
    Reverse(io.id) & Reverse(idMask),
    ((bLanes - 1) to 0 by -1).map(UInt(_)))


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


  val qcntr = Reg(init = 0.U((log2Up(nVMUIQ + 2)).W))
  val qcnts = Wire(Vec(io.issue.size, UInt(width = log2Up(nVMUIQ + 1))))
  val aret_pending = Reg(init = Bool(false))
  val enq = io.span(io.issue.zipWithIndex.map { case (deq, i) =>
    val q = Module(new Queue(new VMUDecodedOp, nVMUIQ))
    qcnts(i) := q.io.count
    q.suggestName("qInst")
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
  io.aret := Bool(false)
  enq.valid := Bool(false)

  when(io.issue(3).fire) {
    qcntr := Mux(qcntr === 0.U, 0.U, (qcntr.zext - 1.S).asUInt)
    io.aret := qcntr === 1.U || aret_pending
    aret_pending := Bool(false)
  }

  val s_idle :: s_busy :: s_setup :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      when (io.op.valid) {
        op := io.op.bits
        when(io.id =/= UInt(0)) {
          idMask := ~UInt(0, bLanes)
          state := Mux(indexed, s_busy, s_setup)
        } .otherwise {
          state := s_busy
        }
      }
    }

    is (s_busy) {
      io.agu.in.valid := !indexed
      enq.valid := !aret_pending && (indexed || io.agu.out.valid)

      when (enq.fire) {
        when (!indexed) {
          op.base := io.agu.out.bits.addr
        }
        op.vlen := vlen_next.asUInt()
        op.eidx := eidx_next
        op.first := Bool(false)
        when (vlen_end) {
          state := s_idle
          io.op.ready := Bool(true)
          // Last queue is abox2 deepest stage
          // +1+1 because we are enqing this cycle and need to wait for the next op to be eaten by abox2
          qcntr := qcnts(3) + 1.U + Mux(io.issue(3).fire, 0.U, 1.U)
          // aret after next issue3.fire
          aret_pending := qcntr =/= 0.U
          assert(qcntr <= UInt(1), "IBox: qcntr too large. aret broken")
        }
      }
    }

    is (s_setup) {
      when (io.id =/= UInt(0)) {
        shift := pMux
        io.agu.in.valid := Bool(true)
        when (io.agu.out.valid) {
          op.base := io.agu.out.bits.addr
          val newMask = idMask & ((UInt(1) << pMux) - UInt(1))
          idMask := newMask
          when (newMask === UInt(0) || !((newMask & io.id).orR)) {
            state := s_busy
          }
        }
      }
    }
  }
}

class VMU(resetSignal: Bool = null)(implicit p: Parameters)
  extends VMUModule(_reset = resetSignal)(p) {
  val io = new Bundle {
    val id = UInt(INPUT)
    val op = Decoupled(new VMUOp).flip
    val cfg = new HwachaConfigIO().flip
    val lane = new VMUIO().flip
    val tlb = new RocketTLBIO
    val memif = new VMUMemIO

    val sret = new CounterUpdateIO(bSRet)
    val aret = Bool()
    val irq = new IRQIO
    val xcpt = new XCPTIO().flip
  }

  private val confml = (nLanes > 1)
  val ibox = Module(new IBox)
  ibox.suggestName("iboxInst")
  val pbox = Module(new PBox)
  pbox.suggestName("pboxInst")
  val abox = Module(new ABox)
  abox.suggestName("aboxInst")
  val tbox = Module(new TBox(1))
  tbox.suggestName("tboxInst")
  val sbox = Module(new SBox)
  sbox.suggestName("sboxInst")
  val vldq = Module(new Queue(io.lane.vldq.bits, nVLDQ))
  vldq.suggestName("vldqInst")
  val mbox = Module(new MBox)
  mbox.suggestName("mboxInst")
  val agu = Module(new AGU(if (confml) 2 else 1))
  agu.suggestName("aguInst")

  ibox.io.id := io.id
  ibox.io.op <> io.op
  ibox.io.cfg <> io.cfg
  io.aret <> ibox.io.aret
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
  io.lane.vldq <> vldq.io.deq

  mbox.io.inner.abox <> abox.io.mem
  mbox.io.inner.sbox <> sbox.io.mem
  vldq.io.enq <> mbox.io.inner.lbox
  io.sret <> mbox.io.sret

  io.memif <> mbox.io.outer
}

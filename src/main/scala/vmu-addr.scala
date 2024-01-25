package hwacha

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

class AGUOperand(implicit p: Parameters) extends VMUBundle()(p) {
  val base = UInt(bVAddrExtended.W)
  val offset = UInt(bVAddrExtended.W)
  val shift = UInt(math.max(bLStride + bLanes, bStrip).W)
}
class AGUResult(implicit p: Parameters) extends VVAQEntry()(p)
class AGUIO(implicit p: Parameters) extends VMUBundle()(p) {
  val in = Valid(new AGUOperand)
  val out = Flipped(Valid(new AGUResult))
}

class AGU(n: Int)(implicit p: Parameters) extends VMUModule()(p) {
// COLIN TODO FIXME: report issue in Chisel3 that io can't just equal Vec
  val io = IO(new Bundle {
    val ports = Flipped(Vec(n, new AGUIO))
  })

  val in = io.ports.init.foldRight(io.ports.last.in.bits) { case (a, b) =>
    Mux(a.in.valid, a.in.bits, b) /* Priority mux */
  }
  val offset = in.offset << in.shift
  val addr = in.base + offset

  val mask = io.ports.init.map(!_.in.valid).scanLeft(true.B)(_ && _)
  io.ports.zip(mask).foreach { case (x, m) =>
    x.out.valid := x.in.valid && m
    x.out.bits.addr := addr
  }
}


class VMUPipeEntry(implicit p: Parameters) extends VMUBundle()(p) {
  val addr = UInt((bPAddr - tlByteAddrBits).W)
  val meta = new VMUMetaAddr
}
class VMUPipeIO(implicit p: Parameters) extends DecoupledIO(new VMUPipeEntry()(p)) {
}


class VVAQ(implicit p: Parameters) extends VMUModule()(p) {
  val io = IO(new Bundle {
    val enq = Flipped(new VVAQIO())
    val deq = new VVAQIO
  })

  val q = Module(new Queue(io.enq.bits, nVVAQ))
  q.suggestName("qInst")
  q.io.enq <> io.enq
  io.deq <> q.io.deq
}

class ABox0(implicit p: Parameters) extends VMUModule()(p) {
  val io = IO(new VMUIssueIO {
    val vvaq = Flipped(new VVAQIO())
    val vpaq = new VPAQIO
    val vcu = new VCUIO

    val agu = new AGUIO
    val tlb = new TLBIO
    val mask = Flipped(new VMUMaskIO_0())
    val xcpt = Flipped(new XCPTIO())
  })

  val op = Reg(new VMUDecodedOp)
  private val mask = io.mask.bits
  val pred = mask.pred

  when (!op.mode.unit) {
    assert(!io.mask.valid || !pred || (mask.nonunit.shift === 0.U),
      "ABox0: simultaneous true predicate and non-zero stride shift")
  }

  io.agu.in.valid := false.B
  io.agu.in.bits.base := op.base
  io.agu.in.bits.offset := MuxCase(op.stride, Seq(
    op.mode.indexed -> io.vvaq.bits.addr,
    op.mode.unit -> pgSize.U))
  io.agu.in.bits.shift := Mux(op.mode.unit || op.mode.indexed, 0.U, mask.nonunit.shift)
  val addr = Mux(op.mode.indexed, io.agu.out.bits.addr, op.base)

  io.tlb.req.bits.vaddr := addr
  io.tlb.req.bits.passthrough := false.B
  io.tlb.req.bits.size := op.mt.shift()
  io.tlb.req.bits.cmd := op.cmd.bits
  io.tlb.req.bits.prv := op.status.dprv
  io.tlb.req.bits.v := op.status.dv
  io.tlb.status := op.status
  io.vpaq.bits.addr := io.tlb.paddr()
  io.vcu.bits.ecnt := mask.ecnt

  val vvaq_en = op.mode.indexed && pred
  val vvaq_valid = !vvaq_en || io.vvaq.valid
  val vpaq_ready = !pred || io.vpaq.ready
  val tlb_ready = !pred || io.tlb.req.ready

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(io.mask.valid, vvaq_valid, vpaq_ready, tlb_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.op.ready := false.B
  io.mask.ready := false.B
  io.vvaq.ready := false.B
  io.vpaq.valid := false.B
  io.vcu.valid := false.B
  io.tlb.req.valid := false.B

  val s_idle :: s_busy :: Nil = Enum(2)
  val state = RegInit(s_idle)
  val stall = RegInit(false.B)

  switch (state) {
    is (s_idle) {
      io.op.ready := true.B
    }

    is (s_busy) {
      when (!(stall || io.xcpt.prop.vmu.stall)) {
        io.mask.ready := fire(io.mask.valid)
        io.vvaq.ready := fire(vvaq_valid, vvaq_en)
        io.vpaq.valid := fire(vpaq_ready, pred, !io.tlb.resp.xcpt)
        io.tlb.req.valid := vvaq_valid && io.mask.valid && pred

        io.agu.in.valid := op.mode.indexed || !mask.last

        when (fire(null)) {
          when (!(op.mode.indexed || (op.mode.unit && !mask.unit.page))) {
            op.base := io.agu.out.bits.addr
          }
          when (io.tlb.resp.xcpt && pred) {
            stall := true.B
          } .otherwise {
            io.vcu.valid := true.B
            when (mask.last) {
              state := s_idle
              io.op.ready := true.B
            }
          }
        }
      }
    }
  }

  when (io.op.fire) { /* initialization */
    state := s_busy
    op := io.op.bits
  }
}

class VPAQ(implicit p: Parameters) extends VMUModule()(p) with SeqParameters {
  val io = IO(new Bundle {
    val enq = Flipped(new VPAQIO())
    val deq = new VPAQIO

    val vcu = Flipped(Valid(new VCUEntry))
    val la = Flipped(new CounterLookAheadIO())
  })

  val q = Module(new Queue(io.enq.bits, nVPAQ))
  q.suggestName("qInst")
  q.io.enq <> io.enq
  io.deq <> q.io.deq

  val vcucntr = Module(new LookAheadCounter(0, maxVCU))
  vcucntr.suggestName("vcucntrInst")
  vcucntr.io.inc.cnt := io.vcu.bits.ecnt
  vcucntr.io.inc.update := io.vcu.valid
  vcucntr.io.dec <> io.la
}

class ABox1(implicit p: Parameters) extends VMUModule()(p) {
  val io = IO(new VMUIssueIO {
    val vpaq = Flipped(new VPAQIO())
    val pipe = new VMUPipeIO

    val mask = Flipped(new VMUMaskIO_1())
    val xcpt = Flipped(new XCPTIO())
    val la = Flipped(new CounterLookAheadIO())
  })

  val op = Reg(new VMUDecodedOp)
  val shift = Reg(UInt(2.W))

  private val limit = tlDataBytes >> 1
  private val lglimit = tlByteAddrBits - 1

  val lead = Reg(Bool())
  val beat = Reg(UInt(1.W))
  private val beat_1 = (beat === 1.U)

  private def offset(x: UInt) = x(tlByteAddrBits-1, 0)
  val off_u = offset(op.base)
  val off_n = offset(io.vpaq.bits.addr)
  val off = Mux(op.mode.unit, off_u, off_n)
  val eoff = off >> shift
  val pad_u_rear = Cat(op.mt.b && beat_1, 0.U((tlByteAddrBits-1).W))
//val pad_u_rear = Mux(op.mt.b && beat_1, UInt(limit), 0.U)
  val pad_u = Mux(lead, off_u, pad_u_rear)
  val epad = Mux(op.mode.unit && !lead, pad_u_rear, eoff)

  /* Equivalence: x modulo UInt(limit) */
  private def truncate(x: UInt) = x(tlByteAddrBits-2, 0)
  private def saturate(x: UInt) = {
    /* Precondition: x != 0.U */
    val v = truncate(x)
    Cat(v === 0.U, v)
  }

  val ecnt_u_max = (tlDataBytes.U - pad_u) >> shift
  val ecnt_u = saturate(ecnt_u_max)
  val ecnt_max = Mux(op.mode.unit, ecnt_u, 1.U)

  val vlen_next = op.vlen.zext - ecnt_max.zext
  val vlen_end = (vlen_next <= SInt(0))
  val ecnt_test = Mux(vlen_end, offset(op.vlen), ecnt_max)

  val xcpt = io.xcpt.prop.top.stall

  val valve = RegInit(0.U(bVCU.W))
  val valve_off = (valve < ecnt_test)
  val valve_end = xcpt && (valve_off || (valve === ecnt_test))
  val ecnt = Mux(valve_off, offset(valve), ecnt_test)
  /* Track number of elements permitted to depart following VCU */
  val valve_add = Mux(io.la.reserve, io.la.cnt, 0.U)
  val valve_sub = Mux(io.mask.fire, ecnt, 0.U)
  valve := valve + valve_add - valve_sub

  val en = !valve_off || xcpt
  val end = vlen_end || valve_end

  val blkidx = Reg(UInt())
  val blkidx_next = blkidx + 1.U
  val blkidx_update = !op.mt.b || beat_1
  val blkidx_end = (blkidx_update && (blkidx_next === 0.U))

  val addr_ppn = io.vpaq.bits.addr(bPAddr-1, bPgIdx)
  val addr_pgidx = Mux(op.mode.unit,
    Cat(blkidx, Bits(0, tlByteAddrBits)),
    io.vpaq.bits.addr(bPgIdx-1, 0))
  val addr = Cat(addr_ppn, addr_pgidx)

  /* Clear predicates unrelated to the current request */
  val mask_ecnt = EnableDecoder(ecnt_max, limit)
  val mask_xcpt = EnableDecoder(valve, limit)
  val mask_aux_base = mask_ecnt & mask_xcpt
  val mask_aux = (mask_aux_base << truncate(epad))(limit-1, 0)
  val mask_data = io.mask.bits.data & mask_aux
  val pred = mask_data.orR
  val pred_hold = Reg(Bool())
  val pred_u = pred_hold || pred

  io.mask.meta.eoff := truncate(eoff)
  io.mask.meta.last := vlen_end

  io.pipe.bits.addr := addr(bPAddr-1, tlByteAddrBits)
  io.pipe.bits.meta.ecnt.encode(ecnt)
  io.pipe.bits.meta.epad := epad
  io.pipe.bits.meta.last := end
  io.pipe.bits.meta.mask := mask_data
  io.pipe.bits.meta.vsdq := io.mask.bits.vsdq

  val vpaq_deq_u = pred_u && (blkidx_end || vlen_end)
  val vpaq_deq = Mux(op.mode.unit, vpaq_deq_u, pred)
  val vpaq_valid = !pred || io.vpaq.valid

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(io.mask.valid, vpaq_valid, io.pipe.ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.op.ready := false.B
  io.mask.ready := false.B
  io.vpaq.ready := false.B
  io.pipe.valid := false.B

  val s_idle :: s_busy :: Nil = Enum(2)
  val state = RegInit(s_idle)

  switch (state) {
    is (s_idle) {
      io.op.ready := true.B
    }

    is (s_busy) {
      when (en) {
        io.mask.ready := fire(io.mask.valid)
        io.vpaq.ready := fire(vpaq_valid, vpaq_deq)
        io.pipe.valid := fire(io.pipe.ready)

        when (fire(null)) {
          lead := false.B
          pred_hold := pred_u && !blkidx_end
          when (op.mt.b) {
            beat := beat + 1.U
          }
          when (op.mode.unit && blkidx_update) {
            blkidx := blkidx_next
          }
          op.vlen := vlen_next.asUInt
          when (end) {
            state := s_idle
            io.op.ready := true.B
          }
        }
      }
    }
  }

  when (io.op.fire) { /* initialization */
    state := s_busy
    op := io.op.bits
    blkidx := io.op.bits.base(bPgIdx-1, tlByteAddrBits)
    shift := io.op.bits.mt.shift()
    beat := io.op.bits.base(tlByteAddrBits-1)
    lead := true.B
    pred_hold := false.B
  }
}

class ABox2(implicit p: Parameters) extends VMUModule()(p) {
  val io = IO(new VMUIssueIO {
    val inner = Flipped(new VMUPipeIO())
    val outer = new VMUAddrIO
    val store = Valid(new VMUStoreCtrl)
    val load = Flipped(new VLUSelectIO())
  })

  private val outer = io.outer.bits
  private val inner = io.inner.bits

  val op = Reg(new VMUDecodedOp)
  val mt = DecodedMemType(op.fn.mt)
  val vidx = Reg(io.load.bits.vidx)

  val offset = (inner.meta.epad << mt.shift())(tlByteAddrBits-1, 0)

  outer.addr := Cat(inner.addr, offset)
  outer.fn.cmd := op.fn.cmd
  outer.fn.mt := op.fn.mt
  outer.meta.vidx := vidx
  outer.meta.eidx := op.eidx
  outer.meta.ecnt := inner.meta.ecnt
  outer.meta.epad := inner.meta.epad
  outer.meta.last := inner.meta.last
  outer.meta.mask := inner.meta.mask
  outer.meta.vsdq := inner.meta.vsdq

  io.store.bits.mode.unit := op.mode.unit
  io.store.bits.base := op.base(tlByteAddrBits-1, 0)
  io.store.bits.mt := mt

  val load_en = io.op.bits.first && io.op.bits.cmd.read
  val load_valid = !load_en || io.load.valid

  private def issue(exclude: Bool, include: Bool*) = {
    val rvs = Seq(io.op.valid, load_valid)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(io.inner.valid, io.outer.ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.op.ready := false.B
  io.inner.ready := false.B
  io.outer.valid := false.B
  io.store.valid := false.B
  io.load.ready := false.B

  val s_idle :: s_busy :: Nil = Enum(2)
  val state = RegInit(s_idle)
  val init = Wire(Bool())
  init := false.B

  switch (state) {
    is (s_idle) {
      init := true.B
    }

    is (s_busy) {
      io.inner.ready := fire(io.inner.valid)
      io.outer.valid := fire(io.outer.ready)
      io.store.valid := op.cmd.write

      when (fire(null)) {
        op.eidx := op.eidx + inner.meta.ecnt.decode()
        when (inner.meta.last) {
          state := s_idle
          init := true.B
        }
      }
    }
  }

  when (init) { /* initialization */
    io.op.ready := issue(io.op.valid)
    io.load.ready := issue(load_valid, load_en)
    when (issue(null)) {
      state := s_busy
      op := io.op.bits
      when (io.op.bits.first) {
        vidx := io.load.bits.vidx
      }
    }
  }
}

class ABox(implicit p: Parameters) extends VMUModule()(p) {
  val io = IO(new Bundle {
    val op = Flipped(Vec(3, Decoupled(new VMUDecodedOp)))
    val lane = Flipped(new VVAQIO())
    val mem = new VMUAddrIO

    val agu = new AGUIO
    val tlb = new TLBIO
    val mask = Flipped(new VMUMaskIO())
    val xcpt = Flipped(new XCPTIO())
    val la = Flipped(new CounterLookAheadIO())

    val store = Valid(new VMUStoreCtrl)
    val load = Flipped(new VLUSelectIO())
  })

  val vvaq = Module(new VVAQ)
  vvaq.suggestName("vvaqInst")
  val vpaq = Module(new VPAQ)
  vpaq.suggestName("vpaqInst")
  val abox0 = Module(new ABox0)
  abox0.suggestName("abox0Inst")
  val abox1 = Module(new ABox1)
  abox1.suggestName("abox1Inst")
  val abox2 = Module(new ABox2)
  abox2.suggestName("abox2Inst")

  vvaq.io.enq <> io.lane

  abox0.io.op <> io.op(0)
  abox0.io.mask <> io.mask.ante
  abox0.io.vvaq <> vvaq.io.deq
  abox0.io.xcpt <> io.xcpt
  io.agu <> abox0.io.agu
  io.tlb <> abox0.io.tlb

  vpaq.io.enq <> abox0.io.vpaq
  vpaq.io.vcu <> abox0.io.vcu
  vpaq.io.la <> io.la

  abox1.io.op <> io.op(1)
  abox1.io.mask <> io.mask.post
  abox1.io.vpaq <> vpaq.io.deq
  abox1.io.xcpt.prop.top.stall :=  io.xcpt.prop.top.stall
  abox1.io.la.cnt := io.la.cnt
  abox1.io.la.reserve := io.la.reserve

  val pipe = Module(new Queue(abox1.io.pipe.bits, 2))
  pipe.suggestName("pipeInst")
  pipe.io.enq <> abox1.io.pipe

  abox2.io.op <> io.op(2)
  abox2.io.inner <> pipe.io.deq
  io.mem <> abox2.io.outer

  abox2.io.load <> io.load
  io.store <> abox2.io.store
}

package hwacha

import Chisel._
import Constants._
import uncore.constants.MemoryOpConstants._

class VMUAddr extends VMUMemOp {
  val meta = new VMUMetaUnion
}
class VMUAddrIO extends DecoupledIO(new VMUAddr)

class VVAQ extends VMUModule {
  val io = new Bundle {
    val enq = new VVAQIO().flip
    val deq = new VVAQIO
    val la = new CounterLookAheadIO(valaBits).flip
  }

  val q = Module(new Queue(io.enq.bits.clone, confvmu.nvvaq))
  q.io.enq <> io.enq
  io.deq <> q.io.deq

  val lacntr = Module(new LookAheadCounter(valaMax, valaMax))
  lacntr.io.la <> io.la
  lacntr.io.inc.cnt := UInt(1)
  lacntr.io.inc.update := io.deq.fire()
  lacntr.io.dec.update := Bool(false)
}

class TLBIO extends VMUBundle {
  val req = Decoupled(new rocket.TLBReq)
  val resp = new rocket.TLBRespNoHitIndex().flip

  def query(vpn: UInt, store: Bool): Bool = {
    this.req.bits.vpn := vpn
    this.req.bits.asid := UInt(0)
    this.req.bits.passthrough := Bool(false)
    this.req.bits.instruction := Bool(false)
    this.req.bits.store := store

    this.req.ready && !this.resp.miss
  }

  def query(op: VMUDecodedOp, addr: UInt, irq: IRQIO): (Bool, Bool) = {
    val ld = op.cmd.load || op.cmd.amo
    val st = op.cmd.store || op.cmd.amo

    val tlb_ready = this.query(addr(vaddrBits, pgIdxBits), st)
    val tlb_finish = tlb_ready && this.req.valid

    val addr_ma = Seq(op.mt.h, op.mt.w, op.mt.d).zipWithIndex.map(x =>
        x._1 && (addr(x._2, 0) != UInt(0))).reduce(_ || _) // misalignment

    val xcpts = Seq(addr_ma && ld, addr_ma && st,
      this.resp.xcpt_ld && ld, this.resp.xcpt_st && st)
    val irqs = Seq(irq.vmu.ma_ld, irq.vmu.ma_st,
      irq.vmu.faulted_ld, irq.vmu.faulted_st)

    irqs.zip(xcpts).foreach { case (irq, xcpt) =>
      irq := xcpt && tlb_finish
    }
    irq.vmu.aux := addr

    (tlb_ready, xcpts.reduce(_ || _))
  }
}

class ABox0 extends VMUModule {
  val io = new Bundle {
    val issue = new VMUIssueOpIO().flip
    val xcpt = new XCPTIO().flip
    val irq = new IRQIO

    val tlb = new TLBIO
    val vvaq = new VVAQIO().flip
    val vpaq = new VPAQIO
  }

  val op = io.issue.op
  private val mt = Seq(op.mt.b, op.mt.h, op.mt.w, op.mt.d)

  private def pgidx(x: UInt) = x(pgIdxBits-1, 0)
  val offset = pgidx(op.base)
  val ecnt_off = offset >> op.mt.shamt()

  val first = Reg(Bool())
  val ecnt_pg = Mux1H(mt, (0 until mt.size).map(i => UInt(pgSzBytes >> i)))
  val ecnt_max = Mux(op.unit && !op.mode.indexed,
    ecnt_pg - (ecnt_off & Fill(first, pgIdxBits)), UInt(1))

  val count = Reg(UInt(width = SZ_VLEN))
  val count_next = count.zext - ecnt_max.zext
  val count_last = (count_next <= SInt(0))
  val ecnt = Mux(count_next(SZ_VLEN), count, ecnt_max)

  val stride = Mux(op.unit, UInt(pgSzBytes), op.aux.v.stride)
  val addr_gen = Reg(UInt())
  val addr = Mux(op.mode.indexed, io.vvaq.bits.addr, addr_gen)
  val addr_valid = !op.mode.indexed || io.vvaq.valid

  val (tlb_ready, xcpt) = io.tlb.query(op, addr, io.irq)

  io.vpaq.bits.addr := Cat(io.tlb.resp.ppn, pgidx(addr))
  io.vpaq.bits.ecnt := ecnt

  val stall_hold = Reg(init = Bool(false))
  val stall = stall_hold || io.xcpt.prop.vmu.stall
  when (io.tlb.req.fire() && xcpt) {
    stall_hold := Bool(true)
  }

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  val busy = (state === s_busy)
  io.issue.busy := busy

  val en = busy && !stall
  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(addr_valid, tlb_ready, io.vpaq.ready)
    (rvs.filter(_.ne(exclude)) ++ include).reduce(_ && _) && en
  }

  io.vvaq.ready := fire(addr_valid, op.mode.indexed)
  io.tlb.req.valid := fire(tlb_ready)
  io.vpaq.valid := fire(io.vpaq.ready, !xcpt)

  switch (state) {
    is (s_idle) {
      when (io.issue.fire) {
        state := s_busy
        count := op.vlen
        when (!op.mode.indexed) {
          addr_gen := op.base
        }
        first := Bool(true)
      }
    }

    is (s_busy) {
      when (io.vpaq.fire()) {
        when (!op.mode.indexed) {
          addr_gen := addr_gen + stride
        }
        count := count_next
        when (count_last) {
          state := s_idle
        }
        first := Bool(false)
      }
    }
  }
}

class TBox(n: Int) extends VMUModule {
  private val lgn = log2Up(n)
  val io = new Bundle {
    val inner = Vec.fill(n)(new TLBIO).flip
    val outer = new TLBIO

    val sel = UInt(INPUT, lgn)
  }

  val sel = UIntToOH(io.sel, n)
  val sel_seq = (0 until n).map(sel(_))

  io.outer.req.valid := Mux1H(sel_seq, io.inner.map(_.req.valid))
  io.outer.query(
    Mux1H(sel_seq, io.inner.map(_.req.bits.vpn)),
    Mux1H(sel_seq, io.inner.map(_.req.bits.store)))

  io.inner.zip(sel_seq).foreach { case (inner, en) =>
    inner.resp := io.outer.resp
    inner.req.ready := io.outer.req.ready && en
  }
}

class VPAQ extends VMUModule {
  val io = new Bundle {
    val enq = new VPAQIO().flip
    val deq = Decoupled(UInt(width = paddrBits))
    val la = new CounterLookAheadIO(palaBits).flip
  }

  val q = Module(new Queue(io.enq.bits.addr.clone, confvmu.nvpaq))
  q.io.enq.valid := io.enq.valid
  q.io.enq.bits := io.enq.bits.addr
  io.enq.ready := q.io.enq.ready
  io.deq <> q.io.deq

  val lacntr = Module(new LookAheadCounter(0, palaMax))
  lacntr.io.la <> io.la
  lacntr.io.inc.cnt := io.enq.bits.ecnt
  lacntr.io.inc.update := io.enq.fire()
  lacntr.io.dec.update := Bool(false)
}

class ABox1 extends VMUModule {
  val io = new Bundle {
    val issue = new VMUIssueOpIO().flip
    val xcpt = new XCPTIO().flip
    val la = new CounterLookAheadIO(palaBits).flip

    val vpaq = Decoupled(UInt(width = paddrBits)).flip
    val mbox = new VMUAddrIO
  }

  val op = io.issue.op
  private val mt = Seq(op.mt.b, op.mt.h, op.mt.w, op.mt.d)
  val packed = op.unit && !op.mode.indexed

  val count = Reg(UInt(width = SZ_VLEN))
  val first = Reg(Bool())

  // Byte offset of first element
  val offset = io.vpaq.bits(tlByteAddrBits-1,0) &
    Fill(tlByteAddrBits, !packed || first)
  val eskip = offset >> op.mt.shamt()

  val ecnt_blk = Mux1H(mt, (0 until mt.size).map(i => UInt(tlDataBytes >> i)))
  val ecnt_max = Mux(packed, ecnt_blk - eskip, UInt(1))
  val count_next = count.zext - ecnt_max
  val last = (count_next <= SInt(0))
  val ecnt = Mux(last, count, ecnt_max)
  val eidx = op.vlen - count

  val subblock = Reg(UInt(width = pgIdxBits - tlByteAddrBits))
  val subblock_next = subblock + UInt(1)
  val dequeue = !packed || (subblock_next === UInt(0)) || last

  // Track number of elements permitted to depart
  val valve = Reg(init = UInt(0, palaBits))
  val valve_add = Mux(io.la.reserve, io.la.cnt, UInt(0))
  val valve_sub = Mux(io.mbox.fire(), io.mbox.bits.meta.ecnt, UInt(0))
  valve := valve + valve_add - valve_sub

  // Flush valve following exception
  val valve_on = (valve >= ecnt)
  val valve_ok = valve_on || (io.xcpt.prop.top.stall && (valve != UInt(0)))
  val ecnt_valve = Mux(!valve_on && io.xcpt.prop.top.stall, valve, ecnt)

  val ppn = io.vpaq.bits(paddrBits-1, pgIdxBits)
  val pgidx_blk = Mux(packed, subblock, io.vpaq.bits(pgIdxBits-1, tlByteAddrBits))
  val pgidx_off = io.vpaq.bits(tlByteAddrBits-1, 0) & Fill(tlByteAddrBits, !packed)
  val addr = Cat(ppn, pgidx_blk, pgidx_off)

  io.issue.busy := Bool(false)
  io.vpaq.ready := Bool(false)
  io.mbox.valid := Bool(false)
  io.mbox.bits.cmd := op.fn.cmd
  io.mbox.bits.mt := op.fn.mt
  io.mbox.bits.addr := addr
  io.mbox.bits.meta.eidx := eidx
  io.mbox.bits.meta.ecnt := ecnt_valve
  io.mbox.bits.meta.eskip := eskip

  io.mbox.bits.meta.offset := offset
  io.mbox.bits.meta.first := first
  io.mbox.bits.meta.last := last

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      when (io.issue.fire) {
        state := s_busy
        count := op.vlen
        when (packed) {
          subblock := op.base(pgIdxBits-1, tlByteAddrBits)
        }
      }
      first := Bool(true)
    }

    is (s_busy) {
      io.issue.busy := Bool(true)
      io.mbox.valid := io.vpaq.valid && valve_ok
      io.vpaq.ready := io.mbox.ready && valve_ok && dequeue

      when (io.mbox.fire()) {
        count := count_next
        when (packed) {
          subblock := subblock_next
        }
        first := Bool(false)
        when (last) {
          state := s_idle
        }
      }
    }
  }
}

class VPFQ extends VMUModule {
  val io = new Bundle {
    val xcpt = new XCPTIO().flip
    val tlb = new TLBIO

    val enq = new VVAPFQIO().flip
    val deq = new VMUAddrIO
  }

  private def vpn(x: UInt) = x(vaddrBits, pgIdxBits)
  private def pgidx(x: UInt) = x(pgIdxBits-1, 0)

  val vvapfq = Module(new Queue(io.enq.bits.clone, confvmu.nvvapfq))
  vvapfq.io.enq <> io.enq

  val tlb_ready = io.tlb.query(
    vpn(vvapfq.io.deq.bits.addr), vvapfq.io.deq.bits.store)

  val en = !io.xcpt.prop.vmu.stall
  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(vvapfq.io.deq.valid, tlb_ready, io.deq.ready)
    (rvs.filter(_.ne(exclude)) ++ include).reduce(_ && _) && en
  }

  vvapfq.io.deq.ready := fire(vvapfq.io.deq.valid)
  io.tlb.req.valid := fire(tlb_ready)
  io.deq.valid := fire(io.deq.ready,
    !(io.tlb.resp.xcpt_ld && io.tlb.resp.xcpt_st))

  io.deq.bits.cmd := Mux(vvapfq.io.deq.bits.store, M_PFW, M_PFR)
  io.deq.bits.mt := Bits(0)
  io.deq.bits.addr := Cat(io.tlb.resp.ppn, pgidx(vvapfq.io.deq.bits.addr))
  io.deq.bits.meta := (new VMUMetaUnion).fromBits(Bits(0))
}

class ABox extends VMUModule {
  val io = new Bundle {
    val issue = new VMUIssueOpIO().flip
    val xcpt = new XCPTIO().flip
    val irq = new IRQIO

    val tlb = new Bundle {
      val lane = new TLBIO
      val pf = new TLBIO
    }

    val lane = new VVAQIO().flip
    val pf = new VVAPFQIO().flip
    val la = new VMULookAheadIO().flip

    val mbox = new VMUAddrIO
  }

  val vvaq = Module(new VVAQ)
  val vpaq = Module(new VPAQ)
  val abox0 = Module(new ABox0)
  val abox1 = Module(new ABox1)

  vvaq.io.enq <> io.lane
  vvaq.io.la <> io.la.vala

  abox0.io.vvaq <> vvaq.io.deq
  abox0.io.issue.op := io.issue.op
  abox0.io.issue.fire := io.issue.fire
  abox0.io.xcpt <> io.xcpt
  abox0.io.tlb <> io.tlb.lane

  vpaq.io.enq <> abox0.io.vpaq
  vpaq.io.la <> io.la.pala

  abox1.io.vpaq <> vpaq.io.deq
  abox1.io.issue <> io.issue
  abox1.io.xcpt <> io.xcpt
  abox1.io.la <> io.la.pala

  val mboxq = Module(new Queue(io.mbox.bits.clone.asDirectionless(), 2))
  if (confvru) {
    val vpfq = Module(new VPFQ)
    vpfq.io.enq <> io.pf
    vpfq.io.xcpt <> io.xcpt
    vpfq.io.tlb <> io.tlb.pf

    val arb = Module(new RRArbiter(io.mbox.bits.clone.asDirectionless(), 2))
    arb.io.in(0) <> abox1.io.mbox
    arb.io.in(1) <> vpfq.io.deq
    mboxq.io.enq <> arb.io.out
  } else {
    mboxq.io.enq <> abox1.io.mbox
    io.pf.ready := Bool(false)
    io.tlb.pf.req.valid := Bool(false)
  }
  io.mbox <> mboxq.io.deq

  io.irq <> abox0.io.irq
}

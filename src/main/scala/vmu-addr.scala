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
    val enq = Decoupled(UInt(width = maxAddrBits)).flip
    val deq = Decoupled(UInt(width = maxAddrBits))
    val la = new LookAheadPortIO(valaBits).flip
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

class ABox0 extends VMUModule {
  val io = new Bundle {
    val issue = new VMUIssueOpIO().flip
    val xcpt = new XCPTIO().flip
    val irq = new IRQIO

    val tbox = new TLBQueryIO
    val vvaq = Decoupled(UInt(width = maxAddrBits)).flip
    val vpaq = Decoupled(new VPAQEntry)
  }

  val op = io.issue.op
  private val mt = Seq(op.mt.b, op.mt.h, op.mt.w, op.mt.d)

  private def pgidx(x: UInt) = x(pgIdxBits-1, 0)
  val offset = pgidx(op.addr.base)
  val ecnt_off = offset >> op.mt.shamt()

  val first = Reg(Bool())
  val ecnt_pg = Mux1H(mt, (0 until mt.size).map(i => UInt(pgSzBytes >> i)))
  val ecnt_max = Mux(op.unit && !op.fn.indexed,
    ecnt_pg - (ecnt_off & Fill(first, pgIdxBits)), UInt(1))

  val count = Reg(UInt(width = SZ_VLEN))
  val count_next = count.zext - ecnt_max.zext
  val count_last = (count_next <= SInt(0))
  val ecnt = Mux(count_next(SZ_VLEN), count, ecnt_max)

  val stride = Mux(op.unit, UInt(pgSzBytes), op.addr.stride)
  val addr_gen = Reg(UInt())
  val addr = Mux(op.fn.indexed, io.vvaq.bits, addr_gen)
  io.tbox.vpn.bits := addr(vaddrBits-1, pgIdxBits)
  io.vpaq.bits.addr := Cat(io.tbox.ppn, pgidx(addr))
  io.vpaq.bits.ecnt := ecnt

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  val busy = (state === s_busy)
  io.issue.busy := busy

  val stall_hold = Reg(init = Bool(false))
  val stall = stall_hold || io.xcpt.prop.vmu.stall
  val xcpt = io.irq.vmu.ma_ld || io.irq.vmu.ma_st ||
    io.irq.vmu.faulted_ld || io.irq.vmu.faulted_st

  when (io.tbox.vpn.fire() && xcpt) {
    stall_hold := Bool(true)
  }

  val addr_valid = !op.fn.indexed || io.vvaq.valid
  val tlb_ready = io.tbox.vpn.ready && !io.tbox.miss

  val en = busy && !stall
  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(addr_valid, tlb_ready, io.vpaq.ready)
    (rvs.filter(_ != exclude) ++ include).reduce(_ && _) && en
  }

  io.vvaq.ready := fire(addr_valid, op.fn.indexed)
  io.tbox.vpn.valid := fire(tlb_ready)
  io.vpaq.valid := fire(io.vpaq.ready, !xcpt)

  switch (state) {
    is (s_idle) {
      when (io.issue.fire) {
        state := s_busy
        count := op.cmd.vlen
        when (!op.fn.indexed) {
          addr_gen := op.addr.base
        }
        first := Bool(true)
      }
    }

    is (s_busy) {
      when (io.vpaq.fire()) {
        when (!op.fn.indexed) {
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

  io.tbox.store := op.fn.store || op.fn.amo

  val fn_ld = op.fn.load || op.fn.amo
  val fn_st = op.fn.store || op.fn.amo
  val tlb_finish = tlb_ready && io.tbox.vpn.valid
  val addr_misaligned = mt.tail.zipWithIndex.map(i =>
      i._1 && (addr(i._2, 0) != UInt(0))).reduce(_||_) &&
      addr_valid && en

  io.irq.vmu.ma_ld := addr_misaligned && fn_ld
  io.irq.vmu.ma_st := addr_misaligned && fn_st
  io.irq.vmu.faulted_ld := tlb_finish && fn_ld && io.tbox.xcpt.ld
  io.irq.vmu.faulted_st := tlb_finish && fn_st && io.tbox.xcpt.st
  io.irq.vmu.aux := addr
}

class TBoxQueryIO extends Bundle {
  val lane = new TLBQueryIO
  val evac = new TLBQueryIO
  val pf = new TLBQueryIO
}

class TBox extends VMUModule {
  val io = new Bundle {
    val abox = new TBoxQueryIO().flip
    val vtlb = new TLBIO
    val vpftlb = new TLBIO

    val xcpt = new XCPTIO().flip
  }

  private def translate(tlb: TLBIO, query: TLBQueryIO) {
    tlb.req.valid := query.vpn.valid
    tlb.req.bits.asid := UInt(0)
    tlb.req.bits.vpn := query.vpn.bits
    tlb.req.bits.passthrough := Bool(false)
    tlb.req.bits.instruction := Bool(false)
    tlb.req.bits.store := query.store

    query.vpn.ready := tlb.req.ready
    query.ppn := tlb.resp.ppn
    query.miss := tlb.resp.miss
    query.xcpt.ld := tlb.resp.xcpt_ld
    query.xcpt.st := tlb.resp.xcpt_st
  }

  val drain = io.xcpt.prop.vmu.drain

  // Bi-directional arbiter
  val arb = new TLBQueryIO().asDirectionless()
  private val arb_io = Seq(io.abox.lane, io.abox.evac)
  private val arb_sel = Seq(!drain, drain)

  arb.vpn.valid := Mux1H(arb_sel, arb_io.map(_.vpn.valid))
  arb.vpn.bits := Mux1H(arb_sel, arb_io.map(_.vpn.bits))
  arb.store := Mux1H(arb_sel, arb_io.map(_.store))

  translate(io.vtlb, arb)
  arb_io.zip(arb_sel).foreach { case (port, sel) =>
    port.ppn := arb.ppn
    port.miss := arb.miss
    port.xcpt := arb.xcpt
    port.vpn.ready := io.vtlb.req.ready && sel
  }

  translate(io.vpftlb, io.abox.pf)
}

class VPAQ extends VMUModule {
  val io = new Bundle {
    val enq = Decoupled(new VPAQEntry).flip
    val deq = Decoupled(enq.bits.addr.clone)
    val la = new LookAheadPortIO(palaBits).flip
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
    val la = new LookAheadPortIO(palaBits).flip

    val vpaq = Decoupled(UInt(width = paddrBits)).flip
    val mbox = new VMUAddrIO
  }

  val op = io.issue.op
  private val mt = Seq(op.mt.b, op.mt.h, op.mt.w, op.mt.d)
  val packed = op.unit && !op.fn.indexed

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
  val eidx = op.cmd.vlen - count

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
  io.mbox.bits.fn := op.cmd.fn
  io.mbox.bits.mt := op.cmd.mt
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
        count := op.cmd.vlen
        when (packed) {
          subblock := op.addr.base(pgIdxBits-1, tlByteAddrBits)
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
    val tbox = new TLBQueryIO

    val enq = new VVAPFQIO().flip
    val deq = new VMUAddrIO
  }

  private def vpn(x: UInt) = x(vaddrBits-1, pgIdxBits)
  private def pgidx(x: UInt) = x(pgIdxBits-1, 0)

  val vvapfq = Module(new Queue(io.enq.bits.clone, confvmu.nvvapfq))
  vvapfq.io.enq <> io.enq

  val en = !io.xcpt.prop.vmu.stall
  io.tbox.vpn.bits := vpn(vvapfq.io.deq.bits.addr)
  io.tbox.vpn.valid := vvapfq.io.deq.valid && en
  io.tbox.store := Bool(false)

  val tlb_ready = io.tbox.vpn.ready && !io.tbox.miss
  io.deq.valid := vvapfq.io.deq.valid && tlb_ready && en &&
    !(io.tbox.xcpt.ld && io.tbox.xcpt.st)
  vvapfq.io.deq.ready := io.deq.ready && tlb_ready && en

  io.deq.bits.fn := Mux(vvapfq.io.deq.bits.write, M_PFW, M_PFR)
  io.deq.bits.mt := Bits(0)
  io.deq.bits.addr := Cat(io.tbox.ppn, pgidx(vvapfq.io.deq.bits.addr))
  io.deq.bits.meta := (new VMUMetaUnion).fromBits(Bits(0))
}

class ABox2 extends VMUModule {
  val io = new Bundle {
    val xcpt = new XCPTIO().flip
    val irq = new IRQIO
    val tbox = new TLBQueryIO

    val abox1 = new VMUAddrIO().flip
    val evac = Decoupled(UInt(width = maxAddrBits)).flip
    val mbox = new VMUAddrIO
  }

  private def vpn(x: UInt) = x(vaddrBits-1, pgIdxBits)
  private def pgidx(x: UInt) = x(pgIdxBits-1, 0)

  val op = io.mbox.bits.clone.asDirectionless()
  op.fn := M_XWR
  op.mt := MT_D
  op.addr := Cat(io.tbox.ppn, pgidx(io.evac.bits))
  op.meta.eidx := UInt(0)
  op.meta.ecnt := UInt(1)
  op.meta.eskip := op.addr(tlByteAddrBits-1,3)
  op.meta.offset := op.addr(tlByteAddrBits-1,0)
  op.meta.first := Bool(false)
  op.meta.last := Bool(false)

  val drain = io.xcpt.prop.vmu.drain
  val tlb_ready = io.tbox.vpn.ready && !io.tbox.miss
  io.mbox.bits := Mux(drain, op, io.abox1.bits)
  io.mbox.valid := Mux(drain, io.evac.valid && tlb_ready, io.abox1.valid)
  io.evac.ready := drain && io.mbox.ready && tlb_ready
  io.abox1.ready := !drain && io.mbox.ready

  io.tbox.vpn.bits := vpn(io.evac.bits)
  io.tbox.vpn.valid := drain && io.evac.valid //&& io.mbox.ready
  io.tbox.store := Bool(true)

  io.irq.vmu.ma_ld := Bool(false)
  io.irq.vmu.ma_st := (io.evac.bits(2, 0) != UInt(0))
  io.irq.vmu.faulted_ld := Bool(false)
  io.irq.vmu.faulted_st := io.tbox.vpn.fire() && !io.tbox.miss && io.tbox.xcpt.st
  io.irq.vmu.aux := op.addr
}

object VMUIRQIO {
  def apply(out: IRQIO, in: Iterable[IRQIO]) {
    out.vmu.ma_ld := in.map(_.vmu.ma_ld).reduce(_||_)
    out.vmu.ma_st := in.map(_.vmu.ma_st).reduce(_||_)
    out.vmu.faulted_ld := in.map(_.vmu.faulted_ld).reduce(_||_)
    out.vmu.faulted_st := in.map(_.vmu.faulted_st).reduce(_||_)
    out.vmu.aux := in.map(_.vmu.aux).reduce(_|_)
  }
}

class ABox extends VMUModule {
  val io = new Bundle {
    val issue = new VMUIssueOpIO().flip
    val xcpt = new XCPTIO().flip
    val irq = new IRQIO

    val tbox = new TBoxQueryIO

    val lane = new VAQLaneIO().flip
    val evac = new VVAQIO().flip
    val pf = new VVAPFQIO().flip
    val mbox = new VMUAddrIO
  }

  val vvaq = Module(new VVAQ)
  val vpaq = Module(new VPAQ)
  val abox0 = Module(new ABox0)
  val abox1 = Module(new ABox1)
  val abox2 = Module(new ABox2)

  vvaq.io.enq <> io.lane.q
  vvaq.io.la <> io.lane.vala

  abox0.io.vvaq <> vvaq.io.deq
  abox0.io.issue.op := io.issue.op
  abox0.io.issue.fire := io.issue.fire
  abox0.io.xcpt <> io.xcpt
  abox0.io.tbox <> io.tbox.lane

  vpaq.io.enq <> abox0.io.vpaq
  vpaq.io.la <> io.lane.pala

  abox1.io.vpaq <> vpaq.io.deq
  abox1.io.issue <> io.issue
  abox1.io.xcpt <> io.xcpt
  abox1.io.la <> io.lane.pala

  abox2.io.abox1 <> abox1.io.mbox
  abox2.io.evac <> io.evac
  abox2.io.tbox <> io.tbox.evac
  abox2.io.xcpt <> io.xcpt

  val mboxq = Module(new Queue(io.mbox.bits.clone.asDirectionless(), 2))
  if (confvru) {
    val vpfq = Module(new VPFQ)
    vpfq.io.enq <> io.pf
    vpfq.io.xcpt <> io.xcpt
    vpfq.io.tbox <> io.tbox.pf

    val arb = Module(new RRArbiter(io.mbox.bits.clone.asDirectionless(), 2))
    arb.io.in(0) <> abox2.io.mbox
    arb.io.in(1) <> vpfq.io.deq
    mboxq.io.enq <> arb.io.out
  } else {
    mboxq.io.enq <> abox2.io.mbox
    io.pf.ready := Bool(false)
    io.tbox.pf.vpn.valid := Bool(false)
  }
  io.mbox <> mboxq.io.deq

  VMUIRQIO(io.irq, Seq(abox0.io.irq, abox2.io.irq))
}

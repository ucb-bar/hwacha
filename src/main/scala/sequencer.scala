package hwacha

import Chisel._
import Node._
import Constants._

class SequencerOpIO extends ValidIO(new SequencerOp)

class Sequencer(resetSignal: Bool = null)(implicit conf: HwachaConfiguration) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val xcpt = new XCPTIO().flip

    val issueop = new IssueOpIO().flip
    val seqop = new SequencerOpIO
    val aiwop = new AIWOpIO

    val vmu = new VMUIO
    val lreq = new LookAheadPortIO(log2Down(conf.nvlreq)+1)
    val sreq = new LookAheadPortIO(log2Down(conf.nvsreq)+1)

    val seq_to_hazard = new SequencerToHazardIO
  }

  class BuildSequencer[T<:Data](n: Int)
  {
    val valid = Vec.fill(n){Reg(init=Bool(false))}
    val stall = Vec.fill(n){Reg(init=Bool(false))}
    val vlen = Vec.fill(n){Reg(UInt(width=SZ_VLEN))}
    val last = Vec.fill(n){Reg(Bool())}
    val e = Vec.fill(n){Reg(new SequencerEntry)}
    val aiw = Vec.fill(n){Reg(new AIWEntry)}

    def defreset = {
      when (reset) {
        for (i <- 0 until n) {
          e(i).active := e(i).active.clone().fromBits(Bits(0))
          aiw(i).active := aiw(i).active.clone().fromBits(Bits(0))
        }
      }
    }

    def min(a: UInt, b: UInt) = Mux(a < b, a, b)
    def div2ceil(x: UInt) = (x + UInt(1)) >> UInt(1)
    def div4ceil(x: UInt) = (x + UInt(3)) >> UInt(2)

    // increase throughput when certain operations (FMA, VLDQ, VSDQ) are used by
    // allowing up to 32 elements to proceed in half precision
    // allowing up to 16 elements to proceed in single precision
    def turbo_capable(slot: UInt) =
      e(slot).active.vau1t || e(slot).active.vau1f ||
      e(slot).active.vlu ||
      e(slot).active.vsu

    def nstrip(slot: UInt) = {
      val ret = UInt(width = SZ_BCNT)
      ret := min(vlen(slot), io.cfg.bcnt)

      when (turbo_capable(slot)) {
        when (io.cfg.prec === PREC_SINGLE) {
          ret := min(div2ceil(vlen(slot)), io.cfg.bcnt)
        }
        when (io.cfg.prec === PREC_HALF) {
          ret := min(div4ceil(vlen(slot)), io.cfg.bcnt)
        }
      }

      ret
    }

    def nelements(slot: UInt) = {
      val ret = UInt(width = SZ_BCNT+2)
      ret := min(vlen(slot), io.cfg.bcnt)

      when (turbo_capable(slot)) {
        when (io.cfg.prec === PREC_SINGLE) {
          ret := min(vlen(slot), io.cfg.bcnt << UInt(1))
        }
        when (io.cfg.prec === PREC_HALF) {
          ret := min(vlen(slot), io.cfg.bcnt << UInt(2))
        }
      }

      ret
    }

    def stride(float: Bool) =
      Mux(float, io.cfg.fstride, io.cfg.xstride)

    def islast(slot: UInt) = {
      val ret = Bool()
      ret := vlen(slot) <= io.cfg.bcnt

      when (turbo_capable(slot)) {
        when (io.cfg.prec === PREC_SINGLE) {
          ret := (vlen(slot) >> UInt(1)) <= io.cfg.bcnt
        }
        when (io.cfg.prec === PREC_HALF) {
          ret := (vlen(slot) >> UInt(2)) <= io.cfg.bcnt
        }
      }

      ret
    }

    def next_addr_base(slot: UInt) =
      e(slot).imm.imm + (e(slot).imm.stride << UInt(3))

    def vgu_val(slot: UInt) = valid(slot) && e(slot).active.vgu
    def vcu_val(slot: UInt) = valid(slot) && e(slot).active.vcu
    def vlu_val(slot: UInt) = valid(slot) && e(slot).active.vlu
    def vsu_val(slot: UInt) = valid(slot) && e(slot).active.vsu

    def alu_active(slot: UInt) =
      e(slot).active.viu ||
      e(slot).active.vau0 || e(slot).active.vau1t || e(slot).active.vau1f || e(slot).active.vau2

    def ldst_active(slot: UInt) =
      e(slot).active.vlu || e(slot).active.vsu

    def vldst_active(slot: UInt) =
      ldst_active(slot) && !e(slot).fn.vmu.utmemop()

    def utldst_active(slot: UInt) =
      ldst_active(slot) && e(slot).fn.vmu.utmemop()

    def mem_active(slot: UInt) =
      e(slot).active.vgu || e(slot).active.vcu || ldst_active(slot)

    def active(slot: UInt) =
      alu_active(slot) || mem_active(slot)
  }

  class BuildOrderTable(nslots: Int)
  {
    val nvfu = 4
    val vgu = 0
    val vcu = 1
    val vlu = 2
    val vsu = 3

    val tbl = Vec.fill(nslots){Vec.fill(nvfu){Reg(init=Bool(false))}}

    // if there are no vfu depndencies issuing
    // then depend on every vfu
    def mark(cond: Bool, slot: UInt) = {
      when (cond) {
        for (i <- 0 until nvfu)
          tbl(slot)(i) := Bool(true)
      }
    }

    // if there are vfu dependencies issuing
    def mark(cond: Bool, deps: (UInt, Int)*) = {
      when (cond) {
        val dep = Array.fill(nvfu){true}
        for ((slot, vfu) <- deps) {
          // then first clear all dependence
          for (i <- 0 until nslots)
            tbl(i)(vfu) := Bool(false)
          // then don't depend on that particular vfu
          dep(vfu) = false
          for (i <- 0 until nvfu)
            tbl(slot)(i) := Bool(dep(i))
        }
      } // end when
    }

    def check(slot: UInt, vfu: Int) = tbl(slot)(vfu)
  }

  val bcnt = io.cfg.bcnt

  val ptr = Reg(init = UInt(0, SZ_BPTR))
  val ptr1 = PtrIncr(ptr, 1, bcnt)
  val ptr2 = PtrIncr(ptr, 2, bcnt)
  val ptr3 = PtrIncr(ptr, 3, bcnt)
  ptr := ptr1

  val seq = new BuildSequencer(SZ_BANK)

  val vlen = io.issueop.bits.vlen
  val last = vlen <= bcnt

  val turbo_last = MuxLookup(io.cfg.prec, vlen < bcnt, Array(
    PREC_DOUBLE -> (vlen <= bcnt),
    PREC_SINGLE -> ((vlen >> UInt(1)) <= bcnt),
    PREC_HALF -> ((vlen >> UInt(2)) <= bcnt)
  ))

  when (io.issueop.valid) {

    when (io.issueop.bits.active.viu) {
      seq.valid(ptr1) := Bool(true)
      seq.vlen(ptr1) := vlen
      seq.last(ptr1) := last
      seq.e(ptr1).active.viu := Bool(true)
      seq.e(ptr1).utidx := io.issueop.bits.utidx
      seq.e(ptr1).fn.viu := io.issueop.bits.fn.viu
      seq.e(ptr1).reg.vs := io.issueop.bits.reg.vs
      seq.e(ptr1).reg.vt := io.issueop.bits.reg.vt
      seq.e(ptr1).reg.vd := io.issueop.bits.reg.vd
      seq.e(ptr1).imm.imm := io.issueop.bits.imm.imm
      seq.aiw(ptr1) := io.issueop.bits.aiw
    }

    when (io.issueop.bits.active.vau0) {
      seq.valid(ptr1) := Bool(true)
      seq.vlen(ptr1) := vlen
      seq.last(ptr1) := last
      seq.e(ptr1).active.vau0 := Bool(true)
      seq.e(ptr1).fn.vau0 := io.issueop.bits.fn.vau0
      seq.e(ptr1).reg.vs := io.issueop.bits.reg.vs
      seq.e(ptr1).reg.vt := io.issueop.bits.reg.vt
      seq.e(ptr1).reg.vd := io.issueop.bits.reg.vd
      seq.aiw(ptr1) := io.issueop.bits.aiw
    }

    when (io.issueop.bits.active.vau1) {
      seq.valid(ptr1) := Bool(true)
      seq.vlen(ptr1) := vlen
      seq.last(ptr1) := turbo_last
      when (io.issueop.bits.sel.vau1) { seq.e(ptr1).active.vau1t := Bool(true) }
      .otherwise { seq.e(ptr1).active.vau1f := Bool(true) }
      seq.e(ptr1).fn.vau1 := io.issueop.bits.fn.vau1
      seq.e(ptr1).reg.vs := io.issueop.bits.reg.vs
      seq.e(ptr1).reg.vt := io.issueop.bits.reg.vt
      seq.e(ptr1).reg.vr := io.issueop.bits.reg.vr
      seq.e(ptr1).reg.vd := io.issueop.bits.reg.vd
      seq.aiw(ptr1) := io.issueop.bits.aiw
    }

    when (io.issueop.bits.active.vau2) {
      seq.valid(ptr1) := Bool(true)
      seq.vlen(ptr1) := vlen
      seq.last(ptr1) := last
      seq.e(ptr1).active.vau2 := Bool(true)
      seq.e(ptr1).fn.vau2 := io.issueop.bits.fn.vau2
      seq.e(ptr1).reg.vs := io.issueop.bits.reg.vs
      seq.e(ptr1).reg.vd := io.issueop.bits.reg.vd
      seq.aiw(ptr1) := io.issueop.bits.aiw
    }

    when (io.issueop.bits.active.amo) {
      seq.valid(ptr1) := Bool(true)
      seq.vlen(ptr1) := vlen
      seq.last(ptr1) := last
      seq.e(ptr1).active.vgu := Bool(true)
      seq.e(ptr1).fn.vmu := io.issueop.bits.fn.vmu
      seq.e(ptr1).reg.vs := io.issueop.bits.reg.vs
      seq.e(ptr1).imm.imm := Bits(0)

      seq.valid(ptr2) := Bool(true)
      seq.vlen(ptr2) := vlen
      seq.last(ptr2) := last
      seq.e(ptr2).active.vcu := Bool(true)
      seq.e(ptr2).active.vsu := Bool(true)
      seq.e(ptr2).fn.vmu := io.issueop.bits.fn.vmu
      seq.e(ptr2).reg.vt := io.issueop.bits.reg.vt

      seq.valid(ptr3) := Bool(true)
      seq.vlen(ptr3) := vlen
      seq.last(ptr3) := last
      seq.e(ptr3).active.vlu := Bool(true)
      seq.e(ptr3).fn.vmu := io.issueop.bits.fn.vmu
      seq.e(ptr3).reg.vd := io.issueop.bits.reg.vd
      seq.aiw(ptr3) := io.issueop.bits.aiw
    }

    when (io.issueop.bits.active.utld) {
      seq.valid(ptr1) := Bool(true)
      seq.vlen(ptr1) := vlen
      seq.last(ptr1) := last
      seq.e(ptr1).active.vgu := Bool(true)
      seq.e(ptr1).fn.vmu := io.issueop.bits.fn.vmu
      seq.e(ptr1).reg.vs := io.issueop.bits.reg.vs
      seq.e(ptr1).imm.imm := io.issueop.bits.imm.imm

      seq.valid(ptr2) := Bool(true)
      seq.vlen(ptr2) := vlen
      seq.last(ptr2) := last
      seq.e(ptr2).active.vcu := Bool(true)
      seq.e(ptr2).fn.vmu := io.issueop.bits.fn.vmu

      seq.valid(ptr3) := Bool(true)
      seq.vlen(ptr3) := vlen
      seq.last(ptr3) := turbo_last
      seq.e(ptr3).active.vlu := Bool(true)
      seq.e(ptr3).fn.vmu := io.issueop.bits.fn.vmu
      seq.e(ptr3).reg.vd := io.issueop.bits.reg.vd
      seq.aiw(ptr3) := io.issueop.bits.aiw
    }

    when (io.issueop.bits.active.utst) {
      seq.valid(ptr1) := Bool(true)
      seq.vlen(ptr1) := vlen
      seq.last(ptr1) := last
      seq.e(ptr1).active.vgu := Bool(true)
      seq.e(ptr1).fn.vmu := io.issueop.bits.fn.vmu
      seq.e(ptr1).reg.vs := io.issueop.bits.reg.vs
      seq.e(ptr1).imm.imm := io.issueop.bits.imm.imm

      seq.valid(ptr2) := Bool(true)
      seq.vlen(ptr2) := vlen
      seq.last(ptr2) := turbo_last
      seq.e(ptr2).active.vcu := Bool(true)
      seq.e(ptr2).active.vsu := Bool(true)
      seq.e(ptr2).fn.vmu := io.issueop.bits.fn.vmu  
      seq.e(ptr2).reg.vt := io.issueop.bits.reg.vt
      seq.aiw(ptr2) := io.issueop.bits.aiw
    }

    when (io.issueop.bits.active.vld) {
      seq.valid(ptr1) := Bool(true)
      seq.vlen(ptr1) := vlen
      seq.last(ptr1) := last
      seq.e(ptr1).active.vgu := Bool(true)
      seq.e(ptr1).fn.vmu := io.issueop.bits.fn.vmu
      seq.e(ptr1).imm := io.issueop.bits.imm

      seq.valid(ptr2) := Bool(true)
      seq.vlen(ptr2) := vlen
      seq.last(ptr2) := last
      seq.e(ptr2).active.vcu := Bool(true)
      seq.e(ptr2).fn.vmu := io.issueop.bits.fn.vmu

      seq.valid(ptr3) := Bool(true)
      seq.vlen(ptr3) := vlen
      seq.last(ptr3) := last
      seq.e(ptr3).active.vlu := Bool(true)
      seq.e(ptr3).fn.vmu := io.issueop.bits.fn.vmu
      seq.e(ptr3).reg.vd := io.issueop.bits.reg.vd
      seq.e(ptr3).imm := io.issueop.bits.imm
      seq.aiw(ptr3) := io.issueop.bits.aiw
    }

    when (io.issueop.bits.active.vst) {
      seq.valid(ptr1) := Bool(true)
      seq.vlen(ptr1) := vlen
      seq.last(ptr1) := last
      seq.e(ptr1).active.vgu := Bool(true)
      seq.e(ptr1).fn.vmu := io.issueop.bits.fn.vmu 
      seq.e(ptr1).imm := io.issueop.bits.imm

      seq.valid(ptr2) := Bool(true)
      seq.vlen(ptr2) := vlen
      seq.last(ptr2) := last
      seq.e(ptr2).active.vcu := Bool(true)
      seq.e(ptr2).active.vsu := Bool(true)
      seq.e(ptr2).fn.vmu := io.issueop.bits.fn.vmu
      seq.e(ptr2).reg.vt := io.issueop.bits.reg.vt
      seq.e(ptr2).imm := io.issueop.bits.imm
      seq.aiw(ptr2) := io.issueop.bits.aiw
    }

  }

  // common
  val nelements = seq.nelements(ptr)
  val nstrip = seq.nstrip(ptr)
  val islast = seq.islast(ptr)

  // figure out stalls due to vmu
  val ot = new BuildOrderTable(SZ_BANK)
  ot.mark(io.issueop.valid && io.issueop.bits.active.viu, ptr1)
  ot.mark(io.issueop.valid && io.issueop.bits.active.vau0, ptr1)
  ot.mark(io.issueop.valid && io.issueop.bits.active.vau1, ptr1)
  ot.mark(io.issueop.valid && io.issueop.bits.active.vau2, ptr1)
  ot.mark(io.issueop.valid && io.issueop.bits.active.amo, (ptr1, ot.vgu), (ptr2, ot.vcu), (ptr2, ot.vsu), (ptr3, ot.vlu))
  ot.mark(io.issueop.valid && io.issueop.bits.active.utld, (ptr1, ot.vgu), (ptr2, ot.vcu), (ptr3, ot.vlu))
  ot.mark(io.issueop.valid && io.issueop.bits.active.utst, (ptr1, ot.vgu), (ptr2, ot.vcu), (ptr2, ot.vsu))
  ot.mark(io.issueop.valid && io.issueop.bits.active.vld, (ptr1, ot.vgu), (ptr2, ot.vcu), (ptr3, ot.vlu))
  ot.mark(io.issueop.valid && io.issueop.bits.active.vst, (ptr1, ot.vgu), (ptr2, ot.vcu), (ptr2, ot.vsu))

  io.vmu.addr.vala.cnt := nelements
  io.vmu.addr.pala.cnt := nelements
  io.vmu.ldata.la.cnt := nelements
  io.vmu.sdata.la.cnt := nelements
  io.lreq.cnt := nelements
  io.sreq.cnt := nelements

  val vgu_stall = // stall vgu op when
    !io.vmu.addr.vala.available // not enough space in vvaq
  val vcu_stall = // stall vcu op when
    !io.vmu.addr.pala.available || // not sufficient physical addresses
    seq.e(ptr).fn.vmu.lreq() && !io.lreq.available || // not enough space in lreq counter
    seq.e(ptr).fn.vmu.sreq() && !io.sreq.available    // not enough space in sreq counter
  val vlu_stall = // stall vlu op when
    !io.vmu.ldata.la.available // not sufficient data in vldq
  val vsu_stall = // stall vsu op when
    !io.vmu.sdata.la.available // not enough space in vsdq

  val reg_vgu_stall = Reg(init = Bool(false))
  val reg_vcu_stall = Reg(init = Bool(false))
  val reg_vlu_stall = Reg(init = Bool(false))
  val reg_vsu_stall = Reg(init = Bool(false))

  val masked_vgu_stall = ot.check(ptr, ot.vgu) & reg_vgu_stall
  val masked_vcu_stall = ot.check(ptr, ot.vcu) & reg_vcu_stall
  val masked_vlu_stall = ot.check(ptr, ot.vlu) & reg_vlu_stall
  val masked_vsu_stall = ot.check(ptr, ot.vsu) & reg_vsu_stall

  def construct_mask(exclude: Bool) = {
    Array(masked_vgu_stall, masked_vcu_stall, masked_vlu_stall, masked_vsu_stall).filter(_ != exclude).reduce(_||_)
  }

  when (seq.vgu_val(ptr) && !construct_mask(masked_vgu_stall)) { reg_vgu_stall := vgu_stall }
  when (seq.vcu_val(ptr) && !construct_mask(masked_vcu_stall)) { reg_vcu_stall := vcu_stall }
  when (seq.vlu_val(ptr) && !construct_mask(masked_vlu_stall)) { reg_vlu_stall := vlu_stall }
  when (seq.vsu_val(ptr) && !construct_mask(masked_vsu_stall)) { reg_vsu_stall := vsu_stall }

  val masked_xcpt_stall = io.xcpt.prop.seq.stall && !seq.vlu_val(ptr)

  val stall =
    masked_xcpt_stall ||
    masked_vgu_stall || masked_vcu_stall || masked_vlu_stall || masked_vsu_stall ||
    seq.vgu_val(ptr) && vgu_stall ||
    seq.vcu_val(ptr) && vcu_stall ||
    seq.vlu_val(ptr) && vlu_stall ||
    seq.vsu_val(ptr) && vsu_stall

  seq.stall(ptr) := stall

  val valid = !stall && seq.valid(ptr)

  // update sequencer state
  when (valid) {
    when (seq.active(ptr)) {
      seq.vlen(ptr) := seq.vlen(ptr) - nelements
      seq.e(ptr).utidx := seq.e(ptr).utidx + bcnt
      seq.e(ptr).reg.vs.id := seq.e(ptr).reg.vs.id + seq.stride(seq.e(ptr).reg.vs.float)
      seq.e(ptr).reg.vt.id := seq.e(ptr).reg.vt.id + seq.stride(seq.e(ptr).reg.vt.float)
      seq.e(ptr).reg.vr.id := seq.e(ptr).reg.vr.id + seq.stride(seq.e(ptr).reg.vr.float)
      seq.e(ptr).reg.vd.id := seq.e(ptr).reg.vd.id + seq.stride(seq.e(ptr).reg.vd.float)

      when (islast) {
        seq.valid(ptr) := Bool(false)
        seq.last(ptr) := Bool(false)
        seq.e(ptr).active.viu := Bool(false)
        seq.e(ptr).active.vau0 := Bool(false)
        seq.e(ptr).active.vau1t := Bool(false)
        seq.e(ptr).active.vau1f := Bool(false)
        seq.e(ptr).active.vau2 := Bool(false)
        seq.e(ptr).active.vgu := Bool(false)
        seq.e(ptr).active.vcu := Bool(false)
        seq.e(ptr).active.vlu := Bool(false)
        seq.e(ptr).active.vsu := Bool(false)
        seq.aiw(ptr).active.imm1 := Bool(false)
        seq.aiw(ptr).active.cnt := Bool(false)
      }
      .otherwise {
        seq.last(ptr) := islast
      }
    }

    when (seq.mem_active(ptr) && !seq.e(ptr).fn.vmu.utmemop()) {
      seq.e(ptr).imm.imm := seq.next_addr_base(ptr)
    }

    when (seq.alu_active(ptr) || seq.ldst_active(ptr)) {
      seq.aiw(ptr).cnt.utidx := seq.aiw(ptr).cnt.utidx + nelements
    }
  }

  // output
  io.seq_to_hazard.stall := (seq.stall.toBits & seq.valid.toBits).orR
  io.seq_to_hazard.last := valid && islast
  io.seq_to_hazard.active := seq.e(ptr).active
  io.seq_to_hazard.cnt := nstrip

  io.seqop.valid := valid
  io.seqop.bits.cnt := nstrip
  io.seqop.bits.last := islast
  io.seqop.bits <> seq.e(ptr)

  io.vmu.addr.vala.reserve := valid && seq.vgu_val(ptr)
  io.vmu.addr.pala.reserve := valid && (seq.vcu_val(ptr) || seq.vsu_val(ptr))
  io.vmu.ldata.la.reserve := valid && seq.vlu_val(ptr)
  io.vmu.sdata.la.reserve := valid && seq.vsu_val(ptr)
  io.lreq.reserve := valid && (seq.vcu_val(ptr) && seq.e(ptr).fn.vmu.lreq())
  io.sreq.reserve := valid && (seq.vsu_val(ptr) && seq.e(ptr).fn.vmu.sreq())

  // aiw
  io.aiwop.imm1.valid := Bool(false)
  io.aiwop.cnt.valid := Bool(false)
  io.aiwop.numcnt.valid := Bool(false)
  io.aiwop.numcnt.bits.last := Bool(false)
  
  when (valid) {
    when (seq.alu_active(ptr) || seq.ldst_active(ptr)) {
      io.aiwop.cnt.valid := seq.aiw(ptr).active.cnt

      when (islast) {
        io.aiwop.numcnt.valid := seq.aiw(ptr).active.cnt
        io.aiwop.numcnt.bits.last := seq.aiw(ptr).active.cnt
      }
    }

    when (seq.alu_active(ptr) || seq.utldst_active(ptr)) {
      when (islast) {
        io.aiwop.imm1.valid := seq.aiw(ptr).active.imm1
      }
    } 

    when (seq.vldst_active(ptr)) {
      io.aiwop.imm1.valid := seq.aiw(ptr).active.imm1
    }
  }

  io.aiwop.imm1.bits <> seq.aiw(ptr).imm1
  io.aiwop.imm1.bits.base := seq.next_addr_base(ptr)
  io.aiwop.imm1.bits.ldst := seq.ldst_active(ptr)
  io.aiwop.cnt.bits.rtag := seq.aiw(ptr).cnt.rtag
  io.aiwop.cnt.bits.utidx := seq.aiw(ptr).cnt.utidx + nelements
  io.aiwop.numcnt.bits <> seq.aiw(ptr).numcnt

  seq.defreset
}

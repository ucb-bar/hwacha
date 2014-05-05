package hwacha

import Chisel._
import Constants._

class StoreAligner(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val ctrl = new VMUBackendIO().flip
    val enq = new VSDQIO().flip
    val deq = new VSDQIO
  }

  val s_idle :: s_head :: s_pack :: s_tail :: Nil = Enum(UInt(), 4)
  val state = Reg(init = s_idle)

  val op_type = Vec(io.ctrl.op.typ.d, io.ctrl.op.typ.w, io.ctrl.op.typ.h, io.ctrl.op.typ.b)
  val op_pack = if (conf.vmu.pack_st)
    (io.ctrl.op.cmd.st && io.ctrl.op.unit && io.ctrl.op.tvec) else Bool(false)

  val utidx = Reg(UInt(width = SZ_VLEN))
  val utcnt = UInt()
  utcnt := UInt(1)
  val utidx_next = utidx + utcnt
  val at_end = (utidx_next === io.ctrl.op.vlen)

  val data_hold = Reg(Bits(width = conf.vmu.sz_data))
  val dequeue = Bool()
  dequeue := Bool(false)

  when (io.deq.fire()) {
    utidx := utidx_next
    when (dequeue) {
      data_hold := io.enq.bits
    }
  }

  val utcnt_pack = Mux1H(op_type,
    Vec(UInt(conf.vmu.nd), UInt(conf.vmu.nw), UInt(conf.vmu.nh), UInt(conf.vmu.nb)))
  val utcnt_resid = io.ctrl.op.vlen - (utidx_next & Fill(state != s_idle, SZ_VLEN))
  val at_tail = (utcnt_resid < utcnt_pack)
  val at_edge = Mux1H(op_type, Vec(Bool(true), utidx(0), utidx(1,0).andR, utidx(2,0).andR))

  private val w = log2Up(conf.vmu.nb)
  val offset = io.ctrl.op.base(w-1,0)
  val offset_ut = Mux1H(op_type, Vec(UInt(0), offset(w-1,2), offset(w-1,1), offset))
  val utcnt_head = utcnt_pack - offset_ut

  val rshift = /*Fill(op_pack, 3) &*/ Mux1H(op_type,
    Vec(UInt(0), Cat(utidx(0), Bits(0,2)), Cat(utidx(1,0), Bits(0,1)), utidx(2,0)))

  val data_solo = Vec((0 until conf.vmu.sz_data by SZ_XB).map(i => io.enq.bits(conf.vmu.sz_data-1,i)))
  val data_pack = Vec(io.enq.bits +:
    (conf.vmu.sz_data-SZ_XB until 0 by -SZ_XB).map(i =>
      Cat(io.enq.bits(i-1,0), data_hold(conf.vmu.sz_data-1,i))))
  io.deq.bits := Mux(state === s_pack, data_pack(offset), data_solo(rshift))


  io.ctrl.busy := Bool(true)
  io.enq.ready := io.deq.ready && dequeue
  io.deq.valid := io.enq.valid

  switch (state) {
    is (s_idle) {
      io.enq.ready := Bool(false)
      io.deq.valid := Bool(false)
      io.ctrl.busy := Bool(false)

      when (io.ctrl.fire && (io.ctrl.op.cmd.st || io.ctrl.op.cmd.amo)) {
        utidx := UInt(0)
        state := Mux(!op_pack || at_tail, s_tail,
          Mux(offset === UInt(0), s_pack, s_head))
      }
    }

    is (s_head) {
      dequeue := at_end
      when (io.deq.fire()) {
        when (at_end) {
          state := s_idle
        }
        .elsewhen (utidx_next === utcnt_head) {
          when (at_tail) {
            state := s_tail
          }
          .otherwise {
            state := s_pack
            dequeue := Bool(true)
          }
        }
      }
    }

    is (s_pack) {
      utcnt := utcnt_pack
      dequeue := !at_tail || at_end
      when (io.deq.fire()) {
        when (at_end) {
          state := s_idle
        }
        .elsewhen (at_tail) {
          state := s_tail
        }
      }
    }

    is (s_tail) {
      dequeue := (if (conf.vmu.pack_st) (at_edge || at_end) else Bool(true))
      when (io.deq.fire()) {
        when (at_end) {
          state := s_idle
        }
      }
    }
  }
}

class StoreDataUnit(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val ctrl = new VMUBackendIO().flip
    val lane = new VSDQIO().flip
    val evac = new VSDQIO().flip
    val memif = new VSDQIO
  }

  val arb = Module(new Arbiter(Bits(width = conf.vmu.sz_data), 2))
  val vsdq = Module(new Queue(Bits(width = conf.vmu.sz_data), conf.vmu.nvsdq))

  val align = Module(new StoreAligner)
  align.io.enq <> io.lane
  io.ctrl <> align.io.ctrl

  arb.io.in(0) <> align.io.deq
  arb.io.in(1) <> io.evac
  vsdq.io.enq <> arb.io.out
  io.memif <> vsdq.io.deq
}

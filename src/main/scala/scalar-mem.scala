package hwacha

import Chisel._
import Constants._
import uncore.constants.MemoryOpConstants._

class ScalarMemIO extends DecoupledIO(new VMUAuxScalar)

class SMU extends VMUModule {
  val io = new Bundle {
    val issue = new VMUIssueOpIO().flip
    val xcpt = new XCPTIO().flip
    val irq = new IRQIO

    val tlb = new TLBIO

    val scalar = new ScalarMemIO
    val outer = new VMUMemIO
  }

  private val scalarDataBits = params(HwachaScalarDataBits)
  private def pgidx(x: UInt) = x(pgIdxBits-1, 0)

  private val op = io.issue.op
  private val mt = Seq(op.mt.b, op.mt.h, op.mt.w, op.mt.d)

  private val req = io.outer.req
  private val resp = io.outer.resp

  val addr = Cat(io.tlb.resp.ppn, pgidx(op.base))
  val shift = addr(tlByteAddrBits-1, 0)
  val mask = Mux1H(mt, Seq(Bits(0x00), Bits(0x01), Bits(0x07), Bits(0x7f)))

  val (tlb_ready, xcpt) = io.tlb.query(op, op.base, io.irq)

  val en = !io.xcpt.prop.vmu.stall
  private def fire_req(exclude: Bool, include: Bool*) = {
    val rvs = Seq(tlb_ready, req.ready)
    (rvs.filter(_.ne(exclude)) ++ include).reduce(_ && _) && en
  }

  req.bits.cmd := op.fn.cmd
  req.bits.mt := op.fn.mt
  req.bits.addr := addr
  req.bits.tag := UInt(0)
  req.bits.store.data := op.aux.s.data << Cat(shift, Bits(0,3))
  req.bits.store.mask := Cat(mask, Bits(1,1)) << shift

  val resp_data = resp.bits.data >> Cat(shift, Bits(0,3))
  val resp_mask = FillInterleaved(SZ_B, mask)
  val resp_extend = Fill(scalarDataBits-SZ_B, !op.mt.unsigned &&
    Mux1H(mt, Seq(SZ_B, SZ_H, SZ_W, SZ_D).map(i => resp_data(i-1))))

  io.scalar.valid := Bool(false)
  io.scalar.bits.id := op.aux.s.id
  io.scalar.bits.data := Cat(
    (resp_data(scalarDataBits-1, SZ_B) & resp_mask) |
    (resp_extend & ~resp_mask),
    resp_data(SZ_B-1, 0))

  io.tlb.req.valid := Bool(false)
  resp.ready := Bool(false)
  req.valid := Bool(false)

  io.issue.busy := Bool(true)

  val s_idle :: s_req :: s_resp :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      io.issue.busy := Bool(false)
      when (io.issue.fire) {
        state := s_req
      }
    }

    is (s_req) {
      io.tlb.req.valid := fire_req(tlb_ready)
      req.valid := fire_req(req.ready, !xcpt)
      when (fire_req(null)) {
        state := Mux(xcpt, s_idle, s_resp)
      }
    }

    is (s_resp) {
      resp.ready := io.scalar.ready
      io.scalar.valid := resp.valid
      when (resp.fire()) {
        state := s_idle
      }
    }
  }
}

// TODO: Remove

class DeprecatedScalarMemOp extends Bundle {
  val fn = new VMUFn
  val addr = Bits(width = params(HwachaScalarDataBits))
  val data = Bits(width = params(HwachaScalarDataBits))
}

class DeprecatedScalarMemIO extends Bundle {
  val op = Valid(new DeprecatedScalarMemOp)
  val xcpt = Bool(INPUT)
  val resp = Decoupled(Bits(width = params(HwachaScalarDataBits))).flip
} 

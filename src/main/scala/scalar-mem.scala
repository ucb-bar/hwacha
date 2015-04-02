package hwacha

import Chisel._
import Constants._
import uncore.constants.MemoryOpConstants._

class ScalarMemOp extends Bundle {
  val fn = new Bundle {
    val cmd = Bits(width = M_SZ)
    val mt = Bits(width = MT_SZ)
  }
  val addr = Bits(width = params(HwachaScalarDataBits))
  val data = Bits(width = params(HwachaScalarDataBits))
}

class ScalarMemIO extends Bundle {
  val op = Valid(new ScalarMemOp)
  val xcpt = Bool(INPUT)
  val resp = Decoupled(Bits(width = params(HwachaScalarDataBits))).flip
}

class SMU extends VMUModule {
  val io = new Bundle {
    val inner = new ScalarMemIO().flip
    val outer = new VMUMemIO
    val tbox = new TLBQueryIO

    val active = Bool(OUTPUT)
  }

  private val scalarDataBits = params(HwachaScalarDataBits)

  private val tlb = io.tbox
  private val req = io.outer.req
  private val resp = io.outer.resp

  val op = Reg(new ScalarMemOp)
  val mt = DecodedMemType(op.fn.mt)
  val mt_sel = Seq(mt.b, mt.h, mt.w, mt.d)

  val fn = op.fn.cmd
  val fn_load = (fn === M_XRD)
  val fn_store = (fn === M_XWR)
  val fn_amo = isAMO(fn)
  val fn_get = fn_load || fn_amo
  val fn_put = fn_store || fn_amo

  tlb.vpn.bits := op.addr(vaddrBits-1, pgIdxBits)
  tlb.store := fn_put
  val tlb_ready = tlb.vpn.ready && !tlb.miss
  val xcpt = (fn_get && tlb.xcpt.ld) || (fn_put && tlb.xcpt.st)
  io.inner.xcpt := Bool(false)

  private def fire_req(exclude: Bool, include: Bool*) = {
    val rvs = Seq(tlb_ready, req.ready)
    (rvs.filter(_ != exclude) ++ include).reduce(_ && _)
  }


  val addr = Cat(io.tbox.ppn, op.addr(pgIdxBits-1, 0))
  val shift = addr(tlByteAddrBits-1, 0)
  val mask = Mux1H(mt_sel,
    Seq(Bits(0x00), Bits(0x01), Bits(0x07), Bits(0x7f)))

  req.bits.fn := op.fn.cmd
  req.bits.mt := op.fn.mt
  req.bits.addr := addr
  req.bits.tag := UInt(0) // FIXME?
  req.bits.store.data := op.data << Cat(shift, Bits(0,3))
  req.bits.store.mask := Cat(mask, Bits(1,1)) << shift

  val resp_data = resp.bits.data >> Cat(shift, Bits(0,3))
  val resp_mask = FillInterleaved(SZ_B, mask)
  val resp_extend = Fill(scalarDataBits-SZ_B, !mt.unsigned &&
    Mux1H(mt_sel, Seq(SZ_B, SZ_H, SZ_W, SZ_D).map(i => resp_data(i-1))))

  io.inner.resp.bits := Cat(
    (resp_data(scalarDataBits-1, SZ_B) & resp_mask) |
    (resp_extend & ~resp_mask),
    resp_data(SZ_B-1, 0))


  tlb.vpn.valid := Bool(false)
  req.valid := Bool(false)
  resp.ready := Bool(false)
  io.inner.resp.valid := Bool(false)
  io.active := Bool(true)

  val s_idle :: s_req :: s_resp :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      io.active := Bool(false)
      when (io.inner.op.valid) {
        op := io.inner.op.bits
        state := s_req
      }
    }

    is (s_req) {
      tlb.vpn.valid := fire_req(tlb_ready)
      req.valid := fire_req(req.ready, !xcpt)
      when (fire_req(null)) {
        state := Mux(xcpt, s_idle, s_resp)
        io.inner.xcpt := xcpt
      }
    }

    is (s_resp) {
      resp.ready := io.inner.resp.ready
      io.inner.resp.valid := resp.valid
      when (resp.fire()) {
        state := s_idle
      }
    }
  }
}

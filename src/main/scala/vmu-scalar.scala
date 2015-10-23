package hwacha

import Chisel._
import cde.Parameters

class ScalarMemIO(implicit p: Parameters) extends DecoupledIO(new VMUAuxScalar()(p))

class SMU(implicit p: Parameters) extends VMUModule()(p) {
  val io = new VMUIssueIO {
    val inner = new ScalarMemIO
    val outer = new VMUMemIO
    val tlb = new TLBIO

    val xcpt = new XCPTIO().flip
  }

  val op = Reg(new VMUDecodedOp)
  private val aux = op.aux.scalar()
  private val mts = Seq(op.mt.b, op.mt.h, op.mt.w, op.mt.d)

  private val req = io.outer.req
  private val resp = io.outer.resp

  io.tlb.req.bits.addr := op.base
  io.tlb.req.bits.store := op.cmd.write
  io.tlb.req.bits.mt := op.mt

  val addr = io.tlb.paddr()
  val offset = addr(tlByteAddrBits-1, 0)
  val shift = Cat(offset, UInt(0,3))

  val mask = Mux1H(mts.zipWithIndex.map { case (s, i) =>
    (s, Fill((1 << i) - 1, Bool(true)))
  })

  req.bits.fn := op.fn
  req.bits.addr := addr
  req.bits.mask := Cat(mask, Bits(1,1)) << offset
  req.bits.data := aux.data << shift
  req.bits.last := Bool(true)
  req.bits.pred := Bool(true)
  req.bits.tag := UInt(0)

  val resp_data = resp.bits.data >> shift
  val resp_mask = FillInterleaved(8, mask)
  val resp_sign = Mux1H(mts.zipWithIndex.map { case (s, i) =>
    val w = 1 << (i + 3)
    (s, resp_data(w - 1))
  })
  val resp_extend = Fill(regLen-8, op.mt.signed && resp_sign)

  io.inner.bits.id := aux.id
  io.inner.bits.data := Cat(
    (resp_data(regLen-1, 8) & resp_mask) |
      (resp_extend & (~resp_mask)),
    resp_data(7, 0))

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(io.tlb.req.ready, req.ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.op.ready := Bool(false)
  io.tlb.req.valid := Bool(false)
  req.valid := Bool(false)
  resp.ready := Bool(false)
  io.inner.valid := Bool(false)

  val s_idle :: s_req :: s_resp :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      io.op.ready := Bool(true)
      when (io.op.valid) {
        state := s_req
        op := io.op.bits
      }
    }

    is (s_req) {
      unless (io.xcpt.prop.vmu.stall) {
        io.tlb.req.valid := Bool(true)

        unless (io.tlb.resp.xcpt) {
          req.valid := fire(req.ready)
          when (fire(null)) {
            state := s_resp
          }
        }
      }
    }

    is (s_resp) {
      resp.ready := io.inner.ready
      io.inner.valid := resp.valid

      when (resp.fire()) {
        state := s_idle
      }
    }
  }
}

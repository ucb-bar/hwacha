package hwacha

import Chisel._
import cde.Parameters

class TLBRequest(implicit p: Parameters) extends VMUBundle()(p) {
    val addr = UInt(width = bVAddrExtended)
    val store = Bool()
    val mt = new DecodedMemType
}

class TLBIO(implicit p: Parameters) extends VMUBundle()(p) {
  val req = Decoupled(new TLBRequest)
  val resp = new Bundle {
    val ppn = UInt(INPUT, bPgIdx)
    val xcpt = Bool(INPUT)
  }

  def pgidx(dummy: Int = 0): UInt = this.req.bits.addr(bPgIdx-1, 0)
  def vpn(dummy: Int = 0): UInt = this.req.bits.addr(bVAddrExtended-1, bPgIdx)
  def paddr(dummy: Int = 0): UInt = Cat(this.resp.ppn, this.pgidx())
}

class RTLBIO(implicit p: Parameters) extends VMUBundle()(p) {
  val req = Decoupled(new rocket.TLBReq)
  val resp = new rocket.TLBRespNoHitIndex().flip

  def bridge(client: TLBIO) {
    this.req.bits.vpn := client.vpn()
    this.req.bits.store := client.req.bits.store
    this.req.bits.asid := UInt(0)
    this.req.bits.passthrough := Bool(false)
    this.req.bits.instruction := Bool(false)

    this.req.valid := client.req.valid
    client.req.ready := this.req.ready && !this.resp.miss

    client.resp.ppn := this.resp.ppn
  }
}

class TBox(n: Int)(implicit p: Parameters) extends VMUModule()(p) {
  val io = new Bundle {
    val inner = Vec.fill(n)(new TLBIO().flip)
    val outer = new RTLBIO
    val irq = new IRQIO
  }

  val arb = Wire(new TLBIO())
  io.outer.bridge(arb)

  /* Priority mux */
  arb.req.bits := io.inner.init.foldRight(io.inner.last.req.bits) {
    case (a, b) => Mux(a.req.valid, a.req.bits, b)
  }
  arb.req.valid := io.inner.map(_.req.valid).reduce(_ || _)

  val ready = io.inner.init.map(!_.req.valid).scanLeft(arb.req.ready)(_ && _)
  io.inner.zip(ready).foreach { case (i, r) =>
    i.req.ready := r
    i.resp := arb.resp
  }

  /* Misalignment */
  val mt = arb.req.bits.mt
  val ma = Seq(mt.h, mt.w, mt.d).zipWithIndex.map(x =>
    x._1 && (arb.req.bits.addr(x._2, 0) =/= UInt(0))).reduce(_ || _)

  val write = arb.req.bits.store
  val read = !write

  val xcpts = Seq(
    ma && read,
    ma && write,
    io.outer.resp.xcpt_ld && read,
    io.outer.resp.xcpt_st && write)
  val irqs = Seq(
    io.irq.vmu.ma_ld,
    io.irq.vmu.ma_st,
    io.irq.vmu.faulted_ld,
    io.irq.vmu.faulted_st)

  val fire = arb.req.fire()
  irqs.zip(xcpts).foreach { case (irq, xcpt) =>
    irq := xcpt && fire
  }
  io.irq.vmu.aux := arb.req.bits.addr
  arb.resp.xcpt := xcpts.reduce(_ || _)
}

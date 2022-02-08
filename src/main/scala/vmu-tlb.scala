package hwacha

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket.{TLBReq, TLBResp, MStatus}

abstract class TLBReqIO(implicit p: Parameters) extends VMUBundle()(p) {
  val status = new MStatus().asOutput
  val req = Decoupled(new TLBReq(log2Ceil(regBytes)))
}

class TLBIO(implicit p: Parameters) extends TLBReqIO()(p) {
  val resp = new Bundle {
    val ppn = UInt(INPUT, bPPN)
    val xcpt = Bool(INPUT)
  }

  def pgidx(dummy: Int = 0): UInt = this.req.bits.vaddr(bPgIdx-1, 0)
  def vpn(dummy: Int = 0): UInt = this.req.bits.vaddr(bVAddrExtended-1, bPgIdx)
  def paddr(dummy: Int = 0): UInt = Cat(this.resp.ppn, this.pgidx())
}

class RocketTLBIO(implicit p: Parameters) extends TLBReqIO()(p) {
  val resp = new TLBResp().flip

  def bridge(client: TLBIO) {
    this.status := client.status
    this.req.bits := client.req.bits
    this.req.valid := client.req.valid
    client.req.ready := this.req.ready && !this.resp.miss
    client.resp.ppn := this.resp.paddr(bPAddr-1, bPgIdx)
  }
}

class TBox(n: Int)(implicit p: Parameters) extends VMUModule()(p) {
  val io = new Bundle {
    val inner = Vec(n, new TLBIO()).flip
    val outer = new RocketTLBIO
    val irq = new IRQIO
  }

  val arb = Wire(new TLBIO())
  io.outer.bridge(arb)

  arb.status := PriorityMux(io.inner.map(x => x.req.valid -> x.status))
  arb.req.bits := PriorityMux(io.inner.map(x => x.req.valid -> x.req.bits))
  arb.req.valid := io.inner.map(_.req.valid).reduce(_ || _)

  val ready = io.inner.init.map(!_.req.valid).scanLeft(arb.req.ready)(_ && _)
  io.inner.zip(ready).foreach { case (i, r) =>
    i.req.ready := r
    i.resp.ppn <> arb.resp.ppn
    i.resp.xcpt <> arb.resp.xcpt
  }

  val xcpts = Seq(
    io.outer.resp.ma.ld,
    io.outer.resp.ma.st,
    io.outer.resp.pf.ld,
    io.outer.resp.pf.st,
    io.outer.resp.ae.ld,
    io.outer.resp.ae.st)
  val irqs = Seq(
    io.irq.vmu.ma_ld,
    io.irq.vmu.ma_st,
    io.irq.vmu.pf_ld,
    io.irq.vmu.pf_st,
    io.irq.vmu.ae_ld,
    io.irq.vmu.ae_st)

  val fire = arb.req.fire
  irqs.zip(xcpts).foreach { case (irq, xcpt) =>
    irq := xcpt && fire
  }
  io.irq.vmu.aux := arb.req.bits.vaddr
  arb.resp.xcpt := xcpts.reduce(_ || _)
}

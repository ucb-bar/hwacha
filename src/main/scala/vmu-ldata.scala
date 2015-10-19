package hwacha

import Chisel._

class VLTWIO(implicit val p: Parameters) extends DecoupledIO(new VLTEntry()(p)) with VMUParameters {
  val tag = UInt(INPUT, bTag)
}

class VLTRIO(implicit p: Parameters) extends ValidIO(new VMUBundle()(p) with VMUTag) {
  val meta = new VLTEntry().asInput()
}

class VMLUIO(implicit p: Parameters) extends VMUBundle()(p) {
  val load = Decoupled(new VMLUData)
  val meta = new VLTWIO
}

class VLT(implicit p: Parameters) extends VMUModule()(p) {
  val io = new Bundle {
    val r = new VLTRIO().flip
    val w = new VLTWIO().flip
  }

  val valid = Reg(init = Bits(0, nVLT))
  val table = Mem(new VLTEntry, nVLT)

  io.w.ready := !valid.andR

  private val rtag = io.r.bits.tag
  private val wtag = io.w.tag
  wtag := PriorityEncoder(~valid)

  val wen = io.w.fire()
  val ren = io.r.valid
  val valid_mask_r = (Bits(1) << rtag) & Fill(nVLT, ren)
  val valid_mask_w = (Bits(1) << wtag) & Fill(nVLT, wen)
  valid := (valid & (~valid_mask_r)) | valid_mask_w

  assert(!ren || valid(rtag), "VLT: invalid read tag")

  io.r.meta := table(rtag)
  when (wen) {
    table(io.w.tag) := io.w.bits
  }
}

class LBox(implicit p: Parameters)  extends VMUModule()(p) {
  val io = new Bundle {
    val mem = new VMLUIO().flip
    val lane = new VLDQIO
  }

  val vldq = Module(new Queue(io.lane.bits, nVLDQ))
  val vlt = Module(new VLT)

  vlt.io.w <> io.mem.meta
  vlt.io.r.bits.tag := io.mem.load.bits.tag
  vlt.io.r.valid := io.mem.load.fire()

  vldq.io.enq.bits.data := io.mem.load.bits.data
  vldq.io.enq.bits.meta := vlt.io.r.meta
  vldq.io.enq.bits.meta.last := io.mem.load.bits.last
  vldq.io.enq.valid := io.mem.load.valid
  io.mem.load.ready := vldq.io.enq.ready

  io.lane <> vldq.io.deq
}

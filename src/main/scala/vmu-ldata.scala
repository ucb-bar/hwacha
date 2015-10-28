package hwacha

import Chisel._
import cde.Parameters

class TableWIO[T <: Data](gen: => T, sztag: Int)
  extends DecoupledIO(gen) {
  val tag = UInt(INPUT, sztag)
}

class TableRIO[T <: Data](gen: => T, sztag: Int)
  extends ValidIO(UInt(width = sztag)) {
  val record = gen.asInput()
}

class Table[T <: Data](n: Int, gen: => T) extends Module {
  private val sztag = log2Up(n)
  val io = new Bundle {
    val r = new TableRIO(gen, sztag).flip
    val w = new TableWIO(gen, sztag).flip
  }

  val valid = Reg(init = Bits(0, n))
  val array = Mem(gen, n)

  io.w.ready := !valid.andR

  private val rtag = io.r.bits
  private val wtag = io.w.tag
  wtag := CTZ(~valid, n)

  val wen = io.w.fire()
  val ren = io.r.valid
  val valid_mask_r = ren << rtag
  val valid_mask_w = wen << wtag
  valid := (valid & (~valid_mask_r)) | valid_mask_w

  assert(!ren || valid(rtag), "table: invalid read tag")

  io.r.record := array(rtag)
  when (wen) {
    array(io.w.tag) := io.w.bits
  }
}

class VMLUIO(implicit p: Parameters) extends VMUBundle()(p) {
  val load = Decoupled(new VMLUData)
  val meta = new TableWIO(new VLTEntry, bVMUTag)
}

class LBox(implicit p: Parameters)  extends VMUModule()(p) {
  val io = new Bundle {
    val mem = new VMLUIO().flip
    val lane = new VLDQIO
  }

  val vldq = Module(new Queue(io.lane.bits, nVLDQ))
  val vlt = Module(new Table(nVLT, new VLTEntry))

  vlt.io.w <> io.mem.meta
  vlt.io.r.bits := io.mem.load.bits.tag
  vlt.io.r.valid := io.mem.load.fire()

  vldq.io.enq.bits.data := io.mem.load.bits.data
  vldq.io.enq.bits.meta := vlt.io.r.record
  vldq.io.enq.valid := io.mem.load.valid
  io.mem.load.ready := vldq.io.enq.ready

  io.lane <> vldq.io.deq
}

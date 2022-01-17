package hwacha

import Chisel._
import freechips.rocketchip.config._

class TableWIO[T <: Data](gen: => T, sztag: Int)
  extends DecoupledIO(gen) {
  val tag = UInt(INPUT, sztag)

}

class TableRIO[T <: Data](gen: => T, sztag: Int)
  extends ValidIO(UInt(width = sztag)) {
  val record = gen.asInput

}

class Table[T <: Data](n: Int, gen: => T) extends Module {
  private val sztag = log2Up(n)
  val io = new Bundle {
    val r = new TableRIO(gen, sztag).flip
    val w = new TableWIO(gen, sztag).flip
  }

  val valid = Reg(init = Bits(0, n))
  val array = Mem(n, gen)

  io.w.ready := !valid.andR

  private val rtag = io.r.bits
  private val wtag = io.w.tag
  wtag := CTZ(~valid, n)

  val wen = io.w.fire
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

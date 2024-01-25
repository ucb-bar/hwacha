package hwacha

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

class TableWIO[T <: Data](gen: => T, sztag: Int)
  extends DecoupledIO(gen) {
  val tag = Input(UInt(sztag.W))

}

class TableRIO[T <: Data](gen: => T, sztag: Int)
  extends ValidIO(UInt(sztag.W)) {
  val record = Input(gen)

}

class Table[T <: Data](n: Int, gen: => T) extends Module {
  private val sztag = log2Up(n)
  val io = IO(Flipped(new Bundle {
    val r = new TableRIO(gen, sztag)
    val w = new TableWIO(gen, sztag)
  }))

  val valid = RegInit(0.U(n.W))
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

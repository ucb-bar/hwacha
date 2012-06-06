package hwacha

import Chisel._
import Node._
import scala.collection.mutable.ArrayBuffer
import scala.math._

object Match
{
  def apply(x: Bits, IOs: Bits*) =
  {
    val ioList = IOs.toList
    var offset = 0
    for (io <- IOs.toList.reverse)
    {
      io := x(offset+io.width-1, offset)
      offset += io.width
    }
  }
}

class Mux1H [T <: Data](n: Int)(gen: => T) extends Component
{
  val io = new Bundle {
    val sel = Vec(n) { Bool(dir = INPUT) }
    val in  = Vec(n) { gen }.asInput
    val out = gen.asOutput
  }

  io.out := Mux1H(io.sel, io.in)
}

class CoarseRRArbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioArbiter(n)(data)

  val last_grant = Reg(resetVal = Bits(0, log2Up(n)))
  val g = ArbiterCtrl((0 until n).map(i => io.in(i).valid && UFix(i) >= last_grant) ++ io.in.map(_.valid))
  val grant = (0 until n).map(i => g(i) && UFix(i) >= last_grant || g(i+n))
  (0 until n).map(i => io.in(i).ready := grant(i) && io.out.ready)

  var choose = Bits(n-1)
  for (i <- n-2 to 0 by -1)
    choose = Mux(io.in(i).valid, Bits(i), choose)
  for (i <- n-1 to 0 by -1)
    choose = Mux(io.in(i).valid && UFix(i) >= last_grant, Bits(i), choose)
  when (Reg(io.out.valid) && !io.out.valid && io.out.ready) {
    last_grant := choose
  }

  val dvec = Vec(n) { data }
  (0 until n).map(i => dvec(i) := io.in(i).bits )

  io.out.valid := foldR(io.in.map(_.valid))(_||_)
  io.out.bits := dvec(choose)
  io.chosen := choose
}

class maskstall[T <: Data](data: => T) extends Component
{
  val io = new Bundle()
  {
    val input = (new ioDecoupled()){ data }.flip
    val output = (new ioDecoupled()){ data }
    val stall = Bool(INPUT)
  }

  io.output.valid := io.input.valid && !io.stall
  io.output.bits := io.input.bits
  io.input.ready := io.output.ready && !io.stall
}

object MaskStall
{
  def apply[T <: Data](deq: ioDecoupled[T], stall: Bool) =
  {
    val ms = new maskstall( deq.bits.clone )
    ms.io.input <> deq
    ms.io.stall := stall
    ms.io.output
  }
}

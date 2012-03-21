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

object Reverse
{
  def apply(in: Bits): Bits =
  {
    var res = in(0)
    for(i <- 1 until 64)
      res = Cat(res, in(i))
    res
  }
}

object ShiftRegister
{
  def apply(n: Int, width: Int, valid: Bool, base: Bits): Bits =
  {
    if (n == 0)
    {
      val res = Reg() { Bits(width = width) }
      when (valid)
      {
        res := base
      }
      res
    }
    else
    {
      Reg(apply(n-1, width, valid, base))
    }
  }
}

object foldR
{
  def apply[T <: Bits](x: Seq[T])(f: (T, T) => T): T =
    if (x.length == 1) x(0) else f(x(0), foldR(x.slice(1, x.length))(f))
}

object log2up
{
  def apply(x: Int) = if (x == 1) 1 else ceil(log(x)/log(2.0)).toInt
}

object log2down
{
  def apply(x : Int) = if (x == 1) 1 else floor(log(x)/log(2.0)).toInt
}

object isPow2
{
  def apply(x: Int) = (x == pow(2,log2down(x)))
}

object UFixToOH
{
  def apply(in: UFix, width: Int): Bits =
  {
    val out = Bits(1, width)
    (out << in)(width-1,0)
  }
}

object Mux1H 
{
  def buildMux[T <: Data](sel: Bits, in: Vec[T], i: Int, n: Int): T = {
    if (n == 1)
      in(i)
    else
    {
      val half_n = (1 << log2up(n))/2
      val left = buildMux(sel, in, i, half_n)
      val right = buildMux(sel, in, i + half_n, n - half_n)
      Mux(sel(i+n-1,i+half_n).orR, right, left)
    }
  }

  def apply [T <: Data](sel: Bits, in: Vec[T]): T = buildMux(sel, in, 0, sel.getWidth)
  def apply [T <: Data](sel: Vec[Bool], in: Vec[T]): T = apply(sel.toBits, in)
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

class ioArbiter[T <: Data](n: Int)(data: => T) extends Bundle {
  val in  = Vec(n) { (new ioDecoupled()) { data } }.flip
  val out = (new ioDecoupled()) { data }
  val chosen = Bits(log2up(n), OUTPUT)
}

object ArbiterCtrl
{
  def apply(request: Seq[Bool]) = {
    Bool(true) +: (1 until request.length).map(i => !foldR(request.slice(0, i))(_||_))
  }
}

class Arbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioArbiter(n)(data)

  val grant = ArbiterCtrl(io.in.map(_.valid))
  (0 until n).map(i => io.in(i).ready := grant(i) && io.out.ready)

  var dout = io.in(n-1).bits
  var choose = Bits(n-1)
  for (i <- n-2 to 0 by -1) {
    dout = Mux(io.in(i).valid, io.in(i).bits, dout)
    choose = Mux(io.in(i).valid, Bits(i), choose)
  }

  io.out.valid := foldR(io.in.map(_.valid))(_||_)
  io.out.bits <> dout
  io.chosen := choose
}

class RRArbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioArbiter(n)(data)

  val last_grant = Reg(resetVal = Bits(0, log2up(n)))
  val g = ArbiterCtrl((0 until n).map(i => io.in(i).valid && UFix(i) > last_grant) ++ io.in.map(_.valid))
  val grant = (0 until n).map(i => g(i) && UFix(i) > last_grant || g(i+n))
  (0 until n).map(i => io.in(i).ready := grant(i) && io.out.ready)

  var choose = Bits(n-1)
  for (i <- n-2 to 0 by -1)
    choose = Mux(io.in(i).valid, Bits(i), choose)
  for (i <- n-1 to 1 by -1)
    choose = Mux(io.in(i).valid && UFix(i) > last_grant, Bits(i), choose)
  when (io.out.valid && io.out.ready) {
    last_grant := choose
  }

  val dvec = Vec(n) { Wire() { data } }
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

class maskstall2[T <: Data](data: => T) extends Component
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

object MaskStall2
{
  def apply[T <: Data](deq: ioDecoupled[T], stall: Bool) =
  {
    val ms = new maskstall2( deq.bits.clone )
    ms.io.input <> deq
    ms.io.stall := stall
    ms.io.output
  }
}

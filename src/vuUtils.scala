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

object log2up
{
  def apply(in: Int) = if (in == 1) 1 else ceil(log(in)/log(2)).toInt
}

object UFixToOH
{
  def apply(in: UFix, width: Int): Bits =
  {
    val out = Bits(1, width)
    (out << in)(width-1,0)
  }
}

class Mux1H_(n: Int, w: Int) extends Component
{
  val io = new Bundle {
    val sel = Vec(n) { Bool(dir = INPUT) }
    val in = Vec(n) { Bits(width = w, dir = INPUT) }
    val out = Bits(width = w, dir = OUTPUT)
  }

  if (n > 1) {
    var out = io.in(0) & Fill(w, io.sel(0))
    for (i <- 1 to n-1)
      out = out | (io.in(i) & Fill(w, io.sel(i)))
    io.out := out
  } else {
    io.out := io.in(0)
  }
}

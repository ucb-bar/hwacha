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

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

class CoarseRRArbiter[T <: Data](n: Int)(data: => T) extends Module {
  val io = new ArbiterIO(data, n)

  val last_grant = RegReset(Bits(0, log2Up(n)))
  val g = ArbiterCtrl((0 until n).map(i => io.in(i).valid && UInt(i) >= last_grant) ++ io.in.map(_.valid))
  val grant = (0 until n).map(i => g(i) && UInt(i) >= last_grant || g(i+n))
  (0 until n).map(i => io.in(i).ready := grant(i) && io.out.ready)

  var choose = Bits(n-1)
  for (i <- n-2 to 0 by -1)
    choose = Mux(io.in(i).valid, Bits(i), choose)
  for (i <- n-1 to 0 by -1)
    choose = Mux(io.in(i).valid && UInt(i) >= last_grant, Bits(i), choose)
  when (RegUpdate(io.out.valid) && !io.out.valid && io.out.ready) {
    last_grant := choose
  }

  val dvec = Vec.fill(n){data}
  (0 until n).map(i => dvec(i) := io.in(i).bits )

  io.out.valid := foldR(io.in.map(_.valid))(_||_)
  io.out.bits := dvec(choose)
  io.chosen := choose
}

class MaskStall[T <: Data](data: => T) extends Module
{
  val io = new Bundle()
  {
    val input = Decoupled(data).flip
    val output = Decoupled(data)
    val stall = Bool(INPUT)
  }

  io.output.valid := io.input.valid && !io.stall
  io.output.bits := io.input.bits
  io.input.ready := io.output.ready && !io.stall
}

object MaskStall
{
  def apply[T <: Data](deq: DecoupledIO[T], stall: Bool) =
  {
    val ms = Module(new MaskStall(deq.bits.clone))
    ms.io.input <> deq
    ms.io.stall := stall
    ms.io.output
  }
}

class MaskReady[T <: Data](data: => T) extends Module
{
  val io = new Bundle()
  {
    val input = Decoupled(data).flip
    val output = Decoupled(data)
    val ready = Bool(INPUT)
  }

  io.output.valid := io.input.valid
  io.output.bits := io.input.bits
  io.input.ready := io.output.ready && io.ready
}

object MaskReady
{
  def apply[T <: Data](deq: DecoupledIO[T], ready: Bool) =
  {
    val mr = Module(new MaskReady(deq.bits.clone))
    mr.io.input <> deq
    mr.io.ready := ready
    mr.io.output
  }
}

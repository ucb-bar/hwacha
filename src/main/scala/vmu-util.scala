package hwacha

import Chisel._

// Bidirectional barrel shifter
// Selects n-element field from 2n-element input
class FunnelShifter[T <: Data](gen: T, n: Int) extends Module {
  private val lgn = log2Up(n)
  require(n == (1 << lgn))

  val io = new Bundle {
    val in0 = Vec(n, gen.cloneType).asInput
    val in1 = Vec(n, gen.cloneType).asInput // left-shift input
    val out = Vec(n, gen.cloneType).asOutput
    val shift = SInt(INPUT, lgn + 1)
  }

  // Right shift by n
  private var data = Vec((1 until n).map(i =>
    Mux(io.shift(lgn), io.in0(i), io.in1(i))) ++ io.in0)

  // Left shift by (n - 1) .. 0
  for (stage <- (lgn - 1) to 0 by -1) {
    val m = (0 until stage).map(1 << _).sum
    val k = 1 << stage
    data = Vec.tabulate(n + m){ i =>
      Mux(io.shift(stage), data(i), data(i + k))
    }
  }
  io.out := data
}

// Rotates n input elements into m output slots
class Rotator[T <: Data](gen: T, n: Int, m: Int, rev: Boolean = false) extends Module {
  require(n <= m)
  val io = new Bundle {
    val in = Vec(n, gen.cloneType).asInput
    val out = Vec(m, gen.cloneType).asOutput
    val sel = UInt(INPUT, log2Up(m))
  }

  var barrel = io.in
  for (stage <- 0 until log2Up(m)) {
    val shift = 1 << stage
    val len = math.min(barrel.length + shift, m)
    barrel = Vec.tabulate(len){ i => {
      // k: source index with rotation enabled
      // i: source index with rotation disabled
      val k = if (rev) (i + shift) % m // shift backward
      else ((i - shift) + m) % m // shift forward
        if (i < barrel.length && k < barrel.length) {
          Mux(io.sel(stage), barrel(k), barrel(i))
        } else {
          // If either entry does not exist, use the other.
          if (i < barrel.length) barrel(i) else barrel(k)
        }
    }}
  }
  io.out := barrel
}

object EnableDecoder {
  def apply[T <: UInt](in: T, n: Int): UInt = {
    val lgn = log2Up(n)
    val lut = Vec(
      (0 until n).map(i => Bits((1 << i) - 1, n)) ++
      Seq.fill((1 << lgn) - n)(Fill(n, Bool(true))))
    val mask = ((in >> lgn) =/= UInt(0))
    lut(in(lgn-1, 0)) | Fill(n, mask)
  }
}

object Ceil {
  def apply[T <: UInt](in: T, shift: Int): UInt =
    if (shift == 0) in else
      ((in >> shift) + in(shift-1, 0).orR.asUInt)
}

/* Count trailing zeroes */
object CTZ {
  private def mux[T <: Data](in: Seq[(Bool, T)]): (Bool, T) = {
    /* Returns the last (lowest-priority) item if none are selected */
    val elt = in.init.foldRight(in.last._2) {
      case ((sel, elt0), elt1) => Mux(sel, elt0, elt1)
    }
    val sel = in.map(_._1).reduce(_ || _)
    (sel, elt)
  }

  private def tree[T <: Data](in: Seq[(Bool, T)]): Seq[(Bool, T)] = {
    val stage = in.grouped(2).map(mux(_)).toSeq
    if (stage.size > 1) tree(stage) else stage
  }

  def apply[T <: Bits](in: T, n: Int): UInt = {
    val init = (0 until n).map(i => (in(i), UInt(i))) :+
      (Bool(true), UInt(n)) /* Result for zero input */
    tree(init).head._2
  }
}

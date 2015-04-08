package hwacha

import Chisel._

// Bidirectional barrel shifter
// Selects n-element field from 2n-element input
class FunnelShifter[T <: Data](gen: T, n: Int) extends Module {
  private val lgn = log2Up(n)
  require(n == (1 << lgn))

  val io = new Bundle {
    val in0 = Vec.fill(n){ gen.clone.asInput }
    val in1 = Vec.fill(n){ gen.clone.asInput } // left-shift input
    val out = Vec.fill(n){ gen.clone.asOutput }
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
    val in = Vec.fill(n){ gen.clone.asInput }
    val out = Vec.fill(m){ gen.clone.asOutput }
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
  def apply[T <: Data](in: T, n: Int) = {
    val out = Vec.fill(n)(Bool())
    val sel = (n until 0 by -1).map(i => (in === Bits(i)))
    out := Vec((0 until n).map(i => sel.take(sel.size - i).reduce(_||_)))
    out
  }
}

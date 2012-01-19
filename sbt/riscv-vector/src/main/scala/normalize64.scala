package Fpu {

import Chisel._
import Node._;

import normalize64._;
import scala.collection.mutable.ArrayBuffer;

class normalize64_io(width: Int, num_bits: Int) extends normalize_io {
  override val in       = Bits(width, INPUT);
  override val distance = Bits(num_bits, OUTPUT);
  override val out      = Bits(width, OUTPUT);
}

object normalize64 {
  //def apply() = { val c = new normalize64(); withComp(c, i => c.init()); c }
  def fold(buf: ArrayBuffer[Bits], fun: (Bits, Bits) => Bits) = {
    var res = buf(0);
    for (i <- 1 until buf.length)
      res = fun(res, buf(i))
    res
  }
}

class normalize64(width: Int = 64, num_bits: Int = 6) extends Component {
  override val io = new normalize64_io(width, num_bits);

  var distances = ArrayBuffer[Bits]();
  var norms     = ArrayBuffer[Bits]();
  var shift_bits = width;

  norms += io.in;

  for (i <- 0 until num_bits) {
    shift_bits >>= 1; // Divide by 2
    distances += norms.last(width - 1, width - shift_bits) === Bits(0, shift_bits);
    norms     += Mux(distances.last, norms.last << UFix(shift_bits), norms.last);
  }

  io.out      := norms.last(width-1,0);

  io.distance := fold(distances, Cat(_, _));
  
}
}

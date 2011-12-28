package Fpu {

import Chisel._
import Node._;

class normalize_io extends Bundle() {
  val in       = Bits('input);
  val distance = Bits('output);
  val out      = Bits('output);
}

class normalize32_io extends normalize_io {
  override val in       = Bits(32, 'input);
  override val distance = Bits(5, 'output);
  override val out      = Bits(32, 'output);
}

class normalize32 extends Component {
  override val io: normalize32_io = new normalize32_io();
  val dist_4  = io.in(31, 16) === Bits(0, 16);
  val norm_16 = Mux(dist_4, io.in << UFix(16), io.in);
  val dist_3  = norm_16(31, 24) === Bits(0, 8);
  val norm_8  = Mux(dist_3, norm_16 << UFix(8), norm_16);
  val dist_2  = norm_8(31, 28) === Bits(0, 4);
  val norm_4  = Mux(dist_2, norm_8 << UFix(4), norm_8);
  val dist_1  = norm_4(31, 30) === Bits(0, 2);
  val norm_2  = Mux(dist_1, norm_4 << UFix(2), norm_4);
  val dist_0  = norm_2(31) === Bits(0, 1);
  io.out      := Mux(dist_0, norm_2 << UFix(1), norm_2)(31,0);
  io.distance := Cat(dist_4, dist_3, dist_2, dist_1, dist_0);
}
}


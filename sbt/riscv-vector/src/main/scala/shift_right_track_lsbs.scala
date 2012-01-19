package Fpu {

import Chisel._
import Node._;

import scala.collection.mutable.ArrayBuffer;

class shift_right_track_lsbs_io(
	DATA_WIDTH: Int,
	SHIFT_BITS: Int) extends Bundle {
  val do_shift = Bits(1, INPUT);
  val in       = Bits(DATA_WIDTH, INPUT);
  val in_lsb   = Bits(1, INPUT);
  val out      = Bits(DATA_WIDTH, OUTPUT);
  val out_lsb  = Bits(1, OUTPUT);
}

class shift_right_track_lsbs(
		DATA_WIDTH: Int = 64,
		SHIFT_BITS: Int = 6) extends Component {
  override val io = new shift_right_track_lsbs_io(DATA_WIDTH, SHIFT_BITS);

  io.out := Mux(io.do_shift,
	Cat(Fill(SHIFT_BITS, Bits(0, 1)), io.in(DATA_WIDTH-1, SHIFT_BITS)),
	io.in);

  io.out_lsb := Mux(io.do_shift,
	io.in_lsb.toBool || (io.in(SHIFT_BITS-1, 0) != Bits(0, SHIFT_BITS)),
	io.in_lsb);
}
}

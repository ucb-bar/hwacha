package Fpu {

import Chisel._
import Node._;
import rF32_rF64._;

class rF32_rF64_io extends Bundle() {
  val in  = Bits(IN_BITS, 'input);
  val out = Bits(OUT_BITS, 'output);
  val exception_flags = Bits(5, 'output);
}
object rF32_rF64 { 
  val IN_EXP_BITS  = 9;
  val IN_SIG_BITS  = 23;
  val OUT_EXP_BITS = 12;
  val OUT_SIG_BITS = 52;

  val IN_BITS      = IN_EXP_BITS + IN_SIG_BITS + 1;
  val OUT_BITS     = OUT_EXP_BITS + OUT_SIG_BITS + 1;
}

class rF32_rF64 extends Component {
  override val io = new rF32_rF64_io();
  val sign             = io.in(IN_BITS-1);
  val exponent_in      = io.in(IN_BITS-2,IN_SIG_BITS);
  val exponent_code    = exponent_in(IN_EXP_BITS-1,IN_EXP_BITS-3);
  val is_signaling_nan = exponent_code === Bits("b111",3) && io.in(IN_SIG_BITS-1) === Bits(0,1);
  val sig_in           = io.in(IN_SIG_BITS-1,0);

  val exponent_extended =
    Mux(exponent_code === Bits("b000",3), Bits("b0",12),
    Mux(exponent_code === Bits("b001",3), Cat(Bits("b0111",4), exponent_in(IN_EXP_BITS-2,0)),
    Mux(exponent_code === Bits("b010",3), Cat(Bits("b0111",4), exponent_in(IN_EXP_BITS-2,0)),
    Mux(exponent_code === Bits("b011",3), Cat(Bits("b0111",4), exponent_in(IN_EXP_BITS-2,0)),
    Mux(exponent_code === Bits("b100",3), Cat(Bits("b1000",4), exponent_in(IN_EXP_BITS-2,0)),
    Mux(exponent_code === Bits("b101",3), Cat(Bits("b1000",4), exponent_in(IN_EXP_BITS-2,0)),
    Mux(exponent_code === Bits("b110",3), Bits("b110000000000",12),
    Mux(exponent_code === Bits("b111",3), Bits("b111000000000",12),
    Bits(0,12)))))))));

  io.out := Cat(sign, exponent_extended,
                Mux(is_signaling_nan, Bits(1,1), sig_in(IN_SIG_BITS-1)),
                sig_in(IN_SIG_BITS-2,0),
                Fill(OUT_SIG_BITS-IN_SIG_BITS, Bits(0,1)));

  io.exception_flags := Cat(is_signaling_nan, Bits(0,4));
}

}

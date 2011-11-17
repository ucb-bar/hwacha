package Fpu {

import Chisel._
import Node._;

class recodedFloat32Compare_io(SIG_WIDTH: Int, EXP_WIDTH: Int) extends Bundle {
  val a              = Bits(SIG_WIDTH + EXP_WIDTH + 1, 'input);
  val b              = Bits(SIG_WIDTH + EXP_WIDTH + 1, 'input);
  val a_eq_b         = Bool('output);
  val a_lt_b         = Bool('output);
  val a_eq_b_invalid = Bool('output);
  val a_lt_b_invalid = Bool('output);
}


class recodedFloat32Compare(SIG_WIDTH: Int = 23, EXP_WIDTH: Int = 9) extends Component {
  override val io = new recodedFloat32Compare_io(SIG_WIDTH, EXP_WIDTH);

	val FLOAT_WIDTH = SIG_WIDTH + EXP_WIDTH + 1;
        //
        // Break out Cat(sign, exponent, significand) for inputs
        //
        val a_sign = io.a(FLOAT_WIDTH-1);
        val a_exp  = io.a(FLOAT_WIDTH-2,SIG_WIDTH).toUFix;
        val a_sig  = io.a(SIG_WIDTH-1,0).toUFix;

        val b_sign = io.b(FLOAT_WIDTH-1);
        val b_exp  = io.b(FLOAT_WIDTH-2,SIG_WIDTH).toUFix;
        val b_sig  = io.b(SIG_WIDTH-1,0).toUFix;

        val a_code = io.a(FLOAT_WIDTH-2,FLOAT_WIDTH-4);
        val b_code = io.b(FLOAT_WIDTH-2,FLOAT_WIDTH-4);

        //
        // Compare Cat(sign, exponent, significand) separately
        //
        val sign_equal =  (a_sign === b_sign);
        val exp_equal  =  (a_exp  === b_exp);
        val sig_equal  =  (a_sig  === b_sig);

        val exp_a_lt_exp_b = (a_exp < b_exp);
        val exp_a_gt_exp_b = ~exp_a_lt_exp_b && ~exp_equal;
        val sig_a_lt_sig_b = (a_sig < b_sig);
        val sig_a_gt_sig_b = ~sig_a_lt_sig_b && ~sig_equal;

        //
        // Special case checks
        //
        val neg_zero_eq    = (a_code === Bits("b000",3) && b_code === Bits("b000",3) && ~sign_equal);
        val a_or_b_is_nan  = (a_code === Bits("b111",3) || b_code === Bits("b111",3));
        val signalling_nan = Cat(a_code,io.a(SIG_WIDTH-1)) === Bits("b1110",4) ||
                             Cat(b_code,io.b(SIG_WIDTH-1)) === Bits("b1110",4);

        //
        // Equality test.  Special cases include:
        //       -0 === +0  => true
        //      NaN === NaN => false
        //
        io.a_eq_b :=          Mux(neg_zero_eq, Bool(true),
			      Mux(~sign_equal, Bool(false),
                              Mux(~exp_equal,  Bool(false),
                              Mux(~sig_equal,  Bool(false),
                              Mux(a_code === Bits("b111",3), Bool(false),
                              Bool(true))))));

        //
        // Less-Than test.  Special cases include:
        //      NaN < any => false
        //      any < NaN => false
        //      -0  < +0  => false
        //  +0  < -0  => false
        //
        io.a_lt_b :=         Mux(a_or_b_is_nan, Bool(false),
                             Mux(neg_zero_eq, Bool(false),
                             Mux(a_sign === Bits("b0",1) && b_sign === Bits("b0",1),
			     Mux(exp_equal, sig_a_lt_sig_b, exp_a_lt_exp_b),
                             Mux(a_sign === Bits("b1",1) && b_sign === Bits("b0",1), Bool(true),
                             Mux(a_sign === Bits("b0",1) && b_sign === Bits("b1",1), Bool(false),
                             Mux(a_sign === Bits("b1",1) && b_sign === Bits("b1",1),
                             Mux(exp_equal, sig_a_gt_sig_b, exp_a_gt_exp_b),
			     Bool(false)))))));

        io.a_eq_b_invalid := signalling_nan;
        io.a_lt_b_invalid := a_or_b_is_nan;
}
}

//
// recodedFloat64ToRecodedFloat32( in, out );
// Author: Brian Richards, 11/8/2010
// Revised: Brian Richards, 7/8/2011:
//   Bitsed rounding modes, replaced subblocks with shift ops.
//
package Fpu {

import Chisel._;
import Node._;
import LitConv._;
import fpu_recoded._;
import scala.math._;
import recodedFloat64ToRecodedFloat32._;
import shift_round_position._;

class recodedFloat64ToRecodedFloat32_io() extends Bundle {
  val in             = Bits(65, INPUT);
  val roundingMode   = Bits(2, INPUT);
  val out            = Bits(33, OUTPUT);
  val exceptionFlags = Bits(5, OUTPUT);
}

object recodedFloat64ToRecodedFloat32 {

  val max_f32_exp       = Bits("h87f", 12).toUFix
  val min_f32_exp       = Bits("h76a", 12).toUFix
  val underflow_f32_exp = Bits("h782", 12).toUFix
  val max_subnormal_exp = Bits("h781", 12).toUFix// -127, 23.1 digits, round_shift = 0
  val min_subnormal_exp = Bits("h76a", 12).toUFix// -150,  0.1 digits, round_shift = 23

  val zero_f32_exp      =  Bits("h000", 9);
  val zero_f32_sig      = Bits("h000000", 23);
  val small_f32_exp     =  Bits("h06b", 9);
  val small_f32_sig     = Bits("h000000", 23);
  val big_f32_exp       =  Bits("h17f", 9);
  val big_f32_sig       = Bits("h7fffff", 23);
  val inf_f32_exp       =  Bits("h180", 9);
  val inf_f32_sig       = Bits("h000000", 23);

  val IN_SIG_BITS	= 52; 
  val OUT_SIG_BITS	= 23;
  val SHIFT_WIDTH 	= OUT_SIG_BITS+1;
  val STAGES		= ceil(log10(OUT_SIG_BITS)/log10(2)).toInt;
}

class recodedFloat64ToRecodedFloat32 extends Component{

  override val io = new recodedFloat64ToRecodedFloat32_io();

  // Arrays for storing intermediate values between rounding stages.
  val sig_masked_vector  = new Array[Bits](STAGES+1);
  val sig_shifted_vector = new Array[Bits](STAGES+1);
  val sig_sticky_vector  = new Array[Bits](STAGES+1);

  // Break the input f64 into fields:
  val sign           = io.in(64);
  val exponent_in    = io.in(63,52).toUFix;
  val sig_in         = io.in(51,0).toUFix;

  // Determine the type of float from the coded exponent bits:
  val exponentIsSubnormal = ((exponent_in > min_subnormal_exp || exponent_in === min_subnormal_exp)  &&
  		     (exponent_in < max_subnormal_exp || exponent_in === max_subnormal_exp));
  //val exponentIsSubnormal = exponent_in >= min_subnormal_exp &&
  //		     exponent_in <= max_subnormal_exp;
  //val exponentIsSubnormal = ~(exponent_in < min_subnormal_exp) &&
  //		     ~(exponent_in > max_subnormal_exp);
  val exponentSpecial     = (exponent_in(11,9) === Bits("b000",3) || 
      			     exponent_in(11,10) === Bits("b11",2));
  val isNaN               = (exponent_in(11,9) === Bits("b111",3));
  val isInvalidNaN        = isNaN && sig_in(51) != Bits("b1",1);
  val exponentUnderflow   = (exponent_in < min_f32_exp && !exponentSpecial);
  val exponentOverflow    = (exponent_in > max_f32_exp && !exponentSpecial);
  val exponentInRange     = (!exponentUnderflow && !exponentOverflow);

  // For the recoded float representation, the significand must be
  // rounded to bit positions that depend on the exponent.
  val round_position =
	Mux(exponentIsSubnormal, (max_subnormal_exp.toUFix + UFix(1, 12) - exponent_in.toUFix)(STAGES-1,0),
				 Bits(0, STAGES));

  // Normally, round off to most-significant 23 bits (51,29), and track
  // half-digit in bit 28 and remaining sticky bits.
  val sigMSBs           = sig_in(51,28);
  val sigMSBsShifted    = Cat(Bits("b1",1), sigMSBs, Fill(SHIFT_WIDTH, Bits("b0",1))) >> round_position.toUFix;
  val subNormStickyBit  = (sigMSBsShifted(SHIFT_WIDTH-1,0) != Fill(SHIFT_WIDTH, Bits("b0", 1))) ||
			  (sig_in(27,0) != Bits("b0", 28));
  val roundBits = Cat(sigMSBsShifted(SHIFT_WIDTH+1,SHIFT_WIDTH), subNormStickyBit);

  val roundInexact = (roundBits(1,0) != Bits(0,2) && !exponentSpecial);
  // Determine the rounding increment, based on the rounding mode.
  val roundEvenOffset = (roundBits(1,0) === Bits("b11",2) || roundBits(2,1) === Bits("b11",2));
  val roundOffset =
	Mux(io.roundingMode === round_nearest_even, roundEvenOffset,
	Mux(io.roundingMode === round_minMag,       Bits(0, 1),
	Mux(io.roundingMode === round_min,          Mux(sign & roundInexact, Bits(1, 1), Bits(0, 1)),
	Mux(io.roundingMode === round_max,          Mux(~sign & roundInexact, Bits(1, 1), Bits(0, 1)),
	Bits(0, 1)))));

  // Round the significand, and increment the exponent if the
  // Prepend an extra bit and the implicit 1 to the
  // significand, mask unusable subnormal bits, round
  // the significand, and increment the exponent if the
  // significand overflows.
  val roundMaskShifted   = (Fill(SHIFT_WIDTH+1,Bits("b1",1)) << round_position.toUFix)(SHIFT_WIDTH,0);
  val sig_pre_round      = Cat(Bits("b01", 2), sig_in(51,29)).toUFix;
  val sig_round          = (sig_pre_round | ~roundMaskShifted.toUFix) + roundOffset.toUFix;
  val exp_round          = exponent_in.toUFix - Mux(sig_round(24), Bits("h6ff",12), Bits("h700", 12)).toUFix;

  // Pre-calculate overflow and underflow values.
  val underflow_to_small = (io.roundingMode === round_min && sign === Bits("b1",1)) ||
			   (io.roundingMode === round_max && sign === Bits("b0",1));
  val overflow_to_inf    = underflow_to_small || (io.roundingMode === round_nearest_even);

  val sig_overflow_val   = Mux(overflow_to_inf,    inf_f32_sig,   big_f32_sig);
  val exp_overflow_val   = Mux(overflow_to_inf,    inf_f32_exp,   big_f32_exp);
  val sig_underflow_val  = Mux(underflow_to_small, small_f32_sig, zero_f32_sig);
  val exp_underflow_val  = Mux(underflow_to_small, small_f32_exp, zero_f32_exp);

  // Assemble the recoded f32 exponent.
  val exponent_out =
	Mux(exponentSpecial,   Cat(exponent_in(11,9), Bits(0,6)),
	Mux(exponentInRange,   exp_round(8,0),
	Mux(exponentOverflow,  exp_overflow_val,
	Mux(exponentUnderflow, exp_underflow_val,
	Bits(0,9)))));

  // Assemble the recoded f32 significand.
  val sig_out =
	Mux(exponentSpecial,   Cat(isInvalidNaN || sig_in(51).toBool, sig_in(50,29)),
	Mux(exponentInRange,   sig_round(22,0),
	Mux(exponentOverflow,  sig_overflow_val,
	Mux(exponentUnderflow, sig_underflow_val,
	Bits(0,23)))));

  io.out := Cat(sign, exponent_out, sig_out(22,0));

  // Assemble the exception flags vector.
  val flagUnderflow  = exponentUnderflow || exponentIsSubnormal && roundInexact;
  val flagOverflow   = exponentOverflow || 
    		     exponent_in === max_f32_exp && sig_round(24).toBool;
  io.exceptionFlags := Cat(isInvalidNaN, Bits("b0",1), flagOverflow, flagUnderflow,
			 roundInexact | exponentOverflow | exponentUnderflow);
}
}

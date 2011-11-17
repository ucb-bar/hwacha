//
// anyToRecodedFloat64( in, out );
// Author: Brian Richards, 10/14/2010
// Based on float32ToRecodedFloat32 from John Hauser
//
package Fpu {

import Chisel._
import Node._;

class anyToRecodedFloat64_io(SIG_WIDTH: Int, EXP_WIDTH: Int, INT_WIDTH: Int) extends Bundle {
  val in             = Bits(INT_WIDTH ,'input);
  val roundingMode   = Bits(2 ,'input);
  val typeOp         = Bits(2 ,'input);
  val out            = Bits(SIG_WIDTH + EXP_WIDTH + 1, 'output);
  val exceptionFlags = Bits(5, 'output);
}

class anyToRecodedFloat64(
	SIG_WIDTH: Int = 52,
	EXP_WIDTH: Int = 12,
	INT_WIDTH: Int = 64) extends Component {
 override val io = new anyToRecodedFloat64_io(SIG_WIDTH, EXP_WIDTH, INT_WIDTH);

	val FLOAT_WIDTH = SIG_WIDTH + EXP_WIDTH + 1;
	val SHIFT_WIDTH = INT_WIDTH + 1; // Save one fraction bit.
	val STAGES = 7;                  // $clog2(SHIFT_WIDTH);
	val EXP_OFFSET = Bits("h800", 12); //12'h800; // Recoded offset=2048 (IEEE offset=1023)

	val type_uint32 = Bits(0, 2);
	val type_int32  = Bits(1, 2);
	val type_uint64 = Bits(2, 2);
	val type_int64  = Bits(3, 2);

	val round_nearest_even = Bits(0, 2);
	val round_minMag       = Bits(1, 2);
	val round_min          = Bits(2, 2);
	val round_max          = Bits(3, 2);

// Generate the absolute value of the input.
val sign =
	Mux(io.typeOp === type_uint32, Bool(false),
	Mux(io.typeOp === type_int32,  io.in(31).toBool,
	Mux(io.typeOp === type_uint64, Bool(false),
	Mux(io.typeOp === type_int64,  io.in(63).toBool,
	Bool(false)))));

val norm_in =
	Mux(io.typeOp === type_uint32, Cat(Bits(0,32), io.in(31,0)),
	Mux(io.typeOp === type_int32,  Cat(Bits(0,32), Mux(sign, -io.in(31,0), io.in(31,0))),
	Mux(io.typeOp === type_uint64, io.in,
	Mux(io.typeOp === type_int64,  Mux(sign, -io.in , io.in),
	Bits(0,64)))));

// Normalize to generate the fractional part.
val normalizeFract = new normalize64();
normalizeFract.io.in := norm_in;
val norm_count = normalizeFract.io.distance;
val norm_out   = normalizeFract.io.out;

// Rounding depends on:
//  norm_out(11):  The LSB of the significand
//  norm_out(10):  The MSB of the extra bits to be rounded
//  norm_out(9,0): Remaining Extra bits
val roundBits = Cat(norm_out(11,10),(norm_out(9,0) != Bits(0,10)));

// Check if rounding is necessary.
val roundInexact =
	Mux(io.typeOp === type_uint32, Bool(false),
	Mux(io.typeOp === type_int32,  Bool(false),
	Mux(io.typeOp === type_uint64, roundBits(1,0) != Bits(0,2),
	Mux(io.typeOp === type_int64,  roundBits(1,0) != Bits(0,2),
	Bool(false)))));

// Determine the rounding increment, based on the rounding mode.
val roundEvenOffset = (roundBits(1,0) === Bits("b11",2) ||
		       roundBits(2,1) === Bits("b11",2));
val roundOffset =
	Mux(io.roundingMode === round_nearest_even, roundEvenOffset,
	Mux(io.roundingMode === round_minMag,       Bits(0,1),
	Mux(io.roundingMode === round_min,
			sign & Mux(roundInexact, Bits(1,1), Bits(0,1)),
	Mux(io.roundingMode === round_max,
			~sign & Mux(roundInexact, Bits(1,1), Bits(0,1)),
	Bits(0,1))))).toUFix;

// The rounded normalized significand includes the carry-out, implicit unit
// digit, and 52-bits of final significand (54 bits total).
val norm_round = (Cat(Bits(0, 1), norm_out(63, 11)).toUFix + roundOffset);

// For the Recoded Float64:
//  norm_count Exponent  Recoded Exponent      IEEE Exponent
//   63, msb=0    2^0      12'b000---------      11'b00000000000
//   63, msb=1    2^0      12'b100000000000      11'b01111111111
//   62           2^1      12'b100000000001      11'b10000000000
//   61           2^2      12'b100000000010      11'b10000000001
//     ...
//   1            2^62     12'b100000111110      11'b10000111101
//   0            2^63     12'b100000111111      11'b10000111110
//

// Construct the exponent from the norm_count, and increment the
// exponent if the rounding overflows (the significand will still
// be all zeros in this case).
val exponent_offset = Cat(Bits("b100000",6), ~norm_count).toUFix + norm_round(53);
val exponent =
	Mux(norm_out(63) === Bits(0,1) && norm_count === Bits(63,6),
		Bits(0,12),
		exponent_offset);

io.out := Cat(sign, exponent, norm_round(51,0));
io.exceptionFlags := Cat(Bits(0,4), roundInexact);
}
}

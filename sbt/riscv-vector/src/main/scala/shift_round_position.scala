//
// shift_round_position -- Logarithmic stage for rounding at a
// programmable bit position.
// Author: Brian Richards, 11/11/2010
//
// Inputs:
//  do_shift:        If 1, shift and pad inputs with ones.
//  in               Initially masked significand.
//  in_round_bits    Initially right-shifted significand.
//  in_sticky        Initial value of the sticky bit (extra LSBS != 1).
// Outputs:
//  out              Conditionally updated significand (trailing ones).
//  out_round_bits   Conditionally shifted significand (leading zeros).
//  out_sticky       Updated sticky bit.
package Fpu {

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet

import Chisel._
import Node._;

class shift_round_position_io(DATA_WIDTH: Int, SHIFT_BITS: Int) extends Bundle() {
  val do_shift = Bits(1, INPUT);
  val in        = Bits(DATA_WIDTH, INPUT);     // The LSB is 1/2 digit.
  val in_round_bits =	Bits(DATA_WIDTH, INPUT);
  val in_sticky     =  Bits(1, INPUT);
  val out	     =  Bits(DATA_WIDTH, OUTPUT);
  val out_round_bits = 	Bits(DATA_WIDTH, OUTPUT);
  val out_sticky     =	Bits(1, OUTPUT);
}

class shift_round_position(DATA_WIDTH: Int = 32, SHIFT_BITS: Int = 16) extends Component {
  override val io = new shift_round_position_io(DATA_WIDTH, SHIFT_BITS);


// Conditionally pad the right SHIFT_BITS bits with ones.

  /*
  var right_shift_bits: Node = Lit(1, 1);
  for(i <- 0 until SHIFT_BITS)
    right_shift_bits = Cat(right_shift_bits, Lit("b1", 1))
 */

  val right_shift_bits = Fill(SHIFT_BITS, Bits(1, 1));

  io.out := 
  	 Mux(io.do_shift, Cat(io.in(DATA_WIDTH-1, SHIFT_BITS), right_shift_bits),
	 	       io.in);
		

// The round bits are a conditionally right-shifted version of the input bits.
  io.out_round_bits :=
	Mux(io.do_shift, 
		 Cat(Bits(0, SHIFT_BITS), io.in(DATA_WIDTH-1,SHIFT_BITS)),
	         io.in_round_bits);

  val zero_lsbs = io.in_round_bits(SHIFT_BITS-1,0) != Bits(0, SHIFT_BITS);
  io.out_sticky := Mux(io.do_shift, (zero_lsbs || io.in_sticky.toBool), 
  		  		    io.in_sticky);	 
  
}
}

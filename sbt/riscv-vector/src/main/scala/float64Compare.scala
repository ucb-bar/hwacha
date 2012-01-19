//
// float64Compare
// Author: Brian Richards, 4/14/2011
//
package Fpu {

import Chisel._
import Node._;

class float64Compare_io extends Bundle() {
  val a_in =           Bits(64, INPUT);
  val b_in =           Bits(64, INPUT);
  val a_eq_b =         Bits(1, OUTPUT);
  val a_eq_b_invalid = Bits(1, OUTPUT);
  val a_lt_b =         Bits(1, OUTPUT);
  val a_lt_b_invalid = Bits(1, OUTPUT);
}

class float64Compare extends Component {
  override val io = new float64Compare_io();

  val a_f64_to_rf64 = new float64ToRecodedFloat64();
  val b_f64_to_rf64 = new float64ToRecodedFloat64();
  val compare_dut   = new recodedFloat64Compare();

  a_f64_to_rf64.io.in            ^^ io.a_in;
  a_f64_to_rf64.io.out           <> compare_dut.io.a;

  b_f64_to_rf64.io.in            ^^ io.b_in;
  b_f64_to_rf64.io.out           <> compare_dut.io.b;

  compare_dut.io.a_eq_b         ^^ io.a_eq_b;
  compare_dut.io.a_eq_b_invalid ^^ io.a_eq_b_invalid;
  compare_dut.io.a_lt_b         ^^ io.a_lt_b;
  compare_dut.io.a_lt_b_invalid ^^ io.a_lt_b_invalid;
}

} // End Package Fpu

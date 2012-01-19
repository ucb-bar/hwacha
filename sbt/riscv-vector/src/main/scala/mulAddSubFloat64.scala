//
// float64Compare
// Author: Brian Richards, 4/14/2011
//
package Fpu {

import Chisel._
import Node._;

class mulAddSubFloat64_io extends Bundle() {
  val a               = Bits(64, INPUT);
  val b               = Bits(64, INPUT);
  val c               = Bits(64, INPUT);
  val op              = Bits(2, INPUT);
  val rounding_mode   = Bits(2, INPUT);
  val exception_flags = Bits(5, OUTPUT);
  val out             = Bits(64, OUTPUT);
}

class mulAddSubFloat64 extends Component {
  override val io = new mulAddSubFloat64_io();

  val mulAf64rf64 = new float64ToRecodedFloat64();
  val mulBf64rf64 = new float64ToRecodedFloat64();
  val mulCf64rf64 = new float64ToRecodedFloat64();
  val mulAddR64   = new mulAddSubRecodedFloat64_1();
  val mulrf64f64  = new recodedFloat64ToFloat64();

  mulAf64rf64.io.in ^^ io.a;
  mulBf64rf64.io.in ^^ io.b;
  mulCf64rf64.io.in ^^ io.c;
  mulAf64rf64.io.out <> mulAddR64.io.a;
  mulBf64rf64.io.out <> mulAddR64.io.b;
  mulCf64rf64.io.out <> mulAddR64.io.c;
  mulAddR64.io.roundingMode ^^ io.rounding_mode;
  mulAddR64.io.exceptionFlags ^^ io.exception_flags;
  mulAddR64.io.op ^^ io.op;
  mulAddR64.io.out <> mulrf64f64.io.in;
  mulrf64f64.io.out ^^ io.out;  
}

} // End Package Fpu

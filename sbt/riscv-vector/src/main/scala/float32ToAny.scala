//
// float32ToAny
// Author: Brian Richards, 5/17/2011
//
package Fpu {

import Chisel._
import Node._;

class float32ToAny_io extends Bundle() {
  val in =             Bits(32, INPUT);
  val roundingMode =   Bits(2, INPUT);
  val typeOp =         Bits(2, INPUT);
  val out =            Bits(64, OUTPUT);
  val exceptionFlags = Bits(5, OUTPUT);
}

class float32ToAny extends Component {
  override val io = new float32ToAny_io();

  val f32_to_rf32 = new float32ToRecodedFloat32();
  val rf32_to_any  = new recodedFloat32ToAny();

  f32_to_rf32.io.in            ^^ io.in;
  f32_to_rf32.io.out           <> rf32_to_any.io.in;
  rf32_to_any.io.roundingMode   ^^ io.roundingMode;
  rf32_to_any.io.typeOp         ^^ io.typeOp;
  rf32_to_any.io.out            ^^ io.out;
  rf32_to_any.io.exceptionFlags ^^ io.exceptionFlags;
}

} // End Package Fpu

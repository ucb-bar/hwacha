//
// anyToFloat32
// Author: Brian Richards, 5/17/2011, U. C. Berkeley
//
package Fpu {

import Chisel._
import Node._;

class anyToFloat32_io extends Bundle() {
  val in =             Bits(64 ,INPUT);
  val roundingMode =   Bits(2 ,INPUT);
  val typeOp =         Bits(2 ,INPUT);
  val out =            Bits(32, OUTPUT);
  val exceptionFlags = Bits(5, OUTPUT);
}

class anyToFloat32 extends Component {
  override val io = new anyToFloat32_io();

  val any_to_rf32  = new anyToRecodedFloat32();
  val rf32_to_f32 = new recodedFloat32ToFloat32();

  any_to_rf32.io.in             ^^ io.in;
  any_to_rf32.io.roundingMode   ^^ io.roundingMode;
  any_to_rf32.io.typeOp         ^^ io.typeOp;
  any_to_rf32.io.exceptionFlags ^^ io.exceptionFlags;
  any_to_rf32.io.out            <> rf32_to_f32.io.in;

  rf32_to_f32.io.out            ^^ io.out;
}

} // End Package Fpu

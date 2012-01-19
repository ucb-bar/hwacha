package Fpu {

import Chisel._
import Node._;

class float32ToRecodedFloat32_io extends Bundle() {
  val in  = Bits(32, INPUT);
  val out = Bits(33, OUTPUT);
}

class float32ToRecodedFloat32 extends Component {
  override val io = new float32ToRecodedFloat32_io();

  val sign    = io.in(31);
  val expIn   = io.in(30, 23).toUFix;
  val fractIn = io.in(22, 0).toUFix;
  val isZeroExpIn = ( expIn === Bits(0, 9) );
  val isZeroFractIn = ( fractIn === Bits(0, 23) );
  val isZeroOrSubnormal = isZeroExpIn;
  val isZero      = isZeroOrSubnormal &   isZeroFractIn;
  val isSubnormal = isZeroOrSubnormal & ~ isZeroFractIn;
  val isNormalOrSpecial = ~ isZeroExpIn;

  val norm_in = Cat(fractIn, Bits(0,9));

  val normalizeFract = new normalize32();
  normalizeFract.io.in := norm_in;
  val norm_count = normalizeFract.io.distance;
  val norm_out   = normalizeFract.io.out;

  val normalizedFract = norm_out(30,8);
  val commonExp =
    Mux(isSubnormal, Cat(Bits("b1111",4), ~norm_count), Bits(0,9)) |
    Mux(isNormalOrSpecial, expIn, Bits(0,9));
  val expAdjust = Mux(isZero, Bits(0,9), Bits("b010000001", 9));
  val adjustedCommonExp = commonExp.toUFix + expAdjust.toUFix + isSubnormal.toUFix;
  val isNaN = (adjustedCommonExp(8,7) === Bits("b11",2)) & ~ isZeroFractIn;

  //val expOut = adjustedCommonExp | (isNaN(6, 0)<<6);
  val expOut = adjustedCommonExp | (isNaN << UFix(6));
  val fractOut = Mux(isZeroOrSubnormal, normalizedFract, fractIn);

  io.out := Cat(sign, expOut, fractOut);
}
}

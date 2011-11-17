package Fpu {

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet

import Chisel._;
import Node._;
import LitConv._;
  
class float64ToRecodedFloat64_io extends Bundle() {
  val in = Bits(64, 'input);
  val out = Bits(65, 'output);
}

class float64ToRecodedFloat64 extends Component {
      override val io = new float64ToRecodedFloat64_io();

    val sign    = io.in(63);
    val expIn   = io.in(62,52);
    val fractIn = io.in(51,0);
    val isZeroExpIn = ( expIn === Bits(0) );
    val isZeroFractIn = ( fractIn === Bits(0) );
    val isZeroOrSubnormal = isZeroExpIn;
    val isZero      = isZeroOrSubnormal &   isZeroFractIn;
    val isSubnormal = isZeroOrSubnormal & ~ isZeroFractIn;
    val isNormalOrSpecial = ~ isZeroExpIn;

    val norm_in = Cat(fractIn, Bits(0, 12) );
    
	val normalizeFract = new normalize64(64, 6);
    	normalizeFract.io.in := norm_in;
    	val norm_count = normalizeFract.io.distance;
    	val norm_out   = normalizeFract.io.out;

    val normalizedFract = norm_out(62,11);
    val commonExp =
                Mux(isSubnormal, Cat( Bits("b111111", 6), ~ norm_count) , Bits(0, 12) ) | 
		Mux( isNormalOrSpecial, expIn, Bits(0, 12) );
    val expAdjust = Mux(isZero, Bits(0, 12), Bits("b010000000001", 12));
    val adjustedCommonExp = commonExp.toUFix + expAdjust.toUFix + isSubnormal.toUFix;
    val isNaN = (adjustedCommonExp(11,10) === Bits("b11",2)) & ~ isZeroFractIn;

    val expOut = adjustedCommonExp | Cat(isNaN, Bits(0,9)); // isNaN << 9;
    val fractOut = Mux(isZeroOrSubnormal, normalizedFract, fractIn);
    io.out := Cat(sign, expOut, fractOut);

}
}

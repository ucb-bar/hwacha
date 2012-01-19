package Fpu {

import Chisel._
import Node._;

class recodedFloat32ToFloat32_io extends Bundle() {
  val in  = Bits(33, INPUT);
  val out = Bits(32, OUTPUT);
}

class recodedFloat32ToFloat32 extends Component {
    override val io = new recodedFloat32ToFloat32_io();

    val sign    = io.in(32);
    val expIn   = io.in(31, 23).toUFix;
    val fractIn = io.in(22, 0).toUFix;
    val exp01_isHighSubnormalIn = ( expIn(6, 0).toUFix < UFix(2, 7) );
    val isSubnormal =
        ( expIn(8, 6) === Bits("b001", 3) ) | 
		( ( expIn(8, 7) === Bits("b01", 2) ) & exp01_isHighSubnormalIn );
    val isNormal =
        ( ( expIn(8, 7) === Bits("b01", 2) ) & ~ exp01_isHighSubnormalIn ) | 
		( expIn(8, 7) === Bits("b10", 2) );
    val isSpecial = ( expIn(8, 7) === Bits("b11", 2) );
    val isNaN = isSpecial & expIn(6);

    val denormShiftDist = UFix(2, 5) - expIn(4, 0).toUFix;
    val subnormal_fractOut = Cat(Bits(1, 1), fractIn) >> denormShiftDist(4,0).toUFix;
    val normal_expOut = expIn - Bits("b010000001", 9).toUFix;

    val expOut =
        Mux( isNormal, normal_expOut ,  Bits(0, 8) ) | 
	Mux( isSpecial, Bits("b11111111", 8) ,  Bits(0, 8) );
    val fractOut =
          Mux(isSubnormal,       subnormal_fractOut,  Bits(0, 23) ) | 
	  	  Mux(isNormal | isNaN , fractIn,             Bits(0, 23) );
    io.out := Cat(sign, expOut(7, 0), fractOut(22, 0));
}
}

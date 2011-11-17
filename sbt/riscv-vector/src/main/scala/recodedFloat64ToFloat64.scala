package Fpu {

import Chisel._
import Node._;

class recodedFloat64ToFloat64_io extends Bundle {
  val in  = Bits(65, 'input);
  val out = Bits(64, 'output);
}

class recodedFloat64ToFloat64 extends Component {
  override val io = new recodedFloat64ToFloat64_io();
  val sign    = io.in(64);
  val expIn   = io.in(63,52).toUFix;
  val fractIn = io.in(51,0);

  val exp01_isHighSubnormalIn = ( expIn(9,0).toUFix < UFix(2,10) );
  val isSubnormal =
        ( expIn(11,9) === Bits("b001",3) ) | 
	( ( expIn(11,10) === Bits("b01",2) ) & exp01_isHighSubnormalIn );
  val isNormal    =
        ( ( expIn(11,10) === Bits("b01",2) ) & ~ exp01_isHighSubnormalIn ) | 
	( expIn(11,10) === Bits("b10",2) );
  val isSpecial   = ( expIn(11,10) === Bits("b11",2) );
  val isNaN       = isSpecial & expIn(9);

  val denormShiftCount = UFix(2,6) - expIn(5,0).toUFix;
  val subnormal_fractOut = (Cat(Bits(1,1), fractIn) >> denormShiftCount.toUFix)(51,0);
  val normal_expOut = (expIn - Bits("b010000000001",12).toUFix)(10,0);

  val expOut = ( Mux(isNormal,  normal_expOut,         Bits(0,11) ) ) | 
      	       ( Mux(isSpecial, Bits("b11111111111",11), Bits(0,11) ) );
  val fractOut = ( Mux(isSubnormal,      subnormal_fractOut, Bits(0,52) ) ) |
      	       	 ( Mux(isNormal | isNaN, fractIn,            Bits(0,52) ) );

  io.out := Cat(sign, expOut, fractOut);
}
}

package Fpu{

import Chisel._;
import Node._;

class estNormDistP24PosSum50_io() extends Bundle{
  val a = Bits(50 ,'input); 
  val b = Bits(50 ,'input);	
  val out = Bits(7, 'output);			  
}

class estNormDistP24PosSum50 extends Component{
    override val io = new estNormDistP24PosSum50_io();
    //val key = ( io.a ^ io.b ) ^ ( ( io.a | io.b )<<1 );
    val key = (( io.a ^ io.b ) ^ Cat(io.a | io.b, Bits(0,1)))(49, 0);
    io.out := UFix(73, 7) - Log2(key.toUFix, 49);
/*
        Mux( key(49) , Lit(24, 7),
        Mux( key(48) , Lit(25, 7),
        Mux( key(47) , Lit(26, 7),
        Mux( key(46) , Lit(27, 7),
        Mux( key(45) , Lit(28, 7),
        Mux( key(44) , Lit(29, 7),
        Mux( key(43) , Lit(30, 7),
        Mux( key(42) , Lit(31, 7),
        Mux( key(41) , Lit(32, 7),
        Mux( key(40) , Lit(33, 7),
        Mux( key(39) , Lit(34, 7),
        Mux( key(38) , Lit(35, 7),
        Mux( key(37) , Lit(36, 7),
        Mux( key(36) , Lit(37, 7),
        Mux( key(35) , Lit(38, 7),
        Mux( key(34) , Lit(39, 7),
        Mux( key(33) , Lit(40, 7),
        Mux( key(32) , Lit(41, 7),
        Mux( key(31) , Lit(42, 7),
        Mux( key(30) , Lit(43, 7),
        Mux( key(29) , Lit(44, 7),
        Mux( key(28) , Lit(45, 7),
        Mux( key(27) , Lit(46, 7),
        Mux( key(26) , Lit(47, 7),
        Mux( key(25) , Lit(48, 7),
        Mux( key(24) , Lit(49, 7),
        Mux( key(23) , Lit(50, 7),
        Mux( key(22) , Lit(51, 7),
        Mux( key(21) , Lit(52, 7),
        Mux( key(20) , Lit(53, 7),
        Mux( key(19) , Lit(54, 7),
        Mux( key(18) , Lit(55, 7),
        Mux( key(17) , Lit(56, 7),
        Mux( key(16) , Lit(57, 7),
        Mux( key(15) , Lit(58, 7),
        Mux( key(14) , Lit(59, 7),
        Mux( key(13) , Lit(60, 7),
        Mux( key(12) , Lit(61, 7),
        Mux( key(11) , Lit(62, 7),
        Mux( key(10) , Lit(63, 7),
        Mux( key(9)  , Lit(64, 7),
        Mux( key(8)  , Lit(65, 7),
        Mux( key(7)  , Lit(66, 7),
        Mux( key(6)  , Lit(67, 7),
        Mux( key(5)  , Lit(68, 7),
        Mux( key(4)  , Lit(69, 7),
        Mux( key(3)  , Lit(70, 7),
        Mux( key(2)  , Lit(71, 7),
        Mux( key(1)  , Lit(72, 7),
        Lit(73, 7))))))))))))))))))))))))))))))))))))))))))))))))));
*/
}
}



package Fpu{

import Chisel._;
import Node._;

class estNormDistP53NegSum108_io() extends Bundle{
  val a = Bits(108 ,INPUT);
  val b = Bits(108 ,INPUT);
  val out = Bits(8, OUTPUT);
}

class estNormDistP53NegSum108 extends Component{
  override val io = new estNormDistP53NegSum108_io();
    //val key = ~(( io.a ^ io.b ) ^ ( ( io.a & io.b )<<1 ));
    val key_tmp = ~(( io.a ^ io.b ) ^ Cat(io.a & io.b, Bits(0,1)));
    val key = key_tmp(107,0);
/*
    val lower_half =
        Mux(key(54) , Lit(106, 8), 
        Mux(key(53) , Lit(107, 8), 
        Mux(key(52) , Lit(108, 8), 
        Mux(key(51) , Lit(109, 8), 
        Mux(key(50) , Lit(110, 8), 
        Mux(key(49) , Lit(111, 8), 
        Mux(key(48) , Lit(112, 8), 
        Mux(key(47) , Lit(113, 8), 
        Mux(key(46) , Lit(114, 8), 
        Mux(key(45) , Lit(115, 8), 
        Mux(key(44) , Lit(116, 8), 
        Mux(key(43) , Lit(117, 8), 
        Mux(key(42) , Lit(118, 8), 
        Mux(key(41) , Lit(119, 8), 
        Mux(key(40) , Lit(120, 8), 
        Mux(key(39) , Lit(121, 8), 
        Mux(key(38) , Lit(122, 8), 
        Mux(key(37) , Lit(123, 8), 
        Mux(key(36) , Lit(124, 8), 
        Mux(key(35) , Lit(125, 8), 
        Mux(key(34) , Lit(126, 8), 
        Mux(key(33) , Lit(127, 8), 
        Mux(key(32) , Lit(128, 8), 
        Mux(key(31) , Lit(129, 8), 
        Mux(key(30) , Lit(130, 8), 
        Mux(key(29) , Lit(131, 8), 
        Mux(key(28) , Lit(132, 8), 
        Mux(key(27) , Lit(133, 8), 
        Mux(key(26) , Lit(134, 8), 
        Mux(key(25) , Lit(135, 8), 
        Mux(key(24) , Lit(136, 8), 
        Mux(key(23) , Lit(137, 8), 
        Mux(key(22) , Lit(138, 8), 
        Mux(key(21) , Lit(139, 8), 
        Mux(key(20) , Lit(140, 8), 
        Mux(key(19) , Lit(141, 8), 
        Mux(key(18) , Lit(142, 8), 
        Mux(key(17) , Lit(143, 8), 
        Mux(key(16) , Lit(144, 8), 
        Mux(key(15) , Lit(145, 8), 
        Mux(key(14) , Lit(146, 8), 
        Mux(key(13) , Lit(147, 8), 
        Mux(key(12) , Lit(148, 8), 
        Mux(key(11) , Lit(149, 8), 
        Mux(key(10) , Lit(150, 8), 
        Mux(key(9)  , Lit(151, 8), 
        Mux(key(8)  , Lit(152, 8), 
        Mux(key(7)  , Lit(153, 8), 
        Mux(key(6)  , Lit(154, 8), 
        Mux(key(5)  , Lit(155, 8), 
        Mux(key(4)  , Lit(156, 8), 
        Mux(key(3)  , Lit(157, 8), 
        Mux(key(2)  , Lit(158, 8), 
        Mux(key(1)  , Lit(159, 8), 
        Lit(160, 8)))))))))))))))))))))))))))))))))))))))))))))))))))))));
    io.out :=
        Mux(key(107),  Lit(53, 8),
        Mux(key(106),  Lit(54, 8),
        Mux(key(105),  Lit(55, 8),
        Mux(key(104),  Lit(56, 8),
        Mux(key(103),  Lit(57, 8),
        Mux(key(102),  Lit(58, 8),
        Mux(key(101),  Lit(59, 8),
        Mux(key(100),  Lit(60, 8),
        Mux(key(99) ,  Lit(61, 8), 
        Mux(key(98) ,  Lit(62, 8), 
        Mux(key(97) ,  Lit(63, 8), 
        Mux(key(96) ,  Lit(64, 8), 
        Mux(key(95) ,  Lit(65, 8), 
        Mux(key(94) ,  Lit(66, 8), 
        Mux(key(93) ,  Lit(67, 8), 
        Mux(key(92) ,  Lit(68, 8), 
        Mux(key(91) ,  Lit(69, 8), 
        Mux(key(90) ,  Lit(70, 8), 
        Mux(key(89) ,  Lit(71, 8), 
        Mux(key(88) ,  Lit(72, 8), 
        Mux(key(87) ,  Lit(73, 8), 
        Mux(key(86) ,  Lit(74, 8), 
        Mux(key(85) ,  Lit(75, 8), 
        Mux(key(84) ,  Lit(76, 8), 
        Mux(key(83) ,  Lit(77, 8), 
        Mux(key(82) ,  Lit(78, 8), 
        Mux(key(81) ,  Lit(79, 8), 
        Mux(key(80) ,  Lit(80, 8), 
        Mux(key(79) ,  Lit(81, 8), 
        Mux(key(78) ,  Lit(82, 8), 
        Mux(key(77) ,  Lit(83, 8), 
        Mux(key(76) ,  Lit(84, 8), 
        Mux(key(75) ,  Lit(85, 8), 
        Mux(key(74) ,  Lit(86, 8), 
        Mux(key(73) ,  Lit(87, 8), 
        Mux(key(72) ,  Lit(88, 8), 
        Mux(key(71) ,  Lit(89, 8), 
        Mux(key(70) ,  Lit(90, 8), 
        Mux(key(69) ,  Lit(91, 8), 
        Mux(key(68) ,  Lit(92, 8), 
        Mux(key(67) ,  Lit(93, 8), 
        Mux(key(66) ,  Lit(94, 8), 
        Mux(key(65) ,  Lit(95, 8), 
        Mux(key(64) ,  Lit(96, 8), 
        Mux(key(63) ,  Lit(97, 8), 
        Mux(key(62) ,  Lit(98, 8), 
        Mux(key(61) ,  Lit(99, 8), 
        Mux(key(60) , Lit(100, 8), 
        Mux(key(59) , Lit(101, 8), 
        Mux(key(58) , Lit(102, 8), 
        Mux(key(57) , Lit(103, 8), 
        Mux(key(56) , Lit(104, 8), 
        Mux(key(55) , Lit(105, 8), 
        lower_half)))))))))))))))))))))))))))))))))))))))))))))))))))));
*/
  io.out := UFix(160, 8) - Log2(key.toUFix, 107);

}
}


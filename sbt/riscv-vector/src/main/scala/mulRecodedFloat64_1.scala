
//*** THIS MODULE IS NOT FULLY OPTIMIZED.

//*** DO THIS ANOTHER WAY?
package Fpu{

import Chisel._;
import Node._;
import LitConv._;
import mulRecodedFloat32_1._;

object mulRecodedFloat64_1 {
  val round_nearest_even = Bits("b00",2);
  val round_minMag       = Bits("b01",2);
  val round_min          = Bits("b10",2);
  val round_max          = Bits("b11",2);
}

class mulRecodedFloat64_1_io() extends Bundle{
  val a = Bits(65, 'input);
  val b = Bits(65, 'input);
  val roundingMode = Bits(2, 'input);
  val out = Bits(65, 'output);
  val exceptionFlags = Bits(65, 'output);
}

class mulRecodedFloat64_1 extends Component{
    override val io = new mulRecodedFloat64_1_io();
    val signA  = io.a(64);
    val expA   = io.a(63,52).toUFix;
    val fractA = io.a(51,0).toUFix;
    val isZeroA = ( expA(11,9) === Bits("b000",3) );
    val isSpecialA = ( expA(11,10) === Bits("b11",2) );
    val isInfA = isSpecialA & ~ expA(9);
    val isNaNA = isSpecialA &   expA(9);
    val isSigNaNA = isNaNA & ~ fractA(51);
    val sigA = Cat(~ isZeroA, fractA).toUFix;

    val signB  = io.b(64);
    val expB   = io.b(63,52).toUFix;
    val fractB = io.b(51,0).toUFix;
    val isZeroB = ( expB(11,9) === Bits("b000",3) );
    val isSpecialB = ( expB(11,10) === Bits("b11",2) );
    val isInfB = isSpecialB & ~ expB(9);
    val isNaNB = isSpecialB &   expB(9);
    val isSigNaNB = isNaNB & ~ fractB(51);
    val sigB = Cat(~ isZeroB, fractB).toUFix;

    val roundingMode_nearest_even = ( io.roundingMode === round_nearest_even );
    val roundingMode_minMag       = ( io.roundingMode === round_minMag       );
    val roundingMode_min          = ( io.roundingMode === round_min          );
    val roundingMode_max          = ( io.roundingMode === round_max          );


    val signOut = signA ^ signB;

    val expProd = expA + Cat(Fill(2, ~ expB(11)), expB(10,0)).toUFix;
    val notNeg_expProd = expProd(11,0);
    val sigProd = sigA * sigB;
    val prodShift1 = sigProd(105);
    val sigProdX = Cat(sigProd(105,51), ( sigProd(50,0) != Bits(0) )).toUFix;

//*** FIRST TWO BITS NEEDED?
    val roundMask =
        Cat(( notNeg_expProd <= Bits("b001111001100",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111001101",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111001110",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111001111",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111010000",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111010001",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111010010",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111010011",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111010100",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111010101",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111010110",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111010111",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111011000",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111011001",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111011010",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111011011",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111011100",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111011101",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111011110",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111011111",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111100000",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111100001",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111100010",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111100011",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111100100",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111100101",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111100110",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111100111",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111101000",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111101001",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111101010",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111101011",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111101100",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111101101",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111101110",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111101111",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111110000",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111110001",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111110010",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111110011",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111110100",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111110101",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111110110",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111110111",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111111000",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111111001",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111111010",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111111011",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111111100",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111111101",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111111110",12).toUFix ),
         ( notNeg_expProd <= Bits("b001111111111",12).toUFix ),
         ( notNeg_expProd <= Bits("b010000000000",12).toUFix ),
         ( notNeg_expProd <= Bits("b010000000001",12).toUFix ) | prodShift1,
         Bits("b11",2));
    val roundPosMask = ~ Cat(Bits("b0",1), roundMask>>UFix(1)) & roundMask;
    val roundIncr =
          ( Mux(roundingMode_nearest_even, roundPosMask , Bits(0) )) |
          ( Mux( Mux(signOut, roundingMode_min , roundingMode_max ) , roundMask,
                Bits(0) ));
    val roundSigProdX = sigProdX + Cat(Bits("b0",1), roundIncr).toUFix;
    val roundPosBit = ( ( sigProdX & roundPosMask ) != Bits(0) );
    val anyRoundExtra = ( ( sigProdX & roundMask>>UFix(1) ) != Bits(0) );
    val roundInexact = roundPosBit | anyRoundExtra;
    val roundEven =
        roundingMode_nearest_even & roundPosBit & ! anyRoundExtra;
    val sigProdY =
        roundSigProdX>>UFix(2) & ~ ( Mux(roundEven, roundMask>>UFix(1) , roundMask>>UFix(2) ));
//*** COMPOUND ADD FOR `expProd'?
    val sExpY = expProd + sigProdY(54,53).toUFix;
    val expY = sExpY(11,0);
    val fractY = Mux(prodShift1, sigProdY(52,1) , sigProdY(51,0));

    val overflowY = ( sExpY(12,10) === Bits("b011",3) );
//*** CHANGE TO USE `expProd'/`notNeg_expProd'?
    val totalUnderflowY = sExpY(12) | ( sExpY(11,0) < Bits("b001111001110",12).toUFix );
    val underflowY =
//*** REPLACE?:
        totalUnderflowY |
//*** USE EARLIER BITS FROM `roundMask'?
              ( ( notNeg_expProd <=
                         ( Mux(prodShift1, Bits("b010000000000",12) , Bits("b010000000001",12) ) ).toUFix) &
                      roundInexact );
    val inexactY = roundInexact;

    val overflowY_roundMagUp =
        roundingMode_nearest_even | ( roundingMode_min & signOut ) |
              ( roundingMode_max & ~ signOut );

    val mulSpecial = isSpecialA | isSpecialB;
    val commonCase = ~ mulSpecial & ~ isZeroA & ~ isZeroB;

    val notSigNaN_invalid = ( isInfA & isZeroB ) | ( isZeroA & isInfB );
    val invalid = isSigNaNA | isSigNaNB | notSigNaN_invalid;
    val overflow = commonCase & overflowY;
    val underflow = commonCase & underflowY;
//*** SPEED BY USING `commonCase & totalUnderflowY' INSTEAD OF `underflow'?
    val inexact = overflow | underflow | ( commonCase & inexactY );

    val notSpecial_isZeroOut = isZeroA | isZeroB | totalUnderflowY;
    val isSatOut = overflow & ~ overflowY_roundMagUp;
    val notNaN_isInfOut =
        isInfA | isInfB | ( overflow & overflowY_roundMagUp );
    val isNaNOut = isNaNA | isNaNB | notSigNaN_invalid;

    val expOut =
        (   expY &
            ~ ( Mux(notSpecial_isZeroOut, Bits("b111000000000",12) , Bits(0) )) &
            ~ ( Mux(isSatOut, Bits("b010000000000",12) , Bits(0) )) &
            ~ ( Mux(notNaN_isInfOut, Bits("b001000000000",12) , Bits(0) )) ) |
            ( Mux(isSatOut, Bits("b101111111111",12) , Bits(0) )) |
            ( Mux(notNaN_isInfOut, Bits("b110000000000",12) , Bits(0) )) |
            ( Mux(isNaNOut, Bits("b111000000000",12) , Bits(0) ));
    val fractOut = fractY | ( Mux(isNaNOut | isSatOut, Bits("hFFFFFFFFFFFFF",52) , Bits(0) ));
    io.out := Cat(signOut, expOut, fractOut);

    io.exceptionFlags := Cat(invalid, Bits("b0",1), overflow, underflow, inexact);

}
}

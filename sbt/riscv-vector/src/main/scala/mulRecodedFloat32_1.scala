
//*** THIS MODULE HAS NOT BEEN FULLY OPTIMIZED.
package Fpu{
import Chisel._;
import Node._;
import LitConv._;
import mulRecodedFloat32_1._;

object mulRecodedFloat32_1 {
  val round_nearest_even = Bits("b00",2);
  val round_minMag       = Bits("b01",2);
  val round_min          = Bits("b10",2);
  val round_max          = Bits("b11",2);
}

class mulRecodedFloat32_1_io() extends Bundle{
  val a = Bits(33, 'input);
  val b = Bits(33, 'input);
  val roundingMode = Bits(2, 'input);
  val out = Bits(33, 'output);
  val exceptionFlags = Bits(5, 'output);
}

class mulRecodedFloat32_1 extends Component{
    override val io = new mulRecodedFloat32_1_io();
    val signA  = io.a(32);
    val expA   = io.a(31,23).toUFix;
    val fractA = io.a(22,0).toUFix;
    val isZeroA = ( expA(8,6) === Bits("b000",3) );
    val isSpecialA = ( expA(8,7) === Bits("b11",2) );
    val isInfA = isSpecialA & ~ expA(6).toBool;
    val isNaNA = isSpecialA &   expA(6).toBool;
    val isSigNaNA = isNaNA & ~ fractA(22).toBool;
    val sigA = Cat(~ isZeroA, fractA).toUFix;

    val signB  = io.b(32);
    val expB   = io.b(31,23).toUFix;
    val fractB = io.b(22,0).toUFix;
    val isZeroB = ( expB(8,6) === Bits("b000",3) );
    val isSpecialB = ( expB(8,7) === Bits("b11",2) );
    val isInfB = isSpecialB & ~ expB(6).toBool;
    val isNaNB = isSpecialB &   expB(6).toBool;
    val isSigNaNB = isNaNB & ~ fractB(22).toBool;
    val sigB = Cat(~ isZeroB, fractB).toUFix;

    val roundingMode_nearest_even = ( io.roundingMode === round_nearest_even );
    val roundingMode_minMag       = ( io.roundingMode === round_minMag       );
    val roundingMode_min          = ( io.roundingMode === round_min          );
    val roundingMode_max          = ( io.roundingMode === round_max          );

    val signOut = signA ^ signB;

    val sSumExps = expA + Cat(Fill(2, ~ expB(8)), expB(7,0)).toUFix;
    val notNeg_sumExps = sSumExps(8,0);
    val sigProd = sigA * sigB;
    val prodShift1 = sigProd(47).toBool;
    val sigProdX = Cat(sigProd(47,22), ( sigProd(21,0) != Bits(0) )).toUFix;

//*** FIRST TWO BITS NEEDED?
    val roundMask =
//*** OPTIMIZE.
        Cat(( notNeg_sumExps <= Bits("b001101001",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001101010",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001101011",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001101100",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001101101",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001101110",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001101111",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001110000",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001110001",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001110010",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001110011",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001110100",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001110101",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001110110",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001110111",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001111000",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001111001",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001111010",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001111011",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001111100",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001111101",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001111110",9).toUFix ),
         ( notNeg_sumExps <= Bits("b001111111",9).toUFix ),
         ( notNeg_sumExps <= Bits("b010000000",9).toUFix ),
         ( notNeg_sumExps <= Bits("b010000001",9).toUFix ) | prodShift1,
         Bits("b11",2));
    val roundPosMask = ~ Cat(Bits("b0",1), roundMask>>UFix(1)) & roundMask;
    val roundIncr =
          ( Mux(roundingMode_nearest_even, roundPosMask , Bits(0) )) |
          ( Mux( Mux(signOut, roundingMode_min , roundingMode_max ) , roundMask
                , Bits(0) ));
    val roundSigProdX = sigProdX + Cat(Bits("b0",1), roundIncr).toUFix;
    val roundPosBit = ( ( sigProdX & roundPosMask ) != Bits(0) );
    val anyRoundExtra = ( ( sigProdX & roundMask>>UFix(1) ) != Bits(0) );
    val roundInexact = roundPosBit | anyRoundExtra;
    val roundEven =
        roundingMode_nearest_even & roundPosBit & ! anyRoundExtra;
    val sigProdY =
        roundSigProdX>>UFix(2) & ~ ( Mux(roundEven, roundMask>>UFix(1) , roundMask>>UFix(2) ));
//*** COMPOUND ADD FOR `sSumExps'?
    val sExpY = sSumExps + sigProdY(25,24).toUFix;
    val expY = sExpY(8,0);
    val fractY = Mux(prodShift1, sigProdY(23,1) , sigProdY(22,0));

    val overflowY = ( sExpY(9,7) === Bits("b011",3) );
//*** CHANGE TO USE `sSumExps'/`notNeg_sumExps'?
    val totalUnderflowY = sExpY(9) | ( sExpY(8,0) < Bits("b001101011",9).toUFix );
    val underflowY =
//*** REPLACE?:
        totalUnderflowY |
//*** USE EARLIER BITS FROM `roundMask'?
              ( ( notNeg_sumExps <=
                         ( Mux(prodShift1, Bits("b010000000",9) , Bits("b010000001",9) ) ).toUFix) &
                      roundInexact );
    val inexactY = roundInexact;

    val overflowY_roundMagUp =
        roundingMode_nearest_even | ( roundingMode_min & signOut ) |
              ( roundingMode_max & ~ signOut );


    val mulSpecial = isSpecialA | isSpecialB;
    val commonCase = ~ mulSpecial & ~ isZeroA & ~ isZeroB;

    val common_invalid = ( isInfA & isZeroB ) | ( isZeroA & isInfB );
    val invalid = isSigNaNA | isSigNaNB | common_invalid;
    val overflow = commonCase & overflowY;
    val underflow = commonCase & underflowY;
//*** SPEED BY USING `commonCase & totalUnderflowY' INSTEAD OF `underflow'?
    val inexact = overflow | underflow | ( commonCase & inexactY );

    val notSpecial_isZeroOut = isZeroA | isZeroB | totalUnderflowY;
    val isSatOut = overflow & ~ overflowY_roundMagUp;
    val notNaN_isInfOut =
        isInfA | isInfB | ( overflow & overflowY_roundMagUp );
    val isNaNOut = isNaNA | isNaNB | common_invalid;

    val expOut =
        (   expY &
            ~ ( Mux(notSpecial_isZeroOut, Bits("b111000000",9) , Bits(0) )) &
            ~ ( Mux(isSatOut, Bits("b010000000",9) , Bits(0) )) &
            ~ ( Mux(notNaN_isInfOut, Bits("b001000000",9) , Bits(0) )) ) |
            ( Mux(isSatOut, Bits("b101111111",9) , Bits(0) )) |
            ( Mux(notNaN_isInfOut, Bits("b110000000",9) , Bits(0) )) |
            ( Mux(isNaNOut, Bits("b111000000",9) , Bits(0) ));
    val fractOut = fractY | ( Mux(isNaNOut | isSatOut, Bits("h7FFFFF",23) , Bits(0) ));
    io.out := Cat(signOut, expOut, fractOut);

    io.exceptionFlags := Cat(invalid, Bits("b0",1), overflow, underflow, inexact);

}
}


//*** THIS MODULE HAS NOT BEEN FULLY OPTIMIZED.

//*** DO THIS ANOTHER WAY?
package Fpu{
import Chisel._;
import Node._;
import LitConv._;
import addSubRecodedFloat64_1._;

object addSubRecodedFloat64_1 {
  val round_nearest_even = Bits("b00",2);
  val round_minMag       = Bits("b01",2);
  val round_min          = Bits("b10",2);
  val round_max          = Bits("b11",2);
}

class addSubRecodedFloat64_1_io() extends Bundle{
  val op = Bits(1 ,INPUT);
  val a = Bits(65 ,INPUT);
  val b = Bits(65 ,INPUT);
  val roundingMode = Bits(2 ,INPUT);
  val out = Bits(65, OUTPUT);
  val exceptionFlags = Bits(5, OUTPUT);
}

class addSubRecodedFloat64_1 extends Component{
    override val io = new addSubRecodedFloat64_1_io();
    val signA  = io.a(64);
    val expA   = io.a(63,52).toUFix;
    val fractA = io.a(51,0).toUFix;
    val isZeroA = ( expA(11,9) === Bits("b000",3) );
    val isSpecialA = ( expA(11,10) === Bits("b11",2) );
    val isInfA = isSpecialA & ~ expA(9).toBool;
    val isNaNA = isSpecialA &   expA(9).toBool;
    val isSigNaNA = isNaNA & ~ fractA(51).toBool;
    val sigA = Cat(~ isZeroA, fractA);

    val opSignB = io.op ^ io.b(64);
    val expB    = io.b(63,52).toUFix;
    val fractB  = io.b(51,0).toUFix;
    val isZeroB = ( expB(11,9) === Bits("b000",3) );
    val isSpecialB = ( expB(11,10) === Bits("b11",2) );
    val isInfB = isSpecialB & ~ expB(9).toBool;
    val isNaNB = isSpecialB &   expB(9).toBool;
    val isSigNaNB = isNaNB & ~ fractB(51).toBool;
    val sigB = Cat(~ isZeroB, fractB);

    val roundingMode_nearest_even = ( io.roundingMode === round_nearest_even );
    val roundingMode_minMag       = ( io.roundingMode === round_minMag       );
    val roundingMode_min          = ( io.roundingMode === round_min          );
    val roundingMode_max          = ( io.roundingMode === round_max          );


    //| `satAbsDiffExps' is the distance to shift the significand of the operand
    //| with the smaller exponent, maximized to 63.

//*** USE SIGN FROM `sSubExps'?
    val hasLargerExpB = ( expA < expB );
    val signLarger = Mux(hasLargerExpB, opSignB , signA).toBool;
    val expLarger  = Mux(hasLargerExpB, expB    , expA);
    val sigLarger  = Mux(hasLargerExpB, sigB    , sigA);
    val sigSmaller = Mux(hasLargerExpB, sigA    , sigB);

    val eqOpSigns = ( signA === opSignB );
    val sSubExps = Cat(Bits("b0",1), expA).toUFix - expB;
//*** IMPROVE?
    val overflowSubExps =
          ( sSubExps(12,6) != Bits(0) ) &
          ( ( sSubExps(12,6) != Bits("b1111111",7) ) | ( sSubExps(5,0) === Bits(0) ) );
    val wrapAbsDiffExps =
        Mux(hasLargerExpB, expB(5,0) - expA(5,0) , sSubExps(5,0));
    val satAbsDiffExps = wrapAbsDiffExps | ( Mux(overflowSubExps, UFix(63) , UFix(0) ));
    val doCloseSubMags =
        ~ eqOpSigns & ~ overflowSubExps & ( wrapAbsDiffExps <= UFix(1) );

    //-----------------------------------------------------------------
    //| The close-subtract case.
    //|   If the difference significand < 1, it must be exact (when normalized).
    //| If it is < 0 (negative), the round bit will in fact be 0.  If the
    //| difference significand is > 1, it may be inexact, but the rounding
    //| increment cannot carry out (because that would give a rounded difference
    //| >= 2, which is impossibly large).  Hence, the rounding increment can
    //| be done before normalization.  (A significand >= 1 is unaffected by
    //| normalization, whether done before or after rounding.)  The increment
    //| for negation and for rounding are combined before normalization.
    //------------------------------------------------------------------
//*** MASK SIGS TO SAVE ENERGY?  (ALSO EMPLOY LATER WHEN MERGING TWO PATHS.)
    val close_alignedSigSmaller =
        Mux( ( expA(0) === expB(0) ) , Cat(sigSmaller, Bits("b0",1)) , Cat(Bits("b0",1), sigSmaller)).toUFix;
    val close_sSigSum = Cat(Bits("b0",1), sigLarger, Bits("b0",1)).toUFix - close_alignedSigSmaller;
    val close_signSigSum = close_sSigSum(54).toBool;
    val close_pos_isNormalizedSigSum = close_sSigSum(53);
    val close_roundInexact =
        close_sSigSum(0) & close_pos_isNormalizedSigSum;
    val close_roundIncr =
        close_roundInexact &
              (   ( roundingMode_nearest_even & Bits(1)            ) |
                  ( roundingMode_minMag       & Bits(0)            ) |
                  ( roundingMode_min          &   signLarger ) |
                  ( roundingMode_max          & ~ signLarger )
              );
    val close_roundEven = roundingMode_nearest_even & close_roundInexact;
    val close_negSigSumA =
        Mux(close_signSigSum, ~ close_sSigSum(53,1) , close_sSigSum(53,1));
    val close_sigSumAIncr = close_signSigSum | close_roundIncr;
    val close_roundedAbsSigSumAN = close_negSigSumA + close_sigSumAIncr.toUFix;
    val close_roundedAbsSigSum =
        Cat(close_roundedAbsSigSumAN(52,1),
         close_roundedAbsSigSumAN(0) & ~ close_roundEven,
         close_sSigSum(0) & ~ close_pos_isNormalizedSigSum);
    val close_norm_in = Cat(close_roundedAbsSigSum, Bits("b0",10));
    val close_normalizeSigSum = new normalize64();
    close_normalizeSigSum.io.in := close_norm_in;
    val close_norm_count = close_normalizeSigSum.io.distance.toUFix;
    val close_norm_out = close_normalizeSigSum.io.out;

    val close_isZeroY = ~ close_norm_out(63).toBool;
    val close_signY = ~ close_isZeroY & ( signLarger ^ close_signSigSum );
//*** COMBINE EXP ADJUST ADDERS FOR CLOSE AND FAR PATHS?
    val close_expY = expLarger - close_norm_count;
    val close_fractY = close_norm_out(62,11);

    //--------------------------------------------------------------
    // The far/add case.
    //   `far_sigSum' has two integer bits and a value in the range (1/2, 4).
    //-------------------------------------------------------------
//*** MASK SIGS TO SAVE ENERGY?  (ALSO EMPLOY LATER WHEN MERGING TWO PATHS.)
//*** BREAK UP COMPUTATION OF EXTRA MASK?
    val far_roundExtraMask =
        Cat(( UFix(55) <= satAbsDiffExps ), ( UFix(54) <= satAbsDiffExps ),
         ( UFix(53) <= satAbsDiffExps ), ( UFix(52) <= satAbsDiffExps ),
         ( UFix(51) <= satAbsDiffExps ), ( UFix(50) <= satAbsDiffExps ),
         ( UFix(49) <= satAbsDiffExps ), ( UFix(48) <= satAbsDiffExps ),
         ( UFix(47) <= satAbsDiffExps ), ( UFix(46) <= satAbsDiffExps ),
         ( UFix(45) <= satAbsDiffExps ), ( UFix(44) <= satAbsDiffExps ),
         ( UFix(43) <= satAbsDiffExps ), ( UFix(42) <= satAbsDiffExps ),
         ( UFix(41) <= satAbsDiffExps ), ( UFix(40) <= satAbsDiffExps ),
         ( UFix(39) <= satAbsDiffExps ), ( UFix(38) <= satAbsDiffExps ),
         ( UFix(37) <= satAbsDiffExps ), ( UFix(36) <= satAbsDiffExps ),
         ( UFix(35) <= satAbsDiffExps ), ( UFix(34) <= satAbsDiffExps ),
         ( UFix(33) <= satAbsDiffExps ), ( UFix(32) <= satAbsDiffExps ),
         ( UFix(31) <= satAbsDiffExps ), ( UFix(30) <= satAbsDiffExps ),
         ( UFix(29) <= satAbsDiffExps ), ( UFix(28) <= satAbsDiffExps ),
         ( UFix(27) <= satAbsDiffExps ), ( UFix(26) <= satAbsDiffExps ),
         ( UFix(25) <= satAbsDiffExps ), ( UFix(24) <= satAbsDiffExps ),
         ( UFix(23) <= satAbsDiffExps ), ( UFix(22) <= satAbsDiffExps ),
         ( UFix(21) <= satAbsDiffExps ), ( UFix(20) <= satAbsDiffExps ),
         ( UFix(19) <= satAbsDiffExps ), ( UFix(18) <= satAbsDiffExps ),
         ( UFix(17) <= satAbsDiffExps ), ( UFix(16) <= satAbsDiffExps ),
         ( UFix(15) <= satAbsDiffExps ), ( UFix(14) <= satAbsDiffExps ),
         ( UFix(13) <= satAbsDiffExps ), ( UFix(12) <= satAbsDiffExps ),
         ( UFix(11) <= satAbsDiffExps ), ( UFix(10) <= satAbsDiffExps ),
         (  UFix(9) <= satAbsDiffExps ), (  UFix(8) <= satAbsDiffExps ),
         (  UFix(7) <= satAbsDiffExps ), (  UFix(6) <= satAbsDiffExps ),
         (  UFix(5) <= satAbsDiffExps ), (  UFix(4) <= satAbsDiffExps ),
         (  UFix(3) <= satAbsDiffExps ));
//*** USE `wrapAbsDiffExps' AND MASK RESULT?
    val far_alignedSigSmaller =
        Cat(Cat(sigSmaller, Bits("b0",2))>>satAbsDiffExps,
         ( ( sigSmaller & far_roundExtraMask ) != Bits(0) ));
    val far_negAlignedSigSmaller =
        Mux(eqOpSigns , Cat(Bits("b0",1), far_alignedSigSmaller),
              Cat(Bits("b1",1), ~ far_alignedSigSmaller)).toUFix;
    val far_sigSumIncr = ~ eqOpSigns;
    val far_sigSum =
        Cat(Bits("b0",1), sigLarger, Bits("b0",3)).toUFix + far_negAlignedSigSmaller + far_sigSumIncr.toUFix;
    val far_sumShift1  = far_sigSum(56).toBool;
    val far_sumShift0  = ( far_sigSum(56,55) === Bits("b01",2) );
    val far_sumShiftM1 = ( far_sigSum(56,55) === Bits("b00",2) );
    val far_fractX =
          ( Mux(far_sumShift1, Cat(far_sigSum(55,3), ( far_sigSum(2,0) != Bits(0) )) , Bits(0) )) |
          ( Mux(far_sumShift0, Cat(far_sigSum(54,2), ( far_sigSum(1,0) != Bits(0) )) , Bits(0) )) |
          ( Mux(far_sumShiftM1, far_sigSum(53,0)                            , Bits(0) ));

    val far_roundInexact = ( far_fractX(1,0) != Bits(0) );
    val far_roundIncr =
          ( roundingMode_nearest_even & far_fractX(1)                   ) |
          ( roundingMode_minMag       & Bits(0)                          ) |
          ( roundingMode_min          &   signLarger & far_roundInexact ) |
          ( roundingMode_max          & ~ signLarger & far_roundInexact );
    val far_roundEven =
        roundingMode_nearest_even & ( far_fractX(1,0) === Bits("b10",2) );
    val far_cFractYN = ( far_fractX.toUFix>>UFix(2) ) + far_roundIncr.toUFix;
    val far_roundCarry = far_cFractYN(52).toBool;
//*** COMBINE EXP ADJUST ADDERS FOR CLOSE AND FAR PATHS?
    val far_expAdjust =
          Mux( far_sumShift1 | ( far_sumShift0 & far_roundCarry ) , Bits(1) , Bits(0) ) |
          ( Mux(far_sumShiftM1 & ~ far_roundCarry, Bits("b111111111111",12) , Bits(0) ));
    val far_expY = expLarger + far_expAdjust.toUFix;
    val far_fractY =
        Cat(far_cFractYN(51,1), far_cFractYN(0) & ~ far_roundEven);


    val isZeroY = doCloseSubMags & close_isZeroY;
    val signY  = Mux(doCloseSubMags, close_signY  , signLarger);
    val expY   = Mux(doCloseSubMags, close_expY   , far_expY);
    val fractY = Mux(doCloseSubMags, close_fractY , far_fractY);
    val overflowY = ~ doCloseSubMags & ( far_expY(11,10) === Bits("b11",2) );
    val inexactY = Mux(doCloseSubMags, close_roundInexact , far_roundInexact);

    val overflowY_roundMagUp =
        roundingMode_nearest_even | ( roundingMode_min & signLarger ) |
              ( roundingMode_max & ~ signLarger );


    val addSpecial = isSpecialA | isSpecialB;
    val addZeros = isZeroA & isZeroB;
    val commonCase = ~ addSpecial & ~ addZeros;

    val common_invalid = isInfA & isInfB & ~ eqOpSigns;
    val invalid = isSigNaNA | isSigNaNB | common_invalid;
    val overflow = commonCase & overflowY;
    val inexact = overflow | ( commonCase & inexactY );

    val notSpecial_isZeroOut = addZeros | isZeroY;
    val isSatOut = overflow & ~ overflowY_roundMagUp;
    val notNaN_isInfOut =
        isInfA | isInfB | ( overflow & overflowY_roundMagUp );
    val isNaNOut = isNaNA | isNaNB | common_invalid;

    val signOut =
          ( eqOpSigns              & signA   ) |
          ( isNaNA                 & signA   ) |
          ( ~ isNaNA & isNaNB      & opSignB ) |
          ( isInfA & ~ isSpecialB  & signA   ) |
          ( ~ isSpecialA & isInfB  & opSignB ) |
          ( invalid                & Bits(0)       ) |
          ( addZeros & ~ eqOpSigns & Bits(0)       ) |
          ( commonCase             & signY   );
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

    io.exceptionFlags := Cat(invalid, Bits("b0",1), overflow, Bits("b0",1), inexact);

}
}


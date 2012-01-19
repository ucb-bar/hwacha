
//*** THIS MODULE HAS NOT BEEN FULLY OPTIMIZED.

//*** DO THIS ANOTHER WAY?
package Fpu{
import Chisel._;
import Node._;
import LitConv._;
import addSubRecodedFloat32_1._;

object addSubRecodedFloat32_1 {
  val round_nearest_even   = Bits("b00",2)
  val round_minMag         = Bits("b01",2)
  val round_min            = Bits("b10",2)
  val round_max            = Bits("b11",2)
}

class addSubRecodedFloat32_1_io() extends Bundle{
  val op = Bits(1, INPUT);
  val a = Bits(33, INPUT);
  val b = Bits(33, INPUT);
  val roundingMode = Bits(2, INPUT);
  val out = Bits(33, OUTPUT);
  val exceptionFlags = Bits(5, OUTPUT);
}

class addSubRecodedFloat32_1 extends Component {
  override val io = new addSubRecodedFloat32_1_io();
    val signA  = io.a(32);
    val expA   = io.a(31,23).toUFix;
    val fractA = io.a(22,0).toUFix;
    val isZeroA = ( expA(8,6) === Bits("b000",3) );
    val isSpecialA = ( expA(8,7) === Bits("b11",2) );
    val isInfA = isSpecialA & ~ expA(6).toBool;
    val isNaNA = isSpecialA &   expA(6).toBool;
    val isSigNaNA = isNaNA & ~ fractA(22).toBool;
    val sigA = Cat(~ isZeroA, fractA);

    val opSignB = io.op ^ io.b(32);
    val expB    = io.b(31,23).toUFix;
    val fractB  = io.b(22,0).toUFix;
    val isZeroB = ( expB(8,6) === Bits("b000",3) );
    val isSpecialB = ( expB(8,7) === Bits("b11",2) );
    val isInfB = isSpecialB & ~ expB(6).toBool;
    val isNaNB = isSpecialB &   expB(6).toBool;
    val isSigNaNB = isNaNB & ~ fractB(22).toBool;
    val sigB = Cat(~ isZeroB, fractB);

    val roundingMode_nearest_even = ( io.roundingMode === round_nearest_even );
    val roundingMode_minMag       = ( io.roundingMode === round_minMag       );
    val roundingMode_min          = ( io.roundingMode === round_min          );
    val roundingMode_max          = ( io.roundingMode === round_max          );

    //---------------------------------------
    // `satAbsDiffExps' is the distance to shift the significand of the operand
    // with the smaller exponent, maximized to 31.
    //---------------------------------------
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
          ( sSubExps(9,5) != Bits(0) ) &
          ( ( sSubExps(9,5) != Bits("b11111",5) ) | ( sSubExps(4,0) === Bits(0) ) );
    val wrapAbsDiffExps =
        Mux(hasLargerExpB, expB(4,0).toUFix - expA(4,0).toUFix , sSubExps(4,0).toUFix);
    val satAbsDiffExps = wrapAbsDiffExps | ( Mux(overflowSubExps, UFix(31) , UFix(0) ));
    val doCloseSubMags =
        ~ eqOpSigns & ~ overflowSubExps & ( wrapAbsDiffExps <= UFix(1) );

    //---------------------------------
    // The close-subtract case.
    //   If the difference significand < 1, it must be exact (when normalized).
    // If it is < 0 (negative), the round bit will in fact be 0.  If the
    // difference significand is > 1, it may be inexact, but the rounding
    // increment cannot carry out (because that would give a rounded difference
    // >= 2, which is impossibly large).  Hence, the rounding increment can
    // be done before normalization.  (A significand >= 1 is unaffected by
    // normalization, whether done before or after rounding.)  The increment
    // for negation and for rounding are combined before normalization.
    //-------------------------------------
//*** MASK SIGS TO SAVE ENERGY?  (ALSO EMPLOY LATER WHEN MERGING TWO PATHS.)
    val close_alignedSigSmaller =
        Mux( ( expA(0) === expB(0) ) , Cat(sigSmaller, Bits("b0",1)) , Cat(Bits("b0",1), sigSmaller)).toUFix;
    val close_sSigSum = Cat(Bits("b0",1), sigLarger, Bits("b0",1)).toUFix - close_alignedSigSmaller;
    val close_signSigSum = close_sSigSum(25).toBool;
    val close_pos_isNormalizedSigSum = close_sSigSum(24);
    val close_roundInexact =
        close_sSigSum(0) & close_pos_isNormalizedSigSum;
    val close_roundIncr =
        close_roundInexact &
              (   ( roundingMode_nearest_even & Bits(1)       ) |
                  ( roundingMode_minMag       & Bits(0)       ) |
                  ( roundingMode_min          &   signLarger ) |
                  ( roundingMode_max          & ~ signLarger )
              );
    val close_roundEven = roundingMode_nearest_even & close_roundInexact;
    val close_negSigSumA =
        Mux(close_signSigSum, ~ close_sSigSum(24,1) , close_sSigSum(24,1));
    val close_sigSumAIncr = close_signSigSum | close_roundIncr;
    val close_roundedAbsSigSumAN = close_negSigSumA.toUFix + close_sigSumAIncr.toUFix;
    val close_roundedAbsSigSum =
        Cat(close_roundedAbsSigSumAN(23,1),
         close_roundedAbsSigSumAN(0) & ~ close_roundEven,
         close_sSigSum(0) & ~ close_pos_isNormalizedSigSum);
    val close_norm_in = Cat(close_roundedAbsSigSum, Bits("d0",7));
    val close_normalizeSigSum = new normalize32();
    close_normalizeSigSum.io.in := close_norm_in;
    val close_norm_count = close_normalizeSigSum.io.distance;
    val close_norm_out = close_normalizeSigSum.io.out;
					    
    val close_isZeroY = ~ close_norm_out(31).toBool;
    val close_signY = ~ close_isZeroY & ( signLarger ^ close_signSigSum );
//*** COMBINE EXP ADJUST ADDERS FOR CLOSE AND FAR PATHS?
    val close_expY = expLarger.toUFix - close_norm_count.toUFix;
    val close_fractY = close_norm_out(30,8);

    /*------------------------------------------------------------------------*/
    // The far/add case.
    //   `far_sigSum' has two integer bits and a value in the range (1/2, 4).
    /*------------------------------------------------------------------------*/
//*** MASK SIGS TO SAVE ENERGY?  (ALSO EMPLOY LATER WHEN MERGING TWO PATHS.)
//*** BREAK UP COMPUTATION OF EXTRA MASK?
    val far_roundExtraMask =
        Cat(( UFix(26) <= satAbsDiffExps ), ( UFix(25) <= satAbsDiffExps ),
         ( UFix(24) <= satAbsDiffExps ), ( UFix(23) <= satAbsDiffExps ),
         ( UFix(22) <= satAbsDiffExps ), ( UFix(21) <= satAbsDiffExps ),
         ( UFix(20) <= satAbsDiffExps ), ( UFix(19) <= satAbsDiffExps ),
         ( UFix(18) <= satAbsDiffExps ), ( UFix(17) <= satAbsDiffExps ),
         ( UFix(16) <= satAbsDiffExps ), ( UFix(15) <= satAbsDiffExps ),
         ( UFix(14) <= satAbsDiffExps ), ( UFix(13) <= satAbsDiffExps ),
         ( UFix(12) <= satAbsDiffExps ), ( UFix(11) <= satAbsDiffExps ),
         ( UFix(10) <= satAbsDiffExps ), (  UFix(9) <= satAbsDiffExps ),
         (  UFix(8) <= satAbsDiffExps ), (  UFix(7) <= satAbsDiffExps ),
         (  UFix(6) <= satAbsDiffExps ), (  UFix(5) <= satAbsDiffExps ),
         (  UFix(4) <= satAbsDiffExps ), (  UFix(3) <= satAbsDiffExps ));
//*** USE `wrapAbsDiffExps' AND MASK RESULT?
    val far_alignedSigSmaller =
        Cat(Cat(sigSmaller, Bits("d0",2))>>satAbsDiffExps,
         ( ( sigSmaller & far_roundExtraMask ) != Bits(0) ));
    val far_negAlignedSigSmaller =
        Mux(eqOpSigns , Cat(Bits("b0",1), far_alignedSigSmaller),
                  Cat(Bits("b1",1), ~ far_alignedSigSmaller));
    val far_sigSumIncr = ~ eqOpSigns;
    val far_sigSum =
        Cat(Bits("b0",1), sigLarger, Bits("d0",3)).toUFix + far_negAlignedSigSmaller.toUFix + far_sigSumIncr.toUFix;
    val far_sumShift1  = far_sigSum(27).toBool;
    val far_sumShift0  = ( far_sigSum(27,26) === Bits("b01",2) );
    val far_sumShiftM1 = ( far_sigSum(27,26) === Bits("b00",2) );
    val far_fractX =
          ( Mux(far_sumShift1, Cat(far_sigSum(26,3), ( far_sigSum(2,0) != Bits(0) )) , Bits(0) )) |
          ( Mux(far_sumShift0, Cat(far_sigSum(25,2), ( far_sigSum(1,0) != Bits(0) )) , Bits(0) )) |
          ( Mux(far_sumShiftM1, far_sigSum(24,0)                            , Bits(0) ));

    val far_roundInexact = ( far_fractX(1,0) != Bits(0) );
    val far_roundIncr =
          ( roundingMode_nearest_even & far_fractX(1)                   ) |
          ( roundingMode_minMag       & Bits(0)                          ) |
          ( roundingMode_min          &   signLarger & far_roundInexact ) |
          ( roundingMode_max          & ~ signLarger & far_roundInexact );
    val far_roundEven =
        roundingMode_nearest_even & ( far_fractX(1,0) === Bits("b10",2) );
    val far_cFractYN = ( far_fractX.toUFix>>UFix(2) ) + far_roundIncr.toUFix;
    val far_roundCarry = far_cFractYN(23).toBool;
//*** COMBINE EXP ADJUST ADDERS FOR CLOSE AND FAR PATHS?
    val far_expAdjust =
          Mux( far_sumShift1 | ( far_sumShift0 & far_roundCarry ) , UFix(1)       , UFix(0) ) |
          ( Mux(far_sumShiftM1 & ~ far_roundCarry, Bits("b111111111",9).toUFix , UFix(0) ));
    val far_expY = expLarger + far_expAdjust;
    val far_fractY =
        Cat(far_cFractYN(22,1), far_cFractYN(0) & ~ far_roundEven);

    /*------------------------------------------------------------------------*/
    /*------------------------------------------------------------------------*/
    val isZeroY = doCloseSubMags & close_isZeroY;
    val signY  = Mux(doCloseSubMags, close_signY  , signLarger);
    val expY   = Mux(doCloseSubMags, close_expY   , far_expY);
    val fractY = Mux(doCloseSubMags, close_fractY , far_fractY);
    val overflowY = ~ doCloseSubMags & ( far_expY(8,7) === Bits("b11",2) );
    val inexactY = Mux(doCloseSubMags, close_roundInexact , far_roundInexact);

    val overflowY_roundMagUp =
        roundingMode_nearest_even | ( roundingMode_min & signLarger ) |
              ( roundingMode_max & ~ signLarger );

    /*------------------------------------------------------------------------*/
    /*------------------------------------------------------------------------*/
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
            ~ ( Mux(notSpecial_isZeroOut, Bits("b111000000",9) , Bits(0) )) &
            ~ ( Mux(isSatOut, Bits("b010000000",9) , Bits(0) )) &
            ~ ( Mux(notNaN_isInfOut, Bits("b001000000",9) , Bits(0) )) ) | 
            ( Mux(isSatOut, Bits("b101111111",9) , Bits(0) )) |
            ( Mux(notNaN_isInfOut, Bits("b110000000",9) , Bits(0) )) |
            ( Mux(isNaNOut, Bits("b111000000",9) , Bits(0) ));
    val fractOut = fractY | ( Mux(isNaNOut | isSatOut, Bits("h7FFFFF",23) , Bits(0) ));
    io.out := Cat(signOut, expOut, fractOut);

    io.exceptionFlags := Cat(invalid, Bits("b0",1), overflow, Bits("b0",1), inexact);
}
}


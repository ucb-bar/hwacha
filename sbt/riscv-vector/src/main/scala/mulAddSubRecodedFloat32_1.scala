
//*** THIS MODULE HAS NOT BEEN FULLY OPTIMIZED.

//*** DO THIS ANOTHER WAY?
package Fpu{

import Chisel._;
import Node._;
import mulAddSubRecodedFloat32_1._;

class mulAddSubRecodedFloat32_1_io() extends Bundle {
  val op = Bits(2, 'input);
  val a = Bits(33, 'input)
  val b = Bits(33, 'input);
  val c = Bits(33, 'input);
  val out = Bits(33, 'output);
  val roundingMode = Bits(2, 'input);
  val exceptionFlags = Bits(5, 'output);
}

object mulAddSubRecodedFloat32_1 {
  val round_nearest_even =  Bits("b00", 2);
  val round_minMag       =  Bits("b01", 2);
  val round_min          =  Bits("b10", 2);
  val round_max          =  Bits("b11", 2);
}

class mulAddSubRecodedFloat32_1 extends Component {
    override val io = new mulAddSubRecodedFloat32_1_io();
    val signA  = io.a(32);
    val expA   = io.a(31, 23).toUFix;
    val fractA = io.a(22, 0).toUFix;
    val isZeroA = ( expA(8,6) === Bits("b000", 3) );
    val isSpecialA = ( expA(8, 7) === Bits("b11", 2) );
    val isInfA = isSpecialA & ~ expA(6);
    val isNaNA = isSpecialA &   expA(6);
    val isSigNaNA = isNaNA & ~ fractA(22);
    val sigA = Cat(~ isZeroA, fractA).toUFix;

    val signB  = io.b(32);
    val expB   = io.b(31, 23).toUFix;
    val fractB = io.b(22, 0).toUFix;
    val isZeroB = ( expB(8, 6) === Bits("b000", 3) );
    val isSpecialB = ( expB(8, 7) === Bits("b11", 2) );
    val isInfB = isSpecialB & ~ expB(6);
    val isNaNB = isSpecialB &   expB(6);
    val isSigNaNB = isNaNB & ~ fractB(22);
    val sigB = Cat(~ isZeroB, fractB).toUFix;

    val opSignC = io.c(32) ^ io.op(0);
    val expC    = io.c(31, 23).toUFix;
    val fractC  = io.c(22, 0).toUFix;
    val isZeroC = ( expC(8, 6) === Bits("b000", 3) );
    val isSpecialC = ( expC(8, 7) === Bits("b11", 2) );
    val isInfC = isSpecialC & ~ expC(6);
    val isNaNC = isSpecialC &   expC(6);
    val isSigNaNC = isNaNC & ~ fractC(22);
    val sigC = Cat(~ isZeroC, fractC).toUFix;

    val roundingMode_nearest_even = ( io.roundingMode === round_nearest_even );
    val roundingMode_minMag       = ( io.roundingMode === round_minMag       );
    val roundingMode_min          = ( io.roundingMode === round_min          );
    val roundingMode_max          = ( io.roundingMode === round_max          );

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val signProd = signA ^ signB ^ io.op(1);
    val isZeroProd = isZeroA | isZeroB;
    //val sExpAlignedProd = expA + Cat(Fill(~ expB(8), 3), expB(7, 0)) + 27;
    val sExpAlignedProd = Cat(Fill(3, ~ expB(8)), expB(7, 0)).toUFix + expA + UFix(27);
    val sigProd = sigA * sigB;

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val doSubMags = signProd ^ opSignC;

    val sNatCAlignDist = sExpAlignedProd - expC;
    val CAlignDist_floor = isZeroProd | sNatCAlignDist(10);
    val CAlignDist_0 = CAlignDist_floor | ( sNatCAlignDist(9, 0) === Bits(0) );
    val isCDominant =
        ~ isZeroC & ( CAlignDist_floor | ( sNatCAlignDist(9, 0) < UFix(25) ) );
    val CAlignDist =
        Mux(  CAlignDist_floor, UFix(0), 
	Mux(( sNatCAlignDist(9, 0) < UFix(74) ), sNatCAlignDist, 
	      UFix(74)))(6,0);
    val sExpSum = Mux(CAlignDist_floor, expC, sExpAlignedProd);
// *** USE `sNatCAlignDist'?
    val CExtraMask =
        Cat(( UFix(74) === CAlignDist ), ( UFix(73) <= CAlignDist ),
         ( UFix(72) <= CAlignDist ), ( UFix(71) <= CAlignDist ),
         ( UFix(70) <= CAlignDist ), ( UFix(69) <= CAlignDist ),
         ( UFix(68) <= CAlignDist ), ( UFix(67) <= CAlignDist ),
         ( UFix(66) <= CAlignDist ), ( UFix(65) <= CAlignDist ),
         ( UFix(64) <= CAlignDist ), ( UFix(63) <= CAlignDist ),
         ( UFix(62) <= CAlignDist ), ( UFix(61) <= CAlignDist ),
         ( UFix(60) <= CAlignDist ), ( UFix(59) <= CAlignDist ),
         ( UFix(58) <= CAlignDist ), ( UFix(57) <= CAlignDist ),
         ( UFix(56) <= CAlignDist ), ( UFix(55) <= CAlignDist ),
         ( UFix(54) <= CAlignDist ), ( UFix(53) <= CAlignDist ),
         ( UFix(52) <= CAlignDist ), ( UFix(51) <= CAlignDist ));
    val negSigC = Mux(doSubMags, ~ sigC, sigC);
    val alignedNegSigC =
        Cat(Cat(Fill(74, doSubMags), negSigC, Fill(50, doSubMags))>>CAlignDist,
            ( ( sigC & CExtraMask ) != Bits(0) ) ^ doSubMags)(74, 0);

    val sigSum = (alignedNegSigC.toUFix + ( sigProd<<UFix(1) ))(74,0);

    val estNormPos_a = Cat(doSubMags, alignedNegSigC(49, 1));
    val estNormPos_b = sigProd;
    val estNormPosSigSum = new estNormDistP24PosSum50();
    estNormPosSigSum.io.a := estNormPos_a;
    estNormPosSigSum.io.b := estNormPos_b;
    val estNormPos_dist = estNormPosSigSum.io.out.toUFix;

    val estNormNeg_a = Cat(Bits("b1", 1), alignedNegSigC(49, 1));
    val estNormNeg_b = sigProd;
    val estNormNegSigSum = new estNormDistP24NegSum50();
    estNormNegSigSum.io.a := estNormNeg_a;
    estNormNegSigSum.io.b := estNormNeg_b;
    val estNormNeg_dist = estNormNegSigSum.io.out.toUFix;

    val firstReduceSigSum = Cat(( sigSum(33, 18) != Bits(0) ), ( sigSum(17, 0) != Bits(0) ));
    val notSigSum = ~ sigSum;
    val firstReduceNotSigSum =
        Cat(( notSigSum(33, 18) != Bits(0) ), ( notSigSum(17, 0) != Bits(0) ));
  // *** USE RESULT OF `CAlignDest - 1' TO TEST FOR ZERO?
    val CDom_estNormDist =
        Mux(CAlignDist_0 | doSubMags, CAlignDist, CAlignDist - UFix(1))(4,0);
    val CDom_firstNormAbsSigSum =
          ( Mux(~ doSubMags & ~ CDom_estNormDist(4),
              Cat(sigSum(74, 34), ( firstReduceSigSum != Bits(0) )),
              Bits(0)) 
	  ) | 
	  ( Mux(~ doSubMags & CDom_estNormDist(4),
              Cat(sigSum(58, 18), firstReduceSigSum(0)),
              Bits(0)) 
	  ) | 
	  ( Mux(doSubMags & ~ CDom_estNormDist(4),
              Cat(notSigSum(74, 34), ( firstReduceNotSigSum != Bits(0) )),
              Bits(0)) 
	  )|
          ( Mux(doSubMags & CDom_estNormDist(4),
               Cat(notSigSum(58, 18), firstReduceNotSigSum(0)),
               Bits(0))
	  );
       
    //------------------------------------------------------------------------
    // (For this case, bits above `sigSum(50)' are never interesting.  Also,
    // if there is any significant cancellation, then `sigSum(0)' must equal
    // `doSubMags'.)
    //------------------------------------------------------------------------
    val notCDom_pos_firstNormAbsSigSum =
          ( Mux(( estNormPos_dist(5, 4) === Bits("b01", 2) ),
               Cat(sigSum(50, 18),
                   Mux(doSubMags , ~ firstReduceNotSigSum(0) , firstReduceSigSum(0))),
               Bits(0))) |
          ( Mux(( estNormPos_dist(5, 4) === Bits("b10", 2) ) , sigSum(42, 1) , Bits(0) )) |
          ( Mux(( estNormPos_dist(5, 4) === Bits("b11", 2) ) , Cat(sigSum(26, 1), Fill(16, doSubMags)), 
                 Bits(0) )) |
          ( Mux(( estNormPos_dist(5, 4) === Bits("b00", 2) ) , Cat(sigSum(10, 1), Fill(32, doSubMags)),
                   Bits(0) ));
    //------------------------------------------------------------------------
    // (For this case, bits above `notSigSum(49)' are never interesting.  Also,
    // if there is any significant cancellation, then `notSigSum(0)' must be
    // zero.)
    //------------------------------------------------------------------------

    val notCDom_neg_cFirstNormAbsSigSum =
          Mux(( estNormNeg_dist(5, 4) === Bits("b01", 2) ),
                Cat(Bits(0, 10), notSigSum(49, 18), firstReduceNotSigSum(0)),
                 Bits(0)) |
          Mux(( estNormNeg_dist(5, 4) === Bits("b10", 2) ) , notSigSum(43, 1)     , Bits(0) ) |
          Mux(( estNormNeg_dist(5, 4) === Bits("b11", 2) ) , (notSigSum(27, 1))<<UFix(16) , Bits(0) ) |
          Mux(( estNormNeg_dist(5, 4) === Bits("b00", 2) ) , (notSigSum(11, 1))<<UFix(32), Bits(0) );
    val notCDom_signSigSum = sigSum(51);
    val doNegSignSum =
        Mux(isCDominant, doSubMags & ~ isZeroC, notCDom_signSigSum);
    val estNormDist =
          Mux(  isCDominant                       , CDom_estNormDist, UFix(0) ) |
          Mux(~ isCDominant & ~ notCDom_signSigSum, estNormPos_dist , UFix(0) ) |
          Mux(~ isCDominant &   notCDom_signSigSum, estNormNeg_dist , UFix(0) );
    val cFirstNormAbsSigSum =
          Mux(isCDominant , CDom_firstNormAbsSigSum , Bits(0) ) |
          ( Mux(~ isCDominant & ~ notCDom_signSigSum, 
                notCDom_pos_firstNormAbsSigSum,
                Bits(0))
          ) |
          ( Mux(~ isCDominant & notCDom_signSigSum,
                notCDom_neg_cFirstNormAbsSigSum,
                Bits(0))
          );
    val doIncrSig = ~ isCDominant & ~ notCDom_signSigSum & doSubMags;
    val normTo2ShiftDist = ~ estNormDist(3, 0);
    val absSigSumExtraMask =
        Cat(( estNormDist(3, 0) ===  Bits(0) ), ( estNormDist(3, 0) <=  UFix(1) ),
         ( estNormDist(3, 0) <=  UFix(2) ), ( estNormDist(3, 0) <=  UFix(3) ),
         ( estNormDist(3, 0) <=  UFix(4) ), ( estNormDist(3, 0) <=  UFix(5) ),
         ( estNormDist(3, 0) <=  UFix(6) ), ( estNormDist(3, 0) <=  UFix(7) ),
         ( estNormDist(3, 0) <=  UFix(8) ), ( estNormDist(3, 0) <=  UFix(9) ),
         ( estNormDist(3, 0) <= UFix(10) ), ( estNormDist(3, 0) <= UFix(11) ),
         ( estNormDist(3, 0) <= UFix(12) ), ( estNormDist(3, 0) <= UFix(13) ),
         ( estNormDist(3, 0) <= UFix(14) ), Bits("b1", 1));
    val sigX3 =
        Cat(cFirstNormAbsSigSum(42, 1)>>normTo2ShiftDist,
         Mux(doIncrSig,
            ( ( ~ cFirstNormAbsSigSum(15, 0) & absSigSumExtraMask ) === Bits(0) ),
            ( (   cFirstNormAbsSigSum(15, 0) & absSigSumExtraMask ) != Bits(0) )))(27, 0);
    val sigX3Shift1 = ( sigX3(27, 26) === Bits(0) );
    val sExpX3 = sExpSum - estNormDist;

    val isZeroY = ( sigX3(27, 25) === Bits(0) );
    val signY = ~ isZeroY & ( signProd ^ doNegSignSum );
    val roundMask =
          Mux(sExpX3(10) , Bits("h7FFFFFF", 27),
          Cat( (sExpX3(9, 0) <= Bits("b0001101010", 10).toUFix ) ,
           ( sExpX3(9, 0) <= Bits("b0001101011", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001101100", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001101101", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001101110", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001101111", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001110000", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001110001", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001110010", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001110011", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001110100", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001110101", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001110110", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001110111", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001111000", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001111001", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001111010", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001111011", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001111100", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001111101", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001111110", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0001111111", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0010000000", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0010000001", 10).toUFix  ),
           ( sExpX3(9, 0) <= Bits("b0010000010", 10).toUFix  ) | sigX3(26),
           Bits("b11", 2)));
    val roundPosMask = ~ Cat(Bits("b0", 1), roundMask>>UFix(1)) & roundMask;
    val roundPosBit = ( ( sigX3 & roundPosMask ) != Bits(0, 28) );
    val anyRoundExtra = ( (   sigX3 & roundMask>>UFix(1) ) != Bits(0, 28) );
    val allRoundExtra = ( ( ~ sigX3 & roundMask>>UFix(1) ) === Bits(0, 28 ));
    val anyRound = roundPosBit | anyRoundExtra;
    val allRound = roundPosBit & allRoundExtra;
    val roundDirectUp = Mux(signY, roundingMode_min, roundingMode_max)
    val roundUp =
          ( ~ doIncrSig & roundingMode_nearest_even
                                               & roundPosBit & anyRoundExtra ) |
          ( ~ doIncrSig & roundDirectUp             & anyRound    ) |
          (   doIncrSig                             & allRound    ) |
          (   doIncrSig & roundingMode_nearest_even & roundPosBit ) |
          (   doIncrSig & roundDirectUp             & Bool(true)   );
    val roundEven =
        Mux(doIncrSig,
              roundingMode_nearest_even & ~ roundPosBit &   allRoundExtra,
              roundingMode_nearest_even &   roundPosBit & ~ anyRoundExtra);
    val roundInexact = Mux(doIncrSig, ~ allRound, anyRound);
    val roundUp_sigY3 = ( sigX3>>UFix(2) | roundMask>>UFix(2) ).toUFix + UFix(1);
    val sigY3 =
          ( Mux(~ roundUp & ~ roundEven , ( sigX3 & ~ roundMask )>>UFix(2)         , Bits(0) )) |
          ( Mux(roundUp                 , roundUp_sigY3                      , Bits(0) )) |
          ( Mux(roundEven               , roundUp_sigY3 & ~ ( roundMask>>UFix(1) ) , Bits(0) ));
  //  *** HANDLE DIFFERENTLY?  (NEED TO ACCOUNT FOR ROUND-EVEN ZEROING MSB.)
    val sExpY =
           Mux(sigY3(25)              , sExpX3 + UFix(1) , UFix(0) ) |
           Mux(sigY3(24)              , sExpX3           , UFix(0) ) |
           Mux( (sigY3(25, 24) === Bits(0) ) , sExpX3 - UFix(1) , UFix(0) );
    val expY = sExpY(8, 0);
    val fractY = Mux(sigX3Shift1, sigY3(22, 0), sigY3(23, 1))

    val overflowY = ( sExpY(9, 7) === Bits("b011", 3) );
  // *** HANDLE DIFFERENTLY?  (NEED TO ACCOUNT FOR ROUND-EVEN ZEROING MSB.)
    val totalUnderflowY = sExpY(9) | ( sExpY(8, 0) < Bits("b001101011", 9).toUFix );
    val underflowY =
        ( sExpX3(10) |
                ( sExpX3(9, 0) <=
                         ( Mux(sigX3Shift1 , Bits("b0010000010", 10) , Bits("b0010000001", 10) ) ).toUFix )) &
              roundInexact;
    val inexactY = roundInexact;

    val overflowY_roundMagUp =
        roundingMode_nearest_even | ( roundingMode_min & signY ) |
              ( roundingMode_max & ~ signY );

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val mulSpecial = isSpecialA | isSpecialB;
    val addSpecial = mulSpecial | isSpecialC;
    val notSpecial_addZeros = isZeroProd & isZeroC;
    val commonCase = ~ addSpecial & ~ notSpecial_addZeros;

    val notSigNaN_invalid =
          ( isInfA & isZeroB ) |
          ( isZeroA & isInfB ) |
          ( ~ isNaNA & ~ isNaNB & ( isInfA | isInfB ) & isInfC & doSubMags );
    val invalid = isSigNaNA | isSigNaNB | isSigNaNC | notSigNaN_invalid;
    val overflow = commonCase & overflowY;
    val underflow = commonCase & underflowY;
    val inexact = overflow | ( commonCase & inexactY );

    val notSpecial_isZeroOut =
        notSpecial_addZeros | isZeroY | totalUnderflowY;
    val isSatOut = overflow & ~ overflowY_roundMagUp;
    val notNaN_isInfOut =
        isInfA | isInfB | isInfC | ( overflow & overflowY_roundMagUp );
    val isNaNOut = isNaNA | isNaNB | isNaNC | notSigNaN_invalid;

    val signOut =
          ( ~ doSubMags                                    & opSignC  ) |
          ( ( isNaNA | isNaNB )                            & signProd ) |
          ( ~ isNaNA & ~ isNaNB & isNaNC                   & opSignC  ) |
          ( mulSpecial & ~ isSpecialC                      & signProd ) |
          ( ~ mulSpecial & isSpecialC                      & opSignC  ) |
          ( ~ mulSpecial & notSpecial_addZeros & doSubMags & Bits(0)        ) |
          ( commonCase                                     & signY    );
    val expOut =
        (   expY &
            ~  Mux(notSpecial_isZeroOut, Bits("b111000000", 9), Bits(0))  &
            ~  Mux(isSatOut            , Bits("b010000000", 9), Bits(0))  &
            ~  Mux(notNaN_isInfOut     , Bits("b001000000", 9), Bits(0))  ) | 
            Mux(isSatOut       , Bits("b101111111", 9), Bits(0))  |
            Mux(notNaN_isInfOut, Bits("b110000000", 9), Bits(0))  |
            Mux(isNaNOut       , Bits("b111000000", 9), Bits(0));
    val fractOut = fractY | ( Mux(isNaNOut | isSatOut , Bits("h7FFFFF",23) , Bits(0) ));
    io.out := Cat(signOut, expOut, fractOut);

    io.exceptionFlags := Cat(invalid, Bits("b0", 1), overflow, underflow, inexact);

}
}

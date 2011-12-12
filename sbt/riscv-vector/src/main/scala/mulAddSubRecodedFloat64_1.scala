
//*** THIS MODULE HAS NOT BEEN FULLY OPTIMIZED.

//*** DO THIS ANOTHER WAY?
package Fpu{

import Chisel._;
import Node._;
import mulAddSubRecodedFloat64_1._;

class mulAddSubRecodedFloat64_1_io() extends Bundle {
  val op = Bits(2, 'input);
  val a = Bits(65, 'input)
  val b = Bits(65, 'input);
  val c = Bits(65, 'input);
  val roundingMode = Bits(2, 'input);
  val out = Bits(65, 'output);
  val exceptionFlags = Bits(5, 'output);
}

object mulAddSubRecodedFloat64_1 {
  val round_nearest_even =  Bits("b00", 2);
  val round_minMag       =  Bits("b01", 2);
  val round_min          =  Bits("b10", 2);
  val round_max          =  Bits("b11", 2);
}

class mulAddSubRecodedFloat64_1 extends Component{
  override val io = new mulAddSubRecodedFloat64_1_io();
    val signA  = io.a(64);
    val expA   = io.a(63, 52).toUFix;
    val fractA = io.a(51, 0).toUFix;
    val isZeroA = ( expA(11, 9) === Bits("b000", 3) );
    val isSpecialA = ( expA(11, 10) === Bits("b11", 2) );
    val isInfA = isSpecialA & ~ expA(9);
    val isNaNA = isSpecialA &   expA(9);
    val isSigNaNA = isNaNA & ~ fractA(51);
    val sigA = Cat(~ isZeroA, fractA).toUFix;

    val signB  = io.b(64);
    val expB   = io.b(63, 52).toUFix;
    val fractB = io.b(51, 0).toUFix;
    val isZeroB = ( expB(11, 9) === Bits("b000", 3) );
    val isSpecialB = ( expB(11, 10) === Bits("b11", 2) );
    val isInfB = isSpecialB & ~ expB(9);
    val isNaNB = isSpecialB &   expB(9);
    val isSigNaNB = isNaNB & ~ fractB(51);
    val sigB = Cat(~ isZeroB, fractB).toUFix;

    val opSignC  = io.c(64) ^ io.op(0);
    val expC   = io.c(63, 52).toUFix;
    val fractC = io.c(51, 0).toUFix;
    val isZeroC = ( expC(11, 9) === Bits("b000", 3) );
    val isSpecialC = ( expC(11, 10) === Bits("b11", 2) );
    val isInfC = isSpecialC & ~ expC(9);
    val isNaNC = isSpecialC &   expC(9);
    val isSigNaNC = isNaNC & ~ fractC(51);
    val sigC = Cat(~ isZeroC, fractC).toUFix;

    val roundingMode_nearest_even = ( io.roundingMode === round_nearest_even );
    val roundingMode_minMag       = ( io.roundingMode === round_minMag       );
    val roundingMode_min          = ( io.roundingMode === round_min          );
    val roundingMode_max          = ( io.roundingMode === round_max          );

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val signProd = signA ^ signB ^ io.op(1);
    val isZeroProd = isZeroA | isZeroB;
    // val sExpAlignedProd = expA + Cat(Fill(~ expB(11), 3), expB(10, 0)) + 56;
    val sExpAlignedProd = Cat(Fill(3, ~ expB(11)), expB(10, 0)).toUFix + expA + UFix(56);

    val sigProd = sigA * sigB;

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val doSubMags = signProd ^ opSignC;

    val sNatCAlignDist = sExpAlignedProd - expC;
    val CAlignDist_floor = isZeroProd | sNatCAlignDist(13);
    val CAlignDist_0 = CAlignDist_floor | ( sNatCAlignDist(12, 0) === Bits(0) );
    val isCDominant =
        ~ isZeroC & ( CAlignDist_floor | ( sNatCAlignDist(12, 0) < UFix(54) ) );
    val CAlignDist =
          Mux(CAlignDist_floor, UFix(0),
          Mux(( sNatCAlignDist(12, 0) < UFix(161, 13) ), sNatCAlignDist,
          UFix(161)))(7, 0);
    val sExpSum = Mux(CAlignDist_floor, expC, sExpAlignedProd);

    val CExtraMaskGen = Fill(53, Bits(1)) >> (UFix(161) - CAlignDist);

// *** USE `sNatCAlignDist'?
    val CExtraMask = 
        Cat(( UFix(161) === CAlignDist ), ( UFix(160) <= CAlignDist ),
         ( UFix(159) <= CAlignDist ), ( UFix(158) <= CAlignDist ),
         ( UFix(157) <= CAlignDist ), ( UFix(156) <= CAlignDist ),
         ( UFix(155) <= CAlignDist ), ( UFix(154) <= CAlignDist ),
         ( UFix(153) <= CAlignDist ), ( UFix(152) <= CAlignDist ),
         ( UFix(151) <= CAlignDist ), ( UFix(150) <= CAlignDist ),
         ( UFix(149) <= CAlignDist ), ( UFix(148) <= CAlignDist ),
         ( UFix(147) <= CAlignDist ), ( UFix(146) <= CAlignDist ),
         ( UFix(145) <= CAlignDist ), ( UFix(144) <= CAlignDist ),
         ( UFix(143) <= CAlignDist ), ( UFix(142) <= CAlignDist ),
         ( UFix(141) <= CAlignDist ), ( UFix(140) <= CAlignDist ),
         ( UFix(139) <= CAlignDist ), ( UFix(138) <= CAlignDist ),
         ( UFix(137) <= CAlignDist ), ( UFix(136) <= CAlignDist ),
         ( UFix(135) <= CAlignDist ), ( UFix(134) <= CAlignDist ),
         ( UFix(133) <= CAlignDist ), ( UFix(132) <= CAlignDist ),
  	 ( UFix(131) <= CAlignDist ), ( UFix(130) <= CAlignDist ),
         ( UFix(129) <= CAlignDist ), ( UFix(128) <= CAlignDist ),
         ( UFix(127) <= CAlignDist ), ( UFix(126) <= CAlignDist ),
         ( UFix(125) <= CAlignDist ), ( UFix(124) <= CAlignDist ),
         ( UFix(123) <= CAlignDist ), ( UFix(122) <= CAlignDist ),
         ( UFix(121) <= CAlignDist ), ( UFix(120) <= CAlignDist ),
	 ( UFix(119) <= CAlignDist ), ( UFix(118) <= CAlignDist ),
         ( UFix(117) <= CAlignDist ), ( UFix(116) <= CAlignDist ),
         ( UFix(115) <= CAlignDist ), ( UFix(114) <= CAlignDist ),
         ( UFix(113) <= CAlignDist ), ( UFix(112) <= CAlignDist ),
         ( UFix(111) <= CAlignDist ), ( UFix(110) <= CAlignDist ),
         ( UFix(109) <= CAlignDist ));
    val negSigC = Mux(doSubMags, ~ sigC, sigC);
    val alignedNegSigC =
        Cat(Cat(Fill(161, doSubMags), negSigC, Fill(108, doSubMags))>>CAlignDist,
         ( ( sigC & CExtraMask ) != Bits(0) ) ^ doSubMags)(161, 0);

    // val sigSum = (alignedNegSigC + ( sigProd<<1 ))(161,0); // BCR Tmp Fix?

    val sigSum = (alignedNegSigC.toUFix + Cat(sigProd, Bits(0,1)).toUFix)(161,0); // BCR Tmp Fix?

    val estNormPos_a = Cat(doSubMags, alignedNegSigC(107, 1));
    val estNormPos_b = sigProd;
    val estNormPosSigSum = new estNormDistP53PosSum108();
    estNormPosSigSum.io.a := estNormPos_a;
    estNormPosSigSum.io.b := estNormPos_b;
    val estNormPos_dist = estNormPosSigSum.io.out.toUFix;

    val estNormNeg_a = Cat(Bits("b1", 1), alignedNegSigC(107, 1));
    val estNormNeg_b = sigProd;
    val estNormNegSigSum = new estNormDistP53NegSum108();
    estNormNegSigSum.io.a := estNormNeg_a;
    estNormNegSigSum.io.b := estNormNeg_b;
    val estNormNeg_dist = estNormNegSigSum.io.out.toUFix;

    val firstReduceSigSum = Cat(( sigSum(75, 44) != Bits(0) ), ( sigSum(43, 0) != Bits(0) ));
    val notSigSum = ~ sigSum;
    val firstReduceNotSigSum =
        Cat(( notSigSum(75, 44) != Bits(0) ), ( notSigSum(43, 0) != Bits(0) ));
//*** USE RESULT OF `CAlignDest - 1' TO TEST FOR ZERO?
    val CDom_estNormDist =
        Mux(CAlignDist_0 | doSubMags, CAlignDist, (CAlignDist - UFix(1))(5, 0));
    val CDom_firstNormAbsSigSum =
          ( Mux(~ doSubMags & ~ CDom_estNormDist(5), 
	    Cat(sigSum(161, 76), ( firstReduceSigSum != Bits(0) )),
            Bits(0))
          ) |
          ( Mux(~ doSubMags & CDom_estNormDist(5),
            Cat(sigSum(129, 44), firstReduceSigSum(0)),
            Bits(0))
          ) |
          ( Mux(doSubMags & ~ CDom_estNormDist(5),
            Cat(notSigSum(161, 76), ( firstReduceNotSigSum != Bits(0) )),
            Bits(0))
          ) |
          ( Mux(doSubMags & CDom_estNormDist(5),
            Cat(notSigSum(129, 44), firstReduceNotSigSum(0)),
            Bits(0))
          );
    //------------------------------------------------------------------------
    // (For this case, bits above `sigSum(108)' are never interesting.  Also,
    // if there is any significant cancellation, then `sigSum(0)' must equal
    // `doSubMags'.)
    //------------------------------------------------------------------------
    val notCDom_pos_firstNormAbsSigSum =
          ( Mux(( estNormPos_dist(6, 4) === Bits("b011", 3) ),
            	  Cat(sigSum(108, 44),
               	      Mux(doSubMags, ~ firstReduceNotSigSum(0), firstReduceSigSum(0))),
          	  Bits(0))
          ) |
          ( Mux(( estNormPos_dist(6, 5) === Bits("b10", 2) ),
            	  Cat(sigSum(97, 12),
               	      Mux(doSubMags , ( notSigSum(11, 1) === Bits(0) ) , ( sigSum(11, 1) != Bits(0) ))),
            	  Bits(0))
          ) |
          ( Mux(( estNormPos_dist(6, 5) === Bits("b11", 2) ) , Cat(sigSum(65, 1), Fill(22, doSubMags)),
                  Bits(0) )) |
          ( Mux(( estNormPos_dist(6, 5) === Bits("b00", 2) ) , Cat(sigSum(33, 1), Fill(54, doSubMags)),
                  Bits(0) )) |
          ( Mux(( estNormPos_dist(6, 4) === Bits("b010", 3) ) , Cat(sigSum(1), Fill(86, doSubMags)),
                  Bits(0) ));
    //------------------------------------------------------------------------
    // (For this case, bits above `notSigSum(107)' are never interesting.
    // Also, if there is any significant cancellation, then `notSigSum(0)' must
    // be zero.)
    //------------------------------------------------------------------------
    val notCDom_neg_cFirstNormAbsSigSum =
          Mux(( estNormNeg_dist(6, 4) === Bits("b011", 3) ),
		Cat(notSigSum(107, 44), firstReduceNotSigSum(0)), Bits(0)) |
          Mux(( estNormNeg_dist(6, 5) === Bits("b10", 2) ),
		Cat(notSigSum(98, 12), ( notSigSum(11, 1) != Bits(0) )), Bits(0)) |
          Mux(( estNormNeg_dist(6, 5) === Bits("b11", 2) ),
		Cat(notSigSum(66, 1), Fill(22, Bits(0,1))), Bits(0) ) |
          Mux(( estNormNeg_dist(6, 5) === Bits("b00", 2) ),
		Cat(notSigSum(34, 1), Fill(54,Bits(0,1))), Bits(0) ) |
          Mux(( estNormNeg_dist(6, 4) === Bits("b010", 3) ),
		Cat(notSigSum(2, 1),  Fill(86,Bits(0,1))), Bits(0) );
    val notCDom_signSigSum = sigSum(109);
    val doNegSignSum =
        Mux(isCDominant, doSubMags & ~ isZeroC, notCDom_signSigSum);
    val estNormDist =
          Mux(  isCDominant                       , CDom_estNormDist, UFix(0) ) |
          Mux(~ isCDominant & ~ notCDom_signSigSum, estNormPos_dist , UFix(0) ) |
          Mux(~ isCDominant &   notCDom_signSigSum, estNormNeg_dist , UFix(0) );
    val cFirstNormAbsSigSum =
          Mux(isCDominant, CDom_firstNormAbsSigSum, Bits(0) ) |
          ( Mux(~ isCDominant & ~ notCDom_signSigSum,
            	  notCDom_pos_firstNormAbsSigSum,
            	  Bits(0))
          ) |
          ( Mux(~ isCDominant & notCDom_signSigSum,
            	  notCDom_neg_cFirstNormAbsSigSum,
            	  Bits(0))
          );
    val doIncrSig = ~ isCDominant & ~ notCDom_signSigSum & doSubMags;
    val estNormDist_5 = estNormDist(4, 0).toUFix;
    val normTo2ShiftDist = ~ estNormDist_5;
    val absSigSumExtraMask =
        Cat( 
         ( estNormDist_5 === UFix(0) ), ( estNormDist_5 <=  UFix(1) ),
         ( estNormDist_5 <=  UFix(2) ), ( estNormDist_5 <=  UFix(3) ),
         ( estNormDist_5 <=  UFix(4) ), ( estNormDist_5 <=  UFix(5) ),
         ( estNormDist_5 <=  UFix(6) ), ( estNormDist_5 <=  UFix(7) ),
         ( estNormDist_5 <=  UFix(8) ), ( estNormDist_5 <=  UFix(9) ),
         ( estNormDist_5 <= UFix(10) ), ( estNormDist_5 <= UFix(11) ),
         ( estNormDist_5 <= UFix(12) ), ( estNormDist_5 <= UFix(13) ),
         ( estNormDist_5 <= UFix(14) ), ( estNormDist_5 <= UFix(15) ),
         ( estNormDist_5 <= UFix(16) ), ( estNormDist_5 <= UFix(17) ),
         ( estNormDist_5 <= UFix(18) ), ( estNormDist_5 <= UFix(19) ),
         ( estNormDist_5 <= UFix(20) ), ( estNormDist_5 <= UFix(21) ),
         ( estNormDist_5 <= UFix(22) ), ( estNormDist_5 <= UFix(23) ),
         ( estNormDist_5 <= UFix(24) ), ( estNormDist_5 <= UFix(25) ),
         ( estNormDist_5 <= UFix(26) ), ( estNormDist_5 <= UFix(27) ),
         ( estNormDist_5 <= UFix(28) ), ( estNormDist_5 <= UFix(29) ),
         ( estNormDist_5 <= UFix(30) ), Bits("b1", 1));
    //val absSigSumExtraMaskGen = Fill(Bits(1,1), (UFix(32, 6) - estNormDist_5));
    val sigX3 =
        Cat(cFirstNormAbsSigSum(87, 1)>>normTo2ShiftDist,
         Mux(doIncrSig,
         ( ( ~ cFirstNormAbsSigSum(31, 0) & absSigSumExtraMask ) === Bits(0) ),
         ( (   cFirstNormAbsSigSum(31, 0) & absSigSumExtraMask ) != Bits(0) )))(56, 0);
    val sigX3Shift1 = ( sigX3(56, 55) === Bits(0) );
    val sExpX3 = sExpSum - estNormDist;

    val isZeroY = ( sigX3(56, 54) === Bits(0) );
    val signY = ~ isZeroY & ( signProd ^ doNegSignSum );
    val sExpX3_13 = sExpX3(12, 0);
    val roundMask =
          Mux(sExpX3(13) , Bits("hFFFFFFFFFFFFFF", 56),
          Cat(
           ( sExpX3_13 <= Bits("b0001111001101", 13).toUFix ), // 973 = 3*256(768) + 12*16(192) + 13
           ( sExpX3_13 <= Bits("b0001111001110", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111001111", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111010000", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111010001", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111010010", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111010011", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111010100", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111010101", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111010110", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111010111", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111011000", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111011001", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111011010", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111011011", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111011100", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111011101", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111011110", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111011111", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111100000", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111100001", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111100010", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111100011", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111100100", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111100101", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111100110", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111100111", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111101000", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111101001", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111101010", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111101011", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111101100", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111101101", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111101110", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111101111", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111110000", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111110001", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111110010", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111110011", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111110100", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111110101", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111110110", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111110111", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111111000", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111111001", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111111010", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111111011", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111111100", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111111101", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111111110", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0001111111111", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0010000000000", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0010000000001", 13).toUFix ),
           ( sExpX3_13 <= Bits("b0010000000010", 13).toUFix ) | sigX3(55), // 256*4 + 2 = 1026
           Bits("b11", 2)));
    // val roundMaskGen = 
    //   Mux(sExpX3(13) , Bits("hFFFFFFFFFFFFFFL", 56),
    //       Fill(Lit(1,1), (Lit(1026+2, 15) - sExpX3_13)(6, 0)) | Cat(sigX3(55), Lit(0, 2)));

    val roundPosMask = ~ Cat(Bits("b0", 1), roundMask>>UFix(1)) & roundMask;
    val roundPosBit = ( ( sigX3 & roundPosMask ) != Bits(0) );
    val anyRoundExtra = ( (   sigX3 & roundMask>>UFix(1) ) != Bits(0) );
    val allRoundExtra = ( ( ~ sigX3 & roundMask>>UFix(1) ) === Bits(0) );
    val anyRound = roundPosBit | anyRoundExtra;
    val allRound = roundPosBit & allRoundExtra;
    val roundDirectUp = Mux(signY, roundingMode_min, roundingMode_max);
    val roundUp =
          ( ~ doIncrSig & roundingMode_nearest_even &
                          roundPosBit & anyRoundExtra ) |
          ( ~ doIncrSig & roundDirectUp             & anyRound    ) |
          (   doIncrSig                             & allRound    ) |
          (   doIncrSig & roundingMode_nearest_even & roundPosBit ) |
          (   doIncrSig & roundDirectUp             & Bits(1)  );
    val roundEven =
        Mux(doIncrSig,
            roundingMode_nearest_even & ~ roundPosBit &   allRoundExtra,
            roundingMode_nearest_even &   roundPosBit & ~ anyRoundExtra);
    val roundInexact = Mux(doIncrSig, ~ allRound, anyRound);
    val roundUp_sigY3 = (( sigX3>>UFix(2) | roundMask>>UFix(2) ).toUFix + UFix(1))(54, 0);
    val sigY3 =
          (Mux(~ roundUp & ~ roundEven, ( sigX3 & ~ roundMask )>>UFix(2)        , Bits(0) ) |
          Mux(roundUp                , roundUp_sigY3                     , Bits(0) ) |
          Mux(roundEven              , roundUp_sigY3 & ~ ( roundMask>>UFix(1) ), Bits(0) ))(54, 0);
//*** HANDLE DIFFERENTLY?  (NEED TO ACCOUNT FOR ROUND-EVEN ZEROING MSB.)
    val sExpY =
          Mux(sigY3(54)                    , sExpX3 + UFix(1), UFix(0) ) |
          Mux(sigY3(53)                    , sExpX3          , UFix(0) ) |
          Mux(( sigY3(54, 53) === Bits(0) ), sExpX3 - UFix(1), UFix(0) );
    val expY = sExpY(11, 0);
    val fractY = Mux(sigX3Shift1, sigY3(51, 0), sigY3(52, 1));

    val overflowY = ( sExpY(12, 10) === Bits("b011", 3) );
//*** HANDLE DIFFERENTLY?  (NEED TO ACCOUNT FOR ROUND-EVEN ZEROING MSB.)
    val totalUnderflowY = sExpY(12) | ( sExpY(11, 0) < Bits("b001111001110", 12).toUFix );
    val underflowY =
        ( sExpX3(13) |
                ( sExpX3_13 <=
                        ( Mux(sigX3Shift1 , Bits("b0010000000010", 13).toUFix,
                               Bits("b0010000000001", 13).toUFix) ) ) ) &
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
            ~ Mux(notSpecial_isZeroOut, Bits("b111000000000", 12), Bits(0) ) &
            ~ Mux(isSatOut            , Bits("b010000000000", 12), Bits(0) ) &
            ~ Mux(notNaN_isInfOut     , Bits("b001000000000", 12), Bits(0) ) ) | 
	    Mux(isSatOut       , Bits("b101111111111", 12), Bits(0) ) |
            Mux(notNaN_isInfOut, Bits("b110000000000", 12), Bits(0) ) |
            Mux(isNaNOut       , Bits("b111000000000", 12), Bits(0) );
    val fractOut = fractY | Mux(isNaNOut | isSatOut, Bits("hFFFFFFFFFFFFF", 52), Bits(0) );
    io.out := Cat(signOut, expOut, fractOut);

    io.exceptionFlags := Cat(invalid, Bits("b0", 1), overflow, underflow, inexact);

}
}

`include "fpu_common.v"

/*----------------------------------------------------------------------------
| `expSize' is size of exponent in usual format.  The recoded exponent size is
| `expSize+1'.  Likewise for `size'.
*----------------------------------------------------------------------------*/


//*** THIS MODULE IS NOT FULLY OPTIMIZED.

module floatNToRecodedFloatN( in, out );

    parameter expSize = 8;
    parameter sigSize = 24;

    localparam size = expSize + sigSize;
    localparam logNormSize = `ceilLog2( sigSize );
    localparam normSize = 1 << logNormSize;

    input  [size-1:0] in;
    output [size:0]   out;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    wire                   sign;
    wire [expSize-1:0]     expIn;
    wire [sigSize-2:0]     fractIn;
    wire                   isZeroExpIn, isZeroFractIn, isZeroOrSubnormal;
    wire                   isZero, isSubnormal, isNormalOrSpecial;

    wire [normSize-1:0]    norm_in;
    wire [logNormSize-1:0] norm_count;
    wire [normSize-1:0]    norm_out;
    wire [sigSize-2:0]     normalizedFract;
    wire [expSize:0]       commonExp, expAdjust, adjustedCommonExp;
    wire                   isNaN;

    wire [expSize:0]       expOut;
    wire [sigSize-2:0]     fractOut;
    wire [size:0]          out;

    /*------------------------------------------------------------------------
    *------------------------------------------------------------------------*/
    assign sign    = in[size-1];
    assign expIn   = in[size-2:sigSize-1];
    assign fractIn = in[sigSize-2:0];
    assign isZeroExpIn = ( expIn == 0 );
    assign isZeroFractIn = ( fractIn == 0 );
    assign isZeroOrSubnormal = isZeroExpIn;
    assign isZero      = isZeroOrSubnormal &   isZeroFractIn;
    assign isSubnormal = isZeroOrSubnormal & ~ isZeroFractIn;
    assign isNormalOrSpecial = ~ isZeroExpIn;

    assign norm_in = {fractIn, {(normSize-sigSize+1){1'b0}}};
    normalizeN #(expSize, sigSize)
        normalizeFract( norm_in, norm_count, norm_out );
    assign normalizedFract = norm_out[normSize-2:normSize-sigSize];
    assign commonExp =
          ( isSubnormal  ? {{(expSize-logNormSize+1){1'b1}}, ~ norm_count} : 1'b0 )
        | ( isNormalOrSpecial ? expIn                                    : 1'b0 );
    assign expAdjust = isZero ? {1'b0} : ( 1'b1<<( expSize - 1 ) ) + 1'b1;
    assign adjustedCommonExp = commonExp + expAdjust + isSubnormal;
    assign isNaN =
        ( adjustedCommonExp[expSize:expSize-1] == 2'b11 ) & ~ isZeroFractIn;

    assign expOut = adjustedCommonExp | isNaN<<( expSize - 2 );
    assign fractOut = isZeroOrSubnormal ? normalizedFract : fractIn;
    assign out = {sign, expOut, fractOut};

endmodule


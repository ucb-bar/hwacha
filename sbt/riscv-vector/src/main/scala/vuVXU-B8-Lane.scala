package riscvVector {
import Chisel._
import Node._

class CPIO extends Bundle {
  val imul_val = Bool('input);
  val imul_rdy = Bool('output);
  val imul_fn  = Bits(DEF_VAU0_FN, 'input);
  val imul_in0 = UFix(DEF_XLEN, 'input);
  val imul_in1 = UFix(DEF_XLEN, 'input);
  val imul_out = UFix(DEF_XLEN, 'output);

  val fma_val  = Bool('input);
  val fma_rdy  = Bool('output);
  val fma_fn   = Bits(DEF_VAU1_FN, 'input);
  val fma_in0  = Bits(DEF_FLEN, 'input);
  val fma_in1  = Bits(DEF_FLEN, 'input);
  val fma_in2  = Bits(DEF_FLEN, 'input);
  val fma_out  = Bits(DEF_FLEN, 'output);
  val fma_exec = Bits(DEF_EXC, 'output);
}

class ExpanderIO extends Bundle {
  val bank = new BankToBankIO.flip();

  val vau0    = Bool('output);
  val vau0_fn = UFix(DEF_VAU0_FN, 'output);
  val vau1    = Bool('output);
  val vau1_fn = UFix(DEF_VAU1_FN, 'output);
  val vau2    = Bool('output);
  val vau2_fn = UFix(DEF_VAU2_FN, 'output);
  val vldq    = Bool('output);
  val vsdq    = Bool('output);
  val utaq    = Bool('output);
  val utldq   = Bool('output);
  val utsdq   = Bool('output);
}

val VMUIO extends Bundle 
{
  val vldq_rdy  = Bool('output);
  val vldq_bits = Bits(DEF_DATA, 'input)
  val vsdq_val  = Bool('output);
  val vsdq_bits = Bits(DEF_DATA, 'output);

  val utaq_val  = Bool('output);
  val utaq_bits = Bits(DEF_DATA, 'output);

  val utldq_rdy  = Bool('output);
  val utldq_bits = Bits(DEF_DATA, 'input)
  val utsdq_val  = Bool('output);
  val utsdq_bits = Bits(DEF_DATA, 'output);
}

class vuVXU_LaneIO extends Bundle 
{
  val cp = new CPIO();
  val bactive = Bits(DEF_BANK, 'input);
  val expand = new ExpanderIO.flip();
  val lane_rlast = Bool('output);
  val lane_wlast = Bool('output);
  val vmu = new VMUIO();
}

class vuVXU_Banked8_Lane extends Component
{
  val io = new vuVXU_LaneIO();

  val conn = new ArrayBuffer[BankToBankIO];
  var first = true;

  val rblen = new ArrayBuffer[UFix];
  val rdata = new ArrayBuffer[UFix];
  val ropl0 = new ArrayBuffer[UFix];
  val ropl1 = new ArrayBuffer[UFix];

  for (i <- 0 until SZ_BANK) 
  {
    val bank = new vuVXU_Banked8_Bank();
    bank.io.active := bactive(i);
    
    if (first)
    { 
      bank.io.in ^^ io.expand.bank; 
      first = false ;
    } 
    else 
    {
      bank.io.in <> conn.last;
    }
    
    conn  += bank.io.out;
    rblen += bank.io.rw.rblen;
    rdata += bank.io.rw.rdata;
    ropl0 += bank.io.rw.ropl0;
    ropl1 += bank.io.rw.ropl1;

    bank.io.rw.wbl0 := imul.out;
    bank.io.rw.wbl1 := fma.out;
    bank.io.rw.wbl2 := conv.out;
    bank.io.rw.wbl3 := Mux(io.vmu.utld_rdy, io.vmu.utldq_bits, io.vmu.vldq_bits);
  }

  io.lane_rlast := con.last.rlast;
  io.land_wlast := con.last.wlast;

  vuVXU_Banked8_Lane_Xbar xbar
  (
    .rblen(rblen),
    .rdata(rdata),
    .ropl0(ropl0),
    .ropl1(ropl1),

    .rbl0(rbl0),
    .rbl1(rbl1),
    .rbl2(rbl2),
    .rbl3(rbl3),
    .rbl4(rbl4),
    .rbl5(rbl5),
    .rbl6(rbl6),
    .rbl7(rbl7)
  );

  wire              vau0_val;
  wire `DEF_VAU0_FN vau0_fn;
  wire              vau1_val;
  wire `DEF_VAU1_FN vau1_fn;
  wire              vau2_val;
  wire `DEF_VAU2_FN vau2_fn;

  vuVXU_Banked8_Lane_LFU lfu
  (
    .clk(clk),
    .reset(reset),

    .expand_rcnt(expand_rcnt),
    .expand_wcnt(expand_wcnt),

    .expand_vau0(expand_vau0),
    .expand_vau0_fn(expand_vau0_fn),
    .expand_vau1(expand_vau1),
    .expand_vau1_fn(expand_vau1_fn),
    .expand_vau2(expand_vau2),
    .expand_vau2_fn(expand_vau2_fn),
    .expand_vldq(expand_vldq),
    .expand_vsdq(expand_vsdq),
    .expand_utaq(expand_utaq),
    .expand_utldq(expand_utldq),
    .expand_utsdq(expand_utsdq),

    .vau0_val(vau0_val),
    .vau0_fn(vau0_fn),
    .vau1_val(vau1_val),
    .vau1_fn(vau1_fn),
    .vau2_val(vau2_val),
    .vau2_fn(vau2_fn),
    .vldq_rdy(vldq_rdy),
    .vsdq_val(vsdq_val),
    .utaq_val(utaq_val),
    .utldq_rdy(utldq_rdy),
    .utsdq_val(utsdq_val)
  );

  wire `DEF_VAU0_FN imul_fn = vau0_val ? vau0_fn : cp_imul_fn;
  wire `DEF_DATA imul_in0 = vau0_val ? rbl0 : {1'b0, cp_imul_in0};
  wire `DEF_DATA imul_in1 = vau0_val ? rbl1 : {1'b0, cp_imul_in1};
  wire `DEF_DATA imul_out;

  assign cp_imul_rdy = ~vau0_val;
  assign cp_imul_out = imul_out[`SZ_XLEN-1:0];
  assign wbl0 = imul_out;

  vuVXU_Banked8_FU_imul imul
  (
    .clk(clk),
    .reset(reset),
    .val(vau0_val | cp_imul_val),
    .fn(imul_fn),
    .in0(imul_in0),
    .in1(imul_in1),
    .out(imul_out)
  );

  wire `DEF_VAU1_FN fma_fn = vau1_val ? vau1_fn : cp_fma_fn;
  wire `DEF_DATA fma_in0 = vau1_val ? rbl2 : cp_fma_in0;
  wire `DEF_DATA fma_in1 = vau1_val ? rbl3 : cp_fma_in1;
  wire `DEF_DATA fma_in2 = vau1_val ? rbl4 : cp_fma_in2;
  wire `DEF_EXC  fma_exc;
  wire `DEF_DATA fma_out;

  assign cp_fma_rdy = ~vau1_val;
  assign cp_fma_out = fma_out;
  assign cp_fma_exc = fma_exc;
  assign wbl1 = fma_out;

  vuVXU_Banked8_FU_fma fma
  (
    .clk(clk),
    .reset(reset),
    .val(vau1_val | cp_fma_val),
    .fn(fma_fn),
    .in0(fma_in0),
    .in1(fma_in1),
    .in2(fma_in2),
    .out(fma_out),
    .exc(fma_exc)
  );

  vuVXU_Banked8_FU_conv conv
  (
    .clk(clk),
    .reset(reset),
    .val(vau2_val),
    .fn(vau2_fn),
    .in(rbl5),
    .exc(),
    .out(wbl2)
  );

  assign utaq_bits = rbl6[`SZ_ADDR-1:0];
  assign vsdq_bits = rbl7;
  assign utsdq_bits = rbl7;

  assign wbl3 = utldq_rdy ? utldq_bits : vldq_bits;

endmodule

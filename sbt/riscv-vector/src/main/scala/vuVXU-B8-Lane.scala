package riscvVector {
import Chisel._
import Node._

class CPIO extends Bundle {
  val imul_val = Bool('input);
  val imul_rdy = Bool('output);
  val imul_fn  = Bits(DEF_VAU0_FN, 'input);
  val imul_in0 = Bits(DEF_XLEN, 'input);
  val imul_in1 = Bits(DEF_XLEN, 'input);
  val imul_out = Bits(DEF_XLEN, 'output);

  val fma_val  = Bool('input);
  val fma_rdy  = Bool('output);
  val fma_fn   = Bits(DEF_VAU1_FN, 'input);
  val fma_in0  = Bits(DEF_FLEN, 'input);
  val fma_in1  = Bits(DEF_FLEN, 'input);
  val fma_in2  = Bits(DEF_FLEN, 'input);
  val fma_out  = Bits(DEF_FLEN, 'output);
  val fma_exec = Bits(DEF_EXC, 'output);
}

class ExpanderToLFUIO extends Bundle {
  val rcnt   = Bits(DEF_BVLEN  , 'output);
  val wcnt  = Bits(DEF_BVLEN  , 'output);

  val vau0    = Bool('output);
  val vau0_fn = Bits(DEF_VAU0_FN, 'output);
  val vau1    = Bool('output);
  val vau1_fn = Bits(DEF_VAU1_FN, 'output);
  val vau2    = Bool('output);
  val vau2_fn = Bits(DEF_VAU2_FN, 'output);
  val vldq    = Bool('output);
  val vsdq    = Bool('output);
  val utaq    = Bool('output);
  val utldq   = Bool('output);
  val utsdq   = Bool('output);
}

class ExpanderIO extends Bundle {
  val bank = new BankToBankIO.flip();

  val ren    = Bool('output);
  val rlast  = Bool('output);
  val raddr  = Bits(DEF_BREGLEN, 'output);
  val roplen = Bits(DEF_BOPL   , 'output);
  val rblen  = Bits(DEF_BRPORT , 'output);

  val wen   = Bool('output);
  val wlast = Bool('output);
  val waddr = Bits(DEF_BREGLEN, 'output);
  val wsel  = Bits(DEF_BWPORT , 'output);

  val viu       = Bool('output);
  val viu_fn    = Bits(DEF_VIU_FN , 'output);
  val viu_utidx = Bits(DEF_VLEN   , 'output);
  val viu_imm   = Bits(DEF_DATA   , 'output);

  val lfu = new ExpanderToLFUIO();
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

  val rblen = new ArrayBuffer[Bits];
  val rdata = new ArrayBuffer[Bits];
  val ropl0 = new ArrayBuffer[Bits];
  val ropl1 = new ArrayBuffer[Bits];

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

  val xbar = new vuVXU_Banked8_Lane_Xbar();
  xbar.io.blen  <> rblen;
  xbar.io.rdata <> rdata;
  xbar.io.ropl0 <> ropl0;
  xbar.io.ropl1 <> ropl1;
  val rbl = xbar.io.rbl;

  val lfu = new vuVXU_Banked8_Lane_LFU();
  lfu.io.expand <> io.expand.lfu;
  val vau0_val  = lfu.io.vau0_val;
  val vau0_fn   = lfu.io.vau0_fn;
  val vau1_val  = lfu.io.vau1_val;
  val vau1_fn   = lfu.io.vau1_fn;
  val vau2_val  = lfu.io.vau2_val;
  val vau2_fn   = lfu.io.vau2_fn;
  val vldq_rdy  = lfu.io.vldq_rdy;
  val vsdq_val  = lfu.io.vsdq_val;
  val utaq_val  = lfu.io.utaq_val;
  val utldq_rdy = lfu.io.utldq_rdy;
  val utsdq_val = lfu.io.utsdq_val;

  val imul_fn  = Mux(vau0_val, vau0_fn, io.cp.imul_fn);
  val imul_in0 = Mux(vau0_val, rbl0, Cat(Bits(0,1), io.cp.imul_in0));
  val imul_in1 = Mux(vau0_val, rbl1, Cat(Bits(0,1), io.cp.imul_in1));

  io.cp.imul_rdy := ~vau0_val;
  io.cp.imul_out := imul_out(SZ_XLEN-1:0);

  vuVXU_Banked8_FU_imul imul
  (
    .clk(clk),
    .reset(reset),
    .val(vau0_val | io.cp.imul_val),
    .fn(imul_fn),
    .in0(imul_in0),
    .in1(imul_in1),
    .out(imul_out)
  );

  wire `DEF_VAU1_FN fma_fn = Mux(vau1_val, vau1_fn, io.cp.fma_fn);
  wire `DEF_DATA fma_in0 = Mux(vau1_val, rbl2, io.cp.fma_in0);
  wire `DEF_DATA fma_in1 = Mux(vau1_val, rbl3, io.cp.fma_in1);
  wire `DEF_DATA fma_in2 = Mux(vau1_val, rbl4, io.cp.fma_in2);
  wire `DEF_EXC  fma_exc;
  wire `DEF_DATA fma_out;

  assign io.cp.fma_rdy = ~vau1_val;
  assign io.cp.fma_out = fma_out;
  assign io.cp.fma_exc = fma_exc;
  assign wbl1 = fma_out;

  vuVXU_Banked8_FU_fma fma
  (
    .clk(clk),
    .reset(reset),
    .val(vau1_val | io.cp.fma_val),
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

  assign wbl3 = Mux(utldq_rdy, utldq_bits, vldq_bits);

endmodule

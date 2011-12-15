package riscvVector
{

import Chisel._
import Node._
import Config._
import scala.collection.mutable.ArrayBuffer

class CPIO extends Bundle
{
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
  val fma_exc = Bits(DEF_EXC, 'output);
}

class ExpanderToLFUIO extends Bundle
{
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

class ExpanderIO extends Bundle
{
  val bank = new BankToBankIO().flip();
  val lfu = new ExpanderToLFUIO();
}

class VMUIO extends Bundle 
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
  val expand = new ExpanderIO().flip();
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

  //forward declaring imul, fma, and conv units
  val imul = new vuVXU_Banked8_FU_imul();
  val fma  = new vuVXU_Banked8_FU_fma();
  val conv = new vuVXU_Banked8_FU_conv();

  for (i <- 0 until SZ_BANK) 
  {
    val bank = new vuVXU_Banked8_Bank();
    bank.io.active := io.bactive(i).toBool;
    
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

    bank.io.rw.wbl0 := imul.io.out;
    bank.io.rw.wbl1 := fma.io.out;
    bank.io.rw.wbl2 := conv.io.out;
    bank.io.rw.wbl3 := Mux(io.vmu.utldq_rdy, io.vmu.utldq_bits, io.vmu.vldq_bits);
  }

  io.lane_rlast := conn.last.rlast;
  io.lane_wlast := conn.last.wlast;

  val xbar = new vuVXU_Banked8_Lane_Xbar();
  xbar.io.rblen <> rblen;
  xbar.io.rdata <> rdata;
  xbar.io.ropl0 <> ropl0;
  xbar.io.ropl1 <> ropl1;
  val rbl = xbar.io.rbl;

  val lfu = new vuVXU_Banked8_Lane_LFU();
  lfu.io.expand_rcnt := io.expand.bank.rcnt.toUFix;
  lfu.io.expand_wcnt := io.expand.bank.wcnt.toUFix;
  lfu.io.expand ^^ io.expand.lfu;

  val vau0_val  = lfu.io.vau0_val;
  val vau0_fn   = lfu.io.vau0_fn;
  val vau1_val  = lfu.io.vau1_val;
  val vau1_fn   = lfu.io.vau1_fn;
  val vau2_val  = lfu.io.vau2_val;
  val vau2_fn   = lfu.io.vau2_fn;

  io.vmu.vldq_rdy  := lfu.io.vldq_rdy;
  io.vmu.vsdq_val  := lfu.io.vsdq_val;
  io.vmu.utaq_val  := lfu.io.utaq_val;
  io.vmu.utldq_rdy := lfu.io.utldq_rdy;
  io.vmu.utsdq_val := lfu.io.utsdq_val;

  val imul_fn  = Mux(vau0_val, vau0_fn, io.cp.imul_fn);
  val imul_in0 = Mux(vau0_val, rbl(0), Cat(Bits(0,1), io.cp.imul_in0));
  val imul_in1 = Mux(vau0_val, rbl(1), Cat(Bits(0,1), io.cp.imul_in1));

  io.cp.imul_rdy := ~vau0_val;
  io.cp.imul_out := imul.io.out(SZ_XLEN-1,0);

  //integer multiply
  imul.io.valid := vau0_val | io.cp.imul_val;
  imul.io.fn  := imul_fn;
  imul.io.in0 := imul_in0;
  imul.io.in1 := imul_in1;

  val fma_fn  = Mux(vau1_val, vau1_fn, io.cp.fma_fn);
  val fma_in0 = Mux(vau1_val, rbl(2), io.cp.fma_in0);
  val fma_in1 = Mux(vau1_val, rbl(3), io.cp.fma_in1);
  val fma_in2 = Mux(vau1_val, rbl(4), io.cp.fma_in2);

  io.cp.fma_rdy := ~vau1_val;
  io.cp.fma_out := fma.io.out;
  io.cp.fma_exc := fma.io.exc;

  //fma
  fma.io.valid := vau1_val | io.cp.fma_val;
  fma.io.fn    := fma_fn;
  fma.io.in0   := fma_in0;
  fma.io.in1   := fma_in1;
  fma.io.in2   := fma_in2;

  //conv
  conv.io.valid := vau2_val;
  conv.io.fn := vau2_fn;
  conv.io.in := rbl(5);

  io.vmu.utaq_bits  := rbl(6)(SZ_ADDR-1,0);
  io.vmu.vsdq_bits  := rbl(7);
  io.vmu.utsdq_bits := rbl(7);
}

}

package hwacha
{

import Chisel._
import Node._
import Config._

class RegfileIO extends Bundle
{
  val ren    = Bool(INPUT);
  val raddr  = Bits(DEF_BREGLEN, INPUT);
  val roplen = Bits(DEF_BOPL, INPUT);

  val wen   = Bool(INPUT);
  val waddr = Bits(DEF_BREGLEN, INPUT);
  val wsel  = Bits(DEF_BWPORT, INPUT);

  val rdata = Bits(DEF_DATA, OUTPUT);
  val ropl0 = Bits(DEF_DATA, OUTPUT);
  val ropl1 = Bits(DEF_DATA, OUTPUT);

  val wbl0 = Bits(DEF_DATA, INPUT);
  val wbl1 = Bits(DEF_DATA, INPUT);
  val wbl2 = Bits(DEF_DATA, INPUT);
  val wbl3 = Bits(DEF_DATA, INPUT);

  val viu_rdata = Bits(DEF_DATA, OUTPUT);
  val viu_ropl  = Bits(DEF_DATA, OUTPUT);
  val viu_wdata = Bits(DEF_DATA, INPUT);
}

class vuVXU_Banked8_Bank_Regfile extends Component
{
  val io = new RegfileIO();

  val wdata = MuxLookup(
    io.wsel, Bits(0, SZ_DATA), Array(
      Bits(0, SZ_BWPORT) -> io.wbl0,
      Bits(1, SZ_BWPORT) -> io.wbl1,
      Bits(2, SZ_BWPORT) -> io.wbl2,
      Bits(3, SZ_BWPORT) -> io.wbl3,
      Bits(4, SZ_BWPORT) -> io.viu_wdata
    ));

  val rfile = Mem(256, io.wen, io.waddr.toUFix, wdata, resetVal = null);
  val rdata_rf = Mux(Reg(io.ren), rfile(Reg(io.raddr)), Bits(0)); 
  io.rdata := rdata_rf;

  val ropl0Reg = Reg(){Bits(width = DEF_DATA)};
  val ropl1Reg = Reg(){Bits(width = DEF_DATA)};
  when(io.roplen(0).toBool){ropl0Reg <== rdata_rf};
  when(io.roplen(1).toBool){ropl1Reg <== rdata_rf};

  io.ropl0 := ropl0Reg;
  io.ropl1 := ropl1Reg;

  io.viu_rdata := rdata_rf;
  io.viu_ropl := io.ropl0;
}

}

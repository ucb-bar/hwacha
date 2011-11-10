class XBarMux8IO extends Bundle 
{
  val rblen = Vec(8){ Bits(DEF_BRPORT, 'input) };
  val port  = Bits('input);
  val rdata = Vec(8){ Bits(DEF_DATA, 'input) };
  val rbl   = Bits(DEF_DATA, 'output);
}

class vuVXU_Banked8_Lane_Xbar_Mux8 extends Component
{
  val io = new XBarMux8IO();
  io.rbl := Fill(SZ_DATA, rblen(0)(port)) & rdata(0) |
            Fill(SZ_DATA, rblen(1)(port)) & rdata(1) |
            Fill(SZ_DATA, rblen(2)(port)) & rdata(2) |
            Fill(SZ_DATA, rblen(3)(port)) & rdata(3) |
            Fill(SZ_DATA, rblen(4)(port)) & rdata(4) |
            Fill(SZ_DATA, rblen(5)(port)) & rdata(5) |
            Fill(SZ_DATA, rblen(6)(port)) & rdata(6) |
            Fill(SZ_DATA, rblen(7)(port)) & rdata(7);
  
}

object vuVXU_Banked8_lane_Xbar_Mux8
{
  def apply(rblen: Vec[Bits], port: Int, rdata: Vec[Bits]): Bits = 
    {
      val mux8 = new vuVXU_Banked8_Lane_Xbar_Mux8();
      mux8.io.rblen ^^ rblen;
      mux8.io.port := Bits(port);
      mux8.io.rdata ^^ rdata;
      mux8.io.rbl
    }
}

class XbarIO extends Bundle 
{
  val rblen = Vec(8){ Bits(DEF_BRPORT, 'input) };
  val rdata = Vec(8){ Bits(DEF_DATA, 'input) };
  val ropl0 = Vec(8){ Bits(DEF_DATA, 'input) };
  val ropl1 = Vec(8){ Bits(DEF_DATA, 'input) };

  val rbl = Vec(8){ Bits(DEF_DATA, 'output) };
}

class vuVXU_Banked8_Lane_Xbar extends Component
{
  val io = new XbarIO();
  io.rbl(0) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 0, io.ropl0);
  io.rbl(1) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 1, io.rdata);
  io.rbl(2) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 2, io.ropl1);
  io.rbl(3) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 3, io.ropl0);
  io.rbl(4) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 4, io.rdata);
  io.rbl(5) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 5, io.rdata);
  io.rbl(6) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 6, io.rdata);
  io.rbl(7) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 7, io.rdata);

}

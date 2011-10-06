class RegfileIO extends Bundle {
  val ren    = Bool('input);
  val raddr  = UFix(DEF_BREGLEN, 'input);
  val roplen = UFix(DEF_BOPL, 'input);

  val wen   = Bool('input);
  val waddr = UFix(DEF_BREGLEN, 'input);
  val wsel  = UFix(DEF_BWPORT, 'input);

  val rdata = Bits(DEF_DATA, 'output);
  val ropl0 = UFix(DEF_DATA, 'output);
  val ropl1 = UFix(DEF_DATA, 'output);

  val wbl0 = UFix(DEF_DATA, 'input);
  val wbl1 = UFix(DEF_DATA, 'input);
  val wbl2 = UFix(DEF_DATA, 'input);
  val wbl3 = UFix(DEF_DATA, 'input);

  val viu_rdata = UFix(DEF_DATA, 'output);
  val viu_ropl  = UFix(DEF_DATA, 'output);
  val viu_wdata = UFix(DEF_DATA, 'input);
}

class vuVXU_Banked8_Bank_Regfile extends Component {
  val io = RegfileIO();

  val wdata = MuxLookup(io.wsel, UFix(0, SZ_DATA), Array(
    UFix(0, SZ_BWPORT) -> io.wbl0,
    UFix(1, SZ_BWPORT) -> io.wbl1,
    UFix(2, SZ_BWPORT) -> io.wbl2,
    UFix(3, SZ_BWPORT) -> io.wbl3,
    UFix(4, SZ_BWPORT) -> io.viu_wdata));

  val rfile = Mem(32, io.wen, io.waddr, wdata, resetVal = null);
  val rdata_rf = Reg(resetVal = UFix(0, DEF_DATA));
  when(io.ren){rdata_rf <== rfile(io.raddr)};

  io.rdata := rdata_rf;

  val ropl0Reg = Reg(){UFix(width = DEF_DATA)};
  val ropl1Reg = Reg(){UFix(width = DEF_DATA)};
  when(io.roplen(0).toBool){ropl0Reg <== rdata_rf};
  when(io.roplen(1).toBool){ropl1Reg <== rdata_rf};

  io.ropl0 := ropl0Reg;
  io.ropl1 := ropl1Reg;

  io.viu_rdata := rdata_rf;
  io.viu_ropl := io.ropl0;

}

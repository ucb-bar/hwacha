package riscvVector {
  import Chisel._
  import Node._

// byte/halfword/word/doubleword select + sign extension logic

class vuVMU_BHWD_selIO extends Bundle {
  val bhwd_sel		= UFix(2, 'input);
  val signext		  = Bool('input);
  val addr_lsb		= UFix(4, 'input);
  val din		      = UFix(128, 'input);
  val dout		    = UFix(64, 'output);
}

class vuVMU_BHWD_sel extends Component {
  val io = new vuVMU_BHWD_selIO();

  val dword_sel     = Mux(addr_lsb(3), io.din(127,64), io.din(63,0));

  val word_sel      = Mux(addr_lsb(2), dword_sel(63,32), dword_sel(31,0));
  val word_sel_ext  = Mux(io.signext, Cat(UFix(0, 32), word_sel), Cat(Fill(32, word_sel(31)), word_sel));

  val hw_sel        = Mux(addr_lsb(1), word_sel(31,16), word_sel(15,0));
  val hw_sel_ext    = Mux(io.signext, Cat(UFix(0, 48), hw_sel), Cat(Fill(48, hw_sel(15)), hw_sel));
  
  val byte_sel      = Mux(addr_lsb(1), hw_sel(15,8), hw_sel(7,0));
  val byte_sel_ext  = Mux(io.signext, Cat(UFix(0, 56), byte_sel), Cat(Fill(56, byte_sel(7)), hw_sel));

  io.dout           := MuxLookup(io.bhwd_sel, UFix(0, 64), Array(
    UFix(0, 2) -> byte_sel_ext,
    UFix(1, 2) -> hw_sel_ext,
    UFix(2, 2) -> word_sel_ext,
    UFix(3, 2) -> dword_sel_ext));
}

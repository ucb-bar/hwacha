package hwacha {
  import Chisel._
  import Node._

// byte/halfword/word/doubleword select + sign extension logic

class vuVMU_BHWD_selIO extends Bundle
{
  val bhwd_sel		= Bits(2, INPUT)
  val signext		  = Bool(INPUT)
  val addr_lsb		= Bits(4, INPUT)
  val din		      = Bits(128, INPUT)
  val dout		    = Bits(64, OUTPUT)
}

class vuVMU_BHWD_sel extends Component 
{
  val io = new vuVMU_BHWD_selIO()

  val dword_sel     = Mux(io.addr_lsb(3)===Bits(1,1), io.din(127,64), io.din(63,0))

  val word_sel      = Mux(io.addr_lsb(2)===Bits(1,1), dword_sel(63,32), dword_sel(31,0))
  val word_sel_ext  = Mux(io.signext, Cat(Bits(0, 32), word_sel), Cat(Fill(32, word_sel(31)), word_sel))

  val hw_sel        = Mux(io.addr_lsb(1)===Bits(1,1), word_sel(31,16), word_sel(15,0))
  val hw_sel_ext    = Mux(io.signext, Cat(Bits(0, 48), hw_sel), Cat(Fill(48, hw_sel(15)), hw_sel))
  
  val byte_sel      = Mux(io.addr_lsb(0)===Bits(1,1), hw_sel(15,8), hw_sel(7,0))
  val byte_sel_ext  = Mux(io.signext, Cat(Bits(0, 56), byte_sel), Cat(Fill(56, byte_sel(7)), byte_sel))

  io.dout           := MuxLookup(io.bhwd_sel, Bits(0, 64), Array(
    Bits(0, 2) -> byte_sel_ext,
    Bits(1, 2) -> hw_sel_ext,
    Bits(2, 2) -> word_sel_ext,
    Bits(3, 2) -> dword_sel))
}

}

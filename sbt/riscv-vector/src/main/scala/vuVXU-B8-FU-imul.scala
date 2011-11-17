package riscvVector {
import Chisel._
import Node._
import Config._

class vuVXU_Banked8_FU_imul extends Component
{
  val io = new Bundle{
    val valid = Bool('input);
    val fn  = Bits(DEF_VAU0_FN, 'input);
    val in0 = Bits(DEF_DATA, 'input);
    val in1 = Bits(DEF_DATA, 'input);
    val out = Bits(DEF_DATA, 'output);
  }

  val sxl64 = io.fn === VAU0_64H  | io.fn === VAU0_64HSU;
  val sxr64 = io.fn === VAU0_64H;
  val zxl32 = io.fn === VAU0_32HU;
  val zxr32 = io.fn === VAU0_32HU | io.fn === VAU0_32HSU;
  val sxl32 = io.fn === VAU0_32H | io.fn === VAU0_32HSU;
  val sxr32 = io.fn === VAU0_32H;

  val lhs = Cat(io.in0(63) & sxl64,
                     Fill(32, ~zxl32)&io.in0(63,32) | Fill(32, sxl32&io.in0(31)),
                     io.in0(31,0)); //TODO: 65 bits
  val rhs = Cat(io.in1(63) & sxr64,
                     Fill(32, ~zxr32)&io.in1(63,32) | Fill(32, sxr32&io.in1(31)),
                     io.in1(31,0)); //TODO: 65 bits

  val mul_result = lhs.toFix * rhs.toFix; //TODO:130 bits
  //reg [63:0] reg_result [0:IMUL_STAGES-1];

  val mul_output_mux = MuxLookup(io.fn, Bits(0,64), Array(
    VAU0_64    -> mul_result(63,0),
    VAU0_64H   -> mul_result(127,64),
    VAU0_64HU  -> mul_result(127,64),
    VAU0_64HSU -> mul_result(127,64),
    VAU0_32    -> Cat(Fill(32, mul_result(31)), mul_result(31,0)),
    VAU0_32H   -> Cat(Fill(32, mul_result(63)), mul_result(63,32)),
    VAU0_32HU  -> Cat(Fill(32, mul_result(63)), mul_result(63,32)),
    VAU0_32HSU -> Cat(Fill(32, mul_result(63)), mul_result(63,32))
  ));

  def shift_register(n: Int): Bits = 
    {
      if(n == 0)
	{
	  val res = Reg(){Bits(width = 64)};
	  when(io.valid) { res <== mul_output_mux }
	  return res
	} 
      else 
	{
	  Reg(shift_register(n-1));
	}
    }

  io.out := shift_register(IMUL_STAGES-1);

}
}

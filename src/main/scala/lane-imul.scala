package hwacha

import Chisel._
import Node._
import Constants._

class LaneMul(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val valid = Bool(INPUT)
    val fn = new VAU0Fn().asInput
    val in0 = Bits(INPUT, SZ_DATA)
    val in1 = Bits(INPUT, SZ_DATA)
    val out = Bits(OUTPUT, SZ_DATA)
  }

  def FN(dw: Bits, op: Bits) = io.fn.dw === dw && io.fn.op === op
  val sxl64 = FN(DW64, A0_MH) | FN(DW64, A0_MHSU)
  val sxr64 = FN(DW64, A0_MH)
  val zxl32 = FN(DW32, A0_MHU)
  val zxr32 = FN(DW32, A0_MHU) | FN(DW32, A0_MHSU)
  val sxl32 = FN(DW32, A0_MH) | FN(DW32, A0_MHSU)
  val sxr32 = FN(DW32, A0_MH)

  val lhs = Cat(
    io.in0(63) & sxl64,
    Fill(32, ~zxl32)&io.in0(63,32) | Fill(32, sxl32&io.in0(31)),
    io.in0(31,0)) //TODO: 65 bits
  val rhs = Cat(
    io.in1(63) & sxr64,
    Fill(32, ~zxr32)&io.in1(63,32) | Fill(32, sxr32&io.in1(31)),
    io.in1(31,0)) //TODO: 65 bits

  val mul_result = lhs.toSInt * rhs.toSInt //TODO:130 bits

  val mul_output_mux = MuxCase(
    Bits(0, 64), Array(
      FN(DW64, A0_M)    -> mul_result(63,0),
      FN(DW64, A0_MH)   -> mul_result(127,64),
      FN(DW64, A0_MHU)  -> mul_result(127,64),
      FN(DW64, A0_MHSU) -> mul_result(127,64),
      FN(DW32, A0_M)    -> Cat(Fill(32, mul_result(31)), mul_result(31,0)),
      FN(DW32, A0_MH)   -> Cat(Fill(32, mul_result(63)), mul_result(63,32)),
      FN(DW32, A0_MHU)  -> Cat(Fill(32, mul_result(63)), mul_result(63,32)),
      FN(DW32, A0_MHSU) -> Cat(Fill(32, mul_result(63)), mul_result(63,32))
    ))

  io.out := ShiftRegister(mul_output_mux, conf.imul_stages, io.valid)
}

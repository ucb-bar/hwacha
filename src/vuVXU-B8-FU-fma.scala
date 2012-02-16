package hwacha

import Chisel._
import Node._
import hardfloat._
import Config._

class vuVXU_Banked8_FU_fma extends Component
{
  val io = new Bundle
  {
    val valid = Bool(INPUT)
    val fn    = Bits(DEF_VAU1_FN, INPUT)
    val in0   = Bits(DEF_DATA, INPUT)
    val in1   = Bits(DEF_DATA, INPUT)
    val in2   = Bits(DEF_DATA, INPUT)
    val out   = Bits(DEF_DATA, OUTPUT)
    val exc   = Bits(DEF_EXC, OUTPUT)
  }

  // use in0 & in2 for a two operand flop (add,sub,mul)
  // use in0, in1, & in2 otherwise

  val fma_op = MuxCase(
    Bits("b00",2), Array(
      (io.fn(RG_VAU1_FN) === VAU1_SUB || io.fn(RG_VAU1_FN) === VAU1_MSUB) -> Bits("b01",2),
      (io.fn(RG_VAU1_FN) === VAU1_NMSUB) -> Bits("b10",2),
      (io.fn(RG_VAU1_FN) === VAU1_NMADD) -> Bits("b11",2)
    ))

  val one_dp = Bits("h8000000000000000", 65)
  val one_sp = Bits("h80000000", 65)
  val fma_multiplicand = io.in0
  val fma_multiplier = MuxCase(
    io.in1, Array(
      ((io.fn(RG_VAU1_FP) === Bits("b1",1)) && (io.fn(RG_VAU1_FN) === VAU1_ADD || io.fn(RG_VAU1_FN) === VAU1_SUB)) -> one_dp,
      ((io.fn(RG_VAU1_FP) === Bits("b0",1)) && (io.fn(RG_VAU1_FN) === VAU1_ADD || io.fn(RG_VAU1_FN) === VAU1_SUB)) -> one_sp,
      ((io.fn(RG_VAU1_FN) === VAU1_MUL)) -> io.in2
    ))

  val fma_addend = Mux(
    io.fn(RG_VAU1_FN) === VAU1_MUL, Bits(0,65),
    io.in2)

  val val_fma_dp = io.valid & (io.fn(RG_VAU1_FP) === Bits("b1",1))
  val val_fma_sp = io.valid & (io.fn(RG_VAU1_FP) === Bits("b0",1))

  val fma_dp = new mulAddSubRecodedFloat64_1()
  fma_dp.io.op := Fill(2,val_fma_dp) & fma_op
  fma_dp.io.a  := Fill(65,val_fma_dp) & fma_multiplicand
  fma_dp.io.b  := Fill(65,val_fma_dp) & fma_multiplier
  fma_dp.io.c  := Fill(65,val_fma_dp) & fma_addend
  fma_dp.io.roundingMode := Fill(2,val_fma_dp) & io.fn(RG_VAU1_RM)
  val result_dp = Cat(fma_dp.io.exceptionFlags, fma_dp.io.out)

  val fma_sp = new mulAddSubRecodedFloat32_1()
  fma_sp.io.op := Fill(2,val_fma_sp) & fma_op
  fma_sp.io.a  := Fill(33,val_fma_sp) & fma_multiplicand(32,0)
  fma_sp.io.b  := Fill(33,val_fma_sp) & fma_multiplier(32,0)
  fma_sp.io.c  := Fill(33,val_fma_sp) & fma_addend(32,0)
  fma_sp.io.roundingMode := Fill(2,val_fma_sp) & io.fn(RG_VAU1_RM)
  val result_sp = Cat(fma_sp.io.exceptionFlags, fma_sp.io.out)

  val result = Mux(
    io.fn(RG_VAU1_FP), result_dp,
    Cat(result_sp(37,33), Bits("hFFFFFFFF",32), result_sp(32,0)))

  val pipereg = ShiftRegister(FMA_STAGES-1, 70, io.valid, result)

  Match(pipereg, io.exc, io.out)
}

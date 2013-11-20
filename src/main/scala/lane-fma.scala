package hwacha

import Chisel._
import Node._
import Constants._

class io_cp_fma(width: Int) extends Bundle
{
  val valid = Bool(OUTPUT)
  val cmd = Bits(OUTPUT, FCMD_WIDTH)
  val rm = Bits(OUTPUT, 3)
  val in1 = Bits(OUTPUT, width)
  val in2 = Bits(OUTPUT, width)
  val in3 = Bits(OUTPUT, width)
  val out = Bits(INPUT, width)
  val exc = Bits(INPUT, 5)
}

class io_cp_dfma extends io_cp_fma(65)
class io_cp_sfma extends io_cp_fma(33)

class LaneFMA extends Module
{
  val io = new Bundle
  {
    val valid = Bool(INPUT)
    val fn    = Bits(INPUT, SZ_VAU1_FN)
    val in0   = Bits(INPUT, SZ_DATA)
    val in1   = Bits(INPUT, SZ_DATA)
    val in2   = Bits(INPUT, SZ_DATA)
    val out   = Bits(OUTPUT, SZ_DATA)
    val exc   = Bits(OUTPUT, SZ_EXC)

    val cp_dfma = new io_cp_dfma()
    val cp_sfma = new io_cp_sfma()
  }

  if (HAVE_FMA)
  {
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

    val dfma = Module(new hardfloat.mulAddSubRecodedFloatN(52, 12))
    dfma.io.op := Fill(2,val_fma_dp) & fma_op
    dfma.io.a := Fill(65,val_fma_dp) & fma_multiplicand
    dfma.io.b := Fill(65,val_fma_dp) & fma_multiplier
    dfma.io.c := Fill(65,val_fma_dp) & fma_addend
    dfma.io.roundingMode := Fill(3,val_fma_dp) & io.fn(RG_VAU1_RM)
    val result_dp = Cat(dfma.io.exceptionFlags, dfma.io.out)

    val sfma = Module(new hardfloat.mulAddSubRecodedFloatN(23, 9))
    sfma.io.op := Fill(2,val_fma_sp) & fma_op
    sfma.io.a := Fill(33,val_fma_sp) & fma_multiplicand(32,0)
    sfma.io.b := Fill(33,val_fma_sp) & fma_multiplier(32,0)
    sfma.io.c := Fill(33,val_fma_sp) & fma_addend(32,0)
    sfma.io.roundingMode := Fill(3,val_fma_sp) & io.fn(RG_VAU1_RM)
    val result_sp = Cat(sfma.io.exceptionFlags, sfma.io.out)

    val result = Mux(
      io.fn(RG_VAU1_FP), result_dp,
      Cat(result_sp(37,33), Bits("hFFFFFFFF",32), result_sp(32,0)))

    val pipereg = ShiftRegister(result, FMA_STAGES, io.valid)

    Match(pipereg, io.exc, io.out)

    io.cp_dfma.valid := Bool(false)
    io.cp_sfma.valid := Bool(false)
  }
  else
  {
    require(DFMA_STAGES >= SFMA_STAGES)

    val rocket_cmd = MuxLookup(
      io.fn(RG_VAU1_FN), FCMD_X, Array(
        VAU1_ADD -> FCMD_ADD,
        VAU1_SUB -> FCMD_SUB,
        VAU1_MUL -> FCMD_MUL,
        VAU1_MADD -> FCMD_MADD,
        VAU1_MSUB -> FCMD_MSUB,
        VAU1_NMSUB -> FCMD_NMSUB,
        VAU1_NMADD -> FCMD_NMADD
      ))

    val fn = io.fn(RG_VAU1_FN)
    val two_operands = fn === VAU1_ADD || fn === VAU1_SUB || fn === VAU1_MUL

    io.cp_dfma.valid := io.valid && io.fn(RG_VAU1_FP) === Bits(1)
    io.cp_dfma.cmd := rocket_cmd
    io.cp_dfma.rm := io.fn(RG_VAU1_RM)
    io.cp_dfma.in1 := io.in0
    io.cp_dfma.in2 := Mux(two_operands, io.in2, io.in1)
    io.cp_dfma.in3 := io.in2

    io.cp_sfma.valid := io.valid && io.fn(RG_VAU1_FP) === Bits(0)
    io.cp_sfma.cmd := rocket_cmd
    io.cp_sfma.rm := io.fn(RG_VAU1_RM)
    io.cp_sfma.in1 := io.in0
    io.cp_sfma.in2 := Mux(two_operands, io.in2, io.in1)
    io.cp_sfma.in3 := io.in2

    val dp = ShiftRegister(io.fn(RG_VAU1_FP), DFMA_STAGES-1)

    io.out := Mux(dp, io.cp_dfma.out,
                  Cat(Bits("hFFFFFFFF",32), ShiftRegister(io.cp_sfma.out, DFMA_STAGES-SFMA_STAGES)))
    io.exc := Mux(dp, io.cp_dfma.exc,
                  ShiftRegister(io.cp_sfma.exc, DFMA_STAGES-SFMA_STAGES))
  }
}

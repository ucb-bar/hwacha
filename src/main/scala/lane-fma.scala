package hwacha

import Chisel._
import Node._
import Constants._

class LaneFMA(implicit conf: HwachaConfiguration) extends Module
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

    val cp_dfma = new rocket.ioFMA(65).flip
    val cp_sfma = new rocket.ioFMA(33).flip
  }

  if (conf.fma)
  {
    // use in0 & in2 for a two operand flop (add,sub,mul)
    // use in0, in1, & in2 otherwise

    val fma_op = MuxCase(
      Bits("b00",2), Array(
        (io.fn(RG_VAU1_FN) === VAU1_SUB || io.fn(RG_VAU1_FN) === VAU1_MSUB) -> Bits("b01",2),
        (io.fn(RG_VAU1_FN) === VAU1_NMSUB) -> Bits("b10",2),
        (io.fn(RG_VAU1_FN) === VAU1_NMADD) -> Bits("b11",2)
      ))

    val one_dp = Bits("h8000000000000000", 65) // recoded
    val one_sp = Bits("h80000000", 65) // recoded
    val one_hp = Bits("h3c00",65) // not recoded
    val fma_multiplicand = io.in0
    val fma_multiplier = MuxCase(
      io.in1, Array(
        ((io.fn(RG_VAU1_FP) === Bits("b01",2)) && (io.fn(RG_VAU1_FN) === VAU1_ADD || io.fn(RG_VAU1_FN) === VAU1_SUB)) -> one_dp,
        ((io.fn(RG_VAU1_FP) === Bits("b00",2)) && (io.fn(RG_VAU1_FN) === VAU1_ADD || io.fn(RG_VAU1_FN) === VAU1_SUB)) -> one_sp,
        ((io.fn(RG_VAU1_FP) === Bits("b10",2)) && (io.fn(RG_VAU1_FN) === VAU1_ADD || io.fn(RG_VAU1_FN) === VAU1_SUB)) -> one_hp,
        ((io.fn(RG_VAU1_FN) === VAU1_MUL)) -> io.in2
      ))

    val fma_addend = Mux(
      io.fn(RG_VAU1_FN) === VAU1_MUL, Bits(0,65),
      io.in2)

    val val_fma_dp = io.valid & (io.fn(RG_VAU1_FP) === Bits("b01",2))
    val val_fma_sp = io.valid & (io.fn(RG_VAU1_FP) === Bits("b00",2))
    val val_fma_hp = io.valid & (io.fn(RG_VAU1_FP) === Bits("b10",2))

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


    val recoded_hp_a = hardfloat.floatNToRecodedFloatN(fma_multiplicand, 10, 6)
    val recoded_hp_b = hardfloat.floatNToRecodedFloatN(fma_multiplier, 10, 6)
    val recoded_hp_c = hardfloat.floatNToRecodedFloatN(fma_addend, 10, 6)
    val hfma = Module(new hardfloat.mulAddSubRecodedFloatN(10, 6))
    hfma.io.op := Fill(2, val_fma_hp) & fma_op
    hfma.io.a := Fill(17, val_fma_hp) & recoded_hp_a
    hfma.io.b := Fill(17, val_fma_hp) & recoded_hp_b
    hfma.io.c := Fill(17, val_fma_hp) & recoded_hp_c
    hfma.io.roundingMode := Fill(3, val_fma_hp) & io.fn(RG_VAU1_RM)
    val result_hp_unrecoded = hardfloat.recodedFloatNToFloatN(hfma.io.out, 10, 6)

    val result = MuxCase(
      Bits("h1FFFFFFFFFFFFFFFFF",70), Array(
      (val_fma_dp) -> result_dp,
      (val_fma_sp) -> Cat(result_sp(37,33), Bits("hFFFFFFFF",32), result_sp(32,0)),
      (val_fma_hp) -> Cat(hfma.io.exceptionFlags, Bits("h1FFFFFFFFFFFF",49), result_hp_unrecoded)))

    val pipereg = ShiftRegister(result, conf.fma_stages, io.valid)

    Match(pipereg, io.exc, io.out)

    io.cp_dfma.valid := Bool(false)
    io.cp_sfma.valid := Bool(false)
  }
  else
  {
    require(conf.dfma_stages >= conf.sfma_stages)

    val rocket_cmd = MuxLookup(
      io.fn(RG_VAU1_FN), rocket.FPConstants.FCMD_ADD, Array(
        VAU1_ADD -> rocket.FPConstants.FCMD_ADD,
        VAU1_SUB -> rocket.FPConstants.FCMD_SUB,
        VAU1_MUL -> rocket.FPConstants.FCMD_MUL,
        VAU1_MADD -> rocket.FPConstants.FCMD_MADD,
        VAU1_MSUB -> rocket.FPConstants.FCMD_MSUB,
        VAU1_NMSUB -> rocket.FPConstants.FCMD_NMSUB,
        VAU1_NMADD -> rocket.FPConstants.FCMD_NMADD
      ))

    val fn = io.fn(RG_VAU1_FN)
    val two_operands = fn === VAU1_ADD || fn === VAU1_SUB || fn === VAU1_MUL

    io.cp_dfma.valid := io.valid && io.fn(RG_VAU1_FP) === Bits("b01",2)
    io.cp_dfma.cmd := rocket_cmd
    io.cp_dfma.rm := io.fn(RG_VAU1_RM)
    io.cp_dfma.in1 := io.in0
    io.cp_dfma.in2 := Mux(two_operands, io.in2, io.in1)
    io.cp_dfma.in3 := io.in2

    io.cp_sfma.valid := io.valid && io.fn(RG_VAU1_FP) === Bits("b00",2)
    io.cp_sfma.cmd := rocket_cmd
    io.cp_sfma.rm := io.fn(RG_VAU1_RM)
    io.cp_sfma.in1 := io.in0
    io.cp_sfma.in2 := Mux(two_operands, io.in2, io.in1)
    io.cp_sfma.in3 := io.in2

    val dpp = io.fn(RG_VAU1_FP)
    val dp  = ShiftRegister(dpp(0), conf.dfma_stages-1)

    io.out := Mux(dp, io.cp_dfma.out,
                  Cat(Bits("hFFFFFFFF",32), ShiftRegister(io.cp_sfma.out, conf.dfma_stages-conf.sfma_stages)))
    io.exc := Mux(dp, io.cp_dfma.exc,
                  ShiftRegister(io.cp_sfma.exc, conf.dfma_stages-conf.sfma_stages))
  }
}

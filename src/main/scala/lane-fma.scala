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

    val one_dp = Bits("h8000000000000000", 66) // recoded
    val one_sp = Bits("h1000000080000000", 66) // recoded, confprec'd
    val one_hp = Bits("h780078003c003c00", 66) // not recoded, confprec'd
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
    dfma.io.a := Fill(65,val_fma_dp) & fma_multiplicand(65,0)
    dfma.io.b := Fill(65,val_fma_dp) & fma_multiplier(65,0)
    dfma.io.c := Fill(65,val_fma_dp) & fma_addend(65,0)
    dfma.io.roundingMode := Fill(3,val_fma_dp) & io.fn(RG_VAU1_RM)
    val result_dp = Cat(dfma.io.exceptionFlags, Bits(0), dfma.io.out)

    val sfma = Module(new hardfloat.mulAddSubRecodedFloatN(23, 9))
    sfma.io.op := Fill(2,val_fma_sp) & fma_op
    sfma.io.a := Fill(33,val_fma_sp) & fma_multiplicand(32,0)
    sfma.io.b := Fill(33,val_fma_sp) & fma_multiplier(32,0)
    sfma.io.c := Fill(33,val_fma_sp) & fma_addend(32,0)
    sfma.io.roundingMode := Fill(3,val_fma_sp) & io.fn(RG_VAU1_RM)
    val result_sp = Cat(sfma.io.exceptionFlags, sfma.io.out)

    // instantiate second sfma unit (confprec)
    val result_sp1 = Bits(width = 38)
    if (conf.confprec) {
      val sfma1 = Module(new hardfloat.mulAddSubRecodedFloatN(23, 9))
      sfma1.io.op := Fill(2,val_fma_sp) & fma_op
      sfma1.io.a := Fill(33,val_fma_sp) & fma_multiplicand(65,33)
      sfma1.io.b := Fill(33,val_fma_sp) & fma_multiplier(65,33)
      sfma1.io.c := Fill(33,val_fma_sp) & fma_addend(65,33)
      sfma1.io.roundingMode := Fill(3,val_fma_sp) & io.fn(RG_VAU1_RM)
      result_sp1 := Cat(sfma1.io.exceptionFlags, sfma1.io.out)
    } else {
      result_sp1 := Bits(0, 38)
    }

    val recoded_hp_a = hardfloat.floatNToRecodedFloatN(fma_multiplicand(15,0), 10, 6)
    val recoded_hp_b = hardfloat.floatNToRecodedFloatN(fma_multiplier(15,0), 10, 6)
    val recoded_hp_c = hardfloat.floatNToRecodedFloatN(fma_addend(15,0), 10, 6)
    val hfma = Module(new hardfloat.mulAddSubRecodedFloatN(10, 6))
    hfma.io.op := Fill(2, val_fma_hp) & fma_op
    hfma.io.a := Fill(17, val_fma_hp) & recoded_hp_a
    hfma.io.b := Fill(17, val_fma_hp) & recoded_hp_b
    hfma.io.c := Fill(17, val_fma_hp) & recoded_hp_c
    hfma.io.roundingMode := Fill(3, val_fma_hp) & io.fn(RG_VAU1_RM)
    val result_hp_unrecoded = hardfloat.recodedFloatNToFloatN(hfma.io.out, 10, 6)

    // packing format: 65 64       49 48       33 32 31       16 15       00
    //                  0 [  hfma3  ] [  hfma2  ]  0 [  hfma1  ] [  hfma0  ]
    val result_hp1 = Bits(width = 16)
    val result_hp2 = Bits(width = 16)
    val result_hp3 = Bits(width = 16)
    val or_exc_hp = Bits(width = 5)
    if (conf.confprec) {
      val recoded_hp_a1 = hardfloat.floatNToRecodedFloatN(fma_multiplicand(31,16), 10, 6)
      val recoded_hp_b1 = hardfloat.floatNToRecodedFloatN(fma_multiplier(31,16), 10, 6)
      val recoded_hp_c1 = hardfloat.floatNToRecodedFloatN(fma_addend(31,16), 10, 6)
      val hfma1 = Module(new hardfloat.mulAddSubRecodedFloatN(10, 6))
      hfma1.io.op := Fill(2, val_fma_hp) & fma_op
      hfma1.io.a := Fill(17, val_fma_hp) & recoded_hp_a1
      hfma1.io.b := Fill(17, val_fma_hp) & recoded_hp_b1
      hfma1.io.c := Fill(17, val_fma_hp) & recoded_hp_c1
      hfma1.io.roundingMode := Fill(3, val_fma_hp) & io.fn(RG_VAU1_RM)
      result_hp1 := hardfloat.recodedFloatNToFloatN(hfma1.io.out, 10, 6)

      val recoded_hp_a2 = hardfloat.floatNToRecodedFloatN(fma_multiplicand(48,33), 10, 6)
      val recoded_hp_b2 = hardfloat.floatNToRecodedFloatN(fma_multiplier(48,33), 10, 6)
      val recoded_hp_c2 = hardfloat.floatNToRecodedFloatN(fma_addend(48,33), 10, 6)
      val hfma2 = Module(new hardfloat.mulAddSubRecodedFloatN(10, 6))
      hfma2.io.op := Fill(2, val_fma_hp) & fma_op
      hfma2.io.a := Fill(17, val_fma_hp) & recoded_hp_a2
      hfma2.io.b := Fill(17, val_fma_hp) & recoded_hp_b2
      hfma2.io.c := Fill(17, val_fma_hp) & recoded_hp_c2
      hfma2.io.roundingMode := Fill(3, val_fma_hp) & io.fn(RG_VAU1_RM)
      result_hp2 := hardfloat.recodedFloatNToFloatN(hfma2.io.out, 10, 6)

      val recoded_hp_a3 = hardfloat.floatNToRecodedFloatN(fma_multiplicand(64,49), 10, 6)
      val recoded_hp_b3 = hardfloat.floatNToRecodedFloatN(fma_multiplier(64,49), 10, 6)
      val recoded_hp_c3 = hardfloat.floatNToRecodedFloatN(fma_addend(64,49), 10, 6)
      val hfma3 = Module(new hardfloat.mulAddSubRecodedFloatN(10, 6))
      hfma3.io.op := Fill(2, val_fma_hp) & fma_op
      hfma3.io.a := Fill(17, val_fma_hp) & recoded_hp_a3
      hfma3.io.b := Fill(17, val_fma_hp) & recoded_hp_b3
      hfma3.io.c := Fill(17, val_fma_hp) & recoded_hp_c3
      hfma3.io.roundingMode := Fill(3, val_fma_hp) & io.fn(RG_VAU1_RM)
      result_hp3 := hardfloat.recodedFloatNToFloatN(hfma3.io.out, 10, 6)

      or_exc_hp := hfma.io.exceptionFlags | hfma1.io.exceptionFlags | 
                   hfma2.io.exceptionFlags | hfma3.io.exceptionFlags
    } else {
      result_hp1 := Bits(0, 16)
      result_hp2 := Bits(0, 16)
      result_hp3 := Bits(0, 16)
      or_exc_hp := Bits(0, 5)
    }


    val result = Bits(width = 71)

    if (conf.confprec) {
      val or_exc_sp = result_sp(37,33) | result_sp1(37,33)
      result := MuxCase(
        Bits("h3FFFFFFFFFFFFFFFFF",71), Array(
        (val_fma_dp) -> result_dp,
        (val_fma_sp) -> Cat(or_exc_sp, result_sp1(32,0), result_sp(32,0)),
        (val_fma_hp) -> Cat(or_exc_hp, Bits(0), result_hp3, result_hp2, 
                                       Bits(0), result_hp1, result_hp_unrecoded)))
    } else {
      result := MuxCase(
        Bits("h3FFFFFFFFFFFFFFFFF",71), Array(
        (val_fma_dp) -> result_dp,
        (val_fma_sp) -> Cat(result_sp(37,33), Bits("h1FFFFFFFF",33), result_sp(32,0)),
        (val_fma_hp) -> Cat(hfma.io.exceptionFlags, Bits("h3FFFFFFFFFFFF",50), result_hp_unrecoded)))
    }

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
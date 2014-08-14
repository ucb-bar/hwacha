package hwacha

import Chisel._
import Node._
import Constants._
import Compaction._

class LaneFMA extends HwachaModule
{
  val io = new Bundle {
    val valid = Bool(INPUT)
    val fn = new VAU1Fn().asInput
    val in0 = Bits(INPUT, SZ_DATA)
    val in1 = Bits(INPUT, SZ_DATA)
    val in2 = Bits(INPUT, SZ_DATA)
    val out = Bits(OUTPUT, SZ_DATA)
    val exc = Bits(OUTPUT, rocket.FPConstants.FLAGS_SZ)
  }

  def OP(ops: Bits*) = ops.toList.map(x => {io.fn.op === x}).reduceLeft(_ || _)
  def FP(fp: Bits) = io.fn.fp === fp

  // use in0 & in2 for a two operand flop (add,sub,mul)
  // use in0, in1, & in2 otherwise

  val fma_op = MuxCase(
    Bits("b00",2), Array(
      OP(A1_SUB, A1_MSUB) -> Bits("b01",2),
      OP(A1_NMSUB) -> Bits("b10",2),
      OP(A1_NMADD) -> Bits("b11",2)
    ))

  val one_dp = repack_float_d(Bits("h8000000000000000", 65)) // recoded, swizzled
  val one_sp = repack_float_s(Bits("h080000000", 33), Bits("h080000000", 33)) // recoded, confprec'd, swizzled
  val one_hp = repack_float_h(Bits("h3c00", 16), Bits("h3c00", 16), Bits("h3c00", 16), Bits("h3c00", 16)) // not recoded, confprec'd, swizzled
  val fma_multiplicand = io.in0
  val fma_multiplier = MuxCase(
    io.in1, Array(
      (FP(FPD) && OP(A1_ADD, A1_SUB)) -> one_dp,
      (FP(FPS) && OP(A1_ADD, A1_SUB)) -> one_sp,
      (FP(FPH) && OP(A1_ADD, A1_SUB)) -> one_hp,
      OP(A1_MUL) -> io.in2
    ))

  val fma_addend = Mux(OP(A1_MUL), Bits(0, 65), io.in2)

  val val_fma_dp = io.valid & FP(FPD)
  val val_fma_sp = io.valid & FP(FPS)
  val val_fma_hp = io.valid & FP(FPH)

  val dfma = Module(new hardfloat.mulAddSubRecodedFloatN(52, 12))
  dfma.io.op := Fill(2,val_fma_dp) & fma_op
  dfma.io.a := Fill(65,val_fma_dp) & unpack_float_d(fma_multiplicand, 0)
  dfma.io.b := Fill(65,val_fma_dp) & unpack_float_d(fma_multiplier, 0)
  dfma.io.c := Fill(65,val_fma_dp) & unpack_float_d(fma_addend, 0)
  dfma.io.roundingMode := Fill(3,val_fma_dp) & io.fn.rm
  val result_dp0 = dfma.io.out

  val sfma = Module(new hardfloat.mulAddSubRecodedFloatN(23, 9))
  sfma.io.op := Fill(2,val_fma_sp) & fma_op
  sfma.io.a := Fill(33,val_fma_sp) & unpack_float_s(fma_multiplicand, 0)
  sfma.io.b := Fill(33,val_fma_sp) & unpack_float_s(fma_multiplier, 0)
  sfma.io.c := Fill(33,val_fma_sp) & unpack_float_s(fma_addend, 0)
  sfma.io.roundingMode := Fill(3,val_fma_sp) & io.fn.rm
  val result_sp0 = sfma.io.out

  // instantiate second sfma unit (confprec)
  val result_sp1 = Bits(width = 33)
  val or_exc_sp = Bits(width = 5)
  if (confprec) {
    val sfma1 = Module(new hardfloat.mulAddSubRecodedFloatN(23, 9))
    sfma1.io.op := Fill(2,val_fma_sp) & fma_op
    sfma1.io.a := Fill(33,val_fma_sp) & unpack_float_s(fma_multiplicand, 1)
    sfma1.io.b := Fill(33,val_fma_sp) & unpack_float_s(fma_multiplier, 1)
    sfma1.io.c := Fill(33,val_fma_sp) & unpack_float_s(fma_addend, 1)
    sfma1.io.roundingMode := Fill(3,val_fma_sp) & io.fn.rm
    result_sp1 := sfma1.io.out

    or_exc_sp := sfma.io.exceptionFlags | sfma1.io.exceptionFlags
  } else {
    result_sp1 := Bits(0, 38)
    or_exc_sp := Bits(0, 5)
  }

  val recoded_hp_a = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_multiplicand, 0).toUInt, 10, 6)
  val recoded_hp_b = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_multiplier, 0).toUInt, 10, 6)
  val recoded_hp_c = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_addend, 0).toUInt, 10, 6)
  val hfma = Module(new hardfloat.mulAddSubRecodedFloatN(10, 6))
  hfma.io.op := Fill(2, val_fma_hp) & fma_op
  hfma.io.a := Fill(17, val_fma_hp) & recoded_hp_a
  hfma.io.b := Fill(17, val_fma_hp) & recoded_hp_b
  hfma.io.c := Fill(17, val_fma_hp) & recoded_hp_c
  hfma.io.roundingMode := Fill(3, val_fma_hp) & io.fn.rm
  val result_hp0 = hardfloat.recodedFloatNToFloatN(hfma.io.out, 10, 6)

  // packing format: 65 64       49 48       33 32 31       16 15       00
  //                  0 [  hfma3  ] [  hfma2  ]  0 [  hfma1  ] [  hfma0  ]
  val result_hp1 = Bits(width = 16)
  val result_hp2 = Bits(width = 16)
  val result_hp3 = Bits(width = 16)
  val or_exc_hp = Bits(width = 5)
  if (confprec) {
    val recoded_hp_a1 = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_multiplicand, 1).toUInt, 10, 6)
    val recoded_hp_b1 = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_multiplier, 1).toUInt, 10, 6)
    val recoded_hp_c1 = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_addend, 1).toUInt, 10, 6)
    val hfma1 = Module(new hardfloat.mulAddSubRecodedFloatN(10, 6))
    hfma1.io.op := Fill(2, val_fma_hp) & fma_op
    hfma1.io.a := Fill(17, val_fma_hp) & recoded_hp_a1
    hfma1.io.b := Fill(17, val_fma_hp) & recoded_hp_b1
    hfma1.io.c := Fill(17, val_fma_hp) & recoded_hp_c1
    hfma1.io.roundingMode := Fill(3, val_fma_hp) & io.fn.rm
    result_hp1 := hardfloat.recodedFloatNToFloatN(hfma1.io.out, 10, 6)

    val recoded_hp_a2 = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_multiplicand, 2)toUInt, 10, 6)
    val recoded_hp_b2 = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_multiplier, 2).toUInt, 10, 6)
    val recoded_hp_c2 = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_addend, 2).toUInt, 10, 6)
    val hfma2 = Module(new hardfloat.mulAddSubRecodedFloatN(10, 6))
    hfma2.io.op := Fill(2, val_fma_hp) & fma_op
    hfma2.io.a := Fill(17, val_fma_hp) & recoded_hp_a2
    hfma2.io.b := Fill(17, val_fma_hp) & recoded_hp_b2
    hfma2.io.c := Fill(17, val_fma_hp) & recoded_hp_c2
    hfma2.io.roundingMode := Fill(3, val_fma_hp) & io.fn.rm
    result_hp2 := hardfloat.recodedFloatNToFloatN(hfma2.io.out, 10, 6)

    val recoded_hp_a3 = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_multiplicand, 3).toUInt, 10, 6)
    val recoded_hp_b3 = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_multiplier, 3).toUInt, 10, 6)
    val recoded_hp_c3 = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_addend, 3).toUInt, 10, 6)
    val hfma3 = Module(new hardfloat.mulAddSubRecodedFloatN(10, 6))
    hfma3.io.op := Fill(2, val_fma_hp) & fma_op
    hfma3.io.a := Fill(17, val_fma_hp) & recoded_hp_a3
    hfma3.io.b := Fill(17, val_fma_hp) & recoded_hp_b3
    hfma3.io.c := Fill(17, val_fma_hp) & recoded_hp_c3
    hfma3.io.roundingMode := Fill(3, val_fma_hp) & io.fn.rm
    result_hp3 := hardfloat.recodedFloatNToFloatN(hfma3.io.out, 10, 6)

    or_exc_hp := hfma.io.exceptionFlags | hfma1.io.exceptionFlags | hfma2.io.exceptionFlags | hfma3.io.exceptionFlags
  } else {
    result_hp1 := Bits(0, 16)
    result_hp2 := Bits(0, 16)
    result_hp3 := Bits(0, 16)
    or_exc_hp := Bits(0, 5)
  }


  val result = Bits(width = 71)

  if (confprec) {
    result := MuxCase(
      Bits("h3FFFFFFFFFFFFFFFFF",71), Array(
      (val_fma_dp) -> Cat(dfma.io.exceptionFlags, repack_float_d(result_dp0)),
      (val_fma_sp) -> Cat(or_exc_sp, repack_float_s(result_sp0, result_sp1)),
      (val_fma_hp) -> Cat(or_exc_hp, repack_float_h(result_hp0, result_hp1, result_hp2, result_hp3))
      ))
  } else {
    result := MuxCase(
      Bits("h3FFFFFFFFFFFFFFFFF",71), Array(
      (val_fma_dp) -> Cat(dfma.io.exceptionFlags, pack_float_d(result_dp0, 0)),
      (val_fma_sp) -> Cat(sfma.io.exceptionFlags, pack_float_s(result_sp0, 0)),
      (val_fma_hp) -> Cat(hfma.io.exceptionFlags, pack_float_h(result_hp0, 0))
      ))
  }

  val pipereg = ShiftRegister(result, fma_stages, io.valid)

  Match(pipereg, io.exc, io.out)
}

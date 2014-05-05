package hwacha

import Chisel._
import Node._
import Constants._
import Compaction._
import scala.collection.mutable.ArrayBuffer

class LaneFMA(implicit conf: HwachaConfiguration) extends Module
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

  val dfmas = new ArrayBuffer[Bits]
  val or_exc_dps = new ArrayBuffer[Bits]
  for (i <- 0 until N_FPD) {
    if (conf.mixedprec || i == 0) {
      val dfma = Module(new hardfloat.mulAddSubRecodedFloatN(52, 12))
      dfma.io.op := Fill(2,val_fma_dp) & fma_op
      dfma.io.a := Fill(65,val_fma_dp) & unpack_float_d(fma_multiplicand, i)
      dfma.io.b := Fill(65,val_fma_dp) & unpack_float_d(fma_multiplier, i)
      dfma.io.c := Fill(65,val_fma_dp) & unpack_float_d(fma_addend, i)
      dfma.io.roundingMode := Fill(3, val_fma_dp) & io.fn.rm
      dfmas += dfma.io.out
      or_exc_dps += dfma.io.exceptionFlags
    } else {
      dfmas += Bits(0, SZ_FPD)
    }
  }

  val sfmas = new ArrayBuffer[Bits]
  val or_exc_sps = new ArrayBuffer[Bits]
  for (i <- 0 until N_FPS) {
    if (conf.mixedprec || i == 0) {
      val sfma = Module(new hardfloat.mulAddSubRecodedFloatN(23, 9))
      sfma.io.op := Fill(2,val_fma_sp) & fma_op
      sfma.io.a := Fill(33,val_fma_sp) & unpack_float_s(fma_multiplicand, i)
      sfma.io.b := Fill(33,val_fma_sp) & unpack_float_s(fma_multiplier, i)
      sfma.io.c := Fill(33,val_fma_sp) & unpack_float_s(fma_addend, i)
      sfma.io.roundingMode := Fill(3, val_fma_sp) & io.fn.rm
      sfmas += sfma.io.out
      or_exc_sps += sfma.io.exceptionFlags
    } else {
      sfmas += Bits(0, SZ_FPS)
    }
  }

  // packing format: 65 64       49 48       33 32 31       16 15       00
  //                  0 [  hfma3  ] [  hfma2  ]  0 [  hfma1  ] [  hfma0  ]
  val hfmas = new ArrayBuffer[Bits]
  val or_exc_hps = new ArrayBuffer[Bits]
  for (i <- 0 until N_FPH) {
    if (conf.mixedprec || i == 0) {
      val recoded_hp_a = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_multiplicand, i).toUInt, 10, 6)
      val recoded_hp_b = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_multiplier, i).toUInt, 10, 6)
      val recoded_hp_c = hardfloat.floatNToRecodedFloatN(unpack_float_h(fma_addend, i).toUInt, 10, 6)

      val hfma = Module(new hardfloat.mulAddSubRecodedFloatN(10, 6))
      hfma.io.op := Fill(2, val_fma_hp) & fma_op
      hfma.io.a := Fill(17, val_fma_hp) & recoded_hp_a
      hfma.io.b := Fill(17, val_fma_hp) & recoded_hp_b
      hfma.io.c := Fill(17, val_fma_hp) & recoded_hp_c
      hfma.io.roundingMode := Fill(3, val_fma_hp) & io.fn.rm
      hfmas += hardfloat.recodedFloatNToFloatN(hfma.io.out, 10, 6)
      or_exc_hps += hfma.io.exceptionFlags
    } else {
      hfmas += Bits(0, SZ_FPH)
    }
  }

  val or_exc_dp = or_exc_dps.reduce(_ | _)
  val or_exc_sp = or_exc_sps.reduce(_ | _)
  val or_exc_hp = or_exc_hps.reduce(_ | _)

  val result = Bits(width = 71)

  if (conf.mixedprec) {
    result := MuxCase(
      Bits("h3FFFFFFFFFFFFFFFFF",71), Array(
      (val_fma_dp) -> Cat(or_exc_dp, repack_float_d(dfmas(0))),
      (val_fma_sp) -> Cat(or_exc_sp, repack_float_s(sfmas(0), sfmas(1))),
      (val_fma_hp) -> Cat(or_exc_hp, repack_float_h(hfmas(0), hfmas(1), hfmas(2), hfmas(3)))
      ))
  } else {
    result := MuxCase(
      Bits("h3FFFFFFFFFFFFFFFFF",71), Array(
      (val_fma_dp) -> Cat(or_exc_dp, pack_float_d(dfmas(0), 0)),
      (val_fma_sp) -> Cat(or_exc_sp, pack_float_s(sfmas(0), 0)),
      (val_fma_hp) -> Cat(or_exc_hp, pack_float_h(hfmas(0), 0))
      ))
  }

  val pipereg = ShiftRegister(result, conf.fma_stages, io.valid)

  Match(pipereg, io.exc, io.out)
}

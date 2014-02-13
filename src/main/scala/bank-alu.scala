package hwacha

import Chisel._
import Node._
import Constants._
import Compaction._

class Shifter extends Module
{
  val io = new Bundle()
  {
    val fn = Bits(INPUT, SZ_VIU_FN)
    val shamt = UInt(INPUT, 6)
    val in = Bits(INPUT, 64)
    val out = Bits(OUTPUT, 64)
  }

  val left = MuxLookup(
    io.fn(RG_VIU_FN), Bool(false), Array(
      I_SLL -> Bool(true),
      I_SRL -> Bool(false),
      I_SRA -> Bool(false)
    ))

  val arith = MuxLookup(
    io.fn(RG_VIU_FN), Bool(false), Array(
      I_SLL -> Bool(false),
      I_SRL -> Bool(false),
      I_SRA -> Bool(true)
    ))

  val trunc = MuxCase(
    Bool(false), Array(
      (io.fn(RG_VIU_FN) === I_SRL && io.fn(RG_VIU_DW) === DW32) -> Bool(true),
      (io.fn(RG_VIU_FN) === I_SRL && io.fn(RG_VIU_DW) === DW64) -> Bool(false),
      (io.fn(RG_VIU_FN) === I_SRA && io.fn(RG_VIU_DW) === DW64) -> Bool(false),
      (io.fn(RG_VIU_FN) === I_SRA && io.fn(RG_VIU_DW) === DW32) -> Bool(true)
    ))

  val shift_in_hi_32 = Mux(io.fn(RG_VIU_FN) === I_SRA, Fill(32, io.in(31)), UInt(0,32))
  val shift_in_hi = Mux(io.fn(RG_VIU_DW) === DW64, io.in(63,32), shift_in_hi_32)
  val shift_in_r = Cat(shift_in_hi, io.in(31,0))
  val shift_in = Mux(left, Reverse(shift_in_r), shift_in_r)

  val shift_out = (Cat(arith & shift_in(63), shift_in).toSInt >> io.shamt)(63,0)

  io.out := Mux(
    left, Reverse(shift_out),
    shift_out)
}

class BankALU extends Module
{
  val io = new Bundle()
  {
    val valid = Bool(INPUT)
    val wen = Bool(INPUT)
    val wen_masked = Bool(OUTPUT)
    val fn = Bits(INPUT, SZ_VIU_FN)
    val utidx = Bits(INPUT, SZ_VLEN)
    val in0 = Bits(INPUT, SZ_DATA)
    val in1 = Bits(INPUT, SZ_DATA)
    val out = Bits(OUTPUT, SZ_DATA)
  }

  def VIU_FN(fn: Bits*) = fn.toList.map(x => {s1_fn(RG_VIU_FN) === x}).reduceLeft(_ || _)
  def VIU_FP(fp: Bits) = s1_fn(RG_VIU_FP) === fp
  def VIU_DW(dw: Bits) = s1_fn(RG_VIU_DW) === dw

  val s1_valid = Reg(next=io.valid)
  val s1_fn = RegEnable(io.fn, io.valid)
  val s1_utidx = RegEnable(io.utidx, io.valid)
  val s1_in0 = RegEnable(io.in0, io.valid)
  val s1_in1 = RegEnable(io.in1, io.valid)

  val sub = MuxCase(
    Bits(0, 1), Array(
      VIU_FN(I_ADD) -> Bits(0, 1),
      VIU_FN(I_SUB) -> Bits(1, 1)
    ))

  val adder_out =
    (Cat(s1_in0(63, 0), sub).toUInt +
     Cat(s1_in1(63, 0) ^ Fill(64, sub), sub).toUInt)(64, 1)

  // SLL, SRL, SRA
  val dw = s1_fn(RG_VIU_DW)
  val sra = (s1_fn(RG_VIU_FN) === I_SRA)
  val shamt = Cat(s1_in1(5) & (dw === DW64), s1_in1(4,0)).toUInt
  val shright = sra || (s1_fn(RG_VIU_FN) === I_SRL)
  val shin_hi_32 = Mux(sra, Fill(32, s1_in0(31)), UInt(0,32))
  val shin_hi = Mux(dw === DW64, s1_in0(63,32), shin_hi_32)
  val shin_r = Cat(shin_hi, s1_in0(31,0))
  val shin = Mux(shright, shin_r, Reverse(shin_r))
  val shout_r = (Cat(sra & shin_r(63), shin).toSInt >> shamt)(63,0)
  val shift_out = Mux(s1_fn(RG_VIU_FN) === I_SLL, Reverse(shout_r), shout_r)


  val ltu = (s1_in0.toUInt < s1_in1.toUInt)
  val lt = (s1_in0(63) === s1_in1(63)) && ltu || s1_in0(63) && ~s1_in1(63)

  val unpacked_s_s1_in0 = unpack_float_s(s1_in0, 0)
  val unpacked_s_s1_in1 = unpack_float_s(s1_in1, 0)

  val unpacked_d_s1_in0 = unpack_float_d(s1_in0, 0)
  val unpacked_d_s1_in1 = unpack_float_d(s1_in1, 0)

  val comp_sp = Module(new hardfloat.recodedFloat32Compare(24,8))
  comp_sp.io.a := unpacked_s_s1_in0
  comp_sp.io.b := unpacked_s_s1_in1
  val less_sp = comp_sp.io.a_lt_b
  val equal_sp = comp_sp.io.a_eq_b

  val comp_dp = Module(new hardfloat.recodedFloat64Compare(53,11))
  comp_dp.io.a := unpacked_d_s1_in0
  comp_dp.io.b := unpacked_d_s1_in1
  val less_dp = comp_dp.io.a_lt_b
  val equal_dp = comp_dp.io.a_eq_b

  val comp = 
    VIU_FN(I_SLT)  & lt  |
    VIU_FN(I_SLTU) & ltu |
    VIU_FN(I_FEQ)  & (VIU_FP(FPS) & equal_sp | VIU_FP(FPD) & equal_dp) |
    VIU_FN(I_FLE)  & (VIU_FP(FPS) & (equal_sp | less_sp) | VIU_FP(FPD) & (equal_dp | less_dp)) |
    VIU_FN(I_FLT)  & (VIU_FP(FPS) & less_sp | VIU_FP(FPD) & less_dp)

  val sj_sp =
    VIU_FN(I_FSJ) & unpacked_s_s1_in1(32)   |
    VIU_FN(I_FSJN) & ~unpacked_s_s1_in1(32) |
    VIU_FN(I_FSJX) & (unpacked_s_s1_in1(32) ^ unpacked_s_s1_in0(32))

  val sj_dp = 
    VIU_FN(I_FSJ) & unpacked_d_s1_in1(64)   |
    VIU_FN(I_FSJN) & ~unpacked_d_s1_in1(64) |
    VIU_FN(I_FSJX) & (unpacked_d_s1_in1(64) ^ unpacked_d_s1_in0(64))

  val is_in0_nan =
    VIU_FP(FPS) & (unpacked_s_s1_in0(31,29) === Bits("b111", 3)) |
    VIU_FP(FPD) & (unpacked_d_s1_in0(63,61) === Bits("b111", 3))

  val is_in1_nan = 
    VIU_FP(FPS) & (unpacked_s_s1_in1(31,29) === Bits("b111", 3)) |
    VIU_FP(FPD) & (unpacked_d_s1_in1(63,61) === Bits("b111", 3))

  val want_min = MuxCase(Bool(false), Array(
    VIU_FN(I_FMIN) -> Bool(true),
    VIU_FN(I_FMAX) -> Bool(false)))

  val less_fp = 
    VIU_FP(FPS) & less_sp | 
    VIU_FP(FPD) & less_dp

  val fminmax = Mux(
    is_in1_nan || ~is_in0_nan && (want_min === less_fp), s1_in0,
    s1_in1)

  val next_mask = MuxCase(
    Bool(true), Array(
      VIU_FN(I_MOVZ) -> ~s1_in0(0),
      VIU_FN(I_MOVN) ->  s1_in0(0)
    ))

  val next_result64 = MuxCase(
    Bits(0, SZ_DATA), Array(
      VIU_FN(I_IDX) -> Cat(Fill(SZ_DATA-SZ_VLEN, Bits("b0", 1)), s1_utidx),
      VIU_FN(I_MOV,I_MOVZ,I_MOVN) -> s1_in1,
      VIU_FN(I_ADD,I_SUB) -> Cat(Bits(0, 1), adder_out),
      VIU_FN(I_SLL,I_SRL,I_SRA) -> Cat(Bits(0, 1), shift_out),
      VIU_FN(I_SLT,I_SLTU,I_FEQ,I_FLE,I_FLT) -> Cat(Bits(0, 64), comp),
      VIU_FN(I_AND) -> (s1_in0 & s1_in1),
      VIU_FN(I_OR) -> (s1_in0 | s1_in1),
      VIU_FN(I_XOR) -> (s1_in0 ^ s1_in1),
      (VIU_FN(I_FSJ,I_FSJN,I_FSJX) && VIU_FP(FPS)) -> pack_float_s(Cat(sj_sp, unpacked_s_s1_in0(31,0)), 0),
      (VIU_FN(I_FSJ,I_FSJN,I_FSJX) && VIU_FP(FPD)) -> pack_float_d(Cat(sj_dp, unpacked_d_s1_in0(63,0)), 0),
      VIU_FN(I_FMIN,I_FMAX) -> fminmax
    ))

  val next_result = MuxCase(
    Bits(0, SZ_DATA), Array(
      VIU_DW(DW64) -> next_result64,
      VIU_DW(DW32) -> Cat(Bits(0, 1), Fill(32,next_result64(31)), next_result64(31,0))
    ))

  val s2_mask_en = Reg(next = s1_valid && (VIU_FN(I_MOVZ) || VIU_FN(I_MOVN)))
  val s2_mask = Reg(Bool())
  val s2_result = Reg(Bits(width = 65))

  when (s1_valid) {
    s2_mask := next_mask
    s2_result := next_result
  }

  io.wen_masked := io.wen && (!s2_mask_en || s2_mask)
  io.out := s2_result
}

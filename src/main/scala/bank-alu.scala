package hwacha

import Chisel._
import Node._
import Constants._
import Compaction._

class BankALU extends Module
{
  val io = new Bundle() {
    val valid = Bool(INPUT)
    val wen = Bool(INPUT)
    val wen_masked = Bool(OUTPUT)
    val fn = new VIUFn().asInput
    val utidx = Bits(INPUT, SZ_VLEN)
    val in0 = Bits(INPUT, SZ_DATA)
    val in1 = Bits(INPUT, SZ_DATA)
    val out = Bits(OUTPUT, SZ_DATA)
  }

  def OP(ops: Bits*) = ops.toList.map(x => {s1_fn.op === x}).reduceLeft(_ || _)
  def FP(fp: Bits) = s1_fn.fp === fp
  def DW(dw: Bits) = s1_fn.dw === dw

  val s1_valid = Reg(next=io.valid)
  val s1_fn = RegEnable(io.fn, io.valid)
  val s1_utidx = RegEnable(io.utidx, io.valid)
  val s1_in0 = RegEnable(io.in0, io.valid)
  val s1_in1 = RegEnable(io.in1, io.valid)

  val sub = MuxCase(
    Bits(0, 1), Array(
      OP(I_ADD) -> Bits(0, 1),
      OP(I_SUB) -> Bits(1, 1)
    ))

  val adder_out =
    (Cat(s1_in0(63, 0), sub).toUInt +
     Cat(s1_in1(63, 0) ^ Fill(64, sub), sub).toUInt)(64, 1)

  // SLL, SRL, SRA
  val sra = OP(I_SRA)
  val shamt = Cat(s1_in1(5) & DW(DW64), s1_in1(4,0)).toUInt
  val shright = sra || OP(I_SRL)
  val shin_hi_32 = Mux(sra, Fill(32, s1_in0(31)), UInt(0,32))
  val shin_hi = Mux(DW(DW64), s1_in0(63,32), shin_hi_32)
  val shin_r = Cat(shin_hi, s1_in0(31,0))
  val shin = Mux(shright, shin_r, Reverse(shin_r))
  val shout_r = (Cat(sra & shin_r(63), shin).toSInt >> shamt)(63,0)
  val shift_out = Mux(OP(I_SLL), Reverse(shout_r), shout_r)


  val ltu = (s1_in0.toUInt < s1_in1.toUInt)
  val lt = (s1_in0(63) === s1_in1(63)) && ltu || s1_in0(63) && ~s1_in1(63)

  val unpacked_s_s1_in0 = unpack_w(s1_in0, 0)
  val unpacked_s_s1_in1 = unpack_w(s1_in1, 0)

  val unpacked_d_s1_in0 = unpack_d(s1_in0, 0)
  val unpacked_d_s1_in1 = unpack_d(s1_in1, 0)

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
    OP(I_SLT)  & lt  |
    OP(I_SLTU) & ltu |
    OP(I_FEQ)  & (FP(FPS) & equal_sp | FP(FPD) & equal_dp) |
    OP(I_FLE)  & (FP(FPS) & (equal_sp | less_sp) | FP(FPD) & (equal_dp | less_dp)) |
    OP(I_FLT)  & (FP(FPS) & less_sp | FP(FPD) & less_dp)

  val sj_sp =
    OP(I_FSJ) & unpacked_s_s1_in1(32)   |
    OP(I_FSJN) & ~unpacked_s_s1_in1(32) |
    OP(I_FSJX) & (unpacked_s_s1_in1(32) ^ unpacked_s_s1_in0(32))

  val sj_dp = 
    OP(I_FSJ) & unpacked_d_s1_in1(64)   |
    OP(I_FSJN) & ~unpacked_d_s1_in1(64) |
    OP(I_FSJX) & (unpacked_d_s1_in1(64) ^ unpacked_d_s1_in0(64))

  val is_in0_nan =
    FP(FPS) & (unpacked_s_s1_in0(31,29) === Bits("b111", 3)) |
    FP(FPD) & (unpacked_d_s1_in0(63,61) === Bits("b111", 3))

  val is_in1_nan = 
    FP(FPS) & (unpacked_s_s1_in1(31,29) === Bits("b111", 3)) |
    FP(FPD) & (unpacked_d_s1_in1(63,61) === Bits("b111", 3))

  val want_min = MuxCase(Bool(false), Array(
    OP(I_FMIN) -> Bool(true),
    OP(I_FMAX) -> Bool(false)))

  val less_fp = 
    FP(FPS) & less_sp | 
    FP(FPD) & less_dp

  val fminmax = Mux(
    is_in1_nan || ~is_in0_nan && (want_min === less_fp), s1_in0,
    s1_in1)

  val next_mask = MuxCase(
    Bool(true), Array(
      OP(I_MOVZ) -> ~s1_in0(0),
      OP(I_MOVN) ->  s1_in0(0)
    ))

  val next_result64 = MuxCase(
    Bits(0, SZ_DATA), Array(
      OP(I_IDX) -> s1_utidx,
      OP(I_MOV1) -> s1_in0,
      OP(I_MOV2,I_MOVZ,I_MOVN) -> s1_in1,
      OP(I_ADD,I_SUB) -> adder_out,
      OP(I_SLL,I_SRL,I_SRA) -> shift_out,
      OP(I_SLT,I_SLTU,I_FEQ,I_FLE,I_FLT) -> comp,
      OP(I_AND) -> (s1_in0 & s1_in1),
      OP(I_OR) -> (s1_in0 | s1_in1),
      OP(I_XOR) -> (s1_in0 ^ s1_in1),
      (OP(I_FSJ,I_FSJN,I_FSJX) && FP(FPS)) -> repack_w(Cat(sj_sp, unpacked_s_s1_in0(31,0))),
      (OP(I_FSJ,I_FSJN,I_FSJX) && FP(FPD)) -> repack_d(Cat(sj_dp, unpacked_d_s1_in0(63,0))),
      OP(I_FMIN,I_FMAX) -> fminmax
    ))

  val next_result = MuxCase(
    Bits(0, SZ_DATA), Array(
      DW(DW64) -> next_result64,
      DW(DW32) -> Cat(Bits(0, 1), Fill(32,next_result64(31)), next_result64(31,0))
    ))

  val s2_mask_en = Reg(next = s1_valid && (OP(I_MOVZ) || OP(I_MOVN)))
  val s2_mask = Reg(Bool())
  val s2_result = Reg(Bits(width = SZ_DATA))

  when (s1_valid) {
    s2_mask := next_mask
    s2_result := next_result
  }

  io.wen_masked := io.wen && (!s2_mask_en || s2_mask)
  io.out := s2_result
}

package hwacha

import Chisel._
import Node._
import Constants._
import Packing._

class BankALUResult extends Bundle
{
  val out = Bits(width = SZ_D)
}

class BankALUSlice extends Module
{
  val io = new Bundle() {
    val req = Valid(new Bundle {
      val fn = new VIUFn
      val eidx = Bits(INPUT, SZ_VLEN)
      val in0 = Bits(INPUT, SZ_D)
      val in1 = Bits(INPUT, SZ_D)
    }).flip
    val resp = Valid(new BankALUResult)
  }

  val s1_valid = Reg(next=io.req.valid)
  val s1_fn = RegEnable(io.req.bits.fn, io.req.valid)
  val s1_eidx = RegEnable(io.req.bits.eidx, io.req.valid)
  val s1_in0 = RegEnable(io.req.bits.in0, io.req.valid)
  val s1_in1 = RegEnable(io.req.bits.in1, io.req.valid)

  val sub = MuxCase(
    Bits(0, 1), Array(
      s1_fn.op_is(I_ADD) -> Bits(0, 1),
      s1_fn.op_is(I_ADDU) -> Bits(0, 1),
      s1_fn.op_is(I_SUB) -> Bits(1, 1)
    ))

  val adder_out =
    (Cat(s1_in0(63, 0), sub).toUInt +
     Cat(s1_in1(63, 0) ^ Fill(64, sub), sub).toUInt)(64, 1)

  // SLL, SRL, SRA
  val sra = s1_fn.op_is(I_SRA)
  val shamt = Cat(s1_in1(5) & s1_fn.dw_is(DW64), s1_in1(4,0)).toUInt
  val shright = sra || s1_fn.op_is(I_SRL)
  val shin_hi_32 = Mux(sra, Fill(32, s1_in0(31)), UInt(0,32))
  val shin_hi = Mux(s1_fn.dw_is(DW64), s1_in0(63,32), shin_hi_32)
  val shin_r = Cat(shin_hi, s1_in0(31,0))
  val shin = Mux(shright, shin_r, Reverse(shin_r))
  val shout_r = (Cat(sra & shin_r(63), shin).toSInt >> shamt)(63,0)
  val shift_out = Mux(s1_fn.op_is(I_SLL), Reverse(shout_r), shout_r)


  val ltu = (s1_in0.toUInt < s1_in1.toUInt)
  val lt = (s1_in0(63) === s1_in1(63)) && ltu || s1_in0(63) && ~s1_in1(63)

  val comp = s1_fn.op_is(I_SLT) & lt | s1_fn.op_is(I_SLTU) & ltu

  val s1_in0_sp = unpack_w(s1_in0, 0)
  val s1_in1_sp = unpack_w(s1_in1, 0)

  val s1_in0_dp = unpack_d(s1_in0, 0)
  val s1_in1_dp = unpack_d(s1_in1, 0)

  val sj_sp =
    s1_fn.op_is(I_FSJ) & s1_in1_sp(31)   |
    s1_fn.op_is(I_FSJN) & ~s1_in1_sp(31) |
    s1_fn.op_is(I_FSJX) & (s1_in1_sp(31) ^ s1_in0_sp(31))

  val sj_dp = 
    s1_fn.op_is(I_FSJ) & s1_in1_dp(63)   |
    s1_fn.op_is(I_FSJN) & ~s1_in1_dp(63) |
    s1_fn.op_is(I_FSJX) & (s1_in1_dp(63) ^ s1_in0_dp(63))

  val s1_result64 = MuxCase(
    Bits(0, SZ_D), Array(
      s1_fn.op_is(I_IDX) -> s1_eidx,
      s1_fn.op_is(I_MOV0) -> s1_in0,
      s1_fn.op_is(I_ADD,I_ADDU,I_SUB) -> adder_out,
      s1_fn.op_is(I_SLL,I_SRL,I_SRA) -> shift_out,
      s1_fn.op_is(I_SLT,I_SLTU) -> comp,
      s1_fn.op_is(I_AND) -> (s1_in0 & s1_in1),
      s1_fn.op_is(I_OR) -> (s1_in0 | s1_in1),
      s1_fn.op_is(I_XOR) -> (s1_in0 ^ s1_in1),
      (s1_fn.op_is(I_FSJ,I_FSJN,I_FSJX) && s1_fn.fp_is(FPS)) -> expand_float_s(Cat(sj_sp, s1_in0_sp(30,0))),
      (s1_fn.op_is(I_FSJ,I_FSJN,I_FSJX) && s1_fn.fp_is(FPD)) -> expand_float_d(Cat(sj_dp, s1_in0_dp(62,0)))
    ))

  val s1_result = MuxCase(
    Bits(0, SZ_D), Array(
      s1_fn.dw_is(DW64) -> s1_result64,
      s1_fn.dw_is(DW32) -> expand_w(s1_result64(31,0))
    ))

  val result = new BankALUResult
  result.out := s1_result

  io.resp := Pipe(s1_valid, result, 1)
}

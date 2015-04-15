package hwacha

import Chisel._
import Node._
import Constants._

class ALUResult extends Bundle {
  val out = Bits(width = SZ_D)
}

class ALUSlice extends HwachaModule with Packing {
  val io = new Bundle {
    val req = Valid(new HwachaBundle {
      val fn = new VIUFn
      val eidx = Bits(width = bVLen)
      val in0 = Bits(width = SZ_D)
      val in1 = Bits(width = SZ_D)
    }).flip
    val resp = Valid(new ALUResult)
  }

  val fn = io.req.bits.fn
  val eidx = io.req.bits.eidx
  val in0 = io.req.bits.in0
  val in1 = io.req.bits.in1

  val sub = MuxCase(
    Bits(0, 1), Array(
      fn.op_is(I_ADD) -> Bits(0, 1),
      fn.op_is(I_ADDU) -> Bits(0, 1),
      fn.op_is(I_SUB) -> Bits(1, 1)
    ))

  val adder_out =
    (Cat(in0(63, 0), sub).toUInt +
     Cat(in1(63, 0) ^ Fill(64, sub), sub).toUInt)(64, 1)

  // SLL, SRL, SRA
  val sra = fn.op_is(I_SRA)
  val shamt = Cat(in1(5) & fn.dw_is(DW64), in1(4,0)).toUInt
  val shright = sra || fn.op_is(I_SRL)
  val shin_hi_32 = Mux(sra, Fill(32, in0(31)), UInt(0,32))
  val shin_hi = Mux(fn.dw_is(DW64), in0(63,32), shin_hi_32)
  val shin_r = Cat(shin_hi, in0(31,0))
  val shin = Mux(shright, shin_r, Reverse(shin_r))
  val shout_r = (Cat(sra & shin_r(63), shin).toSInt >> shamt)(63,0)
  val shift_out = Mux(fn.op_is(I_SLL), Reverse(shout_r), shout_r)


  val ltu = (in0.toUInt < in1.toUInt)
  val lt = (in0(63) === in1(63)) && ltu || in0(63) && ~in1(63)

  val comp = fn.op_is(I_SLT) & lt | fn.op_is(I_SLTU) & ltu

  val in0_sp = unpack_w(in0, 0)
  val in1_sp = unpack_w(in1, 0)

  val in0_dp = unpack_d(in0, 0)
  val in1_dp = unpack_d(in1, 0)

  val sj_sp =
    fn.op_is(I_FSJ) & in1_sp(31)   |
    fn.op_is(I_FSJN) & ~in1_sp(31) |
    fn.op_is(I_FSJX) & (in1_sp(31) ^ in0_sp(31))

  val sj_dp = 
    fn.op_is(I_FSJ) & in1_dp(63)   |
    fn.op_is(I_FSJN) & ~in1_dp(63) |
    fn.op_is(I_FSJX) & (in1_dp(63) ^ in0_dp(63))

  val s0_result64 = MuxCase(
    Bits(0, SZ_D), Array(
      fn.op_is(I_IDX) -> eidx,
      fn.op_is(I_MOV0) -> in0,
      fn.op_is(I_ADD,I_ADDU,I_SUB) -> adder_out,
      fn.op_is(I_SLL,I_SRL,I_SRA) -> shift_out,
      fn.op_is(I_SLT,I_SLTU) -> comp,
      fn.op_is(I_AND) -> (in0 & in1),
      fn.op_is(I_OR) -> (in0 | in1),
      fn.op_is(I_XOR) -> (in0 ^ in1),
      (fn.op_is(I_FSJ,I_FSJN,I_FSJX) && fn.fp_is(FPS)) -> expand_float_s(Cat(sj_sp, in0_sp(30,0))),
      (fn.op_is(I_FSJ,I_FSJN,I_FSJX) && fn.fp_is(FPD)) -> expand_float_d(Cat(sj_dp, in0_dp(62,0)))
    ))

  val s0_result = MuxCase(
    Bits(0, SZ_D), Array(
      fn.dw_is(DW64) -> s0_result64,
      fn.dw_is(DW32) -> expand_w(s0_result64(31,0))
    ))

  val result = new ALUResult
  result.out := s0_result

  io.resp := Pipe(io.req.valid, result, 1)
}

package hwacha

import Chisel._
import freechips.rocketchip.config._

class ALUOperand(implicit p: Parameters) extends VXUBundle()(p)
  with LanePred with Rate {
  val fn = new VIUFn
  val eidx = Bits(width = bMLVLen - bStrip)
  val in0 = Bits(width = SZ_D)
  val in1 = Bits(width = SZ_D)
}

class ALUResult(implicit p: Parameters) extends VXUBundle()(p) {
  val out = Bits(width = SZ_D)
  val cmp = Bits(width = nPack)
}

class ALUSlice(aid: Int)(implicit p: Parameters) extends VXUModule()(p) with Packing {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val req = Valid(new ALUOperand).flip
    val resp = Valid(new ALUResult)
  }

  val fn = io.req.bits.fn
  val eidx = io.req.bits.eidx
  val in0 = io.req.bits.in0
  val in1 = io.req.bits.in1
  val (in0_hi, in0_lo) = (in0(63, 32), in0(31, 0))
  val (in1_hi, in1_lo) = (in1(63, 32), in1(31, 0))

  val (fn_dw64, fn_dw32) = (fn.dw_is(DW64), fn.dw_is(DW32))

  // TODO: Support x4 rate with halfwords
  val rate_x2 = if (confprec) (io.req.bits.rate === UInt(1)) else Bool(false)

  val sub = fn.op_is(I_SUB)
  class Adder(in0: UInt, in1: UInt, cin: UInt, w: Int = SZ_W) {
    private val bits =
      Cat(UInt(0, 1), in0, cin).asUInt +
      Cat(UInt(0, 1), in1 ^ Fill(w, sub), cin).asUInt
    val (cout, out) = (bits(w+1), bits(w, 1))
  }
  val adder_out = if (confprec) {
    val lo = new Adder(in0_lo, in1_lo, sub)
    val hi = new Adder(in0_hi, in1_hi, Mux(rate_x2, sub, lo.cout))
    Cat(hi.out, lo.out)
  } else new Adder(in0, in1, sub, SZ_D).out

  // SLL, SRL, SRA
  val sra = fn.op_is(I_SRA)
  val shright = sra || fn.op_is(I_SRL)
  val shamt_lo = Cat(in1(5) & fn_dw64, in1(4,0)).asUInt
  val shamt_hi = Cat(in1(37) & fn_dw64, in1(36,32)).asUInt
  // Swap shift amounts if left shift and x2 rate
  val shamt_hi_swap = Mux(rate_x2 && shright, shamt_hi, shamt_lo)
  val shamt_lo_swap = Mux(rate_x2 && !shright, shamt_hi, shamt_lo)
  val shfill_lo = sra & in0(31)
  val shin_hi = Mux(fn_dw32 && !rate_x2, Fill(32, shfill_lo), in0_hi)
  val shin_r = Cat(shin_hi, in0_lo)
  val shin = Mux(shright, shin_r, Reverse(shin_r))
  val shfill_hi = sra & shin_r(63)
  val shout_r = (0 to 5).foldLeft(shin) { case (bits, i) =>
    val n = 1 << i
    val pad_hi = Fill(n, shfill_hi)
    if (confprec) {
      val pad_lo = Mux(rate_x2, Fill(n, shfill_lo), bits(31+n, 32))
      Cat(Mux(shamt_hi_swap(i), if (i < 5) Cat(pad_hi, bits(63, 32+n)) else pad_hi, bits(63, 32)),
          Mux(shamt_lo_swap(i), if (i < 5) Cat(pad_lo, bits(31, n)) else pad_lo, bits(31, 0)))
    } else Mux(shamt_lo(i), Cat(pad_hi, bits(63, n)), bits)
  }
  val shift_out = Mux(fn.op_is(I_SLL), Reverse(shout_r), shout_r)

  val slt = fn.op_is(I_SLT)
  val sltu = fn.op_is(I_SLTU)
  trait CmpResult {
    val ltu, lt, eq: Bool
    val set = (slt && lt) || (sltu && ltu)
  }
  class Comparator(in0: UInt, in1: UInt, w: Int = SZ_W) extends {
    private val (neg0, neg1) = (in0(w-1), in1(w-1))
    val ltu = (in0 < in1)
    val lt = ((neg0 === neg1) && ltu) || (neg0 && !neg1)
    val eq = (in0 === in1)
  } with CmpResult
  val cmp_lo = new Comparator(in0_lo, in1_lo)
  val cmp_hi = new Comparator(in0_hi, in1_hi)
  val cmp_d = new {
    private val _ltu = cmp_hi.eq && cmp_lo.ltu
    val ltu = cmp_hi.ltu || _ltu
    val lt = cmp_hi.lt || _ltu
    val eq = cmp_hi.eq && cmp_lo.eq
  } with CmpResult
  private def cmp(fn: CmpResult => Bool) =
    Mux(rate_x2, Cat(fn(cmp_hi), fn(cmp_lo)), fn(cmp_d))
  val set = cmp(_.set)
  val set_out = if (confprec) Cat(set(1), UInt(0, 31), set(0)) else cmp_d.set

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

  val s0_result64 = Mux1H(Seq(
      fn.op_is(I_IDX) -> Cat(eidx, UInt(aid, bStrip)),
      fn.op_is(I_MOV0) -> in0,
      fn.op_is(I_ADD,I_ADDU,I_SUB) -> adder_out,
      fn.op_is(I_SLL,I_SRL,I_SRA) -> shift_out,
      fn.op_is(I_SLT,I_SLTU) -> set_out,
      fn.op_is(I_AND) -> (in0 & in1),
      fn.op_is(I_OR) -> (in0 | in1),
      fn.op_is(I_XOR) -> (in0 ^ in1),
      (fn.op_is(I_FSJ,I_FSJN,I_FSJX) && fn.fp_is(FPS)) -> expand_float_s(Cat(sj_sp, in0_sp(30,0))),
      (fn.op_is(I_FSJ,I_FSJN,I_FSJX) && fn.fp_is(FPD)) -> expand_float_d(Cat(sj_dp, in0_dp(62,0)))
    ))

  val s0_result = MuxCase(
    Bits(0, SZ_D), Array(
      (fn_dw64 || rate_x2) -> s0_result64,
      fn_dw32 -> expand_w(s0_result64(31,0))
    ))

  val s0_cmp = Mux1H(Seq(
      fn.op_is(I_CEQ) -> cmp(_.eq),
      fn.op_is(I_CLT) -> cmp(_.lt),
      fn.op_is(I_CLTU) -> cmp(_.ltu)
    ))

  val result = Wire(new ALUResult)
  result.out := s0_result
  result.cmp := s0_cmp

  io.resp := Pipe(io.req.valid && io.req.bits.active(), result, stagesALU)
}

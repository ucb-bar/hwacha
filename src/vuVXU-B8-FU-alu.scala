package hwacha

import Chisel._
import Node._
import Constants._
import hardfloat._

class Shifter extends Component
{
  val io = new Bundle()
  {
    val fn    = Bits(SZ_VIU_FN, INPUT)
    val shamt = UFix(6, INPUT)
    val in    = Bits(64, INPUT)
    val out   = Bits(64, OUTPUT)
  }

  val left = MuxLookup(
    io.fn(RG_VIU_FN), Bits(0, 1), Array(
      VIU_SLL -> Bits(1, 1),
      VIU_SRL -> Bits(0, 1),
      VIU_SRA -> Bits(0, 1)
    ))

  val arith = MuxLookup(
    io.fn(RG_VIU_FN), Bits(0, 1), Array(
      VIU_SLL -> Bits(0, 1),
      VIU_SRL -> Bits(0, 1),
      VIU_SRA -> Bits(1, 1)
    ))

  val trunc = MuxCase(
    Bits(0, 1), Array(
      (io.fn(RG_VIU_FN) === VIU_SRL && io.fn(RG_VIU_DW) === DW32) -> Bits(1,1),
      (io.fn(RG_VIU_FN) === VIU_SRL && io.fn(RG_VIU_DW) === DW64) -> Bits(0,1),
      (io.fn(RG_VIU_FN) === VIU_SRA) -> Bits(0,1)
    ))

  val shift_in = Mux(
    left, Reverse(io.in), 
    Cat(Fill(32, ~trunc) & io.in(63,32), io.in(31,0)))

  val shift_out = (Cat(arith & shift_in(63), shift_in).toFix >> io.shamt)(63,0)

  io.out := Mux(
    left, Reverse(shift_out),
    shift_out)
}

class vuVXU_Banked8_FU_alu extends Component
{
  val io = new Bundle()
  {
    val valid      = Bool(INPUT)
    val wen        = Bool(INPUT)
    val wen_masked = Bool(OUTPUT)
    val fn         = Bits(SZ_VIU_FN, INPUT)
    val utidx      = Bits(SZ_VLEN, INPUT)
    val in0        = Bits(SZ_DATA, INPUT)
    val in1        = Bits(SZ_DATA, INPUT)
    val out        = Bits(SZ_DATA, OUTPUT)
  }

  def VIU_FN(fn: Bits*) = fn.toList.map(x => {reg_fn(RG_VIU_FN) === x}).reduceLeft(_ || _)
  def VIU_FP(fp: Bits) = reg_fn(RG_VIU_FP) === fp
  def VIU_DW(dw: Bits) = reg_fn(RG_VIU_DW) === dw

  val reg_fn     = Reg(io.fn)
  val reg_utidx  = Reg(io.utidx)
  val reg_in0    = Reg(io.in0)
  val reg_in1    = Reg(io.in1)
  val reg_mask   = Reg(){Bits(width = 1)}
  val reg_result = Reg(){Bits(width = 65)}

  val sub = MuxCase(
    Bits(0, 1), Array(
      VIU_FN(VIU_ADD) -> Bits(0, 1),
      VIU_FN(VIU_SUB) -> Bits(1, 1)
    ))

  val adder_out =
    (Cat(reg_in0(63, 0), sub).toUFix +
     Cat(reg_in1(63, 0) ^ Fill(64, sub), sub).toUFix)(64, 1)

  val shamt = MuxCase(
    UFix(0, 6), Array(
      VIU_DW(DW64) -> reg_in1(5,0).toUFix,
      VIU_DW(DW32) -> Cat(Bits(0, 1), reg_in1(4,0)).toUFix
    ))

  val shifter = new Shifter()
  shifter.io.fn    := reg_fn
  shifter.io.shamt := shamt
  shifter.io.in    := reg_in0(63,0)
  val shift_out     = shifter.io.out

  val ltu = (reg_in0.toUFix < reg_in1.toUFix)
  val lt = (reg_in0(63) === reg_in1(63)) & ltu | reg_in0(63) & ~reg_in1(63)

  val comp_sp = new recodedFloat32Compare(24,8)
  comp_sp.io.a := reg_in0(32,0)
  comp_sp.io.b := reg_in1(32,0)
  val less_sp = comp_sp.io.a_lt_b
  val equal_sp = comp_sp.io.a_eq_b

  val comp_dp = new recodedFloat64Compare(53,11)
  comp_dp.io.a := reg_in0
  comp_dp.io.b := reg_in1
  val less_dp = comp_dp.io.a_lt_b
  val equal_dp = comp_dp.io.a_eq_b

  val comp = 
    VIU_FN(VIU_SLT)  & lt  |
    VIU_FN(VIU_SLTU) & ltu |
    VIU_FN(VIU_FEQ)  & (VIU_FP(FPS) & equal_sp | VIU_FP(FPD) & equal_dp) |
    VIU_FN(VIU_FLE)  & (VIU_FP(FPS) & (equal_sp | less_sp) | VIU_FP(FPD) & (equal_dp | less_dp)) |
    VIU_FN(VIU_FLT)  & (VIU_FP(FPS) & less_sp | VIU_FP(FPD) & less_dp)

  val sj_sp =
    VIU_FN(VIU_FSJ) & reg_in1(32)   |
    VIU_FN(VIU_FSJN) & ~reg_in1(32) |
    VIU_FN(VIU_FSJX) & (reg_in1(32) ^ reg_in0(32))

  val sj_dp = 
    VIU_FN(VIU_FSJ) & reg_in1(64)   |
    VIU_FN(VIU_FSJN) & ~reg_in1(64) |
    VIU_FN(VIU_FSJX) & (reg_in1(64) ^ reg_in0(64))

  val is_in0_nan =
    VIU_FP(FPS) & (reg_in0(31,29) === Bits("b111", 3)) |
    VIU_FP(FPD) & (reg_in0(63,61) === Bits("b111", 3))

  val is_in1_nan = 
    VIU_FP(FPS) & (reg_in1(31,29) === Bits("b111", 3)) |
    VIU_FP(FPD) & (reg_in1(63,61) === Bits("b111", 3))

  val want_min = MuxCase(Bool(false), Array(
    VIU_FN(VIU_FMIN) -> Bool(true),
    VIU_FN(VIU_FMAX) -> Bool(false)))

  val less_fp = 
    VIU_FP(FPS) & less_sp | 
    VIU_FP(FPD) & less_dp

  val fminmax = Mux(
    is_in1_nan || ~is_in0_nan && (want_min === less_fp), reg_in0,
    reg_in1)

  val next_mask = MuxCase(
    Bits(1, 1), Array(
      VIU_FN(VIU_MOVZ) -> ~reg_in0(0).toBits,
      VIU_FN(VIU_MOVN) ->  reg_in0(0).toBits
    ))

  val next_result64 = MuxCase(
    Bits(0, SZ_DATA), Array(
      VIU_FN(VIU_IDX) -> Cat(Fill(SZ_DATA-SZ_VLEN, Bits("b0", 1)), reg_utidx),
      VIU_FN(VIU_MOV,VIU_MOVZ,VIU_MOVN) -> reg_in1,
      VIU_FN(VIU_ADD,VIU_SUB) -> Cat(Bits(0, 1), adder_out),
      VIU_FN(VIU_SLL,VIU_SRL,VIU_SRA) -> Cat(Bits(0, 1), shift_out),
      VIU_FN(VIU_SLT,VIU_SLTU,VIU_FEQ,VIU_FLE,VIU_FLT) -> Cat(Bits(0, 64), comp),
      VIU_FN(VIU_AND) -> (reg_in0 & reg_in1),
      VIU_FN(VIU_OR) -> (reg_in0 | reg_in1),
      VIU_FN(VIU_XOR) -> (reg_in0 ^ reg_in1),
      (VIU_FN(VIU_FSJ,VIU_FSJN,VIU_FSJX) && VIU_FP(FPS)) -> Cat(Bits("hFFFFFFFF", 32), sj_sp, reg_in0(31,0)),
      (VIU_FN(VIU_FSJ,VIU_FSJN,VIU_FSJX) && VIU_FP(FPD)) -> Cat(sj_dp, reg_in0(63,0)),
      VIU_FN(VIU_FMIN,VIU_FMAX) -> fminmax
    ))

  val next_result = MuxCase(
    Bits(0, SZ_DATA), Array(
      VIU_DW(DW64) -> next_result64,
      VIU_DW(DW32) -> Cat(Bits(0, 1), Fill(32,next_result64(31)), next_result64(31,0))
    ))

  reg_mask   := next_mask
  reg_result := next_result

  io.wen_masked := io.wen & reg_mask
  io.out        := reg_result
}

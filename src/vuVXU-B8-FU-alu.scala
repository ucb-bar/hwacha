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
      viu_SLL -> Bits(1, 1),
      viu_SRL -> Bits(0, 1),
      viu_SRA -> Bits(0, 1)
    ))

  val arith = MuxLookup(
    io.fn(RG_VIU_FN), Bits(0, 1), Array(
      viu_SLL -> Bits(0, 1),
      viu_SRL -> Bits(0, 1),
      viu_SRA -> Bits(1, 1)
    ))

  val trunc = MuxCase(
    Bits(0, 1), Array(
      (io.fn(RG_VIU_FN) === viu_SRL && io.fn(RG_VIU_DW) === DW32) -> Bits(1,1),
      (io.fn(RG_VIU_FN) === viu_SRL && io.fn(RG_VIU_DW) === DW64) -> Bits(0,1),
      (io.fn(RG_VIU_FN) === viu_SRA && io.fn(RG_VIU_DW) === DW64) -> Bits(0,1),
      (io.fn(RG_VIU_FN) === viu_SRA && io.fn(RG_VIU_DW) === DW32) -> Bits(1,1)
    ))

  val shift_in_hi_32 = Mux(io.fn(RG_VIU_FN) === viu_SRA, Fill(32, io.in(31)), UFix(0,32))
  val shift_in_hi = Mux(io.fn(RG_VIU_DW) === DW64, io.in(63,32), shift_in_hi_32)
  val shift_in_r = Cat(shift_in_hi, io.in(31,0))
  val shift_in = Mux(left, Reverse(shift_in_r), shift_in_r)

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
    val branch_result = Bool(OUTPUT)
  }

  def viu_FN(fn: Bits*) = fn.toList.map(x => {reg_fn(RG_VIU_FN) === x}).reduceLeft(_ || _)
  def viu_FP(fp: Bits) = reg_fn(RG_VIU_FP) === fp
  def viu_DW(dw: Bits) = reg_fn(RG_VIU_DW) === dw

  val reg_fn     = Reg(io.fn)
  val reg_utidx  = Reg(io.utidx)
  val reg_in0    = Reg(io.in0)
  val reg_in1    = Reg(io.in1)
  val reg_mask   = Reg(){Bits(width = 1)}
  val reg_result = Reg(){Bits(width = 65)}
  val reg_branch_result = Reg(){ Bool() }

  val sub = MuxCase(
    Bits(0, 1), Array(
      viu_FN(viu_ADD) -> Bits(0, 1),
      viu_FN(viu_SUB) -> Bits(1, 1)
    ))

  val adder_out =
    (Cat(reg_in0(63, 0), sub).toUFix +
     Cat(reg_in1(63, 0) ^ Fill(64, sub), sub).toUFix)(64, 1)

  // SLL, SRL, SRA
  val dw = reg_fn(RG_VIU_DW)
  val sra = (reg_fn(RG_VIU_FN) === viu_SRA)
  val shamt = Cat(reg_in1(5) & (dw === DW64), reg_in1(4,0)).toUFix
  val shright = sra || (reg_fn(RG_VIU_FN) === viu_SRL)
  val shin_hi_32 = Mux(sra, Fill(32, reg_in0(31)), UFix(0,32))
  val shin_hi = Mux(dw === DW64, reg_in0(63,32), shin_hi_32)
  val shin_r = Cat(shin_hi, reg_in0(31,0))
  val shin = Mux(shright, shin_r, Reverse(shin_r))
  val shout_r = (Cat(sra & shin_r(63), shin).toFix >> shamt)(63,0)
  val shift_out = Mux(reg_fn(RG_VIU_FN) === viu_SLL, Reverse(shout_r), shout_r)


  val ltu = (reg_in0.toUFix < reg_in1.toUFix)
  val lt = (reg_in0(63) === reg_in1(63)) & ltu | reg_in0(63) & ~reg_in1(63)
  val eq = reg_in0 === reg_in1

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
    viu_FN(viu_SLT)  & lt  |
    viu_FN(viu_SLTU) & ltu |
    viu_FN(viu_FEQ)  & (viu_FP(FPS) & equal_sp | viu_FP(FPD) & equal_dp) |
    viu_FN(viu_FLE)  & (viu_FP(FPS) & (equal_sp | less_sp) | viu_FP(FPD) & (equal_dp | less_dp)) |
    viu_FN(viu_FLT)  & (viu_FP(FPS) & less_sp | viu_FP(FPD) & less_dp)

  val sj_sp =
    viu_FN(viu_FSJ) & reg_in1(32)   |
    viu_FN(viu_FSJN) & ~reg_in1(32) |
    viu_FN(viu_FSJX) & (reg_in1(32) ^ reg_in0(32))

  val sj_dp = 
    viu_FN(viu_FSJ) & reg_in1(64)   |
    viu_FN(viu_FSJN) & ~reg_in1(64) |
    viu_FN(viu_FSJX) & (reg_in1(64) ^ reg_in0(64))

  val is_in0_nan =
    viu_FP(FPS) & (reg_in0(31,29) === Bits("b111", 3)) |
    viu_FP(FPD) & (reg_in0(63,61) === Bits("b111", 3))

  val is_in1_nan = 
    viu_FP(FPS) & (reg_in1(31,29) === Bits("b111", 3)) |
    viu_FP(FPD) & (reg_in1(63,61) === Bits("b111", 3))

  val want_min = MuxCase(Bool(false), Array(
    viu_FN(viu_FMIN) -> Bool(true),
    viu_FN(viu_FMAX) -> Bool(false)))

  val less_fp = 
    viu_FP(FPS) & less_sp | 
    viu_FP(FPD) & less_dp

  val fminmax = Mux(
    is_in1_nan || ~is_in0_nan && (want_min === less_fp), reg_in0,
    reg_in1)

  val next_mask = MuxCase(
    Bits(1, 1), Array(
      viu_FN(viu_MOVZ) -> ~reg_in0(0),
      viu_FN(viu_MOVN) ->  reg_in0(0)
    ))

  val next_result64 = MuxCase(
    Bits(0, SZ_DATA), Array(
      viu_FN(viu_IDX) -> Cat(Fill(SZ_DATA-SZ_VLEN, Bits("b0", 1)), reg_utidx),
      viu_FN(viu_MOV,viu_MOVZ,viu_MOVN) -> reg_in1,
      viu_FN(viu_ADD,viu_SUB) -> Cat(Bits(0, 1), adder_out),
      viu_FN(viu_SLL,viu_SRL,viu_SRA) -> Cat(Bits(0, 1), shift_out),
      viu_FN(viu_SLT,viu_SLTU,viu_FEQ,viu_FLE,viu_FLT) -> Cat(Bits(0, 64), comp),
      viu_FN(viu_AND) -> (reg_in0 & reg_in1),
      viu_FN(viu_OR) -> (reg_in0 | reg_in1),
      viu_FN(viu_XOR) -> (reg_in0 ^ reg_in1),
      (viu_FN(viu_FSJ,viu_FSJN,viu_FSJX) && viu_FP(FPS)) -> Cat(Bits("hFFFFFFFF", 32), sj_sp, reg_in0(31,0)),
      (viu_FN(viu_FSJ,viu_FSJN,viu_FSJX) && viu_FP(FPD)) -> Cat(sj_dp, reg_in0(63,0)),
      viu_FN(viu_FMIN,viu_FMAX) -> fminmax
    ))

  val next_result = MuxCase(
    Bits(0, SZ_DATA), Array(
      viu_DW(DW64) -> next_result64,
      viu_DW(DW32) -> Cat(Bits(0, 1), Fill(32,next_result64(31)), next_result64(31,0))
    ))

  val branch_result = MuxCase(
    Bool(false), Array(
      viu_FN(viu_BEQ) -> eq,
      viu_FN(viu_BNE) -> !eq,
      viu_FN(viu_BLT) -> lt,
      viu_FN(viu_BLTU) -> ltu,
      viu_FN(viu_BGE) -> !lt,
      viu_FN(viu_BGEU) -> !ltu
    ))

  reg_mask   := next_mask
  reg_result := next_result
  reg_branch_result := branch_result

  io.wen_masked := io.wen & reg_mask
  io.out        := reg_result
  io.branch_result := reg_branch_result
}

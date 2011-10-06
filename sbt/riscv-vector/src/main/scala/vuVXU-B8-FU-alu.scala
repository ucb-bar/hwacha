package riscvVect {
import Chisel._
import Node._

object reverse {
  def apply(in: UFix): UFix = {
    var res = in(0);
    for(i <- 1 until 64)
      res = Cat(res, res(i));
    res
  }
}

class Shifter extends Component {
  val io = new Bundle(){
    val fn     = UFix(DEF_VIU_FN, 'input);,
    val shamt  = UFix(6, 'input);
    val in     = UFix(64, 'input);
    val output = UFix(width = 64, 'output);
  }

  val left = MuxLookup(RG_VIU_FN(io.fn), Bits(0, 1), Array(
    VIU_SLL -> Bits(1, 1),
    VIU_SRL -> Bits(0, 1),
    VIU_SRA -> Bits(0, 1)));

  val arith = MuxLookup(RG_VIU_FN(io.fn), Bits(0, 1), Array(
   VIU_SLL ->  Bits(0, 1),
   VIU_SRL ->  Bits(0, 1),
   VIU_SRA -> Bits(1,1)));

  val trunc = MuxCase(Bits(0, 1), Array(
    (RG_VIU_FN(io.fn) === VIU_SRL && RG_VIU_DW(io.fn) === DW32) -> Bits(1,1),
    (RG_VIU_FN(io.fn) === VIU_SRL && RG_VIU_DW(io.fn) === DW64) -> Bits(0,1),
    (RG_VIU_FN(io.fn) === VIU_SRA) -> Bits(0,1)));

  val shift_in = Mux(left, reverse(io.in), 
		           Cat(Fill(~trunc, 32)&io.in(63,32),io.in(31,0)));

  val shift_out = (Cat(arith & shift_in(63), shift_in).toFix >> io.shamt)(63,0);
  io.out := Mux(left, reverse(shift_out), shift_out);

}

class vuVXU_Banked8_FU_alu extends Component {
  val io = new Bundle() {
    val valid = Bool('input);
    val wen = Bool('input);
    val wen_masked = Bool('output);
    val fn = UFix(DEF_VIU_FN, 'input);
    val utidx = UFix(DEF_VLEN, 'input);
    val in0 = UFix(DEF_DATA, 'input);
    val in1 = UFix(DEF_DATA, 'input);
    val out = UFix(DEF_DATA, 'output);
  );

  def VIU_FN(fn: List[UFix]) = fn.map(x => {RG_VIU_FN(reg_fn) === x}).reduceLeft(_ || _)
  def VIU_FP(fp: Bits) = RG_VIU_FP(reg_fn) === fp;
  def VIU_DW(dw: Bits) = RG_VIU_DW(reg_fn) === dw;

  val reg_fn     = Reg(io.fn);
  val reg_utidx  = Reg(io.utidx);
  val reg_in0    = Reg(io.in0);
  val reg_in1    = Reg(io.in1);
  val reg_mask   = Reg(){Bits(width = 1)};
  val reg_result = Reg(){Bits(width = 65)};

  val sub = MuxCase(Bits(0, 1), Array(
    VIU_FN(List(VIU_ADD)) -> Bits(0, 1),
    VIU_FN(List(VIU_SUB)) -> Bits(1, 1)));

  val adder_out = (Cat(reg_in0(63, 0), sub) + Cat(reg_in1(63, 0) ^ Fill(sub, 64), sub))(64, 1);

  val shamt = MuxCase(Bits(0, 6), Array(
    VIU_DW(DW64) -> reg_in1(5:0),
    VIU_DW(DW32) -> Cat(Bits(0, 1), reg_in1(4:0))));

  val shifter = new Shifter();
  shifter.io.fn    := reg_fn;
  shifter.io.shamt := shamt;
  shifter.io.in    := reg_in0(63,0);
  val shift_out     = shifter.io.out;

  val ltu = (reg_in0 < reg_in1);
  val lt = (reg_in0(63) === reg_in1(63)) & ltu | reg_in0(63) & ~reg_in1(63);

/*
  compareRecodedFloatN#(8,24) comp_sp
  (
    .a(reg_in0[32:0]),
    .b(reg_in1[32:0]),
    .less(less_sp),
    .equal(equal_sp),
    .unordered(unordered_sp),
    .exceptionFlags()
  );

  compareRecodedFloatN#(11,53) comp_dp
  (
    .a(reg_in0),
    .b(reg_in1),
    .less(less_dp),
    .equal(equal_dp),
    .unordered(unordered_dp),
    .exceptionFlags()
  );
*/

  val comp = 
    VIU_FN(List(VIU_SLT)) & lt   |
    VIU_FN(List(VIU_SLTU)) & ltu;
/*
  |
    //VIU_FN(List(VIU_FEQ))      | // & (VIU_FP(FPS) & equal_sp | VIU_FP(FPD) & equal_dp)
    //VIU_FN(List(VIU_FLE))      | // & (VIU_FP(FPS) & (equal_sp | less_sp) | VIU_FP(FPD) & (equal_dp | less_dp))
    //VIU_FN(List(VIU_FLT))        // & (VIU_FP(FPS) & less_sp | VIU_FP(FPD) & less_dp);
*/

  val sj_sp =
    VIU_FN1(List(VIU_FSJ)) & reg_in1(32)   |
    VIU_FN1(List(VIU_FSJN)) & ~reg_in1(32) |
    VIU_FN1(List(VIU_FSJX)) & (reg_in1(32) ^ reg_in0(32));

  val sj_dp = 
    VIU_FN1(List(VIU_FSJ)) & reg_in1(64)   |
    VIU_FN1(List(VIU_FSJN)) & ~reg_in1(64) |
    VIU_FN1(List(VIU_FSJX)) & (reg_in1(64) ^ reg_in0(64));

  val is_in0_nan =
    VIU_FP(FPS) & (reg_in0[31:29] == Bits("b111", 3)) |
    VIU_FP(FPD) & (reg_in0[63:61] == Bits("b111", 3));

  val is_in1_nan = 
    VIU_FP(FPS) & (reg_in1[31:29] == Bits("b111", 3)) |
    VIU_FP(FPD) & (reg_in1[63:61] == Bits("b111", 3));

  val want_min = MuxCase(Bits(0,1), Array(
    VIU_FN1(List(VIU_FMIN)) -> Bits(1, 1),
    VIU_FN1(List(VIU_FMAX)) -> Bits(0, 1)));

/*
  val less_fp = 
    VIU_FP(FPS) & less_sp | 
    VIU_FP(FPD) & less_dp;
*/

  val fminmax = 
    is_in1_nan || ~is_in0_nan && Mux((want_min == less_fp), reg_in0,
				     reg_in1)

  val next_mask = MuxCase(Bits(1, 1), Array(
    VIU_FN1(List(VIU_MOVZ)) -> ~reg_in0(0).toBits,
    VIU_FN1(List(VIU_MOVN)) ->  reg_in0(0).toBits));

  val next_result64 = MuxCase(Bits(0, SZ_DATA), Array(
    VIU_FN( List(VIU_IDX) )                   -> Cat(Fill(Bits("b0", 1), SZ_DATA-SZ_VLEN), reg_utidx),
    VIU_FN( List(VIU_MOV,VIU_MOVZ,VIU_MOVN) ) -> reg_in1m
    VIU_FN( List(VIU_ADD,VIU_SUB) )           -> Cat(Bits(0, 1), adder_out),
    VIU_FN( List(VIU_SLL,VIU_SRL,VIU_SRA) )   -> Cat(Bits(0, 1), shift_out),
    VIU_FN( List(VIU_SLT,VIU_SLTU,VIU_FEQ,VIU_FLE,VIU_FLT) ) -> Cat(Bits(0, 64), comp),
    VIU_FN( List(VIU_AND) )                   -> (reg_in0 & reg_in1),
    VIU_FN( List(VIU_OR) )                    -> (reg_in0 | reg_in1),
    VIU_FN( List(VIU_XOR) )                   -> (reg_in0 ^ reg_in1),
    VIU_FN( List(VIU_FSJ,VIU_FSJN,VIU_FSJX && VIU_FP(FPS)) ) -> Cat(Bits("hFFFF_FFFF", 32), sj_sp, reg_in0(31,0)),
    VIU_FN( List(VIU_FSJ,VIU_FSJN,VIU_FSJX) && VIU_FP(FPD) ) -> Cat(sj_dp, reg_in0(63,0)),
    VIU_FN( List(VIU_FMIN,VIU_FMAX) )         -> fminmax));

  val next_result = MuxCase(Bits(0, SZ_DATA), Array(
    VIU_DW(DW64) -> next_result64
    VIU_DW(DW32) -> Cat(Bits(0, 1), Fill(next_result64(31), 32), next_result64(31,0))));

  reg_mask   := next_mask;
  reg_result := next_result;

  io.wen_masked := wen & reg_mask;
  io.out        := reg_result;

}
}

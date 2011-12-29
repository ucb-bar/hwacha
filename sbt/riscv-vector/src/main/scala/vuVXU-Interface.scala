package riscvVector
{

import Chisel._
import Node._
import Config._

class io_ready_valid[T <: Data]()(data: => T) extends Bundle
{
  val ready = Bool('input);
  val valid = Bool('output);
  val bits = data.asOutput;
}

class io_valid[T <: Data]()(data: => T) extends Bundle
{
  val valid = Bool('output);
  val bits = data.asOutput;
}

class io_arbiter[T <: Data](n: Int)(data: => T) extends Bundle
{
  val in  = Vec(n) { (new io_ready_valid()) { data } }.flip();
  val out = (new io_ready_valid()) { data };
}

class Arbiter[T <: Data](n: Int)(data: => T) extends Component
{
  val io = new io_arbiter(n)(data)
  val vout = Wire { Bool() }

  io.in(0).ready := io.out.ready
  for (i <- 1 to n-1) {
    io.in(i).ready := !io.in(i-1).valid && io.in(i-1).ready
  }

  var dout = io.in(n-1).bits
  for (i <- 1 to n-1)
    dout = Mux(io.in(n-1-i).valid, io.in(n-1-i).bits, dout)

  for (i <- 0 to n-2)
  {
    when (io.in(i).valid) { vout <== Bool(true) }
  }
  vout <== io.in(n-1).valid

  vout ^^ io.out.valid
  dout ^^ io.out.bits
}

//class io_imem_req() extends io_ready_valid() { Bits(width = DEF_ADDR) };
//class io_imem_resp extends io_valid { Bits(width = DEF_INST) };
//class io_vxu_cmdq extends io_ready_valid { Bits(width = DEF_VXU_CMDQ) };
//class io_vxu_immq extends io_ready_valid { Bits(width = DEF_VXU_IMMQ) };
//class io_vxu_ackq extends io_ready_valid { Bits(width = DEF_VXU_ACKQ) };
//class io_vmu_utcmdq extends io_ready_valid { Bits(width = DEF_VMU_UTCMDQ) };
//class io_vmu_utimmq extends io_ready_valid { Bits(width = DEF_VMU_UTIMMQ) };
//class io_vmu_utackq extends io_ready_valid { Bits(width = DEF_VMU_UTACKQ) };
//class io_vldq extends io_ready_valid { Bits(width = DEF_DATA) };
//class io_vsdq extends io_ready_valid { Bits(width = DEF_DATA) };
//class io_utaq extends io_ready_valid { Bits(width = DEF_ADDR) };
//class io_utldq extends io_ready_valid { Bits(width = DEF_DATA) };
//class io_utstq extends io_ready_valid { Bits(width = DEF_DATA) };

class io_imul extends Bundle
{
  val fn = Bits(DEF_VAU0_FN, 'output);
  val in0 = Bits(DEF_XLEN, 'output);
  val in1 = Bits(DEF_XLEN, 'output);
  val out = Bits(DEF_XLEN, 'input);
}

class io_fma extends Bundle
{
  val fn = Bits(DEF_VAU1_FN, 'output);
  val in0 = Bits(DEF_FLEN, 'output);
  val in1 = Bits(DEF_FLEN, 'output);
  val in2 = Bits(DEF_FLEN, 'output);
  val out = Bits(DEF_FLEN, 'input);
  val exc = Bits(DEF_EXC, 'input);
}

//class io_cp_imul extends io_ready_valid { new io_imul() };
//class io_cp_fma extends io_ready_valid { new io_fma() };

class io_vf extends Bundle
{
  val active = Bool('output);
  val fire = Bool('output);
  val stop = Bool('input);
  val pc = Bits(DEF_ADDR, 'output);
  val vlen = Bits(DEF_VLEN, 'output);
  val nxregs = Bits(DEF_REGCNT, 'output);
}

class io_qstall extends Bundle
{
  val vldq = Bool();
  val vsdq = Bool();
  val utaq = Bool();
  val utldq = Bool();
  val utsdq = Bool();
}

class io_vxu_issue_fu extends Bundle
{
  val viu = Bool();
  val vau0 = Bool();
  val vau1 = Bool();
  val vau2 = Bool();
  val vgslu = Bool();
  val vglu = Bool();
  val vgsu = Bool();
  val vgu = Bool();
  val vlu = Bool();
  val vsu = Bool();
}

class io_vxu_issue_fn extends Bundle
{
  val viu = Bits(width = DEF_VIU_FN);
  val vau0 = Bits(width = DEF_VAU0_FN);
  val vau1 = Bits(width = DEF_VAU1_FN);
  val vau2 = Bits(width = DEF_VAU2_FN);
}

class io_vxu_issue_reg extends Bundle
{
  val vs = Bool();
  val vt = Bool();
  val vr = Bool();
  val vd = Bool();
}

class io_vxu_issue_regid_imm extends Bundle
{
  val vs_zero = Bool();
  val vt_zero = Bool();
  val vr_zero = Bool();
  val vd_zero = Bool();
  val vs = Bits(width = DEF_REGLEN);
  val vt = Bits(width = DEF_REGLEN);
  val vr = Bits(width = DEF_REGLEN);
  val vd = Bits(width = DEF_REGLEN);
  val imm = Bits(width = DEF_DATA);
}

class io_vxu_issue_op extends Bundle
{
  val r1w1 = Bool();
  val r2w1 = Bool();
  val r3w1 = Bool();
  val vgslu = Bool();
  val vglu = Bool();
  val vgsu = Bool();
  val vlu = Bool();
  val vsu = Bool();
}

class io_vxu_seq_fu extends Bundle
{
  val viu = Bool();
  val vau0 = Bool();
  val vau1 = Bool();
  val vau2 = Bool();
  val vldq = Bool();
  val vsdq = Bool();
  val utaq = Bool();
  val utldq = Bool();
  val utsdq = Bool();
}

class io_vxu_seq_fn extends Bundle
{
  val viu = Bits(width = DEF_VIU_FN);
  val vau0 = Bits(width = DEF_VAU0_FN);
  val vau1 = Bits(width = DEF_VAU1_FN);
  val vau2 = Bits(width = DEF_VAU2_FN);
}

class io_vxu_seq_regid_imm extends Bundle
{
  val cnt = Bits(width = DEF_BVLEN);
  val utidx = Bits(width = DEF_VLEN);
  val vs_zero = Bool();
  val vt_zero = Bool();
  val vr_zero = Bool();
  val vs = Bits(width = DEF_BREGLEN);
  val vt = Bits(width = DEF_BREGLEN);
  val vr = Bits(width = DEF_BREGLEN);
  val vd = Bits(width = DEF_BREGLEN);
  val imm = Bits(width = DEF_DATA);
}

class io_vxu_expand_read extends Bundle
{
  val ren = Bool();
  val rlast = Bool();
  val rcnt = Bits(width = DEF_BVLEN);
  val raddr = Bits(width = DEF_BREGLEN);
  val roplen = Bits(width = DEF_BOPL);
  val rblen = Bits(width = DEF_BRPORT);
}

class io_vxu_expand_write extends Bundle
{
  val wen = Bool();
  val wlast = Bool();
  val wcnt = Bits(width = DEF_BVLEN);
  val waddr = Bits(width = DEF_BREGLEN);
  val wsel = Bits(width = DEF_BWPORT);
}

class io_vxu_expand_fu_fn extends Bundle
{
  val viu = Bool();
  val viu_fn = Bits(width = DEF_VIU_FN);
  val viu_utidx = Bits(width = DEF_VLEN);
  val viu_imm = Bits(width = DEF_DATA);
  val vau0 = Bool();
  val vau0_fn = Bits(width = DEF_VAU0_FN);
  val vau1 = Bool();
  val vau1_fn = Bits(width = DEF_VAU1_FN);
  val vau2 = Bool();
  val vau2_fn = Bits(width = DEF_VAU2_FN);
  val vldq = Bool();
  val vsdq = Bool();
  val utaq = Bool();
  val utldq = Bool();
  val utsdq = Bool();
}

class io_vxu_issue_to_hazard extends Bundle
{
  val bcnt = Bits(width = DEF_BCNT);
}

class io_vxu_issue_to_seq extends Bundle
{
  val vlen = Bits(width = DEF_VLEN);
  val stride = Bits(width = DEF_REGLEN);
  val bcnt = Bits(width = DEF_BCNT);
}

class io_vxu_issue_to_lane extends Bundle
{
  val bactive = Bits(width = DEF_BANK);
}

class io_vxu_seq_to_hazard extends Bundle
{
  val stall = Bits(width = DEF_STALL);
  val last = Bool();
}

class io_vxu_seq_to_expand extends Bundle
{
  val last = Bool();
}

class io_vxu_expand_to_hazard extends Bundle
{
  val ren = Bool();
  val wen = Bool();
}

class io_lane_to_hazard extends Bundle
{
  val rlast = Bool();
  val wlast = Bool();
}

class io_vxu_issue_tvec extends Bundle
{
  val vf = new io_vf();

  val issue_to_hazard = new io_vxu_issue_to_hazard().asOutput;
  val issue_to_seq = new io_vxu_issue_to_seq().asOutput;
  val issue_to_lane = new io_vxu_issue_to_lane().asOutput;

  val vxu_cmdq = (new io_ready_valid){Bits(width = DEF_VXU_CMDQ)}.flip();
  val vxu_immq = (new io_ready_valid){Bits(width = DEF_VXU_IMMQ)}.flip();
  val vmu_utcmdq = (new io_ready_valid){Bits(width = DEF_VMU_UTCMDQ)};

  val valid = new io_vxu_issue_fu().asOutput;
  val ready = Bool('input);
  val dhazard = new io_vxu_issue_reg().asOutput;
  val shazard = new io_vxu_issue_fu().asOutput;
  val bhazard = new io_vxu_issue_op().asOutput;
  val fn = new io_vxu_issue_fn().asOutput;
  val decoded = new io_vxu_issue_regid_imm().asOutput;
}

class io_vxu_issue_vt extends Bundle
{
  val illegal = Bool('output);

  val imem_req = (new io_ready_valid){Bits(width = DEF_ADDR)};
  val imem_resp = (new io_valid){Bits(width = DEF_INST)}.flip();

  val vmu_utcmdq = (new io_ready_valid){Bits(width = DEF_VMU_UTCMDQ)};
  val vmu_utimmq = (new io_ready_valid){Bits(width = DEF_VMU_UTIMMQ)};

  val vf = new io_vf().flip();

  val valid = new io_vxu_issue_fu().asOutput;
  val ready = Bool('input);
  val dhazard = new io_vxu_issue_reg().asOutput;
  val shazard = new io_vxu_issue_fu().asOutput;
  val bhazard = new io_vxu_issue_op().asOutput;
  val fn = new io_vxu_issue_fn().asOutput;
  val decoded = new io_vxu_issue_regid_imm().asOutput;
}

class io_vxu_issue extends Bundle
{
  val illegal = Bool('output);

  val imem_req = (new io_ready_valid){Bits(width = DEF_ADDR)};
  val imem_resp = (new io_valid){Bits(width = DEF_INST)}.flip();

  val vxu_cmdq = (new io_ready_valid){Bits(width = DEF_VXU_CMDQ)}.flip();
  val vxu_immq = (new io_ready_valid){Bits(width = DEF_VXU_IMMQ)}.flip();
  val vmu_utcmdq = (new io_ready_valid){Bits(width = DEF_VMU_UTCMDQ)};
  val vmu_utimmq = (new io_ready_valid){Bits(width = DEF_VMU_UTIMMQ)};

  val issue_to_hazard = new io_vxu_issue_to_hazard().asOutput;
  val issue_to_seq = new io_vxu_issue_to_seq().asOutput;
  val issue_to_lane = new io_vxu_issue_to_lane().asOutput;

  val tvec_valid = new io_vxu_issue_fu().asOutput;
  val tvec_ready = Bool('input);
  val tvec_dhazard = new io_vxu_issue_reg().asOutput;
  val tvec_shazard = new io_vxu_issue_fu().asOutput;
  val tvec_bhazard = new io_vxu_issue_op().asOutput;
  val tvec_fn = new io_vxu_issue_fn().asOutput;
  val tvec_regid_imm = new io_vxu_issue_regid_imm().asOutput;

  val vt_valid = new io_vxu_issue_fu().asOutput;
  val vt_ready = Bool('input);
  val vt_dhazard = new io_vxu_issue_reg().asOutput;
  val vt_shazard = new io_vxu_issue_fu().asOutput;
  val vt_bhazard = new io_vxu_issue_op().asOutput;
  val vt_fn = new io_vxu_issue_fn().asOutput;
  val vt_regid_imm = new io_vxu_issue_regid_imm().asOutput;
}

class io_vxu_fire extends Bundle
{
  val tvec_valid = new io_vxu_issue_fu().asInput;
  val tvec_ready = Bool('input);
  val tvec_dhazard = new io_vxu_issue_reg().asInput;
  val tvec_shazard = new io_vxu_issue_fu().asInput;
  val tvec_bhazard = new io_vxu_issue_op().asInput;
  val tvec_fn = new io_vxu_issue_fn().asInput;
  val tvec_regid_imm = new io_vxu_issue_regid_imm().asInput;

  val vt_valid = new io_vxu_issue_fu().asInput;
  val vt_ready = Bool('input);
  val vt_dhazard = new io_vxu_issue_reg().asInput;
  val vt_shazard = new io_vxu_issue_fu().asInput;
  val vt_bhazard = new io_vxu_issue_op().asInput;
  val vt_fn = new io_vxu_issue_fn().asInput;
  val vt_regid_imm = new io_vxu_issue_regid_imm().asInput;

  val fire = new io_vxu_issue_fu().asOutput;
  val fire_fn = new io_vxu_issue_fn().asOutput;
  val fire_regid_imm = new io_vxu_issue_regid_imm().asOutput;
}

class io_vxu_hazard extends Bundle
{
  val issue_to_hazard = new io_vxu_issue_to_hazard().asInput;
  val seq_to_hazard = new io_vxu_seq_to_hazard().asInput;
  val expand_to_hazard = new io_vxu_expand_to_hazard().asInput;
  val lane_to_hazard = new io_lane_to_hazard().asInput;

  val tvec_valid = new io_vxu_issue_fu().asInput;
  val tvec_ready = Bool('output);
  val tvec_dhazard = new io_vxu_issue_reg().asInput;
  val tvec_shazard = new io_vxu_issue_fu().asInput;
  val tvec_bhazard = new io_vxu_issue_op().asInput;
  val tvec_fn = new io_vxu_issue_fn().asInput;
  val tvec_regid_imm = new io_vxu_issue_regid_imm().asInput;

  val vt_valid = new io_vxu_issue_fu().asInput;
  val vt_ready = Bool('output);
  val vt_dhazard = new io_vxu_issue_reg().asInput;
  val vt_shazard = new io_vxu_issue_fu().asInput;
  val vt_bhazard = new io_vxu_issue_op().asInput;
  val vt_fn = new io_vxu_issue_fn().asInput;
  val vt_regid_imm = new io_vxu_issue_regid_imm().asInput;

  val fire = new io_vxu_issue_fu().asInput;
  val fire_fn = new io_vxu_issue_fn().asInput;
  val fire_regid_imm = new io_vxu_issue_regid_imm().asInput;
}

class io_vxu_seq extends Bundle
{
  val issue_to_seq = new io_vxu_issue_to_seq().asInput;
  val seq_to_hazard = new io_vxu_seq_to_hazard().asOutput;

  val qstall = new io_qstall().asInput;

  val fire = new io_vxu_issue_fu().asInput;
  val fire_fn = new io_vxu_issue_fn().asInput;
  val fire_regid_imm = new io_vxu_issue_regid_imm().asInput;

  val seq = new io_vxu_seq_fu().asOutput;
  val seq_fn = new io_vxu_seq_fn().asOutput;
  val seq_regid_imm = new io_vxu_seq_regid_imm().asOutput;
}

class io_vxu_expand extends Bundle
{
  val seq_to_expand = new io_vxu_seq_to_expand().asInput;

  val seq = new io_vxu_seq_fu().asInput;
  val seq_fn = new io_vxu_seq_fn().asInput;
  val seq_regid_imm = new io_vxu_seq_regid_imm().asInput;

  val expand_read = new io_vxu_expand_read().asOutput;
  val expand_write = new io_vxu_expand_write().asOutput;
  val expand_fu_fn = new io_vxu_expand_fu_fn().asOutput;
}

class io_vxu extends Bundle
{
  val illegal = Bool('output);

  val vxu_cmdq = (new io_ready_valid){Bits(width = DEF_VXU_CMDQ)}.flip();
  val vxu_immq = (new io_ready_valid){Bits(width = DEF_VXU_IMMQ)}.flip();
  val vxu_ackq = (new io_ready_valid){Bits(width = DEF_VXU_ACKQ)};

  val vmu_utcmdq = (new io_ready_valid){Bits(width = DEF_VMU_UTCMDQ)};
  val vmu_utimmq = (new io_ready_valid){Bits(width = DEF_VMU_UTIMMQ)};
  val vmu_utackq = (new io_ready_valid){Bits(width = DEF_VMU_UTACKQ)};

  val cp_imul = (new io_ready_valid){new io_imul()}.flip();
  val cp_fma = (new io_ready_valid){new io_fma()}.flip();

  val imem_req = (new io_ready_valid){Bits(width = DEF_ADDR)};
  val imem_resp = (new io_valid){Bits(width = DEF_INST)}.flip();

  val lane_vldq = (new io_ready_valid){Bits(width = DEF_DATA)}.flip();
  val lane_vsdq = (new io_ready_valid){Bits(width = DEF_DATA)};
  val lane_utaq = (new io_ready_valid){Bits(width = DEF_ADDR)};
  val lane_utldq = (new io_ready_valid){Bits(width = DEF_DATA)}.flip();
  val lane_utsdq = (new io_ready_valid){Bits(width = DEF_DATA)};
}

}

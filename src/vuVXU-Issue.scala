package hwacha

import Chisel._
import Node._
import Config._
import Commands._
import Interface._

class vuVXU_Issue extends Component
{
  val io = new io_vxu_issue();

  val tvec = new vuVXU_Issue_TVEC();
  val vt = new vuVXU_Issue_VT();
  val utcmdq_arb = (new Arbiter(2)){ new io_vmu_utcmdq() };

  tvec.io.vf <> vt.io.vf;
  utcmdq_arb.io.in(0) <> tvec.io.vmu_utcmdq;
  utcmdq_arb.io.in(1) <> vt.io.vmu_utcmdq;

  vt.io.illegal ^^ io.illegal;
  vt.io.imem_req ^^ io.imem_req;
  vt.io.imem_resp ^^ io.imem_resp;

  tvec.io.vxu_cmdq ^^ io.vxu_cmdq;
  tvec.io.vxu_immq ^^ io.vxu_immq;
  tvec.io.vxu_imm2q ^^ io.vxu_imm2q;
  utcmdq_arb.io.out ^^ io.vmu_utcmdq;
  vt.io.vmu_utimmq ^^ io.vmu_utimmq;

  tvec.io.issue_to_hazard ^^ io.issue_to_hazard;
  tvec.io.issue_to_seq ^^ io.issue_to_seq;
  tvec.io.issue_to_lane ^^ io.issue_to_lane;

  tvec.io.valid ^^ io.tvec_valid;
  tvec.io.ready ^^ io.tvec_ready;
  tvec.io.dhazard ^^ io.tvec_dhazard;
  tvec.io.shazard ^^ io.tvec_shazard;
  tvec.io.bhazard ^^ io.tvec_bhazard;
  tvec.io.fn ^^ io.tvec_fn;
  tvec.io.decoded ^^ io.tvec_regid_imm;

  vt.io.valid ^^ io.vt_valid;
  vt.io.ready ^^ io.vt_ready;
  vt.io.dhazard ^^ io.vt_dhazard;
  vt.io.shazard ^^ io.vt_shazard;
  vt.io.bhazard ^^ io.vt_bhazard;
  vt.io.fn ^^ io.vt_fn;
  vt.io.decoded ^^ io.vt_regid_imm;
}

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

  tvec.io.vf <> vt.io.vf;

  vt.io.illegal ^^ io.illegal;
  vt.io.imem_req ^^ io.imem_req;
  vt.io.imem_resp ^^ io.imem_resp;

  tvec.io.vxu_cmdq ^^ io.vxu_cmdq;
  tvec.io.vxu_immq ^^ io.vxu_immq;
  tvec.io.vxu_imm2q ^^ io.vxu_imm2q;
  tvec.io.vmu_utcmdq ^^ io.vmu_utcmdq;
  vt.io.vmu_utimmq ^^ io.vmu_utimmq;

  tvec.io.no_pending_ldsd <> io.no_pending_ldsd

  tvec.io.issue_to_hazard ^^ io.issue_to_hazard;
  tvec.io.issue_to_seq ^^ io.issue_to_seq;
  tvec.io.issue_to_lane ^^ io.issue_to_lane;

  tvec.io.vec_ackq <> io.vec_ackq
  tvec.io.vxu_ackq <> io.vxu_ackq
  tvec.io.vmu_vcmdq <> io.vmu_vcmdq
  tvec.io.vmu_vackq <> io.vmu_vackq

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

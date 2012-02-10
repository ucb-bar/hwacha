package hwacha
{

import Chisel._
import Node._
import Config._
import Interface._

class vu extends Component
{
  val io = new io_vu();

  val vcu = new vuVCU();
  vcu.io.vec_cmdq <> io.vec_cmdq;
  vcu.io.vec_ximm1q <> io.vec_ximm1q;
  vcu.io.vec_ximm2q <> io.vec_ximm2q;

  val vxu_cmdq = VC_SIMPLE_QUEUE(XCMD_SZ, 8);
  vxu_cmdq.io.enq.bits <> vcu.io.vxu_cmdq.bits;
  vxu_cmdq.io.enq.valid <> vcu.io.vxu_cmdq.valid;
  vxu_cmdq.io.enq.ready <> vcu.io.vxu_cmdq.ready;

  val vxu_immq = VC_SIMPLE_QUEUE(XIMM_SZ, 4);
  vxu_immq.io.enq.bits <> vcu.io.vxu_immq.bits;
  vxu_immq.io.enq.valid <> vcu.io.vxu_immq.valid;
  vxu_immq.io.enq.ready <> vcu.io.vxu_immq.ready;

  // new
  val vxu_imm2q = VC_SIMPLE_QUEUE(XIMM2_SZ, 4);
  vxu_imm2q.io.enq.bits <> vcu.io.vxu_imm2q.bits;
  vxu_imm2q.io.enq.valid <> vcu.io.vxu_imm2q.valid;
  // new

  val vmu_vcmdq = VC_SIMPLE_QUEUE(VMCMD_SZ, 4);
  //vmu_vcmdq.io.enq.bits <> vcu.io.vmu_vcmdq.bits;
  //vmu_vcmdq.io.enq.valid <> vcu.io.vmu_vcmdq.valid;
  vmu_vcmdq.io.enq.ready <> vcu.io.vmu_vcmdq.ready;

  val vmu_vbaseq = VC_SIMPLE_QUEUE(VMIMM_SZ, 4);
  //vmu_vbaseq.io.enq.bits <> vcu.io.vmu_vbaseq.bits;
  //vmu_vbaseq.io.enq.valid <> vcu.io.vmu_vbaseq.valid;
  vmu_vbaseq.io.enq.ready <> vcu.io.vmu_vbaseq.ready;

  val vmu_vstrideq = VC_SIMPLE_QUEUE(VMSTRIDE_SZ, 2);
  //vmu_vstrideq.io.enq.bits <> vcu.io.vmu_vstrideq.bits;
  //vmu_vstrideq.io.enq.valid <> vcu.io.vmu_vstrideq.valid;
  vmu_vstrideq.io.enq.ready <> vcu.io.vmu_vstrideq.ready;

  vcu.io.vec_ackq <> io.vec_ackq;

  val vxu = new vuVXU();
  vxu.io.illegal <> io.illegal;
  
  vxu.io.vxu_cmdq.bits <> vxu_cmdq.io.deq.bits;
  vxu.io.vxu_cmdq.ready <> vxu_cmdq.io.deq.ready;
  vxu.io.vxu_cmdq.valid <> vxu_cmdq.io.deq.valid;
  
  vxu.io.vxu_immq.bits <> vxu_immq.io.deq.bits;
  vxu.io.vxu_immq.ready <> vxu_immq.io.deq.ready;
  vxu.io.vxu_immq.valid <> vxu_immq.io.deq.valid;

  // new
  vxu.io.vxu_imm2q.bits <> vxu_imm2q.io.deq.bits;
  vxu.io.vxu_imm2q.valid <> vxu_imm2q.io.deq.valid;
  vxu.io.vxu_immq.ready <> vxu_imm2q.io.deq.ready;

  vxu.io.vmu_vcmdq.bits <> vmu_vcmdq.io.enq.bits;
  vxu.io.vmu_vcmdq.valid <> vmu_vcmdq.io.enq.valid;
  vxu.io.vmu_vcmdq.ready <> vmu_vcmdq.io.enq.ready;
  
  vxu.io.vmu_vbaseq.bits <> vmu_vbaseq.io.enq.bits;
  vxu.io.vmu_vbaseq.valid <> vmu_vbaseq.io.enq.valid;
  vxu.io.vmu_vbaseq.ready <> vmu_vbaseq.io.enq.ready;

  vxu.io.vmu_vstrideq.bits <> vmu_vstrideq.io.enq.bits;
  vxu.io.vmu_vstrideq.valid <> vmu_vstrideq.io.enq.valid;
  // new

  vxu.io.vxu_ackq <> vcu.io.vxu_ackq;

  val vmu_utcmdq = VC_SIMPLE_QUEUE(UTMCMD_SZ, 4);
  vmu_utcmdq.io.enq.bits <> vxu.io.vmu_utcmdq.bits;
  vmu_utcmdq.io.enq.valid <> vxu.io.vmu_utcmdq.valid;
  vmu_utcmdq.io.enq.ready <> vxu.io.vmu_utcmdq.ready;  

  val vmu_utimmq = VC_SIMPLE_QUEUE(UTMIMM_SZ, 4);
  vmu_utimmq.io.enq.bits <> vxu.io.vmu_utimmq.bits;
  vmu_utimmq.io.enq.valid <> vxu.io.vmu_utimmq.valid;
  vmu_utimmq.io.enq.ready <> vxu.io.vmu_utimmq.ready;  

  vxu.io.cp_imul_req <> io.cp_imul_req;
  vxu.io.cp_imul_resp <> io.cp_imul_resp;

  vxu.io.cp_fma_req <> io.cp_fma_req;
  vxu.io.cp_fma_resp <> io.cp_fma_resp;

  vxu.io.imem_req <> io.imem_req;
  vxu.io.imem_resp <> io.imem_resp;

  val vmu = new vuVMU();
  vmu.io.vmu_vcmdq.bits <> vmu_vcmdq.io.deq.bits;
  vmu.io.vmu_vcmdq.valid <> vmu_vcmdq.io.deq.valid;
  vmu.io.vmu_vcmdq.rdy <> vmu_vcmdq.io.deq.ready;

  vmu.io.vmu_vbaseq.bits <> vmu_vbaseq.io.deq.bits;
  vmu.io.vmu_vbaseq.valid <> vmu_vbaseq.io.deq.valid;
  vmu.io.vmu_vbaseq.rdy <> vmu_vbaseq.io.deq.ready;
  
  vmu.io.vmu_vstrideq.bits <> vmu_vstrideq.io.deq.bits;
  vmu.io.vmu_vstrideq.valid <> vmu_vstrideq.io.deq.valid;
  vmu.io.vmu_vstrideq.rdy <> vmu_vstrideq.io.deq.ready;

  vmu.io.vmu_vackq.bits <> vcu.io.vmu_vackq.bits;
  vmu.io.vmu_vackq.valid <> vcu.io.vmu_vackq.valid;
  vmu.io.vmu_vackq.rdy <> vcu.io.vmu_vackq.ready;
  
  vmu.io.vmu_utcmdq.bits <> vmu_utcmdq.io.deq.bits;
  vmu.io.vmu_utcmdq.valid <> vmu_utcmdq.io.deq.valid;
  vmu.io.vmu_utcmdq.rdy <> vmu_utcmdq.io.deq.ready;

  vmu.io.vmu_utimmq.bits <> vmu_utimmq.io.deq.bits;
  vmu.io.vmu_utimmq.valid <> vmu_utimmq.io.deq.valid;
  vmu.io.vmu_utimmq.rdy <> vmu_utimmq.io.deq.ready;

  vmu.io.vmu_utackq.bits <> vxu.io.vmu_utackq.bits;
  vmu.io.vmu_utackq.valid <> vxu.io.vmu_utackq.valid;
  vmu.io.vmu_utackq.rdy <> vxu.io.vmu_utackq.ready;

  vmu.io.lane_vldq_deq_bits <> vxu.io.lane_vldq.bits;
  vmu.io.lane_vldq_deq_val <> vxu.io.lane_vldq.valid;
  vmu.io.lane_vldq_deq_rdy <> vxu.io.lane_vldq.ready;

  vmu.io.lane_vsdq_enq_bits <> vxu.io.lane_vsdq.bits;
  vmu.io.lane_vsdq_enq_val <> vxu.io.lane_vsdq.valid;
  vmu.io.lane_vsdq_enq_rdy <> vxu.io.lane_vsdq.ready;

  vmu.io.lane_utaq_enq_bits <> vxu.io.lane_utaq.bits;
  vmu.io.lane_utaq_enq_val <> vxu.io.lane_utaq.valid;
  vmu.io.lane_utaq_enq_rdy <> vxu.io.lane_utaq.ready;

  vmu.io.lane_utldq_deq_bits <> vxu.io.lane_utldq.bits;
  vmu.io.lane_utldq_deq_val <> vxu.io.lane_utldq.valid;
  vmu.io.lane_utldq_deq_rdy <> vxu.io.lane_utldq.ready;

  vmu.io.lane_utsdq_enq_bits <> vxu.io.lane_utsdq.bits;
  vmu.io.lane_utsdq_enq_val <> vxu.io.lane_utsdq.valid;
  vmu.io.lane_utsdq_enq_rdy <> vxu.io.lane_utsdq.ready;

  vmu.io.dmem_req_vec.addr <> io.dmem_req_vec.bits.addr;
  vmu.io.dmem_req_vec.op <> io.dmem_req_vec.bits.op;
  vmu.io.dmem_req_vec.data <> io.dmem_req_vec.bits.data;
  vmu.io.dmem_req_vec.wmask <> io.dmem_req_vec.bits.wmask;
  vmu.io.dmem_req_vec.tag <> io.dmem_req_vec.bits.tag;
  vmu.io.dmem_req_vec.valid <> io.dmem_req_vec.valid;
  vmu.io.dmem_req_vec.rdy <> io.dmem_req_vec.ready;
  vmu.io.dmem_resp_vec.valid <> io.dmem_resp_vec.valid;

  vmu.io.dmem_req_ut.addr <> io.dmem_req_ut.bits.addr;
  vmu.io.dmem_req_ut.op <> io.dmem_req_ut.bits.op
  vmu.io.dmem_req_ut.data <> io.dmem_req_ut.bits.data;
  vmu.io.dmem_req_ut.wmask <> io.dmem_req_ut.bits.wmask;
  vmu.io.dmem_req_ut.tag <> io.dmem_req_ut.bits.tag;
  vmu.io.dmem_req_ut.valid <> io.dmem_req_ut.valid;
  vmu.io.dmem_req_ut.rdy <> io.dmem_req_ut.ready;
  vmu.io.dmem_resp_ut.valid <> io.dmem_resp_ut.valid;

  vmu.io.dmem_resp_ut.tag <> io.dmem_resp_ut.bits.tag;
  vmu.io.dmem_resp_ut.data <> io.dmem_resp_ut.bits.data;
  vmu.io.dmem_resp_vec.tag <> io.dmem_resp_vec.bits.tag;
  vmu.io.dmem_resp_vec.data <> io.dmem_resp_vec.bits.data;

}
}

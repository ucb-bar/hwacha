package hwacha

import Chisel._
import Node._
import Config._
import Interface._
import queues._
import hardfloat._

class vuMemIF extends Component
{
  val io = new io_vu_memif()

  val id_pf_cmd = (io.vaq_deq.bits.cmd === M_PFW || io.vaq_deq.bits.cmd === M_PFR)
  val id_load_cmd = (io.vaq_deq.bits.cmd === M_XRD)
  val id_store_cmd = (io.vaq_deq.bits.cmd === M_XWR)
  val id_amo_cmd = (M_XA_ADD <= io.vaq_deq.bits.cmd) && (io.vaq_deq.bits.cmd <= M_XA_MAXU) // assuing amos are contiguous

  val id_pf_val = id_pf_cmd && io.vaq_deq.valid
  val id_load_val = id_load_cmd && io.vaq_deq.valid && io.vldq_deq_rtag.valid
  val id_store_val = id_store_cmd && io.vaq_deq.valid && io.vsdq_deq.valid
  val id_amo_val = id_amo_cmd && io.vaq_deq.valid && io.vsdq_deq.valid && io.vldq_deq_rtag.valid

  val id_vaq_val = id_pf_val || id_load_val || id_store_val || id_amo_val
  val id_vsdq_val = id_store_val || id_amo_val
  val id_vldq_val = id_load_val || id_amo_val

  val ex_vaq_val = Reg(id_vaq_val, resetVal = Bool(false))
  val ex_vsdq_val = Reg(id_vsdq_val, resetVal = Bool(false))
  val ex_vldq_val = Reg(id_vldq_val, resetVal = Bool(false))

  val mem_vaq_val = Reg(ex_vaq_val, resetVal = Bool(false))
  val mem_vsdq_val = Reg(ex_vsdq_val, resetVal = Bool(false))
  val mem_vldq_val = Reg(ex_vldq_val, resetVal = Bool(false))

  val nack_ex_reg = Reg(io.mem_resp.bits.nack, resetVal = Bool(false))
  val nack_id_reg = Reg(nack_ex_reg, resetVal = Bool(false))

  // when the request is nacked, the processor implicitly kills the request in decode and execute
  val ack_common = !io.mem_resp.bits.nack && !nack_ex_reg && !nack_id_reg

  io.vaq_deq.ready :=
    id_pf_cmd ||
    id_load_cmd && io.vldq_deq_rtag.valid ||
    id_store_cmd && io.vsdq_deq.valid ||
    id_amo_cmd && io.vsdq_deq.valid && io.vldq_deq_rtag.valid
  io.vaq_ack := mem_vaq_val && ack_common
  io.vaq_nack := (id_vaq_val || ex_vaq_val || mem_vaq_val) && io.mem_resp.bits.nack

  io.vsdq_deq.ready :=
    id_store_cmd && io.vaq_deq.valid ||
    id_amo_cmd && io.vaq_deq.valid && io.vldq_deq_rtag.valid
  io.vsdq_ack := mem_vsdq_val && ack_common
  io.vsdq_nack := (id_vsdq_val || ex_vsdq_val || mem_vsdq_val) && io.mem_resp.bits.nack

  io.vldq_deq_rtag.ready :=
    id_load_cmd && io.vaq_deq.valid ||
    id_amo_cmd && io.vaq_deq.valid && io.vsdq_deq.valid
  io.vldq_ack := mem_vldq_val && ack_common
  io.vldq_nack := (id_vldq_val || ex_vldq_val || mem_vldq_val) && io.mem_resp.bits.nack

  io.mem_req.valid := id_vaq_val
  io.mem_req.bits.cmd := io.vaq_deq.bits.cmd
  io.mem_req.bits.typ := io.vaq_deq.bits.typ
  io.mem_req.bits.idx := io.vaq_deq.bits.idx
  io.mem_req.bits.ppn := io.vaq_deq.bits.ppn
  io.mem_req.bits.data := io.vsdq_deq.bits
  io.mem_req.bits.tag := Cat(io.vldq_deq_rtag.bits, io.vaq_deq.bits.typ_float)
  
  // load data conversion
  val reg_mem_resp = Reg(io.mem_resp)
    
  val ldq_sp_bits = Wire(){Bits(width=33)}
  val ldq_dp_bits = Wire(){Bits(width=65)}
  
  val load_fp = reg_mem_resp.bits.tag(0)
  val load_fp_d = load_fp && reg_mem_resp.bits.typ === MT_D 
  val load_fp_w = load_fp && reg_mem_resp.bits.typ === MT_W

  val recode_sp = new float32ToRecodedFloat32()
  recode_sp.io.in := reg_mem_resp.bits.data(31,0)
  ldq_sp_bits := recode_sp.io.out
  
  val recode_dp = new float64ToRecodedFloat64()
  recode_dp.io.in := reg_mem_resp.bits.data
  ldq_dp_bits := recode_dp.io.out

  io.vldq_enq.valid := reg_mem_resp.valid
  io.vldq_enq.bits.data := MuxCase(
    Cat(Bits(0,1),reg_mem_resp.bits.data(63,0)), Array(	
	    (load_fp_d) -> ldq_dp_bits,
      (load_fp_w) -> Cat(Bits("hFFFFFFFF",32), ldq_sp_bits)
  ))
  io.vldq_enq.bits.rtag := reg_mem_resp.bits.tag.toUFix >> UFix(1)
}

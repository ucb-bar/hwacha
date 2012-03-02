package hwacha

import Chisel._
import Node._
import Constants._
import queues._
import hardfloat._

class io_vu_memif extends Bundle
{
  val vaq_deq = new ioDecoupled()({ new io_vpaq_bundle() }).flip
  val vaq_ack = Bool(OUTPUT)
  val vaq_nack = Bool(OUTPUT)

  val vsdq_deq = new ioDecoupled()({ Bits(width = 65) }).flip
  val vsdq_ack = Bool(OUTPUT)
  val vsdq_nack = Bool(OUTPUT)

  val vldq_deq_rtag = new ioDecoupled()({ Bits(width = 8) }).flip
  val vldq_ack = Bool(OUTPUT)
  val vldq_nack = Bool(OUTPUT)
  val vldq_enq = new ioDecoupled()({ new io_queue_reorder_qcnt_enq_bundle(65, 8) })

  val mem_req = new io_dmem_req()
  val mem_resp = new io_dmem_resp().flip
}

class vuMemIF extends Component
{
  val io = new io_vu_memif()

  val ex_pf_cmd = (io.vaq_deq.bits.cmd === M_PFW || io.vaq_deq.bits.cmd === M_PFR)
  val ex_load_cmd = (io.vaq_deq.bits.cmd === M_XRD)
  val ex_store_cmd = (io.vaq_deq.bits.cmd === M_XWR)
  val ex_amo_cmd = (M_XA_ADD <= io.vaq_deq.bits.cmd) && (io.vaq_deq.bits.cmd <= M_XA_MAXU) // assuing amos are contiguous

  val ex_pf_val = ex_pf_cmd && io.vaq_deq.valid
  val ex_load_val = ex_load_cmd && io.vaq_deq.valid && io.vldq_deq_rtag.valid
  val ex_store_val = ex_store_cmd && io.vaq_deq.valid && io.vsdq_deq.valid
  val ex_amo_val = ex_amo_cmd && io.vaq_deq.valid && io.vsdq_deq.valid && io.vldq_deq_rtag.valid

  val ex_vaq_val = ex_pf_val || ex_load_val || ex_store_val || ex_amo_val
  val ex_vsdq_val = ex_store_val || ex_amo_val
  val ex_vldq_val = ex_load_val || ex_amo_val

  val mem_vaq_val = Reg(ex_vaq_val, resetVal = Bool(false))
  val mem_vsdq_val = Reg(ex_vsdq_val, resetVal = Bool(false))
  val mem_vldq_val = Reg(ex_vldq_val, resetVal = Bool(false))

  val nack_ex_reg = Reg(io.mem_resp.bits.nack, resetVal = Bool(false))

  // when the request is nacked, the processor implicitly kills the request in decode and execute
  val ack_common = !io.mem_resp.bits.nack && !nack_ex_reg

  io.vaq_deq.ready :=
    ex_pf_cmd ||
    ex_load_cmd && io.vldq_deq_rtag.valid ||
    ex_store_cmd && io.vsdq_deq.valid ||
    ex_amo_cmd && io.vsdq_deq.valid && io.vldq_deq_rtag.valid
  io.vaq_ack := mem_vaq_val && ack_common
  io.vaq_nack := (ex_vaq_val || mem_vaq_val) && io.mem_resp.bits.nack

  io.vsdq_deq.ready :=
    ex_store_cmd && io.vaq_deq.valid ||
    ex_amo_cmd && io.vaq_deq.valid && io.vldq_deq_rtag.valid
  io.vsdq_ack := mem_vsdq_val && ack_common
  io.vsdq_nack := (ex_vsdq_val || mem_vsdq_val) && io.mem_resp.bits.nack

  io.vldq_deq_rtag.ready :=
    ex_load_cmd && io.vaq_deq.valid ||
    ex_amo_cmd && io.vaq_deq.valid && io.vsdq_deq.valid
  io.vldq_ack := mem_vldq_val && ack_common
  io.vldq_nack := (ex_vldq_val || mem_vldq_val) && io.mem_resp.bits.nack

  io.mem_req.valid := ex_vaq_val
  io.mem_req.bits.kill := io.mem_resp.bits.nack // get's delayed one cycle in cpu
  io.mem_req.bits.cmd := io.vaq_deq.bits.cmd
  io.mem_req.bits.typ := io.vaq_deq.bits.typ
  io.mem_req.bits.idx := io.vaq_deq.bits.idx
  io.mem_req.bits.ppn := io.vaq_deq.bits.ppn // get's delayed one cycle in cpu
  io.mem_req.bits.data := io.vsdq_deq.bits // get's delayed one cycle in cpu
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

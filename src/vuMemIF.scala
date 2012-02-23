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

  val id_load_cmd = (io.vaq_deq.bits.cmd === M_XRD)
  val id_store_cmd = (io.vaq_deq.bits.cmd === M_XWR)
  val id_load_val = id_load_cmd && io.vaq_deq.valid && io.vldq_deq_rtag.valid
  val id_store_val = id_store_cmd && io.vaq_deq.valid && io.vsdq_deq.valid

  val ex_load_val = Reg(id_load_val, resetVal = Bool(false))
  val ex_store_val = Reg(id_store_val, resetVal = Bool(false))

  val mem_load_val = Reg(ex_load_val, resetVal = Bool(false))
  val mem_store_val = Reg(ex_store_val, resetVal = Bool(false))

  val nack_reg = Reg(io.mem_resp.bits.nack, resetVal = Bool(false))

  io.vaq_deq.ready := id_load_cmd && io.vldq_deq_rtag.valid || id_store_cmd && io.vsdq_deq.valid
  io.vaq_ack := (mem_load_val || mem_store_val) && !nack_reg && !io.mem_resp.bits.nack
  io.vaq_nack := (mem_load_val || mem_store_val) && io.mem_resp.bits.nack

  io.vsdq_deq.ready := id_store_cmd && io.vaq_deq.valid
  io.vsdq_ack := mem_store_val && !nack_reg && !io.mem_resp.bits.nack
  io.vsdq_nack := mem_store_val && io.mem_resp.bits.nack

  io.vldq_deq_rtag.ready := id_load_cmd && io.vaq_deq.valid
  io.vldq_ack := mem_load_val && !nack_reg && !io.mem_resp.bits.nack
  io.vldq_nack := mem_load_val && io.mem_resp.bits.nack

  io.mem_req.valid := id_load_val || id_store_val
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

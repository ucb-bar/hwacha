package hwacha

import Chisel._
import Node._
import Constants._
import hardfloat._

class io_vmu_memif extends Bundle
{
  val vaq = new FIFOIO()(new io_vpaq_bundle()).flip
  val vsdq = new FIFOIO()(Bits(width = 65)).flip
  val vldq = new PipeIO()(new io_queue_reorder_qcnt_enq_bundle(65, LG_ENTRIES_VLDQ))
  val vldq_rtag = new FIFOIO()(Bits(width = LG_ENTRIES_VLDQ)).flip

  val mem_req = new io_dmem_req()
  val mem_resp = new io_dmem_resp().flip

  val pending_skidbuf = Bool(OUTPUT)

  val flush = Bool(INPUT)
}

class vuVMU_MemIF extends Component
{
  val io = new io_vmu_memif()

  val ex_pf_cmd = is_mcmd_pf(io.vaq.bits.cmd)
  val ex_load_cmd = is_mcmd_load(io.vaq.bits.cmd)
  val ex_store_cmd = is_mcmd_store(io.vaq.bits.cmd)
  val ex_amo_cmd = is_mcmd_amo(io.vaq.bits.cmd)

  val ex_pf_val = ex_pf_cmd && io.vaq.valid
  val ex_load_val = ex_load_cmd && io.vaq.valid && io.vldq_rtag.valid
  val ex_store_val = ex_store_cmd && io.vaq.valid && io.vsdq.valid
  val ex_amo_val = ex_amo_cmd && io.vaq.valid && io.vsdq.valid && io.vldq_rtag.valid

  val flush = reset || io.flush
  val sb = new skidbuf(late_nack = LATE_DMEM_NACK, resetSignal = flush)(new io_dmem_req_bundle())

  io.vaq.ready :=
    sb.io.enq.ready && ( 
      ex_pf_cmd ||
      ex_load_cmd && io.vldq_rtag.valid ||
      ex_store_cmd && io.vsdq.valid ||
      ex_amo_cmd && io.vsdq.valid && io.vldq_rtag.valid
    )

  io.vsdq.ready :=
    sb.io.enq.ready && (
      ex_store_cmd && io.vaq.valid ||
      ex_amo_cmd && io.vaq.valid && io.vldq_rtag.valid
    )

  io.vldq_rtag.ready :=
    sb.io.enq.ready && (
      ex_load_cmd && io.vaq.valid ||
      ex_amo_cmd && io.vaq.valid && io.vsdq.valid
    )

  sb.io.enq.valid := ex_pf_val || ex_load_val || ex_store_val || ex_amo_val
  sb.io.enq.bits.cmd := io.vaq.bits.cmd
  sb.io.enq.bits.typ := io.vaq.bits.typ
  sb.io.enq.bits.addr := io.vaq.bits.addr.toUFix
  sb.io.enq.bits.data := io.vsdq.bits // delayed one cycle in cpu
  sb.io.enq.bits.tag := Cat(io.vldq_rtag.bits, io.vaq.bits.typ_float) // delayed one cycle in cpu

  io.mem_req.valid := sb.io.deq.valid
  io.mem_req.bits.kill := sb.io.kill
  io.mem_req.bits.cmd := sb.io.deq.bits.cmd
  io.mem_req.bits.typ := sb.io.deq.bits.typ
  io.mem_req.bits.addr := sb.io.deq.bits.addr
  io.mem_req.bits.phys := Bool(true)
  io.mem_req.bits.data := sb.io.deq.bits.data
  io.mem_req.bits.tag := sb.io.deq.bits.tag

  sb.io.deq.ready := io.mem_req.ready
  sb.io.nack := io.mem_resp.bits.nack

  io.pending_skidbuf := !sb.io.empty

  // load data conversion
  val reg_mem_resp = Reg(io.mem_resp)
    
  val ldq_sp_bits = Bits(width=33)
  val ldq_dp_bits = Bits(width=65)
  
  val load_fp = reg_mem_resp.bits.tag(0)
  val load_fp_d = load_fp && reg_mem_resp.bits.typ === mtyp_D 
  val load_fp_w = load_fp && reg_mem_resp.bits.typ === mtyp_W

  val recode_sp = new float32ToRecodedFloat32()
  recode_sp.io.in := reg_mem_resp.bits.data(31,0)
  ldq_sp_bits := recode_sp.io.out
  
  val recode_dp = new float64ToRecodedFloat64()
  recode_dp.io.in := reg_mem_resp.bits.data
  ldq_dp_bits := recode_dp.io.out

  io.vldq.valid := reg_mem_resp.valid
  io.vldq.bits.data := MuxCase(
    Cat(Bits(0,1),reg_mem_resp.bits.data(63,0)), Array(	
	    (load_fp_d) -> ldq_dp_bits,
      (load_fp_w) -> Cat(Bits("hFFFFFFFF",32), ldq_sp_bits)
  ))
  io.vldq.bits.rtag := reg_mem_resp.bits.tag.toUFix >> UFix(1)
}

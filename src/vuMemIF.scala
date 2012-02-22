package hwacha

import Chisel._
import Node._
import Config._
import queues._
import hardfloat._

class io_vu_memif extends Bundle
{
  val vaq_deq = new io_ready_valid()({ new io_vaq_bundle() }).flip()
  val vaq_ack = Bool(OUTPUT)
  val vaq_nack = Bool(OUTPUT)

  val vsdq_deq = new io_ready_valid()({ Bits(width = 65) }).flip()
  val vsdq_ack = Bool(OUTPUT)
  val vsdq_nack = Bool(OUTPUT)

  val vldq_deq_rtag = new io_ready_valid()({ Bits(width = 8) }).flip()
  val vldq_ack = Bool(OUTPUT)
  val vldq_nack = Bool(OUTPUT)
  val vldq_enq = new io_ready_valid()({ new io_queue_reorder_qcnt_enq_bundle(65, 8) })

  val mem_req = new io_dmem_req()
  val mem_resp = new io_dmem_resp().flip()
}

class vuMemIF extends Component
{
  val io = new io_vu_memif()

  val store_skidbuf = new queuePipe1PF(64+4+3+PGIDX_BITS+PPN_BITS)
  val load_skidbuf = new queuePipe1PF(10+64) // top bit unused

  val id_load_cmd = (io.vaq_deq.bits.cmd === M_XRD)
  val id_store_cmd = (io.vaq_deq.bits.cmd === M_XWR)
  val id_load_val = id_load_cmd && io.vaq_deq.valid && io.vldq_deq_rtag.valid
  val id_store_val = id_store_cmd && io.vaq_deq.valid && store_skidbuf.io.enq.valid

  val ex_load_val = Reg(id_load_val, resetVal = Bool(false))
  val ex_store_val = Reg(id_store_val, resetVal = Bool(false))

  val mem_load_val = Reg(ex_load_val, resetVal = Bool(false))
  val mem_store_val = Reg(ex_store_val, resetVal = Bool(false))

  val nack_reg = Reg(io.mem_resp.bits.nack, resetVal = Bool(false))
  val nack_reg2 = Reg(nack_reg, resetVal = Bool(false)) // 3 cycles for nack to come back

  io.vaq_deq.ready := id_load_cmd && io.vldq_deq_rtag.valid || id_store_cmd && io.vsdq_deq.valid
  io.vaq_ack := (mem_load_val || mem_store_val) && !nack_reg && !nack_reg2 && !io.mem_resp.bits.nack
  io.vaq_nack := (mem_load_val || mem_store_val) && io.mem_resp.bits.nack

  io.vsdq_deq.ready := id_store_cmd && io.vaq_deq.valid
  io.vsdq_ack := mem_store_val && !nack_reg && !nack_reg2 && !io.mem_resp.bits.nack
  io.vsdq_nack := mem_store_val && io.mem_resp.bits.nack

  io.vldq_deq_rtag.ready := id_load_cmd && io.vaq_deq.valid
  io.vldq_ack := mem_load_val && !nack_reg && !nack_reg2 && !io.mem_resp.bits.nack
  io.vldq_nack := mem_load_val && io.mem_resp.bits.nack

  // store data conversion
  val fp_cmd_req = io.vaq_deq.bits.typ_float

  val rf32f32  = new recodedFloat32ToFloat32()
  rf32f32.io.in := io.vsdq_deq.bits(32,0)
  val vsdq_deq_sp = rf32f32.io.out

  val rf64f64  = new recodedFloat64ToFloat64()
  rf64f64.io.in := io.vsdq_deq.bits
  val vsdq_deq_dp = rf64f64.io.out
 
  val store_data = MuxCase(
      io.vsdq_deq.bits(63,0), Array(	
	      (fp_cmd_req && io.vaq_deq.bits.typ === MT_D ) -> vsdq_deq_dp,
        (fp_cmd_req && io.vaq_deq.bits.typ === MT_W ) -> Fill(2, vsdq_deq_sp)
      ))

  store_skidbuf.io.enq.bits := Cat(store_data,
                             io.vaq_deq.bits.cmd,
                             io.vaq_deq.bits.typ,
                             io.vaq_deq.bits.idx,
                             io.vaq_deq.bits.ppn)
  store_skidbuf.io.enq.valid := io.vsdq_deq.valid
  io.vsdq_deq.ready := store_skidbuf.io.enq.ready

  io.mem_req.valid := id_load_val || id_store_val
  io.mem_req.bits.cmd := store_skidbuf.io.deq.bits(PPN_BITS+PGIDX_BITS+3+4-1,PPN_BITS+PGIDX_BITS+3)
  io.mem_req.bits.typ := store_skidbuf.io.deq.bits(PPN_BITS+PGIDX_BITS+3-1,PPN_BITS+PGIDX_BITS)
  io.mem_req.bits.idx := store_skidbuf.io.deq.bits(PPN_BITS+PGIDX_BITS-1,PPN_BITS)
  io.mem_req.bits.ppn := store_skidbuf.io.deq.bits(PPN_BITS-1,0)
  io.mem_req.bits.data := store_skidbuf.io.deq.bits(PPN_BITS+PGIDX_BITS+3+4+64-1,PPN_BITS+PGIDX_BITS+3+4)
  io.mem_req.bits.tag := Cat(fp_cmd_req, io.vldq_deq_rtag.bits)

  // load data conversion
    
  val ldq_sp_bits = Wire(){Bits(width=33)}
  val ldq_dp_bits = Wire(){Bits(width=65)}
  

  load_skidbuf.io.enq.valid := io.mem_resp.valid
  load_skidbuf.io.enq.bits := Cat(io.mem_resp.bits.tag, io.mem_resp.bits.data)

  val fp_cmd_resp = load_skidbuf.io.deq.bits(72)

  val recode_sp     = new float32ToRecodedFloat32()
  recode_sp.io.in   := load_skidbuf.io.deq.bits(31,0)
  ldq_sp_bits       := recode_sp.io.out
  
  val recode_dp     = new float64ToRecodedFloat64()
  recode_dp.io.in   := load_skidbuf.io.deq.bits
  ldq_dp_bits       := recode_dp.io.out


  val load_data = MuxCase(
    Cat(Bits(0,1),load_skidbuf.io.deq.bits(63,0)), Array(	
	    (fp_cmd_resp && io.vaq_deq.bits.typ === MT_D ) -> ldq_dp_bits,
      (fp_cmd_resp && io.vaq_deq.bits.typ === MT_W ) -> Cat(Bits("hFFFFFFFF",32), ldq_sp_bits)
  ))

  load_skidbuf.io.deq.ready := Bool(true)
  io.vldq_enq.valid := load_skidbuf.io.deq.valid
  io.vldq_enq.bits.data := load_data
  io.vldq_enq.bits.rtag := load_skidbuf.io.deq.bits(71,64).toUFix()

}

package hwacha

import Chisel._
import Node._
import Config._
import queues._

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
  io.mem_req.bits.tag := io.vldq_deq_rtag.bits

  io.vldq_enq.valid := io.mem_resp.valid
  io.vldq_enq.bits.data := io.mem_resp.bits.data
  io.vldq_enq.bits.rtag := io.mem_resp.bits.tag.toUFix()
}

package hwacha

import Chisel._
import Node._
import Constants._
import uncore.constants.MemoryOpConstants._

class MemIF(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val vaq = new VPAQIO().flip
    val vldq = new VLDQMemIfIO().flip
    val vsdq = new VSDQIO().flip

    val dmem = new rocket.HellaCacheIO()(conf.dcache)
  }


//-------------------------------------------------------------------------\\
// REQUEST                                                                 \\
//-------------------------------------------------------------------------\\

  val ex_pf_cmd = is_mcmd_pf(io.vaq.bits.cmd)
  val ex_load_cmd = is_mcmd_load(io.vaq.bits.cmd)
  val ex_store_cmd = is_mcmd_store(io.vaq.bits.cmd)
  val ex_amo_cmd = is_mcmd_amo(io.vaq.bits.cmd)

  val ex_pf_val = ex_pf_cmd && io.vaq.valid
  val ex_load_val = ex_load_cmd && io.vaq.valid && io.vldq.rtag.valid
  val ex_store_val = ex_store_cmd && io.vaq.valid && io.vsdq.valid
  val ex_amo_val = ex_amo_cmd && io.vaq.valid && io.vsdq.valid && io.vldq.rtag.valid

  io.dmem.req.valid := ex_pf_val || ex_load_val || ex_store_val || ex_amo_val
  io.dmem.req.bits.cmd := io.vaq.bits.cmd
  io.dmem.req.bits.typ := io.vaq.bits.typ
  io.dmem.req.bits.addr := io.vaq.bits.addr
  io.dmem.req.bits.data := io.vsdq.bits
  io.dmem.req.bits.tag := io.vldq.rtag.bits
  io.dmem.req.bits.phys := Bool(true)

  io.vaq.ready :=
    io.dmem.req.ready && (
      ex_pf_cmd ||
      ex_load_cmd && io.vldq.rtag.valid ||
      ex_store_cmd && io.vsdq.valid ||
      ex_amo_cmd && io.vsdq.valid && io.vldq.rtag.valid
    )

  io.vsdq.ready :=
    io.dmem.req.ready && (
      ex_store_cmd && io.vaq.valid ||
      ex_amo_cmd && io.vaq.valid && io.vldq.rtag.valid
    )

  io.vldq.rtag.ready :=
    io.dmem.req.ready && (
      ex_load_cmd && io.vaq.valid ||
      ex_amo_cmd && io.vaq.valid && io.vsdq.valid
    )


//-------------------------------------------------------------------------\\
// RESPONSE                                                                \\
//-------------------------------------------------------------------------\\

  io.vldq.update.valid := io.dmem.resp.valid && io.dmem.resp.bits.has_data
  io.vldq.update.bits.data := io.dmem.resp.bits.data_subword
  io.vldq.update.bits.rtag := io.dmem.resp.bits.tag
}

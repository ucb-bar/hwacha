package hwacha

import Chisel._
import Constants._

class MemIF extends HwachaModule
{
  val io = new Bundle {
    val vpaq = new VPAQMemIO().flip
    val vsdq = new VSDQIO().flip
    val vldq = new VLDQMemIO()
    val dmem = new rocket.HellaCacheIO
  }

  val req_cmd_pf = is_mcmd_pf(io.vpaq.bits.cmd)
  val req_cmd_load = is_mcmd_load(io.vpaq.bits.cmd)
  val req_cmd_store = is_mcmd_store(io.vpaq.bits.cmd)
  val req_cmd_amo = is_mcmd_amo(io.vpaq.bits.cmd)

  io.vpaq.ready :=
    io.dmem.req.ready && (
      (req_cmd_pf) ||
      (req_cmd_load && !io.vldq.stall) ||
      (req_cmd_store && io.vsdq.valid) ||
      (req_cmd_amo && io.vsdq.valid && !io.vldq.stall)
    )

  io.vsdq.ready :=
    io.dmem.req.ready && io.vpaq.valid && (
      (req_cmd_store) ||
      (req_cmd_amo && !io.vldq.stall)
    )

  io.dmem.req.valid :=
    io.vpaq.valid && ((!req_cmd_store && !req_cmd_amo) || io.vsdq.valid)

  io.dmem.req.bits.cmd := io.vpaq.bits.cmd
  io.dmem.req.bits.typ := io.vpaq.bits.typ
  io.dmem.req.bits.tag := io.vpaq.bits.tag
  io.dmem.req.bits.addr := io.vpaq.bits.addr
  io.dmem.req.bits.data := io.vsdq.bits
  io.dmem.req.bits.phys := Bool(true)

  val resp_cmd_load = is_mcmd_load(io.dmem.resp.bits.cmd)
  val resp_cmd_store = is_mcmd_store(io.dmem.resp.bits.cmd)
  val resp_cmd_amo = is_mcmd_amo(io.dmem.resp.bits.cmd)

  io.vldq.resp.bits.tag := io.dmem.resp.bits.tag
  io.vldq.resp.bits.data := io.dmem.resp.bits.data_subword
  io.vldq.resp.valid := io.dmem.resp.valid && io.dmem.resp.bits.has_data // (resp_cmd_load || resp_cmd_amo)
}

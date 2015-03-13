package hwacha

import Chisel._
import Constants._
import uncore._

class FinishEntry extends Bundle {
    val xid = Bits(width = params(TLManagerXactIdBits))
    val dst = UInt(width = log2Up(params(LNEndpoints)))
}

class MemIF extends HwachaModule
{
  val io = new Bundle {
    val vmu = new MemIfIO().flip
    val dmem = new HeaderlessUncachedTileLinkIO
  }

  private val tlBeatAddrBits = log2Up(params(TLDataBeats))
  private val tlByteAddrBits = log2Up(params(TLDataBits)) - 3
  private val tlBlockAddrOffset = tlBeatAddrBits + tlByteAddrBits

  val req_cmd_pf = is_mcmd_pf(io.vmu.vpaq.bits.cmd)
  val req_cmd_load = is_mcmd_load(io.vmu.vpaq.bits.cmd)
  val req_cmd_store = is_mcmd_store(io.vmu.vpaq.bits.cmd)
  val req_cmd_amo = is_mcmd_amo(io.vmu.vpaq.bits.cmd)
  val req_cmd_put = req_cmd_store || req_cmd_amo

  assert(!io.vmu.vpaq.valid ||
    req_cmd_pf || req_cmd_load || req_cmd_store || req_cmd_amo,
    "Unknown memory command")

  io.vmu.vpaq.ready := io.dmem.acquire.ready &&
    (req_cmd_pf || req_cmd_load || (req_cmd_put && io.vmu.vsdq.valid))
  io.vmu.vsdq.ready := io.dmem.acquire.ready && io.vmu.vpaq.valid && req_cmd_put

  val req_type = Mux1H(
    Vec(req_cmd_load, req_cmd_store, req_cmd_amo, req_cmd_pf),
    Vec(Acquire.getType, Acquire.putType, Acquire.putAtomicType, Acquire.prefetchType))
  val req_shift = io.vmu.vpaq.bits.addr(tlByteAddrBits-1,0)
  val req_data = io.vmu.vsdq.bits.toBits << Cat(req_shift, Bits(0, 3)).toUInt

  val req_mt_d = is_mtype_doubleword(io.vmu.vpaq.bits.typ)
  val req_mt_w = is_mtype_word(io.vmu.vpaq.bits.typ)
  val req_mt_h = is_mtype_halfword(io.vmu.vpaq.bits.typ)
  val req_mt_b = is_mtype_byte(io.vmu.vpaq.bits.typ)

  assert(!io.vmu.vpaq.valid ||
    req_mt_b || req_mt_h || req_mt_w || req_mt_d,
    "Unknown memory operand type")

  val req_mask = Mux1H(
    Vec(req_mt_b, req_mt_h, req_mt_w, req_mt_d),
    Vec(Bits(0x1), Bits(0x03), Bits(0x0f), Bits(0xff)))
  val req_union_mask = req_mask << req_shift
  val req_union_atomic = Cat(req_shift, io.vmu.vpaq.bits.typ, io.vmu.vpaq.bits.cmd)
  val req_union = Cat(Mux1H(
      Vec(req_cmd_pf || req_cmd_load, req_cmd_store, req_cmd_amo),
      Vec(M_XRD, req_union_mask, req_union_atomic)),
    Bool(true))

  io.dmem.acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = req_type,
    client_xact_id = io.vmu.vpaq.bits.tag,
    addr_block = io.vmu.vpaq.bits.addr(params(PAddrBits)-1, tlBlockAddrOffset),
    addr_beat = io.vmu.vpaq.bits.addr(tlBlockAddrOffset-1, tlByteAddrBits) &
      Fill(tlBeatAddrBits, !req_cmd_pf),
    data = req_data,
    union = req_union)

  io.dmem.acquire.valid := io.vmu.vpaq.valid &&
    ((!req_cmd_store && !req_cmd_amo) || io.vmu.vsdq.valid)

  io.vmu.vldq.bits.tag := io.dmem.grant.bits.payload.client_xact_id
  io.vmu.vldq.bits.data := io.dmem.grant.bits.payload.data
  io.vmu.vldq.valid := io.dmem.grant.fire() && io.dmem.grant.bits.payload.hasData()

  val finishq = Module(new Queue(new FinishEntry, 2))

  io.dmem.grant.ready := (!io.dmem.grant.bits.payload.hasData() || io.vmu.vldq.ready) &&
    (!io.dmem.grant.bits.payload.requiresAck() || finishq.io.enq.ready)

  finishq.io.enq.valid := io.dmem.grant.bits.payload.requiresAck() && io.dmem.grant.valid
  finishq.io.enq.bits.xid := io.dmem.grant.bits.payload.manager_xact_id
  finishq.io.enq.bits.dst := io.dmem.grant.bits.header.src

  io.dmem.finish.valid := finishq.io.deq.valid
  finishq.io.deq.ready := io.dmem.finish.ready
  io.dmem.finish.bits.payload.manager_xact_id := finishq.io.deq.bits.xid
  io.dmem.finish.bits.header.dst := finishq.io.deq.bits.dst
}

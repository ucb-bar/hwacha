package hwacha

import Chisel._
import cde.Parameters

class VMUMemReq(implicit p: Parameters) extends VMUMemOp
  with VMUTag with VMUData {
  val mask = UInt(width = tlDataBytes)
  val pred = Bool()
}

class VMUMemResp(implicit p: Parameters) extends VMULoadData()(p) {
  val store = Bool()
}

class VMUMemIO(implicit p: Parameters) extends VMUBundle()(p) {
  val req = Decoupled(new VMUMemReq)
  val resp = Decoupled(new VMUMemResp).flip
}

class MBox(implicit p: Parameters) extends VMUModule()(p) {
  val io = new Bundle {
    val inner = new Bundle {
      val abox = new VMUAddrIO().flip
      val sbox = new VMUStoreIO().flip
      val lbox = new VMULoadIO
    }
    val outer = new VMUMemIO

    val sret = new CounterUpdateIO(bSRet)
  }

  private val abox = io.inner.abox
  private val sbox = io.inner.sbox
  private val lbox = io.inner.lbox
  private val req = io.outer.req
  private val resp = io.outer.resp

  val mt = DecodedMemType(abox.bits.fn.mt)
  val cmd = DecodedMemCommand(abox.bits.fn.cmd)
  val read = cmd.read
  val write = cmd.write

  val pred = abox.bits.meta.mask.orR

  val lbox_en = pred && read
  val lbox_ready = !lbox_en || lbox.meta.ready
  val sbox_valid = !write || sbox.valid
  val req_ready = !pred || req.ready

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(abox.valid, sbox_valid, lbox_ready, req_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  abox.ready := fire(abox.valid)
  sbox.ready := fire(sbox_valid, write)
  lbox.meta.valid := fire(lbox_ready, lbox_en)
  req.valid := fire(req_ready, pred)

  /* Data mask */
  val mask_shift = abox.bits.meta.epad(tlByteAddrBits - 1)
  val mask = Mux(mask_shift,
    Cat(abox.bits.meta.mask, UInt(0, tlDataBytes >> 1)),
    abox.bits.meta.mask)

  /* Load metadata */
  lbox.meta.bits.vidx := abox.bits.meta.vidx
  lbox.meta.bits.eidx := abox.bits.meta.eidx
  lbox.meta.bits.epad := abox.bits.meta.epad
  lbox.meta.bits.mask := abox.bits.meta.mask

  /* Store metadata */
  sbox.meta.offset := abox.bits.addr(tlByteAddrBits-1, 0)
  sbox.meta.ecnt := abox.bits.meta.ecnt
  sbox.meta.last := abox.bits.meta.last
  sbox.meta.vsdq := abox.bits.meta.vsdq

  /* Request */
  req.bits.fn := abox.bits.fn
  req.bits.addr := abox.bits.addr
  req.bits.mask := PredicateByteMask(mask, mt)
  req.bits.data := sbox.bits.data
  req.bits.pred := pred
  req.bits.tag := Mux(read, lbox.meta.tag, abox.bits.meta.ecnt.raw)
  require(tlByteAddrBits-1 <= bVMUTag)

  /* Response */
  lbox.load.bits := resp.bits
  lbox.load.valid := resp.valid && !resp.bits.store
  resp.ready := resp.bits.store || lbox.load.ready

  /* Store acknowledgement */
  val sret_req = abox.bits.meta.ecnt
  val sret_resp = Wire(new CInt(tlByteAddrBits-1))
  sret_resp.raw := resp.bits.tag

  val sret_req_en = fire(null, cmd.store, !pred)
  val sret_resp_en = resp.fire() && resp.bits.store

  io.sret.update := Bool(true)
  io.sret.cnt :=
    Mux(sret_req_en, Cat(Bits(0,1), sret_req.decode()), UInt(0)) +
    Mux(sret_resp_en, Cat(Bits(0,1), sret_resp.decode()), UInt(0))
}

class VMUTileLink(implicit p: Parameters) extends VMUModule()(p) {
  import uncore.tilelink._

  val io = new Bundle {
    val vmu = new VMUMemIO().flip
    val dmem = new ClientUncachedTileLinkIO
  }

  private val tlBlockAddrOffset = tlBeatAddrBits + tlByteAddrBits

  private val req = io.vmu.req
  private val resp = io.vmu.resp
  private val acquire = io.dmem.acquire
  private val grant = io.dmem.grant

  val cmd = DecodedMemCommand(req.bits.fn.cmd)
  assert(!req.valid || cmd.load || cmd.store || cmd.amo,
    "memif: unknown memory command")

  req.ready := acquire.ready
  acquire.valid := req.valid

  val acq_type = Mux1H(Seq(cmd.load, cmd.store, cmd.amo),
    Seq(Acquire.getType, Acquire.putType, Acquire.putAtomicType))

  val acq_shift = req.bits.addr(tlByteAddrBits-1, 0)
  val acq_union_amo = Cat(acq_shift, req.bits.fn.mt, req.bits.fn.cmd)
  val acq_union = Cat(Mux1H(Seq(
      (cmd.load, req.bits.fn.cmd),
      (cmd.store, req.bits.mask),
      (cmd.amo, acq_union_amo))),
    Bool(true))

  acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = acq_type,
    client_xact_id = req.bits.tag,
    addr_block = req.bits.addr(bPAddr-1, tlBlockAddrOffset),
    addr_beat = req.bits.addr(tlBlockAddrOffset-1, tlByteAddrBits),
    data = req.bits.data,
    union = acq_union)

  val resp_en = grant.bits.hasData() || resp.bits.store
  grant.ready := !resp_en || resp.ready

  resp.valid := grant.valid && resp_en
  resp.bits.tag := grant.bits.client_xact_id
  resp.bits.data := grant.bits.data
  resp.bits.store := grant.bits.isBuiltInType(Grant.putAckType)
}

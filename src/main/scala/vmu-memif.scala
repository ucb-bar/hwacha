package hwacha

import Chisel._

class VMUMemReq extends VMUMemOp
  with VMUTag with VMUData {
  val mask = UInt(width = tlDataBytes)
  val last = Bool()
  val pred = Bool()
}

class VMUMemResp extends VMLUData {
  val store = Bool()
}

class VMUMemIO extends Bundle {
  val req = Decoupled(new VMUMemReq)
  val resp = Decoupled(new VMUMemResp).flip
}

class MBox extends VMUModule {
  val io = new Bundle {
    val inner = new Bundle {
      val abox = new AGUIO().flip
      val sbox = new VMSUIO().flip
      val lbox = new VMLUIO
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

  val sbox_valid = !(pred && write) || sbox.valid
  val lbox_ready = !(pred && read) || lbox.meta.ready
  val req_ready = /*!pred ||*/ req.ready

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(abox.valid, sbox_valid, lbox_ready, req_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  abox.ready := fire(abox.valid)
  sbox.ready := fire(sbox_valid, write)
  lbox.meta.valid := fire(lbox_ready, read, pred)
  req.valid := fire(req_ready/*, pred*/)

  /* Data mask */
  val mask_shift = abox.bits.meta.epad(tlByteAddrBits - 1)
  val mask = Mux(mask_shift,
    Cat(abox.bits.meta.mask, UInt(0, tlDataBytes >> 1)),
    abox.bits.meta.mask)

  /* Load metadata */
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
  req.bits.last := abox.bits.meta.last
  req.bits.pred := pred
  req.bits.tag := Mux(read, lbox.meta.tag, abox.bits.meta.ecnt.raw)
  require(tlByteAddrBits-1 <= bTag)

  /* Response */
  lbox.load.bits := resp.bits
  lbox.load.valid := resp.valid && !resp.bits.store
  resp.ready := resp.bits.store || lbox.load.ready

  /* Store acknowledgement */
  val sret_req = abox.bits.meta.ecnt
  val sret_resp = new CInt(tlByteAddrBits-1)
  sret_resp.raw := resp.bits.tag

  val sret_req_en = fire(null, cmd.store, !pred)
  val sret_resp_en = resp.fire() && resp.bits.store

  io.sret.update := Bool(true)
  io.sret.cnt :=
    Mux(sret_req_en, Cat(Bits(0,1), sret_req.decode()), UInt(0)) +
    Mux(sret_resp_en, Cat(Bits(0,1), sret_resp.decode()), UInt(0))
}

class MBar extends VMUModule {
  val io = new VMUIssueIO {
    val inner = new Bundle {
      val vmu = new VMUMemIO().flip
      val smu = new VMUMemIO().flip
    }
    val outer = new VMUMemIO
    val last = Bool(INPUT)
  }

  private val vmu = io.inner.vmu
  private val smu = io.inner.smu

  val op = Reg(new VMUDecodedOp)
  val scalar = op.mode.scalar

  // Prevent memory requests associated with the next operation from
  // departing until all prior transactions have completed.
  // This ensures that responses attached to different operations never
  // intermix despite arbitrary reordering by the memory system, as is
  // required to compensate for the inability of downstream units
  // (e.g., VLU) to handle multiple simultaneous operations.

  private val w = log2Down(math.max(nvlreq, nvsreq)) + 1
  val count = Reg(init = UInt(0, w))
  val count_inc = io.outer.req.fire()
  val count_dec = io.outer.resp.fire()
  val count_next = count.zext + count_inc.toUInt - count_dec.toUInt
  count := count_next

  assert(!(count_next(w) && count_inc), "VMU: MBar counter overflow")
  assert(!(count_next(w) && count_dec), "VMU: MBar counter underflow")

  val pred = io.outer.req.bits.pred
  val open = Bool()
  val last = Bool()
  open := Bool(false)
  last := Bool(false)

  /* Arbiter */
  io.outer.req.bits := Mux(scalar, smu.req.bits, vmu.req.bits)
  val req_valid = Mux(scalar, smu.req.valid, vmu.req.valid)
  val req_ready = !pred || io.outer.req.ready
  io.outer.req.valid :=  req_valid && open && pred
  io.outer.resp.ready := Mux(scalar, smu.resp.ready, vmu.resp.ready)

  Seq((vmu, !scalar), (smu, scalar)).foreach { case (inner, sel) =>
      inner.req.ready := (!inner.req.bits.pred || io.outer.req.ready) && sel && open
      inner.resp.valid := io.outer.resp.valid && sel
      inner.resp.bits := io.outer.resp.bits
      inner.resp.bits.last := last
  }

  io.op.ready := Bool(false)

  val s_close :: s_open :: s_flush :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_close)

  switch (state) {
    is (s_close) {
      io.op.ready := Bool(true)
      when (io.op.valid) {
        state := s_open
        op := io.op.bits
      }
    }

    is (s_open) {
      open := Bool(true)
      when (req_valid && req_ready && io.outer.req.bits.last) {
        state := s_flush
      }
    }

    is (s_flush) {
      when (count === UInt(0)) {
          state := s_close
      }
      when (count === UInt(1)) {
        last := Bool(true)
        when (count_dec) {
          state := s_close
        }
      }
    }
  }
}

class VMUTileLink extends VMUModule {
  import uncore._

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
  assert(!req.valid || cmd.load || cmd.store || cmd.amo || cmd.pf,
    "memif: unknown memory command")

  req.ready := acquire.ready
  acquire.valid := req.valid

  val acq_type = Mux1H(Seq(cmd.load, cmd.store, cmd.amo, cmd.pf),
    Seq(Acquire.getType, Acquire.putType, Acquire.putAtomicType, Acquire.prefetchType))

  val acq_shift = req.bits.addr(tlByteAddrBits-1, 0)
  val acq_union_amo = Cat(acq_shift, req.bits.fn.mt, req.bits.fn.cmd)
  val acq_union = Cat(Mux1H(Seq(
      (cmd.load || cmd.pf, req.bits.fn.cmd),
      (cmd.store, req.bits.mask),
      (cmd.amo, acq_union_amo))),
    Bool(true))

  acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = acq_type,
    client_xact_id = req.bits.tag,
    addr_block = req.bits.addr(bPAddr-1, tlBlockAddrOffset),
    addr_beat = Mux(cmd.pf, UInt(0),
      req.bits.addr(tlBlockAddrOffset-1, tlByteAddrBits)),
    data = req.bits.data,
    union = acq_union)

  val resp_en = grant.bits.hasData() || resp.bits.store
  grant.ready := !resp_en || resp.ready

  resp.valid := grant.valid && resp_en
  resp.bits.tag := grant.bits.client_xact_id
  resp.bits.data := grant.bits.data
  resp.bits.store := grant.bits.isBuiltInType(Grant.putAckType)
  resp.bits.last := Bool(false) // don't care
}

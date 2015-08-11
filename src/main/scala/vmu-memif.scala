package hwacha

import Chisel._

class VMUMemReq extends VMUMemOp {
  val tag = UInt(width = bTag)
  val store = new VMUStoreData
  val last = Bool()
}

class VMUMemResp extends VMULoadData {
  val store = Bool()
}

class VMUMemIO extends VMUBundle {
  val req = Decoupled(new VMUMemReq)
  val resp = Decoupled(new VMUMemResp).flip
}

class MBox extends VMUModule {
  val io = new Bundle {
    val inner = new Bundle {
      val abox = new VMUAddrIO().flip
      val sbox = new VMUStoreIO
      val lbox = new VMULoadIO
      val sret = new CounterUpdateIO(sretBits)
    }
    val outer = new VMUMemIO
  }

  private val abox = io.inner.abox
  private val sbox = io.inner.sbox
  private val lbox = io.inner.lbox
  private val req = io.outer.req
  private val resp = io.outer.resp

  val cmd = DecodedMemCommand(abox.bits.cmd)
  val sbox_ready = !cmd.write || sbox.ready
  val lbox_ready = !cmd.read || lbox.meta.ready

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(abox.valid, sbox_ready, lbox_ready, req.ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  abox.ready := fire(abox.valid)
  sbox.valid := fire(sbox_ready, cmd.write)
  lbox.meta.valid := fire(lbox_ready, cmd.read)
  req.valid := fire(req.ready)

  // Load metadata
  lbox.meta.data.eidx := abox.bits.meta.eidx
  lbox.meta.data.ecnt := abox.bits.meta.ecnt
  lbox.meta.data.eskip := abox.bits.meta.eskip

  // Store metadata
  sbox.meta.ecnt := abox.bits.meta.ecnt
  sbox.meta.eskip := abox.bits.meta.eskip
  sbox.meta.offset := abox.bits.meta.offset
  sbox.meta.first := abox.bits.meta.first
  sbox.meta.last := abox.bits.meta.last

  req.bits.cmd := abox.bits.cmd
  req.bits.mt := abox.bits.mt
  req.bits.addr := abox.bits.addr
  req.bits.tag := Mux(cmd.read, lbox.meta.tag, sbox.meta.ecnt)
  require(tlByteAddrBits <= bTag)
  req.bits.store := sbox.store
  req.bits.last := abox.bits.meta.last

  // Response demux
  resp.ready := resp.bits.store || lbox.load.ready
  lbox.load.valid := resp.valid && !resp.bits.store
  lbox.load.bits.data := resp.bits.data
  lbox.load.bits.tag := resp.bits.tag
  lbox.load.bits.last := resp.bits.last

  io.inner.sret.update := resp.valid && resp.bits.store
  io.inner.sret.cnt := Mux(resp.bits.tag === UInt(0), UInt(tlDataBytes), resp.bits.tag)
}

class MBar extends VMUModule {
  val io = new Bundle {
    val issue = new VMUIssueOpIO().flip
    val inner = new Bundle {
      val vmu = new VMUMemIO().flip
      val smu = new VMUMemIO().flip
    }
    val outer = new VMUMemIO
  }

  private val vmu = io.inner.vmu
  private val smu = io.inner.smu

  // TODO: Use queue to buffer io.issue.op.mode.scalar flags for greater
  //       decoupling if needed
  val start_hold = Reg(init = Bool(false))
  val start = start_hold || io.issue.fire
  start_hold := start

  io.issue.busy := start_hold

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

  val scalar = Reg(Bool())
  val open = Bool()
  val last = Bool()
  open := Bool(false)
  last := Bool(false)

  // Arbiter
  io.outer.req.bits := Mux(scalar, smu.req.bits, vmu.req.bits)
  io.outer.req.valid := Mux(scalar, smu.req.valid, vmu.req.valid) && open
  io.outer.resp.ready := Mux(scalar, smu.resp.ready, vmu.resp.ready)

  Seq((vmu, !scalar), (smu, scalar)).foreach { case (inner, sel) =>
    inner.req.ready := io.outer.req.ready && sel && open
    inner.resp.valid := io.outer.resp.valid && sel
    inner.resp.bits := io.outer.resp.bits
    inner.resp.bits.last := last
  }

  val s_close :: s_open :: s_flush :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_close)

  switch (state) {
    is (s_close) {
      when (start) {
        start_hold := Bool(false)
        scalar := io.issue.op.mode.scalar
        state := s_open
      }
    }

    is (s_open) {
      open := Bool(true)
      when (count_inc && io.outer.req.bits.last) {
        state := s_flush
      }
    }

    is (s_flush) {
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

  val cmd = DecodedMemCommand(req.bits.cmd)
  assert(!req.valid || cmd.load || cmd.store || cmd.amo || cmd.pf,
    "Unknown memory command")

  req.ready := acquire.ready
  acquire.valid := req.valid

  val acq_type = Mux1H(Seq(cmd.load, cmd.store, cmd.amo, cmd.pf),
    Seq(Acquire.getType, Acquire.putType, Acquire.putAtomicType, Acquire.prefetchType))

  val acq_shift = req.bits.addr(tlByteAddrBits-1, 0)
  val acq_union_amo = Cat(acq_shift, req.bits.mt, req.bits.cmd)
  val acq_union = Cat(Mux1H(Seq(cmd.load || cmd.pf, cmd.store, cmd.amo),
    Seq(req.bits.cmd, req.bits.store.mask, acq_union_amo)), Bool(true))

  acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = acq_type,
    client_xact_id = req.bits.tag,
    addr_block = req.bits.addr(paddrBits-1, tlBlockAddrOffset),
    addr_beat = req.bits.addr(tlBlockAddrOffset-1, tlByteAddrBits) &
      Fill(tlBeatAddrBits, !cmd.pf),
    data = req.bits.store.data,
    union = acq_union)

  val resp_en = grant.bits.hasData() || resp.bits.store
  grant.ready := (!resp_en || resp.ready)

  resp.valid := grant.valid && resp_en
  resp.bits.tag := grant.bits.client_xact_id
  resp.bits.data := grant.bits.data
  resp.bits.store := grant.bits.isBuiltInType(Grant.putAckType)
  resp.bits.last := Bool(false) // don't care
}

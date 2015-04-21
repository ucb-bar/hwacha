package hwacha

import Chisel._

class VMUMemReq extends VMUMemOp {
  val tag = UInt(width = bTag)
  val store = new VMUStoreData
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

  // Response demux
  resp.ready := resp.bits.store || lbox.load.ready
  lbox.load.valid := resp.valid && !resp.bits.store
  lbox.load.bits.data := resp.bits.data
  lbox.load.bits.tag := resp.bits.tag

  io.inner.sret.update := resp.valid && resp.bits.store
  io.inner.sret.cnt := Mux(resp.bits.tag === UInt(0), UInt(tlDataBytes), resp.bits.tag)
}

class VMUMemArb(n: Int) extends VMUModule {
  private val lgn = log2Up(n)
  val io = new Bundle {
    val inner = Vec.fill(n)(new VMUMemIO).flip
    val outer = new VMUMemIO
    val sel = UInt(INPUT, lgn)
  }

  val sel = UIntToOH(io.sel, n)
  val sel_seq = (0 until n).map(sel(_))

  io.outer.req.valid := Mux1H(sel_seq, io.inner.map(_.req.valid))
  io.outer.req.bits := Mux1H(sel_seq, io.inner.map(_.req.bits))
  io.outer.resp.ready := Mux1H(sel_seq, io.inner.map(_.resp.ready))

  io.inner.zip(sel_seq).foreach { case (inner, en) =>
    inner.req.ready := io.outer.req.ready && en
    inner.resp.valid := io.outer.resp.valid && en
    inner.resp.bits := io.outer.resp.bits
  }
}

class VMUMemMonitor extends VMUModule {
  val io = new Bundle {
    val req = Bool(INPUT)
    val resp = Bool(INPUT)
    val quiescent = Bool(OUTPUT)
  }

  val count = Reg(init = UInt(0, math.max(nvlreq, nvsreq)))
  val count_next = count + io.req.toUInt - io.resp.toUInt
  count := count_next
  io.quiescent := (count_next === UInt(0))
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
}

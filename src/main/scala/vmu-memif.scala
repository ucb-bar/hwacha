package hwacha

import Chisel._
import uncore._

class VMUMemReq extends VMUMemOp {
  val tag = UInt(width = tagBits)
  val store = new VMUStoreData
}

class VMUMemResp extends VMULoadData {
  val store = Bool()
}

class VMUMemIO extends VMUBundle {
  val req = Decoupled(new VMUMemReq)
  val resp = Decoupled(new VMUMemResp).flip
}


class FinishEntry extends Bundle {
  val xid = Bits(width = params(TLManagerXactIdBits))
  val dst = UInt(width = log2Up(params(LNEndpoints)))
}

class MBox extends VMUModule {
  val io = new Bundle {
    val inner = new Bundle {
      val abox = new VMUAddrIO().flip
      val sbox = new VMUStoreIO
      val lbox = new VMULoadIO
      val sret = Valid(UInt(width = sretBits))
    }
    val outer = new VMUMemIO
  }

  private val abox = io.inner.abox
  private val sbox = io.inner.sbox
  private val lbox = io.inner.lbox
  private val req = io.outer.req
  private val resp = io.outer.resp

  val cmd = abox.bits.cmd
  val cmd_load = (cmd === M_XRD)
  val cmd_store = (cmd === M_XWR)
  val cmd_amo = isAMO(cmd)

  val lbox_en = cmd_load || cmd_amo
  val sbox_en = cmd_store || cmd_amo
  val sbox_ready = !sbox_en || sbox.ready
  val lbox_ready = !lbox_en || lbox.meta.ready

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(abox.valid, sbox_ready, lbox_ready, req.ready)
    (rvs.filter(_ != exclude) ++ include).reduce(_ && _)
  }

  abox.ready := fire(abox.valid)
  sbox.valid := fire(sbox_ready, sbox_en)
  lbox.meta.valid := fire(lbox_ready, lbox_en)
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
  req.bits.tag := Mux(lbox_en, lbox.meta.tag, sbox.meta.ecnt) // FIXME
  req.bits.store := sbox.store

  // Response demux
  resp.ready := resp.bits.store || lbox.load.ready
  lbox.load.valid := resp.valid && !resp.bits.store
  lbox.load.bits.data := resp.bits.data
  lbox.load.bits.tag := resp.bits.tag
  io.inner.sret.valid := resp.valid && resp.bits.store
  io.inner.sret.bits := Mux(resp.bits.tag === UInt(0), UInt(tlDataBytes), resp.bits.tag)
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

class VMUTileLink extends VMUModule {
  val io = new Bundle {
    val vmu = new VMUMemIO().flip
    val dmem = new HeaderlessUncachedTileLinkIO
  }

  private val tlBlockAddrOffset = tlBeatAddrBits + tlByteAddrBits

  private val req = io.vmu.req
  private val resp = io.vmu.resp

  private val acquire = io.dmem.acquire
  private val grant = io.dmem.grant
  private val finish = io.dmem.finish

  val cmd = req.bits.cmd
  val cmd_load = (cmd === M_XRD)
  val cmd_store = (cmd === M_XWR)
  val cmd_amo = isAMO(cmd)
  val cmd_pf = isPrefetch(cmd)

  assert(!req.valid || cmd_load || cmd_store || cmd_amo || cmd_pf,
    "Unknown memory command")

  val acq_get = cmd_load || cmd_amo
  val acq_put = cmd_store || cmd_amo

  req.ready := acquire.ready
  acquire.valid := req.valid

  val acq_type = Mux1H(Seq(cmd_load, cmd_store, cmd_amo, cmd_pf),
    Seq(Acquire.getType, Acquire.putType, Acquire.putAtomicType, Acquire.prefetchType))

  val acq_shift = req.bits.addr(tlByteAddrBits-1, 0)
  val acq_union_amo = Cat(acq_shift, req.bits.mt, req.bits.cmd)
  val acq_union = Cat(Mux1H(Seq(cmd_load || cmd_pf, cmd_store, cmd_amo),
    Seq(req.bits.cmd, req.bits.store.mask, acq_union_amo)), Bool(true))

  acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = acq_type,
    client_xact_id = req.bits.tag,
    addr_block = req.bits.addr(paddrBits-1, tlBlockAddrOffset),
    addr_beat = req.bits.addr(tlBlockAddrOffset-1, tlByteAddrBits) &
      Fill(tlBeatAddrBits, !cmd_pf),
    data = req.bits.store.data,
    union = acq_union)


  val finishq = Module(new Queue(new FinishEntry, 2))

  val grant_type_load = grant.bits.payload.hasData()
  val grant_type_store =
    grant.bits.payload.isBuiltInType() &&
    grant.bits.payload.is(uncore.Grant.putAckType)
  val resp_en = grant_type_load || grant_type_store
  val finish_en = grant.bits.payload.requiresAck()

  val resp_ready = !resp_en || resp.ready
  val finishq_ready = !finish_en || finishq.io.enq.ready

  private def fire_grant(exclude: Bool, include: Bool*) = {
    val rvs = Seq(grant.valid, resp_ready, finishq_ready)
    (rvs.filter(_ != exclude) ++ include).reduce(_ && _)
  }

  grant.ready := fire_grant(grant.valid)

  resp.valid := fire_grant(resp_ready, resp_en)
  resp.bits.tag := grant.bits.payload.client_xact_id
  resp.bits.data := grant.bits.payload.data
  resp.bits.store := grant_type_store

  finishq.io.enq.valid := fire_grant(finishq_ready, finish_en)
  finishq.io.enq.bits.xid := grant.bits.payload.manager_xact_id
  finishq.io.enq.bits.dst := grant.bits.header.src

  finishq.io.deq.ready := finish.ready
  finish.valid := finishq.io.deq.valid
  finish.bits.payload.manager_xact_id := finishq.io.deq.bits.xid
  finish.bits.header.dst := finishq.io.deq.bits.dst
}

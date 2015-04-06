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

  val fn = abox.bits.fn
  val fn_load = (fn === M_XRD)
  val fn_store = (fn === M_XWR)
  val fn_amo = isAMO(fn)

  val lbox_en = fn_load || fn_amo
  val sbox_en = fn_store || fn_amo
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

  req.bits.fn := abox.bits.fn
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

  val fn = req.bits.fn
  val fn_load = (fn === M_XRD)
  val fn_store = (fn === M_XWR)
  val fn_amo = isAMO(fn)
  val fn_pf = isPrefetch(fn)

  assert(!req.valid || fn_load || fn_store || fn_amo || fn_pf,
    "Unknown memory command")

  val acq_get = fn_load || fn_amo
  val acq_put = fn_store || fn_amo

  req.ready := acquire.ready
  acquire.valid := req.valid

  val acq_type = Mux1H(Seq(fn_load, fn_store, fn_amo, fn_pf),
    Seq(Acquire.getType, Acquire.putType, Acquire.putAtomicType, Acquire.prefetchType))

  val acq_shift = req.bits.addr(tlByteAddrBits-1, 0)
  val acq_union_amo = Cat(acq_shift, req.bits.mt, req.bits.fn)
  val acq_union = Cat(Mux1H(Seq(fn_load || fn_pf, fn_store, fn_amo),
    Seq(req.bits.fn, req.bits.store.mask, acq_union_amo)), Bool(true))

  acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = acq_type,
    client_xact_id = req.bits.tag,
    addr_block = req.bits.addr(paddrBits-1, tlBlockAddrOffset),
    addr_beat = req.bits.addr(tlBlockAddrOffset-1, tlByteAddrBits) &
      Fill(tlBeatAddrBits, !fn_pf),
    data = req.bits.store.data,
    union = acq_union)

  val grant_type_load = grant.bits.hasData()
  val grant_type_store = grant.bits.isBuiltInType(uncore.Grant.putAckType)
  val resp_en = grant_type_load || grant_type_store

  grant.ready := !resp_en || resp.ready
  resp.valid := grant.valid && resp_en
  resp.bits.tag := grant.bits.client_xact_id
  resp.bits.data := grant.bits.data
  resp.bits.store := grant_type_store
}

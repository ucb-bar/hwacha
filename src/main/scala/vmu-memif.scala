package hwacha

import Chisel._
import uncore._

class VMUMemInternalIO extends Bundle {
  val abox = new VMUMemOpIO
  val sbox = new VMUStoreIO
  val lbox = new VMULoadIO().flip
}

class VMUMemIO extends VMUBundle {
  val abox = Decoupled(new VMUMemOp(UInt(width = tagBits)))
  val sbox = Decoupled(new VMUStoreData)
  val lbox = Decoupled(new VMULoadData).flip
}

class FinishEntry extends Bundle {
  val xid = Bits(width = params(TLManagerXactIdBits))
  val dst = UInt(width = log2Up(params(LNEndpoints)))
}


class MBox extends VMUModule {
  val io = new Bundle {
    val in = new VMUMemInternalIO().flip
    val out = new VMUMemIO
  }

  val fn = io.in.abox.bits.fn
  val fn_load = (fn === M_XRD)
  val fn_store = (fn === M_XWR)
  val fn_amo = isAMO(fn)

  val lbox_en = fn_load || fn_amo
  val sbox_en = fn_store || fn_amo

  val sbox_ready = !sbox_en || io.in.sbox.meta.ready
  val lbox_ready = !lbox_en || io.in.lbox.meta.ready

  val meta_ready = sbox_ready && lbox_ready
  io.out.abox.valid := io.in.abox.valid && meta_ready
  io.in.abox.ready := io.out.abox.ready && meta_ready

  val abox_en = io.in.abox.valid && io.out.abox.ready
  io.in.sbox.meta.valid := abox_en && sbox_en && lbox_ready
  io.in.lbox.meta.valid := abox_en && lbox_en && sbox_ready

  // Load metadata
  io.in.lbox.meta.data.eidx := io.in.abox.bits.meta.eidx
  io.in.lbox.meta.data.ecnt := io.in.abox.bits.meta.ecnt
  io.in.lbox.meta.data.eskip := io.in.abox.bits.meta.eskip

  // Store metadata
  io.in.sbox.meta.bits.ecnt := io.in.abox.bits.meta.ecnt
  io.in.sbox.meta.bits.eskip := io.in.abox.bits.meta.eskip
  io.in.sbox.meta.bits.offset := io.in.abox.bits.meta.offset
  io.in.sbox.meta.bits.first := io.in.abox.bits.meta.first
  io.in.sbox.meta.bits.last := io.in.abox.bits.meta.last

  io.out.abox.bits.fn := io.in.abox.bits.fn
  io.out.abox.bits.mt := io.in.abox.bits.mt
  io.out.abox.bits.addr := io.in.abox.bits.addr
  io.out.abox.bits.meta := Mux(lbox_en, io.in.lbox.meta.tag, io.in.abox.bits.meta.ecnt) // FIXME
  io.out.sbox <> io.in.sbox.store
  io.out.lbox <> io.in.lbox.load
}

class VMUTileLink extends VMUModule {
  val io = new Bundle {
    val vmu = new VMUMemIO().flip
    val dmem = new HeaderlessUncachedTileLinkIO
  }

  private val tlBlockAddrOffset = tlBeatAddrBits + tlByteAddrBits

  private val abox = io.vmu.abox
  private val sbox = io.vmu.sbox
  private val lbox = io.vmu.lbox

  private val acquire = io.dmem.acquire
  private val grant = io.dmem.grant
  private val finish = io.dmem.finish

  val fn = abox.bits.fn
  val fn_load = (fn === M_XRD)
  val fn_store = (fn === M_XWR)
  val fn_amo = isAMO(fn)
  val fn_pf = isPrefetch(fn)

  assert(!abox.valid || fn_load || fn_store || fn_amo || fn_pf,
    "Unknown memory command")

  val acq_get = fn_load || fn_amo
  val acq_put = fn_store || fn_amo
/*
  val sbox_valid = !acq_put || sbox.valid
  sbox.ready := acquire.ready && abox.valid && acq_put
  abox.ready := acquire.ready && sbox_valid
  acquire.valid := abox.valid && sbox_valid
*/
  sbox.ready := Bool(true)
  abox.ready := acquire.ready
  acquire.valid := abox.valid

  val acq_type = Mux1H(Seq(fn_load, fn_store, fn_amo, fn_pf),
    Seq(Acquire.getType, Acquire.putType, Acquire.putAtomicType, Acquire.prefetchType))

  val acq_shift = abox.bits.addr(tlByteAddrBits-1, 0)
  val acq_union_amo = Cat(acq_shift, abox.bits.mt, abox.bits.fn)
  val acq_union = Cat(Mux1H(Seq(fn_load || fn_pf, fn_store, fn_amo),
    Seq(abox.bits.fn, sbox.bits.mask, acq_union_amo)), Bool(true))

  acquire.bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = acq_type,
    client_xact_id = abox.bits.meta,
    addr_block = abox.bits.addr(paddrBits-1, tlBlockAddrOffset),
    addr_beat = abox.bits.addr(tlBlockAddrOffset-1, tlByteAddrBits) &
      Fill(tlBeatAddrBits, !fn_pf),
    data = sbox.bits.data,
    union = acq_union)

  val grant_has_data = grant.bits.payload.hasData()
  val grant_needs_ack = grant.bits.payload.requiresAck()

  lbox.bits.tag := grant.bits.payload.client_xact_id
  lbox.bits.data := grant.bits.payload.data
  lbox.valid := grant.valid && grant_has_data

  val finishq = Module(new Queue(new FinishEntry, 2))

  grant.ready := (!grant_has_data || lbox.ready) &&
    (!grant_needs_ack || finishq.io.enq.ready)

  finishq.io.enq.valid := grant_needs_ack && grant.valid && (!grant_has_data || lbox.ready)
  finishq.io.enq.bits.xid := grant.bits.payload.manager_xact_id
  finishq.io.enq.bits.dst := grant.bits.header.src

  finishq.io.deq.ready := finish.ready
  finish.valid := finishq.io.deq.valid
  finish.bits.payload.manager_xact_id := finishq.io.deq.bits.xid
  finish.bits.header.dst := finishq.io.deq.bits.dst
}

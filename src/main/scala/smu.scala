package hwacha

import Chisel._
import cde.{Parameters, Field}

case object HwachaNSMUEntries extends Field[Int]

abstract class SMUBundle(implicit p: Parameters)
  extends HwachaBundle()(p) with SMUParameters

trait SMUParameters extends MemParameters {
  val nSMU = p(HwachaNSMUEntries)
  val bSMUTag = log2Up(nSRegs)
}

class SMUFn extends Bundle {
  val cmd = Bits(width = SZ_SMU_CMD)
  val mt = Bits(width = MT_SZ)
}

trait SMUTag extends SMUBundle {
  val tag = UInt(width = bSMUTag)
}
trait SMUData extends SMUTag {
  val data = Bits(width = regLen)
}

class SMUReq(implicit p: Parameters) extends SMUBundle()(p)
  with SMUData {
  val fn = new SMUFn
  val addr = UInt(width = bVAddrExtended)
}

class SMUResp(implicit p: Parameters) extends SMUBundle()(p)
  with SMUData {
  val store = Bool()
}

class SMUIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val req = Decoupled(new SMUReq)
  val resp = Decoupled(new SMUResp).flip
  val confirm = Bool(INPUT)
}


class SMUEntry(implicit p: Parameters) extends SMUBundle()(p)
  with SMUTag {
  val mt = Bits(width = MT_SZ)
  val offset = UInt(width = tlByteAddrBits)
}

class SMU(implicit p: Parameters) extends HwachaModule()(p)
  with SMUParameters {
  import uncore._

  val io = new Bundle {
    val scalar = new SMUIO().flip
    val dmem = new ClientUncachedTileLinkIO

    val tlb = new RTLBIO
    val irq = new IRQIO
  }

  val table = Module(new Table(nSMU, new SMUEntry))
  private val tw = table.io.w
  private val tr = table.io.r

  //--------------------------------------------------------------------\\
  // request
  //--------------------------------------------------------------------\\
  private val acquire = io.dmem.acquire

  val req = Reg(io.scalar.req.bits)
  val req_mt = DecodedMemType(req.fn.mt)
  val req_store = (req.fn.cmd === SM_S)

  val tbox = Module(new TBox(1))
  private val tlb = tbox.io.inner(0)
  tlb.req.valid := Bool(false)
  tlb.req.bits.addr := req.addr
  tlb.req.bits.store := req_store
  tlb.req.bits.mt := req_mt
  io.tlb <> tbox.io.outer
  io.irq <> tbox.io.irq

  private val tlBlockAddrOffset = tlBeatAddrBits + tlByteAddrBits

  val addr_block = req.addr(bPAddr-1, tlBlockAddrOffset)
  val addr_beat = req.addr(tlBlockAddrOffset-1, tlByteAddrBits)
  val addr_offset = req.addr(tlByteAddrBits-1, 0)

  private def mts(mt: DecodedMemType) = Seq(mt.b, mt.h, mt.w, mt.d)
  private def mask(mt: DecodedMemType) =
    Mux1H(mts(mt).zipWithIndex.map { case (s, i) =>
      (s, Fill((1 << i) - 1, Bool(true)))
    })

  val req_mask_base = Cat(mask(req_mt), Bool(true))
  val req_mask = req_mask_base << addr_offset
  val req_data = req.data << Cat(addr_offset, UInt(0,3))

  tw.valid := Bool(false)
  tw.bits.tag := req.tag
  tw.bits.mt := req.fn.mt
  tw.bits.offset := addr_offset

  acquire.bits := Mux(req_store,
    Put(tw.tag, addr_block, addr_beat, req_data, req_mask),
    Get(tw.tag, addr_block, addr_beat))

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(acquire.ready, tw.ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  val s_idle :: s_tlb :: s_mem :: Nil = Enum(UInt(), 3)
  val state = Reg(init = s_idle)

  io.scalar.confirm := Bool(false)
  io.scalar.req.ready := Bool(false)
  acquire.valid := Bool(false)

  switch (state) {
    is (s_idle) {
      io.scalar.req.ready := Bool(true)
      when (io.scalar.req.valid) {
        state := s_tlb
        req := io.scalar.req.bits
      }
    }

    is (s_tlb) {
      tlb.req.valid := Bool(true)
      when (tlb.req.ready) {
        state := Mux(tlb.resp.xcpt, s_idle, s_mem)
        io.scalar.confirm := !tlb.resp.xcpt
        req.addr := tlb.paddr()
      }
    }

    is (s_mem) {
      acquire.valid := fire(acquire.ready)
      when(fire(acquire.ready)) {
        printf("FIRED: addr: 0x%x\n", addr_block)
        printf("BEAT       : 0x%x\n", addr_beat)
      }
      tw.valid := fire(tw.ready)
      when (fire(null)) {
        state := s_idle
      }
    }
  }

  //--------------------------------------------------------------------\\
  // request
  //--------------------------------------------------------------------\\
  private val grant = io.dmem.grant

  io.scalar.resp.valid := grant.valid
  grant.ready := io.scalar.resp.ready
  tr.valid := io.scalar.resp.fire()
  tr.bits := grant.bits.client_xact_id

  val resp_mt = DecodedMemType(tr.record.mt)
  val resp_shift = Cat(tr.record.offset, UInt(0,3))
  val resp_data = grant.bits.data >> resp_shift

  val resp_mask = FillInterleaved(8, mask(resp_mt))
  val resp_sign = Mux1H(mts(resp_mt).zipWithIndex.map { case (s, i) =>
    val w = 1 << (i + 3)
    (s, resp_data(w - 1))
  })
  val resp_extend = Fill(regLen-8, resp_mt.signed && resp_sign)

  io.scalar.resp.bits.store := grant.bits.isBuiltInType(Grant.putAckType)
  io.scalar.resp.bits.tag := tr.record.tag
  io.scalar.resp.bits.data := Cat(
    (resp_data(regLen-1, 8) & resp_mask) |
      (resp_extend & (~resp_mask)),
    resp_data(7, 0))
}

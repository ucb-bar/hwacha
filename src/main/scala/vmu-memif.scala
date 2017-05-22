package hwacha

import Chisel._
import config._
import uncore.tilelink2._

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
  req.bits.tag := Mux(read, lbox.meta.tag, abox.bits.meta.tag)
  require(tlByteAddrBits-1 <= bVMUTag)

  /* Response */
  lbox.load.bits := resp.bits
  lbox.load.valid := resp.valid && !resp.bits.store
  resp.ready := resp.bits.store || lbox.load.ready

  /* Store acknowledgement */
  val sret_req = abox.bits.meta.ecnt
  val sret_resp = Wire(new CInt(tlByteAddrBits-1))
  sret_resp.raw := resp.bits.tag(tlByteAddrBits-1, 0)
  abox.memif.resp.bits.tag := resp.bits.tag(bVMUTag - 1, tlByteAddrBits - 1)

  val sret_req_en = fire(null, cmd.store, !pred)
  abox.memif.req := fire(null, cmd.store, pred)
  val sret_resp_en = resp.fire() && resp.bits.store
  abox.memif.resp.valid := sret_resp_en

  io.sret.update := Bool(true)
  io.sret.cnt :=
    Mux(sret_req_en, Cat(Bits(0,1), sret_req.decode()), UInt(0)) +
    Mux(sret_resp_en, Cat(Bits(0,1), sret_resp.decode()), UInt(0))
}

class VMUTileLink(edge: TLEdgeOut)(implicit p: Parameters) extends VMUModule()(p) {
  val io = new Bundle {
    val vmu = new VMUMemIO().flip
    val dmem = TLBundle(edge.bundle)
  }

  private val req = io.vmu.req
  private val resp = io.vmu.resp
  private val acquire = io.dmem.a
  private val grant = io.dmem.d

  val cmd = DecodedMemCommand(req.bits.fn.cmd)
  assert(!req.valid || cmd.load || cmd.store || cmd.amo,
    "memif: unknown memory command")

  req.ready := acquire.ready
  acquire.valid := req.valid
  val req_tag = Cat(cmd.read, req.bits.tag)

  val acq_amo = MuxLookup(req.bits.fn.cmd, Wire(new TLBundleA(edge.bundle)), Seq(
    M_XA_SWAP -> edge.Logical(req_tag, req.bits.addr, req.bits.fn.mt, req.bits.data, TLAtomics.SWAP)._2,
    M_XA_XOR  -> edge.Logical(req_tag, req.bits.addr, req.bits.fn.mt, req.bits.data, TLAtomics.XOR)._2,
    M_XA_OR   -> edge.Logical(req_tag, req.bits.addr, req.bits.fn.mt, req.bits.data, TLAtomics.OR)._2,
    M_XA_AND  -> edge.Logical(req_tag, req.bits.addr, req.bits.fn.mt, req.bits.data, TLAtomics.AND)._2,
    M_XA_ADD  -> edge.Arithmetic(req_tag, req.bits.addr, req.bits.fn.mt, req.bits.data, TLAtomics.ADD)._2,
    M_XA_MIN  -> edge.Arithmetic(req_tag, req.bits.addr, req.bits.fn.mt, req.bits.data, TLAtomics.MIN)._2,
    M_XA_MAX  -> edge.Arithmetic(req_tag, req.bits.addr, req.bits.fn.mt, req.bits.data, TLAtomics.MAX)._2,
    M_XA_MINU -> edge.Arithmetic(req_tag, req.bits.addr, req.bits.fn.mt, req.bits.data, TLAtomics.MINU)._2,
    M_XA_MAXU -> edge.Arithmetic(req_tag, req.bits.addr, req.bits.fn.mt, req.bits.data, TLAtomics.MAXU)._2
  ))

  val alignment = Mux(~req.bits.addr(0),
                    Mux(~req.bits.addr(1),
                      Mux(~req.bits.addr(2),
                         Mux(~req.bits.addr(3), 4.U, 3.U),
                      2.U),
                    1.U),
                  0.U)
  val req_size = alignment//Max(req.bits.fn.mt, alignment)
  val req_addr_beat_aligned = (req.bits.addr >> UInt(tlByteAddrBits)) << UInt(tlByteAddrBits)
  acquire.bits := Mux1H(Seq(
    cmd.load -> edge.Get(req_tag, req_addr_beat_aligned, 4.U)._2,
    cmd.store -> Mux(req.bits.mask.andR,
      edge.Put(req_tag, req.bits.addr, 4.U, req.bits.data)._2,
      edge.Put(req_tag, req.bits.addr, req_size, req.bits.data, req.bits.mask)._2),
    cmd.amo -> acq_amo
  ))

  val resp_en = edge.hasData(grant.bits) || resp.bits.store
  grant.ready := !resp_en || resp.ready

  resp.valid := grant.valid && resp_en
  resp.bits.tag := grant.bits.source(bVMUTag-1,0)
  resp.bits.data := grant.bits.data
  resp.bits.store := grant.bits.opcode === TLMessages.AccessAck

  //Tie off unused channels
  io.dmem.b.ready := Bool(true)
  io.dmem.c.valid := Bool(false)
  io.dmem.e.valid := Bool(false)
}

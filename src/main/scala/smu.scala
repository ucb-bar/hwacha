package hwacha

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBPTWIO, TLBConfig}

case object HwachaNSMUEntries extends Field[Int]

abstract class SMUBundle(implicit p: Parameters)
  extends HwachaBundle()(p) with SMUParameters

trait SMUParameters extends MemParameters {
  val nSMU = p(HwachaNSMUEntries)
  val bSMUTag = log2Up(nSRegs)
}

class SMUFn extends Bundle {
  val cmd = UInt(SZ_SMU_CMD.W)
  val mt = UInt(MT_SZ.W)
}

trait SMUTag extends SMUBundle {
  val tag = UInt(bSMUTag.W)
}
trait SMUData extends SMUTag {
  val data = UInt(regLen.W)
}

class SMUReq(implicit p: Parameters) extends SMUBundle()(p)
  with SMUData {
  val fn = new SMUFn
  val addr = UInt(bVAddrExtended.W)
  val status = new freechips.rocketchip.rocket.MStatus
}

class SMUResp(implicit p: Parameters) extends SMUBundle()(p)
  with SMUData {
  val store = Bool()
}

class SMUIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val req = Decoupled(new SMUReq)
  val resp = Flipped(Decoupled(new SMUResp))
  val confirm = Input(Bool())
}


class SMUEntry(implicit p: Parameters) extends SMUBundle()(p)
  with SMUTag {
  val mt = UInt(MT_SZ.W)
  val offset = UInt(tlByteAddrBits.W)
}

class SMU(implicit p: Parameters) extends LazyModule {
  lazy val module = new SMUModule(this)
  val masterNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "HwachaSMU", sourceId = IdRange(0, p(HwachaNSMUEntries)))))))
}

class SMUModule(outer: SMU)(implicit p: Parameters) extends LazyModuleImp(outer)
  with SMUParameters {

  val io = IO(new Bundle {
    val scalar = Flipped(new SMUIO())

    val ptw = new TLBPTWIO
    val irq = new IRQIO
  })
  val (dmem, edge) = outer.masterNode.out.head

  val table = Module(new Table(nSMU, new SMUEntry))
  table.suggestName("tableInst")
  private val tw = table.io.w
  private val tr = table.io.r

  //--------------------------------------------------------------------\\
  // request
  //--------------------------------------------------------------------\\
  private val acquire = dmem.a

  val req = Reg(io.scalar.req.bits)
  val req_mt = DecodedMemType(req.fn.mt)
  val req_store = (req.fn.cmd === SM_S)

  val tbox = Module(new TBox(1))
  tbox.suggestName("tboxInst")
  private val tlb = tbox.io.inner(0)
  tlb.req.valid := false.B
  tlb.req.bits.vaddr := req.addr
  tlb.req.bits.passthrough := false.B
  tlb.req.bits.size := req_mt.shift()
  tlb.req.bits.cmd := Mux(req_store, M_XWR, M_XRD)
  tlb.status := req.status
  io.irq <> tbox.io.irq

  val ptlb = Module(new freechips.rocketchip.rocket.TLB(instruction = false, lgMaxSize = log2Ceil(regBytes), TLBConfig(nSets=nptlb, nWays=1, nSectors=1))(edge, p))
  ptlb.io.req <> tbox.io.outer.req
  tbox.io.outer.resp <> ptlb.io.resp
  io.ptw <> ptlb.io.ptw
  ptlb.io.ptw.status := tbox.io.outer.status
  ptlb.io.sfence.valid := false.B

  val addr_offset = req.addr(tlByteAddrBits-1, 0)

  private def mts(mt: DecodedMemType) = Seq(mt.b, mt.h, mt.w, mt.d)
  private def mask(mt: DecodedMemType) =
    Mux1H(mts(mt).zipWithIndex.map { case (s, i) =>
      // TODO FIXME COLIN: this is a workaround for zero width wires in chisel3
      if(i == 0) (s, UInt(0, width=1)) else (s, Fill((1 << i) - 1, UInt(1, width=1)))
    })

  val req_mask_base = Cat(mask(req_mt), true.B)
  val req_mask = req_mask_base << addr_offset
  val req_data = req.data << Cat(addr_offset, UInt(0,3))

  tw.valid := false.B
  tw.bits.tag := req.tag
  tw.bits.mt := req.fn.mt
  tw.bits.offset := addr_offset

  val SMUID = 1.U(2.W)
  acquire.bits := Mux(req_store,
    edge.Put(tw.tag, req.addr, req_mt.shift(), req_data, req_mask)._2,
    edge.Get(tw.tag, req.addr, req_mt.shift())._2)

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(acquire.ready, tw.ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  val s_idle :: s_tlb :: s_mem :: Nil = Enum(UInt(), 3)
  val state = RegInit(s_idle)

  io.scalar.confirm := false.B
  io.scalar.req.ready := false.B
  acquire.valid := false.B

  switch (state) {
    is (s_idle) {
      io.scalar.req.ready := true.B
      when (io.scalar.req.valid) {
        state := s_tlb
        req := io.scalar.req.bits
      }
    }

    is (s_tlb) {
      tlb.req.valid := true.B
      when (tlb.req.ready) {
        state := Mux(tlb.resp.xcpt, s_idle, s_mem)
        io.scalar.confirm := !tlb.resp.xcpt
        req.addr := tlb.paddr()
      }
    }

    is (s_mem) {
      acquire.valid := fire(acquire.ready)
      tw.valid := fire(tw.ready)
      when (fire(null)) {
        state := s_idle
      }
    }
  }

  //--------------------------------------------------------------------\\
  // request
  //--------------------------------------------------------------------\\
  private val grant = dmem.d

  io.scalar.resp.valid := grant.valid
  grant.ready := io.scalar.resp.ready
  tr.valid := io.scalar.resp.fire
  tr.bits := grant.bits.source(log2Up(nSMU)-1,0)

  val resp_mt = DecodedMemType(tr.record.mt)
  val resp_shift = Cat(tr.record.offset, UInt(0,3))
  val resp_data = grant.bits.data >> resp_shift

  val resp_mask = FillInterleaved(8, mask(resp_mt))
  val resp_sign = Mux1H(mts(resp_mt).zipWithIndex.map { case (s, i) =>
    val w = 1 << (i + 3)
    (s, resp_data(w - 1))
  })
  val resp_extend = Fill(regLen-8, resp_mt.signed && resp_sign)

  io.scalar.resp.bits.store := grant.bits.opcode === TLMessages.AccessAck
  io.scalar.resp.bits.tag := tr.record.tag
  io.scalar.resp.bits.data := Cat(
    (resp_data(regLen-1, 8) & resp_mask) |
      (resp_extend & (~resp_mask)),
    resp_data(7, 0))

  //Tie off unused channels
  dmem.b.ready := true.B
  dmem.c.valid := false.B
  dmem.e.valid := false.B
}

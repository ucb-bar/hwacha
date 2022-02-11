package hwacha

import Chisel._
import freechips.rocketchip.config._

class VMUStoreCtrl(implicit p: Parameters) extends VMUBundle()(p) {
  val mode = new Bundle {
    val unit = Bool()
  }
  val base = UInt(width = tlByteAddrBits)
  val mt = new DecodedMemType
}

class VMUStoreMeta(implicit p: Parameters) extends VMUMetaStore with VMUMetaCount {
  val offset = UInt(width = tlByteAddrBits)
}
class VMUStoreIO(implicit p: Parameters) extends VSDQIO()(p) {
  val meta = new VMUStoreMeta().asInput

}

class SBox(implicit p: Parameters) extends VMUModule()(p) {
  val io = new Bundle {
    val ctrl = Valid(new VMUStoreCtrl).flip
    val lane = new VSDQIO().flip
    val mem = new VMUStoreIO
  }

  private val op = io.ctrl.bits
  private val mts = Seq(op.mt.d, op.mt.w, op.mt.h, op.mt.b)
  private val meta = io.mem.meta

  val vsdq = Module(new Queue(io.lane.bits, nVSDQ))
  vsdq.suggestName("vsdqInst")
  vsdq.io.enq <> io.lane

  /* Byte mode: Ignore MSB */
  private def saturate[T <: Bits](x: T) =
    Cat(x(tlByteAddrBits-1) & (!op.mt.b), x(tlByteAddrBits-2, 0))

  val lead = Reg(init = Bool(true))
  val index = Reg(init = UInt(0, tlByteAddrBits))
  val index_step = Cat(mts)
  val index_next = saturate(index + index_step)
  val index_end = (index_next === UInt(0))

  val offset_u = saturate(op.base)
  val offset = Mux(op.mode.unit, offset_u, meta.offset)

  /* NOTE: Due to read bandwidth constraints, only the lower half of a
     VSDQ entry (width tlDataBits/2) is populated for byte operations
     ("byte mode"). */
  private val bbyte = tlDataBits >> 1
  /* Byte mode: Number of relevant data bits in a partial VSDQ entry to
     save into the hold register */
  private val bpart = bbyte - 8

  val hold = Reg(Bits(width = tlDataBits - 16))
  when (vsdq.io.deq.fire) {
   /* Byte mode: Align relevant data bits to the upper end of the
      hold register */
    hold := Cat(Mux(op.mt.b,
        vsdq.io.deq.bits.data(bpart+7, 8),
        vsdq.io.deq.bits.data(tlDataBits-1, tlDataBits-bpart)),
      /* Lower bits ignored during byte mode */
      vsdq.io.deq.bits.data(tlDataBits-bpart-1, 16))
  }

  val data_head = Cat(vsdq.io.deq.bits.data, hold, Bits(0, 8) /* pad */)
  /* Equivalence: index + (UInt(tlDataBytes) - 1 - offset) */
  val shift = Cat(UInt(0,1), index) + (~offset)

  val data_align = (data_head >> Cat(shift, UInt(0,3)))(tlDataBits-1, 0)
  val data_hi = data_align(tlDataBits-1, bbyte)
  val data_lo = data_align(bbyte-1, 0)
  val data = Cat(Mux(op.mode.unit && op.mt.b, data_lo, data_hi), data_lo)
  io.mem.bits.data := data

  val bcnt = meta.ecnt.decode() << op.mt.shift()
  assert(!op.mt.b || (bcnt <= UInt(tlDataBytes >> 1)),
    "SBox: bcnt exceeds limit")
  val truncate = (bcnt <= offset_u) && !lead && meta.last

  val vsdq_deq = meta.vsdq &&
    Mux(op.mode.unit, !truncate, index_end || meta.last)
  val vsdq_valid = !meta.vsdq || (op.mode.unit && truncate) ||
    vsdq.io.deq.valid

  private def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(vsdq_valid, io.mem.ready, io.ctrl.valid)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  vsdq.io.deq.ready := fire(vsdq_valid, vsdq_deq)
  io.mem.valid := fire(io.mem.ready)

  when (fire(null)) {
    index := Mux(op.mode.unit || meta.last, UInt(0), index_next)
    lead := meta.last
  }
}

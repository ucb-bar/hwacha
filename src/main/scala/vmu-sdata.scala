package hwacha

import Chisel._

class VMUStoreIO extends Bundle {
  val meta = Decoupled(new VMUStoreMetaEntry).flip
  val store = Decoupled(new VMUStoreData)
}

class SBox extends VMUModule {
  val io = new Bundle {
    val issue = new VMUIssueOpIO().flip
    val xcpt = new XCPTIO().flip

    val lane = new VSDQIO().flip
    val evac = new VSDQIO().flip
    val mbox = new VMUStoreIO
  }

  val op = io.issue.op
  val meta = io.mbox.meta.bits
  val packed = op.unit && !op.fn.indexed
  private val mt = Seq(op.mt.b, op.mt.h, op.mt.w, op.mt.d)

  val drain = io.xcpt.prop.vmu.drain

  val vsdq = Module(new Queue(Bits(width = tlDataBits), confvmu.nvsdq))
  vsdq.io.enq.bits := Mux(drain, io.evac.bits, io.lane.bits)
  vsdq.io.enq.valid := Mux(drain, io.evac.valid, io.lane.valid)
  io.evac.ready := vsdq.io.enq.ready && drain
  io.lane.ready := vsdq.io.enq.ready && !drain

  val hold = Reg(Bits(width = tlDataBits - 8))
  when (vsdq.io.deq.fire()) {
    hold := vsdq.io.deq.bits(tlDataBits-1, 8)
  }

  val index = Reg(Bits(width = tlByteAddrBits))
  val index_mask = !(packed || drain || meta.first)
  val index_real = index & Fill(tlByteAddrBits, index_mask)
  val index_step = Cat(mt.reverse)
  val index_next = index_real + index_step
  when (io.mbox.store.fire()) {
    index := index_next
  }

  val offset_base = op.addr.base(tlByteAddrBits-1,0)
  val offset = Mux(packed && !drain, offset_base, meta.offset)

  val funnel = Module(new FunnelShifter(Bits(width = 8), tlDataBytes))
  funnel.io.in0 := Vec((0 until tlDataBits by 8).map(i => vsdq.io.deq.bits(i+7,i)))
  funnel.io.in1 := Vec(Bits(0) +: (0 until tlDataBits-8 by 8).map(i => hold(i+7,i)))
  funnel.io.shift := offset.zext - index_real.zext

  // Map (ecnt == 0) to tlDataBytes
  val mask_lut = Vec(UInt(tlDataBytes) +:
    (1 until tlDataBytes).map(i => UInt((1 << i) - 1)))
  val mask_elt = mask_lut(meta.ecnt)
  val mask_shl = mask_elt << meta.eskip

  val mask_mt = Seq(
    op.mt.b && !drain, op.mt.h && !drain,
    op.mt.w && !drain, op.mt.d || drain)
  val mask = Mux1H(mask_mt, (0 until mt.size).map(i => /* Expand */
    FillInterleaved(1 << i, mask_shl((tlDataBytes >> i)-1, 0))))

  val ecnt = Mux(meta.ecnt === UInt(0), UInt(tlDataBytes), meta.ecnt)
  val eoff = offset_base >> op.mt.shamt()
  val truncate = (ecnt <= eoff) && !meta.first

  val data_valid = vsdq.io.deq.valid || (packed && truncate && !drain)
  val dequeue = Mux(packed, !truncate, (index_next === UInt(0)) || meta.last) || drain

  vsdq.io.deq.ready := io.mbox.store.ready && io.mbox.meta.valid && dequeue
  io.mbox.meta.ready := io.mbox.store.ready && data_valid
  io.mbox.store.valid := io.mbox.meta.valid && data_valid

  io.mbox.store.bits.data := Cat(funnel.io.out.reverse)
  io.mbox.store.bits.mask := mask

  val busy = Reg(init = Bool(false))
  when (io.issue.fire) {
    busy := Bool(true)
  } .elsewhen(io.mbox.meta.fire() && meta.last) {
    busy := Bool(false)
  }
  io.issue.busy := busy
}

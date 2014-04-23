package hwacha

import Chisel._
import Constants._

class MetadataBuffer(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val wr = new VMDBIO().flip
    val rd = new Bundle {
      val tag = Valid(Bits(width = conf.vmu.sz_tag)).flip
      val info = new VMUMetadata().asOutput
    }
  }

  val busy = Reg(init = Bits(0, conf.vmu.nvlmb))
  val data = Mem(new VMUMetadata, conf.vmu.nvlmb)

  io.wr.tag := PriorityEncoder(~busy)
  io.wr.info.ready := !((0 until conf.vmu.nvlmb).map(i => busy(i)).reduce(_&&_))

  val busy_mask_rd = UIntToOH(io.rd.tag.bits) & Fill(conf.vmu.nvlmb, io.rd.tag.valid)
  val busy_mask_wr = UIntToOH(io.wr.tag) & Fill(conf.vmu.nvlmb, io.wr.info.fire())

  busy := (busy & (~busy_mask_rd)) | busy_mask_wr
  when (io.wr.info.fire()) {
    data(io.wr.tag) := io.wr.info.bits
  }

  io.rd.info:= data(io.rd.tag.bits)
}

class LoadDataUnit(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val vmdb = new VMDBIO().flip
    val memif = new VLDQMemIO().flip
    val lane = new VLDQIO
  }

  val vldq = Module(new Queue(new VLDQEntry, conf.vmu.nvldq))
  val vmdb = Module(new MetadataBuffer)

  // Count available space in VLDQ
  val count = Reg(init = UInt(conf.vmu.nvldq))
  val cntr_dec = io.vmdb.info.fire() // Account for in-flight loads
  val cntr_inc = io.lane.fire()
  when (cntr_inc && !cntr_dec) {
    count := count + UInt(1)
  }
  when (!cntr_inc && cntr_dec) {
    count := count - UInt(1)
  }
  io.memif.stall := (count === UInt(0))


  assert(!vldq.io.enq.valid || vldq.io.enq.ready,
    "VLDQ should never be unable to accept a valid response")

  vldq.io.enq.valid := io.memif.resp.valid
  vldq.io.enq.bits.data := io.memif.resp.bits.data
  vldq.io.enq.bits.meta := vmdb.io.rd.info

  vmdb.io.wr <> io.vmdb
  vmdb.io.rd.tag.bits := io.memif.resp.bits.tag
  vmdb.io.rd.tag.valid := vldq.io.enq.fire()

  io.lane <> vldq.io.deq
}

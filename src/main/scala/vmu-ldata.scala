package hwacha

import Chisel._
import Constants._
import uncore.constants.MemoryOpConstants._

class MetadataBuffer extends HwachaModule
{
  val io = new Bundle {
    val wr = new VMDBIO().flip
    val rd = new Bundle {
      val tag = Valid(Bits(width = confvmu.sz_tag)).flip
      val info = new VMUMetadataInternal().asOutput
    }
  }

  val busy = Reg(init = Bits(0, confvmu.nvlmb))
  val data = Mem(new VMUMetadataInternal, confvmu.nvlmb)

  io.wr.tag := PriorityEncoder(~busy)
  io.wr.info.ready := !((0 until confvmu.nvlmb).map(i => busy(i)).reduce(_&&_))

  val busy_mask_rd = UIntToOH(io.rd.tag.bits) & Fill(confvmu.nvlmb, io.rd.tag.valid)
  val busy_mask_wr = UIntToOH(io.wr.tag) & Fill(confvmu.nvlmb, io.wr.info.fire())

  busy := (busy & (~busy_mask_rd)) | busy_mask_wr
  when (io.wr.info.fire()) {
    data(io.wr.tag) := io.wr.info.bits
  }

  io.rd.info:= data(io.rd.tag.bits)
}

class LoadDataUnit extends HwachaModule
{
  val io = new Bundle {
    val vmdb = new VMDBIO().flip
    val memif = new VLDQMemIO().flip
    val lane = new VLDQIO
  }

  val vldq = Module(new Queue(new VLDQEntry, confvmu.nvldq))
  val vmdb = Module(new MetadataBuffer)

  assert(!vldq.io.enq.valid || vldq.io.enq.ready,
    "VLDQ should never be unable to accept a valid response")

  val mt_b = (vmdb.io.rd.info.typ === MT_B)
  val mt_h = (vmdb.io.rd.info.typ === MT_H)
  val mt_w = (vmdb.io.rd.info.typ === MT_W)
  val mt_d = (vmdb.io.rd.info.typ === MT_D)
  val mt_bu = (vmdb.io.rd.info.typ === MT_BU)
  val mt_hu = (vmdb.io.rd.info.typ === MT_HU)
  val mt_wu = (vmdb.io.rd.info.typ === MT_WU)

  val mt_sel = Vec(mt_b, mt_h || mt_hu, mt_w || mt_wu, mt_d)
  val mt_signext = (mt_b || mt_h || mt_w)

  val resp_shift = Cat(vmdb.io.rd.info.offset, Bits(0, 3)).toUInt
  val resp_data = io.memif.bits.data >> resp_shift

  val resp_mask = FillInterleaved(8,
    Mux1H(mt_sel, Vec(Bits(0x00), Bits(0x01), Bits(0x07), Bits(0x7f))))
  val resp_extend = Fill(confvmu.sz_data-8, mt_signext &&
    Mux1H(mt_sel, Vec(Seq(7, 15, 31, 63).map(resp_data(_)))))

  io.memif.ready := vldq.io.enq.ready
  vldq.io.enq.valid := io.memif.valid
  vldq.io.enq.bits.data := Cat(
    (resp_data(confvmu.sz_data-1,8) & resp_mask) | (resp_extend & ~resp_mask),
    resp_data(7,0))
  vldq.io.enq.bits.meta := vmdb.io.rd.info

  vmdb.io.wr <> io.vmdb
  vmdb.io.rd.tag.bits := io.memif.bits.tag
  vmdb.io.rd.tag.valid := vldq.io.enq.fire()

  io.lane <> vldq.io.deq
}

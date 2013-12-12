package hwacha

import Chisel._
import Node._
import scala.math._

class io_qcnt(w: Int) extends Bundle
{
  val inc = Bool(INPUT)
  val dec = Bool(INPUT)
  val qcnt = UInt(INPUT, w)
  val watermark = Bool(OUTPUT)
  val qcnt2 = UInt(INPUT, w)
  val watermark2 = Bool(OUTPUT)
  val full = Bool(OUTPUT)
  val empty = Bool(OUTPUT)
}

class qcnt(reset_cnt: Int, max_cnt: Int, resetSignal: Bool = null) extends Module(_reset = resetSignal)
{
  val size = log2Down(max_cnt) + 1

  val io = new io_qcnt(size)
  val count = Reg(init=UInt(reset_cnt, size))
  val next_count = UInt(width = size)

  next_count := count
  when (io.inc ^ io.dec) {
    when (io.inc) {next_count := count + UInt(1)}
    when (!io.inc) {next_count := count - UInt(1)}
  }

  count := next_count

  // we need to look at what's in the queue on the next cycle
  io.watermark := count >= io.qcnt
  io.watermark2 := count >= io.qcnt2

  io.full := (count === UInt(reset_cnt,size))
  io.empty := (count === UInt(0,size))
}

class VLDQEnqBundle(DATA_SIZE: Int, MASK_SIZE: Int, TAG_SIZE: Int) extends Bundle
{
  val data = Bits(width = DATA_SIZE)
  val mask = Bits(width = MASK_SIZE)
  val rtag = UInt(width = TAG_SIZE)
  override def clone = new VLDQEnqBundle(DATA_SIZE, MASK_SIZE, TAG_SIZE).asInstanceOf[this.type]
}

class VLDQ(DATA_SIZE: Int, SUBWORDS: Int, TAG_ENTRIES: Int, MAX_QCNT: Int) extends Module
{
  val TAG_SIZE = log2Up(TAG_ENTRIES)

  val io = new Bundle {
    val deq_rtag = Decoupled(Bits(width = TAG_SIZE))
    val deq_data = Decoupled(Bits(width = DATA_SIZE))
    val enq = Valid(new VLDQEnqBundle(DATA_SIZE, SUBWORDS, TAG_SIZE)).flip

    val qcnt = UInt(INPUT, TAG_SIZE)
    val watermark = Bool(OUTPUT)
  }

  val read_ptr = Reg(init=UInt(0, TAG_SIZE))
  val read_ptr1 = read_ptr + UInt(1)
  val read_ptr2 = read_ptr + UInt(2)
  val write_ptr = Reg(init=UInt(0, TAG_SIZE))
  val write_ptr1 = write_ptr + UInt(1)
  val full = Reg(init=Bool(false))

  val bump_rptr = io.deq_data.fire()
  val bump_wptr = io.deq_rtag.fire()

  when (bump_wptr) { write_ptr := write_ptr1 }
  when (bump_rptr) { read_ptr := read_ptr1 }

  // TODO: parameterize write mask expansion
  val mem_mask = Cat(
    Fill(17, io.enq.bits.mask(3)),
    Fill(16, io.enq.bits.mask(2)),
    Fill(17, io.enq.bits.mask(1)),
    Fill(16, io.enq.bits.mask(0)))
  val mem_data = Mem(io.enq.bits.data.clone, TAG_ENTRIES, seqRead = true)
  when (io.enq.valid) {
    mem_data.write(io.enq.bits.rtag, io.enq.bits.data, mem_mask)
  }

  val vb_array = Vec.fill(SUBWORDS){ Reg(init=Bits(0, TAG_ENTRIES)) }
  val vb_update_read = Mux(bump_rptr, ~(Bits(1) << read_ptr), Fill(TAG_ENTRIES, Bits(1)))
  val vb_update_write = Vec.fill(SUBWORDS){ Bits(width = TAG_ENTRIES) }
  for (i <- 0 until SUBWORDS) {
    vb_update_write(i) := (io.enq.bits.mask(i) & io.enq.valid) << io.enq.bits.rtag
    vb_array(i) := (vb_array(i) & vb_update_read) | vb_update_write(i)
  }

  val mem_addr = Reg(Bits())
  mem_addr := Mux(bump_rptr, read_ptr2, read_ptr1)

  val deq_data_val = Reg(init=Bool(false))
  val deq_data = Reg(Bits())
  val match_rtag = Mux(bump_rptr, read_ptr1, read_ptr)

  var vb_fold = Bool(true)
  for (i <- 0 until SUBWORDS) {
    vb_fold = vb_fold & vb_array(i)(match_rtag)
  }
  deq_data_val := vb_fold
  when (bump_rptr) {
    deq_data := mem_data(mem_addr)
  }

  when (io.enq.valid && io.enq.bits.rtag === match_rtag) {
    deq_data := io.enq.bits.data
  }

  io.deq_data.valid := deq_data_val
  io.deq_data.bits := deq_data

  io.deq_rtag.valid := !full
  io.deq_rtag.bits := write_ptr

  full := Mux(bump_wptr && !bump_rptr && (write_ptr1 === read_ptr), Bool(true),
          Mux(bump_rptr && full, Bool(false),
              full))

  // Logic for watermark
  val shifted_vb_array = Vec.fill(SUBWORDS){ Reg(init=Bits(0, TAG_ENTRIES)) }
  val shifted_write_ptr =
    Mux(read_ptr <= io.enq.bits.rtag, io.enq.bits.rtag - read_ptr,
        UInt(TAG_ENTRIES) - read_ptr + io.enq.bits.rtag)(TAG_SIZE-1,0)

  val shifted_vb_update_write = Vec.fill(SUBWORDS){ Bits(width = TAG_ENTRIES) }
  val shifted_vb_array_next   = Vec.fill(SUBWORDS){ Bits(width = TAG_ENTRIES) }
  for (i <- 0 until SUBWORDS) {
    shifted_vb_update_write(i) := (io.enq.bits.mask(i) & io.enq.valid) << shifted_write_ptr
    shifted_vb_array_next(i) := shifted_vb_array(i) | shifted_vb_update_write(i)
    shifted_vb_array(i) := Mux(bump_rptr,
      (shifted_vb_array_next(i) >> UInt(1)), shifted_vb_array_next(i))
  }

  // a limited version of leading count ones
  // maximum cnt is defined by MAX_QCNT
  // TODO: correctly handle partially packed VLDQ entries in which
  //       the valid bits of intentionally absent subwords can be
  //       disregarded (e.g., in the case of odd-length vectors)
  var sel = Bool(true)
  for (i <- 0 until SUBWORDS) {
    sel = sel && shifted_vb_array(i)(0)
  }
  var locnt = UInt(0, TAG_SIZE)
  for (i <- 0 until MAX_QCNT) {
    locnt = Mux(sel, UInt(i+1), locnt)
    for (j <- 0 until SUBWORDS) {
      sel = sel && shifted_vb_array(j)(i+1)
    }
  }
  io.watermark := locnt >= io.qcnt
}

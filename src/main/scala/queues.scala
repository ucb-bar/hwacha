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

class LookAheadPortIO(sz: Int) extends Bundle
{
  val reserve = new Bundle {
    val valid = Bool(OUTPUT)
    val cnt = UInt(OUTPUT, sz)
  }
  val available = new Bundle {
    val cnt = UInt(OUTPUT, sz)
    val check = Bool(INPUT)
  }
}

class LookAheadCounter(reset_cnt: Int, max_cnt: Int, resetSignal: Bool = null) extends Module(_reset = resetSignal)
{
  val sz = log2Down(max_cnt)+1
  val io = new Bundle {
    val la = new LookAheadPortIO(sz).flip
    val inc = Bool(INPUT)
    val dec = Bool(INPUT)
  }

  val reg_count = Reg(init = UInt(reset_cnt, sz))

  when (io.la.reserve.valid) {
    reg_count := reg_count - io.la.reserve.cnt
    when (io.inc ^ io.dec) {
      when (io.inc) { reg_count := reg_count - io.la.reserve.cnt + UInt(1) }
      when (io.dec) { reg_count := reg_count - io.la.reserve.cnt - UInt(1) }
    }
  }
  .otherwise {
    when (io.inc ^ io.dec) {
      when (io.inc) { reg_count := reg_count + UInt(1) }
      when (io.dec) { reg_count := reg_count - UInt(1) }
    }
  }

  io.la.available.check := reg_count >= io.la.available.cnt
}

class VLDQEnqOp(DATA_SIZE: Int, TAG_SIZE: Int) extends Bundle
{
  val data = Bits(width = DATA_SIZE)
  val rtag = UInt(width = TAG_SIZE)
  override def clone = new VLDQEnqOp(DATA_SIZE, TAG_SIZE).asInstanceOf[this.type]
}

class VLDQ(DATA_SIZE: Int, TAG_ENTRIES: Int, MAX_QCNT: Int) extends Module
{
  val TAG_SIZE = log2Up(TAG_ENTRIES)

  val io = new Bundle {
    val deq_rtag = Decoupled(Bits(width = TAG_SIZE))
    val deq_data = Decoupled(Bits(width = DATA_SIZE))
    val enq = Valid(new VLDQEnqOp(DATA_SIZE, TAG_SIZE)).flip
    val la = new LookAheadPortIO(log2Down(TAG_ENTRIES)+1).flip
  }

  val read_ptr = Reg(init=UInt(0, TAG_SIZE))
  val read_ptr1 = read_ptr + UInt(1)
  val write_ptr = Reg(init=UInt(0, TAG_SIZE))
  val write_ptr1 = write_ptr + UInt(1)
  val reserve_ptr = Reg(init=UInt(0, TAG_SIZE))
  val full = Reg(init=Bool(false))

  val bump_rptr = io.deq_data.fire()
  val bump_wptr = io.deq_rtag.fire()

  val mem_data = Mem(io.enq.bits.data.clone, TAG_ENTRIES, seqRead = true)
  val mem_addr = Reg(Bits())
  mem_addr := Mux(bump_rptr, read_ptr1, read_ptr)
  when (io.enq.valid) { mem_data(io.enq.bits.rtag) := io.enq.bits.data }

  val vb_array = Reg(init=Bits(0, TAG_ENTRIES))
  val vb_update_read = Mux(bump_rptr, ~(Bits(1) << read_ptr), Fill(TAG_ENTRIES, Bits(1)))
  val vb_update_write = Mux(io.enq.valid, (Bits(1) << io.enq.bits.rtag), Bits(0, TAG_ENTRIES))
  vb_array := (vb_array & vb_update_read) | vb_update_write

  val deq_data_val = Reg(init=Bool(false))

  deq_data_val := vb_array(read_ptr)

  when (bump_wptr) { write_ptr := write_ptr1 }
  when (bump_rptr) {
    deq_data_val := vb_array(read_ptr1)
    read_ptr := read_ptr1
  }

  io.deq_rtag.valid := !full
  io.deq_rtag.bits := write_ptr

  full := Mux(bump_wptr && !bump_rptr && (write_ptr1 === read_ptr), Bool(true),
          Mux(bump_rptr && full, Bool(false),
              full))

  io.deq_data.valid := deq_data_val
  io.deq_data.bits := mem_data(mem_addr)

  // Logic for watermark
  val shifted_vb_array = Reg(init=Bits(0, TAG_ENTRIES))
  val shifted_write_ptr =
    Mux(reserve_ptr <= io.enq.bits.rtag, io.enq.bits.rtag - reserve_ptr,
        UInt(TAG_ENTRIES) - reserve_ptr + io.enq.bits.rtag)(TAG_SIZE-1,0)

  val shifted_vb_update_write =
    Mux(io.enq.valid, (Bits(1) << shifted_write_ptr),
        Bits(0, TAG_ENTRIES))

  var mux_tree = shifted_vb_array | shifted_vb_update_write
  for (i <- 1 until MAX_QCNT)
    mux_tree = Mux(io.la.reserve.cnt === UInt(i), mux_tree >> UInt(i), mux_tree)
  shifted_vb_array := Mux(io.la.reserve.valid, mux_tree, shifted_vb_array | shifted_vb_update_write)

  when (io.la.reserve.valid) {
    reserve_ptr := reserve_ptr + io.la.reserve.cnt
  }

  // a limited version of leading count ones
  // maximum cnt is defined by MAX_QCNT
  var sel = shifted_vb_array(0)
  var locnt = UInt(0, TAG_SIZE)
  for (i <- 0 until MAX_QCNT) {
    locnt = Mux(sel, UInt(i+1), locnt)
    sel = sel & shifted_vb_array(i+1)
  }

  io.la.available.check := locnt >= io.la.available.cnt
}

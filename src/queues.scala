package hwacha

import Chisel._
import Node._
import scala.math._

class io_qcnt(w: Int) extends Bundle
{
  val inc = Bool(INPUT)
  val dec = Bool(INPUT)
  val qcnt = UFix(INPUT, w)
  val watermark = Bool(OUTPUT)
  val qcnt2 = UFix(INPUT, w)
  val watermark2 = Bool(OUTPUT)
  val full = Bool(OUTPUT)
  val empty = Bool(OUTPUT)
}

class qcnt(reset_cnt: Int, max_cnt: Int, resetSignal: Bool = null) extends Component(resetSignal)
{
  val size = log2Down(max_cnt) + 1

  val io = new io_qcnt(size)
  val count = Reg(resetVal = UFix(reset_cnt, size))
  val next_count = UFix(width = size)

  next_count := count
  when (io.inc ^ io.dec) {
    when (io.inc) {next_count := count + UFix(1)}
    when (!io.inc) {next_count := count - UFix(1)}
  }

  count := next_count

  // we need to look at what's in the queue on the next cycle
  io.watermark := count >= io.qcnt
  io.watermark2 := count >= io.qcnt2

  io.full := (count === UFix(reset_cnt,size))
  io.empty := (count === UFix(0,size))
}

class VLDQEnqBundle(DATA_SIZE: Int, TAG_SIZE: Int) extends Bundle
{
  val data = Bits(width = DATA_SIZE)
  val rtag = UFix(width = TAG_SIZE)
}

class VLDQ(DATA_SIZE: Int, TAG_ENTRIES: Int, MAX_QCNT: Int) extends Component
{
  val TAG_SIZE = log2Up(TAG_ENTRIES)

  val io = new Bundle {
    val deq_rtag = new FIFOIO()(Bits(width = TAG_SIZE))
    val deq_data = new FIFOIO()(Bits(width = DATA_SIZE))
    val enq = new PipeIO()(new VLDQEnqBundle(DATA_SIZE, TAG_SIZE)).flip

    val qcnt = UFix(INPUT, TAG_SIZE)
    val watermark = Bool(OUTPUT)
  }

  val read_ptr = Reg(resetVal = UFix(0, TAG_SIZE))
  val read_ptr1 = read_ptr + UFix(1)
  val read_ptr2 = read_ptr + UFix(2)
  val write_ptr = Reg(resetVal = UFix(0, TAG_SIZE))
  val write_ptr1 = write_ptr + UFix(1)
  val full = Reg(resetVal = Bool(false))

  val bump_rptr = io.deq_data.fire()
  val bump_wptr = io.deq_rtag.fire()

  when (bump_wptr) { write_ptr := write_ptr1 }
  when (bump_rptr) { read_ptr := read_ptr1 }

  val mem_data = Mem(TAG_ENTRIES, seqRead = true) { io.enq.bits.data.clone }
  when (io.enq.valid) { mem_data(io.enq.bits.rtag) := io.enq.bits.data }

  val vb_array = Reg(resetVal = Bits(0, TAG_ENTRIES))
  val vb_update_read = Mux(bump_rptr, ~(Bits(1) << read_ptr), Fill(TAG_ENTRIES, Bits(1)))
  val vb_update_write = Mux(io.enq.valid, (Bits(1) << io.enq.bits.rtag), Bits(0, TAG_ENTRIES))
  vb_array := (vb_array & vb_update_read) | vb_update_write

  val mem_addr = Reg() { Bits() }
  mem_addr := Mux(bump_rptr, read_ptr2, read_ptr1)

  val deq_data_val = Reg(resetVal = Bool(false))
  val deq_data = Reg(){ Bits() }
  val match_rtag = Bits()

  deq_data_val := vb_array(read_ptr)
  match_rtag := read_ptr
  when (bump_rptr) {
    deq_data_val := vb_array(read_ptr1)
    deq_data := mem_data(mem_addr)
    match_rtag := read_ptr1
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
  val shifted_vb_array = Reg(resetVal = Bits(0, TAG_ENTRIES))
  val shifted_write_ptr =
    Mux(read_ptr <= io.enq.bits.rtag, io.enq.bits.rtag - read_ptr,
        UFix(TAG_ENTRIES) - read_ptr + io.enq.bits.rtag)(TAG_SIZE-1,0)

  val shifted_vb_update_write =
    Mux(io.enq.valid, (Bits(1) << shifted_write_ptr),
        Bits(0, TAG_ENTRIES))

  shifted_vb_array :=
    Mux(bump_rptr, ((shifted_vb_array | shifted_vb_update_write) >> UFix(1)),
        (shifted_vb_array | shifted_vb_update_write))

  // a limited version of leading count ones
  // maximum cnt is defined by MAX_QCNT
  var sel = shifted_vb_array(0)
  var locnt = UFix(0, TAG_SIZE)
  for (i <- 0 until MAX_QCNT) {
    locnt = Mux(sel, UFix(i+1), locnt)
    sel = sel & shifted_vb_array(i+1)
  }

  io.watermark := locnt >= io.qcnt
}

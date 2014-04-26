package hwacha

import Chisel._
import Node._
import Constants._
import scala.collection.mutable.ArrayBuffer
import scala.math._

object Compaction extends Compaction
{
  def repack_float_d(n: Bits*) = Cat(Bits(1,1), n(0))
  def repack_float_s(n: Bits*) = Cat(n(1)(32), n(0)(32), n(1)(31,0), n(0)(31,0))
  def repack_float_h(n: Bits*) = Cat(Bits("b11",2), n(3), n(2), n(1), n(0))

  def pack_float_d(n: Bits, i: Int): Bits = i match {
    case 0 => (Bits(1,1) ## n(64,0))
    case _ => Bits(0, 66)
  }
  def pack_float_s(n: Bits, i: Int): Bits = i match {
    case 0 => Cat(Bits(1,1), n(32), Bits("hFFFFFFFF",32), n(31,0))
    case 1 => Cat(n(32), Bits(1,1), n(31,0), Bits("hFFFFFFFF",32))
    case _ => Bits(0)
  }
  def pack_float_h(n: Bits, i: Int): Bits = i match {
    case 0 => Cat(Bits("h3FFFFFFFFFFFF",50), n(15,0))
    case 1 => Cat(Bits("h3FFFFFFFF",34), n(15,0), Bits("hFFFF",16))
    case 2 => Cat(Bits("h3FFFF",18), n(15,0), Bits("hFFFFFFFF",32))
    case 3 => Cat(Bits("b11",2), n(15,0), Bits("hFFFFFFFFFFFF",48))
    case _ => Bits(0)
  }

  def unpack_float_d(n: Bits, i: Int): Bits = i match {
    case 0 => (n(64,0))
    case _ => Bits(0)
  }
  def unpack_float_s(n: Bits, i: Int): Bits = i match {
    case 0 => (n(64) ## n(31,0))
    case 1 => (n(65) ## n(63,32))
    case _ => Bits(0)
  }
  def unpack_float_h(n: Bits, i: Int): Bits = i match {
    case 0 => n(15,0)
    case 1 => n(31,16)
    case 2 => n(47,32)
    case 3 => n(63,48)
    case _ => Bits(0)
  }

  def expand_mask(m: Bits) = Cat(
    m(3) & m(2),
    m(1) & m(0),
    Fill(16, m(3)),
    Fill(16, m(2)),
    Fill(16, m(1)),
    Fill(16, m(0)))

  // Pack the given element into the given subword position
  // and propagate the MSB throughout the higher order bits
  private def pack(n: Bits, i: Int, w: Int): Bits = {
    val data = Cat(Fill(SZ_DATA-((i+1)*w), n(w)), n(w-1,0))
    if (i == 0) data else Cat(data, Bits(0, i*w))
  }
  def pack_d(n: Bits, i: Int): Bits = pack(n, i, SZ_XD)
  def pack_w(n: Bits, i: Int): Bits = pack(n, i, SZ_XW)
  def pack_h(n: Bits, i: Int): Bits = pack(n, i, SZ_XH)
  def pack_b(n: Bits, i: Int): Bits = pack(n, i, SZ_XB)
}

trait Compaction
{
  def repack_float_d(n: Bits*): Bits
  def repack_float_s(n: Bits*): Bits
  def repack_float_h(n: Bits*): Bits
  def pack_float_d(n: Bits, i: Int): Bits
  def pack_float_s(n: Bits, i: Int): Bits
  def pack_float_h(n: Bits, i: Int): Bits
  def unpack_float_d(n: Bits, i: Int): Bits
  def unpack_float_s(n: Bits, i: Int): Bits
  def unpack_float_h(n: Bits, i: Int): Bits

  def pack_d(n: Bits, i: Int): Bits
  def pack_w(n: Bits, i: Int): Bits
  def pack_h(n: Bits, i: Int): Bits
  def pack_b(n: Bits, i: Int): Bits
}

object PtrIncr
{
  // runtime incr
  def apply(ptr: UInt, incr: UInt, bcnt: UInt)(implicit conf: HwachaConfiguration) = {
    val rom_nptr_lookup = (
      for { aptr <- 0 to conf.ptr_incr_max; bcnt <- conf.nbanks to conf.nbanks }
        yield (Cat(UInt(aptr, conf.ptr_incr_sz), UInt(bcnt, SZ_BCNT)), UInt((aptr + bcnt) % bcnt, SZ_BPTR))
      ).toArray
    val aptr = UInt(0, conf.ptr_incr_sz) + ptr + incr
    Lookup(Cat(aptr, bcnt), UInt(0, SZ_BPTR), rom_nptr_lookup)
  }

  // fixed incr
  def apply(ptr: UInt, incr: Int, bcnt: UInt)(implicit conf: HwachaConfiguration) = {
    require(incr < conf.nbanks)
    val aptr = ptr + UInt(incr, SZ_BPTR1)
    val aptr_mbcnt = aptr - bcnt
    Mux(aptr < bcnt, aptr, aptr_mbcnt)(SZ_BPTR-1, 0)
  }
}

object Match
{
  def apply(x: Bits, IOs: Bits*) =
  {
    val ioList = IOs.toList
    var offset = 0
    for (io <- IOs.toList.reverse)
    {
      io := x(offset+io.width-1, offset)
      offset += io.width
    }
  }
}

class MaskStall[T <: Data](data: => T) extends Module
{
  val io = new Bundle()
  {
    val input = Decoupled(data).flip
    val output = Decoupled(data)
    val stall = Bool(INPUT)
  }

  io.output.valid := io.input.valid && !io.stall
  io.output.bits := io.input.bits
  io.input.ready := io.output.ready && !io.stall
}

object MaskStall
{
  def apply[T <: Data](deq: DecoupledIO[T], stall: Bool) =
  {
    val ms = Module(new MaskStall(deq.bits.clone))
    ms.io.input <> deq
    ms.io.stall := stall
    ms.io.output
  }
}

class QCounter(reset_cnt: Int, max_cnt: Int, resetSignal: Bool = null) extends Module(_reset = resetSignal)
{
  val sz = log2Down(max_cnt)+1
  val io = new Bundle {
    val inc = Bool(INPUT)
    val dec = Bool(INPUT)
    val qcnt = UInt(INPUT, sz)
    val watermark = Bool(OUTPUT)
    val full = Bool(OUTPUT)
    val empty = Bool(OUTPUT)
  }

  val count = Reg(init = UInt(reset_cnt, sz))

  when (io.inc ^ io.dec) {
    when (io.inc) { count := count + UInt(1) }
    when (io.dec) { count := count - UInt(1) }
  }

  io.watermark := count >= io.qcnt
  io.full := count === UInt(max_cnt)
  io.empty := count === UInt(0)
}

class LookAheadPortIO(sz: Int) extends Bundle
{
  val cnt = UInt(OUTPUT, sz)
  val reserve = Bool(OUTPUT)
  val available = Bool(INPUT)
}

class CounterPortIO(sz: Int) extends Bundle
{
  val cnt = UInt(OUTPUT, sz)
  val update = Bool(OUTPUT)
}

class LookAheadCounter(reset_cnt: Int, max_cnt: Int, resetSignal: Bool = null) extends Module(_reset = resetSignal)
{
  val sz = log2Down(max_cnt)+1
  val io = new Bundle {
    val la = new LookAheadPortIO(sz).flip
    val inc = new CounterPortIO(sz).flip
    val dec = new CounterPortIO(sz).flip
    val full = Bool(OUTPUT)
    val empty = Bool(OUTPUT)
  }

  val count = Reg(init = UInt(reset_cnt, sz))

  val add = (io.inc.cnt & Fill(sz, io.inc.update))
  val sub = (io.dec.cnt & Fill(sz, io.dec.update)) | (io.la.cnt & Fill(sz, io.la.reserve))
  assert(!(io.la.reserve && io.dec.update), "simultaneous reserve and decrement")

  count := count + add - sub

  io.la.available := count >= io.la.cnt
  io.full := count === UInt(max_cnt)
  io.empty := count === UInt(0)
}

class AIWUpdateBufferEntry(DATA_SIZE: Int, ADDR_SIZE: Int) extends Bundle 
{
  val data = Bits(width=DATA_SIZE)
  val addr = UInt(width=ADDR_SIZE)
  override def clone = new AIWUpdateBufferEntry(DATA_SIZE, ADDR_SIZE).asInstanceOf[this.type]
}

class Buffer(DATA_SIZE: Int, DEPTH: Int) extends Module
{
  val ADDR_SIZE = log2Up(DEPTH)

  val io = new Bundle {
    val enq = Decoupled(Bits(width=DATA_SIZE)).flip
    val deq = Decoupled(Bits(width=DATA_SIZE))
    val update = Valid(new AIWUpdateBufferEntry(DATA_SIZE, ADDR_SIZE)).flip

    val rtag = Bits(OUTPUT, ADDR_SIZE)
  }

  val read_ptr_next = UInt( width=ADDR_SIZE)
  val write_ptr_next = UInt( width=ADDR_SIZE)
  val full_next = Bool()
  
  val read_ptr = Reg(init=UInt(0, ADDR_SIZE))
  val write_ptr = Reg(init=UInt(0, ADDR_SIZE))
  val full = Reg(init=Bool(false))

  read_ptr := read_ptr_next
  write_ptr := write_ptr_next
  full := full_next

  read_ptr_next := read_ptr
  write_ptr_next := write_ptr
  full_next := full

  val do_enq = io.enq.valid && io.enq.ready
  val do_deq = io.deq.ready && io.deq.valid

  when (do_deq) { read_ptr_next := read_ptr + UInt(1) }

  when (do_enq) 
  { 
    write_ptr_next := write_ptr + UInt(1) 
  }

  when (do_enq && !do_deq && (write_ptr_next === read_ptr))
  {
    full_next := Bool(true)
  }
  .elsewhen (do_deq && full) 
  {
    full_next := Bool(false)
  }
  .otherwise 
  {
    full_next := full
  }

  val empty = !full && (read_ptr === write_ptr)

  val data_next = Vec.fill(DEPTH){Bits(width=DATA_SIZE)}
  val data_array = Vec.fill(DEPTH){Reg(Bits(width=DATA_SIZE))}
  
  data_array := data_next

  data_next := data_array

  when (do_enq) { data_next(write_ptr) := io.enq.bits }
  when (io.update.valid) { data_next(io.update.bits.addr):= io.update.bits.data }

  io.enq.ready := !full
  io.deq.valid := !empty

  io.deq.bits := data_array(read_ptr)

  io.rtag := write_ptr
}

class AIWUpdateCounterVecIO extends ValidIO(Bits(width=SZ_AIW_NUMCNT))

class CounterVec(DEPTH: Int) extends Module
{
  val ADDR_SIZE = log2Up(DEPTH)
  val io = new Bundle {
    val enq = Decoupled(Bits(width=1)).flip()
    val deq = Decoupled(Bits(width=1))

    val update_from_issue = new AIWUpdateCounterVecIO().flip
    val update_from_seq = new AIWUpdateCounterVecIO().flip
    val update_from_evac = new AIWUpdateCounterVecIO().flip

    val markLast = Bool(INPUT)
    val deq_last = Bool(OUTPUT)
    val rtag = Bits(OUTPUT, ADDR_SIZE)
  }

  val next_write_ptr = UInt(width = ADDR_SIZE)
  val write_ptr = Reg(next = next_write_ptr, init = UInt(0, ADDR_SIZE))

  val next_last_write_ptr = UInt(width = ADDR_SIZE)
  val last_write_ptr = Reg(next = next_last_write_ptr, init = UInt(0, ADDR_SIZE))

  val next_read_ptr = UInt(width = ADDR_SIZE)
  val read_ptr = Reg(next = next_read_ptr, init = UInt(0, ADDR_SIZE))

  val next_full = Bool()
  val full = Reg(next = next_full, init = Bool(false))

  next_write_ptr := write_ptr
  next_last_write_ptr := last_write_ptr
  next_read_ptr := read_ptr
  next_full := full

  val do_enq = io.enq.valid && io.enq.ready
  val do_deq = io.deq.ready && io.deq.valid

  when (do_deq) { next_read_ptr := read_ptr + UInt(1) }

  when (do_enq) 
  { 
    next_write_ptr := write_ptr + UInt(1) 
    next_last_write_ptr := write_ptr
  }

  when (do_enq && !do_deq && (next_write_ptr === read_ptr))
  {
    next_full := Bool(true)
  }
  .elsewhen (do_deq && full) 
  {
    next_full := Bool(false)
  }
  .otherwise 
  {
    next_full := full
  }

  val empty = !full && (read_ptr === write_ptr)

  io.enq.ready := !full
  io.deq.valid := !empty

  val inc_vec = Vec.fill(DEPTH){Bool()}
  val dec_vec = Vec.fill(DEPTH){Bool()}
  val empty_vec = Vec.fill(DEPTH){Bool()}

  val next_last = Vec.fill(DEPTH){Bool()}
  val array_last = Vec.fill(DEPTH){Reg(Bool())}

  array_last := next_last
  next_last := array_last

  for(i <- 0 until DEPTH)
  {
    val counter = Module(new QCounter(0, DEPTH))
    counter.io.inc := inc_vec(i)
    counter.io.dec := dec_vec(i)
    empty_vec(i) := counter.io.empty
  }

  //defaults
  for(i <- 0 until DEPTH)
  {
    inc_vec(i) := Bool(false)
    dec_vec(i) := Bool(false)
  }

  when (do_enq) { 
    // on an enq, a vf instruction will write a zero 
    inc_vec(write_ptr) := io.enq.bits.toBool 
  }

  when (do_enq) {
    // on an enq, a vf instruction will write a zero
    next_last(write_ptr) := io.enq.bits.toBool 
  }
  when (io.markLast) { next_last(last_write_ptr) := Bool(true) }

  when (io.update_from_issue.valid) { inc_vec(io.update_from_issue.bits) := Bool(true) }
  when (io.update_from_seq.valid) { dec_vec(io.update_from_seq.bits) := Bool(true) }
  when (io.update_from_evac.valid) { dec_vec(read_ptr) := Bool(true) }

  io.deq.bits := empty_vec(read_ptr)
  io.deq_last := array_last(read_ptr)

  io.rtag := write_ptr
}

// Rotates n input elements into m output slots
class Rotator[T <: Data](gen: T, n: Int, m: Int, rev: Boolean = false) extends Module
{
  require(n <= m)
  val io = new Bundle {
    val in = Vec.fill(n){ gen.clone.asInput }
    val out = Vec.fill(m){ gen.clone.asOutput }
    val sel = UInt(INPUT, log2Up(m))
  }

  var barrel = io.in
  for (stage <- 0 until log2Up(m)) {
    val shift = 1 << stage
    val len = math.min(barrel.length + shift, m)

    barrel = Vec.tabulate(len){ i => {
      // k: source index with rotation enabled
      // i: source index with rotation disabled
      val k = if (rev) (i + shift) % m // shift backward
        else ((i - shift) + m) % m // shift forward
      if (i < barrel.length && k < barrel.length) {
        Mux(io.sel(stage), barrel(k), barrel(i))
      } else {
        // If either entry does not exist, use the other.
        if (i < barrel.length) barrel(i) else barrel(k)
      }
    }}
  }
  io.out := barrel
}

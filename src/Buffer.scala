package hwacha

import Chisel._
import Node._
import Constants._

class io_buffer(DATA_SIZE: Int, ADDR_SIZE: Int) extends Bundle 
{
  val flush = Bool(INPUT)
  
  val enq = new ioDecoupled()( Bits(width=DATA_SIZE) ).flip
  val deq = new ioDecoupled()( Bits(width=DATA_SIZE) )
  val update = new ioPipe()( new io_aiwUpdateReq(DATA_SIZE, ADDR_SIZE) ).flip

  val markLast = Bool(INPUT)
  val deq_last = Bool(OUTPUT)
  val rtag = Bits(ADDR_SIZE, OUTPUT)
}

class Buffer(DATA_SIZE: Int, DEPTH: Int, useLastPtr: Boolean = false) extends Component 
{
  val ADDR_SIZE = log2up(DEPTH)

  val io = new io_buffer(DATA_SIZE, ADDR_SIZE)

  val read_ptr_next = Wire(){ UFix( width=ADDR_SIZE) }
  val write_ptr_next = Wire(){ UFix( width=ADDR_SIZE) }
  val last_write_ptr_next = Wire(){ UFix( width=ADDR_SIZE) }
  val full_next = Wire(){ Bool() }
  
  val read_ptr = Reg(resetVal = UFix(0, ADDR_SIZE))
  val write_ptr = Reg(resetVal = UFix(0, ADDR_SIZE))
  val last_write_ptr = Reg(resetVal = UFix(0, ADDR_SIZE))
  val full = Reg(resetVal = Bool(false))

  read_ptr := read_ptr_next
  write_ptr := write_ptr_next
  last_write_ptr := last_write_ptr_next
  full := full_next

  read_ptr_next := read_ptr
  write_ptr_next := write_ptr
  last_write_ptr_next := last_write_ptr
  full_next := full

  val do_enq = io.enq.valid && io.enq.ready
  val do_deq = io.deq.ready && io.deq.valid

  when (do_deq) { read_ptr_next := read_ptr + UFix(1) }

  when(do_enq) 
  { 
    write_ptr_next := write_ptr + UFix(1) 
    last_write_ptr_next := write_ptr
  }

  when (io.flush) 
  {
    read_ptr_next := UFix(0, ADDR_SIZE)
    write_ptr_next := UFix(0, ADDR_SIZE)
    last_write_ptr_next := UFix(0, ADDR_SIZE)
  }

  when (io.flush)
  {
    full_next := Bool(false)
  }
  . elsewhen (do_enq && !do_deq && (write_ptr_next === read_ptr))
  {
    full_next := Bool(true)
  }
  . elsewhen (do_deq && full) 
  {
    full_next := Bool(false)
  }
  . otherwise 
  {
    full_next := full
  }

  val empty = !full && (read_ptr === write_ptr)

  val data_next = Vec(DEPTH){ Wire(){ Bits(width=DATA_SIZE) } }
  val data_array = Vec(DEPTH){ Reg(){ Bits(width=DATA_SIZE) } }

  val last_next = Vec(DEPTH){ Wire(){ Bool() } }
  val last_array = Vec(DEPTH){ Reg(){ Bool() } }
  
  data_array := data_next
  last_array := last_next

  data_next := data_array
  last_next := last_array

  when (do_enq) { data_next.write(write_ptr, io.enq.bits) }
  when (io.update.valid) { data_next.write(io.update.bits.addr, io.update.bits.data) }

  if (useLastPtr)
  {
    when (do_enq) { last_next.write(write_ptr, Bool(false)) }
    when (io.markLast) { last_next.write(last_write_ptr, Bool(true)) }
  }

  io.enq.ready := !full
  io.deq.valid := !empty

  val bypass_bits = (read_ptr === last_write_ptr) && do_deq && io.update.valid
  val bypass_last = (read_ptr === io.update.bits.addr) && do_deq && io.markLast

  if (useLastPtr)
  {
    io.deq_last := Mux(bypass_last, last_next.read(read_ptr), last_array.read(read_ptr))
  }
  io.deq.bits := Mux(bypass_bits, data_next.read(read_ptr), data_array.read(read_ptr))

  io.rtag := write_ptr
}

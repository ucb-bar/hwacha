package hwacha

import Chisel._
import Node._
import Config._
import Interface._

class io_buffer(DATA_SIZE: Int, ADDR_SIZE: Int) extends Bundle 
{
  val enq = new io_ready_valid()( Bits(width=DATA_SIZE) ).flip()
  val deq = new io_ready_valid()( Bits(width=DATA_SIZE) )
  val update = new io_valid()( new io_irbUpdateReq(DATA_SIZE, ADDR_SIZE) ).flip()

  val updateLast = Bool(INPUT)
  val rtag = Bits(ADDR_SIZE, OUTPUT)
}

class Buffer(DATA_SIZE: Int, DEPTH: Int, usePrevPtr: Boolean = false) extends Component 
{
  val ADDR_SIZE = log2up(DEPTH)

  val io = new io_buffer(DATA_SIZE, ADDR_SIZE)

  val deq_ptr_next = Wire(){ UFix( width=ADDR_SIZE) }
  val enq_ptr_next = Wire(){ UFix( width=ADDR_SIZE) }
  val full_next = Wire(){ Bool() }
  
  val deq_ptr = Reg(resetVal = UFix(0, ADDR_SIZE))
  val enq_ptr = Reg(resetVal = UFix(0, ADDR_SIZE))
  val enq_ptr_prev = Reg(enq_ptr)
  val full = Reg(resetVal = Bool(false))

  deq_ptr := deq_ptr_next
  enq_ptr := enq_ptr_next
  full := full_next

  deq_ptr_next := deq_ptr
  enq_ptr_next := enq_ptr
  full_next := full

  val do_enq = io.enq.valid && io.enq.ready
  val do_deq = io.deq.ready && io.deq.valid

  when(do_deq) { deq_ptr_next := deq_ptr + UFix(1) }
  when(do_enq) { enq_ptr_next := enq_ptr + UFix(1) }

  when (do_enq && !do_deq && (enq_ptr_next === deq_ptr))
  {
    full_next := Bool(true)
  }
  . elsewhen (do_deq && full) 
  {
    full_next := Bool(true)
  }
  . otherwise 
  {
    full_next := full
  }

  val empty = !full && (deq_ptr === enq_ptr)

  val data_next = GenArray(DEPTH){ Wire(){ Bits(width=DATA_SIZE) } }
  val data_array = GenArray(DEPTH){ Reg(){ Bits(width=DATA_SIZE) } }
  
  data_array := data_next
  data_next := data_array

  when(do_enq) { data_next.write(enq_ptr, io.enq.bits) }

  if(usePrevPtr)
  {
    when(io.updateLast && io.update.valid && enq_ptr_prev === io.update.bits.addr)
    {
      data_next.write(enq_ptr_prev, Cat(io.updateLast, Cat(Bits(0,1), io.update.bits.data)))
    }
    . otherwise 
    {
      when(io.update.valid)
      {
	data_next.write(io.update.bits.addr, Cat(Bits(0,1), io.update.bits.data))
      }
      when(io.updateLast)
      {
	data_next.write(enq_ptr_prev, 
			Cat(io.updateLast, data_array.read(enq_ptr_prev)(DATA_SIZE-2,0))) 
      }
    }    
  } else 
  {
    when(io.update.valid) { data_next.write(io.update.bits.addr, io.update.bits.data) }    
  }

  io.enq.ready := !full
  io.deq.valid := !empty
  io.deq.bits := data_array.read(deq_ptr)
  io.rtag := enq_ptr
}

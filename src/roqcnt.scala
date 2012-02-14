package hwacha

import Chisel._
import Node._
import Interface._

class io_roq_enq_bundle(ROQ_DATA_SIZE :Int, ROQ_TAG_SIZE :Int) extends Bundle
{
  val data = Bits(width=ROQ_DATA_SIZE)
  val rtag = UFix(width=ROQ_TAG_SIZE)
}

class io_roqcnt(ROQ_DATA_SIZE :Int, ROQ_TAG_SIZE :Int) extends Bundle
{
  val roq_deq_rtag = new io_ready_valid()( {Bits(width=ROQ_TAG_SIZE)} )
  val roq_deq_data = new io_ready_valid()( {Bits(width=ROQ_DATA_SIZE)} )
  val roq_enq = new io_valid()( {new io_roq_enq_bundle(ROQ_DATA_SIZE, ROQ_TAG_SIZE)} ).flip()
 
  val qcnt = UFix(ROQ_TAG_SIZE, INPUT)
  val watermark = Bool(OUTPUT)
}

class roqcnt(ROQ_DATA_SIZE :Int, ROQ_TAG_ENTRIES :Int, ROQ_TAG_SIZE :Int, ROQ_MAX_QCNT :Int) extends Component
{
  val io = new io_roqcnt(ROQ_DATA_SIZE, ROQ_TAG_SIZE)
  
  val read_ptr = Reg(resetVal = UFix(0, ROQ_TAG_SIZE))
  val write_ptr = Reg(resetVal = UFix(0, ROQ_TAG_SIZE))
  val read_ptr_next = read_ptr + UFix(1)
  val write_ptr_next = write_ptr + UFix(1)

  val full = (write_ptr_next === read_ptr)
  val data_array = Mem(ROQ_TAG_ENTRIES, io.roq_enq.valid, io.roq_enq.bits.rtag, io.roq_enq.bits.data)
  data_array.setReadLatency(1)
  
  val vb_array = Reg(resetVal=Bits(0, ROQ_TAG_ENTRIES))
  val vb_update_read = Mux(roq_data_deq, ~(Bits(1) << read_ptr), Fill(ROQ_TAG_ENTRIES,Bits(1)))
  val vb_update_write = Mux(io.roq_enq.valid, (Bits(1) << io.roq_enq.bits.rtag), Bits(0,ROQ_TAG_ENTRIES))
  vb_array := (vb_array & vb_update_read) | vb_update_write

  val roq_data_deq = io.roq_deq_data.ready && io.roq_deq_data.valid
  val roq_rtag_deq = io.roq_deq_rtag.ready && io.roq_deq_rtag.valid
  
  val roq_deq_data_val_int = Reg(resetVal = Bool(false))

  roq_deq_data_val_int := vb_array(read_ptr).toBool
  when(roq_rtag_deq)
  {
    write_ptr := write_ptr_next
  }
  when(roq_data_deq)
  {
    roq_deq_data_val_int := vb_array(read_ptr_next).toBool
    read_ptr := read_ptr_next
  }

  io.roq_deq_rtag.valid := !full
  io.roq_deq_rtag.bits := write_ptr.toBits

  io.roq_deq_data.valid := roq_deq_data_val_int
  io.roq_deq_data.bits := data_array(Mux(roq_data_deq, read_ptr_next, read_ptr), oe = Bool(true), cs = Bool(true))

  // Logic for watermark
  val shifted_vb_array = Reg(resetVal=Bits(0, ROQ_TAG_ENTRIES))
  val shifted_write_ptr =
    Mux(read_ptr < io.roq_enq.bits.rtag, io.roq_enq.bits.rtag - read_ptr, 
        UFix(ROQ_TAG_ENTRIES) - read_ptr + io.roq_enq.bits.rtag)

  val shifted_vb_update_write = Mux(io.roq_enq.valid, (Bits(1) << shifted_write_ptr), Bits(0,ROQ_TAG_ENTRIES))
  
  shifted_vb_array := Mux(roq_data_deq,
    (shifted_vb_array | shifted_vb_update_write),
    ((shifted_vb_array | shifted_vb_update_write) >> UFix(1)))
 
  var sel = shifted_vb_array(0)
  var output = UFix(1, ROQ_TAG_SIZE)
  for (i <- 0 until ROQ_MAX_QCNT)
  {
    output = Mux(sel, UFix(i), output)
    sel = sel & shifted_vb_array(i+1)
  }
  
  io.watermark := output >= io.qcnt
}

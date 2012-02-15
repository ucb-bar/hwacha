package queues

import Chisel._
import Node._
import scala.math._

// Util stuff
object log2up
{
  def apply(in: Int) = if (in == 1) 1 else ceil(log(in)/log(2)).toInt
}

class io_ready_valid[T <: Data]()(data: => T) extends Bundle
{
  val ready = Bool(INPUT)
  val valid = Bool(OUTPUT)
  val bits = data.asOutput
}

class io_valid[T <: Data]()(data: => T) extends Bundle
{
  val valid = Bool(OUTPUT)
  val bits = data.asOutput
}
// end util stuff

class IOqueueCtrl1_pipe extends Bundle
{
  val enq_val = Bool(INPUT)
  val enq_rdy = Bool(OUTPUT)
  val deq_val = Bool(OUTPUT)
  val deq_rdy = Bool(INPUT)
  val wen = Bool(OUTPUT)
}

class queueCtrl1_pipe extends Component
{
  override val io = new IOqueueCtrl1_pipe()

  val full = Reg(width = 1, resetVal = Bool(false))
  val empty = !full

  val enq_rdy_int = !full || (full && io.deq_rdy)
  val deq_val_int = !empty
  val do_enq = enq_rdy_int && io.enq_val
  val do_deq = io.deq_rdy && deq_val_int

  val do_pipe = full && do_enq && do_deq

  io.wen := do_enq
  io.enq_rdy := enq_rdy_int
  io.deq_val := deq_val_int

  when(do_deq && !do_pipe)
  {
    full := Bool(false)
  }
  when(do_enq)
  {
    full := Bool(true)
  }
}

class IOqueueCtrl(addr_sz: Int) extends Bundle()
{
  val enq_val = Bool(INPUT)
  val enq_rdy = Bool(OUTPUT)
  val deq_val = Bool(OUTPUT)
  val deq_rdy = Bool(INPUT)
  val wen     = Bool(OUTPUT)
  val waddr   = UFix(addr_sz, OUTPUT)
  val raddr   = UFix(addr_sz, OUTPUT)
}

class queueCtrl_pipe(entries: Int, addr_sz: Int) extends Component
{
  override val io = new IOqueueCtrl(addr_sz)

  // Enqueue and dequeue pointers

  val enq_ptr = Reg(width = addr_sz, resetVal = UFix(0, addr_sz))
  val deq_ptr = Reg(width = addr_sz, resetVal = UFix(0, addr_sz))
  val full    = Reg(width = 1, resetVal = Bool(false))

  io.waddr := enq_ptr
  io.raddr := deq_ptr

  // We enq/deq only when they are both ready and valid
  val empty = ~full && (enq_ptr === deq_ptr)

  val enq_rdy_int = ~full || (full && io.deq_rdy)
  val deq_val_int = ~empty

  val do_enq = enq_rdy_int && io.enq_val
  val do_deq = io.deq_rdy && deq_val_int

  val do_pipe = full && (do_enq && do_deq)
  // Determine if we have pipeline or flowthrough behaviour and
  // set the write enable accordingly.


  io.wen := do_enq

  // Ready signals are calculated from full register. If pipeline
  // behavior is enabled, then the enq_rdy signal is also calculated
  // combinationally from the deq_rdy signal. If flowthrough behavior
  // is enabled then the deq_val signal is also calculated combinationally
  // from the enq_val signal.


  io.enq_rdy := enq_rdy_int
  io.deq_val := deq_val_int

  // Control logic for the enq/deq pointers and full register

  val deq_ptr_inc = deq_ptr + UFix(1, 1)
  val enq_ptr_inc = enq_ptr + UFix(1, 1)

  val deq_ptr_next =
    Mux(do_deq, deq_ptr_inc,
        deq_ptr)

  val enq_ptr_next =
    Mux(do_enq, enq_ptr_inc,
        enq_ptr)

  val full_next =
    Mux(do_enq && ~do_deq && ( enq_ptr_inc === deq_ptr ), Bool(true),
    Mux(do_deq && full && ~do_pipe,                       Bool(false),
        full))

  enq_ptr := enq_ptr_next
  deq_ptr := deq_ptr_next
  full    := full_next
}

class queueCtrl(entries: Int, addr_sz: Int) extends Component
{
  override val io = new IOqueueCtrl(addr_sz)

  // Enqueue and dequeue pointers

  val enq_ptr = Reg(width = addr_sz, resetVal = UFix(0, addr_sz))
  val deq_ptr = Reg(width = addr_sz, resetVal = UFix(0, addr_sz))
  val full    = Reg(width = 1, resetVal = Bool(false))

  io.waddr := enq_ptr
  io.raddr := deq_ptr

  // We enq/deq only when they are both ready and valid

  val do_enq = io.enq_rdy && io.enq_val
  val do_deq = io.deq_rdy && io.deq_val

  // Determine if we have pipeline or flowthrough behaviour and
  // set the write enable accordingly.

  val empty = ~full && (enq_ptr === deq_ptr)

  io.wen := do_enq

  // Ready signals are calculated from full register. If pipeline
  // behavior is enabled, then the enq_rdy signal is also calculated
  // combinationally from the deq_rdy signal. If flowthrough behavior
  // is enabled then the deq_val signal is also calculated combinationally
  // from the enq_val signal.

  io.enq_rdy := ~full
  io.deq_val := ~empty

  // Control logic for the enq/deq pointers and full register

  val deq_ptr_inc = deq_ptr + UFix(1, 1)
  val enq_ptr_inc = enq_ptr + UFix(1, 1)

  val deq_ptr_next =
    Mux(do_deq, deq_ptr_inc,
        deq_ptr)

  val enq_ptr_next =
    Mux(do_enq, enq_ptr_inc,
        enq_ptr)

  val full_next =
    Mux(do_enq && ~do_deq && ( enq_ptr_inc === deq_ptr ), Bool(true),
    Mux(do_deq && full,                                   Bool(false),
        full))

  enq_ptr := enq_ptr_next
  deq_ptr := deq_ptr_next
  full    := full_next
}

class io_queue(data_sz: Int) extends Bundle()
{
  val enq = (new io_ready_valid())(Bits(width=data_sz)).flip()
  val deq = (new io_ready_valid())(Bits(width=data_sz))
}

class queueSimplePF(data_sz: Int, entries: Int) extends Component
{
  val addr_sz = log2up(entries)
  override val io = new io_queue(data_sz)
  val ctrl = new queueCtrl(entries, addr_sz)
  ctrl.io.deq_val <> io.deq.valid
  ctrl.io.enq_rdy <> io.enq.ready
  ctrl.io.enq_val <> io.enq.valid
  ctrl.io.deq_rdy <> io.deq.ready
  val ram = Mem(entries, ctrl.io.wen, ctrl.io.waddr, io.enq.bits)
  io.deq.bits := ram(ctrl.io.raddr)
}

class queuePipePF(data_sz: Int, entries: Int) extends Component
{
  val addr_sz = log2up(entries)
  override val io = new io_queue(data_sz)
  val ctrl = new queueCtrl_pipe(entries, addr_sz)
  ctrl.io.deq_val <> io.deq.valid
  ctrl.io.enq_rdy <> io.enq.ready
  ctrl.io.enq_val <> io.enq.valid
  ctrl.io.deq_rdy <> io.deq.ready
  val ram = Mem(entries, ctrl.io.wen, ctrl.io.waddr, io.enq.bits)
  io.deq.bits := ram(ctrl.io.raddr)
}

// TODO: SHOULD USE INHERITANCE BUT BREAKS INTROSPECTION CODE
// class IOqueueCtrlFlow extends IOqueueCtrl
class IOqueueCtrlFlow(addr_sz: Int) extends Bundle() /* IOqueueCtrl */
{
  val enq_val  = Bool(INPUT)
  val enq_rdy  = Bool(OUTPUT)
  val deq_val  = Bool(OUTPUT)
  val deq_rdy  = Bool(INPUT)
  val wen      = Bool(OUTPUT)
  val waddr    = UFix(addr_sz, OUTPUT)
  val raddr    = UFix(addr_sz, OUTPUT)
  val flowthru = Bool(OUTPUT)
}

class queueCtrlFlow(entries: Int, addr_sz: Int) extends Component
{
  override val io = new IOqueueCtrlFlow(addr_sz)
  // Enqueue and dequeue pointers

  val enq_ptr = Reg(width = addr_sz, resetVal = UFix(0, addr_sz))
  val deq_ptr = Reg(width = addr_sz, resetVal = UFix(0, addr_sz))
  val full    = Reg(width = 1, resetVal = Bool(false))

  io.waddr := enq_ptr
  io.raddr := deq_ptr

  // We enq/deq only when they are both ready and valid

  val do_enq = io.enq_rdy && io.enq_val
  val do_deq = io.deq_rdy && io.deq_val

  // Determine if we have pipeline or flowthrough behaviour and
  // set the write enable accordingly.

  val empty       = ~full && (enq_ptr === deq_ptr)
  val do_flowthru = empty && do_enq && do_deq
  io.flowthru  := do_flowthru

  io.wen    := do_enq && ~do_flowthru

  // Ready signals are calculated from full register. If pipeline
  // behavior is enabled, then the enq_rdy signal is also calculated
  // combinationally from the deq_rdy signal. If flowthrough behavior
  // is enabled then the deq_val signal is also calculated combinationally
  // from the enq_val signal.

  io.enq_rdy  := ~full
  io.deq_val  := ~empty || ( empty && io.enq_val )

  // Control logic for the enq/deq pointers and full register

  val deq_ptr_inc = deq_ptr + UFix(1, 1)
  val enq_ptr_inc = enq_ptr + UFix(1, 1)

  val deq_ptr_next =
    Mux(do_deq && ~do_flowthru, deq_ptr_inc,
        deq_ptr)

  val enq_ptr_next =
    Mux(do_enq && ~do_flowthru, enq_ptr_inc,
        enq_ptr)

  val full_next =
    Mux(do_enq && ~do_deq && ( enq_ptr_inc === deq_ptr ), Bool(true),
    Mux(do_deq && full,                                   Bool(false),
        full))

  enq_ptr := enq_ptr_next
  deq_ptr := deq_ptr_next
  full    := full_next
}

class IOqueueDpathFlow(data_sz: Int, addr_sz: Int) extends Bundle()
{
  val wen         = Bool(INPUT)
  val flowthru    = Bool(INPUT)
  val deq_bits    = Bits(data_sz, OUTPUT)
  val enq_bits    = Bits(data_sz, INPUT)
  val waddr       = UFix(addr_sz, INPUT)
  val raddr       = UFix(addr_sz, INPUT)
}

class queueDpathFlow(data_sz: Int, entries: Int, addr_sz: Int) extends Component
{
  override val io = new IOqueueDpathFlow(data_sz, addr_sz)
  val ram  = Mem(entries, io.wen, io.waddr, io.enq_bits)
  val rout = ram(io.raddr)
  io.deq_bits := Mux(io.flowthru, io.enq_bits, rout)
}

//--------------------------------------------------------------------------
// Single-Element Queues
//--------------------------------------------------------------------------


/*
class queue1PF(data_sz:Int) extends Component
{
  override val io = new IOqueue1PF(data_sz)
  val ctrl = new queue1Simple(data_sz)
  val deq_bits = Reg(width=data_sz)
  io.deq_bits := deq_bits
  when(wen)
  {
    deq_bits := io.enq_bits
  }
}
*/
class queuePipe1PF(data_sz:Int) extends Component
{
  val wen = Wire(){Bool()}
  override val io = new io_queue(data_sz)
  val ctrl = new queueCtrl1_pipe()
  ctrl.io.enq_val <> io.enq.valid
  ctrl.io.enq_rdy <> io.enq.ready
  ctrl.io.deq_val <> io.deq.valid
  ctrl.io.deq_rdy <> io.deq.ready
  wen := ctrl.io.wen
  val deq_bits_reg = Reg(width=data_sz, resetVal=Bits("b0", data_sz))
  io.deq.bits := deq_bits_reg
  when(wen)
  {
    deq_bits_reg := io.enq.bits
  }
}

class queueFlowPF(data_sz: Int, entries: Int) extends Component
{
  override val io = new io_queue(data_sz)
  val addr_sz = log2up(entries)
  val ctrl  = new queueCtrlFlow(entries, addr_sz)
  val dpath = new queueDpathFlow(data_sz, entries, addr_sz)
  ctrl.io.deq_rdy   <> io.deq.ready
  ctrl.io.wen       <> dpath.io.wen
  ctrl.io.raddr     <> dpath.io.raddr
  ctrl.io.waddr     <> dpath.io.waddr
  ctrl.io.flowthru  <> dpath.io.flowthru
  ctrl.io.enq_val   <> io.enq.valid
  dpath.io.enq_bits <> io.enq.bits

  ctrl.io.deq_val   <> io.deq.valid
  ctrl.io.enq_rdy   <> io.enq.ready
  dpath.io.deq_bits <> io.deq.bits
}

class io_queue_spec[T <: Data](data: => T) extends Bundle
{
  val enq = new io_ready_valid()( data ).flip()
  val deq = new io_ready_valid()( data )

  val ack = Bool(INPUT)
  val nack = Bool(INPUT)
}

class queue_spec[T <: Data](entries: Int)(data: => T) extends Component
{
  val io = new io_queue_spec({ data })

  // cannot ack and nack at the same cycle
  // chisel_assert(io.ack && !io.nack)
  // chisel_assert(!io.ack && io.nack)

  val enq_ptr = Reg(resetVal = UFix(0, log2up(entries)))
  val deq_ptr = Reg(resetVal = UFix(0, log2up(entries)))
  val deq_ptr_spec = Reg(resetVal = UFix(0, log2up(entries)))
  val full = Reg(resetVal = Bool(false))
  val full_spec = Reg(resetVal = Bool(false))

  // enq is not affected by nack
  // since enqueuing is governed by the non-spec deq ptr
  io.enq.ready := !full

  // can't bypass the deq pointer
  // since a nack signal might be late
  io.deq.valid := !io.nack && (full_spec || (enq_ptr != deq_ptr_spec))

  val do_enq = io.enq.ready && io.enq.valid
  val do_deq = io.ack
  val do_deq_spec = io.deq.ready && io.deq.valid

  val enq_ptr_inc = enq_ptr + UFix(1)

  when (do_enq) { enq_ptr := enq_ptr_inc }
  when (do_deq) { deq_ptr := deq_ptr + UFix(1) }
  when (do_deq_spec) { deq_ptr_spec := deq_ptr_spec + UFix(1) }

  val full_next =
    Mux(do_enq && !do_deq && (enq_ptr_inc === deq_ptr), Bool(true),
    Mux(do_deq && full, Bool(false),
        full))

  full := full_next

  when (io.nack)
  {
    deq_ptr_spec := deq_ptr
    full_spec := full_next
  }
  .otherwise
  {
    full_spec :=
      Mux(do_enq && !do_deq_spec && (enq_ptr_inc === deq_ptr_spec), Bool(true),
      Mux(do_deq_spec && full_spec, Bool(false),
          full_spec))
  }

  io.deq.bits <> Mem(entries, do_enq, enq_ptr, io.enq.bits).read(deq_ptr_spec)
}

class io_queue_reorder_qcnt_enq_bundle(ROQ_DATA_SIZE: Int, ROQ_TAG_SIZE: Int) extends Bundle
{
  val data = Bits(width=ROQ_DATA_SIZE)
  val rtag = UFix(width=ROQ_TAG_SIZE)
}

class io_queue_reorder_qcnt(ROQ_DATA_SIZE: Int, ROQ_TAG_SIZE: Int) extends Bundle
{
  val deq_rtag = new io_ready_valid()( {Bits(width=ROQ_TAG_SIZE)} )
  val deq_data = new io_ready_valid()( {Bits(width=ROQ_DATA_SIZE)} )
  val enq = new io_valid()( {new io_queue_reorder_qcnt_enq_bundle(ROQ_DATA_SIZE, ROQ_TAG_SIZE)} ).flip()

  val ack = Bool(INPUT)
  val nack = Bool(INPUT)

  val qcnt = UFix(ROQ_TAG_SIZE, INPUT)
  val watermark = Bool(OUTPUT)
}

class queue_reorder_qcnt(ROQ_DATA_SIZE: Int, ROQ_TAG_ENTRIES: Int, ROQ_MAX_QCNT: Int) extends Component
{
  val ROQ_TAG_SIZE = log2up(ROQ_TAG_ENTRIES)

  val io = new io_queue_reorder_qcnt(ROQ_DATA_SIZE, ROQ_TAG_SIZE)

  val read_ptr = Reg(resetVal = UFix(0, ROQ_TAG_SIZE))
  val write_ptr = Reg(resetVal = UFix(0, ROQ_TAG_SIZE))
  val write_ptr_spec = Reg(resetVal = UFix(0, ROQ_TAG_SIZE))
  val read_ptr_next = read_ptr + UFix(1)
  val full = Reg(resetVal = Bool(false))
  val full_spec = Reg(resetVal = Bool(false))

  val roq_data_deq = io.deq_data.ready && io.deq_data.valid
  val roq_rtag_deq = io.deq_rtag.ready && io.deq_rtag.valid

  val data_array = Mem(ROQ_TAG_ENTRIES, io.enq.valid, io.enq.bits.rtag, io.enq.bits.data)

  val vb_array = Reg(resetVal = Bits(0, ROQ_TAG_ENTRIES))
  val vb_update_read = Mux(roq_data_deq, ~(Bits(1) << read_ptr), Fill(ROQ_TAG_ENTRIES, Bits(1)))
  val vb_update_write = Mux(io.enq.valid, (Bits(1) << io.enq.bits.rtag), Bits(0, ROQ_TAG_ENTRIES))
  vb_array := (vb_array & vb_update_read) | vb_update_write

  val roq_data_deq = io.deq_data.ready && io.deq_data.valid
  val roq_rtag_deq_spec = io.deq_rtag.ready && io.deq_rtag.valid
  val roq_rtag_deq = io.ack

  val deq_data_val_int = Reg(resetVal = Bool(false))

  deq_data_val_int := vb_array(read_ptr).toBool

  when(roq_rtag_deq) { write_ptr := write_ptr + UFix(1) }
  when(roq_rtag_deq_spec) { write_ptr_spec := write_ptr_spec + UFix(1) }
  when(roq_data_deq)
  {
    deq_data_val_int := vb_array(read_ptr_next).toBool
    read_ptr := read_ptr_next
  }

  io.deq_rtag.valid := !io.nack && (full_spec || (read_ptr != write_ptr_spec))
  io.deq_rtag.bits := write_ptr.toBits

  val full_next =
    Mux(io.enq.valid && !roq_rtag_deq_spec && (read_ptr_next === write_ptr), Bool(true),
    Mux(roq_rtag_deq && full, Bool(false),
        full))

  full := full_next

  when (io.nack)
  {
    write_ptr_spec := write_ptr
    full_spec := full_next
  }
  .otherwise
  {
    full_spec :=
      Mux(io.enq.valid && !roq_rtag_deq_spec && (read_ptr_next === write_ptr_spec), Bool(true),
      Mux(roq_rtag_deq_spec && full_spec, Bool(false),
          full_spec))
  }


  io.deq_data.valid := deq_data_val_int
  io.deq_data.bits := data_array(Mux(roq_data_deq, read_ptr_next, read_ptr))

  // Logic for watermark
  val shifted_vb_array = Reg(resetVal = Bits(0, ROQ_TAG_ENTRIES))
  val shifted_write_ptr =
    Mux(read_ptr < io.enq.bits.rtag, io.enq.bits.rtag - read_ptr,
        UFix(ROQ_TAG_ENTRIES) - read_ptr + io.enq.bits.rtag)

  val shifted_vb_update_write =
    Mux(io.enq.valid, (Bits(1) << shifted_write_ptr),
        Bits(0, ROQ_TAG_ENTRIES))

  shifted_vb_array :=
    Mux(roq_data_deq, (shifted_vb_array | shifted_vb_update_write),
        ((shifted_vb_array | shifted_vb_update_write) >> UFix(1)))

  // a limited version of leading count ones
  // maximum cnt is defined by ROQ_MAX_QCNT
  var sel = shifted_vb_array(0)
  var output = UFix(1, ROQ_TAG_SIZE)
  // for (i <- 0 until ROQ_MAX_QCNT)
  // {
  //   output = Mux(sel, UFix(i), output)
  //   sel = sel & shifted_vb_array(i+1)
  // }

  io.watermark := output >= io.qcnt
}


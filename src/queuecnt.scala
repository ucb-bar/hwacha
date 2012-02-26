package hwacha

import Chisel._
import Node._
import scala.math.{log, floor}

class io_queuecnt(w: Int) extends Bundle
{
  val inc = Bool(INPUT)
  val dec = Bool(INPUT)
  val watermark = Bool(OUTPUT)
  val qcnt = UFix(w, INPUT)
  val zero = Bool(OUTPUT)
}

class queuecnt(reset_cnt : Int, max_cnt : Int, use_qcnt: Boolean = true, ready_cnt: Int = 0) extends Component
{
  def floorLog2(x : Int)=floor(log(x)/log(2.0)).toInt

  val io = new io_queuecnt(floorLog2(max_cnt)+1)
  val count = Reg(resetVal = UFix(reset_cnt, floorLog2(max_cnt)+1))
  val next_count = Wire(){ UFix(width = floorLog2(max_cnt)+1) }

  next_count := count
  when(io.inc ^ io.dec)
  {
    when(io.inc) {next_count := count + UFix(1)}
    when(!io.inc) {next_count := count - UFix(1)}
  }

  count := next_count

  // we need to look at what's in the queue on the next cycle
  if (!use_qcnt)
    io.watermark := next_count >= UFix(ready_cnt)
  else
    io.watermark := next_count >= io.qcnt

  io.zero := (count === UFix(reset_cnt))
}

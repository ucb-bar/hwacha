package hwacha

import Chisel._
import Node._
import Config._
import Interface._

class io_sackcnt extends Bundle
{
  val vsdq_ack = Bool(INPUT)
  val vsdq_enq_valid = Bool(INPUT)
  val vsdq_enq_ready = Bool(INPUT)
  val qcnt = UFix(VACKCNT_SZ,INPUT)
  val zero = Bool(OUTPUT)
  val watermark = Bool(OUTPUT)
}

class sackcnt extends Component
{
  val io = new io_sackcnt()
  val cnt = Reg(resetVal=UFix(VACKCNT-1,VACKCNT_SZ))

  io.zero := (cnt === UFix(VACKCNT-1))

  val enq = io.vsdq_enq_valid && io.vsdq_enq_ready
  val deq = io.vsdq_ack

  when( enq && !deq ) { cnt := cnt - UFix(1) }
  when( !enq && deq ) { cnt := cnt + UFix(1) }

  io.watermark := (cnt >= io.qcnt)
}

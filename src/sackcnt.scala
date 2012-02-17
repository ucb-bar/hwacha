package hwacha

import Chisel._
import Node._
import Config._
import Interface._

class io_sackcnt extends Bundle
{
  val vsdq_ack = Bool(INPUT)
  val vsdq_deq_valid = Bool(INPUT)
  val vsdq_deq_ready = Bool(INPUT)
  val qcnt = UFix(VACKCNT_SZ,INPUT)
  val zero = Bool(OUTPUT)
  val watermark = Bool(OUTPUT)
}

class sackcnt extends Component
{
  val io = new io_sackcnt()
  val cnt = Reg(resetVal=UFix(VACKCNT-1,VACKCNT_SZ))

  io.zero := (cnt === UFix(VACKCNT-1))

  when( io.vsdq_deq_valid && io.vsdq_deq_ready )
  {
    when( !io.vsdq_ack ) { cnt := cnt - UFix(1) }
  }
  .elsewhen( io.vsdq_ack ) { cnt := cnt + UFix(1) }

  io.watermark := (cnt >= io.qcnt)
}

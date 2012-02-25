package hwacha

import Chisel._
import Node._
import Constants._

class io_sackcnt extends Bundle
{
  val inc = Bool(INPUT)
  val dec = Bool(INPUT)
  val qcnt = UFix(SZ_VACKCNT,INPUT)
  val zero = Bool(OUTPUT)
  val watermark = Bool(OUTPUT)
}

class sackcnt extends Component
{
  val io = new io_sackcnt()
  val cnt = Reg(resetVal=UFix(VACKCNT-1,SZ_VACKCNT))

  io.zero := (cnt === UFix(VACKCNT-1))

  when( io.dec && !io.inc ) { cnt := cnt - UFix(1) }
  when( !io.dec && io.inc ) { cnt := cnt + UFix(1) }

  io.watermark := (cnt >= io.qcnt)
}

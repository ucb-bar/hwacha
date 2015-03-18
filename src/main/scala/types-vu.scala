package hwacha

import Chisel._
import Node._
import Constants._

//-------------------------------------------------------------------------\\
// vector command queue types
//-------------------------------------------------------------------------\\

class HwachaCommand extends Bundle
{
  //COLIN FIXME: parameterize widths
  val cmcode = Bits(width = 8)
  val vd = UInt(width = 8)
}

class HwachaImm1 extends Bundle
{
  val prec = Bits(width = 2)
  val bcnt = UInt(width = SZ_BCNT)
  val bactive = Bits(width = SZ_BANK)
  val nvregs = UInt(width = SZ_REGCNT)
  val npregs = UInt(width = SZ_REGCNT)
  val vlen = UInt(width = SZ_VLEN)
}

class HwachaCnt extends Bundle
{
  val cnt = UInt(width = SZ_VLEN)
  val last = Bool()
}

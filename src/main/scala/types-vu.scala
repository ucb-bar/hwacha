package hwacha

import Chisel._
import Node._
import Constants._

//-------------------------------------------------------------------------\\
// vector command queue types
//-------------------------------------------------------------------------\\

class HwachaCommand extends Bundle
{
  val cmcode = Bits(width = 8)
  val vd = UInt(width = 5)
  val vt = UInt(width = 5)
}

class HwachaImm1 extends Bundle
{
  val prec = Bits(width = 2)
  val xf_split = UInt(width = SZ_BREGLEN) 
  val bcnt = UInt(width = SZ_BCNT)
  val bactive = Bits(width = SZ_BANK)
  val nfregs = UInt(width = SZ_REGCNT)
  val nxregs = UInt(width = SZ_REGCNT)
  val vlen = UInt(width = SZ_VLEN)
}

class HwachaCnt extends Bundle
{
  val cnt = UInt(width = SZ_VLEN)
  val last = Bool()
}

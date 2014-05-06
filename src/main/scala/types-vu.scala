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

/* MUST match order of vimm_vlen in hwacha.scala */
class HwachaImm1 extends Bundle
{
  val ut_per_bank = UInt(width = SZ_BANK+1) // include == 256
  val bcnt = UInt(width = SZ_BCNT)
  val bactive = Bits(width = SZ_BANK)
  val nhfregs = UInt(width = SZ_REGCNT)
  val nsfregs = UInt(width = SZ_REGCNT)
  val ndfregs = UInt(width = SZ_REGCNT)
  val nxregs = UInt(width = SZ_REGCNT)
  val vlen = UInt(width = SZ_VLEN)
}

class HwachaCnt extends Bundle
{
  val cnt = UInt(width = SZ_VLEN)
  val last = Bool()
}

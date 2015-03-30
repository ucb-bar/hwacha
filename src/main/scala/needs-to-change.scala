package hwacha

import Chisel._
import Node._
import Constants._

//-------------------------------------------------------------------------\\
// decoded information types
//-------------------------------------------------------------------------\\

class RegInfo extends Bundle
{
  val zero = Bool()
  val float = Bool()
  val id = Bits(width = SZ_BREGLEN)
}

class DecodedRegister extends Bundle
{
  val vs = new RegInfo
  val vt = new RegInfo
  val vr = new RegInfo
  val vd = new RegInfo
}


//-------------------------------------------------------------------------\\
// deck types
//-------------------------------------------------------------------------\\

class DeckOp extends VMUOpCmd
{
  val utidx = UInt(width = SZ_VLEN)
  val float = Bool()
  val reg = new DecodedRegister
}

class BWQInternalEntry extends HwachaBundle
{
  // Lower eidx bits are evident from the bank ID and therefore omitted
  val tag = UInt(width = SZ_VLEN - log2Up(nbanks))
  val data = Bits(width = SZ_DATA)
}

class HwachaConfigIO extends Bundle
{
  val prec = Bits(OUTPUT, SZ_PREC)
  val bactive = Bits(OUTPUT, SZ_BANK)
  val bcnt = UInt(OUTPUT, SZ_BCNT)
  val nxregs = UInt(OUTPUT, SZ_REGCNT)
  val nfregs = UInt(OUTPUT, SZ_REGCNT)
  val xstride = UInt(OUTPUT, SZ_REGLEN)
  val fstride = UInt(OUTPUT, SZ_REGLEN)
  val xfsplit = UInt(OUTPUT, SZ_BREGLEN)
}

object Compaction extends Compaction
{
  def repack_float_d(n: Bits*) = Cat(Bits(1,1), n(0))
  def repack_float_s(n: Bits*) = Cat(n(1)(32), n(0)(32), n(1)(31,0), n(0)(31,0))
  def repack_float_h(n: Bits*) = Cat(Bits("b11",2), n(3), n(2), n(1), n(0))

  def pack_float_d(n: Bits, i: Int): Bits = i match {
    case 0 => (Bits(1,1) ## n(64,0))
    case _ => Bits(0, 66)
  }
  def pack_float_s(n: Bits, i: Int): Bits = i match {
    case 0 => Cat(Bits(1,1), n(32), Bits("hFFFFFFFF",32), n(31,0))
    case 1 => Cat(n(32), Bits(1,1), n(31,0), Bits("hFFFFFFFF",32))
    case _ => Bits(0)
  }
  def pack_float_h(n: Bits, i: Int): Bits = i match {
    case 0 => Cat(Bits("h3FFFFFFFFFFFF",50), n(15,0))
    case 1 => Cat(Bits("h3FFFFFFFF",34), n(15,0), Bits("hFFFF",16))
    case 2 => Cat(Bits("h3FFFF",18), n(15,0), Bits("hFFFFFFFF",32))
    case 3 => Cat(Bits("b11",2), n(15,0), Bits("hFFFFFFFFFFFF",48))
    case _ => Bits(0)
  }

  def unpack_float_d(n: Bits, i: Int): Bits = i match {
    case 0 => (n(64,0))
    case _ => Bits(0)
  }
  def unpack_float_s(n: Bits, i: Int): Bits = i match {
    case 0 => (n(64) ## n(31,0))
    case 1 => (n(65) ## n(63,32))
    case _ => Bits(0)
  }
  def unpack_float_h(n: Bits, i: Int): Bits = i match {
    case 0 => n(15,0)
    case 1 => n(31,16)
    case 2 => n(47,32)
    case 3 => n(63,48)
    case _ => Bits(0)
  }

  def expand_mask(m: Bits) = Cat(
    m(3) & m(2),
    m(1) & m(0),
    Fill(16, m(3)),
    Fill(16, m(2)),
    Fill(16, m(1)),
    Fill(16, m(0)))
}

trait Compaction
{
  def repack_float_d(n: Bits*): Bits
  def repack_float_s(n: Bits*): Bits
  def repack_float_h(n: Bits*): Bits
  def pack_float_d(n: Bits, i: Int): Bits
  def pack_float_s(n: Bits, i: Int): Bits
  def pack_float_h(n: Bits, i: Int): Bits
  def unpack_float_d(n: Bits, i: Int): Bits
  def unpack_float_s(n: Bits, i: Int): Bits
  def unpack_float_h(n: Bits, i: Int): Bits
}

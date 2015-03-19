package hwacha

import Chisel._
import Constants._
import uncore.constants.MemoryOpConstants._

abstract class VMUModule(clock: Clock = null, _reset: Bool = null)
  extends HwachaModule(clock, _reset) with VMUParameters
abstract class VMUBundle extends HwachaBundle with VMUParameters

class VMUOpCmd extends Bundle {
  val fn = Bits(width = SZ_VMU_OP)
  val mt = Bits(width = MT_SZ)
  val vlen = UInt(width = SZ_VLEN)
}

class VMUOpAddr extends Bundle {
  val base = UInt(width = SZ_ADDR)
  val stride = UInt(width = SZ_VSTRIDE)
}

class DecodedMemType extends Bundle {
  val b = Bool() // byte
  val h = Bool() // halfword
  val w = Bool() // word
  val d = Bool() // doubleword
  val unsigned = Bool()

  def shamt(dummy: Int = 0): UInt =
    Cat(this.w || this.d, this.h || this.d).toUInt
}

object DecodedMemType {
  def apply[T <: Data](mt: T): DecodedMemType = {
    val b = (mt === MT_B)
    val h = (mt === MT_H)
    val w = (mt === MT_W)
    val d = (mt === MT_D)
    val bu = (mt === MT_BU)
    val hu = (mt === MT_HU)
    val wu = (mt === MT_WU)

    val mtd = new DecodedMemType
    mtd.b := (b || bu)
    mtd.h := (h || hu)
    mtd.w := (w || wu)
    mtd.d := d
    mtd.unsigned := (bu || hu || wu)
    mtd
  }
}

class VVAQEntry extends UInt with VMUParameters { setWidth(maxAddrBits) }
class VVAQIO extends DecoupledIO(new VVAQEntry)

class VAQLaneIO extends VMUBundle {
  val q = new VVAQIO
  val vala = new LookAheadPortIO(valaBits)
  val pala = new LookAheadPortIO(palaBits)
}

class VVAPFQEntry extends VMUBundle {
  val addr = UInt(width = maxAddrBits)
  val write = Bool()
}
class VVAPFQIO extends DecoupledIO(new VVAPFQEntry)

class TLBQueryIO extends VMUBundle {
  val vpn = Decoupled(UInt(width = vpnBits))
  val ppn = UInt(INPUT, ppnBits)
  val xcpt = new Bundle {
    val ld = Bool(INPUT)
    val st = Bool(INPUT)
  }
}

class VPAQEntry extends VMUBundle {
  val addr = UInt(width = paddrBits)
  val ecnt = UInt(width = SZ_VLEN) 
}

trait VMUMetadataBase extends VMUParameters {
  val ecnt = UInt(width = tlByteAddrBits)
  val eskip = UInt(width = tlByteAddrBits)
}
trait VMUMetadataLoad extends VMUMetadataBase {
  val eidx = UInt(width = SZ_VLEN)
}
trait VMUMetadataStore extends VMUMetadataBase {
  val first = Bool()
  val last = Bool()
  val offset = UInt(width = tlByteAddrBits)
}

class VMULoadMetaEntry extends VMUBundle with VMUMetadataLoad
class VMUStoreMetaEntry extends VMUBundle with VMUMetadataStore
class VMUMetaUnion extends VMUBundle
  with VMUMetadataLoad with VMUMetadataStore

class VMUMemOp[T <: Data](gen: T) extends VMUBundle {
  val fn = Bits(width = M_SZ)
  val mt = Bits(width = MT_SZ)
  val addr = UInt(width = paddrBits)
  val meta = gen.clone

  override def clone: this.type = new VMUMemOp(gen).asInstanceOf[this.type]
}

class MetaReadIO[T <: Data](gen: T) extends VMUBundle {
  val valid = Bool(OUTPUT)
  val tag = UInt(OUTPUT, tagBits)
  val data = gen.clone.asInput
}

class MetaWriteIO[T <: Data](gen: T) extends VMUBundle {
  val valid = Bool(OUTPUT)
  val ready = Bool(INPUT)
  val data = gen.clone.asOutput
  val tag = UInt(INPUT, tagBits)
}

abstract class VMUData extends VMUBundle {
  val data = Bits(width = tlDataBits)
}

class VMULoadData extends VMUData {
  val tag = UInt(width = tagBits)
}

class VMUStoreData extends VMUData {
  val mask = Bits(width = tlDataBytes)
}

class VLDQEntry extends VMUData {
  val meta = new VMULoadMetaEntry
}
class VLDQIO extends DecoupledIO(new VLDQEntry)

class VSDQEntry extends UInt with VMUParameters { setWidth(tlDataBits) }
class VSDQIO extends DecoupledIO(new VSDQEntry)

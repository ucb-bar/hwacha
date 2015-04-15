package hwacha

import Chisel._
import Constants._
import uncore.constants.MemoryOpConstants._

abstract class VMUModule(clock: Clock = null, _reset: Bool = null)
  extends HwachaModule(clock, _reset) with VMUParameters
abstract class VMUBundle extends HwachaBundle with VMUParameters

class VMUFn extends Bundle {
  val mode = Bits(width = SZ_VMU_MODE)
  val cmd = Bits(width = M_SZ)
  val mt = Bits(width = MT_SZ)
}

class VMUAuxVector extends HwachaBundle {
  val stride = UInt(width = regLen)
}

object VMUAuxVector {
  def apply(stride: UInt) = {
    val aux = new VMUAuxVector
    aux.stride := stride
    aux
  }
}

class VMUAuxScalar extends HwachaBundle {
  val data = Bits(width = regLen)
  val id = UInt(width = log2Up(nSRegs))
}

object VMUAuxScalar {
  def apply(data: Bits, id: UInt) = {
    val aux = new VMUAuxScalar
    aux.data := data
    aux.id := id
    aux
  }
}

class VMUAux extends Bundle {
  val union = Bits(width = math.max(
    new VMUAuxVector().toBits.getWidth,
    new VMUAuxScalar().toBits.getWidth))

  def vector(dummy: Int = 0) = new VMUAuxVector().fromBits(this.union)
  def scalar(dummy: Int = 0) = new VMUAuxScalar().fromBits(this.union)
}

abstract class VMUOpBase extends VMUBundle {
  val fn = new VMUFn
  val vlen = UInt(width = bVLen)
  val base = UInt(width = maxAddrBits)
}

class VMUOp extends VMUOpBase {
  val aux = new VMUAux
}

class DecodedMemCommand extends Bundle {
  val load = Bool()
  val store = Bool()
  val amo = Bool()
  val pf = Bool()

  val read = Bool()
  val write = Bool()
}

object DecodedMemCommand {
  def apply[T <: Bits](cmd: T): DecodedMemCommand = {
    val res = new DecodedMemCommand
    res.load := (cmd === M_XRD)
    res.store := (cmd === M_XWR)
    res.amo := isAMO(cmd)
    res.pf := isPrefetch(cmd)

    res.read := (res.load || res.amo)
    res.write := (res.store || res.amo)
    res
  }
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

class VVAQEntry extends VMUBundle {
  val addr = UInt(width = maxAddrBits)
}
class VVAQIO extends DecoupledIO(new VVAQEntry)

class VVAPFQEntry extends VVAQEntry {
  val store = Bool()
}
class VVAPFQIO extends DecoupledIO(new VVAPFQEntry)

class VPAQEntry extends VMUBundle {
  val addr = UInt(width = paddrBits)
  val ecnt = UInt(width = bVLen) 
}
class VPAQIO extends DecoupledIO(new VPAQEntry)

class VMULookAheadIO extends VMUBundle {
  val vala = new CounterLookAheadIO
  val pala = new CounterLookAheadIO
}

trait VMUMetadataBase extends VMUBundle {
  val ecnt = UInt(width = tlByteAddrBits)
  val eskip = UInt(width = tlByteAddrBits)
}
trait VMUMetadataLoad extends VMUMetadataBase {
  val eidx = UInt(width = bVLen)
}
trait VMUMetadataStore extends VMUMetadataBase {
  val first = Bool()
  val last = Bool()
  val offset = UInt(width = tlByteAddrBits)
}

class VMULoadMetaEntry extends VMUMetadataLoad
class VMUStoreMetaEntry extends VMUMetadataStore
class VMUMetaUnion extends VMUMetadataLoad with VMUMetadataStore

trait VMUMemOp extends VMUBundle {
  val cmd = Bits(width = M_SZ)
  val mt = Bits(width = MT_SZ)
  val addr = UInt(width = paddrBits)
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

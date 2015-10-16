package hwacha

import Chisel._

abstract class VMUModule(clock: Clock = null, _reset: Bool = null)
  extends HwachaModule(clock, _reset) with VMUParameters
abstract class VMUBundle extends HwachaBundle with VMUParameters

class VMUMemFn extends Bundle {
  val cmd = Bits(width = M_SZ)
  val mt = Bits(width = MT_SZ)
}

class VMUFn extends Bundle {
  val mode = Bits(width = SZ_VMU_MODE)
  val cmd = Bits(width = M_SZ)
  val mt = Bits(width = MT_SZ)
}

class VMUAuxVector extends HwachaBundle {
  val stride = UInt(width = regLen)
}

object VMUAuxVector {
  def apply(stride: UInt): VMUAuxVector = {
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
  def apply(data: Bits, id: UInt): VMUAuxScalar = {
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

class VMUOp extends VMUBundle {
  val fn = new VMUFn
  val vlen = UInt(width = bVLen)
  val base = UInt(width = bVAddr)
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
  def apply[T <: UInt](cmd: T): DecodedMemCommand = {
    val dec = new DecodedMemCommand
    dec.load := (cmd === M_XRD)
    dec.store := (cmd === M_XWR)
    dec.amo := isAMO(cmd)
    dec.pf := isPrefetch(cmd)
    dec.read := (dec.load || dec.amo)
    dec.write := (dec.store || dec.amo)
    dec
  }
}

class DecodedMemType extends Bundle {
  val b = Bool() // byte
  val h = Bool() // halfword
  val w = Bool() // word
  val d = Bool() // doubleword
  val signed = Bool()

  def shift(dummy: Int = 0): UInt =
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

    val dec = new DecodedMemType
    dec.b := (b || bu)
    dec.h := (h || hu)
    dec.w := (w || wu)
    dec.d := d
    dec.signed := (b || h || w || d)
    dec
  }
}

/**********************************************************************/

class VVAQEntry extends VMUBundle {
  val addr = UInt(width = bVAddrExtended)
}
class VVAQIO extends DecoupledIO(new VVAQEntry)

trait VMUAddr extends VMUBundle {
  val addr = UInt(width = bPAddr)
}
class VPAQEntry extends VMUAddr
class VPAQIO extends DecoupledIO(new VPAQEntry)


trait VMUData extends VMUBundle {
  val data = Bits(width = tlDataBits)
}

class VSDQEntry extends VMUData
class VSDQIO extends DecoupledIO(new VSDQEntry)

class VCUEntry extends VMUBundle {
  val ecnt = UInt(width = bVLen)
}
class VCUIO extends ValidIO(new VCUEntry)


trait VMUTag extends VMUBundle {
  val tag = UInt(width = bTag)
}

class VMLUData extends VMUData with VMUTag {
  val last = Bool()
}

class VLTEntry extends VMUMetaIndex {
  val mask = Bits(width = tlDataBytes >> 1)
  val shift = Bool()
}

class VLDQEntry extends VMUData {
  val meta = new VLTEntry {
    val last = Bool()
  }
}
class VLDQIO extends DecoupledIO(new VLDQEntry)

/**********************************************************************/

class PredEntry extends HwachaBundle {
  val pred = Bits(width = nPredSet)
}

class VMUMaskEntry_0 extends VMUBundle {
  val pred = Bool()
  val ecnt = UInt(width = bVLen)
  val last = Bool()

  val unit = new Bundle {
    val page = Bool() /* Entry is final for current page */
  }
  val nonunit = new Bundle {
    val shift = UInt(width = log2Up(nPredSet))
  }
}
class VMUMaskIO_0 extends DecoupledIO(new VMUMaskEntry_0)

class VMUMaskEntry_1 extends VMUBundle {
  val data = Bits(width = tlDataBytes >> 1)
  val vsdq = Bool()
}
class VMUMaskIO_1 extends DecoupledIO(new VMUMaskEntry_1) {
  val meta = new VMUBundle {
    val eoff = UInt(INPUT, tlByteAddrBits - 1)
    val last = Bool(INPUT)
  }
}

/**********************************************************************/

/* Encodes 2^n as 0; values 1 to (2^n-1) are represented as normal. */
class CInt(n: Int) extends Bundle {
  val raw = UInt(width = n)
  def encode[T <: UInt](x: T) {
    assert(x != UInt(0), "CInt: invalid value")
    raw := x
  }
  def decode(dummy: Int = 0): UInt = Cat(raw === UInt(0), raw)
}

trait VMUMetaCount extends VMUBundle {
  val ecnt = new CInt(tlByteAddrBits-1)
}
trait VMUMetaPadding extends VMUBundle {
  val epad = UInt(width = tlByteAddrBits)
}
trait VMUMetaIndex extends VMUBundle {
  val eidx = UInt(width = bVLen)
}
trait VMUMetaStore extends VMUBundle {
  val last = Bool()
  val vsdq = Bool()
}

/**********************************************************************/

trait VMUMemOp extends VMUAddr {
  val fn = new VMUMemFn
}

class VMUMetaAddr extends VMUMetaCount
  with VMUMetaPadding with VMUMetaStore {
  val mask = UInt(width = tlDataBytes >> 1)
}

class AGUEntry extends VMUMemOp {
  val meta = new VMUMetaAddr with VMUMetaIndex
}

class AGUIO extends DecoupledIO(new AGUEntry)

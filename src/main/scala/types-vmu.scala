package hwacha

import Chisel._
import freechips.rocketchip.config._

abstract class VMUModule(clock: Clock = null, _reset: Bool = null)(implicit p: Parameters)
  extends HwachaModule(clock, _reset)(p) with VMUParameters
abstract class VMUBundle(implicit p: Parameters)
  extends HwachaBundle()(p) with VMUParameters

class VMUMemFn extends Bundle {
  val cmd = Bits(width = M_SZ)
  val mt = Bits(width = MT_SZ)
}

class VMUFn extends Bundle {
  val mode = Bits(width = SZ_VMU_MODE)
  val cmd = Bits(width = M_SZ)
  val mt = Bits(width = MT_SZ)
}

class VMUOpBase(implicit p: Parameters) extends VMUBundle()(p) {
  val fn = new VMUFn
  val base = UInt(width = bVAddrExtended)
  val stride = UInt(width = regLen)
  val status = new freechips.rocketchip.rocket.MStatus
}

class VMUOp(implicit p: Parameters) extends VMUOpBase()(p) with SingleLaneVLen
class VMUOpML(implicit p: Parameters) extends VMUOpBase()(p) with MultiLaneVLen

class DecodedMemCommand extends Bundle {
  val bits = Bits(width = M_SZ)

  val load = Bool()
  val store = Bool()
  val amo = Bool()
  val pf = Bool()

  val read = Bool()
  val write = Bool()
}

object DecodedMemCommand {
  def apply[T <: UInt](cmd: T): DecodedMemCommand = {
    cmd.suggestName("cmdWire")
    val dec = Wire(new DecodedMemCommand)
    dec.suggestName("decWire")
    dec.bits := cmd
    dec.load := (cmd === M_XRD).suggestName("loadWire")
    dec.store := (cmd === M_XWR).suggestName("storeWire")
    dec.amo := isAMO(cmd)
    dec.pf := isPrefetch(cmd)
    dec.read := (dec.load || dec.amo).suggestName("readWire")
    dec.write := (dec.store || dec.amo).suggestName("writeWire")
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
    Cat(this.w || this.d, this.h || this.d).asUInt
}

object DecodedMemType {
  def apply[T <: UInt](mt: T): DecodedMemType = {
    mt.suggestName("mtWire")
    val b = (mt === MT_B).suggestName("bWire")
    val h = (mt === MT_H).suggestName("hWire")
    val w = (mt === MT_W).suggestName("wWire")
    val d = (mt === MT_D).suggestName("dWire")
    val bu = (mt === MT_BU).suggestName("buWire")
    val hu = (mt === MT_HU).suggestName("huWire")
    val wu = (mt === MT_WU).suggestName("wuWire")

    val dec = Wire(new DecodedMemType)
    dec.suggestName("decWire")
    dec.b := (b || bu).suggestName("decbWire")
    dec.h := (h || hu).suggestName("dechWire")
    dec.w := (w || wu).suggestName("decwWire")
    dec.d := d
    dec.signed := ((b || h).suggestName("bhWire") || (w || d).suggestName("wdWire")).suggestName("signedWire")
    dec
  }
}

/**********************************************************************/

class VVAQEntry(implicit p: Parameters) extends VMUBundle()(p) {
  val addr = UInt(width = bVAddrExtended)
}
class VVAQIO(implicit p: Parameters) extends DecoupledIO(new VVAQEntry()(p)) {
}

trait VMUAddr extends VMUBundle {
  val addr = UInt(width = bPAddr)
}
class VPAQEntry(implicit p: Parameters) extends VMUAddr
class VPAQIO(implicit p: Parameters) extends DecoupledIO(new VPAQEntry()(p)) {
}


trait VMUData extends VMUBundle {
  val data = Bits(width = tlDataBits)
}

class VSDQEntry(implicit p: Parameters) extends VMUData
class VSDQIO(implicit p: Parameters) extends DecoupledIO(new VSDQEntry()(p)) {
}

class VCUEntry(implicit p: Parameters) extends VMUBundle()(p) {
  val ecnt = UInt(width = bVLen)
}
class VCUIO(implicit p: Parameters) extends ValidIO(new VCUEntry()(p))


trait VMUTag extends VMUBundle {
  val tag = UInt(width = bVMUTag)
}

class VMULoadData(implicit p: Parameters) extends VMUData with VMUTag

class VMTLoadEntry(implicit p: Parameters) extends VMUBundle()(p)
  with VMUMetaIndex with VMUMetaPadding with VMUMetaMask with VLUSelect

class VMTStoreEntry(implicit p: Parameters) extends VMUBundle()(p)
  with VMUMetaCount

class VMTEntry(implicit p: Parameters) extends VMUBundle()(p) {
  val union = Bits(math.max(
    new VMTLoadEntry().getWidth,
    new VMTStoreEntry().getWidth).W)

  def load(d: Int = 0) = new VMTLoadEntry().fromBits(this.union)
  def store(d: Int = 0) = new VMTStoreEntry().fromBits(this.union)
}

class VLDQEntry(implicit p: Parameters) extends VMUData {
  val meta = new VMTLoadEntry
}
class VLDQIO(implicit p: Parameters) extends DecoupledIO(new VLDQEntry()(p)) {
}

/**********************************************************************/

class PredEntry(implicit p: Parameters) extends HwachaBundle()(p) {
  val pred = Bits(width = nStrip)
}

class VMUMaskEntry_0(implicit p: Parameters) extends VMUBundle()(p) {
  val pred = Bool()
  val ecnt = UInt(width = bVLen)
  val last = Bool()

  val unit = new Bundle {
    val page = Bool() /* Entry is final for current page */
  }
  val nonunit = new Bundle {
    val shift = UInt(width = bStrip)
  }
}
class VMUMaskIO_0(implicit p: Parameters) extends DecoupledIO(new VMUMaskEntry_0()(p)) {
}

class VMUMaskEntry_1(implicit p: Parameters) extends VMUBundle()(p) {
  val data = Bits(width = tlDataBytes >> 1)
  val vsdq = Bool()
}
class VMUMaskIO_1(implicit p: Parameters) extends DecoupledIO(new VMUMaskEntry_1()(p)) {
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
    //COLIN FIXME: this was not being emitted in chisel2 and always fires in chisel3
    //assert(x != UInt(0), "CInt: invalid value")
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
trait VMUMetaMask extends VMUBundle {
  val mask = Bits(width = nStrip)
}
trait VMUMetaStore extends VMUBundle {
  val last = Bool()
  val vsdq = Bool()
}

/**********************************************************************/

trait VMUMemOp extends VMUAddr {
  val fn = new VMUMemFn
}

class VMUMetaAddr(implicit p: Parameters) extends VMUMetaCount
  with VMUMetaPadding with VMUMetaMask with VMUMetaStore with VLUSelect

class VMUAddrEntry(implicit p: Parameters) extends VMUMemOp {
  val meta = new VMUMetaAddr with VMUMetaIndex
}
class VMUAddrIO(implicit p: Parameters) extends DecoupledIO(new VMUAddrEntry()(p)) {
}

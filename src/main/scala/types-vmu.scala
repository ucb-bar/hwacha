package hwacha

import Chisel._
import cde.Parameters

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
  val status = new rocket.MStatus
}

class VMUOp(implicit p: Parameters) extends VMUOpBase()(p) with SingleLaneVLen
class VMUOpML(implicit p: Parameters) extends VMUOpBase()(p) with MultiLaneVLen

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
    val dec = Wire(new DecodedMemCommand)
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
    Cat(this.w || this.d, this.h || this.d).asUInt
}

object DecodedMemType {
  def apply[T <: UInt](mt: T): DecodedMemType = {
    val b = (mt === MT_B)
    val h = (mt === MT_H)
    val w = (mt === MT_W)
    val d = (mt === MT_D)
    val bu = (mt === MT_BU)
    val hu = (mt === MT_HU)
    val wu = (mt === MT_WU)

    val dec = Wire(new DecodedMemType)
    dec.b := (b || bu)
    dec.h := (h || hu)
    dec.w := (w || wu)
    dec.d := d
    dec.signed := (b || h || w || d)
    dec
  }
}

/**********************************************************************/

class VVAQEntry(implicit p: Parameters) extends VMUBundle()(p) {
  val addr = UInt(width = bVAddrExtended)
}
class VVAQIO(implicit p: Parameters) extends DecoupledIO(new VVAQEntry()(p)) {
  override def cloneType = new VVAQIO().asInstanceOf[this.type]
}

trait VMUAddr extends VMUBundle {
  val addr = UInt(width = bPAddr)
}
class VPAQEntry(implicit p: Parameters) extends VMUAddr
class VPAQIO(implicit p: Parameters) extends DecoupledIO(new VPAQEntry()(p)) {
  override def cloneType = new VPAQIO().asInstanceOf[this.type]
}


trait VMUData extends VMUBundle {
  val data = Bits(width = tlDataBits)
}

class VSDQEntry(implicit p: Parameters) extends VMUData
class VSDQIO(implicit p: Parameters) extends DecoupledIO(new VSDQEntry()(p)) {
  override def cloneType = new VSDQIO().asInstanceOf[this.type]
}

class VCUEntry(implicit p: Parameters) extends VMUBundle()(p) {
  val ecnt = UInt(width = bVLen)
}
class VCUIO(implicit p: Parameters) extends ValidIO(new VCUEntry()(p))


trait VMUTag extends VMUBundle {
  val tag = UInt(width = bVMUTag)
}

class VMULoadData(implicit p: Parameters) extends VMUData with VMUTag

class VLTEntry(implicit p: Parameters) extends VMUBundle()(p)
  with VMUMetaIndex with VMUMetaPadding with VMUMetaMask with VLUSelect

class VLDQEntry(implicit p: Parameters) extends VMUData {
  val meta = new VLTEntry
}
class VLDQIO(implicit p: Parameters) extends DecoupledIO(new VLDQEntry()(p)) {
  override def cloneType = new VLDQIO().asInstanceOf[this.type]
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
  override def cloneType = new VMUMaskIO_0().asInstanceOf[this.type]
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
  override def cloneType = new VMUMaskIO_1().asInstanceOf[this.type]
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
  override def cloneType = new CInt(n).asInstanceOf[this.type]
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

class VMUAddrMemResp(implicit p: Parameters) extends VMUBundle with VMUTag

class VMUAddrMemIf(implicit p: Parameters) extends VMUBundle {
  val req = new Bool(INPUT)
  val resp = new ValidIO(new VMUAddrMemResp()).asInput
}
class VMUAddrEntry(implicit p: Parameters) extends VMUMemOp {
  val meta = new VMUMetaAddr with VMUMetaIndex with VMUTag
}
class VMUAddrIO(implicit p: Parameters) extends DecoupledIO(new VMUAddrEntry()(p)) {
  val memif = new VMUAddrMemIf()

  override def cloneType = new VMUAddrIO().asInstanceOf[this.type]
}

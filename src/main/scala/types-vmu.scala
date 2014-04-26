package hwacha

import Chisel._
import Constants._
import uncore.constants.MemoryOpConstants._

//--------------------------------------------------------------------\\
// vmu queue I/O
//--------------------------------------------------------------------\\

class VMUCommandIO extends DecoupledIO(new VMUOp)
class VMUAddressIO extends DecoupledIO(new VMUAddressOp)

class VVAQIO(implicit conf: HwachaConfiguration) extends DecoupledIO[UInt](UInt(width = conf.vmu.sz_addr))
class VVAPFQIO(implicit conf: HwachaConfiguration) extends DecoupledIO[VVAPFQEntry](new VVAPFQEntry()(conf))
class VPAQIO(implicit conf: HwachaConfiguration) extends DecoupledIO[VPAQEntry](new VPAQEntry()(conf))

class VAQLaneIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val q = new VVAQIO
  val vala = new LookAheadPortIO(log2Down(conf.vmu.nvvaq) + 1)
  val pala = new LookAheadPortIO(log2Down(conf.vmu.nvpaq) + 1)
}

class VSDQIO(implicit conf: HwachaConfiguration) extends DecoupledIO[Bits](Bits(width = conf.vmu.sz_data))
class VLDQIO(implicit conf: HwachaConfiguration) extends DecoupledIO[VLDQEntry](new VLDQEntry()(conf))


//--------------------------------------------------------------------\\
// vmu miscellaneous I/O
//--------------------------------------------------------------------\\

class VATQIO(implicit conf: HwachaConfiguration) extends DecoupledIO[VATQEntry](new VATQEntry()(conf))

class VPAQMemIO(implicit conf: HwachaConfiguration)
  extends DecoupledIO[VPAQMemIf](new VPAQMemIf)

class VLDQMemIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val resp = Valid(new VLDQMemIf)
  val stall = Bool(INPUT)
}

class VMDBIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val tag = Bits(INPUT, conf.vmu.sz_tag)
  val info = Decoupled(new VMUMetadata)
}


//--------------------------------------------------------------------\\
// vmu types
//--------------------------------------------------------------------\\

abstract trait VMUBundle extends Bundle {
  implicit val conf: HwachaConfiguration
  override def clone = this.getClass.getConstructors.head.newInstance(conf).asInstanceOf[this.type]
}

class VMUFn extends Bundle
{
  val op = Bits(width = SZ_VMU_OP)
  val typ = Bits(width = MT_SZ)

  def utmemop(dummy: Int = 0) = !vmu_op_tvec(op)
  def lreq(dummy: Int = 0) = (op === VM_VLD) || (op === VM_ULD) || amoreq()
  def sreq(dummy: Int = 0) = (op === VM_VST) || (op === VM_UST)
  def amoreq(dummy: Int = 0) = is_mcmd_amo(vmu_op_mcmd(op))
  def signext(dummy: Int = 0) = (typ != MT_WU) && (typ != MT_HU) && (typ != MT_BU)
}

class VMUOp extends Bundle
{
  val vlen = UInt(width = SZ_VLEN)
  val fn = new VMUFn
}

class VMUAddressOp extends Bundle
{
  val base = UInt(width = SZ_ADDR)
  val stride = UInt(width = SZ_VSTRIDE)
}

class VMUMetadata extends Bundle
{
  val utidx = UInt(width = SZ_VLEN)
  val utcnt = UInt(width = 4) /* TODO: parameterize */
  val shift = UInt(width = 3) /* TODO: parameterize */
}

class MemOp(n: Int)(implicit val conf: HwachaConfiguration) extends VMUBundle
{
  val cmd = Bits(width = M_SZ)
  val typ = Bits(width = MT_SZ)
  val addr = UInt(width = n)
}

class VVAPFQEntry(implicit conf: HwachaConfiguration) extends MemOp(conf.as.vaddrBits)(conf)

class VATQEntry(implicit conf: HwachaConfiguration) extends MemOp(conf.as.vaddrBits)(conf)
{
  val meta = new VMUMetadata
}

class VPAQEntry(implicit conf: HwachaConfiguration) extends MemOp(conf.as.paddrBits)(conf)
{
  val meta = new VMUMetadata
}

class VPAQMemIf(implicit conf: HwachaConfiguration) extends MemOp(conf.as.paddrBits)(conf)
{
  val tag = Bits(width = conf.vmu.sz_tag)
}

class VLDQMemIf(implicit val conf: HwachaConfiguration) extends VMUBundle
{
  val tag = Bits(width = conf.vmu.sz_tag)
  val data = Bits(width = conf.vmu.sz_data)
}

class VLDQEntry(implicit val conf: HwachaConfiguration) extends VMUBundle
{
  val meta = new VMUMetadata
  val data = Bits(width = conf.vmu.sz_data)
}

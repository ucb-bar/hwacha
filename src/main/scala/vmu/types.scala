package hwacha
package vmunit

import Chisel._
import Constants._
import uncore.constants.MemoryOpConstants._
import uncore.constants.AddressConstants._

//--------------------------------------------------------------------\\
// vmu queue I/O
//--------------------------------------------------------------------\\

class VMUCommandIO extends DecoupledIO(new VMUOp)
class VMUAddressIO extends DecoupledIO(new VMUAddressOp)

class VVAQIO extends DecoupledIO(UInt(width = SZ_VMU_ADDR))
class VPAQIO extends DecoupledIO(new VPAQEntry)

class VAQLaneIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val q = new VVAQIO
  val vala = new LookAheadPortIO(log2Down(conf.nvvaq) + 1)
  val pala = new LookAheadPortIO(log2Down(conf.nvpaq) + 1)
}

class VSDQIO extends DecoupledIO(Bits(width = SZ_VMU_DATA))
class VLDQIO extends DecoupledIO(new VLDQEntry)


//--------------------------------------------------------------------\\
// vmu miscellaneous I/O
//--------------------------------------------------------------------\\

class VATQIO extends DecoupledIO(new VATQEntry)

class VPAQMemIO(implicit conf: HwachaConfiguration)
  extends DecoupledIO[VPAQMemIf](new VPAQMemIf)

class VLDQMemIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val resp = Valid(new VLDQMemIf)
  val stall = Bool(INPUT)
}

class VMDBIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val tag = Bits(INPUT, conf.vmu.SZ_TAG)
  val info = Decoupled(new VMUMetadata)
}


//--------------------------------------------------------------------\\
// vmu types
//--------------------------------------------------------------------\\

class VMUFn extends Bundle
{
  val float = Bool()
  val op = Bits(width = SZ_VMU_OP)
  val typ = Bits(width = MT_SZ)

  def utmemop(dummy: Int = 0) = !vmu_op_tvec(op)
  def lreq(dummy: Int = 0) = (op === VM_VLD) || (op === VM_ULD) || amoreq()
  def sreq(dummy: Int = 0) = (op === VM_VST) || (op === VM_UST)
  def amoreq(dummy: Int = 0) = is_mcmd_amo(vmu_op_mcmd(op))
}

class VMUOp extends Bundle
{
  val fn = new VMUFn
  val vlen = UInt(width = SZ_VLEN)
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

class MemOp(n: Int) extends Bundle
{
  val cmd = Bits(width = M_SZ)
  val typ = Bits(width = MT_SZ)
  val addr = UInt(width = n)
}

class VATQEntry extends MemOp(VADDR_BITS)
{
  val meta = new VMUMetadata
}

class VPAQEntry extends MemOp(PADDR_BITS)
{
  val meta = new VMUMetadata
}

class VPAQMemIf(implicit conf: HwachaConfiguration) extends MemOp(PADDR_BITS)
{
  val tag = Bits(width = conf.vmu.SZ_TAG)
  override def clone = new VPAQMemIf().asInstanceOf[this.type]
}

class VLDQMemIf(implicit conf: HwachaConfiguration) extends Bundle
{
  val tag = Bits(width = conf.vmu.SZ_TAG)
  val data = Bits(width = SZ_VMU_DATA)
  override def clone = new VLDQMemIf().asInstanceOf[this.type]
}

class VLDQEntry extends Bundle
{
  val meta = new VMUMetadata
  val data = Bits(width = SZ_VMU_DATA)
}

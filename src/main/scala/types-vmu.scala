package hwacha

import Chisel._
import Constants._
import uncore.constants.MemoryOpConstants._

//--------------------------------------------------------------------\\
// vmu queue I/O
//--------------------------------------------------------------------\\

class VMUCommandIO extends DecoupledIO(new VMUOp)
class VMUAddressIO extends DecoupledIO(new VMUAddressOp)

class VMUAddr extends UInt with UsesHwachaParameters { setWidth(confvmu.sz_addr) }
class VMUData extends UInt with UsesHwachaParameters { setWidth(confvmu.sz_data) }

class VVAQIO extends DecoupledIO[VMUAddr](new VMUAddr)
class VVAPFQIO extends DecoupledIO[VVAPFQEntry](new VVAPFQEntry)
class VPAQIO extends DecoupledIO[VPAQEntry](new VPAQEntry)

class VAQLaneIO extends HwachaBundle
{
  val q = new VVAQIO
  val vala = new LookAheadPortIO(sz_vala)
  val pala = new LookAheadPortIO(sz_pala)
}

class VSDQIO extends DecoupledIO[VMUData](new VMUData)
class VLDQIO extends DecoupledIO[VLDQEntry](new VLDQEntry)


//--------------------------------------------------------------------\\
// vmu miscellaneous I/O
//--------------------------------------------------------------------\\

class VATQIO extends DecoupledIO[VATQEntry](new VATQEntry)

class VPAQMemIO
  extends DecoupledIO[VPAQMemIf](new VPAQMemIf)

class VLDQMemIO extends Bundle
{
  val resp = Valid(new VLDQMemIf)
  val stall = Bool(INPUT)
}

class VMDBIO extends HwachaBundle
{
  val tag = Bits(INPUT, confvmu.sz_tag)
  val info = Decoupled(new VMUMetadata)
}


//--------------------------------------------------------------------\\
// vmu types
//--------------------------------------------------------------------\\

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

class VMUMetadata extends HwachaBundle
{
  private val n = log2Up(confvmu.max_utcnt)
  val utidx = UInt(width = SZ_VLEN)
  val utcnt = UInt(width = n) /* NOTE: 2^n encoded as 0 */
  val offset = UInt(width = n)
}

class MemOp(n: Int) extends HwachaBundle
{
  val cmd = Bits(width = M_SZ)
  val typ = Bits(width = MT_SZ)
  val addr = UInt(width = n)
}

class VVAPFQEntry extends MemOp(43)

class VATQEntry extends MemOp(43)
{
  val meta = new VMUMetadata
}

class VPAQEntry extends MemOp(32)
{
  val meta = new VMUMetadata
}

class VPAQMemIf extends MemOp(32)
{
  val tag = Bits(width = confvmu.sz_tag)
}

class VLDQMemIf extends HwachaBundle
{
  val tag = Bits(width = confvmu.sz_tag)
  val data = Bits(width = confvmu.sz_data)
}

class VLDQEntry extends HwachaBundle
{
  val meta = new VMUMetadata
  val data = Bits(width = confvmu.sz_data)
}

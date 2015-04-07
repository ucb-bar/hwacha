package hwacha

import Chisel._
import Node._
import Constants._
import uncore.constants.MemoryOpConstants._

//-------------------------------------------------------------------------\\
// vector functional unit fn types
//-------------------------------------------------------------------------\\

class VIUFn extends Bundle
{
  val dw = Bits(width = SZ_DW)
  val fp = Bits(width = SZ_FP)
  val op = Bits(width = SZ_VIU_OP)

  def dgate(valid: Bool) = this.clone.fromBits(
    List(dw, fp, op).map(DataGating.dgate(valid, _)).reverse.reduceLeft(Cat(_, _)))

  def dw_is(_dw: UInt) = dw === _dw
  def fp_is(fps: UInt*) = fps.toList.map(x => {fp === x}).reduceLeft(_ || _)
  def op_is(ops: UInt*) = ops.toList.map(x => {op === x}).reduceLeft(_ || _)
}

class VIXUFn(sz_op: Int) extends Bundle
{
  val dw = UInt(width = SZ_DW)
  val op = UInt(width = sz_op)

  def dgate(valid: Bool) = this.clone.fromBits(
    List(dw, op).map(DataGating.dgate(valid, _)).reverse.reduceLeft(Cat(_, _)))

  def dw_is(_dw: UInt) = dw === _dw
  def op_is(ops: UInt*): Bool = op_is(ops.toList)
  def op_is(ops: List[UInt]): Bool = ops.toList.map(x => {op === x}).reduce(_ || _)
  def is(_dw: UInt, ops: UInt*) = dw_is(_dw) && op_is(ops.toList)
}

class VIMUFn extends VIXUFn(SZ_VIMU_OP)
class VIDUFn extends VIXUFn(SZ_VIDU_OP)

class VFXUFn(sz_op: Int) extends Bundle
{
  val fp = UInt(width = SZ_FP)
  val rm = UInt(width = rocket.FPConstants.RM_SZ)
  val op = UInt(width = sz_op)

  def dgate(valid: Bool) = this.clone.fromBits(
    List(fp, rm, op).map(DataGating.dgate(valid, _)).reverse.reduceLeft(Cat(_, _)))

  def fp_is(fps: UInt*) = fps.toList.map(x => {fp === x}).reduceLeft(_ || _)
  def op_is(ops: UInt*) = ops.toList.map(x => {op === x}).reduceLeft(_ || _)
}

class VFMUFn extends VFXUFn(SZ_VFMU_OP)
class VFDUFn extends VFXUFn(SZ_VFDU_OP)
class VFCUFn extends VFXUFn(SZ_VFCU_OP)
class VFVUFn extends VFXUFn(SZ_VFVU_OP)

class VQUFn extends Bundle
{
  val latch = Bits(width = 2)
}

class VFn extends Bundle
{
  val union = Bits(width = List(
    new VIUFn().toBits.getWidth,
    new VIMUFn().toBits.getWidth,
    new VIDUFn().toBits.getWidth,
    new VFMUFn().toBits.getWidth,
    new VFDUFn().toBits.getWidth,
    new VFCUFn().toBits.getWidth,
    new VFVUFn().toBits.getWidth,
    new VMUFn().toBits.getWidth,
    new VQUFn().toBits.getWidth).reduceLeft((x, y) => if (x > y) x else y)
  )

  def viu(d: Int = 0) = new VIUFn().fromBits(this.union)
  def vimu(d: Int = 0) = new VIMUFn().fromBits(this.union)
  def vidu(d: Int = 0) = new VIDUFn().fromBits(this.union)
  def vfmu(d: Int = 0) = new VFMUFn().fromBits(this.union)
  def vfdu(d: Int = 0) = new VFDUFn().fromBits(this.union)
  def vfcu(d: Int = 0) = new VFCUFn().fromBits(this.union)
  def vfvu(d: Int = 0) = new VFVUFn().fromBits(this.union)
  def vmu(d: Int = 0) = new VMUFn().fromBits(this.union)
  def vqu(d: Int = 0) = new VQUFn().fromBits(this.union)
}


//-------------------------------------------------------------------------\\
// decoded information types
//-------------------------------------------------------------------------\\

class RegInfo extends HwachaBundle
{
  val valid = Bool()
  val scalar = Bool()
  val id = UInt(width = szvregs)
}

class DecodedRegisters extends HwachaBundle
{
  val vs1 = new RegInfo
  val vs2 = new RegInfo
  val vs3 = new RegInfo
  val vd = new RegInfo
}

class ScalarRegisters extends HwachaBundle
{
  val ss1 = Bits(width = SZ_D)
  val ss2 = Bits(width = SZ_D)
  val ss3 = Bits(width = SZ_D)
}

class DecodedInstruction extends HwachaBundle
{
  val fn = new VFn // union
  val reg = new DecodedRegisters
  val sreg = new ScalarRegisters
}


//-------------------------------------------------------------------------\\
// issue op
//-------------------------------------------------------------------------\\

class IssueOp extends DecodedInstruction
{
  val vlen = UInt(width = SZ_VLEN)
  val active = new Bundle {
    val vint = Bool()
    val vimul = Bool()
    val vidiv = Bool()
    val vfma = Bool()
    val vfdiv = Bool()
    val vfcmp = Bool()
    val vfconv = Bool()
    val vamo = Bool()
    val vldx = Bool()
    val vstx = Bool()
    val vld = Bool()
    val vst = Bool()
  }

  def enq_dcc(dummy: Int = 0) =
    active.vamo ||
    active.vldx || active.vstx ||
    active.vld || active.vst
}


//-------------------------------------------------------------------------\\
// sequencer op
//-------------------------------------------------------------------------\\

class VFU extends Bundle
{
  val viu = Bool()
  val vimu = Bool()
  val vidu = Bool()
  val vfmu = Bool()
  val vfdu = Bool()
  val vfcu = Bool()
  val vfvu = Bool()
  val vgu = Bool()
  val vcu = Bool()
  val vlu = Bool()
  val vsu = Bool()
  val vqu = Bool()
}

class SequencerEntry extends DecodedInstruction with SeqParameters
{
  val active = new VFU
  val first = Bool()
  val base = new DecodedRegisters
  val raw = Vec.fill(nseq){Bool()}
  val war = Vec.fill(nseq){Bool()}
  val waw = Vec.fill(nseq){Bool()}
  val rports = UInt(width = szRPorts)
  val wport = UInt(width = szWPortLatency)
}


//-------------------------------------------------------------------------\\
// bank, lane op
//-------------------------------------------------------------------------\\

class LaneOp extends HwachaBundle with LaneParameters
{
  val pred = Bits(width = nSlices)
}

class LaneDecoupledOp extends LaneOp
{
  val bank = UInt(width = log2Up(nbanks))
  val addr = UInt(width = math.max(log2Up(nSRAM), log2Up(nFF)))
  val selff = Bool() // select ff if true
}

class SRAMRFReadOp extends LaneOp
{
  val addr = UInt(width = log2Up(nSRAM))
}

class SRAMRFWriteOp extends LaneOp
{
  val addr = UInt(width = log2Up(nSRAM))
  val selg = Bool()
  val wsel = UInt(width = log2Up(nWSel))
}

class FFRFReadOp extends LaneOp
{
  val addr = UInt(width = log2Up(nFF))
}

class FFRFWriteOp extends LaneOp
{
  val addr = UInt(width = log2Up(nFF))
  val selg = Bool()
  val wsel = UInt(width = log2Up(nWSel))
}

class OPLOp extends LaneOp
{
  val global = new Bundle {
    val latch = Bits(width = nOPL)
    val selff = Bits(width = nOPL)
    val en = Bits(width = nOPL)
  }
  val local = new Bundle {
    val latch = Bits(width = 2)
    val selff = Bits(width = 2)
  }
}

class BRQOp extends LaneOp
{
  val selff = Bool() // select ff if true
  val zero = Bool()
}

class VIUOp extends LaneOp
{
  val fn = new VIUFn
  val eidx = UInt(width = SZ_VLEN)
}

class VIMUOp extends LaneOp
{
  val fn = new VIMUFn
}

class VIDUOp extends LaneDecoupledOp
{
  val fn = new VIDUFn
}

class VFMUOp extends LaneOp
{
  val fn = new VFMUFn
}

class VFDUOp extends LaneDecoupledOp
{
  val fn = new VFDUFn
}

class VFCUOp extends LaneOp
{
  val fn = new VFCUFn
}

class VFVUOp extends LaneOp
{
  val fn = new VFVUFn
}

class VQUOp extends LaneOp
{
  val fn = new VQUFn
}

class VGUOp extends LaneOp
{
  val fn = new VMUFn
}

class BankReadEntry extends Bundle with LaneParameters
{
  val d = Bits(width = SZ_DATA)
}

class BankWriteEntry extends Bundle with LaneParameters
{
  val d = Bits(width = SZ_DATA)
}

class VIXUAck extends Bundle with LaneParameters
{
  val pred = Bits(width = nSlices)
}

class VIUAck extends VIXUAck
class VIMUAck extends VIXUAck
class VIDUAck extends VIXUAck
class VGUAck extends VIXUAck
class VQUAck extends VIXUAck

class VFXUAck extends Bundle with LaneParameters
{
  val pred = Bits(width = nSlices)
  val exc = Bits(OUTPUT, rocket.FPConstants.FLAGS_SZ)
}

class VFMUAck extends VFXUAck
class VFDUAck extends VFXUAck
class VFCUAck extends VIXUAck // no exceptions can occur
class VFVUAck extends VFXUAck


//-------------------------------------------------------------------------\\
// decoupled cluster (dcc) types
//-------------------------------------------------------------------------\\

abstract class DCCOp extends Bundle
{
  val vlen = UInt(width = SZ_VLEN)
}

class DCCMemOp extends DCCOp
{
  val fn = new VMUFn
}

class BRQEntry extends Bundle
{
  val data = Bits(width = SZ_DATA)
}

class BWQEntry extends Bundle with LaneParameters
{
  val selff = Bool() // select ff if true
  val addr = UInt(width = math.max(log2Up(nSRAM), log2Up(nFF)))
  val data = Bits(width = SZ_DATA)
  val mask = Bits(width = SZ_DATA/8)

  def saddr(dummy: Int = 0) = addr(log2Up(nSRAM)-1, 0)
  def faddr(dummy: Int = 0) = addr(log2Up(nFF)-1, 0)
}

package hwacha

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

abstract class VXUModule(clock: Clock = null, _reset: Bool = null)(implicit p: Parameters)
  extends HwachaModule(clock, _reset)(p) with SeqParameters with LaneParameters with DCCParameters with ExpParameters
abstract class VXUBundle(implicit p: Parameters)
  extends HwachaBundle()(p) with SeqParameters with LaneParameters with DCCParameters with ExpParameters

//-------------------------------------------------------------------------\\
// vector functional unit fn types
//-------------------------------------------------------------------------\\

class VIUFn(implicit p: Parameters) extends VXUBundle()(p) {
  val dw = UInt(SZ_DW.W)
  val fp = UInt(SZ_FP.W)
  val op = UInt(SZ_VIU_OP.W)

  def dgate(valid: Bool) = DataGating.dgate(valid, this)

  def dw_is(_dw: UInt) = dw === _dw
  def fp_is(fps: UInt*) = fps.toList.map(x => {fp === x}).reduceLeft(_ || _)
  def op_is(ops: UInt*) = ops.toList.map(x => {op === x}).reduceLeft(_ || _)
}

class VIPUFn(implicit p: Parameters) extends VXUBundle()(p) {
  val op = UInt(SZ_VIPU_OP.W)
}

class VIXUFn(sz_op: Int)(implicit p: Parameters) extends VXUBundle()(p) {
  val dw = UInt(SZ_DW.W)
  val op = UInt(sz_op.W)

  def dgate(valid: Bool) = DataGating.dgate(valid, this)

  def dw_is(_dw: UInt) = dw === _dw
  def op_is(ops: UInt*): Bool = op_is(ops.toList)
  def op_is(ops: List[UInt]): Bool = ops.toList.map(x => {op === x}).reduce(_ || _)
  def is(_dw: UInt, ops: UInt*) = dw_is(_dw) && op_is(ops.toList)
}

class VIMUFn(implicit p: Parameters) extends VIXUFn(SZ_VIMU_OP)(p)
class VIDUFn(implicit p: Parameters) extends VIXUFn(SZ_VIDU_OP)(p)

class VFXUFn(sz_op: Int)(implicit p: Parameters) extends VXUBundle()(p) {
  val fp = UInt(SZ_FP.W)
  val rm = UInt(freechips.rocketchip.tile.FPConstants.RM_SZ.W)
  val op = UInt(sz_op.W)

  def dgate(valid: Bool) = DataGating.dgate(valid, this)

  def fp_is(fps: UInt*) = fps.toList.map(x => {fp === x}).reduceLeft(_ || _)
  def op_is(ops: UInt*) = ops.toList.map(x => {op === x}).reduceLeft(_ || _)
}

class VFMUFn(implicit p: Parameters) extends VFXUFn(SZ_VFMU_OP)(p)
class VFDUFn(implicit p: Parameters) extends VFXUFn(SZ_VFDU_OP)(p)
class VFCUFn(implicit p: Parameters) extends VFXUFn(SZ_VFCU_OP)(p)
class VFVUFn(implicit p: Parameters) extends VFXUFn(SZ_VFVU_OP)(p)

class VQUFn(implicit p: Parameters) extends VXUBundle()(p) {
  val latch = UInt(2.W)
}

class VRPUFn(implicit p: Parameters) extends VXUBundle()(p) {
  val op = UInt(SZ_VRPU_OP.W)
  def op_is(ops: UInt*) = ops.toList.map(x => {op === x}).reduceLeft(_ || _)
}

class VRFUFn(implicit p: Parameters) extends VXUBundle()(p) {
  val sd = UInt(bSRegs.W)
}

class VFn(implicit p: Parameters) extends VXUBundle()(p) {
  val union = UInt(List(
    new VIUFn().getWidth,
    new VIPUFn().getWidth,
    new VIMUFn().getWidth,
    new VIDUFn().getWidth,
    new VFMUFn().getWidth,
    new VFDUFn().getWidth,
    new VFCUFn().getWidth,
    new VFVUFn().getWidth,
    new VMUFn().getWidth,
    new VQUFn().getWidth,
    new VRPUFn().getWidth,
    new VRFUFn().getWidth).max.W
  )

  def viu(d: Int = 0) = this.union.asTypeOf(new VIUFn())
  def vipu(d: Int = 0) = this.union.asTypeOf(new VIPUFn())
  def vimu(d: Int = 0) = this.union.asTypeOf(new VIMUFn())
  def vidu(d: Int = 0) = this.union.asTypeOf(new VIDUFn())
  def vfmu(d: Int = 0) = this.union.asTypeOf(new VFMUFn())
  def vfdu(d: Int = 0) = this.union.asTypeOf(new VFDUFn())
  def vfcu(d: Int = 0) = this.union.asTypeOf(new VFCUFn())
  def vfvu(d: Int = 0) = this.union.asTypeOf(new VFVUFn())
  def vrpu(d: Int = 0) = this.union.asTypeOf(new VRPUFn())
  def vrfu(d: Int = 0) = this.union.asTypeOf(new VRFUFn())
  def vmu(d: Int = 0) = this.union.asTypeOf(new VMUFn())
  def vqu(d: Int = 0) = this.union.asTypeOf(new VQUFn())
}


//-------------------------------------------------------------------------\\
// register information types
//-------------------------------------------------------------------------\\

class RegId(bId: Int)(implicit p: Parameters) extends VXUBundle()(p) {
  val id = UInt(bId.W)
}

class RegInfo(bId: Int)(implicit p: Parameters) extends RegId(bId)(p) {
  val valid = Bool()
  val scalar = Bool()
  val pred = Bool()

  def is_scalar(d: Int = 0) = !pred && scalar
  def is_vector(d: Int = 0) = !pred && !scalar
  def is_pred(d: Int = 0) = pred
  def neg(d: Int = 0) = scalar
}

class BasePRegId(implicit p: Parameters) extends RegId(p(HwachaPredRegUInt))(p)
class BaseRegId(implicit p: Parameters) extends RegId(p(HwachaRegUInt))(p)
class BasePRegInfo(implicit p: Parameters) extends RegInfo(p(HwachaPredRegUInt))(p)
class BaseRegInfo(implicit p: Parameters) extends RegInfo(p(HwachaRegUInt))(p) with RegPrec

class BaseRegisters(implicit p: Parameters) extends VXUBundle()(p) {
  val vp = new BasePRegInfo
  val vs1 = new BaseRegInfo
  val vs2 = new BaseRegInfo
  val vs3 = new BaseRegInfo
  val vd = new BaseRegInfo
}

class PhysicalPRegId(implicit p: Parameters) extends RegId(p(HwachaPRFAddrUInt))(p)
class PhysicalRegId(implicit p: Parameters) extends RegId(p(HwachaRFAddrUInt))(p)
class PhysicalPRegInfo(implicit p: Parameters) extends RegInfo(p(HwachaPRFAddrUInt))(p)
class PhysicalRegInfo(implicit p: Parameters) extends RegInfo(p(HwachaRFAddrUInt))(p) with RegPrec

class PhysicalRegisterIds(implicit p: Parameters) extends VXUBundle()(p) {
  val vp = new PhysicalPRegId
  val vs1 = new PhysicalRegId
  val vs2 = new PhysicalRegId
  val vs3 = new PhysicalRegId
  val vd = new PhysicalRegId
}

class PhysicalRegisters(implicit p: Parameters) extends VXUBundle()(p) {
  val vp = new PhysicalPRegInfo
  val vs1 = new PhysicalRegInfo
  val vs2 = new PhysicalRegInfo
  val vs3 = new PhysicalRegInfo
  val vd = new PhysicalRegInfo
}

class ScalarRegisters(implicit p: Parameters) extends VXUBundle()(p) {
  val ss1 = UInt(regLen.W)
  val ss2 = UInt(regLen.W)
  val ss3 = UInt(regLen.W)
}


//-------------------------------------------------------------------------\\
// decoded instruction
//-------------------------------------------------------------------------\\

class DecodedInst(implicit p: Parameters) extends VXUBundle()(p) {
  val fn = new VFn // union
  val sreg = new ScalarRegisters
}

trait HasBaseRegs extends VXUBundle {
  val base = new BaseRegisters
}

trait HasPhysRegIds extends VXUBundle {
  val reg = new PhysicalRegisterIds
}

trait HasPhysRegs extends VXUBundle {
  val reg = new PhysicalRegisters
}


//-------------------------------------------------------------------------\\
// issue op
//-------------------------------------------------------------------------\\

class IssueType(implicit p: Parameters) extends VXUBundle()(p) {
  val vint = Bool()
  val vipred = Bool()
  val vimul = Bool()
  val vidiv = Bool()
  val vfma = Bool()
  val vfdiv = Bool()
  val vfcmp = Bool()
  val vfconv = Bool()
  val vrpred = Bool()
  val vrfirst = Bool()
  val vamo = Bool()
  val vldx = Bool()
  val vstx = Bool()
  val vld = Bool()
  val vst = Bool()

  def enq_vdu(dummy: Int = 0) = vidiv || vfdiv || vrpred || vrfirst
  def enq_vgu(dummy: Int = 0) = vamo || vldx || vstx
  def enq_vpu(dummy: Int = 0) = vamo || vldx || vstx || vld || vst
  def enq_vlu(dummy: Int = 0) = vamo || vldx || vld
  def enq_vsu(dummy: Int = 0) = vamo || vstx || vst
  def enq_dcc(dummy: Int = 0) = enq_vdu() || enq_vgu() || enq_vpu() || enq_vlu() || enq_vsu()
}

trait SingleLaneVLen extends HwachaBundle {
  val vlen = UInt(bVLen.W)
}

class VLenEntry(implicit p: Parameters) extends HwachaBundle()(p) {
  val active = Bool()
  val vlen = UInt(bVLen.W)
}

trait MultiLaneVLen extends HwachaBundle {
  val lane = Vec(nLanes, new VLenEntry)
}

class IssueOpBase(implicit p: Parameters) extends DecodedInst()(p)
    with HasBaseRegs with HasPhysRegIds {
  val active = new IssueType
}

class IssueOp(implicit p: Parameters) extends IssueOpBase()(p) with SingleLaneVLen
class IssueOpML(implicit p: Parameters) extends IssueOpBase()(p) with MultiLaneVLen


//-------------------------------------------------------------------------\\
// traits
//-------------------------------------------------------------------------\\

trait LaneOp extends VXUBundle with Rate {
  val strip = UInt((bStrip + bPack + 1).W)
}

trait BankPred extends VXUBundle {
  val pred = UInt(wPred.W)
  def active(dummy: Int = 0) = pred.orR
  def neg(cond: Bool) = Mux(cond, ~pred, pred)
}

trait PredMask extends VXUBundle {
  val mask = UInt(wPred.W)
}

trait BankMask extends VXUBundle {
  val mask = UInt((wBank/8).W)
}

trait BankData extends VXUBundle {
  val data = UInt(wBank.W)
}

trait MicroOp extends BankPred with Rate

//-------------------------------------------------------------------------\\
// confprec
//-------------------------------------------------------------------------\\

trait Rate extends VXUBundle {
  val rate = UInt(bRate.W)
}

trait RegPrec extends Bundle {
  val prec = UInt(SZ_PREC.W)
}

trait RegSelect extends VXUBundle {
  val idx = UInt(math.max(bPack, 1).W)
}

class PackInfo(implicit p: Parameters) extends VXUBundle()(p) with RegPrec with RegSelect
class PredPackInfo(implicit p: Parameters) extends VXUBundle()(p) with RegSelect

trait BankPack extends VXUBundle {
  val pack = new PackInfo
}
trait PredPack extends VXUBundle {
  val pack = new PredPackInfo
}

//-------------------------------------------------------------------------\\
// sequencer op
//-------------------------------------------------------------------------\\

class SeqType(implicit p: Parameters) extends VXUBundle()(p) {
  val viu = Bool()
  val vipu = Bool()
  val vimu = Bool()
  val vidu = Bool()
  val vfmu = Bool()
  val vfdu = Bool()
  val vfcu = Bool()
  val vfvu = Bool()
  val vrpu = Bool()
  val vrfu = Bool()
  val vpu = Bool()
  val vgu = Bool()
  val vcu = Bool()
  val vlu = Bool()
  val vsu = Bool()
  val vqu = Bool()
}

class MasterSeqEntry(implicit p: Parameters) extends DecodedInst()(p)
  with HasBaseRegs with Rate {
  val active = new SeqType
  val raw = Vec(nSeq, Bool())
  val war = Vec(nSeq, Bool())
  val waw = Vec(nSeq, Bool())
  val last = Bool()
  val rports = UInt(bRPorts.W)
  val wport = new Bundle {
    val sram = UInt(bWPortLatency.W)
    val pred = UInt(bPredWPortLatency.W)
  }
}

class SeqEntry(implicit p: Parameters) extends VXUBundle()(p)
  with HasPhysRegIds with PredPack {
  val vlen = UInt(bVLen.W)
  val eidx = new Bundle {
    val major = UInt((bMLVLen - bStrip).W)
    val minor = UInt(((1 << maxLStride) - 1).W)
  }
  val sidx = UInt((bVLen - bStrip).W)
  val age = UInt(bBanks.W)
}

class SeqSelect(implicit p: Parameters) extends VXUBundle()(p) {
  val vfmu = UInt(log2Up(nVFMU).W)
}

class SeqOp(implicit p: Parameters) extends DecodedInst()(p)
  with HasPhysRegs with LaneOp with PredPack {
  val active = new SeqType
  val select = new SeqSelect
  val eidx = UInt((bMLVLen - bStrip).W)
  val sidx = UInt((bVLen - bStrip).W)
  val rports = UInt(bRPorts.W)
  val wport = new Bundle {
    val sram = UInt(bWPortLatency.W)
    val pred = UInt(bPredWPortLatency.W)
  }
  val base = new Bundle {
    val vd = new BaseRegId
  }

  def active_vfmu(i: Int) = active.vfmu && select.vfmu === i.U
}

class SeqVPUOp(implicit p: Parameters) extends DecodedInst()(p)
  with HasPhysRegs with LaneOp with PredPack

class SeqVIPUOp(implicit p: Parameters) extends DecodedInst()(p)
  with HasPhysRegs with LaneOp with PredPack {
  val sidx = UInt((bVLen - bStrip).W)
  val base = new Bundle {
    val vd = new BaseRegId
  }
}


//-------------------------------------------------------------------------\\
// lane, micro op
//-------------------------------------------------------------------------\\

abstract class RFWriteOp(implicit p: Parameters) extends BaseRegId()(p) {
  val sidx = UInt((bVLen - bStrip).W)
}

class SRAMRFReadOp(implicit p: Parameters) extends VXUBundle()(p) with BankPack {
  val addr = UInt(log2Up(nSRAM).W)
}

class SRAMRFWriteOp(implicit p: Parameters) extends RFWriteOp()(p) with BankPack {
  val addr = UInt(log2Up(nSRAM).W)
  val selg = Bool()
  val wsel = UInt(log2Up(nWSel).W)
}

class FFRFReadOp(implicit p: Parameters) extends VXUBundle()(p) {
  val addr = UInt(log2Up(nFF).W)
}

class FFRFWriteOp(implicit p: Parameters) extends VXUBundle()(p) {
  val addr = UInt(log2Up(nFF).W)
  val selg = Bool()
  val wsel = UInt(log2Up(nWSel).W)
}

class PredRFReadOp(implicit p: Parameters) extends VXUBundle()(p) with PredPack {
  val addr = UInt(log2Up(nPred).W)
}

class PredRFGatedReadOp(implicit p: Parameters) extends PredRFReadOp()(p) {
  val off = Bool()
  val neg = Bool()
}

class PredRFWriteOp(implicit p: Parameters) extends RFWriteOp()(p) with PredPack {
  val addr = UInt(log2Up(nPred).W)
  val selg = Bool()
  val plu = Bool()
}

class OPLOp(implicit p: Parameters) extends VXUBundle()(p) {
  val selff = Bool()
}

class PDLOp(implicit p: Parameters) extends VXUBundle()(p)

class SRegOp(implicit p: Parameters) extends VXUBundle()(p) {
  val operand = UInt(regLen.W)
}

class XBarOp(implicit p: Parameters) extends VXUBundle()(p) {
  val pdladdr = UInt(log2Up(nGPDL).W)
}

class PXBarOp(implicit p: Parameters) extends VXUBundle()(p)

class VIUOp(implicit p: Parameters) extends VXUBundle()(p) {
  val fn = new VIUFn
  val eidx = UInt(bVLen.W)
}

class VIPUOp(implicit p: Parameters) extends VXUBundle()(p) {
  val fn = new VIPUFn
}

case class SharedLLOp(nOperands: Int)(implicit p: Parameters) extends VXUBundle()(p) {
  val sreg = Vec(nOperands, Bool())
}

class VIMUOp(implicit p: Parameters) extends SharedLLOp(2)(p) {
  val fn = new VIMUFn
}

class VFMUOp(implicit p: Parameters) extends SharedLLOp(3)(p) {
  val fn = new VFMUFn
}

class VFCUOp(implicit p: Parameters) extends SharedLLOp(2)(p) {
  val fn = new VFCUFn
}

class VFVUOp(implicit p: Parameters) extends SharedLLOp(1)(p) {
  val fn = new VFVUFn
}

class VQUOp(implicit p: Parameters) extends SharedLLOp(2)(p) {
  val fn = new VQUFn
}

class VPUOp(implicit p: Parameters) extends VXUBundle()(p)

class VGUOp(implicit p: Parameters) extends SharedLLOp(1)(p) {
  val fn = new VMUFn
}

class VSUOp(implicit p: Parameters) extends VXUBundle()(p) {
  val selff = Bool() // select ff if true
}

//-------------------------------------------------------------------------\\
// lane op
//-------------------------------------------------------------------------\\

class SRAMRFReadLaneOp(implicit p: Parameters) extends SRAMRFReadOp()(p) with LaneOp
class SRAMRFWriteLaneOp(implicit p: Parameters) extends SRAMRFWriteOp()(p) with LaneOp
class FFRFReadLaneOp(implicit p: Parameters) extends FFRFReadOp()(p) with LaneOp
class FFRFWriteLaneOp(implicit p: Parameters) extends FFRFWriteOp()(p) with LaneOp
class PredRFReadLaneOp(implicit p: Parameters) extends PredRFReadOp()(p) with LaneOp
class PredRFGatedReadLaneOp(implicit p: Parameters) extends PredRFGatedReadOp()(p) with LaneOp
class PredRFWriteLaneOp(implicit p: Parameters) extends PredRFWriteOp()(p) with LaneOp
class OPLLaneOp(implicit p: Parameters) extends OPLOp()(p) with LaneOp
class PDLLaneOp(implicit p: Parameters) extends PDLOp()(p) with LaneOp
class SRegLaneOp(implicit p: Parameters) extends SRegOp()(p) with LaneOp
class XBarLaneOp(implicit p: Parameters) extends XBarOp()(p) with LaneOp
class PXBarLaneOp(implicit p: Parameters) extends PXBarOp()(p) with LaneOp
class VIULaneOp(implicit p: Parameters) extends VIUOp()(p) with LaneOp
class VIPULaneOp(implicit p: Parameters) extends VIPUOp()(p) with LaneOp
class VIMULaneOp(implicit p: Parameters) extends VIMUOp()(p) with LaneOp
class VFMULaneOp(implicit p: Parameters) extends VFMUOp()(p) with LaneOp
class VFCULaneOp(implicit p: Parameters) extends VFCUOp()(p) with LaneOp
class VFVULaneOp(implicit p: Parameters) extends VFVUOp()(p) with LaneOp
class VQULaneOp(implicit p: Parameters) extends VQUOp()(p) with LaneOp
class VPULaneOp(implicit p: Parameters) extends VPUOp()(p) with LaneOp
class VGULaneOp(implicit p: Parameters) extends VGUOp()(p) with LaneOp
class VSULaneOp(implicit p: Parameters) extends VSUOp()(p) with LaneOp

class SRAMRFReadExpEntry(implicit p: Parameters) extends SRAMRFReadLaneOp()(p) {
  val global = new VXUBundle {
    val valid = Bool()
    val id = UInt(log2Up(nGOPL).W)
  }
  val local = new VXUBundle {
    val valid = Bool()
    val id = UInt(log2Up(nLOPL).W)
  }
}
class SRAMRFWriteExpEntry(implicit p: Parameters) extends SRAMRFWriteLaneOp()(p)

class PredRFReadExpEntry(implicit p: Parameters) extends PredRFGatedReadLaneOp()(p) {
  val global = new VXUBundle {
    val valid = Bool()
    val id = UInt(log2Up(nGPDL).W)
  }
  val local = new VXUBundle {
    val valid = Bool()
    val id = UInt(log2Up(nLPDL).W)
  }
}

class PredRFWriteExpEntry(implicit p: Parameters) extends PredRFWriteLaneOp()(p)

//-------------------------------------------------------------------------\\
// banks
//-------------------------------------------------------------------------\\

class BankPredEntry(implicit p: Parameters) extends BankPred
class BankPredMaskEntry(implicit p: Parameters) extends BankPred with PredMask
class BankDataEntry(implicit p: Parameters) extends BankData
class BankDataPredEntry(implicit p: Parameters) extends BankData with BankPred
class BankDataMaskEntry(implicit p: Parameters) extends BankData with BankMask

//-------------------------------------------------------------------------\\
// micro op
//-------------------------------------------------------------------------\\

class SRAMRFReadMicroOp(implicit p: Parameters) extends SRAMRFReadOp()(p) with MicroOp
class SRAMRFWriteMicroOp(implicit p: Parameters) extends SRAMRFWriteOp()(p) with MicroOp
class FFRFReadMicroOp(implicit p: Parameters) extends FFRFReadOp()(p) with MicroOp
class FFRFWriteMicroOp(implicit p: Parameters) extends FFRFWriteOp()(p) with MicroOp
class PredRFReadMicroOp(implicit p: Parameters) extends PredRFReadOp()(p) with MicroOp
class PredRFGatedReadMicroOp(implicit p: Parameters) extends PredRFGatedReadOp()(p) with MicroOp
class PredRFWriteMicroOp(implicit p: Parameters) extends PredRFWriteOp()(p) with MicroOp
class OPLMicroOp(implicit p: Parameters) extends OPLOp()(p) with MicroOp
class PDLMicroOp(implicit p: Parameters) extends PDLOp()(p) with MicroOp
class SRegMicroOp(implicit p: Parameters) extends SRegOp()(p) with MicroOp
class XBarMicroOp(implicit p: Parameters) extends XBarOp()(p) with MicroOp
class PXBarMicroOp(implicit p: Parameters) extends PXBarOp()(p) with MicroOp
class VIUMicroOp(implicit p: Parameters) extends VIUOp()(p) with MicroOp
class VIPUMicroOp(implicit p: Parameters) extends VIPUOp()(p) with MicroOp
class VIMUMicroOp(implicit p: Parameters) extends VIMUOp()(p) with MicroOp
class VFMUMicroOp(implicit p: Parameters) extends VFMUOp()(p) with MicroOp
class VFCUMicroOp(implicit p: Parameters) extends VFCUOp()(p) with MicroOp
class VFVUMicroOp(implicit p: Parameters) extends VFVUOp()(p) with MicroOp
class VQUMicroOp(implicit p: Parameters) extends VQUOp()(p) with MicroOp
class VPUMicroOp(implicit p: Parameters) extends VPUOp()(p) with MicroOp
class VGUMicroOp(implicit p: Parameters) extends VGUOp()(p) with MicroOp
class VSUMicroOp(implicit p: Parameters) extends VSUOp()(p) with MicroOp

//-------------------------------------------------------------------------\\
// bank acks
//-------------------------------------------------------------------------\\

class VIUAck(implicit p: Parameters) extends BankPred
class VIPUAck(implicit p: Parameters) extends BankPred
class VIMUAck(implicit p: Parameters) extends BankPred
class VIDUAck(implicit p: Parameters) extends BankPred
class VGUAck(implicit p: Parameters) extends BankPred
class VQUAck(implicit p: Parameters) extends BankPred

class VFXUAck(implicit p: Parameters) extends VXUBundle()(p) with BankPred {
  val exc = Output(UInt(freechips.rocketchip.tile.FPConstants.FLAGS_SZ.W))
}

class VFMUAck(implicit p: Parameters) extends VFXUAck()(p)
class VFDUAck(implicit p: Parameters) extends VFXUAck()(p)
class VFCUAck(implicit p: Parameters) extends BankPred // no exceptions can occur
class VFVUAck(implicit p: Parameters) extends VFXUAck()(p)


//-------------------------------------------------------------------------\\
// decoupled cluster (dcc) types
//-------------------------------------------------------------------------\\

class DCCOp(implicit p: Parameters) extends VXUBundle()(p) {
  val vlen = UInt(bVLen.W)
  val active = new IssueType
  val fn = new VFn
  val vd = new PhysicalRegInfo
}

class LPQEntry(implicit p: Parameters) extends VXUBundle()(p) with BankPred
class BPQEntry(implicit p: Parameters) extends VXUBundle()(p) with BankPred
class LRQEntry(implicit p: Parameters) extends VXUBundle()(p) with BankData
class BRQEntry(implicit p: Parameters) extends VXUBundle()(p) with BankData
class BWQEntry(implicit p: Parameters) extends VXUBundle()(p) with BankData with BankMask {
  val selff = Bool() // select ff if true
  val addr = UInt(math.max(log2Up(nSRAM), log2Up(nFF)).W)

  def saddr(dummy: Int = 0) = addr(log2Up(nSRAM)-1, 0)
  def faddr(dummy: Int = 0) = addr(log2Up(nFF)-1, 0)
}

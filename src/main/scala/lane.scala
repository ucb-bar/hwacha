package hwacha

import Chisel._
import cde.{Parameters, Field}

case object HwachaBankWidth extends Field[Int]
case object HwachaNBanks extends Field[Int]
case object HwachaNSRAMRFEntries extends Field[Int]
case object HwachaNFFRFEntries extends Field[Int]
case object HwachaNFFRFReadPorts extends Field[Int]
case object HwachaNPredRFEntries extends Field[Int]
case object HwachaNPredRFReadPorts extends Field[Int]
case object HwachaNOperandLatches extends Field[Int]
case object HwachaNPredLatches extends Field[Int]
case object HwachaWriteSelects extends Field[Int]
case object HwachaStagesALU extends Field[Int]
case object HwachaStagesPLU extends Field[Int]
case object HwachaStagesIMul extends Field[Int]
case object HwachaStagesFMA extends Field[Int]
case object HwachaStagesFConv extends Field[Int]
case object HwachaStagesFCmp extends Field[Int]

abstract trait LaneParameters extends UsesHwachaParameters {
  val wBank = p(HwachaBankWidth)
  val nBanks = p(HwachaNBanks)
  val nSRAM = p(HwachaNSRAMRFEntries)
  val nFF = p(HwachaNFFRFEntries)
  val bRFAddr = math.max(log2Up(nSRAM), log2Up(nFF))
  val nFFRPorts = p(HwachaNFFRFReadPorts)
  val nPred = p(HwachaNPredRFEntries)
  val bPredAddr = log2Up(nPred)
  val nPredRPorts = p(HwachaNPredRFReadPorts)

  val nSlices = wBank / p(HwachaRegLen)
  val nBankSRAMRegs = nSRAM * nSlices
  val nLaneSRAMRegs = nBanks * nBankSRAMRegs
  val nBatch = nBanks * nSlices

  val nGOPL = p(HwachaNOperandLatches)
  val nLOPL = 3
  val nGPDL = p(HwachaNPredLatches)
  val nLPDL = 2
  val nWSel = p(HwachaWriteSelects)
  val nLPQ = 2
  val nLRQ = 3
  val nDecoupledUnitWBQueue = 4
  val nVFMU = 2

  val stagesALU = p(HwachaStagesALU)
  val stagesPLU = p(HwachaStagesPLU)
  val stagesIMul = p(HwachaStagesIMul)
  val stagesFMA = p(HwachaStagesFMA)
  val stagesFConv = p(HwachaStagesFConv)
  val stagesFCmp = p(HwachaStagesFCmp)

  require(nVRegs <= nSRAM)
}

class LaneOpIO(implicit p: Parameters) extends VXUBundle()(p) {
  val sram = new Bundle {
    val read = Valid(new SRAMRFReadLaneOp)
    val write = Valid(new SRAMRFWriteLaneOp)
  }
  val ff = new Bundle {
    val read = Vec.fill(nFFRPorts){Valid(new FFRFReadLaneOp)}
    val write = Valid(new FFRFWriteLaneOp)
  }
  val pred = new Bundle {
    val gread = Valid(new PredRFGatedReadLaneOp) // gated read
    val pread = Valid(new PredRFGatedReadLaneOp) // vpu read
    val read = Vec.fill(nPredRPorts){Valid(new PredRFReadLaneOp)} // plu read
    val write = Valid(new PredRFWriteLaneOp)
  }
  val opl = new Bundle {
    val global = Vec.fill(nGOPL){Valid(new OPLLaneOp)}
    val local = Vec.fill(nLOPL){Valid(new OPLLaneOp)}
  }
  val pdl = new Bundle {
    val global = Vec.fill(nGPDL){Valid(new PDLLaneOp)}
    val local = Vec.fill(nLPDL){Valid(new PDLLaneOp)}
  }
  val sreg = new Bundle {
    val global = Vec.fill(nGOPL){Valid(new SRegLaneOp)}
    val local = Vec.fill(nLOPL){Valid(new SRegLaneOp)}
  }
  val xbar = Vec.fill(nGOPL){Valid(new XBarLaneOp)}
  val pxbar = Vec.fill(nGPDL){Valid(new PXBarLaneOp)}
  val viu = Valid(new VIULaneOp)
  val vipu = Valid(new VIPULaneOp)
  val vpu = Valid(new VPULaneOp)
  val vsu = Valid(new VSULaneOp)
  val vqu = Valid(new VQULaneOp)
  val vgu = Valid(new VGULaneOp)
  val vimu = Valid(new VIMULaneOp)
  val vfmu = Vec.fill(nVFMU){Valid(new VFMULaneOp)}
  val vfcu = Valid(new VFCULaneOp)
  val vfvu = Valid(new VFVULaneOp)
}

class MicroOpIO(implicit p: Parameters) extends VXUBundle()(p) {
  val bank = Vec.fill(nBanks){new BankOpIO}
  val sreg = Vec.fill(nGOPL){Valid(new SRegMicroOp)}
  val vqu = Valid(new VQUMicroOp)
  val vgu = Valid(new VGUMicroOp)
  val vimu = Valid(new VIMUMicroOp)
  val vfmu = Vec.fill(nVFMU){Valid(new VFMUMicroOp)}
  val vfcu = Valid(new VFCUMicroOp)
  val vfvu = Valid(new VFVUMicroOp)
}

class LaneAckIO(implicit p: Parameters) extends VXUBundle()(p) {
  val viu = Vec.fill(nBanks){Valid(new VIUAck)}
  val vipu = Vec.fill(nBanks){Valid(new VIPUAck)}
  val vqu = Valid(new VQUAck)
  val vgu = Valid(new VGUAck)
  val vimu = Valid(new VIMUAck)
  val vfmu = Vec.fill(nVFMU){Valid(new VFMUAck)}
  val vfcu = Valid(new VFCUAck)
  val vfvu = Valid(new VFVUAck)
}

class LPQIO(implicit p: Parameters) extends DecoupledIO(new LPQEntry()(p))
class LRQIO(implicit p: Parameters) extends DecoupledIO(new LRQEntry()(p))

class Lane(implicit p: Parameters) extends VXUModule()(p) with Packing {
  val io = new Bundle {
    val op = new LaneOpIO().flip
    val ack = new LaneAckIO
    val lpqs = Vec.fill(nLPQ){new LPQIO}
    val lrqs = Vec.fill(nLRQ){new LRQIO}
    val bpqs = Vec.fill(nBanks){new BPQIO}
    val brqs = Vec.fill(nBanks){new BRQIO}
    val bwqs = new Bundle {
      val mem = Vec.fill(nBanks){new BWQIO}.flip
      val fu = Vec.fill(nBanks){new BWQIO}.flip
    }
  }

  val ctrl = Module(new LaneCtrl)
  ctrl.io.op <> io.op

  val banksrw = (0 until nBanks) map { i =>
    val bank = Module(new Bank(i))

    // TODO: this needs to be sequenced
    bank.io.op <> ctrl.io.uop.bank(i)
    io.bpqs(i) <> bank.io.rw.bpq
    io.brqs(i) <> bank.io.rw.brq
    bank.io.rw.bwq.mem <> io.bwqs.mem(i)
    bank.io.rw.bwq.fu <> io.bwqs.fu(i)
    bank.io.rw
  }

  val pdls = (0 until nGPDL) map { o => banksrw.map(_.pdl(o).pred).reduce(_|_) }
  val opls = (0 until nGOPL) map { o => banksrw.map(_.opl(o).data).reduce(_|_) }

  def predicate(n: Int) = new BankPredEntry().fromBits(pdls(n))

  def operands[T <: SharedLLOp](name: String, uop: ValidIO[T], n: Int, rbase: Int) = {
    require(n <= uop.bits.nOperands)
    (0 until n) map { i =>
      val ri = rbase+i
      assert(!uop.valid || !uop.bits.sreg(i) || ctrl.io.uop.sreg(ri).valid, "check sreg sched logic "+name+"_"+i)
      Mux(uop.bits.sreg(i), splat_slice(ctrl.io.uop.sreg(ri).bits.operand), opls(ri))
    }
  }

  require(nLRQ == 3)

  // VIMU:  predicate(0), operands(0, 1)
  // VFMU0: predicate(0), operands(0, 1, 2)
  // VFVU:  predicate(1), operands(2)
  // VFMU1: predicate(2), operands(3, 4, 5)
  // VQU:   predicate(2), operands(3, 4)
  // VFCU:  predicate(2), operands(3, 4)
  // VGU:   predicate(3), operands(5)

  val vqu_pred = predicate(2)
  val vqu_operands = operands("vqu", ctrl.io.uop.vqu, 2, 3)
  io.lpqs(0).valid := ctrl.io.uop.vqu.valid
  io.lpqs(0).bits.pred := vqu_pred.pred
  io.lrqs(0).valid := ctrl.io.uop.vqu.valid && ctrl.io.uop.vqu.bits.fn.latch(0) && vqu_pred.active()
  io.lrqs(0).bits.data := vqu_operands(0)
  io.lrqs(1).valid := ctrl.io.uop.vqu.valid && ctrl.io.uop.vqu.bits.fn.latch(1) && vqu_pred.active()
  io.lrqs(1).bits.data := vqu_operands(1)

  assert(!io.lpqs(0).valid || io.lpqs(0).ready, "check lpqs(0) counter logic")
  assert(!io.lrqs(0).valid || io.lrqs(0).ready, "check lrqs(0) counter logic")
  assert(!io.lrqs(1).valid || io.lrqs(1).ready, "check lrqs(1) counter logic")

  val vgu_pred = predicate(3)
  val vgu_operands = operands("vgu", ctrl.io.uop.vgu, 1, 5)
  io.lpqs(1).valid := ctrl.io.uop.vgu.valid
  io.lpqs(1).bits.pred := vgu_pred.pred
  io.lrqs(2).valid := ctrl.io.uop.vgu.valid && vgu_pred.active()
  io.lrqs(2).bits.data := vgu_operands(0)

  assert(!io.lpqs(1).valid || io.lpqs(1).ready, "check lpqs(1) counter logic")
  assert(!io.lrqs(2).valid || io.lrqs(2).ready, "check lrqs(2) counter logic")

  val vimu_pred = predicate(0)
  val vimu_operands = operands("vimu", ctrl.io.uop.vimu, 2, 0)
  val vimus = (0 until nSlices) map { i =>
    val vimu = Module(new IMulSlice)
    vimu.io.req.valid := ctrl.io.uop.vimu.valid && ctrl.io.uop.vimu.bits.pred(i) && vimu_pred.pred(i)
    vimu.io.req.bits.fn := ctrl.io.uop.vimu.bits.fn
    vimu.io.req.bits.in0 := unpack_slice(vimu_operands(0), i)
    vimu.io.req.bits.in1 := unpack_slice(vimu_operands(1), i)
    vimu.io.resp
  }

  val vfmus = (0 until nVFMU) map { v =>
    val vfmu_pred = predicate(2*v)
    val vfmu_operands = operands("vfmu"+v, ctrl.io.uop.vfmu(v), 3, 3*v)
    (0 until nSlices) map { i =>
      val vfmu = Module(new FMASlice)
      vfmu.io.req.valid := ctrl.io.uop.vfmu(v).valid && ctrl.io.uop.vfmu(v).bits.pred(i) && vfmu_pred.pred(i)
      vfmu.io.req.bits.fn := ctrl.io.uop.vfmu(v).bits.fn
      vfmu.io.req.bits.in0 := unpack_slice(vfmu_operands(0), i)
      vfmu.io.req.bits.in1 := unpack_slice(vfmu_operands(1), i)
      vfmu.io.req.bits.in2 := unpack_slice(vfmu_operands(2), i)
      vfmu.io.resp
    }
  }

  val vfcu_pred = predicate(2)
  val vfcu_operands = operands("vfcu", ctrl.io.uop.vfcu, 2, 3)
  val vfcus = (0 until nSlices) map { i =>
    val vfcu = Module(new FCmpSlice)
    vfcu.io.req.valid := ctrl.io.uop.vfcu.valid && ctrl.io.uop.vfcu.bits.pred(i) && vfcu_pred.pred(i)
    vfcu.io.req.bits.fn := ctrl.io.uop.vfcu.bits.fn
    vfcu.io.req.bits.in0 := unpack_slice(vfcu_operands(0), i)
    vfcu.io.req.bits.in1 := unpack_slice(vfcu_operands(1), i)
    vfcu.io.resp
  }

  val vfvu_pred = predicate(1)
  val vfvu_operands = operands("vfvu", ctrl.io.uop.vfvu, 1, 2)
  val vfvus = (0 until nSlices) map { i =>
    val vfvu = Module(new FConvSlice)
    vfvu.io.req.valid := ctrl.io.uop.vfvu.valid && ctrl.io.uop.vfvu.bits.pred(i) && vfvu_pred.pred(i)
    vfvu.io.req.bits.fn := ctrl.io.uop.vfvu.bits.fn
    vfvu.io.req.bits.in := unpack_slice(vfvu_operands(0), i)
    vfvu.io.resp
  }

  require(nVFMU == 2)

  val vimu_vals = Vec(vimus.map(_.valid)).toBits
  val vfmu_vals = vfmus map { vfmu => Vec(vfmu.map(_.valid)).toBits }
  val vfcu_vals = Vec(vfcus.map(_.valid)).toBits
  val vfvu_vals = Vec(vfvus.map(_.valid)).toBits

  val wdata = List(
    MuxCase(Bits(0), Array(
      vimu_vals.orR -> repack_slice(vimus.map(_.bits.out)),
      vfmu_vals(0).orR -> repack_slice(vfmus(0).map(_.bits.out)),
      vfvu_vals.orR -> repack_slice(vfvus.map(_.bits.out)))),
    MuxCase(Bits(0), Array(
      vfmu_vals(1).orR -> repack_slice(vfmus(1).map(_.bits.out)),
      vfcu_vals.orR -> repack_slice(vfcus.map(_.bits.out)))))

  val wdata_pred = List(
    vimu_vals | vfmu_vals(0) | vfvu_vals,
    vfmu_vals(1) | vfcu_vals)

  banksrw.map { b =>
    b.wpred.pred := Vec(vfcus.map(_.bits.cmp)).toBits
    (b.wdata zipWithIndex) map { case (bwdata, i) =>
      bwdata.data := wdata(i)
      bwdata.pred := wdata_pred(i)
    }
  }

  (io.ack.vfmu zip vfmu_vals) foreach { case (ack, vfmu) =>
    ack.valid := vfmu.orR
    ack.bits.pred := vfmu
  }

  io.ack.vqu.valid := ctrl.io.uop.vqu.valid
  io.ack.vgu.valid := ctrl.io.uop.vgu.valid
  io.ack.vimu.valid := vimu_vals.orR
  io.ack.vfcu.valid := vfcu_vals.orR
  io.ack.vfvu.valid := vfvu_vals.orR

  io.ack.vqu.bits.pred := ctrl.io.uop.vqu.bits.pred
  io.ack.vgu.bits.pred := ctrl.io.uop.vgu.bits.pred
  io.ack.vimu.bits.pred := vimu_vals
  io.ack.vfcu.bits.pred := vfcu_vals
  io.ack.vfvu.bits.pred := vfvu_vals
}

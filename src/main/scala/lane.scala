package hwacha

import Chisel._

case object HwachaBankWidth extends Field[Int]
case object HwachaNBanks extends Field[Int]
case object HwachaNSRAMRFEntries extends Field[Int]
case object HwachaNFFRFEntries extends Field[Int]
case object HwachaNFFRFReadPorts extends Field[Int]
case object HwachaNOperandLatches extends Field[Int]
case object HwachaWriteSelects extends Field[Int]
case object HwachaStagesALU extends Field[Int]
case object HwachaStagesIMul extends Field[Int]
case object HwachaStagesFMA extends Field[Int]
case object HwachaStagesFConv extends Field[Int]
case object HwachaStagesFCmp extends Field[Int]

abstract trait LaneParameters extends UsesHwachaParameters {
  val wBank = params(HwachaBankWidth)
  val nBanks = params(HwachaNBanks)
  val nSRAM = params(HwachaNSRAMRFEntries)
  val nFF = params(HwachaNFFRFEntries)
  val bRFAddr = math.max(log2Up(nSRAM), log2Up(nFF))

  val nSlices = wBank / params(HwachaRegLen)
  val nBankSRAMRegs = nSRAM * nSlices
  val nLaneSRAMRegs = nBanks * nBankSRAMRegs
  val nBatch = nBanks * nSlices

  val nFFRPorts = params(HwachaNFFRFReadPorts)
  val nGOPL = params(HwachaNOperandLatches)
  val nLOPL = 3
  val nWSel = params(HwachaWriteSelects)
  val nLRQOperands = 3
  val nDecoupledUnitWBQueue = 4
  val nVFMU = 2

  val stagesALU = params(HwachaStagesALU)
  val stagesIMul = params(HwachaStagesIMul)
  val stagesFMA = params(HwachaStagesFMA)
  val stagesFConv = params(HwachaStagesFConv)
  val stagesFCmp = params(HwachaStagesFCmp)

  require(nVRegs <= nSRAM)
}

class LaneOpIO extends VXUBundle {
  val sram = new Bundle {
    val read = Valid(new SRAMRFReadLaneOp)
    val write = Valid(new SRAMRFWriteLaneOp)
  }
  val ff = new Bundle {
    val read = Vec.fill(nFFRPorts){Valid(new FFRFReadLaneOp)}
    val write = Valid(new FFRFWriteLaneOp)
  }
  val opl = new Bundle {
    val global = Vec.fill(nGOPL){Valid(new OPLLaneOp)}
    val local = Vec.fill(nLOPL){Valid(new OPLLaneOp)}
  }
  val sreg = new Bundle {
    val global = Vec.fill(nGOPL){Valid(new SRegLaneOp)}
    val local = Vec.fill(nLOPL){Valid(new SRegLaneOp)}
  }
  val xbar = Vec.fill(nGOPL){Valid(new XBarLaneOp)}
  val viu = Valid(new VIULaneOp)
  val vimu = Valid(new VIMULaneOp)
  val vfmu = Vec.fill(nVFMU){Valid(new VFMULaneOp)}
  val vfcu = Valid(new VFCULaneOp)
  val vfvu = Valid(new VFVULaneOp)
  val vqu = Valid(new VQULaneOp)
  val vgu = Valid(new VGULaneOp)
  val vsu = Valid(new VSULaneOp)
}

class MicroOpIO extends VXUBundle {
  val bank = Vec.fill(nBanks){new BankOpIO()}
  val sreg = Vec.fill(nGOPL){Valid(new SRegMicroOp)}
  val vqu = Valid(new VQUMicroOp)
  val vgu = Valid(new VGUMicroOp)
  val vimu = Valid(new VIMUMicroOp)
  val vfmu = Vec.fill(nVFMU){Valid(new VFMUMicroOp)}
  val vfcu = Valid(new VFCUMicroOp)
  val vfvu = Valid(new VFVUMicroOp)
}

class LaneAckIO extends VXUBundle {
  val viu = Vec.fill(nBanks){Valid(new VIUAck)}
  val vqu = Valid(new VQUAck)
  val vgu = Valid(new VGUAck)
  val vimu = Valid(new VIMUAck)
  val vfmu = Vec.fill(nVFMU){Valid(new VFMUAck)}
  val vfcu = Valid(new VFCUAck)
  val vfvu = Valid(new VFVUAck)
}

class LRQIO extends DecoupledIO(new LRQEntry)

class Lane extends VXUModule with Packing {
  val io = new Bundle {
    val op = new LaneOpIO().flip
    val ack = new LaneAckIO
    val lrqs = Vec.fill(nLRQOperands){new LRQIO}
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
    io.brqs(i) <> bank.io.rw.brq
    bank.io.rw.bwq.mem <> io.bwqs.mem(i)
    bank.io.rw.bwq.fu <> io.bwqs.fu(i)
    bank.io.rw
  }

  val rdata = (0 until nGOPL).map { o =>
    Mux(ctrl.io.uop.sreg(o).valid, ctrl.io.uop.sreg(o).bits.operand,
                                   banksrw.map(_.rdata(o).d).reduce(_|_)) }

  require(nLRQOperands == 3)

  io.lrqs(0).valid := ctrl.io.uop.vqu.valid && ctrl.io.uop.vqu.bits.fn.latch(0)
  io.lrqs(0).bits.data := rdata(3)
  io.lrqs(1).valid := ctrl.io.uop.vqu.valid && ctrl.io.uop.vqu.bits.fn.latch(1)
  io.lrqs(1).bits.data := rdata(4)
  io.lrqs(2).valid := ctrl.io.uop.vgu.valid
  io.lrqs(2).bits.data := rdata(5)

  assert(!io.lrqs(0).valid || io.lrqs(0).ready, "check lrqs(0) counter logic")
  assert(!io.lrqs(1).valid || io.lrqs(1).ready, "check lrqs(1) counter logic")
  assert(!io.lrqs(2).valid || io.lrqs(2).ready, "check lrqs(1) counter logic")

  val vfmus = (0 until nVFMU) map { v =>
    (0 until nSlices) map { i =>
      val vfmu = Module(new FMASlice)
      vfmu.io.req.valid := ctrl.io.uop.vfmu(v).valid && ctrl.io.uop.vfmu(v).bits.pred(i)
      vfmu.io.req.bits.fn := ctrl.io.uop.vfmu(v).bits.fn
      vfmu.io.req.bits.in0 := unpack_slice(rdata(3*v+0), i)
      vfmu.io.req.bits.in1 := unpack_slice(rdata(3*v+1), i)
      vfmu.io.req.bits.in2 := unpack_slice(rdata(3*v+2), i)
      vfmu.io.resp
    }
  }

  val vimus = (0 until nSlices) map { i =>
    val vimu = Module(new IMulSlice)
    vimu.io.req.valid := ctrl.io.uop.vimu.valid && ctrl.io.uop.vimu.bits.pred(i)
    vimu.io.req.bits.fn := ctrl.io.uop.vimu.bits.fn
    vimu.io.req.bits.in0 := unpack_slice(rdata(0), i)
    vimu.io.req.bits.in1 := unpack_slice(rdata(1), i)
    vimu.io.resp
  }

  val vfvus = (0 until nSlices) map { i =>
    val vfvu = Module(new FConvSlice)
    vfvu.io.req.valid := ctrl.io.uop.vfvu.valid && ctrl.io.uop.vfvu.bits.pred(i)
    vfvu.io.req.bits.fn := ctrl.io.uop.vfvu.bits.fn
    vfvu.io.req.bits.in := unpack_slice(rdata(2), i)
    vfvu.io.resp
  }

  val vfcus = (0 until nSlices) map { i =>
    val vfcu = Module(new FCmpSlice)
    vfcu.io.req.valid := ctrl.io.uop.vfcu.valid && ctrl.io.uop.vfcu.bits.pred(i)
    vfcu.io.req.bits.fn := ctrl.io.uop.vfcu.bits.fn
    vfcu.io.req.bits.in0 := unpack_slice(rdata(3), i)
    vfcu.io.req.bits.in1 := unpack_slice(rdata(4), i)
    vfcu.io.resp
  }

  require(nVFMU == 2)

  val wdata = List(
    MuxCase(Bits(0), Array(
      vfmus(0).map(_.valid).reduce(_|_) -> repack_slice(vfmus(0).map(_.bits.out)),
      vimus.map(_.valid).reduce(_|_) -> repack_slice(vimus.map(_.bits.out)),
      vfvus.map(_.valid).reduce(_|_) -> repack_slice(vfvus.map(_.bits.out)))),
    MuxCase(Bits(0), Array(
      vfmus(1).map(_.valid).reduce(_|_) -> repack_slice(vfmus(1).map(_.bits.out)),
      vfcus.map(_.valid).reduce(_|_) -> repack_slice(vfcus.map(_.bits.out)))))

  banksrw.map { b => b.wdata.zipWithIndex.map { case (bwdata, i) => bwdata.d := wdata(i) }}

  (io.ack.vfmu zip vfmus) foreach { case (ack, vfmu) =>
    ack.valid := vfmu.map(_.valid).reduce(_|_)
    ack.bits.pred := Vec(vfmu.map(_.valid)).toBits
  }

  io.ack.vqu.valid := ctrl.io.uop.vqu.valid
  io.ack.vgu.valid := ctrl.io.uop.vgu.valid
  io.ack.vimu.valid := vimus.map(_.valid).reduce(_|_)
  io.ack.vfcu.valid := vfcus.map(_.valid).reduce(_|_)
  io.ack.vfvu.valid := vfvus.map(_.valid).reduce(_|_)

  io.ack.vqu.bits.pred := ctrl.io.uop.vqu.bits.pred
  io.ack.vgu.bits.pred := ctrl.io.uop.vgu.bits.pred
  io.ack.vimu.bits.pred := Vec(vimus.map(_.valid)).toBits
  io.ack.vfcu.bits.pred := Vec(vfcus.map(_.valid)).toBits
  io.ack.vfvu.bits.pred := Vec(vfvus.map(_.valid)).toBits
}

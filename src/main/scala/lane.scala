package hwacha

import Chisel._
import Node._
import scala.collection.mutable.ArrayBuffer

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
  val nLOPL = 2
  val nWSel = params(HwachaWriteSelects)
  val nLRQOperands = 2
  val nDecoupledUnitWBQueue = 4

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
  val vfmu0 = Valid(new VFMULaneOp)
  val vfmu1 = Valid(new VFMULaneOp)
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
  val vfmu0 = Valid(new VFMUMicroOp)
  val vfmu1 = Valid(new VFMUMicroOp)
  val vfcu = Valid(new VFCUMicroOp)
  val vfvu = Valid(new VFVUMicroOp)
}

class LaneAckIO extends VXUBundle {
  val viu = Vec.fill(nBanks){Valid(new VIUAck)}
  val vqu = Valid(new VQUAck)
  val vgu = Valid(new VGUAck)
  val vimu = Valid(new VIMUAck)
  val vfmu0 = Valid(new VFMUAck)
  val vfmu1 = Valid(new VFMUAck)
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
    val vmu = new LaneMemIO
  }

  val ctrl = Module(new LaneCtrl)
  val banksrw = new ArrayBuffer[BankRWIO]
  val imuls = new ArrayBuffer[ValidIO[IMulResult]]
  val fma0s = new ArrayBuffer[ValidIO[FMAResult]]
  val fma1s = new ArrayBuffer[ValidIO[FMAResult]]
  val fcmps = new ArrayBuffer[ValidIO[FCmpResult]]
  val fconvs = new ArrayBuffer[ValidIO[FConvResult]]

  ctrl.io.op <> io.op

  for (i <- 0 until nBanks) {
    val bank = Module(new Bank)

    // TODO: this needs to be sequenced
    bank.io.op <> ctrl.io.uop.bank(i)
    banksrw += bank.io.rw
    io.brqs(i) <> bank.io.rw.brq
    bank.io.rw.bwq.mem <> io.bwqs.mem(i)
    bank.io.rw.bwq.fu <> io.bwqs.fu(i)
  }

  val rdata = (0 until nGOPL).map { o =>
    Mux(ctrl.io.uop.sreg(o).valid, ctrl.io.uop.sreg(o).bits.operand,
                                   banksrw.map(_.rdata(o).d).reduce(_|_)) }

  require(nLRQOperands == 2)

  io.lrqs(0).valid := ctrl.io.uop.vqu.valid && ctrl.io.uop.vqu.bits.fn.latch(0)
  io.lrqs(0).bits.data := rdata(3)
  io.lrqs(1).valid := ctrl.io.uop.vqu.valid && ctrl.io.uop.vqu.bits.fn.latch(1)
  io.lrqs(1).bits.data := rdata(4)

  assert(!io.lrqs(0).valid || io.lrqs(0).ready, "check lrqs(0) counter logic")
  assert(!io.lrqs(1).valid || io.lrqs(1).ready, "check lrqs(1) counter logic")

  io.vmu.vaq.valid := ctrl.io.uop.vgu.valid
  io.vmu.vaq.bits.addr := rdata(5)

  assert(!io.vmu.vaq.valid || io.vmu.vaq.ready, "check vaq counter logic")

  for (i <- 0 until nSlices) {
    val fma0 = Module(new FMASlice)
    fma0.io.req.valid := ctrl.io.uop.vfmu0.valid && ctrl.io.uop.vfmu0.bits.pred(i)
    fma0.io.req.bits.fn := ctrl.io.uop.vfmu0.bits.fn
    fma0.io.req.bits.in0 := unpack_slice(rdata(0), i)
    fma0.io.req.bits.in1 := unpack_slice(rdata(1), i)
    fma0.io.req.bits.in2 := unpack_slice(rdata(2), i)
    fma0s += fma0.io.resp

    val imul = Module(new IMulSlice)
    imul.io.req.valid := ctrl.io.uop.vimu.valid && ctrl.io.uop.vimu.bits.pred(i)
    imul.io.req.bits.fn := ctrl.io.uop.vimu.bits.fn
    imul.io.req.bits.in0 := unpack_slice(rdata(0), i)
    imul.io.req.bits.in1 := unpack_slice(rdata(1), i)
    imuls += imul.io.resp

    val fconv = Module(new FConvSlice)
    fconv.io.req.valid := ctrl.io.uop.vfvu.valid && ctrl.io.uop.vfvu.bits.pred(i)
    fconv.io.req.bits.fn := ctrl.io.uop.vfvu.bits.fn
    fconv.io.req.bits.in := unpack_slice(rdata(2), i)
    fconvs += fconv.io.resp

    val fma1 = Module(new FMASlice)
    fma1.io.req.valid := ctrl.io.uop.vfmu1.valid && ctrl.io.uop.vfmu1.bits.pred(i)
    fma1.io.req.bits.fn := ctrl.io.uop.vfmu1.bits.fn
    fma1.io.req.bits.in0 := unpack_slice(rdata(3), i)
    fma1.io.req.bits.in1 := unpack_slice(rdata(4), i)
    fma1.io.req.bits.in2 := unpack_slice(rdata(5), i)
    fma1s += fma1.io.resp

    val fcmp = Module(new FCmpSlice)
    fcmp.io.req.valid := ctrl.io.uop.vfcu.valid && ctrl.io.uop.vfcu.bits.pred(i)
    fcmp.io.req.bits.fn := ctrl.io.uop.vfcu.bits.fn
    fcmp.io.req.bits.in0 := unpack_slice(rdata(3), i)
    fcmp.io.req.bits.in1 := unpack_slice(rdata(4), i)
    fcmps += fcmp.io.resp
  }

  val wdata = List(
    MuxCase(Bits(0), Array(
      fma0s.map(_.valid).reduce(_|_) -> repack_slice(fma0s.map(_.bits.out)),
      imuls.map(_.valid).reduce(_|_) -> repack_slice(imuls.map(_.bits.out)),
      fconvs.map(_.valid).reduce(_|_) -> repack_slice(fconvs.map(_.bits.out)))),
    MuxCase(Bits(0), Array(
      fma1s.map(_.valid).reduce(_|_) -> repack_slice(fma1s.map(_.bits.out)),
      fcmps.map(_.valid).reduce(_|_) -> repack_slice(fcmps.map(_.bits.out)))))

  banksrw.map { b => b.wdata.zipWithIndex.map { case (bwdata, i) => bwdata.d := wdata(i) }}

  io.ack.vqu.valid := ctrl.io.uop.vqu.valid
  io.ack.vgu.valid := ctrl.io.uop.vgu.valid
  io.ack.vimu.valid := imuls.map(_.valid).reduce(_|_)
  io.ack.vfmu0.valid := fma0s.map(_.valid).reduce(_|_)
  io.ack.vfmu1.valid := fma1s.map(_.valid).reduce(_|_)
  io.ack.vfcu.valid := fcmps.map(_.valid).reduce(_|_)
  io.ack.vfvu.valid := fconvs.map(_.valid).reduce(_|_)

  io.ack.vqu.bits.pred := ctrl.io.uop.vqu.bits.pred
  io.ack.vgu.bits.pred := ctrl.io.uop.vgu.bits.pred
  io.ack.vimu.bits.pred := Vec(imuls.map(_.valid)).toBits
  io.ack.vfmu0.bits.pred := Vec(fma0s.map(_.valid)).toBits
  io.ack.vfmu1.bits.pred := Vec(fma1s.map(_.valid)).toBits
  io.ack.vfcu.bits.pred := Vec(fcmps.map(_.valid)).toBits
  io.ack.vfvu.bits.pred := Vec(fconvs.map(_.valid)).toBits
}

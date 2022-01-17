package hwacha

import Chisel._
import freechips.rocketchip.config._

case object HwachaNSRAMRFEntries extends Field[Int]
case object HwachaNFFRFEntries extends Field[Int]
case object HwachaNFFRFReadPorts extends Field[Int]
case object HwachaNPredRFEntries extends Field[Int]
case object HwachaNPredRFReadPorts extends Field[Int]
case object HwachaNOperandLatches extends Field[Int]
case object HwachaNPredLatches extends Field[Int]
case object HwachaWriteSelects extends Field[Int]
case object HwachaRFAddrBits extends Field[Int]
case object HwachaPRFAddrBits extends Field[Int]
case object HwachaStagesALU extends Field[Int]
case object HwachaStagesPLU extends Field[Int]
case object HwachaStagesIMul extends Field[Int]
case object HwachaStagesDFMA extends Field[Int]
case object HwachaStagesSFMA extends Field[Int]
case object HwachaStagesHFMA extends Field[Int]
case object HwachaStagesFConv extends Field[Int]
case object HwachaStagesFCmp extends Field[Int]

abstract trait LaneParameters extends UsesHwachaParameters {
  val nSRAM = p(HwachaNSRAMRFEntries)
  val nFF = p(HwachaNFFRFEntries)
  val nFFRPorts = p(HwachaNFFRFReadPorts)
  val nPred = p(HwachaNPredRFEntries)
  val nPredRPorts = p(HwachaNPredRFReadPorts)
  val bRFAddr = p(HwachaRFAddrBits)
  val bPredAddr = p(HwachaPRFAddrBits)

  val nBankSRAMRegs = nSRAM * nSlices
  val nLaneSRAMRegs = nBanks * nBankSRAMRegs

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
  val stagesDFMA = p(HwachaStagesDFMA)
  val stagesSFMA = p(HwachaStagesSFMA)
  val stagesHFMA = p(HwachaStagesHFMA)
  val stagesFConv = p(HwachaStagesFConv)
  val stagesFCmp = p(HwachaStagesFCmp)

  require(nVRegs <= nSRAM)

  val bPack = if (confprec) log2Floor(regLen/SZ_H) else 0
  val nPack = 1 << bPack
  val bRate = log2Up(bPack + 1)

  val wPred = nSlices << bPack
  require(nPred % nPack == 0)
}

class LaneOpIO(implicit p: Parameters) extends VXUBundle()(p) {
  val sram = new Bundle {
    val read = Valid(new SRAMRFReadLaneOp)
    val write = Valid(new SRAMRFWriteLaneOp)
  }
  val ff = new Bundle {
    val read = Vec(nFFRPorts, Valid(new FFRFReadLaneOp))
    val write = Valid(new FFRFWriteLaneOp)
  }
  val pred = new Bundle {
    val gread = Valid(new PredRFGatedReadLaneOp) // gated read
    val pread = Valid(new PredRFGatedReadLaneOp) // vpu read
    val read = Vec(nPredRPorts, Valid(new PredRFReadLaneOp)) // plu read
    val write = Valid(new PredRFWriteLaneOp)
  }
  val opl = new Bundle {
    val global = Vec(nGOPL, Valid(new OPLLaneOp))
    val local = Vec(nLOPL, Valid(new OPLLaneOp))
  }
  val pdl = new Bundle {
    val global = Vec(nGPDL, Valid(new PDLLaneOp))
    val local = Vec(nLPDL, Valid(new PDLLaneOp))
  }
  val sreg = new Bundle {
    val global = Vec(nGOPL, Valid(new SRegLaneOp))
    val local = Vec(nLOPL, Valid(new SRegLaneOp))
  }
  val xbar = Vec(nGOPL, Valid(new XBarLaneOp))
  val pxbar = Vec(nGPDL, Valid(new PXBarLaneOp))
  val viu = Valid(new VIULaneOp)
  val vipu = Valid(new VIPULaneOp)
  val vpu = Valid(new VPULaneOp)
  val vsu = Valid(new VSULaneOp)
  val vqu = Valid(new VQULaneOp)
  val vgu = Valid(new VGULaneOp)
  val vimu = Valid(new VIMULaneOp)
  val vfmu = Vec(nVFMU, Valid(new VFMULaneOp))
  val vfcu = Valid(new VFCULaneOp)
  val vfvu = Valid(new VFVULaneOp)
}

class MicroOpIO(implicit p: Parameters) extends VXUBundle()(p) {
  val bank = Vec(nBanks, new BankOpIO)
  val sreg = Vec(nGOPL, Valid(new SRegMicroOp))
  val vqu = Valid(new VQUMicroOp)
  val vgu = Valid(new VGUMicroOp)
  val vimu = Valid(new VIMUMicroOp)
  val vfmu = Vec(nVFMU, Valid(new VFMUMicroOp))
  val vfcu = Valid(new VFCUMicroOp)
  val vfvu = Valid(new VFVUMicroOp)
}

class LaneAckIO(implicit p: Parameters) extends VXUBundle()(p) {
  val viu = Vec(nBanks, Valid(new VIUAck))
  val vipu = Vec(nBanks, Valid(new VIPUAck))
  val vqu = Valid(new VQUAck)
  val vgu = Valid(new VGUAck)
  val vimu = Valid(new VIMUAck)
  val vfmu = Vec(nVFMU, Valid(new VFMUAck))
  val vfcu = Valid(new VFCUAck)
  val vfvu = Valid(new VFVUAck)
}

class LPQIO(implicit p: Parameters) extends DecoupledIO(new LPQEntry()(p)) {
}
class LRQIO(implicit p: Parameters) extends DecoupledIO(new LRQEntry()(p)) {
}

trait LanePred extends VXUBundle {
  val pred = Bits(width = nPack)
  def active(dummy: Int = 0) = pred.orR
}

class Lane(implicit p: Parameters) extends VXUModule()(p) with Packing with RateLogic {
  val io = new Bundle {
    val id = UInt(INPUT)
    val cfg = new HwachaConfigIO().flip
    val op = new LaneOpIO().flip
    val ack = new LaneAckIO
    val lpqs = Vec(nLPQ, new LPQIO)
    val lrqs = Vec(nLRQ, new LRQIO)
    val bpqs = Vec(nBanks, new BPQIO)
    val brqs = Vec(nBanks, new BRQIO)
    val bwqs = new Bundle {
      val mem = Vec(nBanks, new BWQIO).flip
      val fu = Vec(nBanks, new BWQIO).flip
    }
  }

  val ctrl = Module(new LaneCtrl)
  ctrl.suggestName("ctrlInst")
  ctrl.io.op <> io.op

  val banksrw = (0 until nBanks) map { i =>
    val bank = Module(new Bank(i))
    bank.suggestName("bankInst")

    bank.io.lid := io.id
    bank.io.cfg <> io.cfg
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
      Mux(uop.bits.sreg(i), splat_scalar(ctrl.io.uop.sreg(ri).bits), opls(ri))
    }
  }

  def valids(valid: Bool, pred: Bits, latency: Int) =
    ShiftRegister(Mux(valid, pred, Bits(0)), latency)

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
    vimu.suggestName("vimuInst")
    vimu.io.req.valid := ctrl.io.uop.vimu.valid && ctrl.io.uop.vimu.bits.pred(i) && vimu_pred.pred(i)
    vimu.io.req.bits.fn := ctrl.io.uop.vimu.bits.fn
    vimu.io.req.bits.in0 := unpack_slice(vimu_operands(0), i)
    vimu.io.req.bits.in1 := unpack_slice(vimu_operands(1), i)
    vimu.io.resp
  }

  val vfmus = (0 until nVFMU) map { v =>
    val vfmu_val = ctrl.io.uop.vfmu(v).valid
    val vfmu_pred = ctrl.io.uop.vfmu(v).bits.pred & predicate(2*v).pred
    val vfmu_operands = operands("vfmu"+v, ctrl.io.uop.vfmu(v), 3, 3*v)
    val vfmu_fn = ctrl.io.uop.vfmu(v).bits.fn
    ((0 until nSlices) map { i =>
      val vfmu = Module(new FMASlice)
      vfmu.suggestName("vfmuInst")
      vfmu.io.req.valid := vfmu_val
      vfmu.io.req.bits.fn := vfmu_fn
      vfmu.io.req.bits.in0 := unpack_slice(vfmu_operands(0), i)
      vfmu.io.req.bits.in1 := unpack_slice(vfmu_operands(1), i)
      vfmu.io.req.bits.in2 := unpack_slice(vfmu_operands(2), i)
      vfmu.io.req.bits.rate := ctrl.io.uop.vfmu(v).bits.rate
      vfmu.io.req.bits.pred := unpack_pred(vfmu_pred, i, vfmu.io.req.bits.rate)
      vfmu.io.resp.bits
    }, {
      val stages = Seq(stagesDFMA, stagesSFMA, stagesHFMA)
      val pipe = (0 until stages.max).scanRight(Bits(0, wPred)){
        case (_, in) => RegNext(next=in, init=Bits(0, wPred)) }
      for ((fp, i) <- Seq(FPD, FPS, FPH).zip(stages)) {
        when (vfmu_val && vfmu_fn.fp_is(fp)) { pipe(i-1) := vfmu_pred }
      }
      pipe.head
    })
  }

  val vfcu_pred = predicate(2)
  val vfcu_operands = operands("vfcu", ctrl.io.uop.vfcu, 2, 3)
  val vfcus = (0 until nSlices) map { i =>
    val vfcu = Module(new FCmpSlice)
    vfcu.suggestName("vfcuInst")
    vfcu.io.req.valid := ctrl.io.uop.vfcu.valid && ctrl.io.uop.vfcu.bits.pred(i) && vfcu_pred.pred(i)
    vfcu.io.req.bits.fn := ctrl.io.uop.vfcu.bits.fn
    vfcu.io.req.bits.in0 := unpack_slice(vfcu_operands(0), i)
    vfcu.io.req.bits.in1 := unpack_slice(vfcu_operands(1), i)
    vfcu.io.resp
  }

  val vfvu_pred = ctrl.io.uop.vfvu.bits.pred & predicate(1).pred
  val vfvu_operands = operands("vfvu", ctrl.io.uop.vfvu, 1, 2)
  val vfvus = ((0 until nSlices) map { i =>
    val vfvu = Module(new FConvSlice)
    vfvu.suggestName("vfvuInst")
    vfvu.io.req.valid := ctrl.io.uop.vfvu.valid
    vfvu.io.req.bits.fn := ctrl.io.uop.vfvu.bits.fn
    vfvu.io.req.bits.in := unpack_slice(vfvu_operands(0), i)
    vfvu.io.req.bits.rate := ctrl.io.uop.vfvu.bits.rate
    vfvu.io.req.bits.pred := unpack_pred(vfvu_pred, i, vfvu.io.req.bits.rate)
    vfvu.io.resp.bits
  }, valids(ctrl.io.uop.vfvu.valid, vfvu_pred, stagesFConv))

  require(nVFMU == 2)

  val vimu_vals = Vec(vimus.map(_.valid)).asUInt
  val vfmu_vals = vfmus.map(_._2)
  val vfcu_vals = Vec(vfcus.map(_.valid)).asUInt
  val vfvu_vals = vfvus._2

  val wdata = List(
    MuxCase(Bits(0), Array(
      vimu_vals.orR -> repack_slice(vimus.map(_.bits.out)),
      vfmu_vals(0).orR -> repack_slice(vfmus(0)._1.map(_.out)),
      vfvu_vals.asUInt.orR -> repack_slice(vfvus._1.map(_.out)))),
    MuxCase(Bits(0), Array(
      vfmu_vals(1).orR -> repack_slice(vfmus(1)._1.map(_.out)),
      vfcu_vals.orR -> repack_slice(vfcus.map(_.bits.out)))))

  val wdata_pred = List(
    vimu_vals | vfmu_vals(0) | vfvu_vals.asUInt,
    vfmu_vals(1) | vfcu_vals)

  banksrw.map { b =>
    b.wpred.pred := Vec(vfcus.map(_.bits.cmp)).asUInt
    b.wpred.mask := vfcu_vals
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
  io.ack.vfvu.valid := vfvu_vals.asUInt.orR

  io.ack.vqu.bits.pred := ctrl.io.uop.vqu.bits.pred
  io.ack.vgu.bits.pred := ctrl.io.uop.vgu.bits.pred
  io.ack.vimu.bits.pred := vimu_vals
  io.ack.vfcu.bits.pred := vfcu_vals
  io.ack.vfvu.bits.pred := vfvu_vals
}

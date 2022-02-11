package hwacha

import Chisel._
import freechips.rocketchip.config._

class BankOpIO(implicit p: Parameters) extends VXUBundle()(p) {
  val sram = new Bundle {
    val read = Valid(new SRAMRFReadMicroOp)
    val write = Valid(new SRAMRFWriteMicroOp)
  }
  val ff = new Bundle {
    val read = Vec(nFFRPorts, Valid(new FFRFReadMicroOp))
    val write = Valid(new FFRFWriteMicroOp)
  }
  val pred = new Bundle {
    val gread = Valid(new PredRFGatedReadMicroOp)
    val pread = Valid(new PredRFGatedReadMicroOp)
    val read = Vec(nPredRPorts, Valid(new PredRFReadMicroOp))
    val write = Valid(new PredRFWriteMicroOp)
  }
  val opl = new Bundle {
    val global = Vec(nGOPL, Valid(new OPLMicroOp))
    val local = Vec(nLOPL, Valid(new OPLMicroOp))
  }
  val pdl = new Bundle {
    val global = Vec(nGPDL, Valid(new PDLMicroOp))
    val local = Vec(nLPDL, Valid(new PDLMicroOp))
  }
  val sreg = Vec(nLOPL, Valid(new SRegMicroOp))
  val xbar = Vec(nGOPL, Valid(new XBarMicroOp))
  val pxbar = Vec(nGPDL, Valid(new PXBarMicroOp))
  val viu = Valid(new VIUMicroOp)
  val vipu = Valid(new VIPUMicroOp)
  val vpu = Valid(new VPUMicroOp)
  val vsu = Valid(new VSUMicroOp)
}

class BPQIO(implicit p: Parameters) extends DecoupledIO(new BPQEntry()(p)) {
}
class BRQIO(implicit p: Parameters) extends DecoupledIO(new BRQEntry()(p)) {
}
class BWQIO(implicit p: Parameters) extends DecoupledIO(new BWQEntry()(p)) {
}

class BankRWIO(implicit p: Parameters) extends VXUBundle()(p) {
  val pdl = Vec(nGPDL, new BankPredEntry()).asOutput
  val opl = Vec(nGOPL, new BankDataEntry()).asOutput
  val wpred = new BankPredMaskEntry().asInput
  val wdata = Vec(nWSel, new BankDataPredEntry()).asInput

  val bpq = new BPQIO
  val brq = new BRQIO
  val bwq = new Bundle {
    val mem = new BWQIO().flip
    val fu = new BWQIO().flip
  }
}

class Bank(bid: Int)(implicit p: Parameters) extends VXUModule()(p) with Packing with RateLogic {
  val io = new Bundle {
    val lid = UInt(INPUT)
    val cfg = new HwachaConfigIO().flip
    val op = new BankOpIO().flip
    val ack = new Bundle {
      val viu = Valid(new VIUAck)
      val vipu = Valid(new VIPUAck)
    }
    val rw = new BankRWIO
  }

  val rf = Module(new BankRegfile(bid))
  rf.suggestName("rfInst")

  rf.io.lid := io.lid
  rf.io.op <> io.op
  io.rw <> rf.io.global

  def valids(valid: Bool, pred: UInt, latency: Int) =
    ShiftRegister(Mux(valid, pred, Bits(0)), latency)

  // ALU
  val alu_pred = io.op.viu.bits.pred & rf.io.local.pdl(0).pred
  val alus = (0 until nSlices) map { i =>
    val alu = Module(new ALUSlice(bid*nSlices+i))
    alu.suggestName("aluInst")
    alu.io.cfg <> io.cfg
    alu.io.req.valid := io.op.viu.valid
    alu.io.req.bits.fn := io.op.viu.bits.fn
    alu.io.req.bits.eidx := io.op.viu.bits.eidx
    alu.io.req.bits.in0 :=
      Mux(io.op.sreg(0).valid, splat_scalar(io.op.sreg(0).bits),
                               unpack_slice(rf.io.local.opl(0).data, i))
    alu.io.req.bits.in1 :=
      Mux(io.op.sreg(1).valid, splat_scalar(io.op.sreg(1).bits),
                               unpack_slice(rf.io.local.opl(1).data, i))
    alu.io.req.bits.rate := io.op.viu.bits.rate
    alu.io.req.bits.pred := unpack_pred(alu_pred, i, alu.io.req.bits.rate)
    alu.io.resp
  }
  val alus_wpred = valids(io.op.viu.valid, alu_pred, stagesALU)

  // PLU: Predicate Logic Unit
  val plus = (0 until wPred) map { i =>
    val plu = Module(new PLUSlice)
    plu.suggestName("pluInst")
    plu.io.req.valid := io.op.vipu.valid && io.op.vipu.bits.pred(i)
    plu.io.req.bits.fn := io.op.vipu.bits.fn
    plu.io.req.bits.in0 := rf.io.local.rpred(0).pred(i)
    plu.io.req.bits.in1 := rf.io.local.rpred(1).pred(i)
    plu.io.req.bits.in2 := rf.io.local.rpred(2).pred(i)
    plu.io.resp
  }
  val plus_wpred = valids(io.op.vipu.valid, io.op.vipu.bits.pred, stagesPLU)

  rf.io.local.wpred(0).pred := Cat(alus.map(_.bits.cmp).reverse)
  rf.io.local.wpred(0).mask := alus_wpred
  rf.io.local.wpred(1).pred := Cat(plus.map(_.bits.out).reverse)
  rf.io.local.wpred(1).mask := plus_wpred
  rf.io.local.wdata.data := repack_slice(alus.map(_.bits.out))
  rf.io.local.wdata.pred := alus_wpred

  // BPQ
  io.rw.bpq.valid := io.op.vpu.valid
  io.rw.bpq.bits.pred := io.op.vpu.bits.pred & rf.io.local.ppred.pred

  assert(!io.op.vpu.valid || io.rw.bpq.ready, "bpq enabled when not ready; check bpq counters")

  // BRQ
  io.rw.brq.valid := io.op.vsu.valid && rf.io.local.pdl(1).active()
  io.rw.brq.bits.data :=
    Mux(io.op.sreg(2).valid, splat_slice(io.op.sreg(2).bits.operand),
                             rf.io.local.opl(2).data)

  assert(!io.op.vsu.valid || io.rw.brq.ready, "brq enabled when not ready; check brq counters")

  // ACK
  io.ack.viu.valid := alus.map(_.valid).reduce(_||_)
  io.ack.viu.bits.pred := alus_wpred
}

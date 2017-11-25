package hwacha

import Chisel._
import freechips.rocketchip.config._

abstract trait DCCParameters extends UsesHwachaParameters {
  val nDCCOpQ = 2
  val nDCCPredQ = 4
  val nVDUOperands = 2

  val nBPQ = 2*nBanks
  val nBRQ = 4
  val nBWQ = 2

  val maxSLA = 7 /* Ideally (2^i - 1) where (i > 1) */

  val nVLU = 2
  val bVLU = log2Ceil(nVLU)
}

class DCCAckIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val vidu = Valid(new VIDUAck)
  val vfdu = Valid(new VFDUAck)
}

class DCCIssueIO(implicit p: Parameters) extends Bundle {
  val op = Decoupled(new DCCOp).flip
}

class DecoupledCluster(implicit p: Parameters) extends VXUModule()(p) {
  val io = new DCCIssueIO {
    val cfg = new HwachaConfigIO().flip
    val ack = new DCCAckIO
    val lpqs = Vec(nLPQ, new LPQIO).flip
    val lrqs = Vec(nLRQ, new LRQIO).flip
    val bpqs = Vec(nBanks, new BPQIO).flip
    val brqs = Vec(nBanks, new BRQIO).flip
    val bwqs = new Bundle {
      val mem = Vec(nBanks, new BWQIO)
      val fu = Vec(nBanks, new BWQIO)
    }
    val dpla = new CounterLookAheadIO().flip // DCC LPQ counter
    val dqla = Vec(nVDUOperands, new CounterLookAheadIO).flip // DCC LRQ counter
    val dila = new CounterLookAheadIO().flip // idiv counter
    val dfla = new CounterLookAheadIO().flip // fdiv counter
    val gpla = new CounterLookAheadIO().flip // VGU LPQ counter
    val gqla = new CounterLookAheadIO().flip // VGU LRQ counter
    val pla = new BPQLookAheadIO().flip // VPU BPQ counter
    val lla = new CounterLookAheadIO().flip // VLU BWQ counter
    val sla = new BRQLookAheadIO().flip // VSU BRQ counter
    val red = new ReduceResultIO
    val vmu = new VMUIO
  }

  val vdu = Module(new VDU)
  vdu.suggestName("vduInst")
  val vgu = Module(new VGU)
  vgu.suggestName("vguInst")
  val vpu = Module(new VPU)
  vpu.suggestName("vpuInst")
  val vlu = Module(new VLU)
  vlu.suggestName("vluInst")
  val vsu = Module(new VSU)
  vsu.suggestName("vsuInst")

  val mask_vdu_ready = !io.op.bits.active.enq_vdu() || vdu.io.op.ready
  val mask_vgu_ready = !io.op.bits.active.enq_vgu() || vgu.io.op.ready
  val mask_vpu_ready = !io.op.bits.active.enq_vpu() || vpu.io.op.ready
  val mask_vlu_ready = !io.op.bits.active.enq_vlu() || vlu.io.op.ready
  val mask_vsu_ready = !io.op.bits.active.enq_vsu() || vsu.io.op.ready

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      io.op.valid, 
      mask_vdu_ready, mask_vgu_ready, mask_vpu_ready, mask_vlu_ready, mask_vsu_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }
  
  io.op.ready := fire(io.op.valid)

  vdu.io.cfg <> io.cfg
  vdu.io.op.valid := fire(mask_vdu_ready, io.op.bits.active.enq_vdu())
  vdu.io.op.bits := io.op.bits
  vdu.io.pla <> io.dpla
  vdu.io.qla <> io.dqla
  vdu.io.ila <> io.dila
  vdu.io.fla <> io.dfla
  vdu.io.lpq <> io.lpqs(0)
  vdu.io.lrqs(0) <> io.lrqs(0)
  vdu.io.lrqs(1) <> io.lrqs(1)
  io.ack <> vdu.io.ack
  io.bwqs.fu <> vdu.io.bwqs
  io.red <> vdu.io.red

  vgu.io.op.valid := fire(mask_vgu_ready, io.op.bits.active.enq_vgu())
  vgu.io.op.bits := io.op.bits
  vgu.io.pla <> io.gpla
  vgu.io.qla <> io.gqla
  vgu.io.lpq <> io.lpqs(1)
  vgu.io.lrq <> io.lrqs(2)
  io.vmu.vaq <> vgu.io.vaq

  vpu.io.op.valid := fire(mask_vpu_ready, io.op.bits.active.enq_vpu())
  vpu.io.op.bits := io.op.bits
  vpu.io.la <> io.pla
  vpu.io.bpqs <> io.bpqs
  io.vmu.pred <> vpu.io.pred
  vlu.io.pred <> vpu.io.lpred
  vsu.io.pred <> vpu.io.spred

  vlu.io.cfg <> io.cfg
  vlu.io.op.valid := fire(mask_vlu_ready, io.op.bits.active.enq_vlu())
  vlu.io.op.bits := io.op.bits
  vlu.io.la <> io.lla
  vlu.io.vldq <> io.vmu.vldq
  io.bwqs.mem <> vlu.io.bwqs
  io.vmu.vlu <> vlu.io.map

  vsu.io.op.valid := fire(mask_vsu_ready, io.op.bits.active.enq_vsu())
  vsu.io.op.bits := io.op.bits
  vsu.io.la <> io.sla
  vsu.io.brqs <> io.brqs
  io.vmu.vsdq <> vsu.io.vsdq
}

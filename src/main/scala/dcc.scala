package hwacha

import Chisel._

abstract trait DCCParameters extends UsesHwachaParameters {
  val nDCCOpQ = 2
  val nDCCPredQ = 2
  val nVDUOperands = 2

  val nBPQ = 2
  val nBRQ = 2
  val nBWQ = 2

  val maxSLA = 7 /* Ideally (2^i - 1) where (i > 1) */
}

class DCCAckIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val vidu = Valid(new VIDUAck)
  val vfdu = Valid(new VFDUAck)
}

class DecoupledCluster(implicit p: Parameters) extends VXUModule()(p) {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val op = Decoupled(new DCCOp).flip
    val ack = new DCCAckIO
    val lpqs = Vec.fill(nLPQ){new LPQIO}.flip
    val lrqs = Vec.fill(nLRQ){new LRQIO}.flip
    val bpqs = Vec.fill(nBanks){new BPQIO}.flip
    val brqs = Vec.fill(nBanks)(new BRQIO).flip
    val bwqs = new Bundle {
      val mem = Vec.fill(nBanks){new BWQIO}
      val fu = Vec.fill(nBanks){new BWQIO}
    }
    val dpla = new CounterLookAheadIO().flip
    val dqla = Vec.fill(nVDUOperands){new CounterLookAheadIO}.flip
    val dila = new CounterLookAheadIO().flip
    val dfla = new CounterLookAheadIO().flip
    val gpla = new CounterLookAheadIO().flip
    val gqla = new CounterLookAheadIO().flip
    val pla = new BPQLookAheadIO().flip
    val lla = new CounterLookAheadIO().flip
    val sla = new BRQLookAheadIO().flip
    val vmu = new LaneMemIO
  }

  val vdu = Module(new VDU)
  val vgu = Module(new VGU)
  val vpu = Module(new VPU)
  val vlu = Module(new VLU)
  val vsu = Module(new VSU)

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

  vsu.io.op.valid := fire(mask_vsu_ready, io.op.bits.active.enq_vsu())
  vsu.io.op.bits := io.op.bits
  vsu.io.la <> io.sla
  vsu.io.brqs <> io.brqs
  io.vmu.vsdq <> vsu.io.vsdq
}

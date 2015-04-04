package hwacha

import Chisel._
import Node._
import Constants._
import Packing._
import scala.collection.mutable.ArrayBuffer

case object HwachaNSRAMRFEntries extends Field[Int]
case object HwachaNFFRFEntries extends Field[Int]
case object HwachaNFFRFReadPorts extends Field[Int]
case object HwachaNOperandLatches extends Field[Int]
case object HwachaWriteSelects extends Field[Int]

abstract trait LaneParameters extends UsesParameters
{
  val nSRAM = params(HwachaNSRAMRFEntries)
  val nFF = params(HwachaNFFRFEntries)
  val nFFRPorts = params(HwachaNFFRFReadPorts)
  val nOPL = params(HwachaNOperandLatches)
  val nWSel = params(HwachaWriteSelects)
  val nSlices = SZ_DATA/SZ_D
  val nDecoupledUnitWBQueue = 4
}

class LaneOpIO extends HwachaBundle
{
  val bank = Vec.fill(nbanks){new BankOpIO()}
  val vqu = Valid(new VQUOp)
  val vgu = Valid(new VGUOp)
  val vimu = Valid(new VIMUOp)
  val vidu = Decoupled(new VIDUOp)
  val vfmu0 = Valid(new VFMUOp)
  val vfmu1 = Valid(new VFMUOp)
  val vfdu = Decoupled(new VFDUOp)
  val vfcu = Valid(new VFCUOp)
  val vfvu = Valid(new VFVUOp)
}

class LaneAckIO extends HwachaBundle
{
  val viu = Vec.fill(nbanks){Valid(new VIUAck)}
  val vqu = Valid(new VQUAck)
  val vgu = Valid(new VGUAck)
  val vimu = Valid(new VIMUAck)
  val vidu = Valid(new VIDUAck)
  val vfmu0 = Valid(new VFMUAck)
  val vfmu1 = Valid(new VFMUAck)
  val vfdu = Valid(new VFDUAck)
  val vfcu = Valid(new VFCUAck)
  val vfvu = Valid(new VFVUAck)
}

class Lane extends HwachaModule with LaneParameters
{
  val io = new Bundle {
    val op = new LaneOpIO().flip
    val ack = new LaneAckIO
    val brqs = Vec.fill(nbanks){new BRQIO}
    val bwqs = Vec.fill(nbanks){new BWQIO().flip}
    val vmu = new VMUIO
  }

  val banksrw = new ArrayBuffer[BankRWIO]
  val imuls = new ArrayBuffer[ValidIO[LaneIMulResult]]
  val fma0s = new ArrayBuffer[ValidIO[LaneFMAResult]]
  val fma1s = new ArrayBuffer[ValidIO[LaneFMAResult]]
  val fcmps = new ArrayBuffer[ValidIO[LaneFCmpResult]]
  val fconvs = new ArrayBuffer[ValidIO[LaneFConvResult]]
  val du = Module(new LaneDecoupledUnits)

  for (i <- 0 until nbanks) {
    val bank = Module(new Bank)

    // TODO: this needs to be sequenced
    bank.io.op <> io.op.bank(i)
    banksrw += bank.io.rw
    io.brqs(i) <> bank.io.rw.brq
    bank.io.rw.bwq.mem <> io.bwqs(i)
    bank.io.rw.bwq.fu <> du.io.bwqs(i)
  }

  val rdata = (0 until nOPL).map { o => banksrw.map(_.rdata(o).d).reduce(_|_) }

  val in0q = Module(new Queue(Bits(width = SZ_DATA), nbanks+2))
  val in1q = Module(new Queue(Bits(width = SZ_DATA), nbanks+2))

  in0q.io.enq.valid := io.op.vqu.valid && io.op.vqu.bits.fn.latch(0)
  in0q.io.enq.bits := rdata(3)
  in1q.io.enq.valid := io.op.vqu.valid && io.op.vqu.bits.fn.latch(1)
  in1q.io.enq.bits := rdata(4)

  assert(!in0q.io.enq.valid || in0q.io.enq.ready, "check in0q counter logic")
  assert(!in1q.io.enq.valid || in1q.io.enq.ready, "check in1q counter logic")

  du.io.in0q <> in0q.io.deq
  du.io.in1q <> in1q.io.deq
  du.io.idiv.op <> io.op.vidu
  du.io.fdiv.op <> io.op.vfdu

  io.vmu.vaq.q.valid := io.op.vgu.valid
  io.vmu.vaq.q.bits := rdata(5)
  io.vmu.vaq.pala.reserve := Bool(false)

  assert(!io.vmu.vaq.q.valid || io.vmu.vaq.q.ready, "check vaq counter logic")

  for (i <- 0 until nSlices) {
    val fma0 = Module(new LaneFMASlice)
    fma0.io.req.valid := io.op.vfmu0.valid && io.op.vfmu0.bits.pred(i)
    fma0.io.req.bits.fn := io.op.vfmu0.bits.fn
    fma0.io.req.bits.in0 := unpack_slice(rdata(0), i)
    fma0.io.req.bits.in1 := unpack_slice(rdata(1), i)
    fma0.io.req.bits.in2 := unpack_slice(rdata(2), i)
    fma0s += fma0.io.resp

    val imul = Module(new LaneIMulSlice)
    imul.io.req.valid := io.op.vimu.valid && io.op.vimu.bits.pred(i)
    imul.io.req.bits.fn := io.op.vimu.bits.fn
    imul.io.req.bits.in0 := unpack_slice(rdata(0), i)
    imul.io.req.bits.in1 := unpack_slice(rdata(1), i)
    imuls += imul.io.resp

    val fconv = Module(new LaneFConvSlice)
    fconv.io.req.valid := io.op.vfvu.valid && io.op.vfvu.bits.pred(i)
    fconv.io.req.bits.fn := io.op.vfvu.bits.fn
    fconv.io.req.bits.in := unpack_slice(rdata(2), i)
    fconvs += fconv.io.resp

    val fma1 = Module(new LaneFMASlice)
    fma1.io.req.valid := io.op.vfmu1.valid && io.op.vfmu1.bits.pred(i)
    fma1.io.req.bits.fn := io.op.vfmu1.bits.fn
    fma1.io.req.bits.in0 := unpack_slice(rdata(3), i)
    fma1.io.req.bits.in1 := unpack_slice(rdata(4), i)
    fma1.io.req.bits.in2 := unpack_slice(rdata(5), i)
    fma1s += fma1.io.resp

    val fcmp = Module(new LaneFCmpSlice)
    fcmp.io.req.valid := io.op.vfcu.valid && io.op.vfcu.bits.pred(i)
    fcmp.io.req.bits.fn := io.op.vfcu.bits.fn
    fcmp.io.req.bits.in0 := unpack_slice(rdata(3), i)
    fcmp.io.req.bits.in1 := unpack_slice(rdata(4), i)
    fcmps += fcmp.io.resp

    val idiv = Module(new LaneIDivSlice)
    idiv.io <> du.io.idiv.fus(i)

    val fdiv = Module(new LaneFDivSlice)
    fdiv.io <> du.io.fdiv.fus(i)
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

  io.ack.vqu.valid := io.op.vqu.valid
  io.ack.vgu.valid := io.op.vgu.valid
  io.ack.vimu.valid := imuls.map(_.valid).reduce(_|_)
  io.ack.vfmu0.valid := fma0s.map(_.valid).reduce(_|_)
  io.ack.vfmu1.valid := fma1s.map(_.valid).reduce(_|_)
  io.ack.vfcu.valid := fcmps.map(_.valid).reduce(_|_)
  io.ack.vfvu.valid := fconvs.map(_.valid).reduce(_|_)

  io.ack.vqu.bits.pred := io.op.vqu.bits.pred
  io.ack.vgu.bits.pred := io.op.vgu.bits.pred
  io.ack.vimu.bits.pred := Vec(imuls.map(_.valid)).toBits
  io.ack.vfmu0.bits.pred := Vec(fma0s.map(_.valid)).toBits
  io.ack.vfmu1.bits.pred := Vec(fma1s.map(_.valid)).toBits
  io.ack.vfcu.bits.pred := Vec(fcmps.map(_.valid)).toBits
  io.ack.vfvu.bits.pred := Vec(fconvs.map(_.valid)).toBits

  io.ack.vidu <> du.io.idiv.ack
  io.ack.vfdu <> du.io.fdiv.ack
}

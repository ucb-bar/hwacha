package hwacha

import Chisel._
import Node._

class SequencerOpIO extends ValidIO(new SequencerOp)

class Sequencer extends HwachaModule with LaneParameters
{
  val io = new Bundle {
    val op = new VXUIssueOpIO().flip
    val lane = new LaneOpIO
    val ack = new LaneAckIO().flip
  }

  // TODO: this is here to make sure things get vlenantiated

  for (i <- 0 until nbanks) {
    io.lane.bank(i).sram.read.valid := io.op.valid
    io.lane.bank(i).sram.read.bits.pred := io.op.bits.vlen
    io.lane.bank(i).sram.read.bits.addr := io.op.bits.vlen

    io.lane.bank(i).sram.write.valid := io.op.valid
    io.lane.bank(i).sram.write.bits.pred := io.op.bits.vlen
    io.lane.bank(i).sram.write.bits.addr := io.op.bits.vlen
    io.lane.bank(i).sram.write.bits.selg := io.op.bits.vlen(7)
    io.lane.bank(i).sram.write.bits.wsel := io.op.bits.vlen

    for (j <- 0 until nFFRPorts) {
      io.lane.bank(i).ff.read(j).valid := io.op.valid
      io.lane.bank(i).ff.read(j).bits.pred := io.op.bits.vlen
      io.lane.bank(i).ff.read(j).bits.addr := io.op.bits.vlen
    }

    io.lane.bank(i).ff.write.valid := io.op.valid
    io.lane.bank(i).ff.write.bits.pred := io.op.bits.vlen
    io.lane.bank(i).ff.write.bits.addr := io.op.bits.vlen
    io.lane.bank(i).ff.write.bits.selg := io.op.bits.vlen(7)
    io.lane.bank(i).ff.write.bits.wsel := io.op.bits.vlen

    io.lane.bank(i).opl.valid := io.op.valid
    io.lane.bank(i).opl.bits.pred := io.op.bits.vlen
    io.lane.bank(i).opl.bits.global.latch := io.op.bits.vlen
    io.lane.bank(i).opl.bits.global.selff := io.op.bits.vlen
    io.lane.bank(i).opl.bits.global.en := io.op.bits.vlen
    io.lane.bank(i).opl.bits.local.latch := io.op.bits.vlen
    io.lane.bank(i).opl.bits.local.selff := io.op.bits.vlen

    io.lane.bank(i).brq.valid := io.op.valid
    io.lane.bank(i).brq.bits.pred := io.op.bits.vlen
    io.lane.bank(i).brq.bits.selff := io.op.bits.vlen(7)
    io.lane.bank(i).brq.bits.zero := io.op.bits.vlen(7)

    io.lane.bank(i).viu.valid := io.op.valid
    io.lane.bank(i).viu.bits.pred := io.op.bits.vlen
    io.lane.bank(i).viu.bits.fn := new VIUFn().fromBits(io.op.bits.vlen)
    io.lane.bank(i).viu.bits.eidx := io.op.bits.vlen
  }

  io.lane.vqu.valid := io.op.valid
  io.lane.vqu.bits.pred := io.op.bits.vlen
  io.lane.vqu.bits.fn := new VQUFn().fromBits(io.op.bits.vlen)

  io.lane.vgu.valid := io.op.valid
  io.lane.vgu.bits.pred := io.op.bits.vlen
  io.lane.vgu.bits.fn := new VMUFn().fromBits(io.op.bits.vlen)

  io.lane.vimu.valid := io.op.valid
  io.lane.vimu.bits.pred := io.op.bits.vlen
  io.lane.vimu.bits.fn := new VIMUFn().fromBits(io.op.bits.vlen)

  io.lane.vidu.valid := io.op.valid
  io.lane.vidu.bits.pred := io.op.bits.vlen
  io.lane.vidu.bits.fn := new VIDUFn().fromBits(io.op.bits.vlen)
  io.lane.vidu.bits.bank := io.op.bits.vlen
  io.lane.vidu.bits.addr := io.op.bits.vlen
  io.lane.vidu.bits.selff := io.op.bits.vlen(8)

  io.lane.vfmu0.valid := io.op.valid
  io.lane.vfmu0.bits.pred := io.op.bits.vlen
  io.lane.vfmu0.bits.fn := new VFMUFn().fromBits(io.op.bits.vlen)

  io.lane.vfmu1.valid := io.op.valid
  io.lane.vfmu1.bits.pred := io.op.bits.vlen
  io.lane.vfmu1.bits.fn := new VFMUFn().fromBits(io.op.bits.vlen)

  io.lane.vfdu.valid := io.op.valid
  io.lane.vfdu.bits.pred := io.op.bits.vlen
  io.lane.vfdu.bits.fn := new VFDUFn().fromBits(io.op.bits.vlen)
  io.lane.vfdu.bits.bank := io.op.bits.vlen
  io.lane.vfdu.bits.addr := io.op.bits.vlen
  io.lane.vfdu.bits.selff := io.op.bits.vlen(8)

  io.lane.vfcu.valid := io.op.valid
  io.lane.vfcu.bits.pred := io.op.bits.vlen
  io.lane.vfcu.bits.fn := new VFCUFn().fromBits(io.op.bits.vlen)

  io.lane.vfvu.valid := io.op.valid
  io.lane.vfvu.bits.pred := io.op.bits.vlen
  io.lane.vfvu.bits.fn := new VFVUFn().fromBits(io.op.bits.vlen)
}

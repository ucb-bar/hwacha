package hwacha

import Chisel._
import Node._

class SequencerOpIO extends ValidIO(new SequencerOp)

class Sequencer extends HwachaModule with HwachaLaneParameters
{
  val io = new Bundle {
    val seqop = new SequencerOpIO().flip
    val laneop = new LaneOpIO
    val laneack = new LaneAckIO().flip
  }

  // TODO: this is here to make sure things get instantiated

  for (i <- 0 until nbanks) {
    io.laneop.bank(i).sram.read.valid := io.seqop.valid
    io.laneop.bank(i).sram.read.bits.pred := io.seqop.bits.inst
    io.laneop.bank(i).sram.read.bits.addr := io.seqop.bits.inst

    io.laneop.bank(i).sram.write.valid := io.seqop.valid
    io.laneop.bank(i).sram.write.bits.pred := io.seqop.bits.inst
    io.laneop.bank(i).sram.write.bits.addr := io.seqop.bits.inst
    io.laneop.bank(i).sram.write.bits.selg := io.seqop.valid
    io.laneop.bank(i).sram.write.bits.wsel := io.seqop.bits.inst

    for (j <- 0 until nFFRPorts) {
      io.laneop.bank(i).ff.read(j).valid := io.seqop.valid
      io.laneop.bank(i).ff.read(j).bits.pred := io.seqop.bits.inst
      io.laneop.bank(i).ff.read(j).bits.addr := io.seqop.bits.inst
    }

    io.laneop.bank(i).ff.write.valid := io.seqop.valid
    io.laneop.bank(i).ff.write.bits.pred := io.seqop.bits.inst
    io.laneop.bank(i).ff.write.bits.addr := io.seqop.bits.inst
    io.laneop.bank(i).ff.write.bits.selg := io.seqop.valid
    io.laneop.bank(i).ff.write.bits.wsel := io.seqop.bits.inst

    io.laneop.bank(i).opl.valid := io.seqop.valid
    io.laneop.bank(i).opl.bits.pred := io.seqop.bits.inst
    io.laneop.bank(i).opl.bits.global.latch := io.seqop.bits.inst
    io.laneop.bank(i).opl.bits.global.selff := io.seqop.bits.inst
    io.laneop.bank(i).opl.bits.global.en := io.seqop.bits.inst
    io.laneop.bank(i).opl.bits.local.latch := io.seqop.bits.inst
    io.laneop.bank(i).opl.bits.local.selff := io.seqop.bits.inst

    io.laneop.bank(i).brq.valid := io.seqop.valid
    io.laneop.bank(i).brq.bits.pred := io.seqop.bits.inst
    io.laneop.bank(i).brq.bits.selff := io.seqop.valid
    io.laneop.bank(i).brq.bits.zero := io.seqop.valid

    io.laneop.bank(i).viu.valid := io.seqop.valid
    io.laneop.bank(i).viu.bits.pred := io.seqop.bits.inst
    io.laneop.bank(i).viu.bits.fn := new VIUFn().fromBits(io.seqop.bits.inst)
    io.laneop.bank(i).viu.bits.eidx := io.seqop.bits.inst
  }

  io.laneop.vqu.valid := io.seqop.valid
  io.laneop.vqu.bits.pred := io.seqop.bits.inst
  io.laneop.vqu.bits.fn := new VQUFn().fromBits(io.seqop.bits.inst)

  io.laneop.vgu.valid := io.seqop.valid
  io.laneop.vgu.bits.pred := io.seqop.bits.inst
  io.laneop.vgu.bits.fn := new VMUFn().fromBits(io.seqop.bits.inst)

  io.laneop.vimu.valid := io.seqop.valid
  io.laneop.vimu.bits.pred := io.seqop.bits.inst
  io.laneop.vimu.bits.fn := new VIMUFn().fromBits(io.seqop.bits.inst)

  io.laneop.vidu.valid := io.seqop.valid
  io.laneop.vidu.bits.pred := io.seqop.bits.inst
  io.laneop.vidu.bits.fn := new VIDUFn().fromBits(io.seqop.bits.inst)

  io.laneop.vfmu0.valid := io.seqop.valid
  io.laneop.vfmu0.bits.pred := io.seqop.bits.inst
  io.laneop.vfmu0.bits.fn := new VFMUFn().fromBits(io.seqop.bits.inst)

  io.laneop.vfmu1.valid := io.seqop.valid
  io.laneop.vfmu1.bits.pred := io.seqop.bits.inst
  io.laneop.vfmu1.bits.fn := new VFMUFn().fromBits(io.seqop.bits.inst)

  io.laneop.vfdu.valid := io.seqop.valid
  io.laneop.vfdu.bits.pred := io.seqop.bits.inst
  io.laneop.vfdu.bits.fn := new VFDUFn().fromBits(io.seqop.bits.inst)

  io.laneop.vfcu.valid := io.seqop.valid
  io.laneop.vfcu.bits.pred := io.seqop.bits.inst
  io.laneop.vfcu.bits.fn := new VFCUFn().fromBits(io.seqop.bits.inst)

  io.laneop.vfvu.valid := io.seqop.valid
  io.laneop.vfvu.bits.pred := io.seqop.bits.inst
  io.laneop.vfvu.bits.fn := new VFVUFn().fromBits(io.seqop.bits.inst)
}

package hwacha

import Chisel._
import Node._

class Expander extends HwachaModule with LaneParameters
{
  val io = new Bundle {
    val seq = new SequencerIO().flip
    val lane = new LaneOpIO
  }

  // TODO: this is here to make sure things get vlenantiated
  
  //val temp_valid = io.op.valid
  val temp_valid = Bool(false)

  for (i <- 0 until nbanks) {
    io.lane.bank(i).sram.read.valid := temp_valid
    io.lane.bank(i).sram.read.bits.pred := io.seq.bits.strip
    io.lane.bank(i).sram.read.bits.addr := io.seq.bits.strip

    io.lane.bank(i).sram.write.valid := temp_valid
    io.lane.bank(i).sram.write.bits.pred := io.seq.bits.strip
    io.lane.bank(i).sram.write.bits.addr := io.seq.bits.strip
    io.lane.bank(i).sram.write.bits.selg := io.seq.bits.strip(0)
    io.lane.bank(i).sram.write.bits.wsel := io.seq.bits.strip

    for (j <- 0 until nFFRPorts) {
      io.lane.bank(i).ff.read(j).valid := temp_valid
      io.lane.bank(i).ff.read(j).bits.pred := io.seq.bits.strip
      io.lane.bank(i).ff.read(j).bits.addr := io.seq.bits.strip
    }

    io.lane.bank(i).ff.write.valid := temp_valid
    io.lane.bank(i).ff.write.bits.pred := io.seq.bits.strip
    io.lane.bank(i).ff.write.bits.addr := io.seq.bits.strip
    io.lane.bank(i).ff.write.bits.selg := io.seq.bits.strip(0)
    io.lane.bank(i).ff.write.bits.wsel := io.seq.bits.strip

    io.lane.bank(i).opl.valid := temp_valid
    io.lane.bank(i).opl.bits.pred := io.seq.bits.strip
    io.lane.bank(i).opl.bits.global.latch := io.seq.bits.strip
    io.lane.bank(i).opl.bits.global.selff := io.seq.bits.strip
    io.lane.bank(i).opl.bits.global.en := io.seq.bits.strip
    io.lane.bank(i).opl.bits.local.latch := io.seq.bits.strip
    io.lane.bank(i).opl.bits.local.selff := io.seq.bits.strip

    io.lane.bank(i).brq.valid := temp_valid
    io.lane.bank(i).brq.bits.pred := io.seq.bits.strip
    io.lane.bank(i).brq.bits.selff := io.seq.bits.strip(0)
    io.lane.bank(i).brq.bits.zero := io.seq.bits.strip(0)

    io.lane.bank(i).viu.valid := temp_valid
    io.lane.bank(i).viu.bits.pred := io.seq.bits.strip
    io.lane.bank(i).viu.bits.fn := io.seq.bits.fn.viu()
    io.lane.bank(i).viu.bits.eidx := io.seq.bits.strip
  }

  io.lane.vqu.valid := temp_valid
  io.lane.vqu.bits.pred := io.seq.bits.strip
  io.lane.vqu.bits.fn := io.seq.bits.fn.vqu()

  io.lane.vgu.valid := temp_valid
  io.lane.vgu.bits.pred := io.seq.bits.strip
  io.lane.vgu.bits.fn := io.seq.bits.fn.vmu()

  io.lane.vimu.valid := temp_valid
  io.lane.vimu.bits.pred := io.seq.bits.strip
  io.lane.vimu.bits.fn := io.seq.bits.fn.vimu()

  io.lane.vidu.valid := temp_valid
  io.lane.vidu.bits.pred := io.seq.bits.strip
  io.lane.vidu.bits.fn := io.seq.bits.fn.vidu()
  io.lane.vidu.bits.bank := io.seq.bits.strip
  io.lane.vidu.bits.addr := io.seq.bits.strip
  io.lane.vidu.bits.selff := io.seq.bits.strip(0)

  io.lane.vfmu0.valid := temp_valid
  io.lane.vfmu0.bits.pred := io.seq.bits.strip
  io.lane.vfmu0.bits.fn := io.seq.bits.fn.vfmu()

  io.lane.vfmu1.valid := temp_valid
  io.lane.vfmu1.bits.pred := io.seq.bits.strip
  io.lane.vfmu1.bits.fn := io.seq.bits.fn.vfmu()

  io.lane.vfdu.valid := temp_valid
  io.lane.vfdu.bits.pred := io.seq.bits.strip
  io.lane.vfdu.bits.fn := io.seq.bits.fn.vfdu()
  io.lane.vfdu.bits.bank := io.seq.bits.strip
  io.lane.vfdu.bits.addr := io.seq.bits.strip
  io.lane.vfdu.bits.selff := io.seq.bits.strip(0)

  io.lane.vfcu.valid := temp_valid
  io.lane.vfcu.bits.pred := io.seq.bits.strip
  io.lane.vfcu.bits.fn := io.seq.bits.fn.vfcu()

  io.lane.vfvu.valid := temp_valid
  io.lane.vfvu.bits.pred := io.seq.bits.strip
  io.lane.vfvu.bits.fn := io.seq.bits.fn.vfvu()
}

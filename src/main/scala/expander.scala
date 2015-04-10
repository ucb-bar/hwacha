package hwacha

import Chisel._
import Node._

class TickerIO extends Bundle with SeqParameters {
  val sram = new Bundle {
    val read = Vec.fill(nRPorts){Valid(new SRAMRFReadLaneOp)}
    val write = Vec.fill(maxWPortLatency){Valid(new SRAMRFWriteLaneOp)}
  }
  val vsu = Vec.fill(nRPorts+2){Valid(new VSULaneOp)}
}

class Expander extends HwachaModule with SeqParameters with LaneParameters
{
  val io = new Bundle {
    val seq = new SequencerIO().flip
    val lane = new LaneOpIO
    val ticker = new TickerIO
  }

  class Ticker[T <: Data](gen: T, n: Int)
  {
    val s = Vec.fill(n){Reg(Valid(gen.clone).asDirectionless)}

    (0 until n).reverse.foreach(i => ({
      val step_en = if (i == n-1) Bool(false) else s(i+1).valid
      s(i).valid := step_en
      if (i != n-1) {
        when (step_en) {
          s(i).bits := s(i+1).bits
        }
      }
      when (reset) {
        s(i).valid := Bool(false)
      }
    }))

    def ondeck = s(0)
  }

  class BuildExpander
  {
    val tick_sram_read = new Ticker(new SRAMRFReadExpEntry, nRPorts)
    val tick_sram_write = new Ticker(new SRAMRFWriteExpEntry, maxWPortLatency)
    val tick_viu = new Ticker(new VIULaneOp, nRPorts+2)
    val tick_vsu = new Ticker(new VSULaneOp, nRPorts+2)

    val rport_valid = Vec(Seq(
      io.seq.bits.reg.vs1.valid && !io.seq.bits.reg.vs1.scalar,
      io.seq.bits.reg.vs2.valid && !io.seq.bits.reg.vs2.scalar,
      io.seq.bits.reg.vs3.valid && !io.seq.bits.reg.vs3.scalar))

    val rport_idx = Vec(Seq(
      UInt(0),
      Mux(rport_valid(0), UInt(1), UInt(0)),
      Mux(rport_valid(0),
        Mux(rport_valid(1), UInt(2), UInt(1)),
        Mux(rport_valid(1), UInt(1), UInt(0)))))

    def mark_opl(n: UInt, idx: Int) = {
      // 3-ported operations
      when (io.seq.bits.active.vfmu) {
        tick_sram_read.s(n).bits.global.valid := Bool(true)
        tick_sram_read.s(n).bits.global.id := UInt(idx)
      }

      // 2-ported operations
      if (idx <= 1) {
        when (io.seq.bits.active.vimu) {
          tick_sram_read.s(n).bits.global.valid := Bool(true)
          tick_sram_read.s(n).bits.global.id := UInt(idx)
        }
        when (io.seq.bits.active.vqu || io.seq.bits.active.vfcu) {
          tick_sram_read.s(n).bits.global.valid := Bool(true)
          tick_sram_read.s(n).bits.global.id := UInt(3+idx)
        }
        when (io.seq.bits.active.viu) {
          tick_sram_read.s(n).bits.local.valid := Bool(true)
          tick_sram_read.s(n).bits.local.id := UInt(idx)
        }
      }

      // 1-ported operations
      if (idx <= 0) {
        when (io.seq.bits.active.vfvu) {
          tick_sram_read.s(n).bits.global.valid := Bool(true)
          tick_sram_read.s(n).bits.global.id := UInt(2)
        }
        when (io.seq.bits.active.vgu) {
          tick_sram_read.s(n).bits.global.valid := Bool(true)
          tick_sram_read.s(n).bits.global.id := UInt(5)
        }
      }
    }

    def mark_sram_reads = {
      Seq((d: DecodedRegisters) => d.vs1,
          (d: DecodedRegisters) => d.vs2,
          (d: DecodedRegisters) => d.vs3).zipWithIndex.foreach {
        case (fn, idx) => {
          val read_idx = rport_idx(idx)
          when (rport_valid(idx)) {
            tick_sram_read.s(read_idx).valid := Bool(true)
            tick_sram_read.s(read_idx).bits.addr := fn(io.seq.bits.reg).id
            tick_sram_read.s(read_idx).bits.strip := io.seq.bits.strip
            mark_opl(read_idx, idx)
          }
        }
      }
    }

    def mark_sram_writes = {
      val wport_idx = io.seq.bits.wport
      when (io.seq.bits.reg.vd.valid && !io.seq.bits.reg.vd.scalar) {
        tick_sram_write.s(wport_idx).valid := Bool(true)
        tick_sram_write.s(wport_idx).bits.addr := io.seq.bits.reg.vd.id
        tick_sram_write.s(wport_idx).bits.selg := Bool(false)
        tick_sram_write.s(wport_idx).bits.strip := io.seq.bits.strip
      }
    }

    val op_idx = io.seq.bits.rports + UInt(1, szRPorts+1)

    def mark_viu = {
      when (io.seq.bits.active.viu) {
        tick_viu.s(op_idx).valid := Bool(true)
        tick_viu.s(op_idx).bits.fn := io.seq.bits.fn.viu()
        tick_viu.s(op_idx).bits.strip := io.seq.bits.strip
      }
    }

    def mark_vsu = {
      when (io.seq.bits.active.vsu) {
        tick_vsu.s(op_idx).valid := Bool(true)
        tick_vsu.s(op_idx).bits.selff := Bool(false)
        tick_vsu.s(op_idx).bits.strip := io.seq.bits.strip
      }
    }
  }

  val exp = new BuildExpander

  when (io.seq.valid) {
    exp.mark_sram_reads
    exp.mark_sram_writes
    exp.mark_viu
    exp.mark_vsu
  }

  val sram_read = exp.tick_sram_read.ondeck

  io.lane.sram.read <> sram_read
  io.lane.sram.write <> exp.tick_sram_write.ondeck

  val opl = Reg(Valid(new OPLLaneOp).asDirectionless)
  opl.valid := sram_read.valid && (sram_read.bits.global.valid || sram_read.bits.local.valid)
  when (sram_read.valid && sram_read.bits.global.valid) {
    opl.bits.global.latch := UInt(1) << sram_read.bits.global.id
    opl.bits.local.latch := Bits(0)
  }
  when (sram_read.valid && sram_read.bits.local.valid) {
    opl.bits.global.latch := Bits(0)
    opl.bits.local.latch := UInt(1) << sram_read.bits.local.id
  }

  io.lane.opl.valid := opl.valid
  io.lane.opl.bits.global.latch := opl.bits.global.latch
  io.lane.opl.bits.global.selff := Bits(0)
  io.lane.opl.bits.local.latch := opl.bits.local.latch
  io.lane.opl.bits.local.selff := Bits(0)

  io.lane.viu <> exp.tick_viu.ondeck
  io.lane.vsu <> exp.tick_vsu.ondeck

  io.lane.ff.read.valid := Bool(false)
  io.lane.ff.write.valid := Bool(false)
  io.lane.xbar.valid := Bool(false)
  io.lane.vimu.valid := Bool(false)
  io.lane.vfmu0.valid := Bool(false)
  io.lane.vfmu1.valid := Bool(false)
  io.lane.vfcu.valid := Bool(false)
  io.lane.vfvu.valid := Bool(false)
  io.lane.vqu.valid := Bool(false)
  io.lane.vgu.valid := Bool(false)

  io.ticker.sram.read zip exp.tick_sram_read.s foreach { case (io, tick) => io := tick }
  io.ticker.sram.write zip exp.tick_sram_write.s foreach { case (io, tick) => io := tick }
  io.ticker.vsu zip exp.tick_vsu.s foreach { case (io, tick) => io := tick }
}

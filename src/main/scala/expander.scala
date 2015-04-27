package hwacha

import Chisel._

abstract trait ExpParameters extends UsesHwachaParameters with SeqParameters {
  val expLatency = 1

  val rpVIU = 2
  val rpVIMU = 2
  val rpVFMU = 3
  val rpVFCU = 2
  val rpVFVU = 1
  val rpVGU = 1
  val rpVSU = 1
  val rpVQU = 2

  val maxSRAMReadTicks = nRPorts
  val maxSRAMWriteTicks = maxWPortLatency
  val maxSRegGlobalTicks = nRPorts+2
  val maxSRegLocalTicks = rpVIU+2
  val maxXbarTicks = nRPorts+2
  val maxVIUTicks = rpVIU+2
  val maxVIMUTicks = rpVIMU+2
  val maxVFMUTicks = rpVFMU+2
  val maxVFCUTicks = rpVFCU+2
  val maxVFVUTicks = rpVFVU+2
  val maxVGUTicks = rpVGU+2
  val maxVSUTicks = rpVSU+2
  val maxVQUTicks = rpVQU+2
}

class TickerIO extends VXUBundle {
  val sram = new Bundle {
    val read = Vec.fill(maxSRAMReadTicks){Valid(new SRAMRFReadExpEntry)}
    val write = Vec.fill(maxSRAMWriteTicks){Valid(new SRAMRFWriteExpEntry)}
  }
  val sreg = new Bundle {
    val global = Vec.fill(nGOPL){Vec.fill(maxSRegGlobalTicks){Valid(new SRegLaneOp)}}
    val local = Vec.fill(nLOPL){Vec.fill(maxSRegLocalTicks){Valid(new SRegLaneOp)}}
  }
  val xbar = Vec.fill(nGOPL){Vec.fill(maxXbarTicks){Valid(new XBarLaneOp)}}
  val viu = Vec.fill(maxVIUTicks){Valid(new VIULaneOp)}
  val vimu = Vec.fill(maxVIMUTicks){Valid(new VIMULaneOp)}
  val vfmu = Vec.fill(nVFMU){Vec.fill(maxVFMUTicks){Valid(new VFMULaneOp)}}
  val vfcu = Vec.fill(maxVFCUTicks){Valid(new VFCULaneOp)}
  val vfvu = Vec.fill(maxVFVUTicks){Valid(new VFVULaneOp)}
  val vgu = Vec.fill(maxVGUTicks){Valid(new VGULaneOp)}
  val vsu = Vec.fill(maxVSUTicks){Valid(new VSULaneOp)}
  val vqu = Vec.fill(maxVQUTicks){Valid(new VQULaneOp)}
}

class Expander extends VXUModule {
  val io = new Bundle {
    val seq = new SequencerIO().flip
    val lane = new LaneOpIO
    val ticker = new TickerIO
  }

  class Ticker[T <: Data](gen: T, n: Int) {
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

    def connect(tio: Vec[ValidIO[T]]) =
      (tio zip s) foreach { case (io, lop) => io := lop }
  }

  class BuildExpander {
    val tick_sram_read = new Ticker(new SRAMRFReadExpEntry, maxSRAMReadTicks)
    val tick_sram_write = new Ticker(new SRAMRFWriteExpEntry, maxSRAMWriteTicks)
    val tick_sreg_global = IndexedSeq.fill(nGOPL){new Ticker(new SRegLaneOp, maxSRegGlobalTicks)}
    val tick_sreg_local = IndexedSeq.fill(nLOPL){new Ticker(new SRegLaneOp, maxSRegLocalTicks)}
    val tick_xbar = IndexedSeq.fill(nGOPL){new Ticker(new XBarLaneOp, maxXbarTicks)}
    val tick_viu = new Ticker(new VIULaneOp, maxVIUTicks)
    val tick_vimu = new Ticker(new VIMULaneOp, maxVIMUTicks)
    val tick_vfmu = IndexedSeq.fill(nVFMU){new Ticker(new VFMULaneOp, maxVFMUTicks)}
    val tick_vfcu = new Ticker(new VFCULaneOp, maxVFCUTicks)
    val tick_vfvu = new Ticker(new VFVULaneOp, maxVFVUTicks)
    val tick_vgu = new Ticker(new VGULaneOp, maxVGUTicks)
    val tick_vsu = new Ticker(new VSULaneOp, maxVSUTicks)
    val tick_vqu = new Ticker(new VQULaneOp, maxVQUTicks)

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

    val op_idx = io.seq.bits.rports + UInt(1, bRPorts+1)

    def mark_opl(n: UInt, idx: Int) = {
      val e = tick_sram_read.s(n)

      e.bits.global.valid := Bool(false)
      e.bits.local.valid := Bool(false)

      // 3-ported operations
      (0 until nVFMU) foreach { i =>
        when (io.seq.bits.active_vfmu(i)) {
          e.bits.global.valid := Bool(true)
          e.bits.global.id := UInt(3*i+idx)
        }
      }

      // 2-ported operations
      if (idx <= 1) {
        when (io.seq.bits.active.vimu) {
          e.bits.global.valid := Bool(true)
          e.bits.global.id := UInt(idx)
        }
        when (io.seq.bits.active.vqu || io.seq.bits.active.vfcu) {
          e.bits.global.valid := Bool(true)
          e.bits.global.id := UInt(3+idx)
        }
        when (io.seq.bits.active.viu) {
          e.bits.local.valid := Bool(true)
          e.bits.local.id := UInt(idx)
        }
      }

      // 1-ported operations
      if (idx <= 0) {
        when (io.seq.bits.active.vfvu) {
          e.bits.global.valid := Bool(true)
          e.bits.global.id := UInt(2)
        }
        when (io.seq.bits.active.vgu) {
          e.bits.global.valid := Bool(true)
          e.bits.global.id := UInt(5)
        }
        when (io.seq.bits.active.vsu) {
          e.bits.local.valid := Bool(true)
          e.bits.local.id := UInt(2+idx)
        }
      }
    }

    def mark_sram_reads = {
      Seq(reg_vs1, reg_vs2, reg_vs3).zipWithIndex.foreach {
        case (fn, idx) => {
          val read_idx = rport_idx(idx)
          when (rport_valid(idx)) {
            val e = tick_sram_read.s(read_idx)
            e.valid := Bool(true)
            e.bits.addr := fn(io.seq.bits.reg).id
            e.bits.strip := io.seq.bits.strip
            mark_opl(read_idx, idx)
          }
        }
      }
    }

    def mark_sram_writes = {
      when (io.seq.bits.reg.vd.valid && !io.seq.bits.reg.vd.scalar) {
        val e = tick_sram_write.s(io.seq.bits.wport)
        e.valid := Bool(true)
        e.bits.addr := io.seq.bits.reg.vd.id
        e.bits.strip := io.seq.bits.strip
        e.bits.selg := Bool(false)
        (0 until nVFMU) foreach { i =>
          when (io.seq.bits.active_vfmu(i)) {
            e.bits.selg := Bool(true)
            e.bits.wsel := UInt(i)
          }
        }
        when (io.seq.bits.active.vimu || io.seq.bits.active.vfvu) {
          e.bits.selg := Bool(true)
          e.bits.wsel := UInt(0)
        }
        when (io.seq.bits.active.vfcu) {
          e.bits.selg := Bool(true)
          e.bits.wsel := UInt(1)
        }
      }
    }

    val reg_vs1 = (reg: DecodedRegisters) => reg.vs1
    val reg_vs2 = (reg: DecodedRegisters) => reg.vs2
    val reg_vs3 = (reg: DecodedRegisters) => reg.vs3
    val sreg_ss1 = (sreg: ScalarRegisters) => sreg.ss1
    val sreg_ss2 = (sreg: ScalarRegisters) => sreg.ss2
    val sreg_ss3 = (sreg: ScalarRegisters) => sreg.ss3

    def mark_xbar(i: Int, fn: DecodedRegisters=>RegInfo) = {
      val rinfo = fn(io.seq.bits.reg)
      when (rinfo.valid && !rinfo.scalar) {
        tick_xbar(i).s(op_idx).valid := Bool(true)
        tick_xbar(i).s(op_idx).bits.strip := io.seq.bits.strip
      }
    }

    def mark_sreg(t: Seq[Ticker[SRegLaneOp]], i: Int, fn: DecodedRegisters=>RegInfo, sfn: ScalarRegisters=>Bits) = {
      val rinfo = fn(io.seq.bits.reg)
      when (rinfo.valid && rinfo.scalar) {
        t(i).s(op_idx).valid := Bool(true)
        t(i).s(op_idx).bits.operand := sfn(io.seq.bits.sreg)
        t(i).s(op_idx).bits.strip := io.seq.bits.strip
      }
    }
    def mark_sreg_global(i: Int, fn: DecodedRegisters=>RegInfo, sfn: ScalarRegisters=>Bits) =
      mark_sreg(tick_sreg_global, i, fn, sfn)
    def mark_sreg_local(i: Int, fn: DecodedRegisters=>RegInfo, sfn: ScalarRegisters=>Bits) =
      mark_sreg(tick_sreg_local, i, fn, sfn)

    def mark_xbars_sregs = {
      (0 until nVFMU) foreach { i =>
        when (io.seq.bits.active_vfmu(i)) {
          mark_xbar(3*i+0, reg_vs1)
          mark_xbar(3*i+1, reg_vs2)
          mark_xbar(3*i+2, reg_vs3)
          mark_sreg_global(3*i+0, reg_vs1, sreg_ss1)
          mark_sreg_global(3*i+1, reg_vs2, sreg_ss2)
          mark_sreg_global(3*i+2, reg_vs3, sreg_ss3)
        }
      }

      when (io.seq.bits.active.vimu) {
        mark_xbar(0, reg_vs1)
        mark_xbar(1, reg_vs2)
        mark_sreg_global(0, reg_vs1, sreg_ss1)
        mark_sreg_global(1, reg_vs2, sreg_ss2)
      }

      when (io.seq.bits.active.vfvu) {
        mark_xbar(2, reg_vs1)
        mark_sreg_global(2, reg_vs1, sreg_ss1)
      }

      when (io.seq.bits.active.vqu || io.seq.bits.active.vfcu) {
        mark_xbar(3, reg_vs1)
        mark_xbar(4, reg_vs2)
        mark_sreg_global(3, reg_vs1, sreg_ss1)
        mark_sreg_global(4, reg_vs2, sreg_ss2)
      }

      when (io.seq.bits.active.vgu) {
        mark_xbar(5, reg_vs1)
        mark_sreg_global(5, reg_vs1, sreg_ss1)
      }

      when (io.seq.bits.active.viu) {
        mark_sreg_local(0, reg_vs1, sreg_ss1)
        mark_sreg_local(1, reg_vs2, sreg_ss2)
      }

      when (io.seq.bits.active.vsu) {
        mark_sreg_local(2, reg_vs1, sreg_ss1)
      }
    }

    def mark_vfu[T <: LaneOp](tick: Ticker[T], opfn: SequencerOp=>Bool, fn: T=>Unit) = {
      when (opfn(io.seq.bits)) {
        tick.s(op_idx).valid := Bool(true)
        tick.s(op_idx).bits.strip := io.seq.bits.strip
        fn(tick.s(op_idx).bits)
      }
    }

    def mark_viu = mark_vfu(tick_viu, (op: SequencerOp) => op.active.viu,
      (lop: VIULaneOp) => { lop.fn := io.seq.bits.fn.viu(); lop.eidx := io.seq.bits.eidx })
    def mark_vimu = mark_vfu(tick_vimu, (op: SequencerOp) => op.active.vimu,
      (lop: VIMULaneOp) => { lop.fn := io.seq.bits.fn.vimu() })
    def mark_vfmu(i: Int) = mark_vfu(tick_vfmu(i), (op: SequencerOp) => op.active_vfmu(i),
      (lop: VFMULaneOp) => { lop.fn := io.seq.bits.fn.vfmu() })
    def mark_vfcu = mark_vfu(tick_vfcu, (op: SequencerOp) => op.active.vfcu,
      (lop: VFCULaneOp) => { lop.fn := io.seq.bits.fn.vfcu() })
    def mark_vfvu = mark_vfu(tick_vfvu, (op: SequencerOp) => op.active.vfvu,
      (lop: VFVULaneOp) => { lop.fn := io.seq.bits.fn.vfvu() })
    def mark_vgu = mark_vfu(tick_vgu, (op: SequencerOp) => op.active.vgu,
      (lop: VGULaneOp) => { lop.fn := io.seq.bits.fn.vmu() })
    def mark_vsu = mark_vfu(tick_vsu, (op: SequencerOp) => op.active.vsu,
      (lop: VSULaneOp) => { lop.selff := Bool(false) })
    def mark_vqu = mark_vfu(tick_vqu, (op: SequencerOp) => op.active.vqu,
      (lop: VQULaneOp) => { lop.fn := io.seq.bits.fn.vqu() })
  }

  val exp = new BuildExpander

  when (io.seq.valid) {
    exp.mark_sram_reads
    exp.mark_sram_writes
    exp.mark_xbars_sregs
    exp.mark_viu
    exp.mark_vimu
    (0 until nVFMU) foreach { i => exp.mark_vfmu(i) }
    exp.mark_vfcu
    exp.mark_vfvu
    exp.mark_vgu
    exp.mark_vsu
    exp.mark_vqu
  }

  io.lane.sram.read <> exp.tick_sram_read.ondeck
  io.lane.sram.write <> exp.tick_sram_write.ondeck
  (0 until nFFRPorts).foreach { io.lane.ff.read(_).valid := Bool(false) }
  io.lane.ff.write.valid := Bool(false)

  val s1_sram_read = Pipe(exp.tick_sram_read.ondeck.valid, exp.tick_sram_read.ondeck.bits)
  (io.lane.opl.global ++ io.lane.opl.local).foreach { opl =>
    opl.valid := Bool(false)
    opl.bits.selff := Bool(false) // FIXME
    opl.bits.strip := s1_sram_read.bits.strip
  }
  when (s1_sram_read.valid && s1_sram_read.bits.global.valid) {
    io.lane.opl.global(s1_sram_read.bits.global.id).valid := Bool(true)
  }
  when (s1_sram_read.valid && s1_sram_read.bits.local.valid) {
    io.lane.opl.local(s1_sram_read.bits.local.id).valid := Bool(true)
  }

  (0 until nGOPL) foreach { i => io.lane.sreg.global(i) := exp.tick_sreg_global(i).ondeck }
  (0 until nLOPL) foreach { i => io.lane.sreg.local(i) := exp.tick_sreg_local(i).ondeck }
  (0 until nGOPL) foreach { i => io.lane.xbar(i) := exp.tick_xbar(i).ondeck }
  io.lane.viu <> exp.tick_viu.ondeck
  io.lane.vimu <> exp.tick_vimu.ondeck
  (0 until nVFMU) foreach { i => io.lane.vfmu(i) <> exp.tick_vfmu(i).ondeck }
  io.lane.vfcu <> exp.tick_vfcu.ondeck
  io.lane.vfvu <> exp.tick_vfvu.ondeck
  io.lane.vgu <> exp.tick_vgu.ondeck
  io.lane.vsu <> exp.tick_vsu.ondeck
  io.lane.vqu <> exp.tick_vqu.ondeck

  exp.tick_sram_read connect io.ticker.sram.read
  exp.tick_sram_write connect io.ticker.sram.write
  (0 until nGOPL) foreach { i => exp.tick_sreg_global(i) connect io.ticker.sreg.global(i) }
  (0 until nLOPL) foreach { i => exp.tick_sreg_local(i) connect io.ticker.sreg.local(i) }
  (0 until nGOPL) foreach { i => exp.tick_xbar(i) connect io.ticker.xbar(i) }
  exp.tick_viu connect io.ticker.viu
  exp.tick_vimu connect io.ticker.vimu
  (0 until nVFMU) foreach { i => exp.tick_vfmu(i) connect io.ticker.vfmu(i) }
  exp.tick_vfcu connect io.ticker.vfcu
  exp.tick_vfvu connect io.ticker.vfvu
  exp.tick_vgu connect io.ticker.vgu
  exp.tick_vsu connect io.ticker.vsu
  exp.tick_vqu connect io.ticker.vqu
}

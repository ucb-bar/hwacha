package hwacha

import Chisel._

abstract trait ExpParameters extends UsesHwachaParameters with SeqParameters {
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
  val maxPredReadTicks = maxSRAMReadTicks
  val maxPredWriteTicks = maxPredWPortLatency
  val maxSRegGlobalTicks = nRPorts+2
  val maxSRegLocalTicks = rpVIU+2
  val maxXbarTicks = nRPorts+2
  val maxPXbarTicks = maxXbarTicks
  val maxVIUTicks = rpVIU+2
  val maxVIPUTicks = 1
  val maxVIMUTicks = rpVIMU+2
  val maxVFMUTicks = rpVFMU+2
  val maxVFCUTicks = rpVFCU+2
  val maxVFVUTicks = rpVFVU+2
  val maxVGUTicks = rpVGU+2
  val maxVPUTicks = 1
  val maxVSUTicks = rpVSU+2
  val maxVQUTicks = rpVQU+2
}

class TickerIO extends VXUBundle {
  val sram = new Bundle {
    val read = Vec.fill(maxSRAMReadTicks){Valid(new SRAMRFReadExpEntry)}
    val write = Vec.fill(maxSRAMWriteTicks){Valid(new SRAMRFWriteExpEntry)}
  }
  val pred = new Bundle {
    val read = Vec.fill(nPredRPorts){Vec.fill(maxPredReadTicks){Valid(new PredRFReadExpEntry)}}
    val write = Vec.fill(maxPredWriteTicks){Valid(new PredRFWriteExpEntry)}
  }
  val sreg = new Bundle {
    val global = Vec.fill(nGOPL){Vec.fill(maxSRegGlobalTicks){Valid(new SRegLaneOp)}}
    val local = Vec.fill(nLOPL){Vec.fill(maxSRegLocalTicks){Valid(new SRegLaneOp)}}
  }
  val xbar = Vec.fill(nGOPL){Vec.fill(maxXbarTicks){Valid(new XBarLaneOp)}}
  val pxbar = Vec.fill(nGPDL){Vec.fill(maxPXbarTicks){Valid(new PXBarLaneOp)}}
  val viu = Vec.fill(maxVIUTicks){Valid(new VIULaneOp)}
  val vipu = Vec.fill(maxVIPUTicks){Valid(new VIPULaneOp)}
  val vpu = Vec.fill(maxVPUTicks){Valid(new VPULaneOp)}
  val vsu = Vec.fill(maxVSUTicks){Valid(new VSULaneOp)}
  val vqu = Vec.fill(maxVQUTicks){Valid(new VQULaneOp)}
  val vgu = Vec.fill(maxVGUTicks){Valid(new VGULaneOp)}
  val vimu = Vec.fill(maxVIMUTicks){Valid(new VIMULaneOp)}
  val vfmu = Vec.fill(nVFMU){Vec.fill(maxVFMUTicks){Valid(new VFMULaneOp)}}
  val vfcu = Vec.fill(maxVFCUTicks){Valid(new VFCULaneOp)}
  val vfvu = Vec.fill(maxVFVUTicks){Valid(new VFVULaneOp)}
}

class Expander extends VXUModule {
  val io = new Bundle {
    val seq = new SequencerIO().flip
    val seq_vpu = new SequencerVPUIO().flip
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

  val exp = new {
    val tick_sram_read = new Ticker(new SRAMRFReadExpEntry, maxSRAMReadTicks)
    val tick_sram_write = new Ticker(new SRAMRFWriteExpEntry, maxSRAMWriteTicks)
    val tick_pred_read = IndexedSeq.fill(nPredRPorts){new Ticker(new PredRFReadExpEntry, maxPredReadTicks)}
    val tick_pred_write = new Ticker(new PredRFWriteExpEntry, maxPredWriteTicks)
    val tick_sreg_global = IndexedSeq.fill(nGOPL){new Ticker(new SRegLaneOp, maxSRegGlobalTicks)}
    val tick_sreg_local = IndexedSeq.fill(nLOPL){new Ticker(new SRegLaneOp, maxSRegLocalTicks)}
    val tick_xbar = IndexedSeq.fill(nGOPL){new Ticker(new XBarLaneOp, maxXbarTicks)}
    val tick_pxbar = IndexedSeq.fill(nGPDL){new Ticker(new PXBarLaneOp, maxPXbarTicks)}
    val tick_viu = new Ticker(new VIULaneOp, maxVIUTicks)
    val tick_vipu = new Ticker(new VIPULaneOp, maxVIPUTicks)
    val tick_vpu = new Ticker(new VPULaneOp, maxVPUTicks)
    val tick_vsu = new Ticker(new VSULaneOp, maxVSUTicks)
    val tick_vqu = new Ticker(new VQULaneOp, maxVQUTicks)
    val tick_vgu = new Ticker(new VGULaneOp, maxVGUTicks)
    val tick_vimu = new Ticker(new VIMULaneOp, maxVIMUTicks)
    val tick_vfmu = IndexedSeq.fill(nVFMU){new Ticker(new VFMULaneOp, maxVFMUTicks)}
    val tick_vfcu = new Ticker(new VFCULaneOp, maxVFCUTicks)
    val tick_vfvu = new Ticker(new VFVULaneOp, maxVFVUTicks)

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

    val op_idx = io.seq.bits.rports + UInt(expLatency, bRPorts+1)

    def check_assert[T <: LaneOp](name: String, t: Ticker[T], n: UInt) = {
      assert(n < UInt(t.s.size), name+" asking over limit")
      assert(n === UInt(t.s.size-1) || !t.s(n+UInt(1, n.getWidth+1)).valid, name+" scheduling is wrong")
    }

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
          e.bits.local.id := UInt(2)
        }
      }
    }

    def mark_pdl(n: UInt, idx: Int) = {
      val p = tick_pred_read(0).s(n)

      p.bits.global.valid := Bool(false)
      p.bits.local.valid := Bool(false)

      // 3-ported operations
      (0 until nVFMU) foreach { i =>
        when (io.seq.bits.active_vfmu(i)) {
          p.bits.global.valid := Bool(true)
          p.bits.global.id := UInt(2*i)
        }
      }

      // 2-ported operations
      if (idx <= 1) {
        when (io.seq.bits.active.vimu) {
          p.bits.global.valid := Bool(true)
          p.bits.global.id := UInt(0)
        }
        when (io.seq.bits.active.vqu || io.seq.bits.active.vfcu) {
          p.bits.global.valid := Bool(true)
          p.bits.global.id := UInt(2)
        }
        when (io.seq.bits.active.viu) {
          p.bits.local.valid := Bool(true)
          p.bits.local.id := UInt(0)
        }
      }

      // 1-ported operations
      if (idx <= 0) {
        when (io.seq.bits.active.vfvu) {
          p.bits.global.valid := Bool(true)
          p.bits.global.id := UInt(1)
        }
        when (io.seq.bits.active.vgu) {
          p.bits.global.valid := Bool(true)
          p.bits.global.id := UInt(3)
        }
        when (io.seq.bits.active.vsu) {
          p.bits.local.valid := Bool(true)
          p.bits.local.id := UInt(1)
        }
      }
    }

    def mark_sram_reads = {
      (Seq(reg_vs1, reg_vs2, reg_vs3) zipWithIndex) foreach {
        case (fn, idx) => {
          val read_idx = rport_idx(idx)
          when (rport_valid(idx)) {
            check_assert("sram read", tick_sram_read, read_idx)
            val e = tick_sram_read.s(read_idx)
            e.valid := Bool(true)
            e.bits.addr := fn(io.seq.bits.reg).id
            e.bits.strip := io.seq.bits.strip
            mark_opl(read_idx, idx)
          }
        }
      }
    }

    def mark_pred_reads = {
      val null_rport = !rport_valid.reduce(_||_)
      (0 until 3) foreach { idx =>
        val read_idx = rport_idx(idx)
        when (rport_valid(idx) || null_rport && Bool(idx == 0)) {
          check_assert("pred read", tick_pred_read(0), read_idx)
          val p = tick_pred_read(0).s(read_idx)
          p.valid := Bool(true)
          p.bits.off := Bool(true)
          p.bits.strip := io.seq.bits.strip
          mark_pdl(read_idx, idx)
        }
      }
    }

    def mark_sram_writes = {
      when (io.seq.bits.reg.vd.valid && !io.seq.bits.reg.vd.scalar) {
        check_assert("sram write", tick_sram_write, io.seq.bits.wport)
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

    def mark_xbar(i: Int, p: Int, fn: RegFn) = {
      val rinfo = fn(io.seq.bits.reg)
      when (rinfo.valid && !rinfo.scalar) {
        check_assert("xbar"+i, tick_xbar(i), op_idx)
        tick_xbar(i).s(op_idx).valid := Bool(true)
        tick_xbar(i).s(op_idx).bits.pdladdr := UInt(p)
        tick_xbar(i).s(op_idx).bits.strip := io.seq.bits.strip
      }
    }

    def mark_pxbar(i: Int) = {
      check_assert("pxbar"+i, tick_pxbar(i), op_idx)
      tick_pxbar(i).s(op_idx).valid := Bool(true)
      tick_pxbar(i).s(op_idx).bits.strip := io.seq.bits.strip
    }

    def mark_sreg(name: String, t: IndexedSeq[Ticker[SRegLaneOp]], i: Int, fn: RegFn, sfn: SRegFn) = {
      val rinfo = fn(io.seq.bits.reg)
      when (rinfo.valid && rinfo.scalar) {
        check_assert("sreg"+name+i, t(i), op_idx)
        t(i).s(op_idx).valid := Bool(true)
        t(i).s(op_idx).bits.operand := sfn(io.seq.bits.sreg)
        t(i).s(op_idx).bits.strip := io.seq.bits.strip
      }
    }
    def mark_sreg_global(i: Int, fn: RegFn, sfn: SRegFn) =
      mark_sreg("global", tick_sreg_global, i, fn, sfn)
    def mark_sreg_local(i: Int, fn: RegFn, sfn: SRegFn) =
      mark_sreg("local", tick_sreg_local, i, fn, sfn)

    def mark_xbars_pxbars_sregs = {
      (0 until nVFMU) foreach { i =>
        when (io.seq.bits.active_vfmu(i)) {
          mark_xbar(3*i+0, 2*i, reg_vs1)
          mark_xbar(3*i+1, 2*i, reg_vs2)
          mark_xbar(3*i+2, 2*i, reg_vs3)
          mark_pxbar(2*i)
          mark_sreg_global(3*i+0, reg_vs1, sreg_ss1)
          mark_sreg_global(3*i+1, reg_vs2, sreg_ss2)
          mark_sreg_global(3*i+2, reg_vs3, sreg_ss3)
        }
      }

      when (io.seq.bits.active.vimu) {
        mark_xbar(0, 0, reg_vs1)
        mark_xbar(1, 0, reg_vs2)
        mark_pxbar(0)
        mark_sreg_global(0, reg_vs1, sreg_ss1)
        mark_sreg_global(1, reg_vs2, sreg_ss2)
      }

      when (io.seq.bits.active.vfvu) {
        mark_xbar(2, 1, reg_vs1)
        mark_pxbar(1)
        mark_sreg_global(2, reg_vs1, sreg_ss1)
      }

      when (io.seq.bits.active.vqu || io.seq.bits.active.vfcu) {
        mark_xbar(3, 2, reg_vs1)
        mark_xbar(4, 2, reg_vs2)
        mark_pxbar(2)
        mark_sreg_global(3, reg_vs1, sreg_ss1)
        mark_sreg_global(4, reg_vs2, sreg_ss2)
      }

      when (io.seq.bits.active.vgu) {
        mark_xbar(5, 3, reg_vs1)
        mark_pxbar(3)
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

    def mark_vfu[T <: LaneOp](name: String, tick: Ticker[T], opfn: SequencerOp=>Bool, fn: T=>Unit) = {
      when (opfn(io.seq.bits)) {
        check_assert(name, tick, op_idx)
        tick.s(op_idx).valid := Bool(true)
        tick.s(op_idx).bits.strip := io.seq.bits.strip
        fn(tick.s(op_idx).bits)
      }
    }

    def mark_lop_sreg(sreg: Vec[Bool], nregs: Int) = {
      (Seq(reg_vs1, reg_vs2, reg_vs3) zipWithIndex) map { case (fn, i) =>
        if (nregs > i) {
          val rinfo = fn(io.seq.bits.reg)
          sreg(i) := rinfo.valid && rinfo.scalar
        }
      }
    }

    def mark_viu = mark_vfu("viu", tick_viu, (op: SequencerOp) => op.active.viu,
      (lop: VIULaneOp) => { lop.fn := io.seq.bits.fn.viu(); lop.eidx := io.seq.bits.eidx })
    def mark_vsu = mark_vfu("vsu", tick_vsu, (op: SequencerOp) => op.active.vsu,
      (lop: VSULaneOp) => { lop.selff := Bool(false) })
    def mark_vqu = mark_vfu("vqu", tick_vqu, (op: SequencerOp) => op.active.vqu,
      (lop: VQULaneOp) => { lop.fn := io.seq.bits.fn.vqu(); mark_lop_sreg(lop.sreg, 2) })
    def mark_vgu = mark_vfu("vgu", tick_vgu, (op: SequencerOp) => op.active.vgu,
      (lop: VGULaneOp) => { lop.fn := io.seq.bits.fn.vmu(); mark_lop_sreg(lop.sreg, 1) })
    def mark_vimu = mark_vfu("vimu", tick_vimu, (op: SequencerOp) => op.active.vimu,
      (lop: VIMULaneOp) => { lop.fn := io.seq.bits.fn.vimu(); mark_lop_sreg(lop.sreg, 2) })
    def mark_vfmu(i: Int) = mark_vfu("vfmu" + i, tick_vfmu(i), (op: SequencerOp) => op.active_vfmu(i),
      (lop: VFMULaneOp) => { lop.fn := io.seq.bits.fn.vfmu(); mark_lop_sreg(lop.sreg, 3) })
    def mark_vfcu = mark_vfu("vfcu", tick_vfcu, (op: SequencerOp) => op.active.vfcu,
      (lop: VFCULaneOp) => { lop.fn := io.seq.bits.fn.vfcu(); mark_lop_sreg(lop.sreg, 2) })
    def mark_vfvu = mark_vfu("vfvu", tick_vfvu, (op: SequencerOp) => op.active.vfvu,
      (lop: VFVULaneOp) => { lop.fn := io.seq.bits.fn.vfvu(); mark_lop_sreg(lop.sreg, 1) })

    def mark_vpu = {
      check_assert("pred read", tick_pred_read(1), UInt(0))
      tick_pred_read(1).s(0).valid := Bool(true)
      tick_pred_read(1).s(0).bits.off := Bool(true)
      tick_pred_read(1).s(0).bits.strip := io.seq_vpu.bits.strip
      check_assert("vpu", tick_vpu, UInt(0))
      tick_vpu.s(0).valid  := Bool(true)
      tick_vpu.s(0).bits.strip := io.seq_vpu.bits.strip
    }
  }

  when (io.seq.valid) {
    exp.mark_sram_reads
    exp.mark_pred_reads
    exp.mark_sram_writes
    exp.mark_xbars_pxbars_sregs
    exp.mark_viu
    exp.mark_vsu
    exp.mark_vqu
    exp.mark_vgu
    exp.mark_vimu
    (0 until nVFMU) foreach { i => exp.mark_vfmu(i) }
    exp.mark_vfcu
    exp.mark_vfvu
  }

  when (io.seq_vpu.valid) {
    exp.mark_vpu
  }

  io.lane.sram.read <> exp.tick_sram_read.ondeck
  io.lane.sram.write <> exp.tick_sram_write.ondeck
  (0 until nPredRPorts) foreach { i => io.lane.pred.read(i) <> exp.tick_pred_read(i).ondeck }
  io.lane.pred.write <> exp.tick_pred_write.ondeck
  (0 until nFFRPorts) foreach { io.lane.ff.read(_).valid := Bool(false) }
  io.lane.ff.write.valid := Bool(false)

  val s1_sram_read = Pipe(exp.tick_sram_read.ondeck.valid, exp.tick_sram_read.ondeck.bits)
  (io.lane.opl.global ++ io.lane.opl.local) foreach { opl =>
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

  val s1_pred_read = Pipe(exp.tick_pred_read(0).ondeck.valid, exp.tick_pred_read(0).ondeck.bits)
  (io.lane.pdl.global ++ io.lane.pdl.local) foreach { pdl =>
    pdl.valid := Bool(false)
    pdl.bits.strip := s1_pred_read.bits.strip
  }
  when (s1_pred_read.valid && s1_pred_read.bits.global.valid) {
    io.lane.pdl.global(s1_pred_read.bits.global.id).valid := Bool(true)
  }
  when (s1_pred_read.valid && s1_pred_read.bits.local.valid) {
    io.lane.pdl.local(s1_pred_read.bits.local.id).valid := Bool(true)
  }

  (0 until nGOPL) foreach { i => io.lane.sreg.global(i) <> exp.tick_sreg_global(i).ondeck }
  (0 until nLOPL) foreach { i => io.lane.sreg.local(i) <> exp.tick_sreg_local(i).ondeck }
  (0 until nGOPL) foreach { i => io.lane.xbar(i) <> exp.tick_xbar(i).ondeck }
  (0 until nGPDL) foreach { i => io.lane.pxbar(i) <> exp.tick_pxbar(i).ondeck }
  io.lane.viu <> exp.tick_viu.ondeck
  io.lane.vipu <> exp.tick_vipu.ondeck
  io.lane.vpu <> exp.tick_vpu.ondeck
  io.lane.vsu <> exp.tick_vsu.ondeck
  io.lane.vqu <> exp.tick_vqu.ondeck
  io.lane.vgu <> exp.tick_vgu.ondeck
  io.lane.vimu <> exp.tick_vimu.ondeck
  (0 until nVFMU) foreach { i => io.lane.vfmu(i) <> exp.tick_vfmu(i).ondeck }
  io.lane.vfcu <> exp.tick_vfcu.ondeck
  io.lane.vfvu <> exp.tick_vfvu.ondeck

  exp.tick_sram_read connect io.ticker.sram.read
  exp.tick_sram_write connect io.ticker.sram.write
  (0 until nPredRPorts) foreach { i => exp.tick_pred_read(i) connect io.ticker.pred.read(i) }
  exp.tick_pred_write connect io.ticker.pred.write
  (0 until nGOPL) foreach { i => exp.tick_sreg_global(i) connect io.ticker.sreg.global(i) }
  (0 until nLOPL) foreach { i => exp.tick_sreg_local(i) connect io.ticker.sreg.local(i) }
  (0 until nGOPL) foreach { i => exp.tick_xbar(i) connect io.ticker.xbar(i) }
  (0 until nGPDL) foreach { i => exp.tick_pxbar(i) connect io.ticker.pxbar(i) }
  exp.tick_viu connect io.ticker.viu
  exp.tick_vipu connect io.ticker.vipu
  exp.tick_vpu connect io.ticker.vpu
  exp.tick_vsu connect io.ticker.vsu
  exp.tick_vqu connect io.ticker.vqu
  exp.tick_vgu connect io.ticker.vgu
  exp.tick_vimu connect io.ticker.vimu
  (0 until nVFMU) foreach { i => exp.tick_vfmu(i) connect io.ticker.vfmu(i) }
  exp.tick_vfcu connect io.ticker.vfcu
  exp.tick_vfvu connect io.ticker.vfvu
}

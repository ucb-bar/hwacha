package hwacha

import Chisel._
import freechips.rocketchip.config._

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
  val maxPredReadTicks = 1
  val maxPredWriteTicks = maxPredWPortLatency
  val maxSRegGlobalTicks = nRPorts+2
  val maxSRegLocalTicks = rpVIU+2
  val maxXbarTicks = nRPorts+2
  val maxPXbarTicks = maxXbarTicks
  val maxVIUTicks = rpVIU+2
  val maxVIPUTicks = 2
  val maxVIMUTicks = rpVIMU+2
  val maxVFMUTicks = rpVFMU+2
  val maxVFCUTicks = rpVFCU+2
  val maxVFVUTicks = rpVFVU+2
  val maxVGUTicks = rpVGU+2
  val maxVPUTicks = 2
  val maxVSUTicks = rpVSU+2
  val maxVQUTicks = rpVQU+2
}

class TickerIO(implicit p: Parameters) extends VXUBundle()(p) {
  val sram = new Bundle {
    val read = Vec(maxSRAMReadTicks, Valid(new SRAMRFReadExpEntry))
    val write = Vec(maxSRAMWriteTicks, Valid(new SRAMRFWriteExpEntry))
  }
  val pred = new Bundle {
    val gread = Vec(maxSRAMReadTicks, Valid(new PredRFReadExpEntry))
    val pread = Vec(maxPredReadTicks, Valid(new PredRFGatedReadLaneOp))
    val read = Vec(nPredRPorts, Vec(maxPredReadTicks, Valid(new PredRFReadLaneOp)))
    val write = Vec(maxPredWriteTicks, Valid(new PredRFWriteExpEntry))
  }
  val sreg = new Bundle {
    val global = Vec(nGOPL, Vec(maxSRegGlobalTicks, Valid(new SRegLaneOp)))
    val local = Vec(nLOPL, Vec(maxSRegLocalTicks, Valid(new SRegLaneOp)))
  }
  val xbar = Vec(nGOPL, Vec(maxXbarTicks, Valid(new XBarLaneOp)))
  val pxbar = Vec(nGPDL, Vec(maxPXbarTicks, Valid(new PXBarLaneOp)))
  val viu = Vec(maxVIUTicks, Valid(new VIULaneOp))
  val vipu = Vec(maxVIPUTicks, Valid(new VIPULaneOp))
  val vpu = Vec(maxVPUTicks, Valid(new VPULaneOp))
  val vsu = Vec(maxVSUTicks, Valid(new VSULaneOp))
  val vqu = Vec(maxVQUTicks, Valid(new VQULaneOp))
  val vgu = Vec(maxVGUTicks, Valid(new VGULaneOp))
  val vimu = Vec(maxVIMUTicks, Valid(new VIMULaneOp))
  val vfmu = Vec(nVFMU, Vec(maxVFMUTicks, Valid(new VFMULaneOp)))
  val vfcu = Vec(maxVFCUTicks, Valid(new VFCULaneOp))
  val vfvu = Vec(maxVFVUTicks, Valid(new VFVULaneOp))
}

class Expander(implicit p: Parameters) extends VXUModule()(p) {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val seq = new SequencerIO().flip
    val lane = new LaneOpIO
    val ticker = new TickerIO
  }

  class Ticker[T <: Data](gen: T, n: Int) {
    val s = Reg(Vec(n,Valid(gen.cloneType)))

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
    val tick_pred_gread = new Ticker(new PredRFReadExpEntry, maxSRAMReadTicks)
    val tick_pred_pread = new Ticker(new PredRFGatedReadLaneOp, maxPredReadTicks)
    val tick_pred_read = IndexedSeq.fill(nPredRPorts){new Ticker(new PredRFReadLaneOp, maxPredReadTicks)}
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

    val seq_exp = io.seq.exp.bits
    val seq_vipu = io.seq.vipu.bits
    val seq_vpu = io.seq.vpu.bits

    val rport_valid = Vec(Seq(
      seq_exp.reg.vs1.valid && seq_exp.reg.vs1.is_vector(),
      seq_exp.reg.vs2.valid && seq_exp.reg.vs2.is_vector(),
      seq_exp.reg.vs3.valid && seq_exp.reg.vs3.is_vector()))

    val rport_idx = Vec(Seq(
      UInt(0),
      Mux(rport_valid(0), UInt(1), UInt(0)),
      Mux(rport_valid(0),
        Mux(rport_valid(1), UInt(2), UInt(1)),
        Mux(rport_valid(1), UInt(1), UInt(0)))))

    val op_idx = seq_exp.rports + UInt(expLatency, bRPorts+1)

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
        when (seq_exp.active_vfmu(i)) {
          e.bits.global.valid := Bool(true)
          e.bits.global.id := UInt(3*i+idx)
        }
      }

      // 2-ported operations
      if (idx <= 1) {
        when (seq_exp.active.vimu) {
          e.bits.global.valid := Bool(true)
          e.bits.global.id := UInt(idx)
        }
        when (seq_exp.active.vqu || seq_exp.active.vfcu) {
          e.bits.global.valid := Bool(true)
          e.bits.global.id := UInt(3+idx)
        }
        when (seq_exp.active.viu) {
          e.bits.local.valid := Bool(true)
          e.bits.local.id := UInt(idx)
        }
      }

      // 1-ported operations
      if (idx <= 0) {
        when (seq_exp.active.vfvu) {
          e.bits.global.valid := Bool(true)
          e.bits.global.id := UInt(2)
        }
        when (seq_exp.active.vgu) {
          e.bits.global.valid := Bool(true)
          e.bits.global.id := UInt(5)
        }
        when (seq_exp.active.vsu) {
          e.bits.local.valid := Bool(true)
          e.bits.local.id := UInt(2)
        }
      }
    }

    def mark_sram_reads = {
      (Seq(preg_vs1, preg_vs2, preg_vs3) zipWithIndex) foreach {
        case (fn, idx) => {
          val read_idx = rport_idx(idx)
          when (rport_valid(idx)) {
            check_assert("sram read", tick_sram_read, read_idx)
            val e = tick_sram_read.s(read_idx)
            e.valid := Bool(true)
            e.bits.addr := fn(seq_exp.reg).id
            e.bits.strip := seq_exp.strip
            e.bits.rate := seq_exp.rate
            e.bits.pack.prec := fn(seq_exp.reg).prec
            e.bits.pack.idx := seq_exp.pack.idx
            mark_opl(read_idx, idx)
          }
        }
      }
    }

    def mark_pdl(n: UInt, idx: Int) = {
      val p = tick_pred_gread.s(n)

      p.bits.global.valid := Bool(false)
      p.bits.local.valid := Bool(false)

      // 3-ported operations
      (0 until nVFMU) foreach { i =>
        when (seq_exp.active_vfmu(i)) {
          p.bits.global.valid := Bool(true)
          p.bits.global.id := UInt(2*i)
        }
      }

      // 2-ported operations
      if (idx <= 1) {
        when (seq_exp.active.vimu) {
          p.bits.global.valid := Bool(true)
          p.bits.global.id := UInt(0)
        }
        when (seq_exp.active.vqu || seq_exp.active.vfcu) {
          p.bits.global.valid := Bool(true)
          p.bits.global.id := UInt(2)
        }
        when (seq_exp.active.viu) {
          p.bits.local.valid := Bool(true)
          p.bits.local.id := UInt(0)
        }
      }

      // 1-ported operations
      if (idx <= 0) {
        when (seq_exp.active.vfvu) {
          p.bits.global.valid := Bool(true)
          p.bits.global.id := UInt(1)
        }
        when (seq_exp.active.vgu) {
          p.bits.global.valid := Bool(true)
          p.bits.global.id := UInt(3)
        }
        when (seq_exp.active.vsu) {
          p.bits.local.valid := Bool(true)
          p.bits.local.id := UInt(1)
        }
      }
    }

    def mark_pred_read = {
      val null_rport = !rport_valid.reduce(_||_)
      (0 until 3) foreach { idx =>
        val read_idx = rport_idx(idx)
        when (rport_valid(idx) || null_rport && Bool(idx == 0)) {
          check_assert("pred gread", tick_pred_gread, read_idx)
          assert(seq_exp.reg.vp.valid && seq_exp.reg.vp.is_pred(), "gread with no guard predicate")
          val p = tick_pred_gread.s(read_idx)
          p.valid := Bool(true)
          p.bits.off := io.cfg.unpred
          p.bits.neg := seq_exp.reg.vp.neg()
          p.bits.addr := seq_exp.reg.vp.id
          p.bits.strip := seq_exp.strip
          p.bits.pack.idx := seq_exp.pack.idx
          mark_pdl(read_idx, idx)
        }
      }
    }

    def mark_sram_pred_write = {
      when (seq_exp.reg.vd.valid) {
        when (seq_exp.reg.vd.is_vector()) {
          check_assert("sram write", tick_sram_write, seq_exp.wport.sram)
          val e = tick_sram_write.s(seq_exp.wport.sram)
          e.valid := Bool(true)
          e.bits.id := seq_exp.base.vd.id
          e.bits.addr := seq_exp.reg.vd.id
          e.bits.strip := seq_exp.strip
          e.bits.selg := Bool(false)
          (0 until nVFMU) foreach { i =>
            when (seq_exp.active_vfmu(i)) {
              e.bits.selg := Bool(true)
              e.bits.wsel := UInt(i)
            }
          }
          when (seq_exp.active.vimu || seq_exp.active.vfvu) {
            e.bits.selg := Bool(true)
            e.bits.wsel := UInt(0)
          }
          when (seq_exp.active.vfcu) {
            e.bits.selg := Bool(true)
            e.bits.wsel := UInt(1)
          }
          e.bits.sidx := seq_exp.sidx
          e.bits.rate := seq_exp.rate
          e.bits.pack.prec := seq_exp.reg.vd.prec
          e.bits.pack.idx := seq_exp.pack.idx
        }
        when (seq_exp.reg.vd.is_pred()) {
          check_assert("pred write", tick_pred_write, seq_exp.wport.pred)
          assert(seq_exp.active.viu || seq_exp.active.vfcu, "check pred write logic")
          val e = tick_pred_write.s(seq_exp.wport.pred)
          e.valid := Bool(true)
          e.bits.id := seq_exp.base.vd.id
          e.bits.addr := seq_exp.reg.vd.id
          e.bits.strip := seq_exp.strip
          when (seq_exp.active.viu) {
            e.bits.selg := Bool(false)
            e.bits.plu := Bool(false)
          }
          when (seq_exp.active.vfcu) {
            e.bits.selg := Bool(true) // plu bit doesn't matter
          }
          e.bits.sidx := seq_exp.sidx
          e.bits.rate := seq_exp.rate
          e.bits.pack.idx := seq_exp.pack.idx
        }
      }
    }

    def mark_xbar(i: Int, p: Int, fn: PRegFn) = {
      val rinfo = fn(seq_exp.reg)
      assert(!rinfo.valid || !rinfo.is_pred(), "xbar op shouldn't be a pred operand")
      when (rinfo.valid && rinfo.is_vector()) {
        check_assert("xbar"+i, tick_xbar(i), op_idx)
        tick_xbar(i).s(op_idx).valid := Bool(true)
        tick_xbar(i).s(op_idx).bits.pdladdr := UInt(p)
        tick_xbar(i).s(op_idx).bits.strip := seq_exp.strip
      }
    }

    def mark_pxbar(i: Int) = {
      check_assert("pxbar"+i, tick_pxbar(i), op_idx)
      tick_pxbar(i).s(op_idx).valid := Bool(true)
      tick_pxbar(i).s(op_idx).bits.strip := seq_exp.strip
    }

    def mark_sreg(name: String, t: IndexedSeq[Ticker[SRegLaneOp]], i: Int, fn: PRegFn, sfn: SRegFn) = {
      val rinfo = fn(seq_exp.reg)
      assert(!rinfo.valid || !rinfo.is_pred(), "sreg op shouldn't be a pred operand")
      when (rinfo.valid && rinfo.is_scalar()) {
        check_assert("sreg"+name+i, t(i), op_idx)
        t(i).s(op_idx).valid := Bool(true)
        t(i).s(op_idx).bits.operand := sfn(seq_exp.sreg)
        t(i).s(op_idx).bits.strip := seq_exp.strip
        t(i).s(op_idx).bits.rate := seq_exp.rate
      }
    }
    def mark_sreg_global(i: Int, fn: PRegFn, sfn: SRegFn) =
      mark_sreg("global", tick_sreg_global, i, fn, sfn)
    def mark_sreg_local(i: Int, fn: PRegFn, sfn: SRegFn) =
      mark_sreg("local", tick_sreg_local, i, fn, sfn)

    def mark_xbars_pxbars_sregs = {
      (0 until nVFMU) foreach { i =>
        when (seq_exp.active_vfmu(i)) {
          mark_xbar(3*i+0, 2*i, preg_vs1)
          mark_xbar(3*i+1, 2*i, preg_vs2)
          mark_xbar(3*i+2, 2*i, preg_vs3)
          mark_pxbar(2*i)
          mark_sreg_global(3*i+0, preg_vs1, sreg_ss1)
          mark_sreg_global(3*i+1, preg_vs2, sreg_ss2)
          mark_sreg_global(3*i+2, preg_vs3, sreg_ss3)
        }
      }

      when (seq_exp.active.vimu) {
        mark_xbar(0, 0, preg_vs1)
        mark_xbar(1, 0, preg_vs2)
        mark_pxbar(0)
        mark_sreg_global(0, preg_vs1, sreg_ss1)
        mark_sreg_global(1, preg_vs2, sreg_ss2)
      }

      when (seq_exp.active.vfvu) {
        mark_xbar(2, 1, preg_vs1)
        mark_pxbar(1)
        mark_sreg_global(2, preg_vs1, sreg_ss1)
      }

      when (seq_exp.active.vqu || seq_exp.active.vfcu) {
        mark_xbar(3, 2, preg_vs1)
        mark_xbar(4, 2, preg_vs2)
        mark_pxbar(2)
        mark_sreg_global(3, preg_vs1, sreg_ss1)
        mark_sreg_global(4, preg_vs2, sreg_ss2)
      }

      when (seq_exp.active.vgu) {
        mark_xbar(5, 3, preg_vs1)
        mark_pxbar(3)
        mark_sreg_global(5, preg_vs1, sreg_ss1)
      }

      when (seq_exp.active.viu) {
        mark_sreg_local(0, preg_vs1, sreg_ss1)
        mark_sreg_local(1, preg_vs2, sreg_ss2)
      }

      when (seq_exp.active.vsu) {
        mark_sreg_local(2, preg_vs1, sreg_ss1)
      }
    }

    def mark_vfu[T <: LaneOp](name: String, tick: Ticker[T], opfn: SeqOp=>Bool, fn: T=>Unit) = {
      when (opfn(seq_exp)) {
        check_assert(name, tick, op_idx)
        tick.s(op_idx).valid := Bool(true)
        tick.s(op_idx).bits.strip := seq_exp.strip
        tick.s(op_idx).bits.rate := seq_exp.rate
        fn(tick.s(op_idx).bits)
      }
    }

    def mark_lop_sreg(sreg: Vec[Bool], nregs: Int) = {
      (Seq(preg_vs1, preg_vs2, preg_vs3) zipWithIndex) map { case (fn, i) =>
        if (nregs > i) {
          val rinfo = fn(seq_exp.reg)
          sreg(i) := rinfo.valid && rinfo.is_scalar()
        }
      }
    }

    def mark_viu = mark_vfu("viu", tick_viu, (op: SeqOp) => op.active.viu,
      (lop: VIULaneOp) => { lop.fn := seq_exp.fn.viu(); lop.eidx := seq_exp.eidx })
    def mark_vsu = mark_vfu("vsu", tick_vsu, (op: SeqOp) => op.active.vsu,
      (lop: VSULaneOp) => { lop.selff := Bool(false) })
    def mark_vqu = mark_vfu("vqu", tick_vqu, (op: SeqOp) => op.active.vqu,
      (lop: VQULaneOp) => { lop.fn := seq_exp.fn.vqu(); mark_lop_sreg(lop.sreg, 2) })
    def mark_vgu = mark_vfu("vgu", tick_vgu, (op: SeqOp) => op.active.vgu,
      (lop: VGULaneOp) => { lop.fn := seq_exp.fn.vmu(); mark_lop_sreg(lop.sreg, 1) })
    def mark_vimu = mark_vfu("vimu", tick_vimu, (op: SeqOp) => op.active.vimu,
      (lop: VIMULaneOp) => { lop.fn := seq_exp.fn.vimu(); mark_lop_sreg(lop.sreg, 2) })
    def mark_vfmu(i: Int) = mark_vfu("vfmu" + i, tick_vfmu(i), (op: SeqOp) => op.active_vfmu(i),
      (lop: VFMULaneOp) => { lop.fn := seq_exp.fn.vfmu(); mark_lop_sreg(lop.sreg, 3) })
    def mark_vfcu = mark_vfu("vfcu", tick_vfcu, (op: SeqOp) => op.active.vfcu,
      (lop: VFCULaneOp) => { lop.fn := seq_exp.fn.vfcu(); mark_lop_sreg(lop.sreg, 2) })
    def mark_vfvu = mark_vfu("vfvu", tick_vfvu, (op: SeqOp) => op.active.vfvu,
      (lop: VFVULaneOp) => { lop.fn := seq_exp.fn.vfvu(); mark_lop_sreg(lop.sreg, 1) })

    def mark_vipu = {
      (Seq(preg_vs1, preg_vs2, preg_vs3) zipWithIndex) foreach { case (fn, idx) =>
        check_assert("pred read" + idx, tick_pred_read(idx), UInt(0))
        assert(fn(seq_vipu.reg).valid, "pred op with no predicate")
        val e = tick_pred_read(idx).s(0)
        e.valid := Bool(true)
        e.bits.addr := fn(seq_vipu.reg).id
        e.bits.strip := seq_vipu.strip
        e.bits.pack := seq_vipu.pack
      }

      check_assert("vipu", tick_vipu, UInt(1))
      tick_vipu.s(1).valid := Bool(true)
      tick_vipu.s(1).bits.fn := seq_vipu.fn.vipu()
      tick_vipu.s(1).bits.strip := seq_vipu.strip

      val wport = 1 + stagesPLU
      check_assert("pred write", tick_pred_write, UInt(wport))
      assert(seq_vipu.reg.vd.valid, "pred op with no predicate")
      tick_pred_write.s(wport).valid := Bool(true)
      tick_pred_write.s(wport).bits.id := seq_vipu.base.vd.id
      tick_pred_write.s(wport).bits.addr := seq_vipu.reg.vd.id
      tick_pred_write.s(wport).bits.selg := Bool(false)
      tick_pred_write.s(wport).bits.plu := Bool(true)
      tick_pred_write.s(wport).bits.sidx := seq_vipu.sidx
      tick_pred_write.s(wport).bits.rate := seq_vipu.rate
      tick_pred_write.s(wport).bits.strip := seq_vipu.strip
      tick_pred_write.s(wport).bits.pack := seq_vipu.pack
    }

    def mark_vpu = {
      check_assert("pred pread", tick_pred_pread, UInt(0))
      assert(seq_vpu.reg.vp.valid && seq_vpu.reg.vp.is_pred(), "pread with no guard predicate")
      tick_pred_pread.s(0).valid := Bool(true)
      tick_pred_pread.s(0).bits.off := io.cfg.unpred
      tick_pred_pread.s(0).bits.neg := seq_vpu.reg.vp.neg()
      tick_pred_pread.s(0).bits.addr := seq_vpu.reg.vp.id
      tick_pred_pread.s(0).bits.strip := seq_vpu.strip
      tick_pred_pread.s(0).bits.pack := seq_vpu.pack

      check_assert("vpu", tick_vpu, UInt(1))
      tick_vpu.s(1).valid := Bool(true)
      tick_vpu.s(1).bits.strip := seq_vpu.strip
    }
  }

  when (io.seq.exp.valid) {
    exp.mark_sram_reads
    exp.mark_pred_read
    exp.mark_sram_pred_write
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

  when (io.seq.vipu.valid) {
    exp.mark_vipu
  }

  when (io.seq.vpu.valid) {
    exp.mark_vpu
  }

  io.lane.sram.read <> exp.tick_sram_read.ondeck
  io.lane.sram.write <> exp.tick_sram_write.ondeck
  io.lane.pred.gread <> exp.tick_pred_gread.ondeck
  io.lane.pred.pread <> exp.tick_pred_pread.ondeck
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

  val s1_pred_read = Pipe(exp.tick_pred_gread.ondeck.valid, exp.tick_pred_gread.ondeck.bits)
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

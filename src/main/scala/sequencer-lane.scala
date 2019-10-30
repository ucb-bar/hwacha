package hwacha

import Chisel._
import freechips.rocketchip.config._
import DataGating._

class SequencerIO(implicit p: Parameters) extends VXUBundle()(p) {
  val exp = Valid(new SeqOp)
  val vipu = Valid(new SeqVIPUOp)
  val vpu = Valid(new SeqVPUOp)
}

class LaneSequencer(implicit p: Parameters) extends VXUModule()(p)
  with SeqLogic with BankLogic with PrecLogic {
  val io = new Bundle {
    val lid = UInt(INPUT)
    val cfg = new HwachaConfigIO().flip
    val op = Valid(new IssueOp).flip
    val master = new MasterSequencerIO().flip
    val mocheck = Vec(nSeq, new MOCheck).asInput
    val seq = new SequencerIO
    val vmu = new VMUIO
    val ticker = new TickerIO().flip

    val dpla = new CounterLookAheadIO
    val dqla = Vec(nVDUOperands, new CounterLookAheadIO)
    val dila = new CounterLookAheadIO
    val dfla = new CounterLookAheadIO
    val gpla = new CounterLookAheadIO
    val gqla = new CounterLookAheadIO
    val pla = new BPQLookAheadIO
    val lla = new CounterLookAheadIO
    val sla = new BRQLookAheadIO
    val lreq = new CounterLookAheadIO
    val sreq = new CounterLookAheadIO
    val areq = new MRTAddrIO

    val lpred = Decoupled(Bits(width=nStrip))
    val spred = Decoupled(Bits(width=nStrip))

    val lack = new LaneAckIO().flip
    val dack = new DCCAckIO().flip

    val debug = new Bundle {
      val valid = Vec(nSeq, Bool(OUTPUT))
      val e = Vec(nSeq, new SeqEntry).asOutput
      val dhazard_raw_vlen = Vec(nSeq, Bool(OUTPUT))
      val dhazard_raw_pred_vp = Vec(nSeq, Bool(OUTPUT))
      val dhazard_raw_pred_vs1 = Vec(nSeq, Bool(OUTPUT))
      val dhazard_raw_pred_vs2 = Vec(nSeq, Bool(OUTPUT))
      val dhazard_raw_pred_vs3 = Vec(nSeq, Bool(OUTPUT))
      val dhazard_raw_vs1 = Vec(nSeq, Bool(OUTPUT))
      val dhazard_raw_vs2 = Vec(nSeq, Bool(OUTPUT))
      val dhazard_raw_vs3 = Vec(nSeq, Bool(OUTPUT))
      val dhazard_war = Vec(nSeq, Bool(OUTPUT))
      val dhazard_waw = Vec(nSeq, Bool(OUTPUT))
      val dhazard = Vec(nSeq, Bool(OUTPUT))
      val bhazard = Vec(nSeq, Bool(OUTPUT))
      val shazard = Vec(nSeq, Bool(OUTPUT))
      val use_mask_sreg_global = Vec(nGOPL, Bits(OUTPUT, maxSRegGlobalTicks+nBanks-1))
      val use_mask_xbar = Vec(nGOPL, Bits(OUTPUT, maxXbarTicks+nBanks-1))
      val use_mask_vimu = Bits(OUTPUT, maxVIMUTicks+nBanks-1)
      val use_mask_vfmu = Vec(nVFMU, Bits(OUTPUT, maxVFMUTicks+nBanks-1))
      val use_mask_vfcu = Bits(OUTPUT, maxVFCUTicks+nBanks-1)
      val use_mask_vfvu = Bits(OUTPUT, maxVFVUTicks+nBanks-1)
      val use_mask_vgu = Bits(OUTPUT, maxVGUTicks+nBanks-1)
      val use_mask_vqu = Bits(OUTPUT, maxVQUTicks+nBanks-1)
      val use_mask_wport_sram = Vec(nWSel, Bits(OUTPUT, maxWPortLatency+nBanks-1))
      val use_mask_wport_pred = Bits(OUTPUT, maxPredWPortLatency+nBanks-1)
      val pred_first = Vec(nSeq, Bool(OUTPUT))
      val consider = Vec(nSeq, Bool(OUTPUT))
      val first_sched = Vec(nSeq, Bool(OUTPUT))
      val second_sched = Vec(nSeq, Bool(OUTPUT))
    }
  }
  chisel3.dontTouch(io.debug)

  val mv = io.master.state.valid
  val me = io.master.state.e
  val head = io.master.state.head

  val v = Reg(init = Vec.fill(nSeq){Bool(false)})
  val e = Reg(Vec(nSeq, new SeqEntry))

  val me_rate = Vec(me.map(UInt(1) << _.rate))

  val e_sidx_next = Vec((0 until nSeq) map { r => e(r).sidx + me_rate(r) })
  val e_strip = Vec((0 until nSeq) map { r =>
    val vl = e(r).vlen
    val strip_max = if (confprec) Cat(me_rate(r), UInt(0, bStrip)) else UInt(nStrip)
    Mux(vl > strip_max, strip_max, vl(bStrip+bPack, 0))
  })
  def stripfn(sched: Vec[Bool]) = Mux1H(sched, e_strip)


  ///////////////////////////////////////////////////////////////////////////
  // data hazard checking helpers

  val dhazard = new {
    val vlen_check_ok =
      Vec((0 until nSeq).map { r =>
        Vec((0 until nSeq).map { c =>
          if (r != c) e_sidx_next(r) <= e(c).sidx
          else Bool(true) }) })

    def scmp_mat[T <: RFWriteOp with Rate](ticker: Vec[ValidIO[T]]) = {
      def cmp(x: UInt, y: UInt) = { val z = (x <= y); (z, !z || (x === y)) }
      val t_range = ticker.map(t =>
        (t.bits.sidx, t.bits.sidx + (UInt(1) << t.bits.rate)))
      Vec((0 until nSeq).map { r =>
        val s = (e(r).sidx, e_sidx_next(r))
        Vec(t_range.map { t =>
          val (s1_lte_t1, t1_lte_s1) = cmp(s._1, t._1)
          val (s2_lte_t2, t2_lte_s2) = cmp(s._2, t._2)
          (s1_lte_t1 && t2_lte_s2) || (t1_lte_s1 && s2_lte_t2)
        }) })
    }

    val wsram_mat_scmp = scmp_mat(io.ticker.sram.write)
    def wsram_mat(fn: RegFn, pfn: PRegIdFn) =
      (0 until nSeq) map { r =>
        Vec((0 until maxWPortLatency) map { l =>
          val t = io.ticker.sram.write(l)
          t.valid && fn(me(r).base).valid && fn(me(r).base).is_vector() && (
            if (confprec) (t.bits.id === fn(me(r).base).id) && wsram_mat_scmp(r)(l)
            else t.bits.addr === pfn(e(r).reg).id )
          }) }
    val wsram_mat_vs1 = wsram_mat(reg_vs1, pregid_vs1) map { m => Vec(m.slice(expLatency, maxWPortLatency)) }
    val wsram_mat_vs2 = wsram_mat(reg_vs2, pregid_vs2) map { m => Vec(m.slice(expLatency, maxWPortLatency)) }
    val wsram_mat_vs3 = wsram_mat(reg_vs3, pregid_vs3) map { m => Vec(m.slice(expLatency, maxWPortLatency)) }
    val wsram_mat_vd = wsram_mat(reg_vd, pregid_vd)

    val wpred_mat_scmp = scmp_mat(io.ticker.pred.write)
    def wpred_mat(fn: RegFn, pfn: PRegIdFn) =
      (0 until nSeq) map { r =>
        Vec((0 until maxPredWPortLatency) map { l =>
          val t = io.ticker.pred.write(l)
          t.valid && fn(me(r).base).valid && fn(me(r).base).is_pred() && (
            if (confprec) (t.bits.id === fn(me(r).base).id) && wpred_mat_scmp(r)(l)
            else t.bits.addr === pfn(e(r).reg).id )
          }) }
    val wpred_mat_vp = wpred_mat(reg_vp, pregid_vp) map { m => Vec(m.slice(expLatency, maxPredWPortLatency)) }
    val wpred_mat_vs1 = wpred_mat(reg_vs1, pregid_vs1) map { m => Vec(m.slice(expLatency, maxPredWPortLatency)) }
    val wpred_mat_vs2 = wpred_mat(reg_vs2, pregid_vs2) map { m => Vec(m.slice(expLatency, maxPredWPortLatency)) }
    val wpred_mat_vs3 = wpred_mat(reg_vs3, pregid_vs3) map { m => Vec(m.slice(expLatency, maxPredWPortLatency)) }
    val wpred_mat_vd = wpred_mat(reg_vd, pregid_vd)

    def wport_lookup(row: Vec[Bool], level: UInt) =
      Vec((row zipWithIndex) map { case (r, i) => r && UInt(i) > level })

    val raw =
      (0 until nSeq).map { r =>
        (me(r).raw.asUInt & ~vlen_check_ok(r).asUInt).orR ||
        wpred_mat_vp(r).asUInt.orR ||
        wpred_mat_vs1(r).asUInt.orR || wsram_mat_vs1(r).asUInt.orR ||
        wpred_mat_vs2(r).asUInt.orR || wsram_mat_vs2(r).asUInt.orR ||
        wpred_mat_vs3(r).asUInt.orR || wsram_mat_vs3(r).asUInt.orR }
    val war =
      (0 until nSeq).map { r =>
        (me(r).war.asUInt & ~vlen_check_ok(r).asUInt).orR }
    val waw =
      (0 until nSeq).map { r =>
        (me(r).waw.asUInt & ~vlen_check_ok(r).asUInt).orR ||
        wport_lookup(wpred_mat_vd(r), me(r).wport.pred).asUInt.orR ||
        wport_lookup(wsram_mat_vd(r), me(r).wport.sram).asUInt.orR }

    val check =
      (0 until nSeq).map { r =>
        raw(r) || war(r) || waw(r) }

    def debug = {
      io.debug.dhazard_raw_vlen := Vec((0 until nSeq) map { r => (me(r).raw.asUInt & ~vlen_check_ok(r).asUInt).orR })
      io.debug.dhazard_raw_pred_vp := Vec((0 until nSeq) map { r => wpred_mat_vp(r).asUInt.orR })
      io.debug.dhazard_raw_pred_vs1 := Vec((0 until nSeq) map { r => wpred_mat_vs1(r).asUInt.orR })
      io.debug.dhazard_raw_pred_vs2 := Vec((0 until nSeq) map { r => wpred_mat_vs2(r).asUInt.orR })
      io.debug.dhazard_raw_pred_vs3 := Vec((0 until nSeq) map { r => wpred_mat_vs3(r).asUInt.orR })
      io.debug.dhazard_raw_vs1 := Vec((0 until nSeq) map { r => wsram_mat_vs1(r).asUInt.orR })
      io.debug.dhazard_raw_vs2 := Vec((0 until nSeq) map { r => wsram_mat_vs2(r).asUInt.orR })
      io.debug.dhazard_raw_vs3 := Vec((0 until nSeq) map { r => wsram_mat_vs3(r).asUInt.orR })
      io.debug.dhazard_war := war
      io.debug.dhazard_waw := waw
      io.debug.dhazard := check
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // bank hazard checking helpers

  val bhazard = new {
    // tail (shift right by one) because we are looking one cycle in the future
    val rport_mask = Vec(io.ticker.sram.read.tail map { _.valid })
    val wport_sram_mask = Vec(io.ticker.sram.write.tail map { _.valid })
    val wport_pred_mask = Vec(io.ticker.pred.write.tail map { _.valid })

    val check =
      (0 until nSeq) map { r =>
        me(r).rports.orR && rport_mask.reduce(_ | _) ||
        me(r).wport.sram.orR && wport_sram_mask(me(r).wport.sram) ||
        me(r).wport.pred.orR && wport_pred_mask(me(r).wport.pred)
      }

    def debug = {
      io.debug.bhazard := check
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // structural hazard checking helpers

  val shazard = new {
    def use_mask_lop[T <: LaneOp](lops: Vec[ValidIO[T]], fn: ValidIO[T]=>Bool) = {
      val mask =
        (lops zipWithIndex) map { case (lop, i) =>
          dgate(fn(lop), Wire(UInt(width = lops.size+nBanks-1), init = strip_to_bmask(lop.bits.strip) << UInt(i)))
        } reduce(_|_)
      mask >> UInt(1) // shift right by one because we are looking one cycle in the future
    }
    def use_mask_lop_valid[T <: LaneOp](lops: Vec[ValidIO[T]]) =
      use_mask_lop(lops, (lop: ValidIO[T]) => lop.valid)
    val use_mask_sreg_global = io.ticker.sreg.global map { use_mask_lop_valid(_) }
    val use_mask_xbar = io.ticker.xbar map { use_mask_lop_valid(_) }
    val use_mask_pxbar = io.ticker.pxbar map { use_mask_lop_valid(_) }
    val use_mask_vimu = use_mask_lop_valid(io.ticker.vimu)
    val use_mask_vfmu = io.ticker.vfmu map { use_mask_lop_valid(_) }
    val use_mask_vfcu = use_mask_lop_valid(io.ticker.vfcu)
    val use_mask_vfvu = use_mask_lop_valid(io.ticker.vfvu)
    val use_mask_vgu = use_mask_lop_valid(io.ticker.vgu)
    val use_mask_vqu = use_mask_lop_valid(io.ticker.vqu)
    val use_mask_wport_sram = (0 until nWSel) map { i =>
      use_mask_lop(
        io.ticker.sram.write,
        (lop: ValidIO[SRAMRFWriteOp]) => lop.valid && lop.bits.selg && lop.bits.wsel === UInt(i)) }
    val use_mask_wport_pred =
      use_mask_lop(
        io.ticker.pred.write,
        (lop: ValidIO[PredRFWriteOp]) => lop.valid && lop.bits.selg)

    val select = Wire(Vec(nSeq, new SeqSelect))

    val check =
      (0 until nSeq) map { r =>
        val op_idx = me(r).rports + UInt(expLatency, bRPorts+1)
        val strip = e_strip(r)
        val ask_op_mask = Wire(UInt(width = maxXbarTicks+nBanks-1), init = strip_to_bmask(strip) << op_idx)
        val ask_wport_sram_mask = Wire(UInt(width = maxWPortLatency+nBanks-1), init = strip_to_bmask(strip) << me(r).wport.sram)
        val ask_wport_pred_mask = Wire(UInt(width = maxPredWPortLatency+nBanks-1), init = strip_to_bmask(strip) << me(r).wport.pred)
        def chk_shazard(use_mask: Bits, ask_mask: Bits) = (use_mask.asUInt & ask_mask.asUInt).orR
        def chk_op_shazard(use_mask: Bits) = chk_shazard(use_mask, ask_op_mask)
        def chk_gpred(i: Int) = chk_op_shazard(use_mask_pxbar(i))
        def chk_rport(fn: RegFn, i: Int) =
          fn(me(r).base).valid && (
            fn(me(r).base).is_vector() && chk_op_shazard(use_mask_xbar(i)) ||
            fn(me(r).base).is_scalar() && chk_op_shazard(use_mask_sreg_global(i)))
        val chk_rport_0_1 = chk_gpred(0) || chk_rport(reg_vs1, 0) || chk_rport(reg_vs2, 1)
        val chk_rport_0_1_2 = chk_rport_0_1 || chk_rport(reg_vs3, 2)
        val chk_rport_2 = chk_gpred(1) || chk_rport(reg_vs1, 2)
        val chk_rport_3_4 = chk_gpred(2) || chk_rport(reg_vs1, 3) || chk_rport(reg_vs2, 4)
        val chk_rport_3_4_5 = chk_rport_3_4 || chk_rport(reg_vs3, 5)
        val chk_rport_5 = chk_gpred(3) || chk_rport(reg_vs1, 5)
        def chk_wport_sram(i: Int) =
          me(r).base.vd.valid && chk_shazard(use_mask_wport_sram(i), ask_wport_sram_mask)
        val chk_wport_sram_0 = chk_wport_sram(0)
        val chk_wport_sram_1 = chk_wport_sram(1)
        val chk_wport_pred =
          me(r).base.vd.valid && chk_shazard(use_mask_wport_pred, ask_wport_pred_mask)
        val shazard_vimu = chk_rport_0_1 || chk_wport_sram_0 || chk_op_shazard(use_mask_vimu)
        val shazard_vfmu0 = chk_rport_0_1_2 || chk_wport_sram_0 || chk_op_shazard(use_mask_vfmu(0))
        val shazard_vfmu1 = chk_rport_3_4_5 || chk_wport_sram_1 || chk_op_shazard(use_mask_vfmu(1))
        val shazard_vfcu = chk_rport_3_4 || chk_wport_sram_1 || chk_wport_pred || chk_op_shazard(use_mask_vfcu)
        val shazard_vfvu = chk_rport_2 || chk_wport_sram_0 || chk_op_shazard(use_mask_vfvu)
        val shazard_vgu = chk_rport_5 || chk_wport_sram_1 || chk_op_shazard(use_mask_vgu)
        val shazard_vqu = chk_rport_3_4 || chk_wport_sram_1 || chk_op_shazard(use_mask_vqu)
        select(r).vfmu := Mux(shazard_vfmu0, UInt(1), UInt(0))
        val a = me(r).active
        val out =
          a.vimu && shazard_vimu ||
          a.vfmu && shazard_vfmu0 && shazard_vfmu1 || a.vfcu && shazard_vfcu || a.vfvu && shazard_vfvu ||
          a.vgu && shazard_vgu || a.vqu && shazard_vqu
        out
      }

    def debug = {
      io.debug.shazard := check
      io.debug.use_mask_sreg_global := Vec((0 until nGOPL) map { i => use_mask_sreg_global(i) })
      io.debug.use_mask_xbar := Vec((0 until nGOPL) map { i => use_mask_xbar(i) })
      io.debug.use_mask_vimu := use_mask_vimu
      io.debug.use_mask_vfmu := Vec((0 until nVFMU) map { i => use_mask_vfmu(i) })
      io.debug.use_mask_vfcu := use_mask_vfcu
      io.debug.use_mask_vfvu := use_mask_vfvu
      io.debug.use_mask_vgu := use_mask_vgu
      io.debug.use_mask_vqu := use_mask_vqu
      io.debug.use_mask_wport_sram := use_mask_wport_sram
      io.debug.use_mask_wport_pred := use_mask_wport_pred
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // issue window helpers

  val iwindow = new {
    val next_update = Wire(Vec(nSeq, Bool()))
    val next_v = Wire(Vec(nSeq, Bool()))

    val set = new {
      def valid(n: UInt) = {
        next_update(n) := Bool(true)
        next_v(n) := Bool(true)
      }
    }

    val clear = new {
      def valid(n: UInt) = {
        next_update(n) := Bool(true)
        next_v(n) := Bool(false)
      }
    }

    def header = {
      (0 until nSeq) map { r =>
        next_update(r) := Bool(false)
        next_v(r) := v(r)
      }
    }

    def logic = {
      (0 until nSeq) map { r =>
        when (next_update(r)) {
          v(r) := next_v(r)
        }
        when (io.op.valid && io.master.update.valid(r)) {
          set.valid(UInt(r))
          e(r).reg := io.master.update.reg(r)
          e(r).vlen := io.op.bits.vlen
          e(r).eidx.major := io.lid << io.cfg.lstride
          e(r).eidx.minor := UInt(0)
          e(r).sidx := UInt(0)
          e(r).age := UInt(0)
          e(r).pack.idx := UInt(0)
        }
        io.master.clear(r) := !v(r) || !next_v(r)
      }
    }

    def debug = {
      io.debug.valid := v
      io.debug.e := e
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // scheduling helpers

  val scheduling = new {
    val ffv = Vec((mv zip v) map { case (mvalid, valid) => mvalid && valid })
    def ff(fn: Int=>Bool) = find_first(ffv, head, fn)

    def mread[T <: Data](sched: Vec[Bool], rfn: MasterSeqEntry=>T) = mreadfn(sched, me, rfn)
    def read[T <: Data](sched: Vec[Bool], rfn: SeqEntry=>T) = readfn(sched, e, rfn)

    def selectfn(sched: Vec[Bool]) =
      new SeqSelect().fromBits(Mux1H(sched, shazard.select.map(_.asUInt)))

    def regfn(sched: Vec[Bool]) = {
      val out = Wire(new PhysicalRegisters)
      List((preg_vp, reg_vp, pregid_vp),
           (preg_vs1, reg_vs1, pregid_vs1),
           (preg_vs2, reg_vs2, pregid_vs2),
           (preg_vs3, reg_vs3, pregid_vs3),
           (preg_vd, reg_vd, pregid_vd)) map {
        case (pfn, rfn, pidfn) =>
          pfn(out) := mread(sched, (me: MasterSeqEntry) => rfn(me.base))
          pfn(out).id := read(sched, (e: SeqEntry) => pidfn(e.reg).id)
      }
      out
    }

    val nohazards = (0 until nSeq) map { i =>
      !dhazard.check(i) && !bhazard.check(i) && !shazard.check(i) }

    // independent ports that don't go to the expander
    class vdu(afn: SeqType=>Bool, la: CounterLookAheadIO) {
      val first = ff((i: Int) => afn(me(i).active))
      val strip = stripfn(first)

      val valids = Vec((first zipWithIndex) map { case (f, i) => f && nohazards(i) })
      val ready = la.available
      def fires(n: Int) = valids(n) && ready
      val fire = valids.reduce(_ || _) && ready

      def logic = {
        la.cnt := strip_to_bcnt(strip)
        la.reserve := fire
      }
    }

    val vidu = new vdu((a: SeqType) => a.vidu, io.dila)
    val vfdu = new vdu((a: SeqType) => a.vfdu, io.dfla)

    val vcu = new {
      val first = ff((i: Int) => me(i).active.vcu)
      val strip = stripfn(first)
      val fn = mread(first, (me: MasterSeqEntry) => me.fn)
      val mcmd = DecodedMemCommand(fn.vmu().cmd)

      val valids = Vec((first zipWithIndex) map { case (f, i) => f && nohazards(i) })
      val readys = Vec((0 until nSeq) map { case i =>
        io.vmu.pala.available &&
        (!mcmd.read || io.mocheck(i).load && io.lreq.available) &&
        (!mcmd.store || io.mocheck(i).store && io.sreq.available) })
      def fires(n: Int) = valids(n) && readys(n)
      val fire = (valids zip readys) map { case (v, r) => v && r } reduce(_ || _)

      def logic = {
        io.vmu.pala.cnt := strip
        io.vmu.pala.reserve := fire
        io.lreq.cnt := strip
        io.lreq.reserve := fire && mcmd.read
        io.sreq.cnt := strip
        io.sreq.reserve := fire && mcmd.store
        io.areq.valid := fire && (read(first, (e: SeqEntry) => e.sidx) === 0.U)
        /* next seq entry after vcu contains corresponding vlu/vsu */
        io.areq.bits := Mux1H(first, Seq.tabulate(nSeq)(i => UInt((i + 1) % nSeq)))
        assert(!io.areq.valid || me(io.areq.bits).active.vlu || me(io.areq.bits).active.vsu,
          "missing vlu/vsu after vcu")
      }
    }

    val vlu = new {
      val first = ff((i: Int) => me(i).active.vlu)
      val strip = stripfn(first)

      val valids = Vec((first zipWithIndex) map { case (f, i) => f && nohazards(i) })
      val ready = io.lla.available
      def fires(n: Int) = valids(n) && ready
      val fire = valids.reduce(_ || _) && ready

      def logic = {
        io.lla.cnt := strip
        io.lla.reserve := fire
      }
    }

    // scheduled ports that go to the expander
    val exp = new {
      val vgu_consider = Wire(Vec(nSeq, Bool()))
      val vsu_consider = Wire(Vec(nSeq, Bool()))
      val vqu_consider = Wire(Vec(nSeq, Bool()))

      val consider = (i: Int) => nohazards(i) && (
        me(i).active.viu || me(i).active.vimu ||
        me(i).active.vfmu || me(i).active.vfcu || me(i).active.vfvu ||
        me(i).active.vgu && vgu_consider(i) ||
        me(i).active.vsu && vsu_consider(i) ||
        me(i).active.vqu && vqu_consider(i))
      val first_sched = ff((i: Int) => consider(i) && e(i).age === UInt(0))
      val second_sched = ff((i: Int) => consider(i))
      val sel = first_sched.reduce(_ || _)
      val sched = Vec(first_sched zip second_sched map { case (f, s) => Mux(sel, f, s) })
      val op = {
        val out = Wire(new SeqOp)
        out.fn := mread(sched, (me: MasterSeqEntry) => me.fn)
        out.reg := regfn(sched)
        out.base.vd := mread(sched, (me: MasterSeqEntry) => me.base.vd)
        out.sreg := mread(sched, (me: MasterSeqEntry) => me.sreg)
        out.active := mread(sched, (me: MasterSeqEntry) => me.active)
        out.select := selectfn(sched)
        out.eidx := read(sched, (e: SeqEntry) => e.eidx.major) +
          read(sched, (e: SeqEntry) => e.eidx.minor)
        out.sidx := read(sched, (e: SeqEntry) => e.sidx)
        out.rports := mread(sched, (me: MasterSeqEntry) => me.rports)
        out.wport.sram := mread(sched, (me: MasterSeqEntry) => me.wport.sram)
        out.wport.pred := mread(sched, (me: MasterSeqEntry) => me.wport.pred)
        out.strip := stripfn(sched)
        out.rate := mread(sched, (me: MasterSeqEntry) => me.rate)
        out.pack := read(sched, (e: SeqEntry) => e.pack)
        out
      }

      def fires(n: Int) = sched(n)
      val fire = sched.reduce(_ || _)
      val fire_vgu = fire && op.active.vgu
      val fire_vsu = fire && op.active.vsu
      val fire_vqu = fire && op.active.vqu
      val fire_vqu_latch = (0 until nVDUOperands) map { fire_vqu && op.fn.vqu().latch(_) }

      def logic = {
        io.seq.exp.valid := fire
        io.seq.exp.bits := op
      }

      def debug = {
        (io.debug.consider zipWithIndex) foreach { case (io, i) => io := consider(i) }
        (io.debug.first_sched zip first_sched) foreach { case (io, c) => io := c }
        (io.debug.second_sched zip second_sched) foreach { case (io, c) => io := c }
      }
    }

    val vipu = new {
      val consider = (i: Int) => me(i).active.vipu && nohazards(i)
      val first_sched = ff((i: Int) => consider(i) && e(i).age === UInt(0))
      val second_sched = ff((i: Int) => consider(i))
      val sel = first_sched.reduce(_ || _)
      val sched = Vec(first_sched zip second_sched map { case (f, s) => Mux(sel, f, s) })
      val op = {
        val out = Wire(new SeqVIPUOp)
        out.fn := mread(sched, (me: MasterSeqEntry) => me.fn)
        out.reg := regfn(sched)
        out.base.vd := mread(sched, (me: MasterSeqEntry) => me.base.vd)
        out.sidx := read(sched, (e: SeqEntry) => e.sidx)
        out.strip := stripfn(sched)
        out.rate := mread(sched, (me: MasterSeqEntry) => me.rate)
        out.pack := read(sched, (e: SeqEntry) => e.pack)
        out
      }

      def fires(n: Int) = sched(n)
      val fire = sched.reduce(_ || _)

      def logic = {
        io.seq.vipu.valid := fire
        io.seq.vipu.bits := op
      }
    }

    val vpu = new {
      val first = ff((i: Int) => me(i).active.vpu || me(i).active.vgu)
      val sel = mread(first, (me: MasterSeqEntry) => me.active.vgu)
      val op = {
        val out = Wire(new SeqVPUOp)
        out.reg := regfn(first)
        out.strip := stripfn(first)
        out.pack := read(first, (e: SeqEntry) => e.pack)
        out
      }

      val valids = Vec((first zipWithIndex) map { case (f, i) => f && nohazards(i) })
      val ready = io.pla.available && (!sel || exp.fire_vgu)
      def fires(n: Int) = valids(n) && ready
      val fire = valids.reduce(_ || _) && ready

      def logic = {
        io.seq.vpu.valid := fire
        io.seq.vpu.bits := op
        io.pla.reserve := fire
        io.pla.mask := strip_to_bmask(op.strip)
      }

      def debug = {
        (io.debug.pred_first zip valids) foreach { case (io, c) => io := c }
      }
    }

    // helpers for the main scheduled port
    val vgu = new {
      val first = ff((i: Int) => me(i).active.vgu)
      val strip = stripfn(first)
      val cnt = strip_to_bcnt(strip)

      val ready = io.pla.available && io.gpla.available && io.gqla.available
      (0 until nSeq) map { i => exp.vgu_consider(i) := vpu.first(i) && first(i) && ready }

      def logic = {
        io.gpla.cnt := cnt
        io.gpla.reserve := exp.fire_vgu
        io.gqla.cnt := cnt
        io.gqla.reserve := exp.fire_vgu
      }
    }

    val vsu = new {
      val first = ff((i: Int) => me(i).active.vsu)
      val strip = stripfn(first)

      val ready = io.sla.available
      (0 until nSeq) map { i => exp.vsu_consider(i) := first(i) && ready }

      def logic = {
        io.sla.reserve := exp.fire_vsu
        io.sla.mask := strip_to_bmask(strip)
      }
    }

    val vqu = new {
      val first = ff((i: Int) => me(i).active.vqu)
      val strip = stripfn(first)
      val cnt = strip_to_bcnt(strip)

      val readys = Vec((0 until nSeq) map { case i =>
        io.dpla.available &&
        (!me(i).fn.vqu().latch(0) || io.dqla(0).available) &&
        (!me(i).fn.vqu().latch(1) || io.dqla(1).available) })
      (0 until nSeq) map { i => exp.vqu_consider(i) := first(i) && readys(i) }

      def logic = {
        io.dpla.cnt := cnt
        io.dpla.reserve := exp.fire_vqu
        (io.dqla zipWithIndex) map { case (la, i) =>
          la.cnt := cnt
          la.reserve := exp.fire_vqu_latch(i)
        }
      }
    }

    def logic = {
      vidu.logic; vfdu.logic; vcu.logic; vlu.logic
      exp.logic; vipu.logic; vpu.logic
      vgu.logic; vsu.logic; vqu.logic

      def fires(n: Int) =
        vidu.fires(n) || vfdu.fires(n) || vcu.fires(n) || vlu.fires(n) ||
        exp.fires(n) || vipu.fires(n) || vpu.fires(n)

      def update_eidx(i: Int) {
        val strip = io.cfg.lstrip >> UInt(bStrip)
        val minor = Cat(UInt(0), e(i).eidx.minor) + me_rate(i)
        when (minor === strip) {
          e(i).eidx.minor := UInt(0)
          e(i).eidx.major := e(i).eidx.major + (strip << UInt(bLanes))
        } .otherwise {
          e(i).eidx.minor := minor
        }
      }
      def update_sidx(i: Int) {
        e(i).sidx := e_sidx_next(i)
      }

      val e_pack_idx_next = Vec((0 until nSeq).map(i =>
        e(i).pack.idx + me_rate(i)))
      def update_pack(i: Int) {
        if (confprec) e(i).pack.idx := e_pack_idx_next(i)
      }

      def step_pstride(i: Int) =
        if (confprec) (e_pack_idx_next(i)(bPack-1, 0) === UInt(0)) else Bool(true)
      def update_vp(i: Int, fn: RegPFn, pfn: PRegIdFn) {
        val info = fn(me(i).base)
        val id = pfn(e(i).reg).id
        when (info.valid && info.is_pred() && step_pstride(i)) {
          id := id + io.cfg.pstride
        }
      }
      def update_vs(i: Int, fn: RegVFn, pfn: PRegIdFn) {
        val info = fn(me(i).base)
        val id = pfn(e(i).reg).id

        val (step_vstride, vstride) = if (confprec)
            confprec_step(info.prec, e_pack_idx_next(i), io.cfg)
          else (Bool(true), io.cfg.vstride.d)

        when (info.valid) {
          id := id + MuxCase(UInt(0), Seq(
            (info.is_vector() && step_vstride) -> vstride,
            (info.is_pred() && step_pstride(i)) -> io.cfg.pstride))
        }
      }
      def update_vd = update_vs _

      for (i <- 0 until nSeq) {
        val strip = e_strip(i)
        when (mv(i) && v(i)) {
          when (fires(i)) {
            e(i).vlen := e(i).vlen - strip
            update_eidx(i)
            update_sidx(i)
            update_pack(i)
            update_vp(i, reg_vp, pregid_vp)
            update_vs(i, reg_vs1, pregid_vs1)
            update_vs(i, reg_vs2, pregid_vs2)
            update_vs(i, reg_vs3, pregid_vs3)
            update_vd(i, reg_vd, pregid_vd)
            when (e(i).vlen === strip) {
              iwindow.clear.valid(UInt(i))
            }
          }
          when (e(i).age.orR) {
            e(i).age := e(i).age - UInt(1)
          }
          when (exp.fires(i)) {
            e(i).age := UInt(nBanks-1)
          }
        }
      }
    }

    def debug = {
      exp.debug
      vpu.debug
    }
  }

  iwindow.header

  iwindow.logic
  scheduling.logic

  iwindow.debug
  dhazard.debug
  bhazard.debug
  shazard.debug
  scheduling.debug
}

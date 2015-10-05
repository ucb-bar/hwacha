package hwacha

import Chisel._
import DataGating._

abstract trait SeqParameters extends UsesHwachaParameters with LaneParameters {
  val nSeq = 8
  val nRPorts = 3
  val bRPorts = log2Down(nRPorts) + 1
  val expLatency = 1
  val maxWPortLatency = nRPorts + 1 + expLatency +
    List(stagesALU, stagesIMul, stagesFMA,
         stagesFConv, stagesFCmp).reduceLeft((x, y) => if (x > y) x else y)
  val bWPortLatency = log2Down(maxWPortLatency) + 1
  val maxPredWPortLatency = expLatency +
    List(1 + stagesPLU, 3 + stagesFCmp).reduceLeft((x, y) => if (x > y) x else y)
  val maxStrip = nBanks * (wBank / SZ_B)
  val bStrip = log2Down(maxStrip) + 1
  val maxLookAhead = math.max(params(uncore.TLDataBits) / SZ_B, nBatch)
  val bLookAhead = log2Down(maxLookAhead) + 1

  // the following needs to hold in order to simplify dhazard_war checking
  // otherwise, you need to check reg_vd against sram read ticker
  require(nRPorts <= 3)
  require(
    List(stagesALU, stagesIMul, stagesFMA,
         stagesFConv, stagesFCmp).reduceLeft((x, y) => if (x > y) y else x) >= 1)

  type RegFn = DecodedRegisters => RegInfo
  val reg_vs1 = (reg: DecodedRegisters) => reg.vs1
  val reg_vs2 = (reg: DecodedRegisters) => reg.vs2
  val reg_vs3 = (reg: DecodedRegisters) => reg.vs3
  val reg_vd  = (reg: DecodedRegisters) => reg.vd

  type SRegFn = ScalarRegisters => Bits
  val sreg_ss1 = (sreg: ScalarRegisters) => sreg.ss1
  val sreg_ss2 = (sreg: ScalarRegisters) => sreg.ss2
  val sreg_ss3 = (sreg: ScalarRegisters) => sreg.ss3
}

class SequencerIO extends VXUBundle {
  val exp = Valid(new SeqOp)
  val vpu = Valid(new SeqVPUOp)
  val vipu = Valid(new SeqVIPUOp)
}

class Sequencer extends VXUModule with BankLogic {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val op = new VXUIssueOpIO().flip
    val seq = new SequencerIO
    val vmu = new LaneMemIO
    val ticker = new TickerIO().flip

    val dpla = new CounterLookAheadIO
    val dqla = Vec.fill(nVDUOperands){new CounterLookAheadIO}
    val dila = new CounterLookAheadIO
    val dfla = new CounterLookAheadIO
    val gpla = new CounterLookAheadIO
    val gqla = new CounterLookAheadIO
    val pla = new BPQLookAheadIO
    val lla = new CounterLookAheadIO
    val sla = new BRQLookAheadIO
    val lreq = new CounterLookAheadIO
    val sreq = new CounterLookAheadIO

    val lpred = Decoupled(Bits(width=nPredSet))
    val spred = Decoupled(Bits(width=nPredSet))

    val lack = new LaneAckIO().flip
    val dack = new DCCAckIO().flip

    val pending = Bool(OUTPUT)
    val debug = new Bundle {
      val valid = Vec.fill(nSeq){Bool(OUTPUT)}
      val e = Vec.fill(nSeq){new SeqEntry}.asOutput
      val head = UInt(OUTPUT, log2Up(nSeq))
      val tail = UInt(OUTPUT, log2Up(nSeq))
      val full = Bool(OUTPUT)
      val dhazard_raw_vlen = Vec.fill(nSeq){Bool(OUTPUT)}
      val dhazard_raw_vs1 = Vec.fill(nSeq){Bool(OUTPUT)}
      val dhazard_raw_vs2 = Vec.fill(nSeq){Bool(OUTPUT)}
      val dhazard_raw_vs3 = Vec.fill(nSeq){Bool(OUTPUT)}
      val dhazard_war = Vec.fill(nSeq){Bool(OUTPUT)}
      val dhazard_waw = Vec.fill(nSeq){Bool(OUTPUT)}
      val dhazard = Vec.fill(nSeq){Bool(OUTPUT)}
      val bhazard = Vec.fill(nSeq){Bool(OUTPUT)}
      val shazard = Vec.fill(nSeq){Bool(OUTPUT)}
      val use_mask_sreg_global = Vec.fill(nGOPL){Bits(OUTPUT, maxSRegGlobalTicks+nBanks-1)}
      val use_mask_xbar = Vec.fill(nGOPL){Bits(OUTPUT, maxXbarTicks+nBanks-1)}
      val use_mask_vimu = Bits(OUTPUT, maxVIMUTicks+nBanks-1)
      val use_mask_vfmu = Vec.fill(nVFMU){Bits(OUTPUT, maxVFMUTicks+nBanks-1)}
      val use_mask_vfcu = Bits(OUTPUT, maxVFCUTicks+nBanks-1)
      val use_mask_vfvu = Bits(OUTPUT, maxVFVUTicks+nBanks-1)
      val use_mask_vgu = Bits(OUTPUT, maxVGUTicks+nBanks-1)
      val use_mask_vqu = Bits(OUTPUT, maxVQUTicks+nBanks-1)
      val use_mask_wport = Vec.fill(nWSel){Bits(OUTPUT, maxWPortLatency+nBanks-1)}
      val pred_first = Vec.fill(nSeq){Bool(OUTPUT)}
      val consider = Vec.fill(nSeq){Bool(OUTPUT)}
      val first_sched = Vec.fill(nSeq){Bool(OUTPUT)}
      val second_sched = Vec.fill(nSeq){Bool(OUTPUT)}
    }
  }

  require(isPow2(nSeq))

  val full = Reg(init = Bool(false))
  val head = Reg(init = UInt(0, log2Up(nSeq)))
  val tail = Reg(init = UInt(0, log2Up(nSeq)))

  val v = Vec.fill(nSeq){Reg(init=Bool(false))}
  val e = Vec.fill(nSeq){Reg(new SeqEntry)}

  def stripfn(vl: UInt, vcu: Bool, fn: VFn) = {
    val max_strip = Mux(vcu, UInt(nBatch << 1), UInt(nBatch))
    Mux(vl > max_strip, max_strip, vl)
  }

  ///////////////////////////////////////////////////////////////////////////
  // data hazard checking helpers

  val dhazard = new {
    val next_update = Vec.fill(nSeq){Bool()}
    val next_raw = Vec.fill(nSeq){Vec.fill(nSeq){Bool()}}
    val next_war = Vec.fill(nSeq){Vec.fill(nSeq){Bool()}}
    val next_waw = Vec.fill(nSeq){Vec.fill(nSeq){Bool()}}

    val set = new {
      def raw(n: UInt, o: UInt) = { next_raw(n)(o) := Bool(true) }
      def war(n: UInt, o: UInt) = { next_war(n)(o) := Bool(true) }
      def waw(n: UInt, o: UInt) = { next_waw(n)(o) := Bool(true) }

      def issue_base_eq(ifn: RegFn, sfn: RegFn) =
        Vec((0 until nSeq) map { i => v(i) &&
          sfn(e(i).base).valid && ifn(io.op.bits.reg).valid &&
          !sfn(e(i).base).scalar && !ifn(io.op.bits.reg).scalar &&
          !(sfn(e(i).base).pred ^ ifn(io.op.bits.reg).pred) &&
          sfn(e(i).base).id === ifn(io.op.bits.reg).id })
      val ivs1_evd_eq = issue_base_eq(reg_vs1, reg_vd)
      val ivs2_evd_eq = issue_base_eq(reg_vs2, reg_vd)
      val ivs3_evd_eq = issue_base_eq(reg_vs3, reg_vd)
      val ivd_evs1_eq = issue_base_eq(reg_vd, reg_vs1)
      val ivd_evs2_eq = issue_base_eq(reg_vd, reg_vs2)
      val ivd_evs3_eq = issue_base_eq(reg_vd, reg_vs3)
      val ivd_evd_eq  = issue_base_eq(reg_vd, reg_vd)

      val ivp_evd_eq =
        Vec((0 until nSeq) map { i => v(i) &&
          e(i).base.vd.valid && io.op.bits.reg.vp.valid &&
          !e(i).base.vd.scalar && e(i).base.vd.pred &&
          e(i).base.vd.id === io.op.bits.reg.vp.id })
      val ivd_evp_eq =
        Vec((0 until nSeq) map {i => v(i) &&
          e(i).base.vp.valid && io.op.bits.reg.vd.valid &&
          !io.op.bits.reg.vd.scalar && io.op.bits.reg.vd.pred &&
          e(i).base.vp.id === io.op.bits.reg.vd.id })

      def raws(n: UInt, eq: Vec[Bool]) = {
        for (i <- 0 until nSeq) {
          when (eq(i)) {
            raw(n, UInt(i))
          }
        }
      }
      def wars(n: UInt) = {
        for (i <- 0 until nSeq) {
          when (ivd_evp_eq(i) || ivd_evs1_eq(i) || ivd_evs2_eq(i) || ivd_evs3_eq(i)) {
            war(n, UInt(i))
          }
        }
      }
      def waws(n: UInt) = {
        for (i <- 0 until nSeq) {
          when (ivd_evd_eq(i)) {
            waw(n, UInt(i))
          }
        }
      }
      def raw_vp(n: UInt) = raws(n, ivp_evd_eq)
      def raw_vs1(n: UInt) = raws(n, ivs1_evd_eq)
      def raw_vs2(n: UInt) = raws(n, ivs2_evd_eq)
      def raw_vs3(n: UInt) = raws(n, ivs3_evd_eq)
      def raw_vd(n: UInt) = raws(n, ivd_evd_eq)
      def war_vd(n: UInt) = wars(n)
      def waw_vd(n: UInt) = waws(n)
    }

    val clear = new {
      def raw(n: UInt, o: UInt) = { next_raw(n)(o) := Bool(false) }
      def war(n: UInt, o: UInt) = { next_war(n)(o) := Bool(false) }
      def waw(n: UInt, o: UInt) = { next_waw(n)(o) := Bool(false) }
    }

    val check = new {
      val vlen_check_ok =
        Vec((0 until nSeq).map { r =>
          Vec((0 until nSeq).map { c =>
            if (r != c) e(UInt(r)).vlen > e(UInt(c)).vlen
            else Bool(true) }) })

      def wmatrix(fn: RegFn) =
        (0 until nSeq) map { r =>
          Vec((0 until maxWPortLatency) map { l =>
            io.ticker.sram.write(l).valid && fn(e(r).reg).valid && !fn(e(r).reg).scalar &&
            io.ticker.sram.write(l).bits.addr === fn(e(r).reg).id }) }
      val wmatrix_vs1 = wmatrix(reg_vs1) map { m => Vec(m.slice(expLatency, maxWPortLatency)) }
      val wmatrix_vs2 = wmatrix(reg_vs2) map { m => Vec(m.slice(expLatency, maxWPortLatency)) }
      val wmatrix_vs3 = wmatrix(reg_vs3) map { m => Vec(m.slice(expLatency, maxWPortLatency)) }
      val wmatrix_vd = wmatrix(reg_vd)

      def wport_lookup(row: Vec[Bool], level: UInt) =
        Vec((row zipWithIndex) map { case (r, i) => r && UInt(i) > level })

      val raw =
        (0 until nSeq).map { r =>
          (e(r).raw.toBits & ~vlen_check_ok(r).toBits).orR ||
          wmatrix_vs1(r).toBits.orR ||
          wmatrix_vs2(r).toBits.orR ||
          wmatrix_vs3(r).toBits.orR }
      val war =
        (0 until nSeq).map { r =>
          (e(r).war.toBits & ~vlen_check_ok(r).toBits).orR }
      val waw =
        (0 until nSeq).map { r =>
          (e(r).waw.toBits & ~vlen_check_ok(r).toBits).orR ||
          wport_lookup(wmatrix_vd(r), e(r).wport).toBits.orR }

      val result =
        (0 until nSeq).map { r =>
          raw(r) || war(r) || waw(r) }

      def debug = {
        io.debug.dhazard_raw_vlen := Vec((0 until nSeq) map { r => (e(r).raw.toBits & ~vlen_check_ok(r).toBits).orR })
        io.debug.dhazard_raw_vs1 := Vec((0 until nSeq) map { r => wmatrix_vs1(r).toBits.orR })
        io.debug.dhazard_raw_vs2 := Vec((0 until nSeq) map { r => wmatrix_vs2(r).toBits.orR })
        io.debug.dhazard_raw_vs3 := Vec((0 until nSeq) map { r => wmatrix_vs3(r).toBits.orR })
        io.debug.dhazard_war := war
        io.debug.dhazard_waw := waw
        io.debug.dhazard := result
      }
    }

    def update(n: UInt) = next_update(n) := Bool(true)

    def header = {
      for (i <- 0 until nSeq) {
        next_update(i) := Bool(false)
        for (j <- 0 until nSeq) {
          next_raw(i)(j) := e(i).raw(j)
          next_war(i)(j) := e(i).war(j)
          next_waw(i)(j) := e(i).waw(j)
        }
      }
    }

    def logic = {
      for (i <- 0 until nSeq) {
        when (next_update(i)) {
          e(i).raw := next_raw(i)
          e(i).war := next_war(i)
          e(i).waw := next_waw(i)
        }
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // bank hazard checking helpers

  val bhazard = new {
    val set = new {
      def nports_list(fns: RegFn*) = {
        val cnt = PopCount(fns.map{ fn => fn(io.op.bits.reg).valid && !fn(io.op.bits.reg).scalar })
        // make sure # of rports is larger than 1, because we need to read predicate
        Mux(cnt > UInt(0), cnt, UInt(1))
      }
      val nrports = nports_list(reg_vs1, reg_vs2, reg_vs3)
      val nrport_vs1 = nports_list(reg_vs1)
      val nrport_vs2 = nports_list(reg_vs2)
      val nrport_vd = nports_list(reg_vd)

      def ports(n: UInt, rports: UInt, wport: UInt) = {
        e(n).rports := rports
        e(n).wport := wport
      }
      def noports(n: UInt) = ports(n, UInt(0), UInt(0))
      def rport_vs1(n: UInt) = ports(n, nrport_vs1, UInt(0))
      def rport_vs2(n: UInt) = ports(n, nrport_vs2, UInt(0))
      def rport_vd(n: UInt) = ports(n, nrport_vd, UInt(0))
      def rports(n: UInt) = ports(n, nrports, UInt(0))
      def rwports(n: UInt, latency: Int) = ports(n, nrports, nrports + UInt(expLatency+latency))
    }

    val check = new {
      // tail (shift right by one) because we are looking one cycle in the future
      val rport_mask = Vec(io.ticker.sram.read.tail map { _.valid })
      val wport_mask = Vec(io.ticker.sram.write.tail map { _.valid })

      val result =
        (0 until nSeq) map { r =>
          e(r).rports.orR && rport_mask.reduce(_ | _) ||
          e(r).wport.orR && wport_mask(e(r).wport)
        }

      def debug = {
        io.debug.bhazard := result
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // structural hazard checking helpers

  val shazard = new {
    val check = new {
      def use_mask_lop[T <: LaneOp](lops: Vec[ValidIO[T]], fn: ValidIO[T]=>Bool) = {
        val mask =
          (lops zipWithIndex) map { case (lop, i) =>
            dgate(fn(lop), UInt(strip_to_bmask(lop.bits.strip) << UInt(i), lops.size+nBanks-1))
          } reduce(_|_)
        mask >> UInt(1) // shift right by one because we are looking one cycle in the future
      }
      def use_mask_lop_valid[T <: LaneOp](lops: Vec[ValidIO[T]]) =
        use_mask_lop(lops, (lop: ValidIO[T]) => lop.valid)
      val use_mask_sreg_global = io.ticker.sreg.global map { use_mask_lop_valid(_) }
      val use_mask_xbar = io.ticker.xbar map { use_mask_lop_valid(_) }
      val use_mask_vimu = use_mask_lop_valid(io.ticker.vimu)
      val use_mask_vfmu = io.ticker.vfmu map { use_mask_lop_valid(_) }
      val use_mask_vfcu = use_mask_lop_valid(io.ticker.vfcu)
      val use_mask_vfvu = use_mask_lop_valid(io.ticker.vfvu)
      val use_mask_vgu = use_mask_lop_valid(io.ticker.vgu)
      val use_mask_vqu = use_mask_lop_valid(io.ticker.vqu)
      val use_mask_wport = (0 until nWSel) map { i =>
        use_mask_lop(
          io.ticker.sram.write,
          (lop: ValidIO[SRAMRFWriteOp]) => lop.valid && lop.bits.selg && lop.bits.wsel === UInt(i)
        ) }

      val select = Vec.fill(nSeq){new SeqSelect}

      val result =
        (0 until nSeq) map { r =>
          val op_idx = e(r).rports + UInt(expLatency, bRPorts+1)
          val strip = stripfn(e(r).vlen, Bool(false), e(r).fn)
          val ask_op_mask = UInt(strip_to_bmask(strip) << op_idx, maxXbarTicks+nBanks-1)
          val ask_wport_mask = UInt(strip_to_bmask(strip) << e(r).wport, maxWPortLatency+nBanks-1)
          def check_shazard(use_mask: Bits, ask_mask: Bits) = (use_mask & ask_mask).orR
          def check_op_shazard(use_mask: Bits) = check_shazard(use_mask, ask_op_mask)
          def check_rport(fn: RegFn, i: Int) =
            fn(e(r).reg).valid && (
              !fn(e(r).reg).scalar && check_op_shazard(use_mask_xbar(i)) ||
              fn(e(r).reg).scalar && check_op_shazard(use_mask_sreg_global(i)))
          val check_rport_0_1 = check_rport(reg_vs1, 0) || check_rport(reg_vs2, 1)
          val check_rport_0_1_2 = check_rport_0_1 || check_rport(reg_vs3, 2)
          val check_rport_2 = check_rport(reg_vs1, 2)
          val check_rport_3_4 = check_rport(reg_vs1, 3) || check_rport(reg_vs2, 4)
          val check_rport_3_4_5 = check_rport_3_4 || check_rport(reg_vs3, 5)
          val check_rport_5 = check_rport(reg_vs1, 5)
          def check_wport(i: Int) =
            e(r).reg.vd.valid && (e(r).reg.vd.scalar || check_shazard(use_mask_wport(i), ask_wport_mask))
          val check_wport_0 = check_wport(0)
          val check_wport_1 = check_wport(1)
          val shazard_vimu = check_rport_0_1 || check_wport_0 || check_op_shazard(use_mask_vimu)
          val shazard_vfmu0 = check_rport_0_1_2 || check_wport_0 || check_op_shazard(use_mask_vfmu(0))
          val shazard_vfmu1 = check_rport_3_4_5 || check_wport_1 || check_op_shazard(use_mask_vfmu(1))
          val shazard_vfcu = check_rport_3_4 || check_wport_1 || check_op_shazard(use_mask_vfcu)
          val shazard_vfvu = check_rport_2 || check_wport_0 || check_op_shazard(use_mask_vfvu)
          val shazard_vgu = check_rport_5 || check_wport_1 || check_op_shazard(use_mask_vgu)
          val shazard_vqu = check_rport_3_4 || check_wport_1 || check_op_shazard(use_mask_vqu)
          select(r).vfmu := Mux(shazard_vfmu0, UInt(1), UInt(0))
          val a = e(r).active
          val out =
            a.vimu && shazard_vimu ||
            a.vfmu && shazard_vfmu0 && shazard_vfmu1 || a.vfcu && shazard_vfcu || a.vfvu && shazard_vfvu ||
            a.vgu && shazard_vgu || a.vqu && shazard_vqu
          out
        }

      def debug = {
        io.debug.shazard := result
        io.debug.use_mask_sreg_global := Vec((0 until nGOPL) map { i => use_mask_sreg_global(i) })
        io.debug.use_mask_xbar := Vec((0 until nGOPL) map { i => use_mask_xbar(i) })
        io.debug.use_mask_vimu := use_mask_vimu
        io.debug.use_mask_vfmu := Vec((0 until nVFMU) map { i => use_mask_vfmu(i) })
        io.debug.use_mask_vfcu := use_mask_vfcu
        io.debug.use_mask_vfvu := use_mask_vfvu
        io.debug.use_mask_vgu := use_mask_vgu
        io.debug.use_mask_vqu := use_mask_vqu
        io.debug.use_mask_wport := use_mask_wport
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // issue window helpers

  val iwindow = new {
    val update_head = Bool()
    val update_tail = Bool()
    val next_head = UInt(width = log2Up(nSeq))
    val next_tail = UInt(width = log2Up(nSeq))

    val set = new {
      def head(n: UInt) = { update_head := Bool(true); next_head := n }
      def tail(n: UInt) = { update_tail := Bool(true); next_tail := n }

      def valid(n: UInt) = { v(n) := Bool(true) }

      def entry(n: UInt) = {
        valid(n)
        e(n).vlen := io.op.bits.vlen
        e(n).eidx := UInt(0)
        e(n).reg.vs1.valid := Bool(false)
        e(n).reg.vs2.valid := Bool(false)
        e(n).reg.vs3.valid := Bool(false)
        e(n).reg.vd.valid := Bool(false)
        e(n).base.vs1.valid := Bool(false)
        e(n).base.vs2.valid := Bool(false)
        e(n).base.vs3.valid := Bool(false)
        e(n).base.vd.valid := Bool(false)
        e(n).age := UInt(0)
        dhazard.update(n)
        for (i <- 0 until nSeq) {
          dhazard.clear.raw(n, UInt(i))
          dhazard.clear.war(n, UInt(i))
          dhazard.clear.waw(n, UInt(i))
        }
      }

      def active(n: UInt, afn: SeqType=>Bool, fn: IssueOp=>Bits) = {
        afn(e(n).active) := Bool(true)
        e(n).fn.union := fn(io.op.bits)
      }

      val fn_identity = (d: IssueOp) => d.fn.union
      val fn_vqu = (d: IssueOp) => {
        assert(d.active.vidiv || d.active.vfdiv, "vqu should only be issued for idiv/fdiv")
        Cat(d.active.vidiv || d.fn.vfdu().op_is(FD_DIV), Bool(true))
      }

      def viu(n: UInt) = active(n, (a: SeqType) => a.viu, fn_identity)
      def vimu(n: UInt) = active(n, (a: SeqType) => a.vimu, fn_identity)
      def vidu(n: UInt) = active(n, (a: SeqType) => a.vidu, fn_identity)
      def vfmu(n: UInt) = active(n, (a: SeqType) => a.vfmu, fn_identity)
      def vfdu(n: UInt) = active(n, (a: SeqType) => a.vfdu, fn_identity)
      def vfcu(n: UInt) = active(n, (a: SeqType) => a.vfcu, fn_identity)
      def vfvu(n: UInt) = active(n, (a: SeqType) => a.vfvu, fn_identity)
      def vpu(n: UInt) = active(n, (a: SeqType) => a.vpu, fn_identity)
      def vgu(n: UInt) = active(n, (a: SeqType) => a.vgu, fn_identity)
      def vcu(n: UInt) = active(n, (a: SeqType) => a.vcu, fn_identity)
      def vlu(n: UInt) = active(n, (a: SeqType) => a.vlu, fn_identity)
      def vsu(n: UInt) = active(n, (a: SeqType) => a.vsu, fn_identity)
      def vqu(n: UInt) = active(n, (a: SeqType) => a.vqu, fn_vqu)

      def vs(n: UInt, e_vsfn: RegFn, op_vsfn: RegFn, e_ssfn: SRegFn, op_ssfn: SRegFn) = {
        when (op_vsfn(io.op.bits.reg).valid) {
          e_vsfn(e(n).reg) := op_vsfn(io.op.bits.reg)
          e_vsfn(e(n).base) := op_vsfn(io.op.bits.reg)
          when (op_vsfn(io.op.bits.reg).scalar) {
            e_ssfn(e(n).sreg) := op_ssfn(io.op.bits.sreg)
          }
        }
      }
      def vs1(n: UInt) = {
        vs(n, reg_vs1, reg_vs1, sreg_ss1, sreg_ss1)
        dhazard.set.raw_vs1(n)
      }
      def vs2(n: UInt) = {
        vs(n, reg_vs2, reg_vs2, sreg_ss2, sreg_ss2)
        dhazard.set.raw_vs2(n)
      }
      def vs3(n: UInt) = {
        vs(n, reg_vs3, reg_vs3, sreg_ss3, sreg_ss3)
        dhazard.set.raw_vs3(n)
      }
      def vs2_as_vs1(n: UInt) = {
        vs(n, reg_vs1, reg_vs2, sreg_ss1, sreg_ss2)
        dhazard.set.raw_vs2(n)
      }
      def vd_as_vs1(n: UInt) = {
        assert(!io.op.bits.reg.vd.valid || !io.op.bits.reg.vd.scalar, "iwindow.set.vd_as_vs1: vd should always be vector")
        when (io.op.bits.reg.vd.valid) {
          e(n).reg.vs1 := io.op.bits.reg.vd
          e(n).base.vs1 := io.op.bits.reg.vd
        }
        dhazard.set.raw_vd(n)
      }
      def vd(n: UInt) = {
        assert(!io.op.bits.reg.vd.valid || !io.op.bits.reg.vd.scalar, "iwindow.set.vd: vd should always be vector")
        when (io.op.bits.reg.vd.valid) {
          e(n).reg.vd := io.op.bits.reg.vd
          e(n).base.vd := io.op.bits.reg.vd
        }
        dhazard.set.war_vd(n)
        dhazard.set.waw_vd(n)
      }
    }

    val clear = new {
      def valid(n: UInt) = { v(n) := Bool(false) }
      def active(n: UInt) = { e(n).active := e(0).active.clone().fromBits(Bits(0)) }
    }

    def retire(n: UInt) = {
      clear.valid(n)
      for (i <- 0 until nSeq) {
        dhazard.update(UInt(i))
        dhazard.clear.raw(UInt(i), n)
        dhazard.clear.war(UInt(i), n)
        dhazard.clear.waw(UInt(i), n)
      }
    }

    def ready = {
      val count = Cat(full && head === tail, tail - head)
      val empty = UInt(nSeq) - count
      val a = io.op.bits.active

      (empty >= UInt(1)) && (a.vint || a.vimul || a.vfma || a.vfcmp || a.vfconv) ||
      (empty >= UInt(2)) && (a.vidiv || a.vfdiv) ||
      (empty >= UInt(3)) && (a.vld || a.vst || a.vldx || a.vstx) ||
      (empty >= UInt(4)) && (a.vamo)
    }

    def header = {
      update_head := Bool(false)
      update_tail := Bool(false)
      next_head := head
      next_tail := tail
    }

    def logic = {
      io.op.ready := ready

      when (update_head) { head := next_head }
      when (update_tail) { tail := next_tail }
      when (update_head && !update_tail) {
        full := Bool(false)
      }
      when (update_tail) {
        full := next_head === next_tail
      }

      when (v(head) && e(head).vlen === UInt(0)) {
        retire(head)
        set.head(head + UInt(1))
      }
    }

    def footer = {
      when (reset) {
        for (i <- 0 until nSeq) {
          clear.valid(UInt(i))
          clear.active(UInt(i))
        }
      }
    }

    def debug = {
      io.debug.head := head
      io.debug.tail := tail
      io.debug.full := full
      io.debug.valid := v
      io.debug.e := e
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // issue helpers

  val issue = new {
    val t0 = tail
    val t1 = tail + UInt(1)
    val t2 = tail + UInt(2)
    val t3 = tail + UInt(3)
    val t4 = tail + UInt(4)

    def start(n: UInt) = iwindow.set.entry(n)
    def stop(n: UInt) = iwindow.set.tail(n)

    def vint = {
      start(t0); { import iwindow.set._; viu(t0); vs1(t0); vs2(t0); vd(t0); }
                 { import bhazard.set._; rwports(t0, stagesALU); }
      stop(t1); }

    def vimul = {
      start(t0); { import iwindow.set._; vimu(t0); vs1(t0); vs2(t0); vd(t0); }
                 { import bhazard.set._; rwports(t0, stagesIMul); }
      stop(t1); }

    def vidiv = {
      start(t0); { import iwindow.set._; vqu(t0); vs1(t0); vs2(t0); }
                 { import bhazard.set._; rports(t0); }
                 { import dhazard.set._; war_vd(t0); waw_vd(t0); }
      start(t1); { import iwindow.set._; vidu(t1); vd(t1); }
                 { import bhazard.set._; noports(t1); }
      stop(t2); }

    def vfma = {
      start(t0); { import iwindow.set._; vfmu(t0); vs1(t0); vs2(t0); vs3(t0); vd(t0); }
                 { import bhazard.set._; rwports(t0, stagesFMA); }
      stop(t1); }

    def vfdiv = {
      start(t0); { import iwindow.set._; vqu(t0); vs1(t0); vs2(t0); }
                 { import bhazard.set._; rports(t0); }
                 { import dhazard.set._; war_vd(t0); waw_vd(t0); }
      start(t1); { import iwindow.set._; vfdu(t1); vd(t1); }
                 { import bhazard.set._; noports(t1); }
      stop(t2); }

    def vfcmp = {
      start(t0); { import iwindow.set._; vfcu(t0); vs1(t0); vs2(t0); vd(t0); }
                 { import bhazard.set._; rwports(t0, stagesFCmp); }
      stop(t1); }

    def vfconv = {
      start(t0); { import iwindow.set._; vfvu(t0); vs1(t0); vd(t0); }
                 { import bhazard.set._; rwports(t0, stagesFConv); }
      stop(t1); }

    def vamo = {
      start(t0); { import iwindow.set._; vgu(t0); vs1(t0); }
                 { import bhazard.set._; rport_vs1(t0); }
      start(t1); { import iwindow.set._; vcu(t1); }
                 { import bhazard.set._; noports(t1); }
                 { import dhazard.set._; war_vd(t1); waw_vd(t1); }
      start(t2); { import iwindow.set._; vsu(t2); vs2_as_vs1(t2); }
                 { import bhazard.set._; rport_vs2(t2); }
                 { import dhazard.set._; raw(t2, t1); }
      start(t3); { import iwindow.set._; vlu(t3); vd(t3); }
                 { import bhazard.set._; noports(t3); }
      stop(t4); }

    def vldx = {
      start(t0); { import iwindow.set._; vgu(t0); vs2_as_vs1(t0); }
                 { import bhazard.set._; rport_vs2(t0); }
      start(t1); { import iwindow.set._; vcu(t1); }
                 { import bhazard.set._; noports(t1); }
                 { import dhazard.set._; war_vd(t1); waw_vd(t1); }
      start(t2); { import iwindow.set._; vlu(t2); vd(t2); }
                 { import bhazard.set._; noports(t2); }
      stop(t3); }

    def vstx = {
      start(t0); { import iwindow.set._; vgu(t0); vs2_as_vs1(t0); }
                 { import bhazard.set._; rport_vs2(t0); }
      start(t1); { import iwindow.set._; vcu(t1); }
                 { import bhazard.set._; noports(t1); }
      start(t2); { import iwindow.set._; vsu(t2); vd_as_vs1(t2); }
                 { import bhazard.set._; rport_vd(t2); }
                 { import dhazard.set._; raw(t2, t1); }
      stop(t3); }

    def vld = {
      start(t0); { import iwindow.set._; vpu(t0); }
                 { import bhazard.set._; noports(t0); }
      start(t1); { import iwindow.set._; vcu(t1); }
                 { import bhazard.set._; noports(t1); }
                 { import dhazard.set._; war_vd(t1); waw_vd(t1); }
      start(t2); { import iwindow.set._; vlu(t2); vd(t2); }
                 { import bhazard.set._; noports(t2); }
      stop(t3); }

    def vst = {
      start(t0); { import iwindow.set._; vpu(t0); }
                 { import bhazard.set._; noports(t0); }
      start(t1); { import iwindow.set._; vcu(t1); }
                 { import bhazard.set._; noports(t1); }
      start(t2); { import iwindow.set._; vsu(t2); vd_as_vs1(t2); }
                 { import bhazard.set._; rport_vd(t2); }
                 { import dhazard.set._; raw(t2, t1); }
      stop(t3); }

    def logic = {
      when (io.op.fire()) {
        when (io.op.bits.active.vint) { vint }
        when (io.op.bits.active.vimul) { vimul }
        when (io.op.bits.active.vidiv) { vidiv }
        when (io.op.bits.active.vfma) { vfma }
        when (io.op.bits.active.vfdiv) { vfdiv }
        when (io.op.bits.active.vfcmp) { vfcmp }
        when (io.op.bits.active.vfconv) { vfconv }
        when (io.op.bits.active.vamo) { vamo }
        when (io.op.bits.active.vldx) { vldx }
        when (io.op.bits.active.vstx) { vstx }
        when (io.op.bits.active.vld) { vld }
        when (io.op.bits.active.vst) { vst }
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // scheduling helpers

  val scheduling = new {
    def find_first(fn: Int=>Bool) = {
      val internal = Vec.fill(2*nSeq){Bool()}
      for (i <- 0 until nSeq) {
        internal(i+nSeq) := v(i) && fn(i)
        internal(i) := internal(i+nSeq) && (UInt(i) >= head)
      }
      val priority_oh = PriorityEncoderOH(internal)
      val out = Vec.fill(nSeq){Bool()}
      for (i <- 0 until nSeq) {
        out(i) := priority_oh(i) | priority_oh(i+nSeq)
      }
      out
    }

    def valfn(sched: Vec[Bool]) = sched.reduce(_||_)

    def readfn[T <: Data](sched: Vec[Bool], rfn: SeqEntry=>T) =
      rfn(e(0)).clone.fromBits(Mux1H(sched, e.map(rfn(_).toBits)))

    def selectfn(sched: Vec[Bool]) =
      new SeqSelect().fromBits(Mux1H(sched, shazard.check.select.map(_.toBits)))

    def nohazards(i: Int) =
      !dhazard.check.result(i) && !bhazard.check.result(i) && !shazard.check.result(i)

    val exp = new {
      val vgu_consider = Vec.fill(nSeq){Bool()}
      val vsu_consider = Vec.fill(nSeq){Bool()}
      val vqu_consider = Vec.fill(nSeq){Bool()}

      val consider = (i: Int) => nohazards(i) && (
        e(i).active.viu || e(i).active.vimu ||
        e(i).active.vfmu || e(i).active.vfcu || e(i).active.vfvu ||
        e(i).active.vgu && vgu_consider(i) ||
        e(i).active.vsu && vsu_consider(i) ||
        e(i).active.vqu && vqu_consider(i))
      val first_sched = find_first((i: Int) => consider(i) && e(i).age === UInt(0))
      val second_sched = find_first((i: Int) => consider(i))
      val sel = first_sched.reduce(_ || _)
      val sched = Vec(first_sched zip second_sched map { case (f, s) => Mux(sel, f, s) })
      val valid = valfn(sched)
      val vlen = readfn(sched, (e: SeqEntry) => e.vlen)
      val op = {
        val out = new SeqOp
        out.fn := readfn(sched, (e: SeqEntry) => e.fn)
        out.reg := readfn(sched, (e: SeqEntry) => e.reg)
        out.sreg := readfn(sched, (e: SeqEntry) => e.sreg)
        out.active := readfn(sched, (e: SeqEntry) => e.active)
        out.select := selectfn(sched)
        out.eidx := readfn(sched, (e: SeqEntry) => e.eidx)
        out.rports := readfn(sched, (e: SeqEntry) => e.rports)
        out.wport := readfn(sched, (e: SeqEntry) => e.wport)
        out.strip := stripfn(vlen, Bool(false), out.fn)
        out
      }

      def fire(n: Int) = sched(n)
      def fire_vgu = valid && op.active.vgu
      def fire_vsu = valid && op.active.vsu
      def fire_vqu = valid && op.active.vqu
      def fire_vqu_latch(n: Int) = fire_vqu && op.fn.vqu().latch(n)

      def logic = {
        io.seq.exp.valid := valid
        io.seq.exp.bits := op
      }

      def debug = {
        (io.debug.consider zipWithIndex) foreach { case (io, i) => io := consider(i) }
        (io.debug.first_sched zip first_sched) foreach { case (io, c) => io := c }
        (io.debug.second_sched zip second_sched) foreach { case (io, c) => io := c }
      }
    }

    val vidu = new {
      val sched = find_first((i: Int) => e(i).active.vidu && nohazards(i))
      val valid = valfn(sched)
      val vlen = readfn(sched, (e: SeqEntry) => e.vlen)
      val fn = readfn(sched, (e: SeqEntry) => e.fn)
      val strip = stripfn(vlen, Bool(false), fn)

      val ready = io.dila.available
      def fire(n: Int) = sched(n) && ready

      def logic = {
        io.dila.cnt := strip_to_bcnt(strip)
        io.dila.reserve := valid && ready
      }
    }

    val vfdu = new {
      val sched = find_first((i: Int) => e(i).active.vfdu && nohazards(i))
      val valid = valfn(sched)
      val vlen = readfn(sched, (e: SeqEntry) => e.vlen)
      val fn = readfn(sched, (e: SeqEntry) => e.fn)
      val strip = stripfn(vlen, Bool(false), fn)

      val ready = io.dfla.available
      def fire(n: Int) = sched(n) && ready

      def logic = {
        io.dfla.cnt := strip_to_bcnt(strip)
        io.dfla.reserve := valid && ready
      }
    }

    val vpu = new {
      val sched = find_first((i: Int) => e(i).active.vpu || e(i).active.vgu)
      val valid = valfn(sched)
      val vlen = readfn(sched, (e: SeqEntry) => e.vlen)
      val fn = readfn(sched, (e: SeqEntry) => e.fn)
      val strip = stripfn(vlen, Bool(false), fn)
      val sel = readfn(sched, (e: SeqEntry) => e.active.vgu)

      val ready = io.pla.available && (!sel || exp.fire_vgu)
      def fire(n: Int) = sched(n) && ready

      def logic = {
        io.seq.vpu.valid := valid && ready
        io.seq.vpu.bits.strip := strip
        io.pla.reserve := valid && ready
        io.pla.mask := strip_to_bmask(strip)
      }

      def debug = {
        (io.debug.pred_first zip sched) foreach { case (io, c) => io := c }
      }
    }

    val vgu = new {
      val first = find_first((i: Int) => e(i).active.vgu)
      val vlen = readfn(first, (e: SeqEntry) => e.vlen)
      val fn = readfn(first, (e: SeqEntry) => e.fn)
      val strip = stripfn(vlen, Bool(false), fn)
      val cnt = strip_to_bcnt(strip)

      val ready = io.gpla.available && io.gqla.available
      (0 until nSeq) map { i =>
        exp.vgu_consider(i) := vpu.sched(i) && first(i) && io.pla.available && ready }

      def logic = {
        io.gpla.cnt := cnt
        io.gpla.reserve := exp.fire_vgu
        io.gqla.cnt := cnt
        io.gqla.reserve := exp.fire_vgu
      }
    }

    val vcu = new {
      val sched = find_first((i: Int) => e(i).active.vcu && nohazards(i))
      val valid = valfn(sched)
      val vlen = readfn(sched, (e: SeqEntry) => e.vlen)
      val fn = readfn(sched, (e: SeqEntry) => e.fn)
      val mcmd = DecodedMemCommand(fn.vmu().cmd)
      val strip = stripfn(vlen, Bool(false), fn)

      val ready =
        io.vmu.pala.available &&
        (!mcmd.read || io.lreq.available) &&
        (!mcmd.write || io.sreq.available)
      def fire(n: Int) = sched(n) && ready

      def logic = {
        io.vmu.pala.cnt := strip
        io.vmu.pala.reserve := valid && ready
        io.lreq.cnt := strip
        io.lreq.reserve := valid && ready && mcmd.read
        io.sreq.cnt := strip
        io.sreq.reserve := valid && ready && mcmd.store
      }
    }

    val vlu = new {
      val sched = find_first((i: Int) => e(i).active.vlu && nohazards(i))
      val valid = valfn(sched)
      val vlen = readfn(sched, (e: SeqEntry) => e.vlen)
      val fn = readfn(sched, (e: SeqEntry) => e.fn)
      val strip = stripfn(vlen, Bool(false), fn)

      val ready = io.lla.available
      def fire(n: Int) = sched(n) && ready

      def logic = {
        io.lla.cnt := strip
        io.lla.reserve := valid && io.lla.available
      }
    }

    val vsu = new {
      val first = find_first((i: Int) => e(i).active.vsu)
      val vlen = readfn(first, (e: SeqEntry) => e.vlen)
      val fn = readfn(first, (e: SeqEntry) => e.fn)
      val strip = stripfn(vlen, Bool(false), fn)

      val ready = io.sla.available
      (0 until nSeq) map { i => exp.vsu_consider(i) := first(i) && ready }

      def logic = {
        io.sla.reserve := exp.fire_vsu
        io.sla.mask := strip_to_bmask(strip)
      }
    }

    val vqu = new {
      val first = find_first((i: Int) => e(i).active.vqu)
      val vlen = readfn(first, (e: SeqEntry) => e.vlen)
      val fn = readfn(first, (e: SeqEntry) => e.fn)
      val strip = stripfn(vlen, Bool(false), fn)
      val cnt = strip_to_bcnt(strip)

      def ready(n: Int) =
        io.dpla.available &&
        (!e(n).fn.vqu().latch(0) || io.dqla(0).available) &&
        (!e(n).fn.vqu().latch(1) || io.dqla(1).available)
      (0 until nSeq) map { i => exp.vqu_consider(i) := first(i) && ready(i) }

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
      exp.logic
      vidu.logic
      vfdu.logic
      vpu.logic
      vgu.logic
      vcu.logic
      vlu.logic
      vsu.logic
      vqu.logic

      io.pending := v.reduce(_ || _)

      def fire(n: Int) =
        vidu.fire(n) || vfdu.fire(n) ||
        vpu.fire(n) || vcu.fire(n) || vlu.fire(n) ||
        exp.fire(n)

      def update_reg(i: Int, fn: RegFn) = {
        when (fn(e(i).reg).valid && !fn(e(i).reg).scalar) {
          fn(e(i).reg).id := fn(e(i).reg).id + io.cfg.vstride
        }
      }

      for (i <- 0 until nSeq) {
        val strip = stripfn(e(i).vlen, Bool(false), e(i).fn)
        when (v(i)) {
          when (fire(i)) {
            e(i).vlen := e(i).vlen - strip
            e(i).eidx := e(i).eidx + strip
            update_reg(i, reg_vs1)
            update_reg(i, reg_vs2)
            update_reg(i, reg_vs3)
            update_reg(i, reg_vd)
            when (e(i).vlen === strip) {
              iwindow.clear.active(UInt(i))
            }
          }
          when (e(i).age.orR) {
            e(i).age := e(i).age - UInt(1)
          }
          when (exp.fire(i)) {
            e(i).age := UInt(nBanks-1)
          }
        }
      }
    }

    def debug = {
      vpu.debug
      exp.debug
    }
  }

  iwindow.header
  dhazard.header

  iwindow.logic
  issue.logic
  dhazard.logic
  scheduling.logic

  iwindow.footer

  iwindow.debug
  dhazard.check.debug
  bhazard.check.debug
  shazard.check.debug
  scheduling.debug
}

package hwacha

import Chisel._
import cde.Parameters
import DataGating._

abstract trait SeqParameters extends UsesHwachaParameters
  with LaneParameters with MemParameters {
  val nSeq = 8
  val nRPorts = 3
  val bRPorts = log2Down(nRPorts) + 1
  val expLatency = 1
  val maxWPortLatency = nRPorts + 1 + expLatency +
    List(stagesALU, stagesIMul, stagesFMA,
         stagesFConv, stagesFCmp).reduceLeft((x, y) => if (x > y) x else y)
  val bWPortLatency = log2Down(maxWPortLatency) + 1
  val maxPredWPortLatency = expLatency +
    List(stagesPLU, 2 + 1 + stagesALU,
         2 + 1 + stagesFCmp).reduceLeft((x, y) => if (x > y) x else y)
  val bPredWPortLatency = log2Down(maxPredWPortLatency) + 1
  val maxStrip = nBanks * (wBank / SZ_B)
  val bfStrip = log2Down(maxStrip) + 1
  val maxLookAhead = math.max(tlDataBits / SZ_B, nStrip)
  val bLookAhead = log2Down(maxLookAhead) + 1

  // the following needs to hold in order to simplify dhazard_war checking
  // otherwise, you need to check reg_vd against sram read ticker
  require(nRPorts <= 3)
  require(
    List(stagesALU, stagesIMul, stagesFMA,
         stagesFConv, stagesFCmp).reduceLeft((x, y) => if (x > y) y else x) >= 1)

  type RegFn = BaseRegisters => RegInfo
  val reg_vp  = (reg: BaseRegisters) => reg.vp
  val reg_vs1 = (reg: BaseRegisters) => reg.vs1
  val reg_vs2 = (reg: BaseRegisters) => reg.vs2
  val reg_vs3 = (reg: BaseRegisters) => reg.vs3
  val reg_vd  = (reg: BaseRegisters) => reg.vd

  type PRegIdFn = PhysicalRegisterIds => RegId
  val pregid_vp  = (reg: PhysicalRegisterIds) => reg.vp
  val pregid_vs1 = (reg: PhysicalRegisterIds) => reg.vs1
  val pregid_vs2 = (reg: PhysicalRegisterIds) => reg.vs2
  val pregid_vs3 = (reg: PhysicalRegisterIds) => reg.vs3
  val pregid_vd  = (reg: PhysicalRegisterIds) => reg.vd

  type PRegFn = PhysicalRegisters => RegInfo
  val preg_vp  = (reg: PhysicalRegisters) => reg.vp
  val preg_vs1 = (reg: PhysicalRegisters) => reg.vs1
  val preg_vs2 = (reg: PhysicalRegisters) => reg.vs2
  val preg_vs3 = (reg: PhysicalRegisters) => reg.vs3
  val preg_vd  = (reg: PhysicalRegisters) => reg.vd

  type SRegFn = ScalarRegisters => Bits
  val sreg_ss1 = (sreg: ScalarRegisters) => sreg.ss1
  val sreg_ss2 = (sreg: ScalarRegisters) => sreg.ss2
  val sreg_ss3 = (sreg: ScalarRegisters) => sreg.ss3
}

class MasterSequencerState(implicit p: Parameters) extends VXUBundle()(p) {
  val valid = Vec.fill(nSeq){Bool()}
  val e = Vec.fill(nSeq){new MasterSeqEntry}
  val head = UInt(OUTPUT, log2Up(nSeq))
}

class UpdateSequencerState(implicit p: Parameters) extends VXUBundle()(p) {
  val valid = Vec.fill(nSeq){Bool()}
  val reg = Vec.fill(nSeq){new PhysicalRegisterIds}
}

class MasterSequencerIO(implicit p: Parameters) extends VXUBundle()(p) {
  val state = new MasterSequencerState().asOutput
  val update = new UpdateSequencerState().asOutput
  val clear = Vec.fill(nSeq){Bool()}.asInput
}

class MasterSequencer(implicit p: Parameters) extends VXUModule()(p) with SeqLogic {
  val io = new Bundle {
    val op = Decoupled(new IssueOpBase).flip
    val master = new MasterSequencerIO
    val busy = Bool(OUTPUT)

    val debug = new Bundle {
      val head = UInt(OUTPUT, log2Up(nSeq))
      val tail = UInt(OUTPUT, log2Up(nSeq))
      val full = Bool(OUTPUT)
    }
  }

  require(isPow2(nSeq))

  val v = Vec.fill(nSeq){Reg(init=Bool(false))}
  val e = Vec.fill(nSeq){Reg(new MasterSeqEntry)}
  val full = Reg(init = Bool(false))
  val head = Reg(init = UInt(0, log2Up(nSeq)))
  val tail = Reg(init = UInt(0, log2Up(nSeq)))

  io.master.state.valid := v
  io.master.state.e := e
  io.master.state.head := head

  var retired = false

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
          sfn(e(i).base).valid && ifn(io.op.bits.base).valid &&
          !sfn(e(i).base).is_scalar() && !ifn(io.op.bits.base).is_scalar() &&
          (sfn(e(i).base).is_vector() && ifn(io.op.bits.base).is_vector() ||
           sfn(e(i).base).is_pred() && ifn(io.op.bits.base).is_pred()) &&
          sfn(e(i).base).id === ifn(io.op.bits.base).id })
      val ivp_evd_eq = issue_base_eq(reg_vp, reg_vd)
      val ivs1_evd_eq = issue_base_eq(reg_vs1, reg_vd)
      val ivs2_evd_eq = issue_base_eq(reg_vs2, reg_vd)
      val ivs3_evd_eq = issue_base_eq(reg_vs3, reg_vd)
      val ivd_evp_eq = issue_base_eq(reg_vd, reg_vp)
      val ivd_evs1_eq = issue_base_eq(reg_vd, reg_vs1)
      val ivd_evs2_eq = issue_base_eq(reg_vd, reg_vs2)
      val ivd_evs3_eq = issue_base_eq(reg_vd, reg_vs3)
      val ivd_evd_eq  = issue_base_eq(reg_vd, reg_vd)

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
        val cnt = PopCount(fns.map{ fn => fn(io.op.bits.base).valid && fn(io.op.bits.base).is_vector() })
        // make sure # of rports is larger than 1, because we need to read predicate
        val gated = !io.op.bits.active.vipred
        val min = Mux(gated, UInt(1), UInt(0))
        Mux(cnt > UInt(0), cnt, min)
      }
      val nrports = nports_list(reg_vs1, reg_vs2, reg_vs3)
      val nrport_vs1 = nports_list(reg_vs1)
      val nrport_vs2 = nports_list(reg_vs2)
      val nrport_vd = nports_list(reg_vd)

      def mark_rports(n: UInt, rports: UInt) = {
        e(n).rports := rports
        e(n).wport.sram := UInt(0)
        e(n).wport.pred := UInt(0)
      }
      def mark_wport(n: UInt, wport: UInt) = {
        when (io.op.bits.base.vd.is_vector()) { e(n).wport.sram := wport }
        when (io.op.bits.base.vd.is_pred()) { e(n).wport.pred := wport }
      }
      def noports(n: UInt) = mark_rports(n, UInt(0))
      def rport_vs1(n: UInt) = mark_rports(n, nrport_vs1)
      def rport_vs2(n: UInt) = mark_rports(n, nrport_vs2)
      def rport_vd(n: UInt) = mark_rports(n, nrport_vd)
      def rports(n: UInt) = mark_rports(n, nrports)
      def rwports(n: UInt, latency: Int) = {
        mark_rports(n, nrports)
        mark_wport(n, nrports + UInt(expLatency+latency))
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
        e(n).base.vp.valid := Bool(false)
        e(n).base.vs1.valid := Bool(false)
        e(n).base.vs2.valid := Bool(false)
        e(n).base.vs3.valid := Bool(false)
        e(n).base.vd.valid := Bool(false)
        dhazard.update(n)
        for (i <- 0 until nSeq) {
          dhazard.clear.raw(n, UInt(i))
          dhazard.clear.war(n, UInt(i))
          dhazard.clear.waw(n, UInt(i))
        }
        io.master.update.valid(n) := Bool(true)
      }

      def active(n: UInt, afn: SeqType=>Bool, fn: IssueOpBase=>Bits) = {
        afn(e(n).active) := Bool(true)
        e(n).fn.union := fn(io.op.bits)
      }

      val fn_identity = (d: IssueOpBase) => d.fn.union
      val fn_vqu = (d: IssueOpBase) => {
        assert(d.active.vidiv || d.active.vfdiv || d.active.vrpred || d.active.vrfirst,
          "vqu should only be issued for idiv/fdiv/rpred/rfirst")
        Cat(d.active.vidiv || d.active.vfdiv && d.fn.vfdu().op_is(FD_DIV),
            d.active.vidiv || d.active.vfdiv || d.active.vrfirst)
      }

      def viu(n: UInt) = active(n, (a: SeqType) => a.viu, fn_identity)
      def vipu(n: UInt) = active(n, (a: SeqType) => a.vipu, fn_identity)
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

      def vp(n: UInt) = {
        when (io.op.bits.base.vp.valid) {
          e(n).base.vp := io.op.bits.base.vp
          io.master.update.reg(n).vp.id := io.op.bits.base.vp.id
        }
        dhazard.set.raw_vp(n)
      }
      def vs(n: UInt, e_vsfn: RegFn, e_pvsfn: PRegIdFn, op_vsfn: RegFn, e_ssfn: SRegFn, op_ssfn: SRegFn) = {
        when (op_vsfn(io.op.bits.base).valid) {
          e_vsfn(e(n).base) := op_vsfn(io.op.bits.base)
          e_pvsfn(io.master.update.reg(n)).id := op_vsfn(io.op.bits.base).id
          when (op_vsfn(io.op.bits.base).is_scalar()) {
            e_ssfn(e(n).sreg) := op_ssfn(io.op.bits.sreg)
          }
        }
      }
      def vs1(n: UInt) = {
        vs(n, reg_vs1, pregid_vs1, reg_vs1, sreg_ss1, sreg_ss1)
        dhazard.set.raw_vs1(n)
      }
      def vs2(n: UInt) = {
        vs(n, reg_vs2, pregid_vs2, reg_vs2, sreg_ss2, sreg_ss2)
        dhazard.set.raw_vs2(n)
      }
      def vs3(n: UInt) = {
        vs(n, reg_vs3, pregid_vs3, reg_vs3, sreg_ss3, sreg_ss3)
        dhazard.set.raw_vs3(n)
      }
      def vs2_as_vs1(n: UInt) = {
        vs(n, reg_vs1, pregid_vs1, reg_vs2, sreg_ss1, sreg_ss2)
        dhazard.set.raw_vs2(n)
      }
      def vd_as_vs1(n: UInt) = {
        assert(!io.op.bits.base.vd.valid || !io.op.bits.base.vd.is_scalar(), "iwindow.set.vd_as_vs1: vd should always be vector")
        when (io.op.bits.base.vd.valid) {
          e(n).base.vs1 := io.op.bits.base.vd
          io.master.update.reg(n).vs1.id := io.op.bits.base.vd.id
        }
        dhazard.set.raw_vd(n)
      }
      def vd(n: UInt) = {
        assert(!io.op.bits.base.vd.valid || !io.op.bits.base.vd.is_scalar(), "iwindow.set.vd: vd should always be vector")
        when (io.op.bits.base.vd.valid) {
          e(n).base.vd := io.op.bits.base.vd
          io.master.update.reg(n).vd.id := io.op.bits.base.vd.id
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
      clear.active(n)
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

      (empty >= UInt(1)) && (a.vint || a.vipred || a.vimul || a.vfma || a.vfcmp || a.vfconv) ||
      (empty >= UInt(2)) && (a.vidiv || a.vfdiv || a.vrpred || a.vrfirst) ||
      (empty >= UInt(3)) && (a.vld || a.vst || a.vldx || a.vstx) ||
      (empty >= UInt(4)) && (a.vamo)
    }

    def header = {
      update_head := Bool(false)
      update_tail := Bool(false)
      next_head := head
      next_tail := tail

      (0 until nSeq) map { r =>
        io.master.update.valid(r) := Bool(false)
        io.master.update.reg(r) := new PhysicalRegisterIds().fromBits(UInt(0))
      }
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

      when (v(head) && io.master.clear(head)) {
        retire(head)
        set.head(head + UInt(1))
      }

      io.busy := v.reduce(_ || _)

      retired = true
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
      io.debug.full := full
      io.debug.head := head
      io.debug.tail := tail
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
      start(t0); { import iwindow.set._; viu(t0); vp(t0); vs1(t0); vs2(t0); vd(t0); }
                 { import bhazard.set._; rwports(t0, stagesALU); }
      stop(t1); }

    def vipred = {
      start(t0); { import iwindow.set._; vipu(t0); vs1(t0); vs2(t0); vs3(t0); vd(t0); }
                 { import bhazard.set._; rwports(t0, stagesPLU); }
      stop(t1); }

    def vimul = {
      start(t0); { import iwindow.set._; vimu(t0); vp(t0); vs1(t0); vs2(t0); vd(t0); }
                 { import bhazard.set._; rwports(t0, stagesIMul); }
      stop(t1); }

    def vidiv = {
      start(t0); { import iwindow.set._; vqu(t0); vp(t0); vs1(t0); vs2(t0); }
                 { import bhazard.set._; rports(t0); }
                 { import dhazard.set._; war_vd(t0); waw_vd(t0); }
      start(t1); { import iwindow.set._; vidu(t1); vd(t1); }
                 { import bhazard.set._; noports(t1); }
      stop(t2); }

    def vfma = {
      start(t0); { import iwindow.set._; vfmu(t0); vp(t0); vs1(t0); vs2(t0); vs3(t0); vd(t0); }
                 { import bhazard.set._; rwports(t0, stagesFMA); }
      stop(t1); }

    def vfdiv = {
      start(t0); { import iwindow.set._; vqu(t0); vp(t0); vs1(t0); vs2(t0); }
                 { import bhazard.set._; rports(t0); }
                 { import dhazard.set._; war_vd(t0); waw_vd(t0); }
      start(t1); { import iwindow.set._; vfdu(t1); vd(t1); }
                 { import bhazard.set._; noports(t1); }
      stop(t2); }

    def vfcmp = {
      start(t0); { import iwindow.set._; vfcu(t0); vp(t0); vs1(t0); vs2(t0); vd(t0); }
                 { import bhazard.set._; rwports(t0, stagesFCmp); }
      stop(t1); }

    def vfconv = {
      start(t0); { import iwindow.set._; vfvu(t0); vp(t0); vs1(t0); vd(t0); }
                 { import bhazard.set._; rwports(t0, stagesFConv); }
      stop(t1); }

    def vrpred = {
      start(t0); { import iwindow.set._; vqu(t0); vp(t0); }
                 { import bhazard.set._; rports(t0); }
      stop(t1); }

    def vrfirst = {
      start(t0); { import iwindow.set._; vqu(t0); vp(t0); vs1(t0); }
                 { import bhazard.set._; rports(t0); }
      stop(t1); }

    def vamo = {
      start(t0); { import iwindow.set._; vgu(t0); vp(t0); vs1(t0); }
                 { import bhazard.set._; rport_vs1(t0); }
      start(t1); { import iwindow.set._; vcu(t1); }
                 { import bhazard.set._; noports(t1); }
                 { import dhazard.set._; war_vd(t1); waw_vd(t1); }
      start(t2); { import iwindow.set._; vsu(t2); vp(t2); vs2_as_vs1(t2); }
                 { import bhazard.set._; rport_vs2(t2); }
                 { import dhazard.set._; raw(t2, t1); }
      start(t3); { import iwindow.set._; vlu(t3); vd(t3); }
                 { import bhazard.set._; noports(t3); }
      stop(t4); }

    def vldx = {
      start(t0); { import iwindow.set._; vgu(t0); vp(t0); vs2_as_vs1(t0); }
                 { import bhazard.set._; rport_vs2(t0); }
      start(t1); { import iwindow.set._; vcu(t1); }
                 { import bhazard.set._; noports(t1); }
                 { import dhazard.set._; war_vd(t1); waw_vd(t1); }
      start(t2); { import iwindow.set._; vlu(t2); vd(t2); }
                 { import bhazard.set._; noports(t2); }
      stop(t3); }

    def vstx = {
      start(t0); { import iwindow.set._; vgu(t0); vp(t0); vs2_as_vs1(t0); }
                 { import bhazard.set._; rport_vs2(t0); }
      start(t1); { import iwindow.set._; vcu(t1); }
                 { import bhazard.set._; noports(t1); }
      start(t2); { import iwindow.set._; vsu(t2); vp(t2); vd_as_vs1(t2); }
                 { import bhazard.set._; rport_vd(t2); }
                 { import dhazard.set._; raw(t2, t1); }
      stop(t3); }

    def vld = {
      start(t0); { import iwindow.set._; vpu(t0); vp(t0); }
                 { import bhazard.set._; noports(t0); }
      start(t1); { import iwindow.set._; vcu(t1); }
                 { import bhazard.set._; noports(t1); }
                 { import dhazard.set._; war_vd(t1); waw_vd(t1); }
      start(t2); { import iwindow.set._; vlu(t2); vd(t2); }
                 { import bhazard.set._; noports(t2); }
      stop(t3); }

    def vst = {
      start(t0); { import iwindow.set._; vpu(t0); vp(t0); }
                 { import bhazard.set._; noports(t0); }
      start(t1); { import iwindow.set._; vcu(t1); }
                 { import bhazard.set._; noports(t1); }
      start(t2); { import iwindow.set._; vsu(t2); vp(t2); vd_as_vs1(t2); }
                 { import bhazard.set._; rport_vd(t2); }
                 { import dhazard.set._; raw(t2, t1); }
      stop(t3); }

    def logic = {
      require(!retired) // must issue before retiring for dhazard bookkeeping

      when (io.op.fire()) {
        when (io.op.bits.active.vint) { vint }
        when (io.op.bits.active.vipred) { vipred }
        when (io.op.bits.active.vimul) { vimul }
        when (io.op.bits.active.vidiv) { vidiv }
        when (io.op.bits.active.vfma) { vfma }
        when (io.op.bits.active.vfdiv) { vfdiv }
        when (io.op.bits.active.vfcmp) { vfcmp }
        when (io.op.bits.active.vfconv) { vfconv }
        when (io.op.bits.active.vrpred) { vrpred }
        when (io.op.bits.active.vrfirst) { vrfirst }
        when (io.op.bits.active.vamo) { vamo }
        when (io.op.bits.active.vldx) { vldx }
        when (io.op.bits.active.vstx) { vstx }
        when (io.op.bits.active.vld) { vld }
        when (io.op.bits.active.vst) { vst }
      }
    }
  }

  iwindow.header
  dhazard.header

  issue.logic
  iwindow.logic
  dhazard.logic

  iwindow.footer

  iwindow.debug
}

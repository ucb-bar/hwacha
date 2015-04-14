package hwacha

import Chisel._
import Node._
import Constants._
import DataGating._
import uncore.constants.MemoryOpConstants._

abstract trait SeqParameters extends UsesHwachaParameters
{
  val nRPorts = 3
  val szRPorts = log2Down(nRPorts) + 1
  val maxWPortLatency = nRPorts + 1 +
    List(int_stages, imul_stages, fma_stages,
         fconv_stages, fcmp_stages).reduceLeft((x, y) => if (x > y) x else y)
  val szWPortLatency = log2Up(maxWPortLatency)
}

class SequencerIO extends ValidIO(new SequencerOp)

class Sequencer extends HwachaModule with SeqParameters with LaneParameters
{
  val io = new Bundle {
    val op = new VXUIssueOpIO().flip
    val seq = new SequencerIO
    val vmu = new LaneMemIO
    val ticker = new TickerIO().flip

    val dqla = new CounterLookAheadIO
    val dila = new CounterLookAheadIO
    val dfla = new CounterLookAheadIO
    val lla = new CounterLookAheadIO
    val sla = new BRQLookAheadIO

    val lack = new LaneAckIO().flip
    val dack = new DCCAckIO().flip

    val pending = Bool(OUTPUT)
    val debug = new Bundle {
      val valid = Vec.fill(nseq){Bool(OUTPUT)}
      val vlen = Vec.fill(nseq){UInt(width=szvlen)}.asOutput
      val e = Vec.fill(nseq){new SeqEntry}.asOutput
      val head = UInt(OUTPUT, log2Up(nseq))
      val tail = UInt(OUTPUT, log2Up(nseq))
      val full = Bool(OUTPUT)
      val dhazard = Vec.fill(nseq){Bool(OUTPUT)}
    }
  }

  class BuildSequencer
  {
    require(isPow2(nseq))

    ///////////////////////////////////////////////////////////////////////////
    // state

    val valid = Vec.fill(nseq){Reg(init=Bool(false))}
    val vlen = Vec.fill(nseq){Reg(UInt(width=szvlen))}
    val e = Vec.fill(nseq){Reg(new SeqEntry)}

    val full = Reg(init = Bool(false))
    val head = Reg(init = UInt(0, log2Up(nseq)))
    val tail = Reg(init = UInt(0, log2Up(nseq)))

    ///////////////////////////////////////////////////////////////////////////
    // wires

    val h1 = head + UInt(1)
    val t0 = tail
    val t1 = tail + UInt(1)
    val t2 = tail + UInt(2)
    val t3 = tail + UInt(3)
    val t4 = tail + UInt(4)

    // clock gate ports and bypass ports
    val update_head = Bool()
    val update_tail = Bool()
    val next_head = UInt(width = log2Up(nseq))
    val next_tail = UInt(width = log2Up(nseq))

    val update_haz = Vec.fill(nseq){Bool()}
    val next_raw = Vec.fill(nseq){Vec.fill(nseq){Bool()}}
    val next_war = Vec.fill(nseq){Vec.fill(nseq){Bool()}}
    val next_waw = Vec.fill(nseq){Vec.fill(nseq){Bool()}}

    val vidu_ready = Bool()
    val vfdu_ready = Bool()
    val vcu_ready = Bool()
    val vlu_ready = Bool()

    ///////////////////////////////////////////////////////////////////////////
    // header

    def header = {
      update_head := Bool(false)
      update_tail := Bool(false)
      next_head := head
      next_tail := tail

      for (i <- 0 until nseq) {
        update_haz(i) := Bool(false)
        for (j <- 0 until nseq) {
          next_raw(i)(j) := e(i).raw(j)
          next_war(i)(j) := e(i).war(j)
          next_waw(i)(j) := e(i).waw(j)
        }
      }

      vidu_ready := Bool(false)
      vfdu_ready := Bool(false)
      vcu_ready := Bool(false)
      vlu_ready := Bool(false)
    }

    ///////////////////////////////////////////////////////////////////////////
    // helpers

    val count = Cat(full && head === tail, tail - head)
    val empty = UInt(nseq) - count

    def ready = {
      val a = io.op.bits.active
      (a.vint || a.vimul || a.vfma || a.vfcmp || a.vfconv) && (empty >= UInt(1)) ||
      (a.vidiv || a.vfdiv || a.vld || a.vst) && (empty >= UInt(2)) ||
      (a.vldx || a.vstx) && (empty >= UInt(3)) ||
      (a.vamo) && (empty >= UInt(4))
    }

    def set_head(n: UInt) = {
      update_head := Bool(true)
      next_head := n
    }
    def set_tail(n: UInt) = {
      update_tail := Bool(true)
      next_tail := n
    }

    def set_valid(n: UInt) = {
      valid(n) := Bool(true)
    }
    def clear_valid(n: UInt) = {
      valid(n) := Bool(false)
    }

    def set_active(n: UInt, afn: VFU=>Bool, fn: IssueOp=>Bits) = {
      afn(e(n).active) := Bool(true)
      e(n).fn.union := fn(io.op.bits)
    }
    def clear_all_active(n: UInt) = {
      e(n).active := e(0).active.clone().fromBits(Bits(0))
    }

    def set_entry(n: UInt) = {
      set_valid(n)
      vlen(n) := io.op.bits.vlen
      update_haz(n) := Bool(true)
      e(n).age := UInt(0)
    }
    def clear_entry(n: UInt) = {
      clear_valid(n)
      e(n).reg.vs1.valid := Bool(false)
      e(n).reg.vs2.valid := Bool(false)
      e(n).reg.vs3.valid := Bool(false)
      e(n).reg.vd.valid := Bool(false)
      e(n).reg.vs1.scalar := Bool(false)
      e(n).reg.vs2.scalar := Bool(false)
      e(n).reg.vs3.scalar := Bool(false)
      e(n).reg.vd.scalar := Bool(false)
      e(n).base.vs1.valid := Bool(false)
      e(n).base.vs2.valid := Bool(false)
      e(n).base.vs3.valid := Bool(false)
      e(n).base.vd.valid := Bool(false)
      e(n).base.vs1.scalar := Bool(false)
      e(n).base.vs2.scalar := Bool(false)
      e(n).base.vs3.scalar := Bool(false)
      e(n).base.vd.scalar := Bool(false)
      for (i <- 0 until nseq) {
        e(n).raw(i) := Bool(false)
        e(n).war(i) := Bool(false)
        e(n).waw(i) := Bool(false)
      }
    }

    def retire_entry(n: UInt) = {
      clear_entry(n)
      for (i <- 0 until nseq) {
        update_haz(UInt(i)) := Bool(true)
        clear_raw_haz(UInt(i), n)
        clear_war_haz(UInt(i), n)
        clear_waw_haz(UInt(i), n)
      }
    }

    def update_counter = {
      when (update_head) { head := next_head }
      when (update_tail) { tail := next_tail }
      when (update_head && !update_tail) {
        full := Bool(false)
      }
      when (update_tail) {
        full := next_head === next_tail
      }
    }

    ///////////////////////////////////////////////////////////////////////////
    // data hazard checking helpers

    val reg_vs1 = (reg: DecodedRegisters) => reg.vs1
    val reg_vs2 = (reg: DecodedRegisters) => reg.vs2
    val reg_vs3 = (reg: DecodedRegisters) => reg.vs3
    val reg_vd  = (reg: DecodedRegisters) => reg.vd

    def set_raw_haz(n: UInt, o: UInt) = { next_raw(n)(o) := Bool(true) }
    def set_war_haz(n: UInt, o: UInt) = { next_war(n)(o) := Bool(true) }
    def set_waw_haz(n: UInt, o: UInt) = { next_waw(n)(o) := Bool(true) }
    def clear_raw_haz(n: UInt, o: UInt) = { next_raw(n)(o) := Bool(false) }
    def clear_war_haz(n: UInt, o: UInt) = { next_war(n)(o) := Bool(false) }
    def clear_waw_haz(n: UInt, o: UInt) = { next_waw(n)(o) := Bool(false) }

    def issue_base_eq(ifn: DecodedRegisters=>RegInfo, sfn: DecodedRegisters=>RegInfo) =
      Vec((0 until nseq).map{ i =>
        valid(i) && sfn(e(i).base).valid && !sfn(e(i).base).scalar &&
        ifn(io.op.bits.reg).valid && !ifn(io.op.bits.reg).scalar &&
        sfn(e(i).base).id === ifn(io.op.bits.reg).id })
    val ivs1_evd_eq = issue_base_eq(reg_vs1, reg_vd)
    val ivs2_evd_eq = issue_base_eq(reg_vs2, reg_vd)
    val ivs3_evd_eq = issue_base_eq(reg_vs3, reg_vd)
    val ivd_evs1_eq = issue_base_eq(reg_vd, reg_vs1)
    val ivd_evs2_eq = issue_base_eq(reg_vd, reg_vs2)
    val ivd_evs3_eq = issue_base_eq(reg_vd, reg_vs3)
    val ivd_evd_eq  = issue_base_eq(reg_vd, reg_vd)

    def set_raw_hazs(n: UInt, eq: Vec[Bool]) = {
      for (i <- 0 until nseq) {
        when (eq(i)) {
          set_raw_haz(n, UInt(i))
        }
      }
    }
    def set_war_hazs(n: UInt) = {
      for (i <- 0 until nseq) {
        when (ivd_evs1_eq(i) || ivd_evs2_eq(i) || ivd_evs3_eq(i)) {
          set_war_haz(n, UInt(i))
        }
      }
    }
    def set_waw_hazs(n: UInt) = {
      for (i <- 0 until nseq) {
        when (ivd_evd_eq(i)) {
          set_waw_haz(n, UInt(i))
        }
      }
    }
    def set_raw_hazs_vs1(n: UInt) = set_raw_hazs(n, ivs1_evd_eq)
    def set_raw_hazs_vs2(n: UInt) = set_raw_hazs(n, ivs2_evd_eq)
    def set_raw_hazs_vs3(n: UInt) = set_raw_hazs(n, ivs3_evd_eq)
    def set_raw_hazs_vd(n: UInt) = set_raw_hazs(n, ivd_evd_eq)
    def set_war_hazs_vd(n: UInt) = set_war_hazs(n)
    def set_waw_hazs_vd(n: UInt) = set_waw_hazs(n)

    val fn_identity = (d: IssueOp) => d.fn.union
    val fn_vqu = (d: IssueOp) => {
      assert(d.active.vidiv || d.active.vfdiv, "vqu should only be issued for idiv/fdiv")
      Mux(d.active.vfdiv && d.fn.vfdu().op_is(FD_SQRT), Bits("b10"), Bits("b11"))
    }

    def set_viu(n: UInt) = set_active(n, (a: VFU) => a.viu, fn_identity)
    def set_vimu(n: UInt) = set_active(n, (a: VFU) => a.vimu, fn_identity)
    def set_vidu(n: UInt) = set_active(n, (a: VFU) => a.vidu, fn_identity)
    def set_vfmu(n: UInt) = set_active(n, (a: VFU) => a.vfmu, fn_identity)
    def set_vfdu(n: UInt) = set_active(n, (a: VFU) => a.vfdu, fn_identity)
    def set_vfcu(n: UInt) = set_active(n, (a: VFU) => a.vfcu, fn_identity)
    def set_vfvu(n: UInt) = set_active(n, (a: VFU) => a.vfvu, fn_identity)
    def set_vgu(n: UInt) = set_active(n, (a: VFU) => a.vgu, fn_identity)
    def set_vcu(n: UInt) = set_active(n, (a: VFU) => a.vcu, fn_identity)
    def set_vlu(n: UInt) = set_active(n, (a: VFU) => a.vlu, fn_identity)
    def set_vsu(n: UInt) = set_active(n, (a: VFU) => a.vsu, fn_identity)
    def set_vqu(n: UInt) = set_active(n, (a: VFU) => a.vqu, fn_vqu)

    def set_vs(n: UInt, vsfn: DecodedRegisters=>RegInfo, ssfn: ScalarRegisters=>Bits) = {
      when (vsfn(io.op.bits.reg).valid) {
        vsfn(e(n).reg) := vsfn(io.op.bits.reg)
        vsfn(e(n).base) := vsfn(io.op.bits.reg)
        when (vsfn(io.op.bits.reg).scalar) {
          ssfn(e(n).sreg) := ssfn(io.op.bits.sreg)
        }
      }
    }
    def set_vs1(n: UInt) = {
      set_vs(n, reg_vs1, (sreg: ScalarRegisters) => sreg.ss1)
      set_raw_hazs_vs1(n)
    }
    def set_vs2(n: UInt) = {
      set_vs(n, reg_vs2, (sreg: ScalarRegisters) => sreg.ss2)
      set_raw_hazs_vs2(n)
    }
    def set_vs3(n: UInt) = {
      set_vs(n, reg_vs3, (sreg: ScalarRegisters) => sreg.ss3)
      set_raw_hazs_vs3(n)
    }
    def set_vd_as_vs1(n: UInt) = {
      assert(!io.op.bits.reg.vd.valid || !io.op.bits.reg.vd.scalar, "set_vd_as_vs1: vd should always be vector")
      when (io.op.bits.reg.vd.valid) {
        e(n).reg.vs1 := io.op.bits.reg.vd
        e(n).base.vs1 := io.op.bits.reg.vd
      }
      set_raw_hazs_vd(n)
    }
    def set_vd(n: UInt) = {
      assert(!io.op.bits.reg.vd.valid || !io.op.bits.reg.vd.scalar, "set_vd: vd should always be vector")
      when (io.op.bits.reg.vd.valid) {
        e(n).reg.vd := io.op.bits.reg.vd
        e(n).base.vd := io.op.bits.reg.vd
      }
      set_war_hazs_vd(n)
      set_waw_hazs_vd(n)
    }

    ///////////////////////////////////////////////////////////////////////////
    // bank hazard checking helpers

    def nports_list(fns: DecodedRegisters=>RegInfo*) =
      PopCount(fns.map{ fn => fn(io.op.bits.reg).valid && !fn(io.op.bits.reg).scalar })
    val nrports = nports_list(reg_vs1, reg_vs2, reg_vs3)
    val nrport_vs1 = nports_list(reg_vs1)
    val nrport_vs2 = nports_list(reg_vs2)
    val nrport_vd = nports_list(reg_vd)

    def set_ports(n: UInt, rports: UInt, wport: UInt) = {
      e(n).rports := rports
      e(n).wport := wport
    }
    def set_noports(n: UInt) = set_ports(n, UInt(0), UInt(0))
    def set_rport_vs1(n: UInt) = set_ports(n, nrport_vs1, UInt(0))
    def set_rport_vs2(n: UInt) = set_ports(n, nrport_vs2, UInt(0))
    def set_rport_vd(n: UInt) = set_ports(n, nrport_vd, UInt(0))
    def set_rports(n: UInt) = set_ports(n, nrports, UInt(0))
    def set_rwports(n: UInt, latency: Int) = set_ports(n, nrports, nrports + UInt(latency+1))

    ///////////////////////////////////////////////////////////////////////////
    // issue -> sequencer

    def issue_vint = {
      set_entry(t0); set_viu(t0); set_vs1(t0); set_vs2(t0); set_vd(t0)
                      set_rwports(t0, int_stages)
      set_tail(t1)
    }
    def issue_vimul = {
      set_entry(t0); set_vimu(t0); set_vs1(t0); set_vs2(t0); set_vd(t0)
                      set_rwports(t0, imul_stages)
      set_tail(t1)
    }
    def issue_vidiv = {
      set_entry(t0); set_vqu(t0); set_vs1(t0); set_vs2(t0)
                      set_rports(t0); set_war_hazs_vd(t0); set_waw_hazs_vd(t0)
      set_entry(t1); set_vidu(t1); set_vd(t1)
                      set_noports(t1)
      set_tail(t2)
    }
    def issue_vfma = {
      set_entry(t0); set_vfmu(t0); set_vs1(t0); set_vs2(t0); set_vs3(t0); set_vd(t0)
                      set_rwports(t0, fma_stages)
      set_tail(t1)
    }
    def issue_vfdiv = {
      set_entry(t0); set_vqu(t0); set_vs1(t0); set_vs2(t0)
                      set_rports(t0); set_war_hazs_vd(t0); set_waw_hazs_vd(t0)
      set_entry(t1); set_vfdu(t1); set_vd(t1)
                      set_noports(t1)
      set_tail(t2)
    }
    def issue_vfcmp = {
      set_entry(t0); set_vfcu(t0); set_vs1(t0); set_vs2(t0); set_vd(t0)
                      set_rwports(t0, fcmp_stages)
      set_tail(t1)
    }
    def issue_vfconv = {
      set_entry(t0); set_vfvu(t0); set_vs1(t0); set_vd(t0)
                      set_rwports(t0, fconv_stages)
      set_tail(t1)
    }
    def issue_vamo = {
      set_entry(t0); set_vgu(t0); set_vs1(t0)
                      set_rport_vs1(t0)
      set_entry(t1); set_vcu(t1)
                      set_noports(t1); set_war_hazs_vd(t1); set_waw_hazs_vd(t1)
      set_entry(t2); set_vsu(t2); set_vs2(t2)
                      set_rport_vs2(t2); set_raw_haz(t2, t1)
      set_entry(t3); set_vlu(t3); set_vd(t3)
                      set_noports(t3)
      set_tail(t4)
    }
    def issue_vldx = {
      set_entry(t0); set_vgu(t0); set_vs2(t0)
                      set_rport_vs2(t0)
      set_entry(t1); set_vcu(t1)
                      set_noports(t1); set_war_hazs_vd(t1); set_waw_hazs_vd(t1)
      set_entry(t2); set_vlu(t2); set_vd(t2)
                      set_noports(t2)
      set_tail(t3)
    }
    def issue_vstx = {
      set_entry(t0); set_vgu(t0); set_vs2(t0)
                      set_rport_vs2(t0)
      set_entry(t1); set_vcu(t1)
                      set_noports(t1)
      set_entry(t2); set_vsu(t2); set_vd_as_vs1(t2)
                      set_rport_vd(t2); set_raw_haz(t2, t1)
      set_tail(t3)
    }
    def issue_vld = {
      set_entry(t0); set_vcu(t0)
                      set_noports(t0); set_war_hazs_vd(t0); set_waw_hazs_vd(t0)
      set_entry(t1); set_vlu(t1); set_vd(t1)
                      set_noports(t1)
      set_tail(t2)
    }
    def issue_vst = {
      set_entry(t0); set_vcu(t0)
                      set_noports(t0)
      set_entry(t1); set_vsu(t1); set_vd_as_vs1(t1)
                      set_rport_vd(t1); set_raw_haz(t1, t0)
      set_tail(t2)
    }

    ///////////////////////////////////////////////////////////////////////////
    // data hazard checking

    val vlen_matrix =
      Vec((0 until nseq).map { r =>
        Vec((0 until nseq).map { c =>
          if (r != c) vlen(UInt(r)) > vlen(UInt(c))
          else Bool(false) }) })

    def wport_matrix(fn: DecodedRegisters=>RegInfo) =
      Vec((0 until nseq).map { r =>
        Vec((0 until maxWPortLatency).map { l =>
          io.ticker.sram.write(l).valid && fn(e(r).reg).valid && !fn(e(r).reg).scalar &&
          io.ticker.sram.write(l).bits.addr === fn(e(r).reg).id }) })
    val wport_vs1_matrix = wport_matrix(reg_vs1)
    val wport_vs2_matrix = wport_matrix(reg_vs2)
    val wport_vs3_matrix = wport_matrix(reg_vs3)
    val wport_vd_matrix = wport_matrix(reg_vd)

    def wport_lookup(row: Vec[Bool], level: UInt) =
      Vec(row.zipWithIndex.map { case (r, i) => r && UInt(i) > level })

    val dhazard_raw =
      (0 until nseq).map { r =>
        (e(r).raw.toBits & ~vlen_matrix(r).toBits).orR ||
        wport_vs1_matrix(r).toBits.orR ||
        wport_vs2_matrix(r).toBits.orR ||
        wport_vs3_matrix(r).toBits.orR }
    val dhazard_war =
      (0 until nseq).map { r =>
        (e(r).war.toBits & ~vlen_matrix(r).toBits).orR }
    val dhazard_waw =
      (0 until nseq).map { r =>
        (e(r).waw.toBits & ~vlen_matrix(r).toBits).orR ||
        wport_lookup(wport_vd_matrix(r), e(r).wport).toBits.orR }
    val dhazard =
      (0 until nseq).map { r =>
        dhazard_raw(r) || dhazard_war(r) || dhazard_waw(r) }

    ///////////////////////////////////////////////////////////////////////////
    // bank hazard checking

    val bhazard =
      (0 until nseq).map { r => Bool(false) }

    ///////////////////////////////////////////////////////////////////////////
    // strucutral hazard checking

    val shazard =
      (0 until nseq).map { r => Bool(false) }

    ///////////////////////////////////////////////////////////////////////////
    // schedule

    def find_first(fn: Int=>Bool) = {
      val internal = Vec.fill(2*nseq){Bool()}
      for (i <- 0 until nseq) {
        internal(i+nseq) := valid(i) && fn(i)
        internal(i) := internal(i+nseq) && (UInt(i) >= head)
      }
      val priority_oh = PriorityEncoderOH(internal)
      val out = Vec.fill(nseq){Bool()}
      for (i <- 0 until nseq) {
        out(i) := priority_oh(i) | priority_oh(i+nseq)
      }
      out
    }

    def nohazards(i: Int) = !dhazard(i) && !bhazard(i) && !shazard(i)

    val vidu_sched = find_first((i: Int) => e(i).active.vidu && nohazards(i))
    val vfdu_sched = find_first((i: Int) => e(i).active.vfdu && nohazards(i))
    val vcu_sched = find_first((i: Int) => e(i).active.vcu && nohazards(i))
    val vlu_sched = find_first((i: Int) => e(i).active.vlu && nohazards(i))
    val vgu_first = find_first((i: Int) => e(i).active.vgu)
    val vsu_first = find_first((i: Int) => e(i).active.vsu)
    val vqu_first = find_first((i: Int) => e(i).active.vqu)
    val exp_sched = {
      val consider = (i: Int) => nohazards(i) && (
        e(i).active.viu || e(i).active.vimu ||
        e(i).active.vfmu || e(i).active.vfcu || e(i).active.vfvu ||
        e(i).active.vgu && vgu_first(i) ||
        e(i).active.vsu && vsu_first(i) && io.sla.available ||
        e(i).active.vqu && vqu_first(i) && io.dqla.available)
      val first_sched = find_first((i: Int) => consider(i) && e(i).age === UInt(0))
      val second_sched = find_first((i: Int) => consider(i))
      val sel = first_sched.reduce(_ || _)
      Vec(first_sched zip second_sched map { case (f, s) => Mux(sel, f, s) })
    }

    def fire_vidu(n: Int) = vidu_sched(n) && vidu_ready
    def fire_vfdu(n: Int) = vfdu_sched(n) && vfdu_ready
    def fire_vcu(n: Int) = vcu_sched(n) && vcu_ready
    def fire_vlu(n: Int) = vlu_sched(n) && vlu_ready
    def fire_exp(n: Int) = exp_sched(n)
    def fire(n: Int) = fire_vidu(n) || fire_vfdu(n) || fire_vcu(n) || fire_vlu(n) || fire_exp(n)

    def active_vmu(n: Int) = e(n).active.vgu || e(n).active.vcu || e(n).active.vlu || e(n).active.vsu

    def stripfn(vl: UInt, vmu: Bool, fn: VFn) = {
      val max_strip = Mux(vmu && fn.vmu().mt === MT_B, UInt(nBatch << 1), UInt(nBatch))
      Mux(vl > max_strip, max_strip, vl)
    }

    def valfn(sched: Vec[Bool]) = sched.reduce(_ || _)

    def vlenfn(sched: Vec[Bool]) =
      sched.zip(vlen).map({ case (s, vl) => dgate(s, vl) }).reduce(_ | _)

    def readfn[T <: Data](sched: Vec[Bool], rfn: SeqEntry=>T) =
      rfn(e(0)).clone.fromBits(sched.zip(e).map({ case (s, e) => dgate(s, rfn(e).toBits) }).reduce(_ | _))

    val vidu_val = valfn(vidu_sched)
    val vidu_vlen = vlenfn(vidu_sched)
    val vidu_fn = readfn(vidu_sched, (e: SeqEntry) => e.fn)
    val vidu_strip = stripfn(vidu_vlen, Bool(false), vidu_fn)

    val vfdu_val = valfn(vfdu_sched)
    val vfdu_vlen = vlenfn(vfdu_sched)
    val vfdu_fn = readfn(vfdu_sched, (e: SeqEntry) => e.fn)
    val vfdu_strip = stripfn(vfdu_vlen, Bool(false), vfdu_fn)

    val vcu_val = valfn(vcu_sched)
    val vcu_vlen = vlenfn(vcu_sched)
    val vcu_fn = readfn(vcu_sched, (e: SeqEntry) => e.fn)
    val vcu_strip = stripfn(vcu_vlen, Bool(true), vcu_fn)

    val vlu_val = valfn(vlu_sched)
    val vlu_vlen = vlenfn(vlu_sched)
    val vlu_fn = readfn(vlu_sched, (e: SeqEntry) => e.fn)
    val vlu_strip = stripfn(vlu_vlen, Bool(true), vlu_fn)

    val vsu_vlen = vlenfn(vsu_first)
    val vsu_fn = readfn(vsu_first, (e: SeqEntry) => e.fn)
    val vsu_strip = stripfn(vsu_vlen, Bool(true), vsu_fn)

    val exp_val = valfn(exp_sched)
    val exp_vlen = vlenfn(exp_sched)
    val exp_seq = {
      val out = new SequencerOp
      out.fn := readfn(exp_sched, (e: SeqEntry) => e.fn)
      out.reg := readfn(exp_sched, (e: SeqEntry) => e.reg)
      out.sreg := readfn(exp_sched, (e: SeqEntry) => e.sreg)
      out.active := readfn(exp_sched, (e: SeqEntry) => e.active)
      out.rports := readfn(exp_sched, (e: SeqEntry) => e.rports)
      out.wport := readfn(exp_sched, (e: SeqEntry) => e.wport)
      out.strip := stripfn(exp_vlen, Bool(false), out.fn)
      out
    }

    ///////////////////////////////////////////////////////////////////////////
    // footer

    def footer = {
      val retire = Vec.fill(nseq){Bool()}

      for (i <- 0 until nseq) {
        val strip = stripfn(vlen(i), active_vmu(i), e(i).fn)
        when (valid(i)) {
          when (fire(i)) {
            vlen(i) := vlen(i) - strip
            when (vlen(i) === strip) {
              clear_all_active(UInt(i))
            }
          }
          when (e(i).age.orR) {
            e(i).age := e(i).age - UInt(1)
          }
          when (fire_exp(i)) {
            e(i).age := UInt(nbanks-1)
          }
        }
      }

      when (valid(head) && vlen(head) === UInt(0)) {
        retire_entry(head)
        set_head(h1)
      }

      update_counter

      for (i <- 0 until nseq) {
        when (update_haz(i)) {
          e(i).raw := next_raw(i)
          e(i).war := next_war(i)
          e(i).waw := next_waw(i)
        }
      }

      io.debug.valid := valid
      io.debug.vlen := vlen
      io.debug.e := e
      io.debug.head := head
      io.debug.tail := tail
      io.debug.full := full
      io.debug.dhazard := dhazard

      when (reset) {
        for (i <- 0 until nseq) {
          clear_entry(UInt(i))
          clear_all_active(UInt(i))
        }
      }
    }
  }

  val seq = new BuildSequencer

  seq.header

  io.op.ready := seq.ready

  when (io.op.fire()) {
    when (io.op.bits.active.vint) { seq.issue_vint }
    when (io.op.bits.active.vimul) { seq.issue_vimul }
    when (io.op.bits.active.vidiv) { seq.issue_vidiv }
    when (io.op.bits.active.vfma) { seq.issue_vfma }
    when (io.op.bits.active.vfdiv) { seq.issue_vfdiv }
    when (io.op.bits.active.vfcmp) { seq.issue_vfcmp }
    when (io.op.bits.active.vfconv) { seq.issue_vfconv }
    when (io.op.bits.active.vamo) { seq.issue_vamo }
    when (io.op.bits.active.vldx) { seq.issue_vldx }
    when (io.op.bits.active.vstx) { seq.issue_vstx }
    when (io.op.bits.active.vld) { seq.issue_vld }
    when (io.op.bits.active.vst) { seq.issue_vst }
  }

  io.seq.valid := seq.exp_val
  io.seq.bits := seq.exp_seq

  io.dqla.cnt := seq.exp_seq.strip
  io.dqla.reserve := seq.exp_val && seq.exp_seq.active.vqu

  io.dila.cnt := seq.vidu_strip
  io.dila.reserve := seq.vidu_val && io.dila.available
  seq.vidu_ready := io.dila.available

  io.dfla.cnt := seq.vfdu_strip
  io.dfla.reserve := seq.vfdu_val && io.dfla.available
  seq.vfdu_ready := io.dfla.available

  io.vmu.la.vala.reserve := Bool(false) // FIXME

  io.vmu.la.pala.cnt := seq.vcu_strip
  io.vmu.la.pala.reserve := seq.vcu_val && io.vmu.la.pala.available
  seq.vcu_ready := io.vmu.la.pala.available

  io.lla.cnt := seq.vlu_strip
  io.lla.reserve := seq.vlu_val && io.lla.available
  seq.vlu_ready := io.lla.available

  io.sla.reserve := seq.exp_val && seq.exp_seq.active.vsu
  io.sla.mask :=
    MuxLookup(seq.vsu_strip, Bits(0), Seq(
      UInt(1) -> Bits("b0001"),
      UInt(2) -> Bits("b0001"),
      UInt(3) -> Bits("b0011"),
      UInt(4) -> Bits("b0011"),
      UInt(5) -> Bits("b0111"),
      UInt(6) -> Bits("b0111"),
      UInt(7) -> Bits("b1111"),
      UInt(8) -> Bits("b1111")))

  io.pending := seq.valid.reduce(_ || _)

  seq.footer
}

package hwacha

import Chisel._
import cde.Parameters
import rocket.ALU._
import ScalarFPUDecode._
import HardFloatHelper._

class CtrlDpathIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val stalld = Bool(OUTPUT)
  val killd = Bool(OUTPUT)
  val stallx = Bool(OUTPUT)
  val killx = Bool(OUTPUT)
  val stallw = Bool(OUTPUT)

  val id_inst = Bits(INPUT, 64)
  val fire_vf = Bool(OUTPUT)

  val id_ctrl = new IntCtrlSigs().asOutput()

  // f/d signals
  val sren = Vec.fill(3)(Bool(OUTPUT))
  val aren = Vec.fill(2)(Bool(OUTPUT))

  // x signals
  val ex_valid = Bool(OUTPUT)
  val ex_ctrl = new IntCtrlSigs().asOutput()
  val ex_br_taken = Bool(OUTPUT)
  val ex_bypass = Vec.fill(3){Bool(OUTPUT)}

  // w signals
  val wb_valid = Bool(OUTPUT)
  val wb_ctrl = new IntCtrlSigs().asOutput()
  val wb_wen = Bool(OUTPUT)
  val wb_ll_valid = Bool(OUTPUT)
  val wb_ll_waddr = UInt(OUTPUT, bSRegs)
  val wb_ll_wdata = UInt(OUTPUT, regLen)
  val swrite = Bool(OUTPUT)
  val awrite = Bool(OUTPUT)

  val vf_active = Bool(OUTPUT)
}

class ScalarRFWritePort(implicit p: Parameters) extends HwachaBundle()(p) {
  val addr = UInt(width = bSRegs)
  val data = UInt(width = regLen)
}

class ScalarCtrl(resetSignal: Bool = null)(implicit p: Parameters) extends HwachaModule(_reset = resetSignal)(p)
  with VMUParameters {
  import Commands._

  val io = new Bundle {
    val dpath = new CtrlDpathIO

    val cfg = new HwachaConfigIO().flip

    val cmdq = new CMDQIO().flip
    val imem = new FrontendIO
    val vxu = Decoupled(new IssueOpML)
    val vmu = Decoupled(new VMUOpML)
    val fpu = new Bundle {
      val req = Decoupled(new HwachaFPInput)
      val resp = Decoupled(new rocket.FPResult()).flip
    }
    val smu = new SMUIO
    val lreq = new CounterLookAheadIO
    val sreq = new CounterLookAheadIO
    val mocheck = new MOCheck().asInput
    val red = new ReduceResultIO().flip

    val busy_mseq = Bool(INPUT)
    val vf_active = Bool(OUTPUT)
    val sboard_marked = Bool(OUTPUT)
  }

  // STATE

  // Copied from rocekt datapath
  class Scoreboard(n: Int) {
    def set(en: Bool, addr: UInt): Unit = update(en, _next | mask(en, addr))
    def clear(en: Bool, addr: UInt): Unit = update(en, _next & ~mask(en, addr))
    def read(addr: UInt): Bool = r(addr)
    def readBypassed(addr: UInt): Bool = _next(addr)

    private val r = Reg(init=Bits(0, n))
    private var _next = r
    private var ens = Bool(false)
    private def mask(en: Bool, addr: UInt) = Mux(en, UInt(1) << addr, UInt(0))
    private def update(en: Bool, update: UInt) = {
      _next = update
      ens = ens || en
      when (ens) { r := _next }
    }
  }
  val sboard = new Scoreboard(nSRegs)

  val vf_active = Reg(init=Bool(false))
  val vl = Vec.fill(nLanes){Reg(new VLenEntry)}

  val ex_ctrl = Reg(new IntCtrlSigs)
  val wb_ctrl = Reg(new IntCtrlSigs)

  val ex_reg_valid = Reg(Bool())
  val wb_reg_valid = Reg(Bool())

  // ll state
  val pending_fpu = Reg(init=Bool(false))
  val pending_fpu_reg = Reg(init=UInt(width=log2Up(nSRegs)))
  val pending_fpu_typ = Reg(init=Bits(width=2))
  val pending_fpu_fn = Reg(new rocket.FPUCtrlSigs())
  val pending_smu = Reg(init=Bool(false))
  val pending_cbranch = Reg(init=Bool(false))

  io.vf_active := vf_active
  io.dpath.vf_active := vf_active
  io.sboard_marked := (0 until nSRegs).map(i => sboard.read(UInt(i))).reduce(_||_)

  // decode cmdq
  val decode_vmss    = io.cmdq.cmd.bits === CMD_VMSS
  val decode_vmsa    = io.cmdq.cmd.bits === CMD_VMSA
  val decode_vf      = io.cmdq.cmd.bits === CMD_VF
  val decode_vft     = io.cmdq.cmd.bits === CMD_VFT
  val decode_vsetvl  = io.cmdq.cmd.bits === CMD_VSETVL
  val decode_vsetcfg = io.cmdq.cmd.bits === CMD_VSETCFG

  val deq_imm = decode_vmss || decode_vmsa || decode_vf || decode_vft || decode_vsetvl || decode_vsetcfg
  val deq_rd  = decode_vmss || decode_vmsa 

  val mask_imm_valid = !deq_imm || io.cmdq.imm.valid
  val mask_rd_valid  = !deq_rd  || io.cmdq.rd.valid

  // TODO: we could fire all cmd but vf* without busy_mseq being clear
  def fire_cmdq(exclude: Bool, include: Bool*) = {
  val rvs = Seq(
      !vf_active, !wb_reg_valid, !io.busy_mseq, io.cmdq.cmd.valid, !pending_smu, !pending_fpu,
      mask_imm_valid, mask_rd_valid) 
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.dpath.swrite := fire_cmdq(null,decode_vmss) 
  io.dpath.awrite := fire_cmdq(null,decode_vmsa) 

  io.cmdq.cmd.ready := fire_cmdq(io.cmdq.cmd.valid)
  io.cmdq.imm.ready := fire_cmdq(mask_imm_valid, deq_imm)
  io.cmdq.rd.ready  := fire_cmdq(mask_rd_valid, deq_rd)

  // FETCH
  io.imem.req.valid := io.dpath.fire_vf || io.dpath.ex_br_taken
  io.imem.active := vf_active
  io.imem.invalidate := Bool(false) // TODO: flush cache/tlb on vfence
  io.imem.resp.ready := !io.dpath.stalld

  // DECODE
  val decode_table = ScalarDecode.table ++ VectorMemoryDecode.table ++ VectorArithmeticDecode.table
  val id_ctrl = new IntCtrlSigs().decode(io.dpath.id_inst, decode_table)
  io.dpath.id_ctrl := id_ctrl

  when (fire_cmdq(null, decode_vsetcfg)) {
    (0 until nLanes) map { case i =>
      vl(i).active := Bool(false)
      vl(i).vlen := UInt(0)
    }
  }
  when (fire_cmdq(null, decode_vsetvl)) {
    val lgStrip = io.cfg.lstride
    val lgLane = log2Floor(nLanes)
    val nStrip = UInt(1) << lgStrip
    val vlen_ml = io.cmdq.imm.bits
    val vlen_base = (vlen_ml >> (UInt(0, lgStrip.getWidth+1) + UInt(lgLane) + lgStrip)) << lgStrip
    val vlen_lane = (vlen_ml >> lgStrip)(lgLane-1, 0)
    val vlen_strip = vlen_ml & (nStrip - UInt(1))
    (0 until nLanes) map { case i =>
      val vlen_fringe =
        Mux(vlen_lane > UInt(i), nStrip,
          Mux(vlen_lane === UInt(i), vlen_strip, UInt(0)))
      val vlen = if (nLanes == 1) vlen_ml else vlen_base + vlen_fringe
      vl(i).active := vlen.orR
      vl(i).vlen := vlen
    }
  }

  io.dpath.fire_vf := Bool(false)
  when (fire_cmdq(null, decode_vf)) {
    vf_active := Bool(true)
    io.dpath.fire_vf := Bool(true)
  }
  when (!io.dpath.killd && id_ctrl.decode_stop) {
    vf_active := Bool(false)
  }

  val sren1 = id_ctrl.vs1_val && id_ctrl.vs1_type === REG_SHR
  val sren2 = id_ctrl.vs2_val && id_ctrl.vs2_type === REG_SHR
  val sren3 = id_ctrl.vs3_val && id_ctrl.vs3_type === REG_SHR
  val aren1 = id_ctrl.vs1_val && id_ctrl.vs1_type === REG_ADDR
  val aren2 = id_ctrl.vs2_val && id_ctrl.vs2_type === REG_ADDR
  io.dpath.sren(0) := sren1
  io.dpath.sren(1) := sren2
  io.dpath.sren(2) := sren3
  io.dpath.aren(0) := aren1
  io.dpath.aren(1) := aren2

  val id_scalar_dest = id_ctrl.vd_val  && (id_ctrl.vd_type  === REG_SHR || id_ctrl.vd_type  === REG_ADDR)
  val id_scalar_src1 = id_ctrl.vs1_val && (id_ctrl.vs1_type === REG_SHR || id_ctrl.vs1_type === REG_ADDR)
  val id_scalar_src2 = id_ctrl.vs2_val && (id_ctrl.vs2_type === REG_SHR || id_ctrl.vs2_type === REG_ADDR)
  val id_scalar_src3 = id_ctrl.vs3_val && (id_ctrl.vs3_type === REG_SHR || id_ctrl.vs3_type === REG_ADDR)
  val id_scalar_inst =
    !id_ctrl.vrfu_val &&
    (!id_ctrl.vd_val || id_scalar_dest) && (!id_ctrl.vs1_val || id_scalar_src1) &&
    (!id_ctrl.vs2_val || id_scalar_src2) && (!id_ctrl.vs3_val || id_scalar_src3)
  val id_branch_inst = id_ctrl.vrpu_val
  val id_first_inst = id_ctrl.vrfu_val
  val id_vector_inst = !id_scalar_inst || id_branch_inst

  val id_val = io.imem.resp.valid && id_ctrl.ival

  // only look at shared reg because addr reg can't be written during vf block
  val id_ctrl_wen_not0 = id_ctrl.vd_val && id_ctrl.vd_type === REG_SHR & id_ctrl.vd =/= UInt(0)
  val id_ctrl_rens1_not0 = sren1 && id_ctrl.vs1 =/= UInt(0)
  val id_ctrl_rens2_not0 = sren2 && id_ctrl.vs2 =/= UInt(0)
  val id_ctrl_rens3_not0 = sren3 && id_ctrl.vs3 =/= UInt(0)

  // stall for RAW hazards on non scalar integer pipes
  val id_can_bypass = id_scalar_inst && !id_branch_inst && !id_ctrl.fpu_val && !id_ctrl.smu_val
  val data_hazard_ex = Vec(
    id_ctrl_rens1_not0 && id_ctrl.vs1 === ex_ctrl.vd,
    id_ctrl_rens2_not0 && id_ctrl.vs2 === ex_ctrl.vd,
    id_ctrl_rens3_not0 && id_ctrl.vs3 === ex_ctrl.vd)
  val id_ex_hazard = !id_can_bypass && ex_reg_valid && data_hazard_ex.reduce(_||_)

  // stall on RAW/WAW hazards on loads/fpu until data returns
  val id_sboard_hazard = 
    id_ctrl_rens1_not0 && sboard.read(id_ctrl.vs1) ||
    id_ctrl_rens2_not0 && sboard.read(id_ctrl.vs2) ||
    id_ctrl_rens3_not0 && sboard.read(id_ctrl.vs3) ||
    id_ctrl_wen_not0 && sboard.read(id_ctrl.vd)

  // only set sboard if we were able to send the req
  val id_smu_load = id_ctrl.fn_smu().cmd === SM_L
  val id_smu_store = id_ctrl.fn_smu().cmd === SM_S
  val id_set_sboard =
    io.vxu.fire() && id_first_inst || io.fpu.req.fire() || io.smu.req.fire() && id_smu_load
  sboard.set(id_set_sboard, id_ctrl.vd)

  io.lreq.cnt := UInt(1)
  io.sreq.cnt := UInt(1)
 
  val enq_vxu = id_val && id_vector_inst
  val enq_vmu = id_val && id_ctrl.vmu_val
  val enq_fpu = id_val && id_scalar_inst && id_ctrl.fpu_val
  val enq_smu = id_val && id_ctrl.smu_val

  val mask_vxu_ready = !enq_vxu || io.vxu.ready
  val mask_vmu_ready = !enq_vmu || io.vmu.ready
  val mask_fpu_ready = !enq_fpu || io.fpu.req.ready
  val mask_smu_ready = !enq_smu || io.smu.req.ready
  val mask_smu_load_ok = !enq_smu || !id_smu_load || io.mocheck.load && io.lreq.available
  val mask_smu_store_ok = !enq_smu || !id_smu_store || io.mocheck.store && io.sreq.available

  val stall_pending_fpu = (id_ctrl.decode_stop || enq_fpu) && pending_fpu
  val stall_pending_smu = (id_ctrl.decode_stop || enq_smu) && pending_smu

  val ctrl_stalld_common =
    !vf_active || id_ex_hazard || id_sboard_hazard ||
    stall_pending_fpu || stall_pending_smu

  val ctrl_fire_common =
    io.imem.resp.valid && id_ctrl.ival && !io.dpath.ex_br_taken

  assert(!vf_active || !io.imem.resp.valid || id_ctrl.ival, "illegal instruction exception!")

  def fire_decode(exclude: Bool, include: Bool*) = {
    val rvs = Seq(!ctrl_stalld_common, ctrl_fire_common,
      mask_vxu_ready, mask_vmu_ready,
      mask_fpu_ready, mask_smu_ready, mask_smu_load_ok, mask_smu_store_ok)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  // stall fetch/decode if we aren't ready to issue the op being decoded
  io.dpath.stalld := !fire_decode(ctrl_fire_common) || io.dpath.stallx || io.dpath.stallw
  io.dpath.killd :=
    !ctrl_fire_common || io.dpath.stalld ||
    id_vector_inst && !id_branch_inst || enq_fpu || enq_smu

  // use rm in inst unless its dynamic then take rocket rm
  // TODO: pipe rockets rm here (FPU outputs it?, or store it in rocc unit)
  val rm = Mux(id_ctrl.rm === Bits("b111"), UInt(0), id_ctrl.rm)

  // to FPU
  io.fpu.req.bits <> id_ctrl.fpu_fn
  io.fpu.req.valid := fire_decode(mask_fpu_ready, enq_fpu)
  when (io.fpu.req.fire()) {
    pending_fpu := Bool(true)
    pending_fpu_typ := Mux(id_ctrl.fpu_fn.fromint, id_ctrl.in_fmt, id_ctrl.out_fmt)
    pending_fpu_reg := id_ctrl.vd
    pending_fpu_fn := id_ctrl.fpu_fn
  }

  // to VXU
  io.vxu.valid := fire_decode(mask_vxu_ready, enq_vxu)
  io.vxu.bits.lane := vl
  io.vxu.bits.active.vint := id_ctrl.active_vint()
  io.vxu.bits.active.vipred := id_ctrl.active_vipred()
  io.vxu.bits.active.vimul := id_ctrl.active_vimul()
  io.vxu.bits.active.vidiv := id_ctrl.active_vidiv()
  io.vxu.bits.active.vfma := id_ctrl.active_vfma()
  io.vxu.bits.active.vfdiv := id_ctrl.active_vfdiv()
  io.vxu.bits.active.vfcmp := id_ctrl.active_vfcmp()
  io.vxu.bits.active.vfconv := id_ctrl.active_vfconv()
  io.vxu.bits.active.vrpred := id_ctrl.active_vrpred()
  io.vxu.bits.active.vrfirst := id_ctrl.active_vrfirst()
  io.vxu.bits.active.vamo := id_ctrl.active_vamo()
  io.vxu.bits.active.vldx := id_ctrl.active_vldx()
  io.vxu.bits.active.vstx := id_ctrl.active_vstx()
  io.vxu.bits.active.vld := id_ctrl.active_vld()
  io.vxu.bits.active.vst := id_ctrl.active_vst()
  io.vxu.bits.fn.union :=
    MuxCase(Bits(0), Array(
      id_ctrl.viu_val  -> id_ctrl.fn_viu().toBits,
      id_ctrl.vipu_val -> id_ctrl.fn_vipu().toBits,
      id_ctrl.vimu_val -> id_ctrl.fn_vimu().toBits,
      id_ctrl.vidu_val -> id_ctrl.fn_vidu().toBits,
      id_ctrl.vfmu_val -> id_ctrl.fn_vfmu(rm).toBits,
      id_ctrl.vfdu_val -> id_ctrl.fn_vfdu(rm).toBits,
      id_ctrl.vfcu_val -> id_ctrl.fn_vfcu(rm).toBits,
      id_ctrl.vfvu_val -> id_ctrl.fn_vfvu(rm).toBits,
      id_ctrl.vrpu_val -> id_ctrl.fn_vrpu().toBits,
      id_ctrl.vrfu_val -> id_ctrl.fn_vrfu().toBits,
      id_ctrl.vmu_val  -> id_ctrl.fn_vmu().toBits
    ))
  io.vxu.bits.base.vs1.valid := id_ctrl.vs1_val
  io.vxu.bits.base.vs2.valid := id_ctrl.vs2_val
  io.vxu.bits.base.vs3.valid := id_ctrl.vs3_val
  io.vxu.bits.base.vd.valid := id_ctrl.vd_val
  io.vxu.bits.base.vs1.scalar := id_scalar_src1
  io.vxu.bits.base.vs2.scalar := id_scalar_src2
  io.vxu.bits.base.vs3.scalar := id_scalar_src3
  io.vxu.bits.base.vd.scalar := id_scalar_dest
  io.vxu.bits.base.vs1.pred := id_ctrl.vs1_type === REG_PRED
  io.vxu.bits.base.vs2.pred := id_ctrl.vs2_type === REG_PRED
  io.vxu.bits.base.vs3.pred := id_ctrl.vs3_type === REG_PRED
  io.vxu.bits.base.vd.pred := id_ctrl.vd_type === REG_PRED
  io.vxu.bits.base.vs1.id := id_ctrl.vs1
  io.vxu.bits.base.vs2.id := id_ctrl.vs2
  io.vxu.bits.base.vs3.id := id_ctrl.vs3
  io.vxu.bits.base.vd.id := id_ctrl.vd
  io.vxu.bits.base.vp.valid := id_ctrl.vp_val
  io.vxu.bits.base.vp.pred := Bool(true)
  io.vxu.bits.base.vp.neg() := id_ctrl.vp_neg
  io.vxu.bits.base.vp.id := id_ctrl.vp
  when (fire_decode(null, id_branch_inst)) { pending_cbranch := Bool(true) }

  // to VMU
  io.vmu.valid := fire_decode(mask_vmu_ready, enq_vmu)
  io.vmu.bits.fn.mode := id_ctrl.vmu_mode
  io.vmu.bits.fn.cmd := id_ctrl.vmu_cmd
  io.vmu.bits.fn.mt := id_ctrl.mt
  io.vmu.bits.lane := vl

  // to SMU
  io.smu.req.valid := fire_decode(mask_smu_ready, enq_smu)
  io.smu.req.bits.fn := id_ctrl.fn_smu()
  when (io.smu.req.fire()) { pending_smu := Bool(true) }
  when (io.smu.confirm) { pending_smu := Bool(false) }

  // to MRT
  io.lreq.reserve := fire_decode(null, enq_smu, id_smu_load)
  io.sreq.reserve := fire_decode(null, enq_smu, id_smu_store)

  // EXECUTE
  // if we didn't stallx (which includes stallw) then we should move the pipe forward
  when (!io.dpath.stallx) {
    ex_reg_valid := !io.dpath.killd
    ex_ctrl := id_ctrl
  }

  io.red.pred.ready := Bool(true)

  val ex_stall_fpu = ex_reg_valid && io.fpu.resp.valid
  val ex_stall_smu = ex_reg_valid && io.smu.resp.valid && !io.smu.resp.bits.store
  val ex_stall_rfirst = ex_reg_valid && io.red.first.valid
  val ex_br_resolved = io.red.pred.fire()
  val ex_br_taken = ex_br_resolved && io.red.pred.bits.cond
  val ex_br_not_taken = ex_br_resolved && !io.red.pred.bits.cond

  when (ex_br_resolved) { pending_cbranch := Bool(false) }

  io.dpath.stallx :=
    pending_cbranch && !ex_br_resolved ||
    ex_stall_fpu || ex_stall_smu || io.dpath.stallw
  io.dpath.killx := !ex_reg_valid || ex_br_not_taken || io.dpath.stallx

  io.dpath.ex_valid := ex_reg_valid
  io.dpath.ex_ctrl := ex_ctrl
  io.dpath.ex_br_taken := ex_br_taken
  io.dpath.ex_bypass := data_hazard_ex.map(ex_reg_valid && !ex_br_not_taken && _)

  val ll_warb = Module(new Arbiter(new ScalarRFWritePort, 3))

  val unrec_s = ieee_sp(io.fpu.resp.bits.data)
  val unrec_d = ieee_dp(io.fpu.resp.bits.data)
  val unrec_fpu_resp =
    Mux(pending_fpu_typ === UInt(0), Cat(Fill(32,unrec_s(31)), unrec_s), unrec_d)

  ll_warb.io.in(0).valid := io.fpu.resp.valid
  ll_warb.io.in(0).bits.addr := pending_fpu_reg
  ll_warb.io.in(0).bits.data :=
    Mux(pending_fpu_fn.toint, io.fpu.resp.bits.data(63, 0), unrec_fpu_resp)
  assert(!io.fpu.resp.valid || ll_warb.io.in(0).ready, "fpu port should always have priority")
  io.fpu.resp.ready := Bool(true)

  ll_warb.io.in(1).valid := io.smu.resp.valid && !io.smu.resp.bits.store
  ll_warb.io.in(1).bits.addr := io.smu.resp.bits.tag
  ll_warb.io.in(1).bits.data := io.smu.resp.bits.data
  io.smu.resp.ready := ll_warb.io.in(1).ready

  ll_warb.io.in(2).valid := io.red.first.valid
  ll_warb.io.in(2).bits.addr := io.red.first.bits.sd
  ll_warb.io.in(2).bits.data := io.red.first.bits.first
  io.red.first.ready := ll_warb.io.in(2).ready

  ll_warb.io.out.ready := Bool(true) // long-latency write port always wins

  // WRITEBACK
  val wb_ll_valid = Reg(next=ll_warb.io.out.valid)
  val wb_ll_waddr = RegEnable(ll_warb.io.out.bits.addr, ll_warb.io.out.valid)
  val wb_ll_wdata = RegEnable(ll_warb.io.out.bits.data, ll_warb.io.out.valid)

  when (!io.dpath.stallw) {
    wb_reg_valid := !io.dpath.killx
    wb_ctrl := ex_ctrl
  }

  io.dpath.stallw := Bool(false)

  io.dpath.wb_valid := wb_reg_valid
  io.dpath.wb_ctrl := wb_ctrl
  io.dpath.wb_wen := wb_ll_valid || wb_reg_valid
  io.dpath.wb_ll_valid := wb_ll_valid
  io.dpath.wb_ll_waddr := wb_ll_waddr
  io.dpath.wb_ll_wdata := wb_ll_wdata

  sboard.clear(wb_ll_valid, wb_ll_waddr)
  when (wb_ll_valid) { pending_fpu := Bool(false) }

  assert(!(wb_ll_valid && wb_reg_valid), "long latency and scalar wb conflict")
}

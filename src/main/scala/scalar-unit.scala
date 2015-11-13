package hwacha

import Chisel._
import cde.Parameters
import rocket.ALU._
import ScalarFPUDecode._
import HardFloatHelper._

class HwachaFPInput extends rocket.FPInput {
  val in_fmt = UInt(width = 2)
}

class ScalarRFWritePort(implicit p: Parameters) extends HwachaBundle()(p) {
  val addr = UInt(width = bSRegs)
  val data = UInt(width = regLen)
}

class ScalarUnit(resetSignal: Bool = null)(implicit p: Parameters) extends HwachaModule(_reset = resetSignal)(p)
  with VMUParameters {
  import Commands._

  val io = new Bundle {
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

    val vf_active = Bool(OUTPUT)
    val pending = new Bundle {
      val mseq = new SequencerPending().asInput
      val mrt = new Bundle {
        val su = new MRTPending().asOutput
        val vus = Vec.fill(nLanes){new MRTPending}.asInput
      }
    }
  }

  // STATE
  class SRegFile {
    private val rf = Mem(UInt(width = regLen), nSRegs-1)
    private val reads = collection.mutable.ArrayBuffer[(UInt,UInt)]()
    private var canRead = true
    def read(addr: UInt) = {
      require(canRead)
      reads += addr -> UInt()
      reads.last._2 := Mux(addr =/= UInt(0), rf(~addr), UInt(0))
      reads.last._2
    }
    def write(addr: UInt, data: UInt) = {
      canRead = false
      when (addr =/= UInt(0)) {
        rf(~addr) := data
        for ((raddr, rdata) <- reads)
          when (addr === raddr) { rdata := data }
      }
    }
  }

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

  val srf = new SRegFile // doesn't have vs0
  val arf = Mem(UInt(width = regLen), nARegs)
  val sboard = new Scoreboard(nSRegs)
  val mrt = Module(new MemTracker(4, 4))
  val muldiv = Module(new rocket.MulDiv(width = regLen, nXpr = nSRegs, unroll = 8, earlyOut = true))

  io.pending.mrt.su := mrt.io.pending

  val vf_active = Reg(init=Bool(false))
  val vl = Vec.fill(nLanes){Reg(new VLenEntry)}
  val busy_scalar = Bool()

  io.vf_active := vf_active

  val decode_vmss    = io.cmdq.cmd.bits === CMD_VMSS
  val decode_vmsa    = io.cmdq.cmd.bits === CMD_VMSA
  val decode_vsetcfg = io.cmdq.cmd.bits === CMD_VSETCFG
  val decode_vsetvl  = io.cmdq.cmd.bits === CMD_VSETVL
  val decode_vf      = io.cmdq.cmd.bits === CMD_VF
  val decode_vft     = io.cmdq.cmd.bits === CMD_VFT

  val deq_imm = decode_vmss || decode_vmsa || decode_vf || decode_vft || decode_vsetvl || decode_vsetcfg
  val deq_rd  = decode_vmss || decode_vmsa

  val mask_imm_valid = !deq_imm || io.cmdq.imm.valid
  val mask_rd_valid  = !deq_rd  || io.cmdq.rd.valid

  // TODO: we could fire all cmd but vf* without pending.mseq being clear
  def fire_cmdq(exclude: Bool, include: Bool*) = {
  val rvs = Seq(
      !vf_active, !busy_scalar, !decode_vmss || !sboard.read(io.cmdq.rd.bits), !io.pending.mseq.all,
      io.cmdq.cmd.valid, mask_imm_valid, mask_rd_valid)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.cmdq.cmd.ready := fire_cmdq(io.cmdq.cmd.valid)
  io.cmdq.imm.ready := fire_cmdq(mask_imm_valid, deq_imm)
  io.cmdq.rd.ready  := fire_cmdq(mask_rd_valid, deq_rd)

  val swrite = fire_cmdq(null, decode_vmss)
  val awrite = fire_cmdq(null, decode_vmsa)

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

  val fire_vf = fire_cmdq(null, decode_vf)
  when (fire_vf) { vf_active := Bool(true) }

  val pending_fpu = Reg(init=Bool(false))
  val pending_fpu_reg = Reg(init=UInt(width=log2Up(nSRegs)))
  val pending_fpu_typ = Reg(init=Bits(width=2))
  val pending_fpu_fn = Reg(new rocket.FPUCtrlSigs())
  val pending_smu = Reg(init=Bool(false))
  val pending_cbranch = Reg(init=Bool(false))

  val ex_reg_valid = Reg(Bool())
  val ex_reg_ctrl = Reg(new IntCtrlSigs)
  val ex_reg_pc = Reg(UInt())
  val ex_reg_inst = Reg(Bits())
  val ex_reg_bypass = Vec.fill(3){Reg(Bool())}
  val ex_reg_srs = Vec.fill(3){Reg(Bits())}
  val ex_reg_ars = Vec.fill(2){Reg(Bits())}

  val wb_reg_valid = Reg(Bool())
  val wb_reg_ctrl = Reg(new IntCtrlSigs)
  val wb_reg_pc = Reg(UInt())
  val wb_reg_inst = Reg(Bits())
  val wb_reg_wdata = Reg(Bits())

  busy_scalar := ex_reg_valid || wb_reg_valid

  // WIRES
  val stalld = Bool()
  val killd = Bool()
  val stallx = Bool()
  val killx = Bool()
  val stallw = Bool()
  val ex_br_resolved = Bool()
  val ex_br_taken = Bool()
  val ex_br_not_taken = Bool()
  val ex_br_taken_pc = UInt()

  // FETCH
  io.imem.req.valid := fire_vf || ex_br_taken
  io.imem.req.bits.pc := Mux(ex_br_taken, ex_br_taken_pc, io.cmdq.imm.bits)
  io.imem.active := vf_active
  io.imem.invalidate := Bool(false) // TODO: flush cache/tlb on vfence
  io.imem.resp.ready := !stalld

  // DECODE
  val id_pc = io.imem.resp.bits.pc
  val id_inst = io.imem.resp.bits.data(0).toBits; require(p(rocket.FetchWidth) == 1)
  val decode_table = ScalarDecode.table ++ VectorMemoryDecode.table ++ VectorArithmeticDecode.table
  val id_ctrl = new IntCtrlSigs().decode(id_inst, decode_table)
  when (!killd && id_ctrl.decode_stop) { vf_active := Bool(false) }

  val sren = Vec(
    id_ctrl.vs1_val && id_ctrl.vs1_type === REG_SHR,
    id_ctrl.vs2_val && id_ctrl.vs2_type === REG_SHR,
    id_ctrl.vs3_val && id_ctrl.vs3_type === REG_SHR)
  val aren = Vec(
    id_ctrl.vs1_val && id_ctrl.vs1_type === REG_ADDR,
    id_ctrl.vs2_val && id_ctrl.vs2_type === REG_ADDR)

  val id_sraddr = Vec(id_ctrl.vs1, id_ctrl.vs2, id_ctrl.vs3)
  val id_araddr = Vec(id_ctrl.as1(), id_ctrl.as2())
  val id_sreads = id_sraddr.map(srf.read(_))
  val id_areads = id_araddr.map(arf(_))

  val id_scalar_dest = id_ctrl.vd_val  && (id_ctrl.vd_type  === REG_SHR || id_ctrl.vd_type  === REG_ADDR)
  val id_scalar_src1 = id_ctrl.vs1_val && (id_ctrl.vs1_type === REG_SHR || id_ctrl.vs1_type === REG_ADDR)
  val id_scalar_src2 = id_ctrl.vs2_val && (id_ctrl.vs2_type === REG_SHR || id_ctrl.vs2_type === REG_ADDR)
  val id_scalar_src3 = id_ctrl.vs3_val && (id_ctrl.vs3_type === REG_SHR || id_ctrl.vs3_type === REG_ADDR)
  val id_scalar_inst =
    !id_ctrl.vrfu_val &&
    (!id_ctrl.vd_val || id_scalar_dest) && (!id_ctrl.vs1_val || id_scalar_src1) &&
    (!id_ctrl.vs2_val || id_scalar_src2) && (!id_ctrl.vs3_val || id_scalar_src3)
  val id_fpu_inst = id_ctrl.fpu_val
  val id_smem_inst = id_ctrl.smu_val
  val id_mul_inst = id_ctrl.vimu_val
  val id_muldiv_inst = id_mul_inst || id_ctrl.vidu_val
  val id_branch_inst = id_ctrl.vrpu_val
  val id_first_inst = id_ctrl.vrfu_val
  val id_vector_inst = !id_scalar_inst || id_branch_inst
  val id_vmem_inst = id_ctrl.vmu_val

  val id_val = io.imem.resp.valid && id_ctrl.ival

  // only look at shared reg because addr reg can't be written during vf block
  val id_ctrl_wen_not0 = id_ctrl.vd_val && id_ctrl.vd_type === REG_SHR & id_ctrl.vd =/= UInt(0)
  val id_ctrl_rens1_not0 = sren(0) && id_ctrl.vs1 =/= UInt(0)
  val id_ctrl_rens2_not0 = sren(1) && id_ctrl.vs2 =/= UInt(0)
  val id_ctrl_rens3_not0 = sren(2) && id_ctrl.vs3 =/= UInt(0)

  // stall for RAW hazards on non scalar integer pipes
  val id_can_bypass =
    id_scalar_inst && !id_branch_inst && !id_fpu_inst && !id_smem_inst && !id_muldiv_inst
  val id_data_hazard_ex = Vec(
    id_ctrl_rens1_not0 && id_ctrl.vs1 === ex_reg_ctrl.vd,
    id_ctrl_rens2_not0 && id_ctrl.vs2 === ex_reg_ctrl.vd,
    id_ctrl_rens3_not0 && id_ctrl.vs3 === ex_reg_ctrl.vd)
  val id_ex_hazard = !id_can_bypass && ex_reg_valid && id_data_hazard_ex.reduce(_||_)

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
    io.vxu.fire() && id_first_inst ||
    io.fpu.req.fire() || io.smu.req.fire() && id_smu_load || muldiv.io.req.fire()
  sboard.set(id_set_sboard, id_ctrl.vd)

  val enq_vxu = id_val && id_vector_inst
  val enq_vmu = id_val && id_vmem_inst
  val enq_fpu = id_val && id_scalar_inst && id_fpu_inst
  val enq_smu = id_val && id_smem_inst
  val enq_muldiv = id_val && id_scalar_inst && id_muldiv_inst

  val stall_fpu = enq_fpu && pending_fpu

  mrt.io.lreq.cnt := UInt(1)
  mrt.io.sreq.cnt := UInt(1)

  val stall_smu =
    pending_smu ||
    enq_smu && id_smu_load && (!io.mocheck.load || !mrt.io.lreq.available) ||
    enq_smu && id_smu_store && (!io.mocheck.store || !mrt.io.sreq.available)

  val stall_pending_fence = id_ctrl.decode_fence && (
    io.pending.mseq.mem ||
    io.pending.mrt.su.all || io.pending.mrt.vus.map(_.all).reduce(_ || _))

  val ctrl_stalld_common =
    !vf_active || id_ex_hazard || id_sboard_hazard ||
    stall_fpu || stall_smu || stall_pending_fence

  val ctrl_fire_common =
    io.imem.resp.valid && id_ctrl.ival && !ex_br_taken

  assert(!vf_active || !io.imem.resp.valid || id_ctrl.ival, "illegal instruction exception!")

  val mask_vxu_ready = !enq_vxu || io.vxu.ready
  val mask_vmu_ready = !enq_vmu || io.vmu.ready
  val mask_fpu_ready = !enq_fpu || io.fpu.req.ready
  val mask_smu_ready = !enq_smu || io.smu.req.ready
  val mask_muldiv_ready = !enq_muldiv || muldiv.io.req.ready

  def fire_decode(exclude: Bool, include: Bool*) = {
    val rvs = Seq(!ctrl_stalld_common, ctrl_fire_common,
      mask_vxu_ready, mask_vmu_ready,
      mask_fpu_ready, mask_smu_ready, mask_muldiv_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  // stall fetch/decode if we aren't ready to issue the op being decoded
  stalld := !fire_decode(ctrl_fire_common) || stallx || stallw
  killd :=
    !ctrl_fire_common || stalld ||
    id_vector_inst && !id_branch_inst || enq_fpu || enq_smu || enq_muldiv

  // use rm in inst unless its dynamic then take rocket rm
  // TODO: pipe rockets rm here (FPU outputs it?, or store it in rocc unit)
  val rm = Mux(id_ctrl.rm === Bits("b111"), UInt(0), id_ctrl.rm)

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
  io.vxu.bits.sreg.ss1 := id_sreads(0)
  io.vxu.bits.sreg.ss2 := id_sreads(1)
  io.vxu.bits.sreg.ss3 := id_sreads(2)
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

  val addr_stride =
    MuxLookup(id_ctrl.mt, UInt(0),Seq(
      MT_B->  UInt(1),
      MT_BU-> UInt(1),
      MT_H->  UInt(2),
      MT_HU-> UInt(2),
      MT_W->  UInt(4),
      MT_WU-> UInt(4),
      MT_D->  UInt(8)
    ))

  io.vmu.bits.base :=
    Mux(aren(0), id_areads(0), // unit-stride
      Mux(isAMO(id_ctrl.vmu_cmd), UInt(0), // AMO
        id_sreads(0))) // indexed
  io.vmu.bits.stride :=
    Mux(aren(1), id_areads(1), // constant-stride
      addr_stride) // unit-stride


  // to FPU
  io.fpu.req.valid := fire_decode(mask_fpu_ready, enq_fpu)
  io.fpu.req.bits <> id_ctrl.fpu_fn
  io.fpu.req.bits.rm := rm
  io.fpu.req.bits.typ := id_ctrl.out_fmt
  io.fpu.req.bits.in_fmt := id_ctrl.in_fmt
  io.fpu.req.bits.in1 := id_sreads(0)
  io.fpu.req.bits.in2 := id_sreads(1)
  io.fpu.req.bits.in3 := id_sreads(2)
  when (io.fpu.req.fire()) {
    pending_fpu := Bool(true)
    pending_fpu_typ := Mux(id_ctrl.fpu_fn.fromint, id_ctrl.in_fmt, id_ctrl.out_fmt)
    pending_fpu_reg := id_ctrl.vd
    pending_fpu_fn := id_ctrl.fpu_fn
  }

  // to SMU
  io.smu.req.valid := fire_decode(mask_smu_ready, enq_smu)
  io.smu.req.bits.fn := id_ctrl.fn_smu()
  io.smu.req.bits.addr := Mux(aren(0), id_areads(0), id_sreads(0))
  io.smu.req.bits.data := id_sreads(1)
  io.smu.req.bits.tag := id_ctrl.vd
  when (io.smu.req.fire()) { pending_smu := Bool(true) }
  when (io.smu.confirm) { pending_smu := Bool(false) }

  // to MUL
  muldiv.io.req.valid := fire_decode(mask_muldiv_ready, enq_muldiv)
  muldiv.io.req.bits.dw :=
    Mux(id_ctrl.alu_dw === DW32, RocketConstants.DW_32, RocketConstants.DW_64)
  muldiv.io.req.bits.fn :=
    Mux(id_mul_inst,
      Mux(id_ctrl.vimu_fn === IM_M,    rocket.ALU.FN_MUL,
      Mux(id_ctrl.vimu_fn === IM_MH,   rocket.ALU.FN_MULH,
      Mux(id_ctrl.vimu_fn === IM_MHU,  rocket.ALU.FN_MULHU,
                                       rocket.ALU.FN_MULHSU))),
      Mux(id_ctrl.vidu_fn === ID_DIV,  rocket.ALU.FN_DIV,
      Mux(id_ctrl.vidu_fn === ID_DIVU, rocket.ALU.FN_DIVU,
      Mux(id_ctrl.vidu_fn === ID_REM,  rocket.ALU.FN_REM,
                                       rocket.ALU.FN_REMU))))
  muldiv.io.req.bits.in1 := id_sreads(0)
  muldiv.io.req.bits.in2 := id_sreads(1)
  muldiv.io.req.bits.tag := id_ctrl.vd
  muldiv.io.kill := Bool(false)

  // to MRT
  mrt.io.lreq.reserve := fire_decode(null, enq_smu, id_smu_load)
  mrt.io.sreq.reserve := fire_decode(null, enq_smu, id_smu_store)

  // EXECUTE
  val ex_stall_fpu = ex_reg_valid && io.fpu.resp.valid
  val ex_stall_smu = ex_reg_valid && io.smu.resp.valid && !io.smu.resp.bits.store
  val ex_stall_muldiv = ex_reg_valid && muldiv.io.resp.valid
  val ex_stall_rfirst = ex_reg_valid && io.red.first.valid

  io.red.pred.ready := Bool(true)
  ex_br_resolved := io.red.pred.fire()
  ex_br_taken := ex_br_resolved && io.red.pred.bits.cond
  ex_br_not_taken := ex_br_resolved && !io.red.pred.bits.cond
  when (ex_br_resolved) { pending_cbranch := Bool(false) }

  stallx :=
    pending_cbranch && !ex_br_resolved ||
    ex_stall_fpu || ex_stall_smu || ex_stall_muldiv || ex_stall_rfirst || stallw
  killx := !ex_reg_valid || ex_br_not_taken || stallx

  when (!stallx) {
    ex_reg_valid := !killd
    ex_reg_ctrl := id_ctrl
  }

  when (!killd) {
    ex_reg_pc := id_pc
    ex_reg_inst := id_inst
    ex_reg_bypass := id_data_hazard_ex.map(ex_reg_valid && !ex_br_not_taken && _)
    for (i <- 0 until id_sreads.size) {
      when (sren(i)) { ex_reg_srs(i) := id_sreads(i) }
    }
    for (i <- 0 until id_areads.size) {
      when (aren(i)) { ex_reg_ars(i) := id_areads(i) }
    }
  }

  val ex_srs = for (i <- 0 until id_sreads.size)
    yield Mux(ex_reg_bypass(i), wb_reg_wdata, ex_reg_srs(i))

  def imm(sel: Bits, inst: Bits) = {
    val sign = inst(63).toSInt
    val b30_3 = inst(62,35)
    val b2_0 = Mux(sel === IMM_I, inst(34,32), Bits(0))
    val out = Cat(sign, b30_3, b2_0).toSInt
    Mux(sel === IMM_L, Cat(out, UInt(0, 32)).toSInt, out)
  }
  val ex_imm = imm(ex_reg_ctrl.sel_imm, ex_reg_inst)

  // vcjalr has vs1_val set, so take the base address from register
  // vcjal doesn't have vs1_val set, so take pc as base address
  ex_br_taken_pc := Mux(ex_reg_ctrl.vs1_val, ex_srs(0).toSInt, ex_reg_pc.toSInt) + ex_imm

  val ex_op1 = MuxLookup(ex_reg_ctrl.alu_sel1, SInt(0), Seq(
    A1_ZERO -> SInt(0),
    A1_RS1  -> ex_srs(0).toSInt,
    A1_PC   -> ex_reg_pc.toSInt))
  val ex_op2 = MuxLookup(ex_reg_ctrl.alu_sel2, SInt(0), Seq(
    A2_8    -> SInt(8),
    A2_RS2  -> ex_srs(1).toSInt,
    A2_IMM  -> ex_imm))

  val alu = Module(new rocket.ALU)
  alu.io.dw := ex_reg_ctrl.alu_dw
  alu.io.fn := ex_reg_ctrl.alu_fn
  alu.io.in2 := ex_op2.toUInt
  alu.io.in1 := ex_op1

  val ll_warb = Module(new Arbiter(new ScalarRFWritePort, 4))

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

  mrt.io.lret.cnt := UInt(1)
  mrt.io.lret.update := io.smu.resp.fire() && !io.smu.resp.bits.store
  mrt.io.sret.cnt := UInt(1)
  mrt.io.sret.update := io.smu.resp.fire() && io.smu.resp.bits.store

  ll_warb.io.in(2).valid := muldiv.io.resp.valid
  ll_warb.io.in(2).bits.addr := muldiv.io.resp.bits.tag
  ll_warb.io.in(2).bits.data := muldiv.io.resp.bits.data
  muldiv.io.resp.ready := ll_warb.io.in(2).ready

  ll_warb.io.in(3).valid := io.red.first.valid
  ll_warb.io.in(3).bits.addr := io.red.first.bits.sd
  ll_warb.io.in(3).bits.data := io.red.first.bits.first
  io.red.first.ready := ll_warb.io.in(3).ready

  ll_warb.io.out.ready := Bool(true) // long-latency write port always wins

  // WRITEBACK
  val wb_ll_valid = Reg(next=ll_warb.io.out.valid)
  val wb_ll_waddr = RegEnable(ll_warb.io.out.bits.addr, ll_warb.io.out.valid)
  val wb_ll_wdata = RegEnable(ll_warb.io.out.bits.data, ll_warb.io.out.valid)

  stallw := Bool(false)

  when (!stallw) {
    wb_reg_valid := !killx
    wb_reg_ctrl := ex_reg_ctrl
  }

  when (!killx) {
    wb_reg_pc := ex_reg_pc
    wb_reg_inst := ex_reg_inst
    wb_reg_wdata := alu.io.out
  }

  val wb_wen = wb_reg_valid && wb_reg_ctrl.vd_val
  val wb_waddr =
    Mux(swrite, io.cmdq.rd.bits,
      Mux(wb_ll_valid, wb_ll_waddr, wb_reg_ctrl.vd))
  val wb_wdata =
    Mux(swrite, io.cmdq.imm.bits,
      Mux(wb_ll_valid, wb_ll_wdata, wb_reg_wdata))

  when (swrite || wb_ll_valid || wb_wen) {
    srf.write(wb_waddr, wb_wdata)
    if (commit_log) printf("H: write_srf %d %x\n", wb_waddr, wb_wdata)
  }
  when (awrite) {
    arf(io.cmdq.rd.bits) := io.cmdq.imm.bits
    if (commit_log) printf("H: write_arf %d %x\n", io.cmdq.rd.bits, io.cmdq.imm.bits)
  }

  sboard.clear(wb_ll_valid, wb_ll_waddr)
  when (wb_ll_valid) { pending_fpu := Bool(false) }

  assert(!(wb_ll_valid && wb_wen), "long latency and scalar wb conflict")
  assert(!((wb_ll_valid || wb_wen) && swrite), "Cannot write vmss and scalar dest")
  assert(!(swrite && sboard.read(wb_waddr)), "Cannot write scalar dest when sboard is set")

  when(vf_active || wb_reg_valid) {
    printf("H: [%x] pc=[%x] SW[r%d=%x][%d] SR[r%d=%x] SR[r%d=%x] inst=[%x] DASM(%x)\n",
         wb_reg_valid, wb_reg_pc,
         wb_waddr, wb_wdata, swrite || wb_ll_valid || wb_wen,
         wb_reg_ctrl.vs1, Mux(wb_reg_ctrl.vs1_type === REG_ADDR,
                            Reg(next=Reg(next=ex_reg_ars(0))),
                            Reg(next=Reg(next=ex_srs(0)))),
         wb_reg_ctrl.vs2, Mux(wb_reg_ctrl.vs2_type === REG_ADDR,
                            Reg(next=Reg(next=ex_reg_ars(1))),
                            Reg(next=Reg(next=ex_srs(1)))),
         wb_reg_inst, wb_reg_inst)
  }
}

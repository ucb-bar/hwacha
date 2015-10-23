package hwacha

import Chisel._
import cde.Parameters
import rocket.ALU._
import ScalarFPUDecode._

class CtrlDpathIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val stalld = Bool(OUTPUT)
  val killd = Bool(OUTPUT)
  val stallx = Bool(OUTPUT)
  val killx = Bool(OUTPUT)
  val stallw = Bool(OUTPUT)

  val inst = Bits(INPUT, 64)
  val ex_inst = Bits(INPUT, 64)
  val wb_inst = Bits(INPUT, 64)
  val fire_vf = Bool(OUTPUT)

  val id_ctrl = new IntCtrlSigs().asOutput()
  val ex_ctrl = new IntCtrlSigs().asOutput()
  val wb_ctrl = new IntCtrlSigs().asOutput()

  // f/d signals
  val sren = Vec.fill(3)(Bool(OUTPUT))
  val aren = Vec.fill(2)(Bool(OUTPUT))

  // x signals
  val ex_waddr = Bits(INPUT, log2Up(nSRegs))
  val bypass = Vec.fill(3){Bool(OUTPUT)}

  // w signals
  val wb_valid = Bool(OUTPUT)
  val wb_wen = Bool(OUTPUT)
  val wb_dmem_load_valid = Bool(OUTPUT)
  val wb_fpu_valid  = Bool(OUTPUT)
  val wb_dmem_waddr = UInt(OUTPUT,log2Up(nSRegs))
  val swrite = Bool(OUTPUT)
  val awrite = Bool(OUTPUT)

  // long-latency signals
  val pending_fpu_reg = UInt(OUTPUT,log2Up(nSRegs))
  val pending_fpu_typ = UInt(OUTPUT,2)
  val pending_fpu_fn = new rocket.FPUCtrlSigs().asOutput()

  val vf_active = Bool(OUTPUT)
}

class ScalarCtrl(resetSignal: Bool = null)(implicit p: Parameters) extends HwachaModule(_reset = resetSignal)(p)
  with VMUParameters {
  import Commands._

  val io = new Bundle {
    val cmdq = new CMDQIO().flip

    val imem = new FrontendIO
    val dpath = new CtrlDpathIO

    val vxu = new VXUIssueOpIO
    val vmu = Decoupled(new VMUOp)
    val dmem = new ScalarMemIO().flip
    val fpu = new Bundle {
      val req = Decoupled(new rocket.FPInput())
      val resp = Decoupled(new rocket.FPResult()).flip
    }

    val vf_active = Bool(OUTPUT)
    val pending_memop = Bool(OUTPUT)
    val pending_seq = Bool(INPUT)
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
  val sboard_not_empty = (0 until nSRegs).map(i => sboard.read(UInt(i))).reduce(_||_)

  val vf_active = Reg(init=Bool(false))
  val vlen = Reg(UInt(width = bVLen))

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
  val pending_smu_store = Reg(Bool())

  io.vf_active := vf_active
  io.dpath.vf_active := vf_active
  io.pending_memop := pending_smu

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

  // TODO: we could fire all cmd but vf* without pending_seq being clear
  def fire_cmdq(exclude: Bool, include: Bool*) = {
  val rvs = Seq(
      !vf_active, !wb_reg_valid, !io.pending_seq, io.cmdq.cmd.valid, !pending_smu, !pending_fpu,
      mask_imm_valid, mask_rd_valid) 
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.dpath.swrite := fire_cmdq(null,decode_vmss) 
  io.dpath.awrite := fire_cmdq(null,decode_vmsa) 

  io.cmdq.cmd.ready := fire_cmdq(io.cmdq.cmd.valid)
  io.cmdq.imm.ready := fire_cmdq(mask_imm_valid, deq_imm)
  io.cmdq.rd.ready  := fire_cmdq(mask_rd_valid, deq_rd)

  // FETCH
  io.imem.req.valid := io.dpath.fire_vf
  io.imem.active := vf_active
  io.imem.invalidate := Bool(false) // TODO: flush cache/tlb on vfence
  io.imem.resp.ready := !io.dpath.stalld

  // DECODE
  val decode_table = ScalarDecode.table ++ VectorMemoryDecode.table ++ VectorArithmeticDecode.table
  val id_ctrl = new IntCtrlSigs().decode(io.dpath.inst, decode_table)
  io.dpath.id_ctrl := id_ctrl
  io.dpath.ex_ctrl := ex_ctrl
  io.dpath.wb_ctrl := wb_ctrl

  when (fire_cmdq(null, decode_vsetcfg)) { vlen := io.cmdq.imm.bits }
  when (fire_cmdq(null, decode_vsetvl)) { vlen := io.cmdq.imm.bits }

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
    (!id_ctrl.vd_val || id_scalar_dest) && (!id_ctrl.vs1_val || id_scalar_src1) &&
    (!id_ctrl.vs2_val || id_scalar_src2) && (!id_ctrl.vs3_val || id_scalar_src3)

  val id_val = io.imem.resp.valid && id_ctrl.ival

  // TOOD: update riscv-opcodes to export these numbers as constants
  val id_waddr = io.dpath.inst(23,16)
  val id_raddrs1 = io.dpath.inst(31,24)
  val id_raddrs2 = io.dpath.inst(40,33)
  val id_raddrs3 = io.dpath.inst(48,41)
  val id_paddr   = io.dpath.inst(15,12)

  // only look at shared reg because addr reg can't be written during vf block
  val id_ctrl_wen_not0 = id_ctrl.vd_val && id_ctrl.vd_type === REG_SHR & id_waddr != UInt(0)
  val id_ctrl_rens1_not0 = sren1 && id_raddrs1 != UInt(0)
  val id_ctrl_rens2_not0 = sren2 && id_raddrs2 != UInt(0)
  val id_ctrl_rens3_not0 = sren3 && id_raddrs3 != UInt(0)

  // only set sboard if we were able to send the req
  val id_set_sboard = io.fpu.req.fire() || io.vmu.fire() && id_scalar_inst
  val id_scalar_store = id_ctrl.vmu_val && id_ctrl.vmu_cmd === M_XWR
  val id_sboard_addr = Mux(id_scalar_store, id_raddrs2, id_waddr)

  // stall for RAW hazards on non scalar integer pipes
  val id_can_bypass = id_scalar_inst && !id_ctrl.fpu_val && !id_ctrl.vmu_val
  val data_hazard_ex = Vec(
    id_ctrl_rens1_not0 && id_raddrs1 === io.dpath.ex_waddr,
    id_ctrl_rens2_not0 && id_raddrs2 === io.dpath.ex_waddr,
    id_ctrl_rens3_not0 && id_raddrs3 === io.dpath.ex_waddr)
  val id_ex_hazard = !id_can_bypass && ex_reg_valid && data_hazard_ex.reduce(_||_)

  // stall on RAW/WAW hazards on loads/fpu until data returns
  val id_sboard_hazard = 
    id_ctrl_rens1_not0 && sboard.read(id_raddrs1) ||
    id_ctrl_rens2_not0 && sboard.read(id_raddrs2) ||
    id_ctrl_rens3_not0 && sboard.read(id_raddrs3) ||
    id_ctrl_wen_not0 && sboard.read(id_waddr)

  sboard.set(id_set_sboard, id_sboard_addr)

  io.dpath.bypass := data_hazard_ex.map(ex_reg_valid && _)
 
  val enq_fpu = id_val && id_scalar_dest && id_ctrl.fpu_val
  val enq_vxu = id_val && !id_scalar_inst
  val enq_vmu = id_val && id_ctrl.vmu_val // both scalar and vector memops
  val enq_smu = enq_vmu && id_scalar_inst

  val mask_fpu_ready = !enq_fpu || io.fpu.req.ready
  val mask_vxu_ready = !enq_vxu || io.vxu.ready
  val mask_vmu_ready = !enq_vmu || io.vmu.ready

  val stall_pending_fpu = (id_ctrl.decode_stop || enq_fpu) && pending_fpu
  val stall_pending_smu = (id_ctrl.decode_stop || enq_smu) && pending_smu

  val ctrl_stalld_common =
    !vf_active || id_ex_hazard || id_sboard_hazard || stall_pending_fpu || stall_pending_smu

  def fire_decode(exclude: Bool, include: Bool*) = {
    val rvs = Seq(!ctrl_stalld_common, mask_fpu_ready, mask_vxu_ready, mask_vmu_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  // stall fetch/decode if we aren't ready to issue the op being decoded
  io.dpath.stalld := !fire_decode(null) || io.dpath.stallx || io.dpath.stallw
  io.dpath.killd :=
    !io.imem.resp.valid || io.dpath.stalld ||
    fire_decode(null, !id_scalar_inst || enq_fpu || enq_smu)

  // use rm in inst unless its dynamic then take rocket rm
  // TODO: pipe rockets rm here (FPU outputs it?, or store it in rocc unit)
  val _rm = io.dpath.inst(52, 50)
  val rm = Mux(_rm === Bits("b111"), UInt(0), _rm)
  val in_fmt = io.dpath.inst(54,53)
  val out_fmt = io.dpath.inst(56,55)

  // to FPU
  io.fpu.req.bits <> id_ctrl.fpu_fn
  io.fpu.req.valid := fire_decode(mask_fpu_ready, enq_fpu)
  io.dpath.pending_fpu_reg := pending_fpu_reg
  io.dpath.pending_fpu_typ := pending_fpu_typ
  io.dpath.pending_fpu_fn := pending_fpu_fn
  when (io.fpu.req.fire()) {
    pending_fpu := Bool(true)
    pending_fpu_typ := Mux(id_ctrl.fpu_fn.fromint, in_fmt, out_fmt)
    pending_fpu_reg := id_waddr
    pending_fpu_fn := id_ctrl.fpu_fn
  }

  // to VXU
  io.vxu.valid := fire_decode(mask_vxu_ready, enq_vxu)
  io.vxu.bits.vlen := vlen
  io.vxu.bits.active.vint := id_ctrl.active_vint()
  io.vxu.bits.active.vipred := id_ctrl.active_vipred()
  io.vxu.bits.active.vimul := id_ctrl.active_vimul()
  io.vxu.bits.active.vidiv := id_ctrl.active_vidiv()
  io.vxu.bits.active.vfma := id_ctrl.active_vfma()
  io.vxu.bits.active.vfdiv := id_ctrl.active_vfdiv()
  io.vxu.bits.active.vfcmp := id_ctrl.active_vfcmp()
  io.vxu.bits.active.vfconv := id_ctrl.active_vfconv()
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
      id_ctrl.vmu_val  -> id_ctrl.fn_vmu().toBits
    ))
  io.vxu.bits.reg.vs1.valid := id_ctrl.vs1_val
  io.vxu.bits.reg.vs2.valid := id_ctrl.vs2_val
  io.vxu.bits.reg.vs3.valid := id_ctrl.vs3_val
  io.vxu.bits.reg.vd.valid := id_ctrl.vd_val
  io.vxu.bits.reg.vs1.scalar := id_scalar_src1
  io.vxu.bits.reg.vs2.scalar := id_scalar_src2
  io.vxu.bits.reg.vs3.scalar := id_scalar_src3
  io.vxu.bits.reg.vd.scalar := id_scalar_dest
  io.vxu.bits.reg.vs1.pred := id_ctrl.vs1_type === REG_PRED
  io.vxu.bits.reg.vs2.pred := id_ctrl.vs2_type === REG_PRED
  io.vxu.bits.reg.vs3.pred := id_ctrl.vs3_type === REG_PRED
  io.vxu.bits.reg.vd.pred := id_ctrl.vd_type === REG_PRED
  io.vxu.bits.reg.vs1.id := id_raddrs1
  io.vxu.bits.reg.vs2.id := id_raddrs2
  io.vxu.bits.reg.vs3.id := id_raddrs3
  io.vxu.bits.reg.vd.id := id_waddr
  io.vxu.bits.reg.vp.valid := id_ctrl.vp_val
  io.vxu.bits.reg.vp.pred := Bool(true)
  io.vxu.bits.reg.vp.neg() := id_ctrl.vp_neg
  io.vxu.bits.reg.vp.id := id_paddr

  // to VMU
  io.vmu.valid := fire_decode(mask_vmu_ready, enq_vmu)
  io.vmu.bits.fn.mode := id_ctrl.vmu_mode
  io.vmu.bits.fn.cmd := id_ctrl.vmu_cmd
  io.vmu.bits.fn.mt := id_ctrl.vmu_mt
  io.vmu.bits.vlen := Mux(id_scalar_inst, UInt(1), vlen)
  when (io.vmu.fire() && id_scalar_inst) {
    pending_smu := Bool(true)
    pending_smu_store := id_ctrl.vmu_cmd === M_XWR
  }

  // EXECUTE
  // if we didn't stallx (which includes stallw) then we should move the pipe forward
  when (!io.dpath.stallx) {
    ex_reg_valid := !io.dpath.killd
    ex_ctrl := id_ctrl
  }

  val ex_stall_fpu = ex_reg_valid && io.fpu.resp.valid
  val ex_stall_smu = ex_reg_valid && io.dmem.valid && !pending_smu_store

  io.dpath.stallx := ex_stall_fpu || ex_stall_smu || io.dpath.stallw
  io.dpath.killx := !ex_reg_valid || io.dpath.stallx

  // give fixed priority to fpu -> mem -> alu
  io.fpu.resp.ready := Bool(true)
  io.dmem.ready := !io.fpu.resp.valid

  // WRITEBACK
  val wb_fpu_valid = Reg(next=io.fpu.resp.valid)
  val wb_dmem_valid = Reg(next=io.dmem.valid && !io.fpu.resp.valid)
  val wb_dmem_load_valid = wb_dmem_valid && !pending_smu_store
  val wb_dmem_waddr = RegEnable(io.dmem.bits.id, io.dmem.valid)

  when (!io.dpath.stallw) {
    wb_reg_valid := !io.dpath.killx
    wb_ctrl := ex_ctrl
  }

  io.dpath.stallw := Bool(false)

  io.dpath.wb_valid := wb_reg_valid
  io.dpath.wb_wen := wb_fpu_valid || wb_dmem_load_valid || wb_reg_valid
  io.dpath.wb_fpu_valid := wb_fpu_valid
  io.dpath.wb_dmem_load_valid := wb_dmem_load_valid
  io.dpath.wb_dmem_waddr := wb_dmem_waddr

  // clear ll sb at cycle of wb not the cycle result returned
  val clear_fpu = wb_fpu_valid
  val clear_mem = wb_dmem_valid

  val sboard_clear_addr = Mux(clear_fpu, pending_fpu_reg, wb_dmem_waddr)
  sboard.clear(clear_mem || clear_fpu, sboard_clear_addr)

  // TODO: this is conservative in that we might be able to get an fpu resp and send out the req in the same cycle rather than waiting 1 cycle (see fpu.req.valid)
  when (clear_fpu) { pending_fpu := Bool(false) }
  when (clear_mem) { pending_smu := Bool(false) }

  assert(!(!vf_active && sboard_not_empty), "vf should not end with non empty scoreboard")
  assert(!(wb_dmem_load_valid && wb_reg_valid), "load result and scalar wb conflict")
  assert(!(wb_fpu_valid && wb_reg_valid), "fpu result and scalar wb conflict")
}

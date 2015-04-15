package hwacha

import Chisel._
import Node._
import Constants._
import rocket.ALU._
import uncore.constants.MemoryOpConstants._
import ScalarFPUDecode._

class CtrlDpathIO extends HwachaBundle {
  val inst = Bits(INPUT, 64)
  val ex_inst = Bits(INPUT, 64)
  val wb_inst = Bits(INPUT, 64)
  val killf = Bool(OUTPUT)
  val fire_vf = Bool(OUTPUT)
  val killd = Bool(OUTPUT)
  val id_ctrl = new IntCtrlSigs().asOutput()
  val sren = Vec.fill(3)(Bool(OUTPUT))
  val aren = Vec.fill(2)(Bool(OUTPUT))
  val ex_scalar_dest = Bool(OUTPUT)
  val ex_ctrl = new IntCtrlSigs().asOutput()
  val ex_valid = Bool(OUTPUT)
  val ex_waddr = Bits(INPUT, log2Up(nsregs))
  val wb_ctrl = new IntCtrlSigs().asOutput()
  val wb_valid = Bool(OUTPUT)
  val wb_waddr = Bits(INPUT, log2Up(nsregs))
  val wb_wen = Bool(OUTPUT)
  val retire = Bool(OUTPUT)
  val bypass = Vec.fill(3)(Bool(OUTPUT))
  val bypass_src = Vec.fill(3)(Bits(OUTPUT, SZ_BYP))
  val pending_fpu_reg = UInt(OUTPUT,log2Up(nsregs))
  val pending_fpu_typ = UInt(OUTPUT,SZ_PREC)
  val swrite = Bool(OUTPUT)
  val awrite = Bool(OUTPUT)
  val wb_vf_active = Bool(OUTPUT)
}

class ScalarCtrl(resetSignal: Bool = null) extends HwachaModule(_reset = resetSignal) {
  import Commands._

  val io = new Bundle {
    val cmdq = new CMDQIO().flip

    val dpath = new CtrlDpathIO
    val fpu = new Bundle {
      val req = Decoupled(new rocket.FPInput())
      val resp = Decoupled(new rocket.FPResult()).flip
    }

    val vmu = Decoupled(new VMUOp)
    val dmem = new ScalarMemIO().flip

    val imem = new FrontendIO

    val vxu = new VXUIssueOpIO

    val vf_active = Bool(OUTPUT)
    val pending_memop = Bool(OUTPUT)
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
  val sboard = new Scoreboard(nsregs)
  val sboard_not_empty = (0 until nsregs).map(i => sboard.read(UInt(i))).reduce(_||_)
  val pending_fpu = Reg(init=Bool(false))
  val pending_fpu_reg = Reg(init=UInt(width=log2Up(nsregs)))
  val pending_fpu_typ = Reg(init=Bits(width=SZ_PREC))

  val vf_active     = Reg(init=Bool(false))
  val ex_vf_active  = Reg(init=Bool(false))
  val wb_vf_active  = Reg(init=Bool(false))
  val vl            = Reg(init=UInt(0, szvlen+1))
  val vregs         = Reg(init=UInt(32, szvregs+1))
  val pregs         = Reg(init=UInt(0, 5))

  val pending_memop = Reg(init=Bool(false))

  io.vf_active     := vf_active
  io.pending_memop := pending_memop

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

  io.dpath.swrite := fire(null,decode_vmss) 
  io.dpath.awrite := fire(null,decode_vmsa) 

  io.cmdq.cmd.ready := fire(io.cmdq.cmd.valid)
  io.cmdq.imm.ready := fire(mask_imm_valid, deq_imm)
  io.cmdq.rd.ready  := fire(mask_rd_valid, deq_rd)

  //decode
  val decode_table = ScalarDecode.table ++ VectorMemoryDecode.table ++ VectorArithmeticDecode.table
  val id_ctrl = new IntCtrlSigs().decode(io.dpath.inst, decode_table)
  val ex_ctrl = Reg(new IntCtrlSigs)
  val wb_ctrl = Reg(new IntCtrlSigs)
  io.dpath.id_ctrl := id_ctrl
  io.dpath.ex_ctrl := ex_ctrl
  io.dpath.wb_ctrl := wb_ctrl

  val ex_reg_valid = Reg(Bool())
  val wb_reg_valid = Reg(Bool())
  val wb_reg_replay = Reg(Bool())

  val ctrl_killd = Bool()
  val ctrl_killx = Bool()
  val ctrl_killm = Bool()

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      !vf_active, io.cmdq.cmd.valid,
      mask_imm_valid, mask_rd_valid)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  when (fire(null,decode_vsetcfg)) {
    vl    := io.cmdq.imm.bits(vl.getWidth, 0)
    vregs := io.cmdq.imm.bits(vl.getWidth+vregs.getWidth, vl.getWidth+1)
    pregs := io.cmdq.imm.bits(vl.getWidth+vregs.getWidth+pregs.getWidth, vl.getWidth+vregs.getWidth+1)
  }
  when (fire(null,decode_vsetvl)) {
    vl    := io.cmdq.imm.bits(vl.getWidth, 0)
  }
  io.dpath.fire_vf := Bool(false)
  when (fire(null,decode_vf)) {
    vf_active := Bool(true)
    io.dpath.fire_vf := Bool(true)
  }
  io.dpath.wb_vf_active    := wb_vf_active

  val vd_val  :: vd_scalar  :: vd_sp  :: vd_dyn  :: Nil = parse_rinfo(id_ctrl.vdi)
  val vs1_val :: vs1_scalar :: vs1_sp :: vs1_dyn :: Nil = parse_rinfo(id_ctrl.vs1i)
  val vs2_val :: vs2_scalar :: vs2_sp :: vs2_dyn :: Nil = parse_rinfo(id_ctrl.vs2i)
  val vs3_val :: vs3_scalar :: vs3_sp :: vs3_dyn :: Nil = parse_rinfo(id_ctrl.vs3i)
  val ren1 = vs1_val && (vs1_scalar || (vs1_dyn && !io.dpath.inst(OPC_VS1)))
  val ren2 = vs2_val && (vs2_scalar || (vs2_dyn && !io.dpath.inst(OPC_VS2)))
  val ren3 = vs3_val && (vs3_scalar || (vs3_dyn && !io.dpath.inst(OPC_VS3)))
  io.dpath.sren(0) := ren1 && vs1_sp
  io.dpath.sren(1) := ren2 && vs2_sp
  io.dpath.sren(2) := ren3 && vs3_sp
  io.dpath.aren(0) := ren1 && !vs1_sp
  io.dpath.aren(1) := ren2 && !vs2_sp

  val ex_vd_val :: ex_vd_scalar :: ex_vd_sp :: ex_vd_dyn :: Nil = parse_rinfo(ex_ctrl.vdi)

  val wb_vd_val :: wb_vd_scalar :: wb_vd_sp :: wb_vd_dyn :: Nil = parse_rinfo(wb_ctrl.vdi)

  val id_scalar_dest = vd_val && (id_ctrl.decode_scalar || vd_scalar || vd_dyn && !io.dpath.inst(OPC_VD))
  val id_scalar_src1 = vs1_val && (id_ctrl.decode_scalar || vs1_scalar || vs1_dyn && !io.dpath.inst(OPC_VS1))
  val id_scalar_src2 = vs2_val && (id_ctrl.decode_scalar || vs2_scalar || vs2_dyn && !io.dpath.inst(OPC_VS2))
  val id_scalar_src3 = vs3_val && (id_ctrl.decode_scalar || vs3_scalar || vs3_dyn && !io.dpath.inst(OPC_VS3))

  val id_val = io.imem.resp.valid && id_ctrl.ival
  val id_scalar_inst =
    (!vd_val || id_scalar_dest) &&
    (!vs1_val || id_scalar_src1) && (!vs2_val || id_scalar_src2) && (!vs3_val || id_scalar_src3)

  //COLIN FIXME: only send over dynamic bits from ex/wb_inst ala rockets ex_waddr
  val ex_scalar_dest = (ex_ctrl.decode_scalar || (ex_vd_val && ex_vd_scalar) || (ex_vd_dyn && !io.dpath.ex_inst(OPC_VD)))
  io.dpath.ex_scalar_dest := Bool(true)
  when(ex_vf_active && !ctrl_killx) { io.dpath.ex_scalar_dest := ex_scalar_dest }

  val wb_scalar_dest = !wb_ctrl.fpu_val && (wb_ctrl.decode_scalar || (wb_vd_val && wb_vd_scalar) || (wb_vd_dyn && !io.dpath.wb_inst(OPC_VD)))

  val id_waddr = io.dpath.inst(23,16)
  val id_raddrs1 = io.dpath.inst(31,24)
  val id_raddrs2 = io.dpath.inst(40,33)
  val id_raddrs3 = io.dpath.inst(48,41)

  val ex_waddr   = io.dpath.ex_waddr

  val wb_waddr   = io.dpath.wb_waddr

  val bypassDst = Array(id_raddrs1, id_raddrs2, id_raddrs3)
  val bypassSrc = Array.fill(NBYP)((Bool(true), UInt(0)))
  bypassSrc(BYP_EX) = (ex_reg_valid && ex_scalar_dest, ex_waddr)

  val doBypass = bypassDst.map(d => bypassSrc.map(s => s._1 && s._2 === d))
  for (i <- 0 until io.dpath.bypass.size) {
    io.dpath.bypass(i) := doBypass(i).reduce(_||_)
    io.dpath.bypass_src(i) := PriorityEncoder(doBypass(i))
  }

  val id_ctrl_wen_not0 = vd_val && vd_scalar && vd_sp && id_waddr != UInt(0)
  val id_ctrl_rens1_not0 = vs1_val && vs1_scalar && vs1_sp && id_raddrs1 != UInt(0)
  val id_ctrl_rens2_not0 = vs2_val && vs2_scalar && vs2_sp && id_raddrs2 != UInt(0)
  val id_ctrl_rens3_not0 = vs3_val && vs3_scalar && vs3_sp && id_raddrs3 != UInt(0)

  // stall for RAW/WAW hazards on memory/fpu
  val ex_cannot_bypass = (ex_ctrl.vmu_val || ex_ctrl.fpu_val)
  val data_hazard_ex = ex_scalar_dest &&
    (id_ctrl_rens1_not0 && id_raddrs1 === ex_waddr ||
     id_ctrl_rens2_not0 && id_raddrs2 === ex_waddr ||
     id_ctrl_wen_not0   && id_waddr  === ex_waddr)

  val id_ex_hazard = ex_reg_valid && (data_hazard_ex && ex_cannot_bypass)

  val id_set_sboard = io.fpu.req.fire() || (id_scalar_dest && io.vmu.fire())

  //stall on RAW/WAW hazards on loads/fpu until data returns
  //stall on WAW hazards on stores until translation succeeds
  val id_sboard_hazard = 
                   (id_ctrl_rens1_not0 && sboard.read(id_raddrs1) ||
                   id_ctrl_rens2_not0 && sboard.read(id_raddrs2) ||
                   id_ctrl_rens3_not0 && sboard.read(id_raddrs3) ||
                   id_ctrl_wen_not0 && sboard.read(id_waddr))

  sboard.set(id_set_sboard, id_waddr)


  //on vmu resp valid we clear the sboard for its addr
  //on fpu resp valid we clear the sboard for its addr
  val clear_mem = io.dmem.valid
  val clear_fpu = io.fpu.resp.valid
  val sboard_clear_addr = Mux(clear_mem, io.dmem.bits.id, pending_fpu_reg)
  sboard.clear(clear_mem || clear_fpu, sboard_clear_addr)
 
  val enq_fpu = id_val && id_ctrl.fpu_val
  val enq_vxu = id_val && !id_scalar_inst
  val enq_vmu = id_val && id_ctrl.vmu_val

  val mask_fpu_ready = !enq_fpu || io.fpu.req.ready
  val mask_vxu_ready = !enq_vxu || io.vxu.ready
  val mask_vmu_ready = !enq_vmu || io.vmu.ready

  val ctrl_stalld_common =
    !vf_active || id_ex_hazard || id_sboard_hazard ||
    (pending_fpu && id_ctrl.decode_stop) //stall stop on outstanding fpu/mem

  def fire_decode(exclude: Bool, include: Bool*) = {
    val rvs = Seq(!ctrl_stalld_common, mask_fpu_ready, mask_vxu_ready, mask_vmu_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  val ctrl_stalld = !fire_decode(null)

  //fetch
  io.imem.req.valid := fire(null,decode_vf)
  io.imem.active := vf_active || fire(null,decode_vf)
  io.imem.invalidate := Bool(false)
  io.imem.resp.ready := !ctrl_stalld

  val id_vf_active = vf_active && !fire_decode(null,id_ctrl.decode_stop)

  when(fire_decode(null,id_ctrl.decode_stop)) {
    vf_active := Bool(false)
  }

  // FIXME: need to take Rocket's rounding mode for dynamic RM
  val rm = io.dpath.inst(52, 50)

  // to FPU
  val fpu_fn = new rocket.FPUCtrlSigs().fromBits(id_ctrl.fpu_fn)
  io.fpu.req.bits := fpu_fn
  io.fpu.req.valid := fire_decode(mask_fpu_ready, enq_fpu)
  io.dpath.pending_fpu_reg := pending_fpu_reg
  io.dpath.pending_fpu_typ := pending_fpu_typ
  when(io.fpu.req.fire()){
    pending_fpu := Bool(true)
    pending_fpu_typ := io.dpath.inst(54,53)
    pending_fpu_reg := id_waddr
  }
  when(io.fpu.resp.fire()){
    pending_fpu := Bool(false)
  }

  // to VXU
  io.vxu.valid := fire_decode(mask_vxu_ready, enq_vxu)
  io.vxu.bits.vlen := vl
  io.vxu.bits.active.vint := id_ctrl.active_vint()
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
      id_ctrl.vimu_val -> id_ctrl.fn_vimu().toBits,
      id_ctrl.vidu_val -> id_ctrl.fn_vidu().toBits,
      id_ctrl.vfmu_val -> id_ctrl.fn_vfmu(rm).toBits,
      id_ctrl.vfdu_val -> id_ctrl.fn_vfdu(rm).toBits,
      id_ctrl.vfcu_val -> id_ctrl.fn_vfcu(rm).toBits,
      id_ctrl.vfvu_val -> id_ctrl.fn_vfvu(rm).toBits,
      id_ctrl.vmu_val  -> id_ctrl.fn_vmu().toBits
    ))
  io.vxu.bits.reg.vs1.valid := vs1_val
  io.vxu.bits.reg.vs2.valid := vs2_val
  io.vxu.bits.reg.vs3.valid := vs3_val
  io.vxu.bits.reg.vd.valid := vd_val
  io.vxu.bits.reg.vs1.scalar := id_scalar_src1
  io.vxu.bits.reg.vs2.scalar := id_scalar_src2
  io.vxu.bits.reg.vs3.scalar := id_scalar_src3
  io.vxu.bits.reg.vd.scalar := id_scalar_dest
  io.vxu.bits.reg.vs1.id := id_raddrs1
  io.vxu.bits.reg.vs2.id := id_raddrs2
  io.vxu.bits.reg.vs3.id := id_raddrs3
  io.vxu.bits.reg.vd.id := id_waddr

  // to VMU
  io.vmu.valid := fire_decode(mask_vmu_ready, enq_vmu)
  io.vmu.bits.fn.mode := id_ctrl.vmu_mode
  io.vmu.bits.fn.cmd := id_ctrl.vmu_cmd
  io.vmu.bits.fn.mt := id_ctrl.vmu_mt
  io.vmu.bits.vlen := Mux(id_scalar_inst, UInt(1), vl)

  //excute
  ctrl_killd := !io.imem.resp.valid || ctrl_stalld 
  ex_reg_valid := !ctrl_killd
  io.dpath.ex_valid := ex_reg_valid
  io.dpath.killd := ctrl_stalld
  io.dpath.killf := !io.imem.resp.valid || ctrl_stalld || id_ctrl.decode_stop

  when (!ctrl_killd) {
    ex_ctrl := id_ctrl
  }
  when(!ctrl_killd || ex_ctrl.decode_stop) {
    ex_vf_active := vf_active
  }

  // stall inst in ex stage
  //if either the vmu or fpu needs to writeback we need to stall
  val ex_ll = ex_ctrl.vmu_val || ex_ctrl.fpu_val
  val vmu_stall_ex = ex_reg_valid && ex_scalar_dest && !ex_ll && io.dmem.valid
  val fpu_stall_ex = ex_reg_valid && ex_scalar_dest && !ex_ll && io.fpu.resp.valid

  val ctrl_stallx = ex_reg_valid && (vmu_stall_ex || fpu_stall_ex)

  //give fixed priority to mem -> fpu -> alu
  io.dmem.ready := Bool(true)
  io.fpu.resp.ready := !io.dmem.valid
 
  //writeback
  ctrl_killx := ctrl_stallx || !ex_reg_valid
  wb_reg_valid := !ctrl_killx
  io.dpath.wb_valid := wb_reg_valid

  when (!ctrl_killx) {
    wb_ctrl := ex_ctrl
  }
  when(!ctrl_killx || wb_ctrl.decode_stop) {
    wb_vf_active := ex_vf_active
  }

  assert(!(!vf_active && sboard_not_empty), "vf should not end with non empty scoreboard")
  assert(!(io.dmem.valid && wb_scalar_dest && wb_reg_valid), "load result and scalar wb conflict")

  io.dpath.retire := wb_reg_valid
  io.dpath.wb_wen := wb_vf_active && ((wb_reg_valid && wb_scalar_dest) || io.dmem.valid || io.fpu.resp.valid)
}

package hwacha

import Chisel._
import Node._
import Constants._
import rocket.ALU._
import uncore.constants.MemoryOpConstants._
import ScalarFPUDecode._

class CtrlDpathIO extends HwachaBundle
{
  val inst    = Bits(INPUT, 64)
  val ex_inst    = Bits(INPUT, 64)
  val wb_inst    = Bits(INPUT, 64)
  val killd   = Bool(OUTPUT)
  val ren     = Vec.fill(3)(Bool(OUTPUT))
  val ex_scalar_dest = Bool(OUTPUT)
  val ex_ctrl = new IntCtrlSigs().asOutput()
  val ex_valid = Bool(OUTPUT)
  val ex_waddr = Bits(INPUT, log2Up(nsregs))
  val wb_ctrl = new IntCtrlSigs().asOutput()
  val wb_valid = Bool(OUTPUT)
  val wb_waddr = Bits(INPUT, log2Up(nsregs))
  val wb_wen   = Bool(OUTPUT)
  val retire   = Bool(OUTPUT)
  val bypass = Vec.fill(3)(Bool(OUTPUT))
  val bypass_src = Vec.fill(3)(Bits(OUTPUT, SZ_BYP))
  val fire_vmu = Bool(OUTPUT)
  val pending_mem_reg = UInt(INPUT)
  val fire_fpu = Bool(OUTPUT)
  val pending_fpu = Bool(INPUT)
  val pending_fpu_reg = UInt(INPUT)
  val swrite  = Valid(new Write().asOutput())
  val awrite  = Valid(new Write().asOutput())
  val wb_vf_active = Bool(OUTPUT)
}

class ScalarCtrl(resetSignal: Bool = null) extends HwachaModule(_reset = resetSignal)
{
  import Commands._

  val io = new Bundle {
    val cmdq = new CMDQIO().flip

    val dpath = new CtrlDpathIO
    val fpu = new Bundle {
      val req = Decoupled(new rocket.FPInput())
      val resp = Decoupled(new rocket.FPResult()).flip
    }

    val vmu = new ScalarMemIO

    val imem = new FrontendIO

    val vf_active = Bool(OUTPUT)
    val pending_seq = Bool(OUTPUT)
    val pending_memop = Bool(OUTPUT)
  }

  class Scoreboard(n: Int)
  {
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
  val pending_mem = Reg(init=Bool(false))//scalar memop in flight

  val vf_active     = Reg(init=Bool(false))
  val ex_vf_active  = Reg(init=Bool(false))
  val wb_vf_active  = Reg(init=Bool(false))
  val vf_pc         = Reg(UInt())

  val pending_seq   = Reg(init=Bool(false))
  val pending_memop = Reg(init=Bool(false))

  io.vf_active    := vf_active
  io.pending_seq   := pending_seq
  io.pending_memop := pending_memop

  val decode_vmss = io.cmdq.cmd.valid && io.cmdq.cmd.bits === CMD_VMSS
  val decode_vmsa = io.cmdq.cmd.valid && io.cmdq.cmd.bits === CMD_VMSA
  val decode_vf   = io.cmdq.cmd.valid && io.cmdq.cmd.bits === CMD_VF
  val decode_vft  = io.cmdq.cmd.valid && io.cmdq.cmd.bits === CMD_VFT

  io.dpath.swrite.valid    := decode_vmss && io.cmdq.rd.valid
  io.dpath.swrite.bits.rd  := io.cmdq.rd.bits
  io.dpath.swrite.bits.imm := io.cmdq.imm.bits

  io.dpath.awrite.valid    := decode_vmsa && io.cmdq.rd.valid
  io.dpath.awrite.bits.rd  := io.cmdq.rd.bits
  io.dpath.awrite.bits.imm := io.cmdq.imm.bits

  io.cmdq.cmd.ready := !vf_active 
  io.cmdq.imm.ready := !vf_active
  io.cmdq.rd.ready  := !vf_active && (decode_vmss || decode_vmsa)

  //default values
  io.imem.req.valid := vf_active
  io.imem.req.bits.pc := vf_pc
  io.imem.req.bits.npc := vf_pc + UInt(8)
  io.imem.req.bits.nnpc := vf_pc + UInt(2*8)
  io.imem.invalidate := Bool(false)
  io.imem.resp.ready := vf_active

  //decode
  val id_ctrl = new IntCtrlSigs().decode(io.dpath.inst, HwachaVFDecodeTable.table)
  val ex_ctrl = Reg(new IntCtrlSigs)
  val wb_ctrl = Reg(new IntCtrlSigs)
  io.dpath.ex_ctrl := ex_ctrl
  io.dpath.wb_ctrl := wb_ctrl

  val ex_reg_valid = Reg(Bool())
  val wb_reg_valid = Reg(Bool())
  val wb_reg_replay = Reg(Bool())

  val ctrl_killd = Bool()
  val ctrl_killx = Bool()
  val ctrl_killm = Bool()

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Array(
      vf_active,
      io.cmdq.cmd.valid, io.cmdq.imm.valid)
    rvs.filter(_ != exclude).reduce(_&&_) && (Bool(true) :: include.toList).reduce(_&&_)
  }

  //start and stop vf block and i$ fetching
  when(fire(vf_active,decode_vf))
  {
    vf_active := Bool(true)
    vf_pc := io.cmdq.imm.bits
  }
  when(id_ctrl.decode_stop && vf_active)
  {
    vf_active := Bool(false)
  }
  io.dpath.wb_vf_active    := wb_vf_active

  val vd_val :: vd_scalar :: vd_sp :: vd_dyn :: Nil = parse_rinfo(id_ctrl.vdi)
  val vr_val :: vr_scalar :: vr_sp :: vr_dyn :: Nil = parse_rinfo(id_ctrl.vri)
  val vs_val :: vs_scalar :: vs_sp :: vs_dyn :: Nil = parse_rinfo(id_ctrl.vsi)
  val vt_val :: vt_scalar :: vt_sp :: vt_dyn :: Nil = parse_rinfo(id_ctrl.vti)
  io.dpath.ren(0) := vr_val && (vr_scalar || (vr_dyn && !io.dpath.inst(OPC_VS1)))
  io.dpath.ren(1) := vs_val && (vs_scalar || (vs_dyn && !io.dpath.inst(OPC_VS2)))
  io.dpath.ren(2) := vt_val && (vt_scalar || (vt_dyn && !io.dpath.inst(OPC_VS3)))

  val ex_vd_val :: ex_vd_scalar :: ex_vd_sp :: ex_vd_dyn :: Nil = parse_rinfo(ex_ctrl.vdi)

  val wb_vd_val :: wb_vd_scalar :: wb_vd_sp :: wb_vd_dyn :: Nil = parse_rinfo(wb_ctrl.vdi)

  val id_scalar_dest = !id_ctrl.fpu_val && (id_ctrl.decode_scalar || (vd_val && vd_scalar) || (vd_dyn && !io.dpath.inst(OPC_VD)))
  val id_scalar_src1 = id_ctrl.decode_scalar || (vr_val && vr_scalar) || (vr_dyn && !io.dpath.inst(OPC_VS1))
  val id_scalar_src2 = id_ctrl.decode_scalar || (vs_val && vs_scalar) || (vs_dyn && !io.dpath.inst(OPC_VS2))
  val id_scalar_src3 = id_ctrl.decode_scalar || (vt_val && vt_scalar) || (vt_dyn && !io.dpath.inst(OPC_VS3))

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

  //COLIN FIXME: do we have any scalar instructions with no destination?
  val id_scalar_inst = id_scalar_dest &&
                       (id_scalar_src1 && vr_val)
                       (id_scalar_src2 && vs_val)
                       (id_scalar_src3 && vt_val)
  //assuming all scalar instructions have a dest

  val bypassDst = Array(id_raddrs1, id_raddrs2, id_raddrs3)
  val bypassSrc = Array.fill(NBYP)((Bool(true), UInt(0)))
  bypassSrc(BYP_EX) = (ex_reg_valid && ex_scalar_dest, ex_waddr)

  val doBypass = bypassDst.map(d => bypassSrc.map(s => s._1 && s._2 === d))
  for (i <- 0 until io.dpath.bypass.size) {
    io.dpath.bypass(i) := doBypass(i).reduce(_||_)
    io.dpath.bypass_src(i) := PriorityEncoder(doBypass(i))
  }

  val id_ctrl_wen_not0 = vd_val && vd_scalar && vd_sp && id_waddr != UInt(0)
  val id_ctrl_rens1_not0 = vr_val && vr_scalar && vr_sp && id_raddrs1 != UInt(0)
  val id_ctrl_rens2_not0 = vs_val && vs_scalar && vs_sp && id_raddrs2 != UInt(0)
  val id_ctrl_rens3_not0 = vt_val && vt_scalar && vt_sp && id_raddrs3 != UInt(0)

  // stall for RAW/WAW hazards on loads, AMOs, and mul/div in execute stage.
  val ex_cannot_bypass = ex_ctrl.vmu_val
  val data_hazard_ex = ex_scalar_dest &&
    (id_ctrl_rens1_not0 && id_raddrs1 === ex_waddr ||
     id_ctrl_rens2_not0 && id_raddrs2 === ex_waddr ||
     id_ctrl_wen_not0   && id_waddr  === ex_waddr)

  val id_ex_hazard = ex_reg_valid && (data_hazard_ex && ex_cannot_bypass)

  val wb_set_sboard = wb_ctrl.vmu_val || wb_ctrl.fpu_val
  // stall for RAW/WAW hazards on load/AMO misses and mul/div in writeback.
  val data_hazard_wb = wb_scalar_dest &&
     (id_ctrl_rens1_not0 && id_raddrs1 === wb_waddr ||
      id_ctrl_rens2_not0 && id_raddrs2 === wb_waddr ||
      id_ctrl_wen_not0   && id_waddr  === wb_waddr)
  val id_wb_hazard = wb_reg_valid && (data_hazard_wb && wb_set_sboard)

  //Stall second load/store on decode
  val id_second_mem = id_ctrl.vmu_val && pending_mem
  //Stall second fpu on decode
  val fpu_hazard = id_ctrl.fpu_val && io.dpath.pending_fpu

  //stall on RAW/WAW hazards on loads until data returns
  //stall on WAW hazards on stores until translation succeeds
  //COLIN FIXME: The current logic doesn't check that the sboard is set
  //because of a load rather than another int op we can bypass from
  val mem_hazard = pending_mem && (
                   id_ctrl_rens1_not0 && sboard.read(id_raddrs1) ||
                   id_ctrl_rens2_not0 && sboard.read(id_raddrs2) ||
                   id_ctrl_rens3_not0 && sboard.read(id_raddrs3) ||
                   id_ctrl_wen_not0 && sboard.read(id_waddr) )

  val id_sboard_hazard = 
                   (id_ctrl_rens1_not0 && sboard.readBypassed(id_raddrs1) ||
                   id_ctrl_rens2_not0 && sboard.readBypassed(id_raddrs2) ||
                   id_ctrl_rens1_not0 && sboard.readBypassed(id_waddr))

  sboard.set(wb_set_sboard && io.dpath.wb_wen, wb_waddr)

  //on vmu resp valid we clear the sboard for its addr
  //on fpu resp valid we clear the sboard for its addr
  val clear_mem = io.vmu.resp.valid
  val clear_fpu = io.fpu.resp.valid
  val sboard_clear_addr = Mux(clear_mem, io.dpath.pending_mem_reg, io.dpath.pending_fpu_reg)
  sboard.clear(clear_mem || clear_fpu, sboard_clear_addr)
 
  val ctrl_stalld = id_ex_hazard || id_wb_hazard || 
                    id_sboard_hazard || id_second_mem || mem_hazard || fpu_hazard

  io.dpath.killd := !vf_active || ctrl_stalld

  //excute
  ctrl_killd := !vf_active || !io.imem.resp.valid || ctrl_stalld 
  ex_reg_valid := !ctrl_killd
  io.dpath.ex_valid := ex_reg_valid

  when (!ctrl_killd) {
    vf_pc := vf_pc + UInt(8)
    ex_ctrl := id_ctrl
  }
  when (!ctrl_stalld && !io.imem.resp.valid) { ex_vf_active := vf_active }

  // replay inst in ex stage
  val replay_ex_structural = ex_ctrl.vmu_val && pending_mem ||
                             ex_ctrl.fpu_val && !io.fpu.req.ready
  //if either the vmu or fpu needs to writeback we need to replay
  val vmu_kill_ex = ex_reg_valid && ex_scalar_dest && io.vmu.resp.valid
  val fpu_kill_ex = ex_reg_valid && ex_scalar_dest && io.fpu.resp.valid
  val replay_ex = ex_reg_valid && (replay_ex_structural || vmu_kill_ex || fpu_kill_ex)

  //fpu
  val fpu_fn = new rocket.FPUCtrlSigs().fromBits(ex_ctrl.fpu_fn)
  io.fpu.req.bits := fpu_fn
  io.fpu.req.valid := ex_reg_valid && ex_ctrl.fpu_val
  io.dpath.fire_fpu := Bool(false)
  when(io.fpu.req.fire()){
    io.dpath.fire_fpu := Bool(true)
  }

  //memory
  io.vmu.op.valid := ex_reg_valid && ex_ctrl.vmu_val
  io.vmu.op.bits.fn.cmd := ex_ctrl.vmu_cmd
  io.vmu.op.bits.fn.mt := ex_ctrl.vmu_type
  io.dpath.fire_vmu := Bool(false)
  when(io.vmu.op.fire()){
    io.dpath.fire_vmu := Bool(true)
    pending_mem := Bool(true)
  }
  when(clear_mem){
    pending_mem := Bool(false)
  }
 
  //writeback
  ctrl_killx := replay_ex || !ex_reg_valid 
  wb_reg_valid := !ctrl_killx
  wb_reg_replay := replay_ex
  io.dpath.wb_valid := wb_reg_valid

  val replay_wb = wb_reg_replay

  when (!ctrl_killx) {
    wb_ctrl := ex_ctrl
  }
  when (!ctrl_stalld && !io.imem.resp.valid) { wb_vf_active := ex_vf_active }

  assert(!(io.vmu.resp.valid && wb_scalar_dest && wb_reg_valid), "load result and scalar wb conflict")

  io.dpath.retire := wb_reg_valid && !replay_wb
  io.dpath.wb_wen := wb_vf_active && (wb_reg_valid && wb_scalar_dest) || io.vmu.resp.valid || io.fpu.resp.valid
  /*
  
  //COLIN FIXME: only recode when sending to shared rocket fpu
  val encode_sp = hardfloat.floatNToRecodedFloatN(io.vcmdq.imm1.bits, 23, 9)
  val encode_dp = hardfloat.floatNToRecodedFloatN(io.vcmdq.imm1.bits, 52, 12)

  */
}

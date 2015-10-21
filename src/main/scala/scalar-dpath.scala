package hwacha

import Chisel._
import DataGating._
import HardFloatHelper._

class Write(implicit p: Parameters) extends HwachaBundle()(p) {
  val rd  = Bits(width = bSDest)
  val imm = Bits(width = regLen)
}

class ScalarDpath(implicit p: Parameters) extends HwachaModule()(p) {
  val io = new Bundle {
    val cmdq = new CMDQIO().flip
    val ctrl = new CtrlDpathIO().flip
    val fpu = new Bundle {
      val req = Decoupled(new rocket.FPInput())
      val resp = Decoupled(new rocket.FPResult()).flip
    }

    val vmu = Decoupled(new VMUOp)
    val dmem = new ScalarMemIO().flip
    val vxu = new VXUIssueOpIO

    val imem = new FrontendIO
  }

  // Fetch/decode definitions
  val vf_pc = Reg(UInt())

  // execute definitions
  val ex_reg_pc = Reg(UInt())
  val ex_reg_inst = Reg(Bits())
  val ex_reg_bypass = Vec.fill(3){Reg(Bool())}
  val ex_reg_srs = Vec.fill(3){Reg(Bits())}
  val ex_reg_ars = Vec.fill(2){Reg(Bits())}

  // writeback definitions
  val wb_reg_pc = Reg(UInt())
  val wb_reg_inst = Reg(Bits())
  val wb_reg_wdata = Reg(Bits())
  val wb_reg_kill = Reg(Bool())

  class SRegFile {
    private val rf = Mem(UInt(width = regLen), nSRegs-1)
    private val reads = collection.mutable.ArrayBuffer[(UInt,UInt)]()
    private var canRead = true
    def read(addr: UInt) = {
      require(canRead)
      reads += addr -> UInt()
      reads.last._2 := Mux(addr != UInt(0), rf(~addr), UInt(0))
      reads.last._2
    }
    def write(addr: UInt, data: UInt) = {
      canRead = false
      when (addr != UInt(0)) {
        rf(~addr) := data
        for ((raddr, rdata) <- reads)
          when (addr === raddr) { rdata := data }
      }
    }
  }
  val srf = new SRegFile // doesn't have vs0
  val arf = Mem(UInt(width = 64), 32)

  // fetch/decode
  when(io.ctrl.fire_vf) { vf_pc := io.cmdq.imm.bits }
  when (!io.ctrl.stalld) { vf_pc := vf_pc + UInt(8) }
  io.imem.req.bits.pc := io.cmdq.imm.bits

  val id_inst = io.imem.resp.bits.data(0).toBits; require(p(rocket.FetchWidth) == 1)
  val id_pc   = io.imem.resp.bits.pc
  //register reads
  val id_sraddr = Vec(id_inst(31,24), id_inst(40,33), id_inst(48,41))
  val id_araddr = Vec(id_inst(28,24), id_inst(37,33))
  val id_sreads = id_sraddr.map(srf.read _ )
  val id_areads = id_araddr.map(arf(_))

  // immediate generation
  def imm(sel: Bits, inst: Bits) = {
    val sign = inst(63).toSInt
    val b30_3 = inst(62,35)
    val b2_0 = Mux(sel === IMM_I, inst(34,32), Bits(0))

    Cat(sign, b30_3, b2_0).toSInt
  }

  io.ctrl.inst := id_inst
  io.ctrl.ex_waddr := ex_reg_inst(23,16)
  io.ctrl.ex_inst := ex_reg_inst
  io.ctrl.wb_inst := wb_reg_inst

  // FPU
  // FIXME: need to take Rocket's rounding mode for dynamic RM
  val _rm = id_inst(52, 50)
  val rm = Mux(_rm === Bits("b111"), UInt(0), _rm)
  val in_fmt = id_inst(54,53)
  val out_fmt = id_inst(56,55)
  io.fpu.req.bits.rm := rm
  io.fpu.req.bits.typ := out_fmt
  io.fpu.req.bits.in1 := 
     Mux(io.ctrl.id_ctrl.fpu_fn.fromint, id_sreads(0),
     Mux(in_fmt === UInt(0), 
     Cat(SInt(-1,32),recode_sp(id_sreads(0))), recode_dp(id_sreads(0))))
  io.fpu.req.bits.in2 := 
     Mux(in_fmt === UInt(0), 
     Cat(SInt(-1,32),recode_sp(id_sreads(1))), recode_dp(id_sreads(1)))
  io.fpu.req.bits.in3 := 
     Mux(in_fmt === UInt(0), 
     Cat(SInt(-1,32),recode_sp(id_sreads(2))), recode_dp(id_sreads(2)))

  val unrec_s = ieee_sp(io.fpu.resp.bits.data)
  val unrec_d = ieee_dp(io.fpu.resp.bits.data)
  val unrec_fpu_resp = Mux(io.ctrl.pending_fpu_typ === UInt(0),
               Cat(Fill(32,unrec_s(31)),unrec_s), unrec_d)

  //Memory requests - COLIN FIXME: check for critical path (need reg?)
  val addr_stride = MuxLookup(io.ctrl.id_ctrl.vmu_mt, UInt(0),Seq(
                      MT_B->  UInt(1),
                      MT_BU-> UInt(1),
                      MT_H->  UInt(2),
                      MT_HU-> UInt(2),
                      MT_W->  UInt(4),
                      MT_WU-> UInt(4),
                      MT_D->  UInt(8) 
                    ))

  io.vmu.bits.base :=
    Mux(io.ctrl.aren(0), id_areads(0), id_sreads(0))
  io.vmu.bits.aux.union :=
    Mux(io.ctrl.aren(1), VMUAuxVector(id_areads(1)).toBits,
                         Mux(io.ctrl.id_ctrl.vmu_mode === MM_VU,
                           VMUAuxVector(addr_stride).toBits,
                           VMUAuxScalar(id_sreads(1),
                           Mux(io.ctrl.id_ctrl.vmu_cmd === M_XWR, 
                           id_inst(40,33), id_inst(23,16))).toBits))

  // execute
  when (!io.ctrl.killd) {
    ex_reg_pc := id_pc
    ex_reg_inst := id_inst
    ex_reg_bypass := io.ctrl.bypass
    for (i <- 0 until id_sreads.size) {
      when (io.ctrl.sren(i)) { ex_reg_srs(i) := id_sreads(i) }
    }
    for (i <- 0 until id_areads.size) {
      when (io.ctrl.aren(i)) { ex_reg_ars(i) := id_areads(i) }
    }
  }

  val ex_srs = for (i <- 0 until id_sreads.size)
    yield Mux(ex_reg_bypass(i), wb_reg_wdata, ex_reg_srs(i))

  val ex_imm = imm(io.ctrl.ex_ctrl.sel_imm, ex_reg_inst)
  val ex_op1 = MuxLookup(io.ctrl.ex_ctrl.alu_sel1, SInt(0), Seq(
    A1_RS1 -> ex_srs(0).toSInt,
    A1_PC -> ex_reg_pc.toSInt))
  val ex_op2 = MuxLookup(io.ctrl.ex_ctrl.alu_sel2, SInt(0), Seq(
    A2_ZERO -> SInt(0),
    A2_RS2 -> ex_srs(1).toSInt,
    A2_IMM -> ex_imm))

  val alu = Module(new rocket.ALU)
  alu.io.dw := io.ctrl.ex_ctrl.alu_dw
  alu.io.fn := io.ctrl.ex_ctrl.alu_fn
  alu.io.in2 := ex_op2.toUInt
  alu.io.in1 := ex_op1

  // Writeback stage
  when (!io.ctrl.killx) {
    wb_reg_pc := ex_reg_pc
    wb_reg_inst := ex_reg_inst
    wb_reg_wdata := alu.io.out
  }
  val wb_ll_wdata = Reg(next=
    Mux(io.fpu.resp.valid,
      Mux(io.ctrl.pending_fpu_fn.toint, io.fpu.resp.bits.data, unrec_fpu_resp),
        io.dmem.bits.data))

  val awrite_valid = io.ctrl.awrite
  val swrite_valid = io.ctrl.swrite
  val aswrite_rd   = io.cmdq.rd.bits
  val aswrite_imm  = io.cmdq.imm.bits

  assert(!(io.ctrl.wb_wen && swrite_valid), "Cannot write vmss and scalar dest")
  assert(!(io.ctrl.wb_wen && awrite_valid), "Cannot write vmsa and scalar dest")

  val wb_waddr = 
    Mux(io.ctrl.wb_fpu_valid, io.ctrl.pending_fpu_reg,
    Mux(io.ctrl.wb_dmem_load_valid, io.ctrl.wb_dmem_waddr,
      wb_reg_inst(23,16)))

  val wb_wdata = Mux(io.ctrl.wb_fpu_valid || io.ctrl.wb_dmem_load_valid, wb_ll_wdata, wb_reg_wdata)
  when (io.ctrl.wb_wen) { srf.write(wb_waddr, wb_wdata) }

  when(swrite_valid) { srf.write(aswrite_rd, aswrite_imm) }
  when(awrite_valid) { arf(aswrite_rd) := aswrite_imm }

  // to VXU
  io.vxu.bits.sreg.ss1 := id_sreads(0)
  io.vxu.bits.sreg.ss2 := id_sreads(1)
  io.vxu.bits.sreg.ss3 := id_sreads(2)

  when(swrite_valid) {
    printf("H: SW[r%d=%x][%d]\n",
         aswrite_rd, aswrite_imm, swrite_valid)
  }
  when(awrite_valid) {
    printf("H: AW[r%d=%x][%d]\n",
         aswrite_rd, aswrite_imm, awrite_valid)
  }
  when(io.ctrl.vf_active || io.ctrl.wb_valid) {
    printf("H: [%x] pc=[%x] SW[r%d=%x][%d] SR[r%d=%x] SR[r%d=%x] inst=[%x] DASM(%x)\n",
         io.ctrl.wb_valid, wb_reg_pc, 
         Mux(io.ctrl.wb_wen, wb_waddr, UInt(0)), wb_wdata, io.ctrl.wb_wen,
         wb_reg_inst(31,24), Mux(io.ctrl.wb_ctrl.vs1_type === REG_ADDR,
                                 Reg(next=Reg(next=ex_reg_ars(0))),
                                 Reg(next=Reg(next=ex_srs(0)))),
         wb_reg_inst(40,33), Mux(io.ctrl.wb_ctrl.vs2_type === REG_ADDR,
                                 Reg(next=Reg(next=ex_reg_ars(1))),
                                 Reg(next=Reg(next=ex_srs(1)))),
         wb_reg_inst, wb_reg_inst)
  }
}

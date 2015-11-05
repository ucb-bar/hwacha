package hwacha

import Chisel._
import cde.Parameters

class Write(implicit p: Parameters) extends HwachaBundle()(p) {
  val rd  = Bits(width = bSDest)
  val imm = Bits(width = regLen)
}

class ScalarDpath(implicit p: Parameters) extends HwachaModule()(p) {
  val io = new Bundle {
    val ctrl = new CtrlDpathIO().flip

    val cmdq = new CMDQIO().flip
    val imem = new FrontendIO
    val vxu = Decoupled(new IssueOpML)
    val vmu = Decoupled(new VMUOpML)
    val fpu = new Bundle {
      val req = Decoupled(new HwachaFPInput)
      val resp = Decoupled(new rocket.FPResult()).flip
    }
    val smu = new SMUIO
  }

  // Fetch/decode definitions
  val id_ctrl = io.ctrl.id_ctrl

  // execute definitions
  val ex_ctrl = io.ctrl.ex_ctrl
  val ex_reg_pc = Reg(UInt())
  val ex_reg_inst = Reg(Bits())
  val ex_reg_bypass = Vec.fill(3){Reg(Bool())}
  val ex_reg_srs = Vec.fill(3){Reg(Bits())}
  val ex_reg_ars = Vec.fill(2){Reg(Bits())}

  // writeback definitions
  val wb_ctrl = io.ctrl.wb_ctrl
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
  val srf = new SRegFile // doesn't have vs0
  val arf = Mem(UInt(width = 64), 32)

  // fetch/decode
  val ex_pc = UInt()
  io.imem.req.bits.pc := Mux(io.ctrl.ex_br_taken, ex_pc, io.cmdq.imm.bits)

  val id_inst = io.imem.resp.bits.data(0).toBits; require(p(rocket.FetchWidth) == 1)
  val id_pc   = io.imem.resp.bits.pc
  //register reads
  val id_sraddr = Vec(id_ctrl.vs1, id_ctrl.vs2, id_ctrl.vs3)
  val id_araddr = Vec(id_ctrl.as1(), id_ctrl.as2())
  val id_sreads = id_sraddr.map(srf.read(_))
  val id_areads = id_araddr.map(arf(_))

  // immediate generation
  def imm(sel: Bits, inst: Bits) = {
    val sign = inst(63).toSInt
    val b30_3 = inst(62,35)
    val b2_0 = Mux(sel === IMM_I, inst(34,32), Bits(0))
    val out = Cat(sign, b30_3, b2_0).toSInt
    Mux(sel === IMM_L, Cat(out, UInt(0, 32)).toSInt, out)
  }

  io.ctrl.id_inst := id_inst

  // FPU
  // FIXME: need to take Rocket's rounding mode for dynamic RM
  val rm = Mux(id_ctrl.rm === Bits("b111"), UInt(0), id_ctrl.rm)
  io.fpu.req.bits.rm := rm
  io.fpu.req.bits.typ := id_ctrl.out_fmt
  io.fpu.req.bits.in_fmt := id_ctrl.in_fmt
  io.fpu.req.bits.in1 := id_sreads(0)
  io.fpu.req.bits.in2 := id_sreads(1)
  io.fpu.req.bits.in3 := id_sreads(2)

  //Memory requests - COLIN FIXME: check for critical path (need reg?)
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
    Mux(io.ctrl.aren(0), id_areads(0), // unit-stride
      Mux(isAMO(id_ctrl.vmu_cmd), UInt(0), // AMO
        id_sreads(0))) // indexed
  io.vmu.bits.stride :=
    Mux(io.ctrl.aren(1), id_areads(1), // constant-stride
      addr_stride) // unit-stride

  io.smu.req.bits.addr := Mux(io.ctrl.aren(0), id_areads(0), id_sreads(0))
  io.smu.req.bits.data := id_sreads(1)
  io.smu.req.bits.tag := id_ctrl.vd

  // execute
  when (!io.ctrl.killd) {
    ex_reg_pc := id_pc
    ex_reg_inst := id_inst
    ex_reg_bypass := io.ctrl.ex_bypass
    for (i <- 0 until id_sreads.size) {
      when (io.ctrl.sren(i)) { ex_reg_srs(i) := id_sreads(i) }
    }
    for (i <- 0 until id_areads.size) {
      when (io.ctrl.aren(i)) { ex_reg_ars(i) := id_areads(i) }
    }
  }

  val ex_srs = for (i <- 0 until id_sreads.size)
    yield Mux(ex_reg_bypass(i), wb_reg_wdata, ex_reg_srs(i))

  val ex_imm = imm(ex_ctrl.sel_imm, ex_reg_inst)
  val ex_op1 = MuxLookup(ex_ctrl.alu_sel1, SInt(0), Seq(
    A1_ZERO -> SInt(0),
    A1_RS1  -> ex_srs(0).toSInt,
    A1_PC   -> ex_reg_pc.toSInt))
  val ex_op2 = MuxLookup(ex_ctrl.alu_sel2, SInt(0), Seq(
    A2_8    -> SInt(8),
    A2_RS2  -> ex_srs(1).toSInt,
    A2_IMM  -> ex_imm))

  val alu = Module(new rocket.ALU)
  alu.io.dw := ex_ctrl.alu_dw
  alu.io.fn := ex_ctrl.alu_fn
  alu.io.in2 := ex_op2.toUInt
  alu.io.in1 := ex_op1

  // vcjalr has vs1_val set, so take the base address from register
  // vcjal doesn't have vs1_val set, so take pc as base address
  ex_pc := Mux(ex_ctrl.vs1_val, ex_srs(0).toSInt, ex_reg_pc.toSInt) + ex_imm

  // Writeback stage
  when (!io.ctrl.killx) {
    wb_reg_pc := ex_reg_pc
    wb_reg_inst := ex_reg_inst
    wb_reg_wdata := alu.io.out
  }

  assert(!(io.ctrl.wb_wen && io.ctrl.swrite), "Cannot write vmss and scalar dest")
  assert(!(io.ctrl.wb_wen && io.ctrl.awrite), "Cannot write vmsa and scalar dest")

  val wb_waddr =
    Mux(io.ctrl.swrite, io.cmdq.rd.bits,
      Mux(io.ctrl.wb_ll_valid, io.ctrl.wb_ll_waddr, wb_ctrl.vd))
  val wb_wdata =
    Mux(io.ctrl.swrite, io.cmdq.imm.bits,
      Mux(io.ctrl.wb_ll_valid, io.ctrl.wb_ll_wdata, wb_reg_wdata))

  when (io.ctrl.wb_wen || io.ctrl.swrite) {
    srf.write(wb_waddr, wb_wdata)
    if (commit_log) printf("H: write_srf %d %x\n", wb_waddr, wb_wdata)
  }
  when (io.ctrl.awrite) {
    arf(io.cmdq.rd.bits) := io.cmdq.imm.bits
    if (commit_log) printf("H: write_arf %d %x\n", io.cmdq.rd.bits, io.cmdq.imm.bits)
  }

  // to VXU
  io.vxu.bits.sreg.ss1 := id_sreads(0)
  io.vxu.bits.sreg.ss2 := id_sreads(1)
  io.vxu.bits.sreg.ss3 := id_sreads(2)

  when(io.ctrl.vf_active || io.ctrl.wb_valid) {
    printf("H: [%x] pc=[%x] SW[r%d=%x][%d] SR[r%d=%x] SR[r%d=%x] inst=[%x] DASM(%x)\n",
         io.ctrl.wb_valid, wb_reg_pc, 
         Mux(io.ctrl.wb_wen, wb_waddr, UInt(0)), wb_wdata, io.ctrl.wb_wen,
         wb_reg_inst(31,24), Mux(wb_ctrl.vs1_type === REG_ADDR,
                                 Reg(next=Reg(next=ex_reg_ars(0))),
                                 Reg(next=Reg(next=ex_srs(0)))),
         wb_reg_inst(40,33), Mux(wb_ctrl.vs2_type === REG_ADDR,
                                 Reg(next=Reg(next=ex_reg_ars(1))),
                                 Reg(next=Reg(next=ex_srs(1)))),
         wb_reg_inst, wb_reg_inst)
  }
}

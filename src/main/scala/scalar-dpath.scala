package hwacha

import Chisel._
import Node._
import Constants._

class Write extends Bundle
{
  val rd  = Bits(width = 8)
  val imm = Bits(width = SZ_ADDR)
}

class ScalarDpath extends HwachaModule
{
  val io = new Bundle {
    val ctrl = new CtrlDpathIO().flip

    val vmu = new ScalarMemIO

    val imem = new rocket.CPUFrontendIO

    val respq = new RESPQIO()
  }


  // execute definitions
  val ex_reg_pc = Reg(UInt())
  val ex_reg_inst = Reg(Bits())
  val ex_reg_kill = Reg(Bool())
  val ex_reg_srs = Vec.fill(4)(Reg(Bits()))
  val ex_reg_ars = Vec.fill(2)(Reg(Bits()))

  // writeback definitions
  val wb_reg_pc = Reg(UInt())
  val wb_reg_inst = Reg(Bits())
  val wb_reg_wdata = Reg(Bits())
  val wb_wdata = Bits()

  class SRegFile {
    private val rf = Mem(UInt(width = 64), 255)
    private val reads = collection.mutable.ArrayBuffer[(UInt,UInt)]()
    private var canRead = true
    def read(addr: UInt) = {
      require(canRead)
      reads += addr -> UInt()
      reads.last._2 := rf(~addr)
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
  val srf = new SRegFile //doesn't have vs0
  val arf = Mem(UInt(width = 64), 32)

  io.respq.value.valid := Bool(false)
  io.respq.value.bits  := UInt(0)

  val id_inst = io.imem.resp.bits.data(0).toBits; require(params(rocket.FetchWidth) == 1)
  val id_pc   = io.imem.resp.bits.pc
  //register reads
  val id_sraddr = Vec(id_inst(31,24), id_inst(40,33), id_inst(48,41), id_inst(23,16))
  val id_araddr = Vec(id_inst(28,24), id_inst(37,33))
  val id_sreads = id_sraddr.map(srf.read(_) )
  val id_areads = id_araddr.map(arf(_))

  // immediate generation
  def imm(sel: Bits, inst: Bits) = {
    val sign = inst(63).toSInt
    val b30_3 = inst(62,35)
    val b2_0 = Mux(sel === IMM_I, inst(34,32), Bits(0))

    Cat(sign, b30_3, b2_0).toSInt
  }

  io.ctrl.inst := id_inst

  //execute stage
  ex_reg_kill := io.ctrl.killd
  when(!io.ctrl.killd)
  {
    ex_reg_pc := id_pc
    ex_reg_inst := id_inst
    for( i <- 0 until id_sreads.size){
      when(io.ctrl.ren(i)){
        ex_reg_srs(i) := id_sreads(i)
      }
    }
    for( i <- 0 until id_areads.size){
      when(io.ctrl.ren(i)){
        ex_reg_ars(i) := id_areads(i)
      }
    }
  }

  val ex_imm = imm(io.ctrl.ex_ctrl.sel_imm, ex_reg_inst)
  val ex_op1 = MuxLookup(io.ctrl.ex_ctrl.sel_alu1, SInt(0), Seq(
    A1_RS1 -> ex_reg_srs(0).toSInt,
    A1_PC -> ex_reg_pc.toSInt))
  val ex_op2 = MuxLookup(io.ctrl.ex_ctrl.sel_alu2, SInt(0), Seq(
    A2_RS2 -> ex_reg_srs(1).toSInt,
    A2_IMM -> ex_imm))

  val alu = Module(new rocket.ALU)
  alu.io.dw := io.ctrl.ex_ctrl.alu_dw
  alu.io.fn := io.ctrl.ex_ctrl.alu_fn
  alu.io.in2 := ex_op2.toUInt
  alu.io.in1 := ex_op1

  //Memory requests - COLIN FIXME: check for criticla path (need reg?)
  io.vmu.op.bits.addr := alu.io.out
  //COLIN FIXME: we need to have store data come from a different reg
  io.vmu.op.bits.data := ex_reg_srs(3)

  //Writeback stage
  when(!ex_reg_kill) {
    wb_reg_pc := ex_reg_pc
    wb_reg_inst := ex_reg_inst
    wb_reg_wdata := alu.io.out
  }

  assert(!(io.ctrl.wb_wen && io.ctrl.swrite.valid), "Cannot write vmss and scalar dest")
  assert(!(io.ctrl.wb_wen && io.ctrl.awrite.valid), "Cannot write vmsa and scalar dest")

  val wb_waddr = Mux(io.vmu.loadData.valid, io.ctrl.mem_pending_reg, wb_reg_inst(23,16))
  wb_wdata := wb_reg_wdata
  when(io.ctrl.wb_wen) { srf.write(wb_waddr, wb_wdata) }

  when(io.ctrl.swrite.valid) { srf.write(io.ctrl.swrite.bits.rd, io.ctrl.swrite.bits.imm) }

  when(io.ctrl.awrite.valid) { arf(io.ctrl.awrite.bits.rd) := io.ctrl.awrite.bits.imm }

  //hookup result to rocc resp
  when(io.ctrl.swrite.valid) { 
    io.respq.value.valid := Bool(true)
    io.respq.value.bits := wb_reg_wdata
  }
  when(io.ctrl.awrite.valid) { 
    io.respq.value.valid := Bool(true)
    io.respq.value.bits := wb_reg_wdata
  }
}

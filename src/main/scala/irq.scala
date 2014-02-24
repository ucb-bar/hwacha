package hwacha

import Chisel._
import Node._
import Constants._

class IRQIssueTVECIO extends Bundle
{
  val illegal = Bool(OUTPUT)
  val cmd = Bits(OUTPUT, SZ_VCMD)
}

class IRQIssueVTIO extends Bundle
{
  val ma_inst = Bool(OUTPUT)
  val fault_inst = Bool(OUTPUT)
  val illegal = Bool(OUTPUT)
  val pc = Bits(OUTPUT, SZ_ADDR)
}

class IRQIssueIO extends Bundle
{
  val tvec = new IRQIssueTVECIO()
  val vt = new IRQIssueVTIO()
}

class IRQVMUIO extends Bundle
{
  val ma_ld = Bool(OUTPUT)
  val ma_st = Bool(OUTPUT)
  val faulted_ld = Bool(OUTPUT)
  val faulted_st = Bool(OUTPUT)
  val mem_xcpt_addr = Bits(OUTPUT, SZ_ADDR)
}

class IRQ(resetSignal: Bool = null) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val issue = new IRQIssueIO().flip
    val vmu = new IRQVMUIO().flip

    val irq = Bool(OUTPUT)
    val irq_cause = UInt(OUTPUT, 5)
    val irq_aux = Bits(OUTPUT, 64)
  }

  val reg_irq_ma_inst = Reg(init=Bool(false))
  val reg_irq_fault_inst = Reg(init=Bool(false))
  val reg_irq_illegal_vt = Reg(init=Bool(false))
  val reg_irq_illegal_tvec = Reg(init=Bool(false))
  val reg_irq_pc = Reg(Bits(width = SZ_ADDR))
  val reg_irq_cmd_tvec = Reg(Bits(width = SZ_VCMD))

  val reg_irq_ma_ld = Reg(init=Bool(false))
  val reg_irq_ma_st = Reg(init=Bool(false))
  val reg_irq_faulted_ld = Reg(init=Bool(false))
  val reg_irq_faulted_st = Reg(init=Bool(false))
  val reg_mem_xcpt_addr = Reg(Bits(width = SZ_ADDR))

  when (!io.irq)
  {
    reg_irq_ma_inst := io.issue.vt.ma_inst
    reg_irq_fault_inst := io.issue.vt.fault_inst
    reg_irq_illegal_vt := io.issue.vt.illegal
    reg_irq_illegal_tvec := io.issue.tvec.illegal

    reg_irq_ma_ld := io.vmu.ma_ld
    reg_irq_ma_st := io.vmu.ma_st
    reg_irq_faulted_ld := io.vmu.faulted_ld
    reg_irq_faulted_st := io.vmu.faulted_st

    when (io.issue.vt.ma_inst || io.issue.vt.fault_inst || io.issue.vt.illegal) { reg_irq_pc := io.issue.vt.pc }
    when (io.issue.tvec.illegal) { reg_irq_cmd_tvec := io.issue.tvec.cmd }

    when (io.vmu.ma_ld || io.vmu.ma_st || io.vmu.faulted_ld || io.vmu.faulted_st) 
    {
      reg_mem_xcpt_addr := io.vmu.mem_xcpt_addr
    }
  }

  val dmem_xcpt = reg_irq_ma_ld || reg_irq_ma_st || reg_irq_faulted_ld || reg_irq_faulted_st

  io.irq := reg_irq_ma_inst || reg_irq_fault_inst || reg_irq_illegal_vt || reg_irq_illegal_tvec || dmem_xcpt

  io.irq_cause :=
    Mux(reg_irq_ma_inst, UInt(4),
    Mux(reg_irq_fault_inst, UInt(5),
    Mux(reg_irq_illegal_vt, UInt(6),
    Mux(reg_irq_illegal_tvec, UInt(1),
    Mux(reg_irq_ma_ld, UInt(8),
    Mux(reg_irq_ma_st, UInt(9),
    Mux(reg_irq_faulted_ld, UInt(10),
    Mux(reg_irq_faulted_st, UInt(11),
        UInt(1)))))))))

  io.irq_aux :=
    Mux(reg_irq_ma_inst || reg_irq_fault_inst || reg_irq_illegal_vt, reg_irq_pc,
    Mux(reg_irq_illegal_tvec, reg_irq_cmd_tvec,
    Mux(dmem_xcpt, reg_mem_xcpt_addr,
        Bits(0))))
}

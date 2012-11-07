package hwacha

import Chisel._
import Node._
import Constants._

class io_irq_to_issue extends Bundle
{
  val stall_tvec = Bool(OUTPUT)
  val stall_vf = Bool(OUTPUT)
}

class io_irq_handler extends Bundle
{
  val issue_to_irq = new io_issue_to_irq_handler().flip
  val vmu_to_irq = new io_vmu_to_irq_handler().flip

  val irq = Bool(OUTPUT)
  val irq_cause = UFix(OUTPUT, 5)
  val irq_aux = Bits(OUTPUT, 64)
}

class vuIRQHandler(resetSignal: Bool = null) extends Component(resetSignal)
{
  val io = new io_irq_handler()

  val reg_irq_ma_inst = Reg(resetVal = Bool(false))
  val reg_irq_fault_inst = Reg(resetVal = Bool(false))
  val reg_irq_illegal_vt = Reg(resetVal = Bool(false))
  val reg_irq_illegal_tvec = Reg(resetVal = Bool(false))
  val reg_irq_pc = Reg(){ Bits(width = SZ_ADDR) }
  val reg_irq_cmd_tvec = Reg(){ Bits(width = SZ_XCMD) }

  val reg_irq_ma_ld = Reg(resetVal = Bool(false))
  val reg_irq_ma_st = Reg(resetVal = Bool(false))
  val reg_irq_faulted_ld = Reg(resetVal = Bool(false))
  val reg_irq_faulted_st = Reg(resetVal = Bool(false))
  val reg_mem_xcpt_addr = Reg(){ Bits(width = SZ_ADDR) }

  when (!io.irq)
  {
    reg_irq_ma_inst := io.issue_to_irq.vt.ma_inst
    reg_irq_fault_inst := io.issue_to_irq.vt.fault_inst
    reg_irq_illegal_vt := io.issue_to_irq.vt.illegal
    reg_irq_illegal_tvec := io.issue_to_irq.tvec.illegal

    reg_irq_ma_ld := io.vmu_to_irq.ma_ld
    reg_irq_ma_st := io.vmu_to_irq.ma_st
    reg_irq_faulted_ld := io.vmu_to_irq.faulted_ld
    reg_irq_faulted_st := io.vmu_to_irq.faulted_st

    when (io.issue_to_irq.vt.ma_inst || io.issue_to_irq.vt.fault_inst || io.issue_to_irq.vt.illegal) { reg_irq_pc := io.issue_to_irq.vt.pc }
    when (io.issue_to_irq.tvec.illegal) { reg_irq_cmd_tvec := io.issue_to_irq.tvec.cmd }

    when (io.vmu_to_irq.ma_ld || io.vmu_to_irq.ma_st || io.vmu_to_irq.faulted_ld || io.vmu_to_irq.faulted_st) 
    {
      reg_mem_xcpt_addr := io.vmu_to_irq.mem_xcpt_addr
    }
  }

  val dmem_xcpt = reg_irq_ma_ld || reg_irq_ma_st || reg_irq_faulted_ld || reg_irq_faulted_st

  io.irq := reg_irq_ma_inst || reg_irq_fault_inst || reg_irq_illegal_vt || reg_irq_illegal_tvec || dmem_xcpt

  io.irq_cause :=
    Mux(reg_irq_ma_inst, UFix(24),
    Mux(reg_irq_fault_inst, UFix(25),
    Mux(reg_irq_illegal_vt, UFix(26),
    Mux(reg_irq_illegal_tvec, UFix(27),
    Mux(reg_irq_ma_ld, UFix(28),
    Mux(reg_irq_ma_st, UFix(29),
    Mux(reg_irq_faulted_ld, UFix(30),
    Mux(reg_irq_faulted_st, UFix(31),
        UFix(31)))))))))

  io.irq_aux :=
    Mux(reg_irq_ma_inst || reg_irq_fault_inst || reg_irq_illegal_vt, reg_irq_pc,
    Mux(reg_irq_illegal_tvec, reg_irq_cmd_tvec,
    Mux(dmem_xcpt, reg_mem_xcpt_addr,
        Bits(0))))
}

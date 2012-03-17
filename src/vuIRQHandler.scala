package hwacha

import Chisel._
import Node._
import Constants._

class io_irq_to_vxu extends Bundle
{

}

class io_irq_to_seq extends Bundle
{

}

class io_irq_to_issue extends Bundle
{
  val stall_tvec = Bool(OUTPUT)
  val stall_vf = Bool(OUTPUT)
}

class io_irq_handler extends Bundle
{
  val flush = Bool(INPUT)

  val irq_illegal_tvec = Bool(INPUT)
  val irq_cmd_tvec = Bits(SZ_XCMD, INPUT)

  val irq_ma_inst = Bool(INPUT)
  val irq_illegal = Bool(INPUT)
  val irq_tlb_fault = Bool(INPUT)
  val irq_pc_if = Bits(SZ_ADDR, INPUT)
  val irq_pc_id = Bits(SZ_ADDR, INPUT)

  val vmu_to_irq = new io_vmu_addr_tlb_irq().flip()

  val irq = Bool(OUTPUT)
  val irq_cause = UFix(5, OUTPUT)
  val irq_aux = Bits(64, OUTPUT)
}

class vuIRQHandler extends Component
{
  val io = new io_irq_handler()

  val reg_irq_illegal_tvec = Reg(){ Bool() }
  val reg_irq_cmd_tvec = Reg(){ Bits(width = SZ_XCMD) }

  val reg_irq_ma_inst = Reg(){ Bool() }
  val reg_irq_illegal = Reg(){ Bool() }
  val reg_irq_tlb_fault = Reg(){ Bool() }
  val reg_irq_pc_if = Reg(){ Bits(width = SZ_ADDR) }
  val reg_irq_pc_id = Reg(){ Bits(width = SZ_ADDR) }

  val reg_irq_ma_ld = Reg(){ Bool() }
  val reg_irq_ma_st = Reg(){ Bool() }
  val reg_irq_faulted_ld = Reg(){ Bool() }
  val reg_irq_faulted_st = Reg(){ Bool() }
  val reg_mem_xcpt_addr = Reg(){ Bits(width = SZ_ADDR) }

  when (!io.irq)
  {
    reg_irq_illegal_tvec := io.irq_illegal_tvec

    reg_irq_ma_inst := io.irq_ma_inst
    reg_irq_illegal := io.irq_illegal
    reg_irq_tlb_fault := io.irq_tlb_fault

    reg_irq_ma_ld := io.vmu_to_irq.ma_ld
    reg_irq_ma_st := io.vmu_to_irq.ma_st
    reg_irq_faulted_ld := io.vmu_to_irq.faulted_ld
    reg_irq_faulted_st := io.vmu_to_irq.faulted_st

    when (io.irq_ma_inst || io.irq_tlb_fault) { reg_irq_pc_if := io.irq_pc_if }
    when (io.irq_illegal) { reg_irq_pc_id := io.irq_pc_id }
    when (io.irq_illegal_tvec) { reg_irq_cmd_tvec := io.irq_cmd_tvec }

    when (io.vmu_to_irq.ma_ld || io.vmu_to_irq.ma_st || io.vmu_to_irq.faulted_ld || io.vmu_to_irq.faulted_st) 
    {
      reg_mem_xcpt_addr := io.vmu_to_irq.mem_xcpt_addr
    }
  }

  val dmem_xcpt = reg_irq_ma_ld || reg_irq_ma_st || reg_irq_faulted_ld || reg_irq_faulted_st

  io.irq :=
    reg_irq_ma_inst || reg_irq_illegal || reg_irq_tlb_fault || reg_irq_illegal_tvec || dmem_xcpt

  io.irq_cause :=
    Mux(reg_irq_ma_inst, UFix(24),
    Mux(reg_irq_tlb_fault, UFix(25),
    Mux(reg_irq_illegal, UFix(26),
    Mux(reg_irq_illegal_tvec, UFix(27),
    Mux(reg_irq_ma_ld, UFix(28),
    Mux(reg_irq_ma_st, UFix(29),
    Mux(reg_irq_faulted_ld, UFix(30),
    Mux(reg_irq_faulted_st, UFix(31),
        UFix(0)))))))))

  io.irq_aux :=
    Mux(reg_irq_ma_inst || reg_irq_tlb_fault, reg_irq_pc_if,
    Mux(reg_irq_illegal, reg_irq_pc_id,
    Mux(reg_irq_illegal_tvec, reg_irq_cmd_tvec,
    Mux(dmem_xcpt, reg_mem_xcpt_addr,
        Bits(0)))))

  when (io.flush)
  {
    reg_irq_illegal_tvec := Bool(false)

    reg_irq_ma_inst := Bool(false)
    reg_irq_illegal := Bool(false)
    reg_irq_tlb_fault := Bool(false)

    reg_irq_ma_ld := Bool(false)
    reg_irq_ma_st := Bool(false)
    reg_irq_faulted_ld := Bool(false)
    reg_irq_faulted_st := Bool(false)
  }
}

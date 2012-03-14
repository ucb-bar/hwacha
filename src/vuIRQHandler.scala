package hwacha

import Chisel._
import Node._
import Constants._

class io_irq_handler extends Bundle
{
  val flush = Bool(INPUT)

  val irq_ma_inst = Bool(INPUT)
  val irq_illegal = Bool(INPUT)
  val irq_pc_if = Bits(SZ_ADDR, INPUT)
  val irq_pc_id = Bits(SZ_ADDR, INPUT)

  val irq = Bool(OUTPUT)
  val irq_cause = UFix(5, OUTPUT)
  val irq_aux = Bits(64, OUTPUT)
}

class vuIRQHandler extends Component
{
  val io = new io_irq_handler()

  val reg_irq_ma_inst = Reg(){ Bool() }
  val reg_irq_illegal = Reg(){ Bool() }
  val reg_irq_pc_if = Reg(){ Bits(width = SZ_ADDR) }
  val reg_irq_pc_id = Reg(){ Bits(width = SZ_ADDR) }

  when (!io.irq)
  {
    reg_irq_ma_inst := io.irq_ma_inst
    reg_irq_illegal := io.irq_illegal

    when (io.irq_ma_inst) { reg_irq_pc_if := io.irq_pc_if }
    when (io.irq_illegal) { reg_irq_pc_id := io.irq_pc_id }
  }

  io.irq :=
    reg_irq_ma_inst || reg_irq_illegal

  io.irq_cause :=
    Mux(reg_irq_ma_inst, UFix(24),
    Mux(reg_irq_illegal, UFix(26),
        UFix(0)))

  io.irq_aux :=
    Mux(reg_irq_ma_inst, reg_irq_pc_if,
    Mux(reg_irq_illegal, reg_irq_pc_id,
        Bits(0)))

  when (io.flush)
  {
    reg_irq_ma_inst := Bool(false)
    reg_irq_illegal := Bool(false)
  }
}

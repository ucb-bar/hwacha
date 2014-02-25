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

class IRQIO extends Bundle
{
  val request = Bool(OUTPUT)
  val cause = UInt(OUTPUT, 5)
  val aux = Bits(OUTPUT, 64)
  val clear = Bool(INPUT)
}

class IRQ extends Module
{
  val io = new Bundle {
    val issue = new IRQIssueIO().flip
    val vmu = new IRQVMUIO().flip

    val irq = new IRQIO
  }

  val reg_irq = Reg(init=Bool(false))
  val reg_cause = Reg(init=UInt(0, 5))
  val reg_aux = Reg(init=Bits(0, 64))

  val irqs = List(
    (io.issue.vt.ma_inst, 4, io.issue.vt.pc),
    (io.issue.vt.fault_inst, 5, io.issue.vt.pc),
    (io.issue.vt.illegal, 6, io.issue.vt.pc),
    (io.issue.tvec.illegal, 1, io.issue.tvec.cmd),
    (io.vmu.ma_ld, 8, io.vmu.mem_xcpt_addr),
    (io.vmu.ma_st, 9, io.vmu.mem_xcpt_addr),
    (io.vmu.faulted_ld, 10, io.vmu.mem_xcpt_addr),
    (io.vmu.faulted_st, 11, io.vmu.mem_xcpt_addr)
  )

  when (!reg_irq) {
    for ((cond, cause, aux) <- irqs) {
      when (cond) {
        reg_irq := Bool(true)
        reg_cause := UInt(cause)
        reg_aux := aux
      }
    }
  }

  when (io.irq.clear) {
    reg_irq := Bool(false)
  }

  io.irq.request := reg_irq
  io.irq.cause := reg_cause
  io.irq.aux := reg_aux
}

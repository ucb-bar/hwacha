package hwacha

import Chisel._
import Node._
import Constants._

class IRQIO extends Bundle
{
  val top = new Bundle {
    val illegal_cfg = Bool(OUTPUT)
    val illegal_inst = Bool(OUTPUT)
    val priv_inst = Bool(OUTPUT)
    val illegal_regid = Bool(OUTPUT)
    val aux = Bits(OUTPUT, 64)
  }
  val issue = new Bundle {
    val ma_inst = Bool(OUTPUT)
    val fault_inst = Bool(OUTPUT)
    val illegal = Bool(OUTPUT)
    val illegal_regid = Bool(OUTPUT)
    val aux = Bits(OUTPUT, 64)
  }
  val vmu = new Bundle {
    val ma_ld = Bool(OUTPUT)
    val ma_st = Bool(OUTPUT)
    val faulted_ld = Bool(OUTPUT)
    val faulted_st = Bool(OUTPUT)
    val aux = Bits(OUTPUT, 64)
  }
}

class IRQ extends Module
{
  val io = new Bundle {
    val vu = new IRQIO().flip
    val rocc = new Bundle {
      val request = Bool(OUTPUT)
      val cause = UInt(OUTPUT, 5)
      val aux = Bits(OUTPUT, 64)
      val clear = Bool(INPUT)
    }
  }

  val reg_irq = Reg(init=Bool(false))
  val reg_cause = Reg(init=UInt(0, 5))
  val reg_aux = Reg(init=Bits(0, 64))

  val irqs = List(
    (io.vu.top.illegal_cfg, rocket.Causes.illegal_instruction, io.vu.top.aux),
    (io.vu.top.illegal_inst, rocket.Causes.illegal_instruction, io.vu.top.aux),
    (io.vu.top.priv_inst, rocket.Causes.illegal_instruction, io.vu.top.aux),
    (io.vu.top.illegal_regid, rocket.Causes.illegal_instruction, io.vu.top.aux),
    (io.vu.issue.ma_inst, rocket.Causes.misaligned_fetch, io.vu.issue.aux),
    (io.vu.issue.fault_inst, rocket.Causes.fault_fetch, io.vu.issue.aux),
    (io.vu.issue.illegal, rocket.Causes.illegal_instruction, io.vu.issue.aux),
    (io.vu.issue.illegal_regid, rocket.Causes.illegal_instruction, io.vu.issue.aux),
    (io.vu.vmu.ma_ld, rocket.Causes.misaligned_load, io.vu.vmu.aux),
    (io.vu.vmu.ma_st, rocket.Causes.misaligned_store, io.vu.vmu.aux),
    (io.vu.vmu.faulted_ld, rocket.Causes.fault_load, io.vu.vmu.aux),
    (io.vu.vmu.faulted_st, rocket.Causes.fault_store, io.vu.vmu.aux)
  )

  when (!reg_irq) {
    for ((cond, cause, aux) <- irqs.reverse) {
      when (cond) {
        reg_irq := Bool(true)
        reg_cause := UInt(cause)
        reg_aux := aux
      }
    }
  }

  when (io.rocc.clear) {
    reg_irq := Bool(false)
  }

  io.rocc.request := reg_irq
  io.rocc.cause := reg_cause
  io.rocc.aux := reg_aux
}

package hwacha

import Chisel._

class IRQIO extends Bundle {
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
    val pf_ld = Bool(OUTPUT)
    val pf_st = Bool(OUTPUT)
    val ae_ld = Bool(OUTPUT)
    val ae_st = Bool(OUTPUT)
    val aux = Bits(OUTPUT, 64)
  }
}

class IRQ extends Module {
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
    (io.vu.top.illegal_cfg, 0, io.vu.top.aux),
    (io.vu.top.illegal_inst, 1, io.vu.top.aux),
    (io.vu.top.priv_inst, 2, io.vu.top.aux),
    (io.vu.top.illegal_regid, 3, io.vu.top.aux),
    (io.vu.issue.ma_inst, 4, io.vu.issue.aux),
    (io.vu.issue.fault_inst, 5, io.vu.issue.aux),
    (io.vu.issue.illegal, 6, io.vu.issue.aux),
    (io.vu.issue.illegal_regid, 7, io.vu.issue.aux),
    (io.vu.vmu.ma_ld, 8, io.vu.vmu.aux),
    (io.vu.vmu.ma_st, 9, io.vu.vmu.aux),
    (io.vu.vmu.pf_ld, 10, io.vu.vmu.aux),
    (io.vu.vmu.pf_st, 11, io.vu.vmu.aux),
    (io.vu.vmu.ae_ld, 12, io.vu.vmu.aux),
    (io.vu.vmu.ae_st, 13, io.vu.vmu.aux),
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

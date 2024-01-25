package hwacha

import chisel3._
import chisel3.util._

class IRQIO extends Bundle {
  val top = new Bundle {
    val illegal_cfg = Output(Bool())
    val illegal_inst = Output(Bool())
    val priv_inst = Output(Bool())
    val illegal_regid = Output(Bool())
    val aux = Output(UInt(64.W))
  }
  val issue = new Bundle {
    val ma_inst = Output(Bool())
    val fault_inst = Output(Bool())
    val illegal = Output(Bool())
    val illegal_regid = Output(Bool())
    val aux = Output(UInt(64.W))
  }
  val vmu = new Bundle {
    val ma_ld = Output(Bool())
    val ma_st = Output(Bool())
    val pf_ld = Output(Bool())
    val pf_st = Output(Bool())
    val ae_ld = Output(Bool())
    val ae_st = Output(Bool())
    val aux = Output(UInt(64.W))
  }
}

class IRQ extends Module {
  val io = IO(new Bundle {
    val vu = Flipped(new IRQIO())
    val rocc = new Bundle {
      val request = Output(Bool())
      val cause = Output(UInt(5.W))
      val aux = Output(UInt(64.W))
      val clear = Input(Bool())
    }
  })

  val reg_irq = RegInit(false.B)
  val reg_cause = RegInit(0.U(5.W))
  val reg_aux = RegInit(Bits(0, 64))

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
        reg_irq := true.B
        reg_cause := cause.U
        reg_aux := aux
      }
    }
  }

  when (io.rocc.clear) {
    reg_irq := false.B
  }

  io.rocc.request := reg_irq
  io.rocc.cause := reg_cause
  io.rocc.aux := reg_aux
}

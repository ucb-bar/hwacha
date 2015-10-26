package hwacha

import Chisel._
import cde.Parameters

class ScalarUnit(implicit p: Parameters) extends HwachaModule()(p) {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip

    val cmdq = new CMDQIO().flip
    val imem = new FrontendIO
    val vxu = Decoupled(new IssueOpML)
    val vmu = Decoupled(new VMUOpML)
    val fpu = new Bundle {
      val req = Decoupled(new rocket.FPInput())
      val resp = Decoupled(new rocket.FPResult()).flip
    }
    val dmem = new ScalarMemIO().flip
    
    val vf_active = Bool(OUTPUT)
    val pending_memop = Bool(OUTPUT)
    val pending_seq = Bool(INPUT)
  }

  val ctrl  = Module(new ScalarCtrl)
  val dpath = Module(new ScalarDpath)

  ctrl.io.dpath <> dpath.io.ctrl

  ctrl.io.cfg <> io.cfg

  ctrl.io.cmdq <> io.cmdq
  dpath.io.cmdq <> io.cmdq

  io.imem <> ctrl.io.imem
  io.imem <> dpath.io.imem
  io.vxu <> ctrl.io.vxu
  io.vxu <> dpath.io.vxu
  io.vmu <> ctrl.io.vmu
  io.vmu <> dpath.io.vmu
  io.fpu <> ctrl.io.fpu
  io.fpu <> dpath.io.fpu

  ctrl.io.dmem <> io.dmem
  dpath.io.dmem <> io.dmem

  io.vf_active := ctrl.io.vf_active
  io.pending_memop := ctrl.io.pending_memop
  ctrl.io.pending_seq := io.pending_seq
}

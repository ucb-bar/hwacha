package hwacha

import Chisel._
import cde.Parameters

class HwachaFPInput extends rocket.FPInput {
  val in_fmt = UInt(width = 2)
}

class ScalarUnit(implicit p: Parameters) extends HwachaModule()(p) {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip

    val cmdq = new CMDQIO().flip
    val imem = new FrontendIO
    val vxu = Decoupled(new IssueOpML)
    val vmu = Decoupled(new VMUOpML)
    val fpu = new Bundle {
      val req = Decoupled(new HwachaFPInput)
      val resp = Decoupled(new rocket.FPResult()).flip
    }
    val smu = new SMUIO
    val mocheck = new MOCheck().asInput
    val red = new ReduceResultIO().flip
    
    val busy_mseq = Bool(INPUT)
    val vf_active = Bool(OUTPUT)
    val pending = new MRTPending().asOutput
  }

  val ctrl = Module(new ScalarCtrl)
  val dpath = Module(new ScalarDpath)
  val mrt = Module(new MemTracker(4, 4))

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
  io.smu <> ctrl.io.smu
  io.smu <> dpath.io.smu

  mrt.io.lreq <> ctrl.io.lreq
  mrt.io.sreq <> ctrl.io.sreq
  mrt.io.lret.cnt := UInt(1)
  mrt.io.lret.update := io.smu.resp.fire() && !io.smu.resp.bits.store
  mrt.io.sret.cnt := UInt(1)
  mrt.io.sret.update := io.smu.resp.fire() && io.smu.resp.bits.store

  ctrl.io.mocheck <> io.mocheck
  ctrl.io.red <> io.red

  ctrl.io.busy_mseq := io.busy_mseq
  io.vf_active := ctrl.io.vf_active
  io.pending := mrt.io.pending

  // we need to delay io.pending.all by one cycle
  // because writeback in the scalar unit is delayed by one cycle
  assert(!(!io.vf_active && !Reg(next=io.pending.all) && ctrl.io.sboard_marked), "vf should not end with non empty scoreboard")
}

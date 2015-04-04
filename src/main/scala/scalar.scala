package hwacha

import Chisel._
import Node._
import Constants._
import uncore.constants.MemoryOpConstants._

class ScalarUnit extends HwachaModule
{
  val io = new Bundle {
    val cmdq = new CMDQIO().flip

    val fpu = new Bundle {
      val req = Decoupled(new rocket.FPInput())
      val resp = Decoupled(new rocket.FPResult()).flip
    }
    val vmu = Decoupled(new VMUOp)
    val dmem = new ScalarMemIO().flip

    val imem = new FrontendIO

    val vxu = new VXUIssueOpIO
    
    val vf_active = Bool(OUTPUT)
    val pending_seq = Bool(OUTPUT)
    val pending_memop = Bool(OUTPUT)
  }

  val ctrl  = Module(new ScalarCtrl)
  val dpath = Module(new ScalarDpath)

  ctrl.io.dpath <> dpath.io.ctrl

  //hook up fpu
  dpath.io.fpu.resp.ready <> io.fpu.resp.ready
  dpath.io.fpu.resp.valid <> io.fpu.resp.valid
  dpath.io.fpu.req.bits <> io.fpu.req.bits
  dpath.io.fpu.resp.bits <> io.fpu.resp.bits

  ctrl.io.fpu.req.bits <> io.fpu.req.bits
  ctrl.io.fpu.req.ready <> io.fpu.req.ready
  ctrl.io.fpu.req.valid <> io.fpu.req.valid
  ctrl.io.fpu.resp.valid <> io.fpu.resp.valid

  //connect scalar vmu port
  io.vmu.bits.aux <> dpath.io.vmu.bits.aux
  io.vmu.bits.base <> dpath.io.vmu.bits.base
  io.dmem <> dpath.io.dmem

  io.vmu.bits.fn <> ctrl.io.vmu.bits.fn
  io.vmu.bits.vlen <> ctrl.io.vmu.bits.vlen
  io.vmu.valid := ctrl.io.vmu.valid
  ctrl.io.vmu.ready := io.vmu.ready
  ctrl.io.dmem.valid := io.dmem.valid
  io.dmem.ready := ctrl.io.dmem.ready

  io.imem <> dpath.io.imem
  io.imem <> ctrl.io.imem

  //connects cmdq and pending_*
  io.cmdq <> ctrl.io.cmdq
  io.cmdq <> dpath.io.cmdq
  io.vf_active <> ctrl.io.vf_active
  io.pending_seq <> ctrl.io.pending_seq
  io.pending_memop <> ctrl.io.pending_memop

  io.vxu <> ctrl.io.vxu
  io.vxu <> dpath.io.vxu
}

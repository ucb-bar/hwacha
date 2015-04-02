package hwacha

import Chisel._
import Node._
import Constants._
import uncore.constants.MemoryOpConstants._

class ScalarUnit extends HwachaModule
{
  val io = new Bundle {
    val cmdq = new CMDQIO().flip

    val respq = new RESPQIO()

    val fpu = new Bundle {
      val req = Decoupled(new FPInput())
      val resp = Decoupled(new FPResult()).flip
    }
    val vmu = new ScalarMemIO

    val imem = new FrontendIO

    val seqop = new SequencerOpIO
    
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

  io.respq <> dpath.io.respq

  //connect scalar vmu port
  io.vmu <> dpath.io.vmu
  io.vmu <> ctrl.io.vmu

  io.imem <> dpath.io.imem
  io.imem <> ctrl.io.imem

  //connects cmdq and pending_*
  io.cmdq <> ctrl.io.cmdq
  io.vf_active <> ctrl.io.vf_active
  io.pending_seq <> ctrl.io.pending_seq
  io.pending_memop <> ctrl.io.pending_memop

  io.seqop <> dpath.io.seqop
}

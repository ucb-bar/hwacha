package hwacha

import Chisel._
import Node._
import Constants._
import uncore.constants.MemoryOpConstants._

class ScalarMemOp extends Bundle
{
  val fn = new Bundle 
  {
    val cmd = Bits(width = M_SZ)
    val mt = Bits(width = MT_SZ)
  }

  val addr = Bits(width = params(HwachaScalarDataBits))
  val data = Bits(width = params(HwachaScalarDataBits))
}

class ScalarMemIO extends Bundle
{
  val op = Valid(new ScalarMemOp)
  val tlbSuccess = Bool(INPUT)
  val storeAck = Bool(INPUT)
  val loadData = Valid(Bits(width = params(HwachaScalarDataBits))).flip
}

class Scalar extends HwachaModule
{
  val io = new Bundle {
    val cmdq = new CMDQIO().flip

    val respq = new RESPQIO()

    val vmu = new ScalarMemIO

    val imem = new rocket.CPUFrontendIO

    val seqop = new SequencerOpIO
    
    val vf_active = Bool(OUTPUT)
    val pending_seq = Bool(OUTPUT)
    val pending_memop = Bool(OUTPUT)
  }

  val ctrl  = Module(new ScalarCtrl)
  val dpath = Module(new ScalarDpath)

  ctrl.io.dpath <> dpath.io.ctrl

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

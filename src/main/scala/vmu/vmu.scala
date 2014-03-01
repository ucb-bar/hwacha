package hwacha
package vmunit

import Chisel._
import Constants._
import uncore.constants.MemoryOpConstants._
import uncore.constants.AddressConstants._

case class HwachaVMUConfig()
{
  val ncmdq = 4
  val nstrideq = 4

  val nvvaq = 16
  val nvpaq = 16
  val nvsdq = 16
  val nvldq = 16
  val nvlmb = 16

  val SZ_TAG = log2Up(nvlmb)
}


class VMUIssueIO extends Bundle
{
  val cmdq = new VMUCommandIO
  val strideq = new VMUStrideIO
}

class VMUIO(implicit conf: HwachaConfiguration) extends Bundle
{
  val issue = new VMUIssueIO
  val addr = new VAQLaneIO
  val sdata = new VSDQIO
  val ldata = new VLDQIO().flip
}

class VMU(resetSignal: Bool = null)(implicit conf: HwachaConfiguration) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val irq = new IRQIO
    val xcpt = new XCPTIO().flip

    val lane = new VMUIO().flip
    val pf = new Bundle {
      val vaq = new VVAQIO().flip
    }
    val evac = new Bundle {
      val vaq = new VVAQIO().flip
      val vsdq = new VSDQIO().flip
    }

    val dmem = new rocket.HellaCacheIO()(conf.dcache)

    val vtlb = new TLBIO
    val vpftlb = new TLBIO
  }

  val ctrl = Module(new VMUControl)
  val addr = Module(new AddressUnit)
  val sdata = Module(new StoreDataUnit)
  val ldata = Module(new LoadDataUnit)
  val memif = Module(new MemIF)

  ctrl.io.issue <> io.lane.issue

  addr.io.ctrl <> ctrl.io.addr
  addr.io.lane <> io.lane.addr
  addr.io.evac <> io.evac.vaq
  addr.io.xcpt <> io.xcpt
  io.irq <> addr.io.irq
  io.vtlb <> addr.io.tlb

  sdata.io.lane <> io.lane.sdata
  sdata.io.evac <> io.evac.vsdq
  sdata.io.ctrl <> ctrl.io.store

  memif.io.vpaq <> addr.io.memif
  memif.io.vsdq <> sdata.io.memif
  io.dmem <> memif.io.dmem

  ldata.io.vmdb <> addr.io.vmdb
  ldata.io.memif <> memif.io.vldq
  io.lane.ldata <> ldata.io.lane

  //FIXME
  io.vpftlb.req.valid := Bool(false)
}


class VMUDecodedOp extends Bundle
{
  val tvec = Bool()
  val cmd = new Bundle {
    val raw = Bits(width = M_SZ)
    val ld = Bool()
    val st = Bool()
    val amo = Bool()
  }
  val typ = new Bundle {
    val raw = Bits(width = MT_SZ)
    val b = Bool()
    val h = Bool()
    val w = Bool()
    val d = Bool()
  }
  val vlen = UInt(width = SZ_VLEN)
  val base = UInt(width = SZ_ADDR)
  val stride = UInt(width = SZ_VSTRIDE)
}

class VMUBackendIO extends Bundle
{
  val op = new VMUDecodedOp().asOutput
  val fire = Bool(OUTPUT)
  val busy = Bool(INPUT)
}

class VMUControl(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val issue = new VMUIssueIO().flip
    val addr = new VMUBackendIO
    val store = new VMUBackendIO
  }

  val cmdq = Module(new Queue(new VMUOp, conf.vmu.ncmdq))
  val strideq = Module(new Queue(UInt(width = SZ_VSTRIDE), conf.vmu.nstrideq))

  cmdq.io.enq <> io.issue.cmdq
  strideq.io.enq <> io.issue.strideq

  cmdq.io.deq.ready := Bool(false)
  strideq.io.deq.ready := Bool(false)

  val cmd_hold = Reg(new VMUOp)
  val stride_hold = Reg(UInt(width = SZ_VSTRIDE))

  val hold = Bool()
  hold := Bool(true)
  val cmd = Mux(hold, cmd_hold, cmdq.io.deq.bits)
  val stride = Mux(hold, stride_hold, strideq.io.deq.bits)

  val decode = new VMUDecodedOp
  decode.tvec := vmu_op_tvec(cmd.fn.op)
  decode.cmd.raw := vmu_op_mcmd(cmd.fn.op)
  decode.cmd.ld := is_mcmd_load(decode.cmd.raw)
  decode.cmd.st := is_mcmd_store(decode.cmd.raw)
  decode.cmd.amo := is_mcmd_amo(decode.cmd.raw)
  decode.typ.raw := cmd.fn.typ
  decode.typ.b := is_mtype_byte(decode.typ.raw)
  decode.typ.h := is_mtype_halfword(decode.typ.raw)
  decode.typ.w := is_mtype_word(decode.typ.raw)
  decode.typ.d := is_mtype_doubleword(decode.typ.raw)
  decode.vlen := cmd.vlen
  decode.base := cmd.base
  decode.stride := stride

  io.addr.op <> decode
  io.addr.fire := Bool(false)
  io.store.op <> decode
  io.store.fire := Bool(false)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  val fire = cmdq.io.deq.valid &&
    (!vmu_op_tvec(cmdq.io.deq.bits.fn.op) || strideq.io.deq.valid)

  switch (state) {
    is (s_idle) {
      cmdq.io.deq.ready := Bool(true)
      strideq.io.deq.ready := Bool(true)
      hold := Bool(false)

      when (fire) {
        state := s_busy
        cmd_hold := cmdq.io.deq.bits
        stride_hold := strideq.io.deq.bits
        io.addr.fire := Bool(true)
        io.store.fire := Bool(true)
      }
    }

    is (s_busy) {
      when (!io.addr.busy && !io.store.busy) {
        state := s_idle
      }
    }
  }
}


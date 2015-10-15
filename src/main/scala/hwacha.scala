package hwacha

import Chisel._
import rocket.NTLBEntries

case object HwachaNAddressRegs extends Field[Int]
case object HwachaNScalarRegs extends Field[Int]
case object HwachaNVectorRegs extends Field[Int]
case object HwachaNPredRegs extends Field[Int]
case object HwachaMaxVLen extends Field[Int]
case object HwachaRegLen extends Field[Int]
case object HwachaNDTLB extends Field[Int]
case object HwachaNPTLB extends Field[Int]
case object HwachaCacheBlockOffsetBits extends Field[Int]
case object HwachaLocalScalarFPU extends Field[Boolean]

abstract trait UsesHwachaParameters extends UsesParameters
  with uncore.TileLinkParameters {

  val nARegs = params(HwachaNAddressRegs)
  val nSRegs = params(HwachaNScalarRegs)
  val nVRegs = params(HwachaNVectorRegs)
  val nPRegs = params(HwachaNPredRegs)

  val bSDest = math.max(log2Up(nARegs), log2Up(nSRegs))

  val bVRegs = log2Down(nVRegs) + 1
  val bPRegs = log2Down(nPRegs) + 1

  val regLen = params(HwachaRegLen)

  require(SZ_D == regLen)

  val maxVLen = params(HwachaMaxVLen)
  val bVLen = log2Down(maxVLen) + 1

  val nPredSet = 8 // FIXME (nBatch)

  val local_sfpu = params(HwachaLocalScalarFPU)

  val ndtlb = params(HwachaNDTLB)
  val nptlb = params(HwachaNPTLB)
  val confvru = false
  val confprec = false

  val _nBanks = params(HwachaNBanks) // FIXME

  val confvcmdq = new {
    val ncmd = 16
    val nimm = 16
    val nrd = 16
    val ncnt = _nBanks
  }

  require(confvcmdq.ncmd >= _nBanks)
  require(confvcmdq.nimm >= _nBanks)
  require(confvcmdq.nrd >= _nBanks)

  val nbpq = 2
  val nbrq = 2
  val nbwq = 2

  val nvsreq = 128
  val nvlreq = 128
  val nvsdq = nbrq * _nBanks
}

abstract class HwachaModule(clock: Clock = null, _reset: Bool = null)
  extends Module(clock, _reset) with UsesHwachaParameters

abstract class HwachaBundle extends Bundle with UsesHwachaParameters

class Hwacha extends rocket.RoCC with UsesHwachaParameters {
  import HwachaDecodeTable._
  import Commands._

  val dtlb = Module(new rocket.TLB, {case NTLBEntries => ndtlb})
  val ptlb = Module(new rocket.TLB, {case NTLBEntries => nptlb})

  val rocc = Module(new RoCCUnit)
  val scalar = Module(new ScalarUnit)
  val quad = Module(new Quad)

  // Connect RoccUnit to top level IO
  rocc.io.rocc.cmd <> io.cmd
  rocc.io.rocc.resp <> io.resp
  rocc.io.rocc.busy <> io.busy
  rocc.io.rocc.s <> io.s
  rocc.io.rocc.interrupt <> io.interrupt
  rocc.io.rocc.exception <> io.exception

  // Connect RoccUnit to ScalarUnit
  rocc.io.pending_memop := quad.io.pending.mem || scalar.io.pending_memop
  rocc.io.pending_seq := quad.io.pending.seq
  rocc.io.vf_active := scalar.io.vf_active
  rocc.io.cmdq <> scalar.io.cmdq

  // Connect ScalarUnit to Rocket's FPU
  if (local_sfpu) {
    val sfpu = Module(new ScalarFPU)
    scalar.io.fpu <> sfpu.io
    io.fpu_req.valid := Bool(false)
  } else {
    io.fpu_req <> scalar.io.fpu.req
    io.fpu_resp <> scalar.io.fpu.resp
  }

  // Connect Scalar to I$
  if (confvru) {
    val icache = Module(new HwachaFrontend, {case uncore.CacheName => "HwI"})
    icache.io.vxu <> scalar.io.imem
    //fake delayed vru
    val delay = 4
    val vru_req = ShiftRegister(scalar.io.imem.req,delay)
    icache.io.vru.req := vru_req
    icache.io.vru.active := ShiftRegister(scalar.io.imem.active, delay)
    icache.io.vru.resp.ready := Bool(true)
    io.imem <> icache.io.mem
    io.iptw <> icache.io.ptw
  } else {
    val icache = Module(new rocket.Frontend, {case uncore.CacheName => "HwI"})
    icache.io.cpu.req <> scalar.io.imem.req
    scalar.io.imem.resp <> icache.io.cpu.resp
    icache.io.cpu.btb_update.valid := Bool(false)
    icache.io.cpu.bht_update.valid := Bool(false)
    icache.io.cpu.ras_update.valid := Bool(false)
    icache.io.cpu.invalidate := scalar.io.imem.invalidate
    io.imem <> icache.io.mem
    io.iptw <> icache.io.ptw
  }

  // Connect supporting Hwacha memory modules to external ports
  io.mem.req.valid := Bool(false)
  io.dptw <> dtlb.io.ptw
  io.pptw <> ptlb.io.ptw

  quad.io.cfg <> rocc.io.cfg
  quad.io.issue.vxu <> scalar.io.vxu
  quad.io.issue.vmu <> scalar.io.vmu
  quad.io.issue.scalar <> scalar.io.dmem

  dtlb.io <> quad.io.dtlb
  ptlb.io <> quad.io.ptlb
  io.dmem <> quad.io.dmem
  scalar.io.pending_seq <> quad.io.pending.seq
}

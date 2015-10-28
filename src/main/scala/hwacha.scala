package hwacha

import Chisel._
import cde.{Parameters, Field}
import junctions.ParameterizedBundle
import rocket.NTLBEntries

case object HwachaCommitLog extends Field[Boolean]
case object HwachaNLanes extends Field[Int]
case object HwachaNBanks extends Field[Int]
case object HwachaNAddressRegs extends Field[Int]
case object HwachaNScalarRegs extends Field[Int]
case object HwachaNVectorRegs extends Field[Int]
case object HwachaNPredRegs extends Field[Int]
case object HwachaRegBits extends Field[Int]
case object HwachaPredRegBits extends Field[Int]
case object HwachaMaxVLen extends Field[Int]
case object HwachaBankWidth extends Field[Int]
case object HwachaRegLen extends Field[Int]
case object HwachaNDTLB extends Field[Int]
case object HwachaNPTLB extends Field[Int]
case object HwachaCacheBlockOffsetBits extends Field[Int]
case object HwachaLocalScalarFPU extends Field[Boolean]

abstract class HwachaModule(clock: Clock = null, _reset: Bool = null)
                           (implicit val p: Parameters) extends Module(clock, _reset)
  with UsesHwachaParameters

abstract class HwachaBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with UsesHwachaParameters

abstract trait UsesHwachaParameters extends UsesParameters {
  implicit val p: Parameters

  val commit_log = p(HwachaCommitLog)

  val nARegs = p(HwachaNAddressRegs)
  val nSRegs = p(HwachaNScalarRegs)
  val nVRegs = p(HwachaNVectorRegs)
  val nPRegs = p(HwachaNPredRegs)

  val bSDest = math.max(log2Up(nARegs), log2Up(nSRegs))

  val bVRegs = log2Down(nVRegs) + 1
  val bPRegs = log2Down(nPRegs) + 1

  val regLen = p(HwachaRegLen)

  require(SZ_D == regLen)

  val nLanes = p(HwachaNLanes)
  val nBanks = p(HwachaNBanks)
  val wBank = p(HwachaBankWidth)
  val nSlices = wBank / regLen
  val nBatch = nBanks * nSlices

  require(isPow2(nLanes))
  require(isPow2(nBanks))
  require(isPow2(nSlices))

  val maxVLen = p(HwachaMaxVLen)
  val bVLen = log2Down(maxVLen) + 1

  val maxMLVLen = nLanes * maxVLen
  val bMLVLen = log2Down(maxMLVLen) + 1

  val nPredSet = 8 // FIXME (nBatch)

  val local_sfpu = p(HwachaLocalScalarFPU)

  val ndtlb = p(HwachaNDTLB)
  val nptlb = p(HwachaNPTLB)
  val confvru = false
  val confprec = false

  val confvcmdq = new {
    val ncmd = 16
    val nimm = 16
    val nrd = 16
    val ncnt = nBanks
  }

  require(confvcmdq.ncmd >= nBanks)
  require(confvcmdq.nimm >= nBanks)
  require(confvcmdq.nrd >= nBanks)

  val nvsreq = 128
  val nvlreq = 128
}

class Hwacha()(implicit p: Parameters) extends rocket.RoCC()(p) with UsesHwachaParameters
{
  import HwachaDecodeTable._
  import Commands._

  val rocc = Module(new RoCCUnit)
  val scalar = Module(new ScalarUnit)
  val vus = (0 until nLanes) map { i => Module(new VectorUnit(i)) }
  val mseq = Module(new MasterSequencer)
  val smu = Module(new SMU)
  val imemarb = Module(new uncore.ClientTileLinkIOArbiter(if (confvru) 3 else 2))

  // Connect RoccUnit to top level IO
  rocc.io.rocc.cmd <> io.cmd
  rocc.io.rocc.resp <> io.resp
  rocc.io.rocc.busy <> io.busy
  rocc.io.rocc.s <> io.s
  rocc.io.rocc.interrupt <> io.interrupt
  rocc.io.rocc.exception <> io.exception

  // Connect RoccUnit to ScalarUnit
  rocc.io.pending_memop := vus.map(_.io.pending).reduce(_||_) || scalar.io.pending_memop
  rocc.io.pending_seq := mseq.io.pending
  rocc.io.vf_active := scalar.io.vf_active
  rocc.io.cmdq <> scalar.io.cmdq
  scalar.io.cfg <> rocc.io.cfg

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
    val icache = Module(new HwachaFrontend()(p.alterPartial({case uncore.CacheName => "HwI"})))
    icache.io.vxu <> scalar.io.imem
    //fake delayed vru
    val delay = 4
    val vru_req = ShiftRegister(scalar.io.imem.req,delay)
    icache.io.vru.req := vru_req
    icache.io.vru.active := ShiftRegister(scalar.io.imem.active, delay)
    icache.io.vru.resp.ready := Bool(true)
    io.iptw <> icache.io.ptw
    imemarb.io.in(0) <> icache.io.mem
    imemarb.io.in(2).acquire.valid := Bool(false) // FIXME
    imemarb.io.in(2).grant.ready := Bool(false) // FIXME
  } else {
    val icache = Module(new rocket.Frontend()(p.alterPartial({case uncore.CacheName => "HwI"})))
    icache.io.cpu.req <> scalar.io.imem.req
    scalar.io.imem.resp <> icache.io.cpu.resp
    icache.io.cpu.btb_update.valid := Bool(false)
    icache.io.cpu.bht_update.valid := Bool(false)
    icache.io.cpu.ras_update.valid := Bool(false)
    icache.io.cpu.invalidate := scalar.io.imem.invalidate
    io.iptw <> icache.io.ptw
    imemarb.io.in(0) <> icache.io.mem
  }

  imemarb.io.in(1) <> smu.io.dmem
  io.imem <> imemarb.io.out

  // Connect supporting Hwacha memory modules to external ports
  io.mem.req.valid := Bool(false)
  io.dptw.req.valid := Bool(false)
  io.pptw.req.valid := Bool(false)

  smu.io.tlb.req.ready := Bool(false) // FIXME
  smu.io.scalar.req.valid := Bool(false) // FIXME
  smu.io.scalar <> scalar.io.dmem
  scalar.io.pending_seq := mseq.io.pending

  val enq_vxus = scalar.io.vxu.bits.lane.map(_.active)
  val enq_vmus = scalar.io.vmu.bits.lane.map(_.active)
  val mask_vxus_ready = (0 until nLanes) map { i => !enq_vxus(i) || vus(i).io.issue.vxu.ready }
  val mask_vmus_ready = (0 until nLanes) map { i => !enq_vmus(i) || vus(i).io.issue.vmu.ready }

  def fire_vxu(exclude: Bool, include: Bool*) = {
    val rvs = Seq(scalar.io.vxu.valid, mseq.io.op.ready) ++ mask_vxus_ready
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  def fire_vmu(exclude: Bool, include: Bool*) = {
    val rvs = Seq(scalar.io.vmu.valid) ++ mask_vmus_ready
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  scalar.io.vxu.ready := fire_vxu(scalar.io.vxu.valid)
  scalar.io.vmu.ready := fire_vmu(scalar.io.vmu.valid)

  mseq.io.op.valid := fire_vxu(mseq.io.op.ready)
  mseq.io.op.bits <> scalar.io.vxu.bits
  (mseq.io.master.clear zipWithIndex) map { case (c, r) =>
    c := vus.map(_.io.mseq.clear(r)).reduce(_&&_)
  }

  (vus zipWithIndex) map { case (vu, i) =>
    val dtlb = Module(new rocket.TLB()(p.alterPartial({case NTLBEntries => ndtlb})))
    val ptlb = Module(new rocket.TLB()(p.alterPartial({case NTLBEntries => nptlb})))

    vu.io.cfg <> rocc.io.cfg
    vu.io.issue.vxu.valid := fire_vxu(mask_vxus_ready(i), enq_vxus(i))
    vu.io.issue.vxu.bits.vlen := scalar.io.vxu.bits.lane(i).vlen
    vu.io.issue.vxu.bits <> scalar.io.vxu.bits
    vu.io.issue.vmu.valid := fire_vmu(mask_vmus_ready(i), enq_vmus(i))
    vu.io.issue.vmu.bits.vlen := scalar.io.vmu.bits.lane(i).vlen
    vu.io.issue.vmu.bits <> scalar.io.vmu.bits
    vu.io.mseq.state <> mseq.io.master.state
    vu.io.mseq.update <> mseq.io.master.update

    dtlb.io <> vu.io.dtlb
    dtlb.io.ptw.req.ready := Bool(true)
    dtlb.io.ptw.resp.valid := Bool(false)
    dtlb.io.ptw.status := io.dptw.status
    dtlb.io.ptw.invalidate := Bool(false)

    ptlb.io <> vu.io.ptlb
    ptlb.io.ptw.req.ready := Bool(true)
    ptlb.io.ptw.resp.valid := Bool(false)
    ptlb.io.ptw.status := io.dptw.status
    ptlb.io.ptw.invalidate := Bool(false)

    io.dmem(i) <> vu.io.dmem
  }
}

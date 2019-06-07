package hwacha

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.util.ParameterizedBundle
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket.ICacheParams
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

case object HwachaCommitLog extends Field[Boolean]
case object HwachaIcacheKey extends Field[ICacheParams]
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
case object HwachaLocalScalarFPU extends Field[Boolean]
case object HwachaBuildVRU extends Field[Boolean]
case object HwachaConfPrec extends Field[Boolean]
case object HwachaVRUMaxOutstandingPrefetches extends Field[Int]
case object HwachaVRUEarlyIgnore extends Field[Int]
case object HwachaVRUMaxRunaheadBytes extends Field[Int]
case object HwachaCMDQLen extends Field[Int]
case object HwachaVSETVLCompress extends Field[Boolean]

abstract class HwachaModule(clock: Clock = null, _reset: Bool = null)
                           (implicit val p: Parameters) extends Module(Option(clock), Option(_reset))
  with UsesHwachaParameters

abstract class HwachaBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with UsesHwachaParameters

abstract trait UsesHwachaParameters extends freechips.rocketchip.tile.HasCoreParameters with UsesHwachaOnlyParameters

abstract trait UsesHwachaOnlyParameters {
  implicit val p: Parameters

  val commit_log = p(HwachaCommitLog)

  val nARegs = p(HwachaNAddressRegs)
  val nSRegs = p(HwachaNScalarRegs)
  val nVRegs = p(HwachaNVectorRegs)
  val nPRegs = p(HwachaNPredRegs)

  val bARegs = log2Up(nARegs)
  val bSRegs = log2Up(nSRegs)
  val bVRegs = log2Up(nVRegs)
  val bPRegs = log2Up(nPRegs)
  val bfVRegs = log2Down(nVRegs) + 1
  val bfPRegs = log2Down(nPRegs) + 1

  val bRegs = List(bARegs, bSRegs, bVRegs).max
  val bSDest = List(bARegs, bSRegs).max

  val regLen = p(HwachaRegLen)
  val regBytes = regLen >> 3

  require(SZ_D == regLen)

  val nLanes = p(HwachaNLanes)
  val nBanks = p(HwachaNBanks)
  val wBank = p(HwachaBankWidth)
  val nSlices = wBank / regLen
  val nStrip = nBanks * nSlices

  require(isPow2(nLanes))
  require(isPow2(nBanks))
  require(isPow2(nSlices))
  val bLanes = log2Ceil(nLanes)
  val bBanks = log2Ceil(nBanks)
  val bSlices = log2Ceil(nSlices)
  val bStrip = bBanks + bSlices

  val maxLStride = 2
  val bLStride = log2Floor(maxLStride) + 1
  val bfLStrip = maxLStride + bStrip + 1

  val maxVLen = p(HwachaMaxVLen)
  val bVLen = log2Down(maxVLen) + 1

  val maxMLVLen = nLanes * maxVLen
  val bMLVLen = log2Down(maxMLVLen) + 1

  val local_sfpu = false //p(HwachaLocalScalarFPU) //TODO: Fix local fpu for new encoding

  val ndtlb = p(HwachaNDTLB)
  val nptlb = p(HwachaNPTLB)
  val confvru = false//p(HwachaBuildVRU) //TODO: Fix prefetcher using TL2 Hints
  val confprec = p(HwachaConfPrec)

  val confvcmdq = new {
    val ncmd = p(HwachaCMDQLen)
    val nimm = p(HwachaCMDQLen)
    val nrd = p(HwachaCMDQLen)
    val ncnt = nBanks
    val nstatus = p(HwachaCMDQLen)
  }

  require(confvcmdq.ncmd >= nBanks)
  require(confvcmdq.nimm >= nBanks)
  require(confvcmdq.nrd >= nBanks)
  require(confvcmdq.nstatus >= nBanks)

  // TODO: Parameterize based on maxVLen and bandwidth-delay product
  val nvsreq = 512
  val nvlreq = 512
}

class HwachaCounterIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val mseq = new MasterSequencerCounterIO
  val rocc = new RoCCCounterIO
  val vru = new VRUCounterIO
}

class Hwacha(implicit p: Parameters) extends LazyRoCC(
    opcodes = OpcodeSet.custom0 | OpcodeSet.custom1,
    nPTWPorts = 2 + p(HwachaNLanes),
    usesFPU = true)
  with UsesHwachaOnlyParameters {
  override lazy val module = new HwachaImp(this)

  val icache = LazyModule(new HwachaFrontend())
  val smu = LazyModule(new SMU())
  val vus = Seq.fill(nLanes) {LazyModule(new VectorUnit())}
  val atlBus = LazyModule(new TLXbar)

  atlNode := atlBus.node
  atlBus.node := icache.masterNode
  atlBus.node := smu.masterNode
  val vru = if(confvru) {
    val vruM = LazyModule(new VRU)
    atlBus.node := vruM.masterNode
    Some(vruM)
  } else None
  vus.map(_.masterNode).foreach { tlNode := _ }
}

class HwachaImp(outer: Hwacha)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
  with UsesHwachaParameters
{
  // TODO: Re-add counters
  /*
  override val io = new RoCCIO {
    val counters = new HwachaCounterIO
  }
  */
  import HwachaDecodeTable._
  import Commands._

  val rocc = Module(new RoCCUnit)
  val icache = outer.icache.module
  val scalar = Module(new ScalarUnit)
  val mseq = Module(new MasterSequencer)
  val vus = outer.vus.map(_.module)
  val rpred = Module(new RPredMaster)
  val rfirst = Module(new RFirstMaster)
  val smu = outer.smu.module
  val mou = Module(new MemOrderingUnit)

  // Connect RoccUnit to top level IO
  rocc.io.rocc.cmd <> io.cmd
  io.resp <> rocc.io.rocc.resp
  io.busy <> rocc.io.rocc.busy
  io.interrupt <> rocc.io.rocc.interrupt
  rocc.io.rocc.exception <> io.exception
  //io.counters.rocc <> rocc.io.counters

  // Connect RoccUnit to ScalarUnit
  rocc.io.pending.mseq := mseq.io.pending.all
  rocc.io.pending.mrt := scalar.io.pending.mrt.su.all || vus.map(_.io.pending.all).reduce(_||_)
  rocc.io.vf_active := scalar.io.vf_active
  scalar.io.cmdq <> rocc.io.cmdqs.vu
  scalar.io.cfg <> rocc.io.cfg

  // Connect ScalarUnit to Rocket's FPU
  if (local_sfpu) {
    val sfpu = Module(new ScalarFPU)
    scalar.io.fpu <> sfpu.io
    io.fpu_req.valid := Bool(false)
  } else {
    val sfpu = Module(new ScalarFPUInterface)
    sfpu.io.hwacha.req <> scalar.io.fpu.req
    io.fpu_req <> sfpu.io.rocc.req
    sfpu.io.rocc.resp <> io.fpu_resp
    scalar.io.fpu.resp <> sfpu.io.hwacha.resp
  }

  // Connect Scalar to I$
  icache.io.vxu <> scalar.io.imem
  io.ptw(0) <> icache.io.ptw
  if (confvru) {
    val vru = outer.vru.get.module
    icache.io.vru <> vru.io.imem
    vru.io.cmdq <> rocc.io.cmdqs.vru
    vru.io.vf_complete_ack := mseq.io.vf.last
    //io.counters.vru <> vru.io.counters
  } else {
    // vru plumbing in RoCCUnit should be automatically optimized out
    rocc.io.cmdqs.vru.cmd.ready := Bool(true)
    rocc.io.cmdqs.vru.imm.ready := Bool(true)
    rocc.io.cmdqs.vru.rd.ready := Bool(true)
    rocc.io.cmdqs.vru.cnt.ready := Bool(true)
    rocc.io.cmdqs.vru.status.ready := Bool(true)

    icache.io.vru.req.valid := Bool(false)
    icache.io.vru.active := Bool(false)

    //io.counters.vru <> (new VRUCounterIO).fromBits(UInt(0))
  }

  // Connect supporting Hwacha memory modules to external ports
  io.mem.req.valid := Bool(false)

  smu.io.scalar <> scalar.io.smu
  io.ptw(1) <> smu.io.ptw

  val enq_vxus = scalar.io.vxu.bits.lane.map(_.active)
  val enq_rpred = scalar.io.vxu.bits.active.vrpred
  val enq_rfirst = scalar.io.vxu.bits.active.vrfirst
  val mask_vxus_ready = (0 until nLanes) map { i => !enq_vxus(i) || vus(i).io.issue.vxu.ready }
  val mask_rpred_ready = !enq_rpred || rpred.io.op.ready
  val mask_rfirst_ready = !enq_rfirst || rfirst.io.op.ready

  def fire_vxu(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      scalar.io.vxu.valid, mseq.io.op.ready,
      mask_rpred_ready, mask_rfirst_ready) ++ mask_vxus_ready
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  val enq_vmus = scalar.io.vmu.bits.lane.map(_.active)
  val mask_vmus_ready = (0 until nLanes) map { i => !enq_vmus(i) || vus(i).io.issue.vmu.ready }

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
  scalar.io.pending.mseq <> mseq.io.pending
  mseq.io.vf.stop := scalar.io.vf_stop
  //io.counters.mseq <> mseq.io.counters

  rpred.io.op.valid := fire_vxu(mask_rpred_ready, enq_rpred)
  rpred.io.op.bits <> scalar.io.vxu.bits
  scalar.io.red.pred <> rpred.io.result

  rfirst.io.op.valid := fire_vxu(mask_rfirst_ready, enq_rfirst)
  rfirst.io.op.bits <> scalar.io.vxu.bits
  scalar.io.red.first <> rfirst.io.result

  mou.io.cfg <> rocc.io.cfg
  mou.io.mseq <> mseq.io.master.state
  mou.io.pending.su <> scalar.io.pending.mrt.su
  (mou.io.pending.vus zip vus) map { case (pending, vu) => pending <> vu.io.pending }
  scalar.io.mocheck <> mou.io.check.su
  (vus zip mou.io.check.vus) map { case (vu, mocheck) => vu.io.mocheck <> mocheck }
  (scalar.io.pending.mrt.vus zip vus) map { case (pending, vu) => pending <> vu.io.pending }

  (vus zipWithIndex) map { case (vu, i) =>
    vu.io.id := UInt(i)

    vu.io.cfg <> rocc.io.cfg
    vu.io.issue.vxu.valid := fire_vxu(mask_vxus_ready(i), enq_vxus(i))
    vu.io.issue.vxu.bits.vlen := scalar.io.vxu.bits.lane(i).vlen
    vu.io.issue.vxu.bits <> scalar.io.vxu.bits
    vu.io.issue.vmu.valid := fire_vmu(mask_vmus_ready(i), enq_vmus(i))
    vu.io.issue.vmu.bits.vlen := scalar.io.vmu.bits.lane(i).vlen
    vu.io.issue.vmu.bits <> scalar.io.vmu.bits
    vu.io.mseq.state <> mseq.io.master.state
    vu.io.mseq.update <> mseq.io.master.update
    rpred.io.lane(i) <> vu.io.red.pred
    rfirst.io.lane(i) <> vu.io.red.first
    io.ptw(2 + i) <> vu.io.ptw
  }
}

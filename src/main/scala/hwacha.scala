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

  val bARegs = log2Up(nARegs)
  val bSRegs = log2Up(nSRegs)
  val bVRegs = log2Up(nVRegs)
  val bPRegs = log2Up(nPRegs)
  val bfVRegs = log2Down(nVRegs) + 1
  val bfPRegs = log2Down(nPRegs) + 1

  val bRegs = List(bARegs, bSRegs, bVRegs).max
  val bSDest = List(bARegs, bSRegs).max

  val regLen = p(HwachaRegLen)

  require(SZ_D == regLen)

  val nLanes = p(HwachaNLanes)
  val nBanks = p(HwachaNBanks)
  val wBank = p(HwachaBankWidth)
  val nSlices = wBank / regLen
  val nStrip = nBanks * nSlices

  require(isPow2(nLanes))
  require(isPow2(nBanks))
  require(isPow2(nSlices))

  val maxVLen = p(HwachaMaxVLen)
  val bVLen = log2Down(maxVLen) + 1

  val maxMLVLen = nLanes * maxVLen
  val bMLVLen = log2Down(maxMLVLen) + 1

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
  val icache = Module(new HwachaFrontend()(p.alterPartial({case uncore.CacheName => "HwI"})))
  val scalar = Module(new ScalarUnit)
  val mseq = Module(new MasterSequencer)
  val vus = (0 until nLanes) map { i => Module(new VectorUnit(i)) }
  val rpred = Module(new RPredMaster)
  val rfirst = Module(new RFirstMaster)
  val smu = Module(new SMU)
  val mou = Module(new MemOrderingUnit)
  val ptlb = Module(new rocket.TLB()(p.alterPartial({case NTLBEntries => nptlb})))
  val imemarb = Module(new uncore.ClientTileLinkIOArbiter(if (confvru) 3 else 2))

  // Connect RoccUnit to top level IO
  rocc.io.rocc.cmd <> io.cmd
  rocc.io.rocc.resp <> io.resp
  rocc.io.rocc.busy <> io.busy
  rocc.io.rocc.s <> io.s
  rocc.io.rocc.interrupt <> io.interrupt
  rocc.io.rocc.exception <> io.exception

  // Connect RoccUnit to ScalarUnit
  rocc.io.pending.mseq := mseq.io.pending.all
  rocc.io.pending.mrt := scalar.io.pending.mrt.su.all || vus.map(_.io.pending.all).reduce(_||_)
  rocc.io.vf_active := scalar.io.vf_active
  rocc.io.cmdqs.vu <> scalar.io.cmdq
  scalar.io.cfg <> rocc.io.cfg

  // Connect ScalarUnit to Rocket's FPU
  if (local_sfpu) {
    val sfpu = Module(new ScalarFPU)
    scalar.io.fpu <> sfpu.io
    io.fpu_req.valid := Bool(false)
  } else {
    import HardFloatHelper._

    val reqq = Module(new Queue(new HwachaFPInput, 2))
    val respq = Module(new Queue(new rocket.FPResult, 2))

    reqq.io.enq <> scalar.io.fpu.req
    io.fpu_req.bits.in1 :=
      Mux(reqq.io.deq.bits.fromint, reqq.io.deq.bits.in1,
        Mux(reqq.io.deq.bits.in_fmt === UInt(0),
          Cat(SInt(-1,32), recode_sp(reqq.io.deq.bits.in1)), recode_dp(reqq.io.deq.bits.in1)))
    io.fpu_req.bits.in2 :=
      Mux(reqq.io.deq.bits.in_fmt === UInt(0),
        Cat(SInt(-1,32), recode_sp(reqq.io.deq.bits.in2)), recode_dp(reqq.io.deq.bits.in2))
    io.fpu_req.bits.in3 :=
       Mux(reqq.io.deq.bits.in_fmt === UInt(0),
       Cat(SInt(-1,32), recode_sp(reqq.io.deq.bits.in3)), recode_dp(reqq.io.deq.bits.in3))
    io.fpu_req <> reqq.io.deq

    respq.io.enq <> io.fpu_resp
    scalar.io.fpu.resp <> respq.io.deq
  }

  // Connect Scalar to I$
  icache.io.vxu <> scalar.io.imem
  if (confvru) {
    val vru = Module(new VRU)
    icache.io.vru <> vru.io.toicache
    vru.io.cmdq <> rocc.io.cmdqs.vru
    vru.io.dmem <> imemarb.io.in(2)
  } else {
    // vru plumbing in RoCCUnit should be automatically optimized out
    rocc.io.cmdqs.vru.cmd.ready := Bool(true)
    rocc.io.cmdqs.vru.imm.ready := Bool(true)
    rocc.io.cmdqs.vru.rd.ready := Bool(true)
    rocc.io.cmdqs.vru.cnt.ready := Bool(true)

    icache.io.vru.req.valid := Bool(false)
    icache.io.vru.active := Bool(false)
  }
  imemarb.io.in(0) <> icache.io.mem
  imemarb.io.in(1) <> smu.io.dmem
  io.imem <> imemarb.io.out
  io.iptw <> icache.io.ptw

  // Connect supporting Hwacha memory modules to external ports
  io.mem.req.valid := Bool(false)
  io.dptw.req.valid := Bool(false)
  io.pptw.req.valid := Bool(false)

  smu.io.scalar <> scalar.io.smu
  ptlb.io <> smu.io.tlb
  ptlb.io.ptw.req.ready := Bool(true)
  ptlb.io.ptw.resp.valid := Bool(false)
  ptlb.io.ptw.status := io.pptw.status
  ptlb.io.ptw.invalidate := Bool(false)

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
    val dtlb = Module(new rocket.TLB()(p.alterPartial({case NTLBEntries => ndtlb})))

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

    dtlb.io <> vu.io.tlb
    dtlb.io.ptw.req.ready := Bool(true)
    dtlb.io.ptw.resp.valid := Bool(false)
    dtlb.io.ptw.status := io.dptw.status
    dtlb.io.ptw.invalidate := Bool(false)

    io.dmem(i) <> vu.io.dmem
  }
}

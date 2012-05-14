package hwacha

import Chisel._
import Node._
import Constants._

class pcUnitBundle extends Bundle {
  val id = Bits(width=SZ_PVFB_TAG)
  val fire = Bool()
  val pc = Bits(width=SZ_ADDR)
  val vlen = Bits(width=SZ_VLEN)
}

class ioPCStage extends Bundle {
  val flush = Bool(INPUT)

  val pcToTVEC = new ioPCToIssueTVEC()
  val pcToVT = new ioPCToIssueVT()

  val pc = new pcUnitBundle().asInput()
  val vtToPC = new ioIssueVTToPC().flip()
  val vtToPVFB = new ioIssueVTToPVFB().flip()

  val laneToIssue = new ioLaneToIssue()flip()
}

class vuPCStage extends Component {
  val io = new ioPCStage()

  val rrArbio = 
    if(coarseGrained) 
      new CoarseRRArbiter(NUM_PVFB)( new pvfBundle ).io 
    else
      new RRArbiter(NUM_PVFB)( new pvfBundle ).io
  
  val pvfb_sel = UFix(1) << io.vtToPVFB.pvfb_tag

  val replay_pre_if_sel = UFix(1) << io.vtToPC.replay_pre_if.bits.tag
  val replay_if_sel = UFix(1) << io.vtToPC.replay_if.bits.tag
  val replay_jump_sel = UFix(1) << io.vtToPC.replay_jump.bits.tag
  val replay_branch_sel = UFix(1) << io.vtToPC.replay_branch.bits.tag
  val replay_stop_sel = UFix(1) << io.vtToPC.replay_stop.bits.tag
  val replay_stalld_sel = UFix(1) << io.vtToPC.replay_stalld.bits.tag

  val lane_sel = UFix(1) << io.laneToIssue.pvfb_tag

  var valid = Bool(false)
  var stop = Bool(false)
  var pending = Bool(false)

  def gen(pcBundle: pcUnitBundle, n: Int): Unit = {
    if(n >= 0) {

      val pcUnit = new vuPC()
      pcUnit.io.flush := io.flush

      pcUnit.io.in <> pcBundle

      val i = NUM_PVFB - 1 - n

      rrArbio.in(i).bits <> pcUnit.io.pcToVT.bits
      rrArbio.in(i).valid := pcUnit.io.pcToVT.valid && !pcUnit.io.pending
      pcUnit.io.pcToVT.ready := rrArbio.in(i).ready && !pcUnit.io.pending

      pcUnit.io.vtToPC.replay_pre_if.valid := io.vtToPC.replay_pre_if.valid && replay_pre_if_sel(i)
      pcUnit.io.vtToPC.replay_if.valid := io.vtToPC.replay_if.valid && replay_if_sel(i)
      pcUnit.io.vtToPC.replay_jump.valid := io.vtToPC.replay_jump.valid && replay_jump_sel(i)
      pcUnit.io.vtToPC.replay_branch.valid := io.vtToPC.replay_branch.valid && replay_branch_sel(i)
      pcUnit.io.vtToPC.replay_stop.valid := io.vtToPC.replay_stop.valid && replay_stop_sel(i)
      pcUnit.io.vtToPC.replay_stalld.valid := io.vtToPC.replay_stalld.valid && replay_stalld_sel(i)

      pcUnit.io.vtToPC.replay_pre_if.bits <> io.vtToPC.replay_pre_if.bits
      pcUnit.io.vtToPC.replay_if.bits <> io.vtToPC.replay_if.bits
      pcUnit.io.vtToPC.replay_jump.bits <> io.vtToPC.replay_jump.bits
      pcUnit.io.vtToPC.replay_branch.bits <> io.vtToPC.replay_branch.bits
      pcUnit.io.vtToPC.replay_stalld.bits <> io.vtToPC.replay_stalld.bits

      pcUnit.io.vtToPVFB.stop := io.vtToPVFB.stop && pvfb_sel(i)
      pcUnit.io.vtToPVFB.pc.valid := io.vtToPVFB.pc.valid && pvfb_sel(i)
      pcUnit.io.vtToPVFB.pc.bits <> io.vtToPVFB.pc.bits

      pcUnit.io.laneToPVFB.mask.valid := io.laneToIssue.mask.valid && lane_sel(i)
      pcUnit.io.laneToPVFB.mask.bits := io.laneToIssue.mask.bits((i+1) * WIDTH_PVFB  - 1, i * WIDTH_PVFB)

      valid = valid || (pcUnit.io.pcToVT.valid && !pcUnit.io.pcToTVEC.stop)
      stop = stop || pcUnit.io.pcToTVEC.stop
      pending = pending || pcUnit.io.pending

      gen(pcUnit.io.out, n-1)
    }
  }

  gen(io.pc, NUM_PVFB-1)

  io.pcToTVEC.stop := stop && !pending && !valid

  io.pcToVT <> rrArbio.out

}

package hwacha

import Chisel._
import Node._
import Constants._

class pcUnitBundle extends Bundle {
  val id = Bits(width=SZ_NUM_PVFB)
  val fire = Bool()
  val pc = Bits(width=SZ_ADDR)
  val vlen = Bits(width=SZ_VLEN)
}

class ioPCStage extends Bundle {
  val flush = Bool(INPUT)

  val pcToTVEC = new ioPCToIssueTVEC()
  val pcToVT = new ioPCToIssueVT()

  val pc = new pcUnitBundle().asInput()
  val vtToPC = Vec(NUM_PVFB){ new ioIssueVTToPC() }.flip()
  val vtToPVFB = Vec(NUM_PVFB){ new ioIssueVTToPVFB() }.flip()

  val hazardToIssue = Vec(NUM_PVFB){ new io_vxu_hazard_to_issue_vt() }.asInput()

  val laneToPVFB = Vec(NUM_PVFB){ new ioLaneToPVFB() }.flip()
}

class vuPCStage extends Component {
  val io = new ioPCStage()

  val rrArb = new RRArbiter(NUM_PVFB)( new pvfBundle )
  
  var fire = Bool(false)
  var stop = Bool(false)

  def gen(pcBundle: pcUnitBundle, n: Int): Unit = {
    if(n >= 0) {

      val pcUnit = new vuPC()
      pcUnit.io.flush := io.flush

      pcUnit.io.in <> pcBundle

      fire = fire || pcBundle.fire

      stop = stop || pcUnit.io.pcToTVEC.stop
      pcUnit.io.pcToVT <> rrArb.io.in(NUM_PVFB - 1 - n)

      pcUnit.io.vtToPC <> io.vtToPC(NUM_PVFB - 1 - n)
      pcUnit.io.vtToPVFB <> io.vtToPVFB(NUM_PVFB - 1 - n)

      pcUnit.io.hazardToIssue <> io.hazardToIssue(NUM_PVFB - 1 - n)

      pcUnit.io.laneToPVFB <> io.laneToPVFB(NUM_PVFB - 1 - n)

      gen(pcUnit.io.out, n-1)
    }
  }

  gen(io.pc, NUM_PVFB-1)

  val counter = new qcnt(0, NUM_PVFB, true)
  counter.io.flush := io.flush
  counter.io.inc := fire
  counter.io.dec := stop
  counter.io.qcnt := UFix(2)

  io.pcToTVEC.stop := !fire && !counter.io.watermark && stop

  io.pcToVT <> rrArb.io.out

}

package hwacha

import Chisel._
import Node._
import Constants._

class MaskBundle extends Bundle
{
  val active = Bits(width=WIDTH_PVFB)
  val resolved = Bits(width=WIDTH_PVFB)
}

class ioMaskPipe extends ioPipe() ( new MaskBundle() )

class ioPCToIssueTVEC extends Bundle
{
  val stop = Bool(OUTPUT)
}

class ioPCToIssueVT extends ioDecoupled()( new pvfBundle )

class ioPC extends Bundle
{
  val flush = Bool(INPUT)
  
  val pcToTVEC = new ioPCToIssueTVEC()
  val pcToVT = new ioPCToIssueVT()

  val tvecToPC = new ioIssueTVECToPC().flip()
  val vtToPC = new ioIssueVTToPC().flip()
  val vtToPVFB = new ioIssueVTToPVFB().flip()

  val hazardToIssue = new io_vxu_hazard_to_issue().asInput()

  val laneToPVFB = new ioLaneToPVFB().flip()
}

class vuPC extends Component
{
  val io = new ioPC()

  val pvfb = new vuPVFB()

  val next_pc = Wire(){ Bits(width=SZ_ADDR) }
  val next_mask = Wire(){ Bits(width=WIDTH_PVFB) }
  val next_valid = Wire(){ Bool() }

  val reg_pc = Reg(next_pc, resetVal = Bits(0,SZ_ADDR))
  val reg_mask = Reg(next_mask, resetVal = Bits(0,WIDTH_PVFB))
  val reg_valid = Reg(next_valid, resetVal = Bool(false))

  pvfb.io.vtToPVFB <> io.vtToPVFB

  pvfb.io.mask.valid := io.laneToPVFB.mask.valid
  pvfb.io.mask.bits.resolved := io.laneToPVFB.mask.bits
  pvfb.io.mask.bits.active := reg_mask

  next_pc := reg_pc
  next_mask := reg_mask
  next_valid := reg_valid

  when (io.tvecToPC.fire) 
  {
    next_pc := io.tvecToPC.pc
    next_mask := Fill(WIDTH_PVFB, Bits(1,1))
    next_valid := Bool(true)
  }
  . elsewhen (pvfb.io.pvf.valid)
  {
    next_pc := pvfb.io.pvf.bits.pc
    next_mask := pvfb.io.pvf.bits.mask
    next_valid := Bool(true)
  }
  . elsewhen (io.vtToPC.replay.valid)
  {
    next_pc := io.vtToPC.replay.bits
  }
  . elsewhen (io.pcToVT.ready) 
  {
    next_pc := reg_pc + UFix(4)
  }

  when (io.vtToPC.stop)
  {
    next_valid := Bool(false)
  }

  when(io.flush)
  {
    next_pc := Bits(0,SZ_ADDR)
    next_valid := Bool(false)
  }

  io.pcToVT.bits.pc := reg_pc
  io.pcToVT.bits.mask := reg_mask
  io.pcToVT.valid := reg_valid && io.hazardToIssue.pending_branch
  io.pcToTVEC.stop := io.vtToPC.stop && pvfb.io.empty
}

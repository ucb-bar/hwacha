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

  val in = new pcUnitBundle().asInput()
  val out = new pcUnitBundle().asOutput()
  
  val pcToTVEC = new ioPCToIssueTVEC()
  val pcToVT = new ioPCToIssueVT()

  val vtToPC = new ioIssueVTToPC().flip()
  val vtToPVFB = new ioIssueVTToPVFB().flip()

  val hazardToIssue = new io_vxu_hazard_to_issue_vt().asInput()

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

  val fire_pass = io.in.vlen >= Bits(WIDTH_PVFB)
  val vlen = Mux(fire_pass, Bits(WIDTH_PVFB-1), io.in.vlen)

  val delay_id = Reg(io.in.id + UFix(1))
  val delay_fire = Reg(io.in.fire && fire_pass)
  val delay_pc = Reg(io.in.pc)
  val delay_vlen = Reg(Mux(fire_pass, io.in.vlen - Bits(WIDTH_PVFB), Bits(0)))

  io.out.id := delay_id
  io.out.fire := delay_fire
  io.out.pc := delay_pc
  io.out.vlen := delay_vlen

  pvfb.io.vtToPVFB <> io.vtToPVFB

  pvfb.io.mask.valid := io.laneToPVFB.mask.valid
  pvfb.io.mask.bits.resolved := io.laneToPVFB.mask.bits
  pvfb.io.mask.bits.active := reg_mask

  next_pc := reg_pc
  next_mask := reg_mask
  next_valid := reg_valid

  when (io.in.fire) 
  {
    next_pc := io.in.pc
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
  . elsewhen (io.pcToVT.ready && io.pcToVT.valid) 
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
  io.pcToVT.bits.id := io.in.id
  io.pcToVT.bits.vlen := vlen
  io.pcToVT.valid := reg_valid && !io.hazardToIssue.pending_branch && !io.vtToPC.replay.valid && !io.vtToPC.stop
  io.pcToTVEC.stop := io.vtToPC.stop && pvfb.io.empty
}

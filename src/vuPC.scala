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

  val laneToPVFB = new ioLaneToPVFB().flip()

  val pending = Bool(OUTPUT)
}

class vuPC extends Component
{
  val io = new ioPC()

  val pvfb = new vuPVFB()

  val next_pc = Wire(){ Bits(width=SZ_ADDR) }
  val next_pending = Wire(){ Bool() }
  val next_mask = Wire(){ Bits(width=WIDTH_PVFB) }
  val next_valid = Wire(){ Bool() }

  val reg_pc = Reg(next_pc, resetVal = Bits(0,SZ_ADDR))
  val reg_pending = Reg(next_pending, resetVal = Bool(false))
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
  next_pending := reg_pending
  next_mask := reg_mask
  next_valid := reg_valid

  when (io.in.fire) 
  {
    next_pc := io.in.pc
    next_mask := Fill(WIDTH_PVFB, Bits(1,1))
    next_pending := Bool(false)
    next_valid := Bool(true)
  }
  . elsewhen (pvfb.io.pvf.valid)
  {
    next_pc := pvfb.io.pvf.bits.pc
    next_mask := pvfb.io.pvf.bits.mask
    next_pending := Bool(false)
    next_valid := Bool(true)
  }
  . elsewhen (io.vtToPC.replay_branch.valid)
  {
    next_pending := Bool(true)
    next_valid := Bool(false)
  }
  . elsewhen (io.vtToPC.replay_jump.valid)
  {
    next_pc := io.vtToPC.replay_jump.bits.pc
  }
  . elsewhen (io.vtToPC.replay_stop.valid)
  {
    next_valid := Bool(false)
    when (!pvfb.io.empty) { next_pending := Bool(true) }
  }
  . elsewhen (io.vtToPC.replay_if.valid)
  {
    next_pc := io.vtToPC.replay_if.bits.pc
  }
  . elsewhen (io.vtToPC.replay_pre_if.valid)
  {
    next_pc := io.vtToPC.replay_pre_if.bits.pc
  }
  . elsewhen (io.pcToVT.ready && io.pcToVT.valid) 
  {
    next_pc := reg_pc + UFix(4)
  }

  when(io.flush)
  {
    next_pc := Bits(0,SZ_ADDR)
    next_pending := Bool(false)
    next_valid := Bool(false)
  }

  io.pcToVT.bits.pc := reg_pc
  io.pcToVT.bits.mask := reg_mask
  io.pcToVT.bits.id := io.in.id
  io.pcToVT.bits.vlen := vlen
  io.pcToVT.valid := reg_valid
  io.pcToTVEC.stop := io.vtToPC.replay_stop.valid && pvfb.io.empty
  io.pending := reg_pending || next_pending
}

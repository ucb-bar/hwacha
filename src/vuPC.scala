package hwacha

import Chisel._
import Node._
import Constants._

class MaskBundle extends Bundle
{
  val active = Bits(width=WIDTH_PVFB)
  val resolved = Bits(width=WIDTH_PVFB)
}

class ioMaskPipe extends ValidIO(new MaskBundle)

class ioPCToIssueTVEC extends Bundle
{
  val stop = Bool(OUTPUT)
}

class ioPCToIssueVT extends DecoupledIO( new pvfBundle )

class ioPC extends Bundle
{
  val in = new pcUnitBundle().asInput()
  val out = new pcUnitBundle().asOutput()
  
  val pcToTVEC = new ioPCToIssueTVEC()
  val pcToVT = new ioPCToIssueVT()

  val vtToPC = new ioIssueVTToPC().flip()
  val vtToPVFB = new ioIssueVTToPVFB().flip()

  val laneToPVFB = new ioLaneToPVFB().flip()

  val pending = Bool(OUTPUT)
}

class vuPC extends Module
{
  val io = new ioPC()

  val pvfb = new vuPVFB()

  val next_pc = Bits(width=SZ_ADDR)
  val next_pending = Bool()
  val next_mask = Bits(width=WIDTH_PVFB)
  val next_valid = Bool()
  val next_stalld = Bool()

  val reg_pc = Reg(updateData = next_pc, resetData = Bits(0,SZ_ADDR))
  val reg_pending = Reg(updateData = next_pending, resetData = Bool(false))
  val reg_mask = Reg(updateData = next_mask, resetData = Bits(0,WIDTH_PVFB))
  val reg_valid = Reg(updateData = next_valid, resetData = Bool(false))
  val reg_stalld = Reg(updateData = next_stalld, resetData = Bool(false))

  val fire_pass = io.in.vlen >= Bits(WIDTH_PVFB)
  val vlen = Mux(fire_pass & Bool(HAVE_PVFB), Bits(WIDTH_PVFB-1), io.in.vlen)

  val delay_id = RegUpdate(io.in.id + UInt(1))
  val delay_fire = RegUpdate(io.in.fire && fire_pass)
  val delay_pc = RegUpdate(io.in.pc)
  val delay_vlen = RegUpdate(Mux(fire_pass, io.in.vlen - Bits(WIDTH_PVFB), Bits(0)))

  io.out.id := delay_id
  io.out.fire := delay_fire
  io.out.pc := delay_pc
  io.out.vlen := delay_vlen

  if (HAVE_PVFB)
    pvfb.io.vtToPVFB <> io.vtToPVFB

  val vlen_mask = Cat(hardfloat.MaskOnes(vlen, 0, WIDTH_PVFB-1), Bits(1))

  if (HAVE_PVFB) {
    pvfb.io.mask.valid := io.laneToPVFB.mask.valid
    pvfb.io.mask.bits.resolved := io.laneToPVFB.mask.bits & vlen_mask
    pvfb.io.mask.bits.active := reg_mask
  }

  next_pc := reg_pc
  next_pending := reg_pending
  next_mask := reg_mask
  next_valid := reg_valid
  next_stalld := Bool(false)

  when (io.in.fire) 
  {
    next_pc := io.in.pc
    next_mask := Fill(WIDTH_PVFB, Bits(1,1)) & vlen_mask
    next_pending := Bool(false)
    next_valid := Bool(true)
  }
  .elsewhen (Bool(HAVE_PVFB) && pvfb.io.pvf.valid)
  {
    next_pc := pvfb.io.pvf.bits.pc
    next_mask := pvfb.io.pvf.bits.mask & vlen_mask
    next_pending := Bool(false)
    next_valid := Bool(true)
  }
  .elsewhen (io.vtToPC.replay_jump.valid)
  {
    next_pc := io.vtToPC.replay_jump.bits.pc
  }
  .elsewhen (io.vtToPC.replay_stop.valid)
  {
    next_valid := Bool(false)
    when (Bool(HAVE_PVFB) && !pvfb.io.empty) { next_pending := Bool(true) }
  }
  .elsewhen (io.vtToPC.replay_stalld.valid)
  {
    next_stalld := Bool(true)
    next_pc := io.vtToPC.replay_stalld.bits.pc
  }
  .elsewhen (io.vtToPC.replay_branch.valid)
  {
    next_pending := Bool(true)
    next_valid := Bool(false)
  }
  .elsewhen (io.vtToPC.replay_if.valid)
  {
    next_pc := io.vtToPC.replay_if.bits.pc
  }
  .elsewhen (io.vtToPC.replay_pre_if.valid)
  {
    next_pc := io.vtToPC.replay_pre_if.bits.pc
  }
  .elsewhen (io.pcToVT.ready && io.pcToVT.valid) 
  {
    next_pc := reg_pc + UInt(4)
  }

  io.pcToVT.bits.pc := reg_pc
  if (HAVE_PVFB) io.pcToVT.bits.mask := reg_mask
  io.pcToVT.bits.id := io.in.id
  io.pcToVT.bits.vlen := vlen
  io.pcToVT.valid := reg_valid
  io.pcToTVEC.stop := io.vtToPC.replay_stop.valid && (Bool(!HAVE_PVFB) | pvfb.io.empty)
  io.pending := reg_pending || reg_stalld
}

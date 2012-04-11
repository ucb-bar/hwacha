package hwacha

import Chisel._
import Node._
import Cosntants._
import Commands._
import Instructions._

class pvfBundle extends Bundle
{
  val pc = Bits(width=SZ_ADDR)
  val mask = Bits(width=WIDTH_PVFB)
}

class IoPVFBToIssue extends Bundle 
{
  val empty = Bool(OUTPUT)
  val pvf = new ioDcoupled()( new pvfbBundle() )
}

class IoPVFB extends Bundle
{
  val issueToPVFB = new IoIssueToPVFB().flip()
  val hazardToPVFB = new IoVXUHazardToPVFB().asInput
  val pvfbToIssue = new IoPVFBToIssue()
  val laneToPVFB = new IoLaneToPVFB().flip()
}

class IoSeqToPVFB extends Bundle
{
  val valid = Bool(OUTPUT)
}

class PVFB extends Component {
  val io = new IoPVFB

  val SIZE_ADDR = Log2Up(DEPTH_PVFB)

  val enq_ptr_next = Wire(){ UFix(width=SIZE_ADDR) }
  val deq_ptr_next = Wire(){ UFix(width=SIZE_ADDR) }
  val full_next = Wire(){ Bool() }

  val enq_ptr = Reg(enq_ptr_next, resetVal = UFix(0, SIZE_ADDR))
  val deq_ptr = Reg(deq_ptr_next, resetVal = UFix(0, SIZE_ADDR))
  val full = Reg(full_next, resetVal = Bool(false))

  enq_ptr_next := enq_ptr
  deq_ptr_next := deq_ptr
  full := full_next

  val ram_empty = ~full && (enq_ptr === deq_ptr)

  val maskRam_wdata = io.laneToPVFB.branch_resolution_mask & io.pvfbToIssue.pvf.bits.mask
  val pcRam_wdata = Reg(resetVal = Bits(0,SZ_ADDR))
  when(io.issueToPVFB.enq.valid) { pcRam_wdata := io.issueToPVFB.enq.bits}

  val do_enq = io.laneToPVFB.valid && maskRam_wdata.orR
  val do_deq = io.issueToPVFB.stop && !ram_empty

  when (do_deq) { read_ptr_next := read_ptr + UFix(1) }
  when (do_enq) { write_ptr_next := write_ptr + UFix(1) }

  when (do_enq && ! do_deq && (write_ptr_next === read_ptr))
  {
    full_next := Bool(true)
  }
  . elsewhen (do_deq && full)
  {
    full_next := Bool(true)
  } 
  . otherwise
  {
    full_next := full
  }

  val maskRam = Mem(DEPTH_PVFB, do_enq, enq_ptr, maskRam_wdata, resetVal = null, cs = do_enq || do_deq)
  val pcRam = Mem(DEPTH_PVFB, do_enq, enq_ptr, pcRam_wdata, resetVal = null, cs = do_enq || do_deq)
  maskRam.setReadLatency(1)
  maskRam.setTarget('inst)
  pcRam.setReadLatency(1)
  pcRam.setTarget('inst)

  val next_valid = Wire(){ Bool() }
  val next_pc = Wire(){ Bits(width=SZ_ADDR) }
  val next_mask = Wire(){ Bits(width=WIDTH_PVFB) }

  val reg_valid = Reg(next_valid, resetVal = Bool(false))
  val reg_pc = Reg(next_pc, resetVal = Bits(0,SZ_ADDR))
  val reg_mask = Reg(next_mask, resetVal = Bits(0, WIDTH_PVFB))

  val reg_do_deq = Reg(do_deq)
  val maskRam_dout = maskRam(deq_ptr)
  val pcRam_dout = pcRam(deq_ptr)

  next_valid := reg_valid
  next_pc := reg_pc
  next_mask := reg_mask
  when (io.issueToPVFB.stop)
  {
    next_valid := Bool(false)
  }
  when (reg_do_deq)
  {
    next_valid := Bool(true)
    next_pc := pcRam_dout
    next_mask := maskRam_dout
  }
  when (io.issueToPVFB.fire.valid)
  {
    next_valid := Bool(true)
    next_pc := io.issueToPVFB.fire.bits
    next_mask := Fill(WIDTH_PVFB, Bits(1,1))
  }
  when (io.issueToPVFB.ready)
  {
    next_pc := reg_pc + UFix(4)
  }
  when (io.laneToPVFB.valid)
  {
    next_mask := ~io.laneToPVFB.branch_resolution_mask & io.pvfbToIssue.pvf.bits.mask
  }

  val pc = Mux(reg_do_deq, pcRam_dout, reg_pc)
  val mask = Mux(reg_do_deq, maskRam_dout, reg_mask)

  io.pvfbToIssue.pvf.valid := (reg_valid || reg_do_deq) && !io.issueToPVFB.stop && !io.hazardToPVFB.pending_branch
  io.pvfbToIssue.pvf.bits.pc := pc
  io.pvfbToIssue.pvf.bits.mask := mask
  io.pvfbToIssue.empty := ram_empty && !reg_valid && !reg_do_deq
}

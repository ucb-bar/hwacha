package hwacha 

import Chisel._
import Node._
import Constants._

class XCPTIO extends Bundle
{
  val exception = Bool(OUTPUT)
  val evac_addr = UInt(OUTPUT, SZ_ADDR)
  val evac = Bool(OUTPUT)
  val hold = Bool(OUTPUT)
  val kill = Bool(OUTPUT)
}

class XCPTVUIO extends Bundle
{
  val busy = Bool(OUTPUT)
  val flush_kill = Bool(OUTPUT)
  val flush_aiw = Bool(OUTPUT)
  val flush_vru = Bool(OUTPUT)
  val flush_vmu = Bool(OUTPUT)
}

class XCPTIssueIO extends Bundle 
{
  val stall = Bool(OUTPUT)
}

class XCPTSequencerIO extends Bundle 
{
  val stall = Bool(OUTPUT)
}

class XCPTVXUIO extends Bundle
{
  val flush = Bool(OUTPUT)
  val issue = new XCPTIssueIO
  val seq = new XCPTSequencerIO
}

class XCPTTLBIO extends Bundle 
{
  val stall = Bool(OUTPUT)
}

class XCPTVMUIO extends Bundle 
{
  val tlb = new XCPTTLBIO
  val drain = Bool(OUTPUT)
}

class XCPTEvacIO extends Bundle
{
  val start = Bool(OUTPUT)
  val addr = UInt(OUTPUT, SZ_ADDR)
}

class io_expand_to_xcpt_handler extends Bundle
{
  val empty = Bool(OUTPUT)
}

class io_vxu_to_xcpt_handler extends Bundle
{
  val expand = new io_expand_to_xcpt_handler
}

class io_evac_to_xcpt_handler extends Bundle
{
  val done = Bool(OUTPUT)
}

class XCPT extends Module 
{
  val io = new Bundle {
    val xcpt = new XCPTIO().flip

    val vu = new XCPTVUIO
    val vxu = new XCPTVXUIO
    val vmu = new XCPTVMUIO
    val evac = new XCPTEvacIO

    val pending_memop = Bool(INPUT)
    val vxu_to_xcpt = new io_vxu_to_xcpt_handler().flip
    val evac_to_xcpt = new io_evac_to_xcpt_handler().flip
  }

  val next_hold_issue = Bool()
  val next_hold_seq = Bool()
  val next_hold_tlb = Bool()

  val hold_issue = Reg(next = next_hold_issue, init = Bool(false))
  val hold_seq = Reg(next = next_hold_seq, init = Bool(false))
  val hold_tlb = Reg(next = next_hold_tlb, init = Bool(false))

  next_hold_issue := hold_issue
  next_hold_seq := hold_seq
  next_hold_tlb := hold_tlb

  // output assignments
  io.vxu.issue.stall := hold_issue
  io.vxu.seq.stall := hold_seq
  io.vmu.tlb.stall := hold_tlb
  io.vmu.drain := Bool(false)

  val NORMAL = Bits(0, 3)
  val XCPT_DRAIN = Bits(1, 3)
  val XCPT_FLUSH = Bits(2, 3)
  val XCPT_EVAC = Bits(3, 3)
  val XCPT_DRAIN_EVAC = Bits(4, 3)  
  val HOLD = Bits(5, 3)

  val next_state = Bits(width = 4)
  val state = Reg(next = next_state, init = NORMAL)

  val next_addr = UInt(width = SZ_ADDR)
  val addr = Reg(next = next_addr, init = UInt(0, SZ_ADDR) )

  val next_evac = Bool()
  val evac = Reg(next = next_evac, init = Bool(false))

  val next_kill = Bool()
  val kill = Reg(next = next_kill, init = Bool(false))

  next_state := state
  next_addr := addr
  next_evac := evac
  next_kill := kill

  when (io.xcpt.evac)
  {
    next_evac := Bool(true)
    next_addr := io.xcpt.evac_addr
  }

  when (io.xcpt.kill)
  {
    next_kill := Bool(true)
  }

  io.evac.addr := addr

  //set defaults
  io.vu.busy := (state != NORMAL) && (state != HOLD)
  io.vu.flush_kill := Bool(false)
  io.vu.flush_aiw := Bool(false)
  io.vu.flush_vru := Bool(false)
  io.vu.flush_vmu := Bool(false)
  io.vxu.flush := Bool(false)

  io.evac.start := Bool(false)

  switch (state)
  {
    is (NORMAL)
    {

      when (io.xcpt.exception) 
      {
        next_hold_issue := Bool(true)
        next_hold_seq := Bool(true)
        next_hold_tlb := Bool(true)

        next_evac := Bool(false)
        next_kill := Bool(false)

        next_state := XCPT_DRAIN
      }

      when (io.xcpt.hold)
      {
        next_hold_issue := Bool(true)
        next_hold_seq := Bool(true)
        next_hold_tlb := Bool(true)

        next_state := HOLD
      }

    }

    is (XCPT_DRAIN)
    {
      when (io.vxu_to_xcpt.expand.empty && !io.pending_memop)
      {
        next_state := XCPT_FLUSH
      }

    }

    is (XCPT_FLUSH)
    {
      io.vu.flush_vru := Bool(true)
      io.vu.flush_vmu := Bool(true)
      io.vxu.flush := Bool(true)

      when (kill)
      { 
        io.vu.flush_kill := Bool(true)
        io.vu.flush_aiw := Bool(true)
      }

      when (evac)
      {
        next_hold_tlb := Bool(false)

        next_state := XCPT_EVAC
      }

      when (kill)
      {
        next_hold_issue := Bool(false)
        next_hold_seq := Bool(false)
        next_hold_tlb := Bool(false)
        next_kill := Bool(false)
        
        next_state := NORMAL
      }
    }

    is (XCPT_EVAC)
    {
      io.evac.start := Bool(true)
      io.vmu.drain := Bool(true)

      when (io.evac_to_xcpt.done) {
        next_state := XCPT_DRAIN_EVAC
      }
    }

    is (XCPT_DRAIN_EVAC)
    {
      io.vmu.drain := Bool(true)

      when (!io.pending_memop) {
        next_hold_issue := Bool(false)
        next_hold_seq := Bool(false)
        next_hold_tlb := Bool(false)
        next_evac := Bool(false)
        
        next_state := NORMAL
      }
    }

    is (HOLD)
    {
      when (!io.xcpt.hold) 
      {
        next_hold_issue := Bool(false)
        next_hold_seq := Bool(false)
        next_hold_tlb := Bool(false)

        next_state := NORMAL
      }
    }

  }
}

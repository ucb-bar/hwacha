package hwacha 

import Chisel._
import Node._
import Constants._

class io_xcpt_handler_to_vu extends Bundle
{
  val flush = Bool(OUTPUT)
}

class io_xcpt_handler_to_aiw extends Bundle
{
  val flush = Bool(OUTPUT)
}

class io_xcpt_handler_to_issue extends Bundle 
{
  val stall = Bool(OUTPUT)
}

class io_xcpt_handler_to_seq extends Bundle 
{
  val stall = Bool(OUTPUT)
}

class io_xcpt_handler_to_tlb extends Bundle 
{
  val stall = Bool(OUTPUT)
}

class io_xcpt_handler_to_vru extends Bundle
{
  val flush = Bool(OUTPUT)
}

class io_xcpt_handler_to_vxu extends Bundle
{
  val flush = Bool(OUTPUT)
  val issue = new io_xcpt_handler_to_issue
  val seq = new io_xcpt_handler_to_seq
}

class io_xcpt_handler_to_vmu extends Bundle 
{
  val flush = Bool(OUTPUT)
  val tlb = new io_xcpt_handler_to_tlb()
}

class io_xcpt_handler_to_evac extends Bundle
{
  val start = Bool(OUTPUT)
  val addr = UFix(SZ_ADDR, OUTPUT)
}

class io_xcpt_handler extends Bundle {
  val xcpt = new io_xcpt().flip()

  val xcpt_to_vu = new io_xcpt_handler_to_vu()
  val xcpt_to_vru = new io_xcpt_handler_to_vru()
  val xcpt_to_vxu = new io_xcpt_handler_to_vxu()
  val xcpt_to_vmu = new io_xcpt_handler_to_vmu()
  val xcpt_to_aiw = new io_xcpt_handler_to_aiw()
  val xcpt_to_evac = new io_xcpt_handler_to_evac()

  val vxu_to_xcpt = new io_vxu_to_xcpt_handler().flip()
  val vmu_to_xcpt = new io_vmu_to_xcpt_handler().flip()
  val evac_to_xcpt = new io_evac_to_xcpt_handler().flip()
}

class vuXCPTHandler extends Component 
{
  val io = new io_xcpt_handler()

  val next_hold_issue = Wire(){ Bool() }
  val next_hold_seq = Wire(){ Bool() }
  val next_hold_tlb = Wire(){ Bool() }

  val hold_issue = Reg(next_hold_issue, resetVal = Bool(false))
  val hold_seq = Reg(next_hold_seq, resetVal = Bool(false))
  val hold_tlb = Reg(next_hold_tlb, resetVal = Bool(false))

  next_hold_issue := hold_issue
  next_hold_seq := hold_seq
  next_hold_tlb := hold_tlb

  // output assignments
  io.xcpt_to_vxu.issue.stall := hold_issue
  io.xcpt_to_vxu.seq.stall := hold_seq
  io.xcpt_to_vmu.tlb.stall := hold_tlb

  val next_saved_earliest_ptr = Wire(){ Bool() }
  val next_earliest_ptr = Wire(){ UFix(width=SZ_LGBANK) }

  val saved_earliest_ptr = Reg(next_saved_earliest_ptr, resetVal = Bool(false) )
  val earliest_ptr = Reg(next_earliest_ptr, resetVal = UFix(0, SZ_LGBANK) )

  next_saved_earliest_ptr := saved_earliest_ptr
  next_earliest_ptr := earliest_ptr

  val NORMAL = Bits(0, 3)
  val XCPT_DRAIN = Bits(1, 3)
  val XCPT_FLUSH = Bits(2, 3)
  val XCPT_EVAC = Bits(3, 3)
  val XCPT_DRAIN_EVAC = Bits(4, 3)  
  val XCPT_ACK = Bits(5, 3)
  val HOLD = Bits(6, 3)
  val HOLD_WAIT = Bits(7, 3)

  val next_state = Wire(){ Bits(width = 4) }
  val state = Reg(next_state, resetVal = NORMAL)

  val next_addr = Wire(){ UFix(width = SZ_ADDR) }
  val addr = Reg(next_addr, resetVal = UFix(0, SZ_ADDR) )

  val next_backup = Wire(){ Bool() }
  val backup = Reg(next_backup, resetVal = Bool(false))

  val next_kill = Wire(){ Bool() }
  val kill = Reg(next_kill, resetVal = Bool(false))

  next_state := state
  next_addr := addr
  next_backup := backup
  next_kill := kill

  when (io.xcpt.backup)
  {
    next_backup := Bool(true)
    next_addr := io.xcpt.backup_addr
  }

  when (io.xcpt.kill)
  {
    next_kill := Bool(true)
  }

  io.xcpt_to_evac.addr := addr

  //set defaults
  io.xcpt.exception_ack_valid := Bool(false)

  io.xcpt_to_vu.flush := Bool(false)
  io.xcpt_to_aiw.flush := Bool(false)
  io.xcpt_to_vru.flush := Bool(false)
  io.xcpt_to_vxu.flush := Bool(false)
  io.xcpt_to_vmu.flush := Bool(false)

  io.xcpt_to_evac.start := Bool(false)

  switch (state)
  {
    is (NORMAL)
    {

      when (io.xcpt.exception) 
      {
        next_hold_issue := Bool(true)
        next_hold_seq := Bool(true)
        next_hold_tlb := Bool(true)

        next_backup := Bool(false)
        next_kill := Bool(false)

        next_state := XCPT_DRAIN
      }

      when (io.xcpt.hold)
      {
        next_hold_seq := Bool(true)

        next_state := HOLD
      }

    }

    is (XCPT_DRAIN)
    {
      when (io.vxu_to_xcpt.expand.empty && io.vmu_to_xcpt.no_pending_load_store)
      {
        next_state := XCPT_FLUSH
      }

    }

    is (XCPT_FLUSH)
    {
      io.xcpt_to_vru.flush := Bool(true)
      io.xcpt_to_vxu.flush := Bool(true)
      io.xcpt_to_vmu.flush := Bool(true)
      when (kill)
      { 
        io.xcpt_to_vu.flush := Bool(true)
        io.xcpt_to_aiw.flush := Bool(true)
      }

      when(backup)
      {
        next_hold_tlb := Bool(false)

        next_state := XCPT_EVAC
      }
      when(kill)
      {
        next_state := XCPT_ACK
      }
    }

    is (XCPT_EVAC)
    {
      io.xcpt_to_evac.start := Bool(true)

      when (io.evac_to_xcpt.done) 
      {
        next_state := XCPT_DRAIN_EVAC
      }
    }

    is (XCPT_DRAIN_EVAC)
    {
      when (io.vmu_to_xcpt.no_pending_load_store)
      {
        next_state := XCPT_ACK
      }
    }

    is (XCPT_ACK)
    {
      io.xcpt.exception_ack_valid := Bool(true)

      when (io.xcpt.exception_ack_ready) 
      {
        next_hold_issue := Bool(false)
        next_hold_seq := Bool(false)
        next_hold_tlb := Bool(false)
        next_backup := Bool(false)
        next_kill := Bool(false)
        
        next_state := NORMAL
      }

    }

    is (HOLD)
    {
      when(!saved_earliest_ptr && io.vxu_to_xcpt.seq.fire_any)
      {
        next_saved_earliest_ptr := Bool(true)
        next_earliest_ptr := io.vxu_to_xcpt.seq.next_ptr1
      }
      
      when (!io.xcpt.hold) 
      {
        next_saved_earliest_ptr := Bool(false)

        next_state := HOLD_WAIT
      }
    }

    is (HOLD_WAIT)
    {
      when(earliest_ptr === io.vxu_to_xcpt.seq.next_ptr1)
      {
        next_hold_seq := Bool(false)
        
        next_state := NORMAL
      }
    }
  }
}

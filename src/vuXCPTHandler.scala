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
  val xcpt_backup = new io_xcpt_backup().flip()
  val xcpt_resume = new io_xcpt_resume().flip()
  val xcpt_kill  = new io_xcpt_kill().flip()

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

  io.xcpt_to_evac.addr := io.xcpt_backup.exception_addr

  val next_saved_earliest_ptr = Wire(){ Bool() }
  val next_earliest_ptr = Wire(){ UFix(width=SZ_LGBANK) }

  val saved_earliest_ptr = Reg(next_saved_earliest_ptr, resetVal = Bool(false) )
  val earliest_ptr = Reg(next_earliest_ptr, resetVal = UFix(0, SZ_LGBANK) )

  next_saved_earliest_ptr := saved_earliest_ptr
  next_earliest_ptr := earliest_ptr

  val NORMAL = Bits(0, 3)
  val XCPT_REQUEST = Bits(1, 3)
  val XCPT_DRAIN = Bits(2, 3)
  val XCPT_FLUSH = Bits(3, 3)
  val XCPT_EVAC = Bits(4, 3)
  val XCPT_ACK = Bits(5, 3)
  val HOLD = Bits(6, 3)
  val HOLD_WAIT = Bits(7, 3)

  val state_next = Wire(){ Bits(width = 3) }
  val state = Reg(state_next, resetVal = NORMAL)

  state_next := state

  //set defaults
  io.xcpt_backup.exception_ack_valid := Bool(false)
  io.xcpt_kill.kill_ack_valid := Bool(false)

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

      when (io.xcpt_backup.exception || io.xcpt_kill.kill) 
      {
        state_next := XCPT_REQUEST
      }

      when (io.xcpt_resume.hold)
      {
        next_hold_seq := Bool(true)

        state_next := HOLD
      }

    }

    is (XCPT_REQUEST)
    {
      next_hold_issue := Bool(true)
      next_hold_seq := Bool(true)
      next_hold_tlb := Bool(true)

      state_next := XCPT_DRAIN
    }

    is (XCPT_DRAIN)
    {
      when(io.vxu_to_xcpt.expand.empty && io.vmu_to_xcpt.no_pending_load_store)
      {
        state_next := XCPT_FLUSH
      }

    }

    is (XCPT_FLUSH)
    {
      io.xcpt_to_vru.flush := Bool(true)
      io.xcpt_to_vxu.flush := Bool(true)
      io.xcpt_to_vmu.flush := Bool(true)
      when (io.xcpt_kill.kill) 
      { 
        io.xcpt_to_vu.flush := Bool(true)
        io.xcpt_to_aiw.flush := Bool(true)
      }
      next_hold_tlb := Bool(false)

      state_next := XCPT_EVAC
    }

    is (XCPT_EVAC)
    {
      io.xcpt_to_evac.start := Bool(true)

      when (io.evac_to_xcpt.done) 
      {
        state_next := XCPT_ACK
      }
    }

    is (XCPT_ACK)
    {
      when (io.xcpt_backup.exception)
      {
        io.xcpt_backup.exception_ack_valid := Bool(true)

        when (io.xcpt_backup.exception_ack_ready) 
        {
          next_hold_issue := Bool(false)
          next_hold_seq := Bool(false)

          state_next := NORMAL
        }
      }

      when(io.xcpt_kill.kill)
      {
        io.xcpt_kill.kill_ack_valid := Bool(true)
        
        when (io.xcpt_kill.kill_ack_ready)
        {
          next_hold_issue := Bool(false)
          next_hold_seq := Bool(false)

          state_next := NORMAL
        }
      }
      
    }

    is (HOLD)
    {
      when(!saved_earliest_ptr && io.vxu_to_xcpt.seq.fire_any)
      {
        next_saved_earliest_ptr := Bool(true)
        next_earliest_ptr := io.vxu_to_xcpt.seq.next_ptr1
      }
      
      when (!io.xcpt_resume.hold) 
      {
        next_saved_earliest_ptr := Bool(false)

        state_next := HOLD_WAIT
      }
    }

    is (HOLD_WAIT)
    {
      when(earliest_ptr === io.vxu_to_xcpt.seq.next_ptr1)
      {
        next_hold_seq := Bool(false)
        
        state_next := NORMAL
      }
    }
  }
}

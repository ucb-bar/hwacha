package hwacha 

class io_xcpt_backup extends Bundle {
  val exception = Bool(OUTPUT)
  val exception_addr = UFix(SZ_ADDR, OUTPUT)
  val exception_ack_valid = Bool(INPUT)
  val exception_ack_ready = Bool(OUTPUT)
}

class io_xcpt_kill extends Bundle {
  val kill = Bool(OUTPUT)
  val kill_ack_valid = Bool(INPUT)
  val kill_ack_ready = Bool(OUTPUT)
}

class io_xcpt_handler_to_issue extends Bundle {
  val stall = Bool(OUTPUT)
}

class io_xcpt_handler_to_tlb extends Bundle {
  val set_stall = Bool(OUTPUT)
  val clear_stall = Bool(OUTPUT)
}

class io_xpct_handler extends Bundle {
  val xpct_backup = new io_xpct_backup().flip()
  val xpct_kill  = new io_xcpt_kill().flip()

  val xcpt_to_issue = new io_xcpt_handler_to_issue()
  val xcpt_to_tlb = new io_xcpt_handle_to_tlb()
}

class vuXCPTHandler extends Component 
{
  val io - new io_xpct_handler()

  val NORMAL = Bits(0, 3)
  val XCPT_REQUEST = Bits(1, 3)
  val XCPT_DRAIN = Bits(2, 3)
  val XCPT_FLUSh = Bits(3, 3)
  val XCPT_EVAC = Bits(4, 3)
  val XCPT_ACK = Bits(5, 3)
  val HOLD = Bits(6, 3)
  val HOLD_WAIT = Bits(7, 3)

  val state_next = Wire(){ Bits(width = 3) }
  val state = Reg(state_next, resetVal = NORMAL)

  state_next := state

  //set defaults
  io.xcpt_to_issue.stall := Bool(false)
  io.xcpt_to_tlb.stall := Bool(false)

  switch (state)
  {
    is (NORMAL)
    {

      when (io.xpct_backup.exception || io.xpct_kill.kill) 
      {
        state_next := XCPT_REQUEST
      }

    }

    is (XCPT_REQUEST)
    {
      io.xcpt_to_issue.stall := Bool(true)
      io.xcpt_to_tlb.set_stall := Bool(true)

      state_next := XCPT_DRAIN
    }

    is (XCPT_DRAIN)
    {
      io.xcpt_to_issue.stall := Bool(true)

    }

    is (XCPT_FLUSH)
    {
      io.xcpt_to_issue.stall := Bool(true)

    }

    is (XCPT_EVAC)
    {
      io.xcpt_to_issue.stall := Bool(true)

    }

    is (XCPT_ACK)
    {
      io.xcpt_to_issue.stall := Bool(true)

    }

    is (HOLD)
    {

    }

    is (HOLD_WAIT)
    {

    }
  }
}

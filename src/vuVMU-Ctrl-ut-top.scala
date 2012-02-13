package hwacha {
  import Chisel._
  import Node._
  import Interface._

  class vuVMU_Ctrl_ut_topIO extends Bundle {
    val utmcmdq           = new utmcmdqIO()
    val utmimmq           = new utmimmqIO()
    val utmrespq          = new utmrespqIO()

    val iscmdq_enq_bits   = Bits(UT_ISCMD_SZ, OUTPUT)
    val iscmdq_enq_val    = Bool(OUTPUT)
    val iscmdq_enq_rdy    = Bool(INPUT)

    val wbcmdq_enq_bits   = Bits(UT_WBCMD_SZ, OUTPUT)
    val wbcmdq_enq_val    = Bool(OUTPUT)
    val wbcmdq_enq_rdy    = Bool(INPUT)

    val stcmdq_enq_bits   = Bits(UT_STCMD_SZ, OUTPUT)
    val stcmdq_enq_val    = Bool(OUTPUT)
    val stcmdq_enq_rdy    = Bool(INPUT)

    val issue_busy        = Bool(INPUT)
    val store_busy        = Bool(INPUT)
  }

  class vuVMU_Ctrl_ut_top extends Component {

    val io = new vuVMU_Ctrl_ut_topIO()

    val VMU_Ctrl_Idle     = Bits(0, 3)
    val VMU_Ctrl_Load     = Bits(1, 3)
    val VMU_Ctrl_Store    = Bits(2, 3)
    val VMU_Ctrl_AMO      = Bits(3, 3)
    val VMU_Ctrl_Sync     = Bits(4, 3)
    val VMU_Ctrl_SyncWait = Bits(5, 3)
    val VMU_Ctrl_Invalid  = Bits(6, 3)

    val state = Reg(resetVal = VMU_Ctrl_Idle)

    val cmd_amo = Wire(){Bool()}

    val cmd = io.utmcmdq.bits(UTMCMD_CMDCODE)
    val vlen = io.utmcmdq.bits(UTMCMD_VLEN_M1)
    val addr = io.utmimmq.bits

    io.iscmdq_enq_bits := Cat(addr, vlen)
    io.wbcmdq_enq_bits := Cat(cmd_amo, cmd(3,0), vlen)
    io.stcmdq_enq_bits := Cat(cmd_amo, cmd(4,0), addr, vlen)

    io.utmcmdq.rdy <== Bool(false)
    io.utmimmq.rdy <== Bool(false)
    io.utmrespq.bits <== Bits(0, UTMRESP_SZ)
    io.utmrespq.valid <== Bool(false)
    io.iscmdq_enq_val <== Bool(false)
    io.wbcmdq_enq_val <== Bool(false)
    io.stcmdq_enq_val <== Bool(false)
    cmd_amo <== Bool(false)

    switch(state)
    {
      is(VMU_Ctrl_Idle)
      {
        when(io.utmcmdq.valid) 
        {
          state <== MuxCase(
            VMU_Ctrl_Invalid, Array(
              (cmd(7,4) === Bits("b0000", 4) && 
                (cmd(3,0) === Bits("b1100", 4) || cmd(3,0) === Bits("b1101", 4) || cmd(3,0) === Bits("b1110", 4) || cmd(3,0) === Bits("b1111", 4))) -> VMU_Ctrl_Sync,
              (cmd(7,4) === Bits("b0000", 4) &&
                !(cmd(3,0) === Bits("b1100", 4) || cmd(3,0) === Bits("b1101", 4) || cmd(3,0) === Bits("b1110", 4) || cmd(3,0) === Bits("b1111", 4))) -> VMU_Ctrl_Invalid,
              (cmd(7,4) === Bits("b1100", 4) && io.utmimmq.valid && io.iscmdq_enq_rdy && io.wbcmdq_enq_rdy && !io.store_busy) -> VMU_Ctrl_Load,
              (cmd(7,4) === Bits("b1101", 4) && io.utmimmq.valid && io.stcmdq_enq_rdy && !io.issue_busy) -> VMU_Ctrl_Store,
              ((cmd(7,4) === Bits("b1110", 4) || cmd(7,4) === Bits("b1111")) && io.stcmdq_enq_rdy && io.wbcmdq_enq_rdy && !io.issue_busy) -> VMU_Ctrl_AMO
            ))
        }
        when(!io.utmcmdq.valid)
        {
          state <== VMU_Ctrl_Idle
        }
      }
      is(VMU_Ctrl_Load)
      {
        io.utmcmdq.rdy <== Bool(true)
        io.utmimmq.rdy <== Bool(true)
        io.iscmdq_enq_val <== Bool(true)
        io.wbcmdq_enq_val <== Bool(true)
        state <== VMU_Ctrl_Idle
      }
      is(VMU_Ctrl_Store)
      {
        io.utmcmdq.rdy <== Bool(true)
        io.utmimmq.rdy <== Bool(true)
        io.stcmdq_enq_val <== Bool(true)
        state <== VMU_Ctrl_Idle
      }
      is(VMU_Ctrl_AMO)
      {
        io.utmcmdq.rdy <== Bool(true)
        io.stcmdq_enq_val <== Bool(true)
        io.wbcmdq_enq_val <== Bool(true)
        cmd_amo <== Bool(true)
        state <== VMU_Ctrl_Idle
      }
      is(VMU_Ctrl_Sync)
      {
        when(!io.store_busy)
        {
          io.utmcmdq.rdy <== Bool(true)
          io.utmrespq.valid <== Bool(true)
          io.utmrespq.bits <== Bits(1)
          when(io.utmrespq.rdy) { state <== VMU_Ctrl_Idle }
          when(!io.utmrespq.rdy) { state <== VMU_Ctrl_SyncWait }
        }
      }
      is(VMU_Ctrl_SyncWait)
      {
        io.utmrespq.valid <== Bool(true)
        io.utmrespq.bits <== Bits(1)
        when(io.utmrespq.rdy) { state <== VMU_Ctrl_Idle }
        when(!io.utmrespq.rdy) { state <== VMU_Ctrl_SyncWait }
      }
      is(VMU_Ctrl_Invalid)
      {
        io.utmcmdq.rdy <== Bool(true)
        state <== VMU_Ctrl_Idle
      }
    }

  }
}

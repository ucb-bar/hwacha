package hwacha {
  import Chisel._
  import Node._
  import Interface._
  import queues._

  class utmcmdqIO extends Bundle
  {
    val valid = Bool(INPUT);
    val rdy   = Bool(OUTPUT);
    val bits  = Bits(UTMCMD_SZ, INPUT);
  }

  class utmimmqIO extends Bundle
  {
    val valid = Bool(INPUT);
    val rdy   = Bool(OUTPUT);
    val bits  = Bits(UTMIMM_SZ, INPUT);
  }

  class utmrespqIO extends Bundle
  {
    val valid = Bool(OUTPUT);
    val rdy   = Bool(INPUT);
    val bits  = Bits(UTMRESP_SZ, OUTPUT);
  }

  // vmr queue interface
  class utaq_deqIO extends Bundle
  {
    val valid = Bool(INPUT);
    val rdy   = Bool(OUTPUT);
    val bits  = Bits(32, INPUT);
  }

  // load data queue interface
  class utldqIO extends Bundle
  {
    val enq_val = Bool(OUTPUT);
    val enq_rdy   = Bool(INPUT);
    val enq_bits  = Bits(65, OUTPUT);
    val deq_rdy = Bool(INPUT);
    val wb_done = Bool(OUTPUT);
  }

  // store data queue interface
  class utsdq_deqIO extends Bundle
  {
    val valid = Bool(INPUT);
    val rdy   = Bool(OUTPUT);
    val bits  = Bits(65, INPUT);
  }

  // D$ interface
  // request
  class ut_dcachereqIO extends Bundle
  {
    val addr  = Bits(30, OUTPUT);
    val tag   = Bits(12, OUTPUT);
    val data  = Bits(64, OUTPUT);
    val wmask = Bits(8, OUTPUT);
    val op    = Bits(4, OUTPUT);
    val valid = Bool(OUTPUT);
    val rdy   = Bool(INPUT);
  }

  // response
  class ut_dcacherespIO extends Bundle
  {
    val data  = Bits(64, INPUT);
    val tag   = Bits(12, INPUT);
    val valid = Bool(INPUT);
  }

  class vuVMU_Ctrl_utIO extends Bundle
  {
    val utmcmdq     = new utmcmdqIO();
    val utmimmq     = new utmimmqIO();
    val utmrespq    = new utmrespqIO();
    val utaq_deq    = new utaq_deqIO();
    val utldq       = new utldqIO();
    val utsdq_deq   = new utsdq_deqIO();
    val dcachereq   = new ut_dcachereqIO();
    val dcacheresp  = new ut_dcacherespIO();
  }

  class vuVMU_Ctrl_ut extends Component
  {
    val io = new vuVMU_Ctrl_utIO();

    val ctrl_ut_top     = new vuVMU_Ctrl_ut_top();
    val iscmdq          = new queuePipePF(UT_ISCMD_SZ, 4);
    val ctrl_ut_issue   = new vuVMU_Ctrl_ut_issue();
    // 12 bits for tag, 30 bits for address
    val lrq             = new queuePipePF(30+12, 4);
    val roq             = new vuVMU_ROQ(65, 8, 3);
    val wbcmdq          = new queuePipePF(UT_WBCMD_SZ, 4);
    val ctrl_ut_wb      = new vuVMU_Ctrl_ut_wb();
    val stcmdq          = new queuePipePF(UT_STCMD_SZ, 4);
    val ctrl_ut_store   = new vuVMU_Ctrl_ut_store();
    // 4 bits for opcode, 12 bits for tag, 30 bits for address, 8 bits for write mask, 64 bits of data
    val srq             = new queuePipePF(4+12+30+8+64, 2);

    roq.io.roq_enq_val      := io.dcacheresp.valid;
    roq.io.roq_enq_tag_bits := io.dcacheresp.tag(7,0).toUFix;
    val dcacheresp_addr_lsb =  io.dcacheresp.tag(10,8);

    roq.io.roq_enq_data_bits := Cat(Bits(0, 1), MuxCase(
      Bits(0, 64), Array(
        (dcacheresp_addr_lsb === Bits(0, 3)) -> io.dcacheresp.data,
        (dcacheresp_addr_lsb === Bits(1, 3)) -> Cat(Bits(0, 8), io.dcacheresp.data(63,8)),
        (dcacheresp_addr_lsb === Bits(2, 3)) -> Cat(Bits(0, 16), io.dcacheresp.data(63,16)),
        (dcacheresp_addr_lsb === Bits(3, 3)) -> Cat(Bits(0, 24), io.dcacheresp.data(63,24)),
        (dcacheresp_addr_lsb === Bits(4, 3)) -> Cat(Bits(0, 32), io.dcacheresp.data(63,32)),
        (dcacheresp_addr_lsb === Bits(5, 3)) -> Cat(Bits(0, 40), io.dcacheresp.data(63,40)),
        (dcacheresp_addr_lsb === Bits(6, 3)) -> Cat(Bits(0, 48), io.dcacheresp.data(63,48)),
        (dcacheresp_addr_lsb === Bits(7, 3)) -> Cat(Bits(0, 56), io.dcacheresp.data(63,56))
      )));

    // load issue and store (for AMOs) both access ROQ and UTAQ but not at the same time (I hope!!)
    roq.io.roq_deq_tag_rdy  := ctrl_ut_issue.io.roq_deq_tag_rdy || ctrl_ut_store.io.roq_deq_tag_rdy;
    io.utaq_deq.rdy         := ctrl_ut_issue.io.utaq_deq_rdy || ctrl_ut_store.io.utaq_deq_rdy;

    // ctrl_ut_top
    ctrl_ut_top.io.utmcmdq        <> io.utmcmdq;
    ctrl_ut_top.io.utmimmq        <> io.utmimmq;
    ctrl_ut_top.io.utmrespq       <> io.utmrespq;
 
    iscmdq.io.enq.bits            := ctrl_ut_top.io.iscmdq_enq_bits;
    iscmdq.io.enq.valid             := ctrl_ut_top.io.iscmdq_enq_val;
    ctrl_ut_top.io.iscmdq_enq_rdy := iscmdq.io.enq.ready;

    wbcmdq.io.enq.bits            := ctrl_ut_top.io.wbcmdq_enq_bits;
    wbcmdq.io.enq.valid             := ctrl_ut_top.io.wbcmdq_enq_val;
    ctrl_ut_top.io.wbcmdq_enq_rdy := wbcmdq.io.enq.ready;

    stcmdq.io.enq.bits            := ctrl_ut_top.io.stcmdq_enq_bits;
    stcmdq.io.enq.valid             := ctrl_ut_top.io.stcmdq_enq_val;
    ctrl_ut_top.io.stcmdq_enq_rdy := stcmdq.io.enq.ready;

    ctrl_ut_top.io.issue_busy     := ctrl_ut_issue.io.issue_busy;
    ctrl_ut_top.io.store_busy     := ctrl_ut_store.io.store_busy;

    // ctrl_ut_issue
    ctrl_ut_issue.io.iscmdq_deq_bits  := iscmdq.io.deq.bits.toUFix;
    ctrl_ut_issue.io.iscmdq_deq_val   := iscmdq.io.deq.valid;
    iscmdq.io.deq.ready                 := ctrl_ut_issue.io.iscmdq_deq_rdy;
    
    ctrl_ut_issue.io.utaq_deq_bits    := io.utaq_deq.bits.toUFix;
    ctrl_ut_issue.io.utaq_deq_val     := io.utaq_deq.valid;
    
    lrq.io.enq.bits                   := Cat(ctrl_ut_issue.io.lrq_enq_addr_bits, ctrl_ut_issue.io.lrq_enq_tag_bits);
    lrq.io.enq.valid                    := ctrl_ut_issue.io.lrq_enq_val;
    ctrl_ut_issue.io.lrq_enq_rdy      := lrq.io.enq.ready;
    
    ctrl_ut_issue.io.roq_deq_tag_bits := roq.io.roq_deq_tag_bits.toUFix;
    ctrl_ut_issue.io.roq_deq_tag_val  := roq.io.roq_deq_tag_val;

    // ctrl_ut_wb
    ctrl_ut_wb.io.ldq               <> io.utldq;
    
    ctrl_ut_wb.io.wbcmdq_deq_bits   := wbcmdq.io.deq.bits;
    ctrl_ut_wb.io.wbcmdq_deq_val    := wbcmdq.io.deq.valid;
    wbcmdq.io.deq.ready               := ctrl_ut_wb.io.wbcmdq_deq_rdy;

    ctrl_ut_wb.io.roq_deq_bits      := roq.io.roq_deq_data_bits(63,0);
    ctrl_ut_wb.io.roq_deq_val       := roq.io.roq_deq_data_val;
    roq.io.roq_deq_data_rdy         := ctrl_ut_wb.io.roq_deq_rdy;

    // ctrl_ut_store
    ctrl_ut_store.io.stcmdq_deq_bits  := stcmdq.io.deq.bits.toUFix;
    ctrl_ut_store.io.stcmdq_deq_val   := stcmdq.io.deq.valid;
    stcmdq.io.deq.ready                 := ctrl_ut_store.io.stcmdq_deq_rdy;
  
    ctrl_ut_store.io.sdq_deq          <> io.utsdq_deq;

    ctrl_ut_store.io.utaq_deq_bits    := io.utaq_deq.bits.toUFix;
    ctrl_ut_store.io.utaq_deq_val     := io.utaq_deq.valid;
    
    ctrl_ut_store.io.roq_deq_tag_bits := roq.io.roq_deq_tag_bits.toUFix;
    ctrl_ut_store.io.roq_deq_tag_val  := roq.io.roq_deq_tag_val;

    srq.io.enq.bits                   := Cat(ctrl_ut_store.io.srq_enq_op_bits,
                                              ctrl_ut_store.io.srq_enq_tag_bits,
                                              ctrl_ut_store.io.srq_enq_addr_bits,
                                              ctrl_ut_store.io.srq_enq_wmask_bits,
                                              ctrl_ut_store.io.srq_enq_data_bits);
    srq.io.enq.valid                    := ctrl_ut_store.io.srq_enq_val;
    ctrl_ut_store.io.srq_enq_rdy      := srq.io.enq.ready;

    // stores are given priority over loads
    io.dcachereq.valid  := lrq.io.deq.valid || srq.io.deq.valid;
    lrq.io.deq.ready      := Mux(srq.io.deq.valid, Bool(false), io.dcachereq.rdy);
    srq.io.deq.ready      := io.dcachereq.rdy;
    io.dcachereq.data   := srq.io.deq.bits(63, 0);
    io.dcachereq.wmask  := srq.io.deq.bits(71 ,64);
    io.dcachereq.addr   := Mux(srq.io.deq.valid, srq.io.deq.bits(101, 72), lrq.io.deq.bits(41, 12));
    io.dcachereq.tag    := Mux(srq.io.deq.valid, srq.io.deq.bits(113, 102), lrq.io.deq.bits(11, 0));
    io.dcachereq.op     := Mux(srq.io.deq.valid, srq.io.deq.bits(117, 114), Bits("b0000", 4));
  }
}

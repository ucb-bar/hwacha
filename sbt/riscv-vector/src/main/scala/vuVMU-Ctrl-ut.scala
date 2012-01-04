package riscvVector {
  import Chisel._
  import Node._
  import Interface._
  import queues._

  class utmcmdqIO extends Bundle
  {
    val valid = Bool('input);
    val rdy   = Bool('output);
    val bits  = Bits(UTMCMD_SZ, 'input);
  }

  class utmimmqIO extends Bundle
  {
    val valid = Bool('input);
    val rdy   = Bool('output);
    val bits  = Bits(UTMIMM_SZ, 'input);
  }

  class utmrespqIO extends Bundle
  {
    val valid = Bool('output);
    val rdy   = Bool('input);
    val bits  = Bits(UTMRESP_SZ, 'output);
  }

  // vmr queue interface
  class utaq_deqIO extends Bundle
  {
    val valid = Bool('input);
    val rdy   = Bool('output);
    val bits  = Bits(32, 'input);
  }

  // load data queue interface
  class utldqIO extends Bundle
  {
    val enq_val = Bool('output);
    val enq_rdy   = Bool('input);
    val enq_bits  = Bits(65, 'output);
    val deq_rdy = Bool('input);
    val wb_done = Bool('output);
  }

  // store data queue interface
  class utsdq_deqIO extends Bundle
  {
    val valid = Bool('input);
    val rdy   = Bool('output);
    val bits  = Bits(65, 'input);
  }

  // D$ interface
  // request
  class dcachereqIO extends Bundle
  {
    val addr  = Bits(30, 'output);
    val tag   = Bits(12, 'output);
    val data  = Bits(64, 'output);
    val wmask = Bits(8, 'output);
    val op    = Bits(4, 'output);
    val valid = Bool('output);
    val rdy   = Bool('intput);
  }

  // response
  class dcacherespIO extends Bundle
  {
    val data  = Bits(64, 'input);
    val tag   = Bits(12, 'input);
    val valid = Bool('input);
  }

  class vuVMU_Ctrl_utIO extends Bundle
  {
    val utmcmdq     = new utmcmdqIO();
    val utmimmq     = new utmimmqIO();
    val utmrespq    = new utmrespqIO();
    val utaq_deq    = new utaq_deqIO();
    val utldq       = new utldqIO();
    val utsdq_deq   = new utsdq_deqIO();
    val dcachereq   = new dcachereqIO();
    val dcacheresp  = new dcacherespIO();
  }

  class vuVMU_Ctrl_ut extends Component
  {
    val io = new vuVMU_Ctrl_utIO();

    val ctrl_ut_top     = new vuVMU_Ctrl_ut_top();
    val iscmdq          = new queuePipePF(UT_ISCMD_SZ, 4, 1);
    val ctrl_ut_issue   = new vuVMU_Ctrl_ut_issue();
    // 12 bits for tag, 30 bits for address
    val lrq             = new queuePipePF(30+12, 4, 1);
    val roq             = new vuVMU_ROQ(65, 256, 8);
    val wbcmdq          = new queuePipePF(UT_WBCMD_SZ, 4, 1);
    val ctrl_ut_wb      = new vuVMU_Ctrl_ut_wb();
    val stcmdq          = new queuePipePF(UT_STCMD_SZ, 4, 1);
    val ctrl_ut_store   = new vuVMU_Ctrl_ut_store();
    // 4 bits for opcode, 12 bits for tag, 30 bits for address, 8 bits for write mask, 64 bits of data
    val srq             = new queuePipePF(4+12+30+8+64, 2, 1);

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
    ctrl_ut_top.io.utmcmdq        ^^ io.utmcmdq;
    ctrl_ut_top.io.utmimmq        ^^ io.utmimmq;
    ctrl_ut_top.io.utmrespq       ^^ io.utmrespq;
 
    iscmdq.io.enq_bits            := ctrl_ut_top.io.iscmdq_enq_bits;
    iscmdq.io.enq_val             := ctrl_ut_top.io.iscmdq_enq_val;
    ctrl_ut_top.io.iscmdq_enq_rdy := iscmdq.io.enq_rdy;

    wbcmdq.io.enq_bits            := ctrl_ut_top.io.wbcmdq_enq_bits;
    wbcmdq.io.enq_val             := ctrl_ut_top.io.wbcmdq_enq_val;
    ctrl_ut_top.io.wbcmdq_enq_rdy := wbcmdq.io.enq_rdy;

    stcmdq.io.enq_bits            := ctrl_ut_top.io.stcmdq_enq_bits;
    stcmdq.io.enq_val             := ctrl_ut_top.io.stcmdq_enq_val;
    ctrl_ut_top.io.stcmdq_enq_rdy := stcmdq.io.enq_rdy;

    ctrl_ut_top.io.issue_busy     := ctrl_ut_issue.io.issue_busy;
    ctrl_ut_top.io.store_busy     := ctrl_ut_store.io.store_busy;

    // ctrl_ut_issue
    ctrl_ut_issue.io.iscmdq_deq_bits  := iscmdq.io.deq_bits.toUFix;
    ctrl_ut_issue.io.iscmdq_deq_val   := iscmdq.io.deq_val;
    iscmdq.io.deq_rdy                 := ctrl_ut_issue.io.iscmdq_deq_rdy;
    
    ctrl_ut_issue.io.utaq_deq_bits    := io.utaq_deq.bits.toUFix;
    ctrl_ut_issue.io.utaq_deq_val     := io.utaq_deq.valid;
    
    lrq.io.enq_bits                   := Cat(ctrl_ut_issue.io.lrq_enq_addr_bits, ctrl_ut_issue.io.lrq_enq_tag_bits);
    lrq.io.enq_val                    := ctrl_ut_issue.io.lrq_enq_val;
    ctrl_ut_issue.io.lrq_enq_rdy      := lrq.io.enq_rdy;
    
    ctrl_ut_issue.io.roq_deq_tag_bits := roq.io.roq_deq_tag_bits.toUFix;
    ctrl_ut_issue.io.roq_deq_tag_val  := roq.io.roq_deq_tag_val;

    // ctrl_ut_wb
    ctrl_ut_wb.io.ldq               ^^ io.utldq;
    
    ctrl_ut_wb.io.wbcmdq_deq_bits   := wbcmdq.io.deq_bits;
    ctrl_ut_wb.io.wbcmdq_deq_val    := wbcmdq.io.deq_val;
    wbcmdq.io.deq_rdy               := ctrl_ut_wb.io.wbcmdq_deq_rdy;

    ctrl_ut_wb.io.roq_deq_bits      := roq.io.roq_deq_data_bits(63,0);
    ctrl_ut_wb.io.roq_deq_val       := roq.io.roq_deq_data_val;
    roq.io.roq_deq_data_rdy         := ctrl_ut_wb.io.roq_deq_rdy;

    // ctrl_ut_store
    ctrl_ut_store.io.stcmdq_deq_bits  := stcmdq.io.deq_bits.toUFix;
    ctrl_ut_store.io.stcmdq_deq_val   := stcmdq.io.deq_val;
    stcmdq.io.deq_rdy                 := ctrl_ut_store.io.stcmdq_deq_rdy;
  
    ctrl_ut_store.io.sdq_deq          ^^ io.utsdq_deq;

    ctrl_ut_store.io.utaq_deq_bits    := io.utaq_deq.bits.toUFix;
    ctrl_ut_store.io.utaq_deq_val     := io.utaq_deq.valid;
    
    ctrl_ut_store.io.roq_deq_tag_bits := roq.io.roq_deq_tag_bits.toUFix;
    ctrl_ut_store.io.roq_deq_tag_val  := roq.io.roq_deq_tag_val;

    srq.io.enq_bits                   := Cat(ctrl_ut_store.io.srq_enq_op_bits,
                                              ctrl_ut_store.io.srq_enq_tag_bits,
                                              ctrl_ut_store.io.srq_enq_addr_bits,
                                              ctrl_ut_store.io.srq_enq_wmask_bits,
                                              ctrl_ut_store.io.srq_enq_data_bits);
    srq.io.enq_val                    := ctrl_ut_store.io.srq_enq_val;
    ctrl_ut_store.io.srq_enq_rdy      := srq.io.enq_rdy;

    // stores are given priority over loads
    io.dcachereq.valid  := lrq.io.deq_val || srq.io.deq_val;
    lrq.io.deq_rdy      := Mux(srq.io.deq_val, Bool(false), io.dcachereq.rdy);
    srq.io.deq_rdy      := io.dcachereq.rdy;
    io.dcachereq.data   := srq.io.deq_bits(63, 0);
    io.dcachereq.wmask  := srq.io.deq_bits(71 ,64);
    io.dcachereq.addr   := Mux(srq.io.deq_val, srq.io.deq_bits(101, 72), lrq.io.deq_bits(41, 12));
    io.dcachereq.tag    := Mux(srq.io.deq_val, srq.io.deq_bits(113, 102), lrq.io.deq_bits(11, 0));
    io.dcachereq.op     := Mux(srq.io.deq_val, srq.io.deq_bits(117, 114), Bits("b0000", 4));
  }
}

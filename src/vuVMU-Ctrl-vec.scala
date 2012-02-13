package hwacha {
  import Chisel._
  import Node._
  import Interface._
  import queues._

  class vsdq_deqIO extends Bundle
  {
    // store data queue interface
    val valid		= Bool(INPUT);
    val rdy		  = Bool(OUTPUT);
    val bits		= Bits(65, INPUT);
  }

  class vldqIO extends Bundle
  {
    // load data queue interface
    val enq_val		= Bool(OUTPUT);
    val enq_rdy		= Bool(INPUT);
    val enq_bits	= Bits(65, OUTPUT);
    val deq_rdy		= Bool(INPUT);
    val wb_done		= Bool(OUTPUT);
  }

  class vmcmdqIO extends Bundle
  {
    val bits		= Bits(VMCMD_SZ, INPUT);
    val valid		= Bool(INPUT);
    val rdy		  = Bool(OUTPUT);
  }

  class vmimmqIO extends Bundle
  {
    val bits		= Bits(VMIMM_SZ, INPUT);
    val valid		= Bool(INPUT);
    val rdy		  = Bool(OUTPUT);
  }

  class vmstrideqIO extends Bundle
  {
    val bits	= Bits(VMSTRIDE_SZ, INPUT);
    val valid	= Bool(INPUT);
    val rdy		= Bool(OUTPUT);
  }

  class vmrespqIO extends Bundle
  {
    val bits	= Bits(VMRESP_SZ, OUTPUT);
    val valid	= Bool(OUTPUT);
    val rdy		= Bool(INPUT);
  }
  
  class iscmdqIO extends Bundle
  {
    val bits		= Bits(VM_ISCMD_SZ, OUTPUT);
    val valid		= Bool(OUTPUT);
    val rdy		  = Bool(INPUT);
  }

  class wbcmdqIO extends Bundle
  {
    val bits		= Bits(VM_WBCMD_SZ, OUTPUT);
    val valid		= Bool(OUTPUT);
    val rdy		  = Bool(INPUT);
  }

  class stcmdqIO extends Bundle
  {
    val bits		= Bits(VM_STCMD_SZ, OUTPUT);
    val valid		= Bool(OUTPUT);
    val rdy		  = Bool(INPUT);
  }

  class vec_dcachereqIO extends Bundle
  {
    // D$ interface
    // request
    val addr	= Bits(28, OUTPUT);
    val tag		= Bits(12, OUTPUT);
    val data	= Bits(128, OUTPUT);
    val wmask	= Bits(16, OUTPUT);
    val op		= Bits(4, OUTPUT);
    val valid = Bool(OUTPUT);
    val rdy		= Bool(INPUT);
  }

  class vec_dcacherespIO extends Bundle
  {
    // response
    val data		= UFix(128, INPUT);
    val tag		  = UFix(12, INPUT);
    val valid		= Bool(INPUT);
  }

  class vuVMU_Ctrl_vecIO extends Bundle
  {
    val vmcmdq      = new vmcmdqIO();
    val vmimmq      = new vmimmqIO();
    val vmstrideq   = new vmstrideqIO();
    val vmrespq     = new vmrespqIO();
    val vldq        = new vldqIO();
    val vsdq_deq    = new vsdq_deqIO();
    val dcachereq   = new vec_dcachereqIO();
    val dcacheresp  = new vec_dcacherespIO();
  }

  class vuVMU_Ctrl_vec extends Component
  {
    val io = new vuVMU_Ctrl_vecIO();
    
    val ctrl_vec_top          = new vuVMU_Ctrl_vec_top();
    val iscmdq                = new queuePipePF(VM_ISCMD_SZ, 4);
    val wbcmdq                = new queuePipePF(VM_WBCMD_SZ, 4);
    val ctrl_vec_load_issue   = new vuVMU_Ctrl_vec_load_issue();
    val ctrl_vec_load_wb      = new vuVMU_Ctrl_vec_load_wb();
    val lrq                   = new queuePipePF(36, 4);
    val roq                   = new vuVMU_ROQ(130, 8, 3);
    val stcmdq                = new queuePipePF(VM_WBCMD_SZ, 4);
    val ctrl_vec_store        = new vuVMU_Ctrl_vec_store();
    val srq                   = new queuePipePF(128+28+16, 2);

    val roq_enq_val           = io.dcacheresp.valid && !(io.dcacheresp.tag(11).toBool);
    val roq_enq_data_bits     = io.dcacheresp.data;
    val roq_enq_tag_bits      = io.dcacheresp.tag(7,0);

    // ctrl_vec_top
    ctrl_vec_top.io.vmcmdq      <> io.vmcmdq;
    ctrl_vec_top.io.vmimmq      <> io.vmimmq;
    ctrl_vec_top.io.vmstrideq   <> io.vmstrideq;
    ctrl_vec_top.io.vmrespq     <> io.vmrespq;
    
    iscmdq.io.enq.bits          := ctrl_vec_top.io.iscmdq.bits;
    iscmdq.io.enq.valid           := ctrl_vec_top.io.iscmdq.valid;
    ctrl_vec_top.io.iscmdq.rdy  := iscmdq.io.enq.ready;
    
    wbcmdq.io.enq.bits          := ctrl_vec_top.io.wbcmdq.bits;
    wbcmdq.io.enq.valid           := ctrl_vec_top.io.wbcmdq.valid;
    ctrl_vec_top.io.wbcmdq.rdy  := wbcmdq.io.enq.ready;

    stcmdq.io.enq.bits          := ctrl_vec_top.io.stcmdq.bits;
    stcmdq.io.enq.valid           := ctrl_vec_top.io.stcmdq.valid;
    ctrl_vec_top.io.stcmdq.rdy  := stcmdq.io.enq.ready;
   
    ctrl_vec_top.io.store_busy  := ctrl_vec_store.io.store_busy;

    // ctrl_vec_load_issue
    ctrl_vec_load_issue.io.iscmdq_deq_bits  := iscmdq.io.deq.bits.toUFix;
    ctrl_vec_load_issue.io.iscmdq_deq_val   := iscmdq.io.deq.valid;
    iscmdq.io.deq.ready                       := ctrl_vec_load_issue.io.iscmdq_deq_rdy;

    lrq.io.enq.bits                         := ctrl_vec_load_issue.io.lrq_enq_bits;
    lrq.io.enq.valid                          := ctrl_vec_load_issue.io.lrq_enq_val;
    ctrl_vec_load_issue.io.lrq_enq_rdy      := lrq.io.enq.ready;

    ctrl_vec_load_issue.io.roq_deq_tag_bits := roq.io.roq_deq_tag_bits.toUFix;
    ctrl_vec_load_issue.io.roq_deq_tag_val  := roq.io.roq_deq_tag_val;
    roq.io.roq_deq_tag_rdy                  := ctrl_vec_load_issue.io.roq_deq_tag_rdy;

    // ctrl_vec_load_wb 
    ctrl_vec_load_wb.io.wbcmdq_deq_bits := wbcmdq.io.deq.bits;
    ctrl_vec_load_wb.io.wbcmdq_deq_val  := wbcmdq.io.deq.valid;
    wbcmdq.io.deq.ready                   := ctrl_vec_load_wb.io.wbcmdq_deq_rdy;

    ctrl_vec_load_wb.io.roq_deq_bits    := roq.io.roq_deq_data_bits(127,0);
    ctrl_vec_load_wb.io.roq_deq_val     := roq.io.roq_deq_data_val;
    roq.io.roq_deq_data_rdy             := ctrl_vec_load_wb.io.roq_deq_rdy;

    ctrl_vec_load_wb.io.ldq <> io.vldq;
   
    // roq
    roq.io.roq_enq_data_bits := Cat(Bits("b00", 2), roq_enq_data_bits);
    roq.io.roq_enq_tag_bits  := roq_enq_tag_bits;
    roq.io.roq_enq_val       := roq_enq_val.toBool;

    // ctrl_vec_store
    ctrl_vec_store.io.stcmdq_deq_bits   := stcmdq.io.deq.bits;
    ctrl_vec_store.io.stcmdq_deq_val    := stcmdq.io.deq.valid;
    stcmdq.io.deq.ready                   := ctrl_vec_store.io.stcmdq_deq_rdy;

    ctrl_vec_store.io.sdq_deq           <> io.vsdq_deq;

    srq.io.enq.bits                     := Cat(ctrl_vec_store.io.srq_enq_addr_bits,
                                               ctrl_vec_store.io.srq_enq_wmask_bits,
                                               ctrl_vec_store.io.srq_enq_data_bits);
    srq.io.enq.valid                      := ctrl_vec_store.io.srq_enq_val;
    ctrl_vec_store.io.srq_enq_rdy       := srq.io.enq.ready;

    // stores are given priority over loads
    io.dcachereq.valid  := lrq.io.deq.valid || srq.io.deq.valid;
    lrq.io.deq.ready      := Mux(srq.io.deq.valid, Bool(false), io.dcachereq.rdy);
    srq.io.deq.ready      := io.dcachereq.rdy;
    io.dcachereq.data   := srq.io.deq.bits(127, 0);
    io.dcachereq.wmask  := srq.io.deq.bits(143, 128);
    io.dcachereq.addr   := Mux(srq.io.deq.valid, srq.io.deq.bits(171, 144), lrq.io.deq.bits(35,8));
    io.dcachereq.tag    := Mux(srq.io.deq.valid, Bits("h800", 12), Cat(Bits("b0", 4), lrq.io.deq.bits(7,0)));
    io.dcachereq.op     := Mux(srq.io.deq.valid, Bits("b0001", 4), Bits("b0000", 4));
  }
}

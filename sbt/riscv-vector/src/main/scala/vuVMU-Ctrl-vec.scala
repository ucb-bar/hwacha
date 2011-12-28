package riscvVector {
  import Chisel._
  import Node._
  import Interface._
  import queues._

  class vsdq_deqIO extends Bundle
  {
    // store data queue interface
    val valid		= Bool('input);
    val rdy		  = Bool('output);
    val bits		= Bits(65, 'input);
  }

  class vldqIO extends Bundle
  {
    // load data queue interface
    val enq_val		= Bool('output);
    val enq_rdy		= Bool('input);
    val enq_bits	= Bits(65, 'output);
    val deq_rdy		= Bool('input);
    val wb_done		= Bool('output);
  }

  class vmcmdqIO extends Bundle
  {
    val bits		= Bits(VMCMD_SZ, 'input);
    val valid		= Bool('input);
    val rdy		  = Bool('output);
  }

  class vmimmqIO extends Bundle
  {
    val bits		= Bits(VMIMM_SZ, 'input);
    val valid		= Bool('input);
    val rdy		  = Bool('output);
  }

  class vmstrideqIO extends Bundle
  {
    val bits	= Bits(VMSTRIDE_SZ, 'input);
    val valid	= Bool('input);
    val rdy		= Bool('output);
  }

  class vmrespqIO extends Bundle
  {
    val bits	= Bits(VMRESP_SZ, 'output);
    val valid	= Bool('output);
    val rdy		= Bool('input);
  }
  
  class iscmdqIO extends Bundle
  {
    val bits		= Bits(VM_ISCMD_SZ, 'output);
    val valid		= Bool('output);
    val rdy		  = Bool('input);
  }

  class wbcmdqIO extends Bundle
  {
    val bits		= Bits(VM_WBCMD_SZ, 'output);
    val valid		= Bool('output);
    val rdy		  = Bool('input);
  }

  class stcmdqIO extends Bundle
  {
    val bits		= Bits(VM_STCMD_SZ, 'output);
    val valid		= Bool('output);
    val rdy		  = Bool('input);
  }

  class vuVMU_Ctrl_vecIO extends Bundle
  {
    val vmcmdq      = new vmcmdqIO();
    val vmimmq      = new vmimmqIO();
    val vmstrideq   = new vmstrideqIO();
    val vmrespq     = new vmrespqIO();
    val vldq        = new vldqIO();
    val vsdq_deq    = new vsdq_deqIO();

    // D$ interface
    // request
    val dcachereq_addr	= UFix(28, 'output);
    val dcachereq_tag		= UFix(12, 'output);
    val dcachereq_data	= UFix(128, 'output);
    val dcachereq_wmask	= UFix(16, 'output);
    val dcachereq_op		= UFix(4, 'output);
    val dcachereq_val		= Bool('output);
    val dcachereq_rdy		= Bool('input);

    // response
    val dcacheresp_data		= UFix(128, 'input);
    val dcacheresp_tag		= UFix(12, 'input);
    val dcacheresp_val		= Bool('input);
  }

  class vuVMU_Ctrl_vec extends Bundle
  {
    val io = new vuVMU_Ctrl_vecIO();
    
    val ctrl_vec_top          = new vuVMU_Ctrl_vec_top;
    val iscmdq                = new queuePipePF(VM_ISCMD_SZ, 4, 1);
    val wbcmdq                = new queuePipePF(VM_WBCMD_SZ, 4, 1);
    val ctrl_vec_load_issue   = new vuVMU_Ctrl_vec_load_issue();
    val ctrl_vec_load_wb      = new vuVMU_Ctrl_vec_load_wb();
    val lrq                   = new queuePipePF(36, 4, 1);
    val roq                   = new vuVMU_ROQ(130, 256, 8);
    val stcmdq                = new queuePipePF(VM_WBCMD_SZ, 4, 1);
    val ctrl_vec_store        = new vuVMU_Ctrl_vec_store();
    val srq                   = new queuePipePF(128+28+16, 2, 1);

    val roq_enq_val           = io.dcacheresp_val & ~io.dcacheresp_tag(11);
    val roq_enq_data_bits     = io.dcacheresp_data;
    val roq_enq_tag_bits      = io.dcacheresp_tag(7, 0);

    // ctrl_vec_top
    ctrl_vec_top.io.vmcmdq      ^^ io.vmcmdq;
    ctrl_vec_top.io.vmimmq      ^^ io.vmimmq;
    ctrl_vec_top.io.vmstrideq   ^^ io.vmstrideq;
    ctrl_vec_top.io.vmrespq     ^^ io.vmrespq;
    
    iscmdq.io.enq_bits          := ctrl_vec_top.io.iscmdq.bits;
    iscmdq.io.enq_val           := ctrl_vec_top.io.iscmdq.valid;
    ctrl_vec_top.io.iscmdq.rdy  := iscmdq.io.enq_rdy;
    
    wbcmdq.io.enq_bits          := ctrl_vec_top.io.wbcmdq.bits;
    wbcmdq.io.enq_val           := ctrl_vec_top.io.wbcmdq.valid;
    ctrl_vec_top.io.wbcmdq.rdy  := wbcmdq.io.enq_rdy;

    stcmdq.io.enq_bits          := ctrl_vec_top.io.stcmdq.bits;
    stcmdq.io.enq_val           := ctrl_vec_top.io.stcmdq.valid;
    ctrl_vec_top.io.stcmdq.rdy  := stcmdq.io.enq_rdy;
   
    ctrl_vec_top.io.store_busy  := ctrl_vec_store.io.store_busy;

    // ctrl_vec_load_issue
    ctrl_vec_load_issue.io.iscmdq_deq_bits  := iscmdq.io.deq_bits;
    ctrl_vec_load_issue.io.iscmdq_deq_val   := iscmdq.io.deq_val;
    iscmdq.io.deq_rdy                       := ctrl_vec_load_issue.io.iscmdq_deq_rdy;

    lrq.io.enq_bits                         := ctrl_vec_load_issue.io.lrq_enq_bits;
    lrq.io.enq_val                          := ctrl_vec_load_issue.io.lrq_enq_val;
    ctrl_vec_load_issue.io.lrq_enq_rdy      := lrq.io.enq_rdy;

    ctrl_vec_load_issue.io.roq_deq_tag_bits := roq.io.roq_deq_tag_bits;
    ctrl_vec_load_issue.io.roq_deq_tag_val  := roq.io.roq_deq_tag_val;
    roq.io.roq_deq_tag_rdy                  := ctrl_vec_load_issue.io.roq_deq_tag_rdy;

    // ctrl_vec_load_wb 
    ctrl_vec_load_wb.io.wbcmdq_deq_bits := wbcmdq.io.deq_bits;
    ctrl_vec_load_wb.io.wbcmdq_deq_val  := wbcmdq.io.deq_val;
    wbcmdq.io.deq_rdy                   := ctrl_vec_load_wb.io.wbcmdq_deq_rdy;

    ctrl_vec_load_wb.io.roq_deq_bits    := roq.io.roq_deq_data_bits;
    ctrl_vec_load_wb.io.roq_deq_val     := roq.io.roq_deq_data_val;
    roq.io.roq_deq_data_rdy             := ctrl_vec_load_wb.io.roq_deq_rdy;

    ctrl_vec_load_wb.io.ldq ^^ io.vldq;
   
    // roq
    roq.io.roq_enq_data_bits := Cat(Bits("b00", 2), roq_enq_data_bits);
    roq.io.roq_enq_tag_bits  := roq_enq_tag_bits;
    roq.io.roq_enq_val       := roq_enq_val;

    // ctrl_vec_store
    ctrl_vec_store.io.stcmdq_deq_bits   := stcmdq.io.deq_bits;
    ctrl_vec_store.io.stcmdq_deq_val    := stcmdq.io.deq_val;
    stcmdq.io.deq_rdy                   := ctrl_vec_store.io.stcmdq_deq_rdy;

    ctrl_vec_store.io.sdq_deq           ^^ io.vsdq_deq;

    srq.io.enq_bits                     := Cat(ctrl_vec_store.io.srq_enq_addr_bits,
                                               ctrl_vec_store.io.srq_enq_wmask_bits,
                                               ctrl_vec_store.io.srq_enq_data_bits);
    srq.io.enq_val                      := ctrl_vec_store.io.srq_enq_val;
    ctrl_vec_store.io.srq_enq_rdy       := srq.io.enq_rdy;

    // stores are given priority over loads
    io.dcachereq_val    := lrq.io.deq_val || srq.io.deq_val;
    lrq.io.deq_rdy      := Mux(srq.io.deq_val, Bool(false), io.dcachereq_rdy);
    srq.io.deq_rdy      := io.dcachereq_rdy;
    io.dcachereq_data   := srq.io.deq_bits(127, 0);
    io.dcachereq_wmask  := srq.io.deq_bits(143, 128);
    io.dcachereq_addr   := Mux(srq.io.deq_val, srq.io.deq_bits(171, 144), lrq.io.deq_bits(35,8));
    io.dcachereq_tag    := Mux(srq.io.deq_val, Bits("h800", 12), Cat(Bits("b0", 4), lrq.io.deq_bits(7,0)));
    io.dcachereq_op     := Mux(srq.io.deq_val, Bits("b0001", 4), Bits("b0000", 4));
  }
}

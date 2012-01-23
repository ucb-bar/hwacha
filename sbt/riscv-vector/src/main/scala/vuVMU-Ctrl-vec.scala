package riscvVector {
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

  class vuVMU_Ctrl_vecIO extends Bundle
  {
    val vmcmdq      = new vmcmdqIO();
    val vmimmq      = new vmimmqIO();
    val vmstrideq   = new vmstrideqIO();
    val vmrespq     = new vmrespqIO();
    val vldq        = new vldqIO();
    val vsdq_deq    = new vsdq_deqIO();
    // vector load and store request queues, go to CP
    val vlrq        = new vlrqIO().flip;
    val vsrq        = new vsrqIO().flip;
    val vmldq       = new vmldqIO().flip;
  }

  class vuVMU_Ctrl_vec extends Component
  {
    val io = new vuVMU_Ctrl_vecIO();

    val ctrl_vec_top          = new vuVMU_Ctrl_vec_top();
    val iscmdq                = new queuePipePF(VM_ISCMD_SZ, 4, 2);
    val wbcmdq                = new queuePipePF(VM_WBCMD_SZ, 4, 2);
    val ctrl_vec_load_issue   = new vuVMU_Ctrl_vec_load_issue();
    val ctrl_vec_load_wb      = new vuVMU_Ctrl_vec_load_wb();
    val lrq                   = new queuePipePF(36, 4, 2);
    val stcmdq                = new queuePipePF(VM_WBCMD_SZ, 4, 2);
    val ctrl_vec_store        = new vuVMU_Ctrl_vec_store();
    val srq                   = new queuePipePF(128+28+16, 2, 1);
    val ctrl_vec_seq          = new vuVMU_Ctrl_vec_sequencer(8, 8);

    // ctrl_vec_top
    ctrl_vec_top.io.vmcmdq      <> io.vmcmdq;
    ctrl_vec_top.io.vmimmq      <> io.vmimmq;
    ctrl_vec_top.io.vmstrideq   <> io.vmstrideq;
    ctrl_vec_top.io.vmrespq     <> io.vmrespq;
    
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
    ctrl_vec_load_issue.io.vlrq <> io.vlrq;

    ctrl_vec_load_issue.io.iscmdq_deq_bits  := iscmdq.io.deq_bits.toUFix;
    ctrl_vec_load_issue.io.iscmdq_deq_val   := iscmdq.io.deq_val;
    iscmdq.io.deq_rdy                       := ctrl_vec_load_issue.io.iscmdq_deq_rdy;

    // ctrl_vec_load_wb
    ctrl_vec_load_wb.io.vmldq             <> io.vmldq;
    ctrl_vec_load_wb.io.vldq              <> io.vldq;

    ctrl_vec_load_wb.io.wbcmdq_deq_bits   := wbcmdq.io.deq_bits;
    ctrl_vec_load_wb.io.wbcmdq_deq_val    := wbcmdq.io.deq_val;
    wbcmdq.io.deq_rdy                     := ctrl_vec_load_wb.io.wbcmdq_deq_rdy;
   
    // ctrl_vec_store
    ctrl_vec_store.io.stcmdq_deq_bits   := stcmdq.io.deq_bits;
    ctrl_vec_store.io.stcmdq_deq_val    := stcmdq.io.deq_val;
    stcmdq.io.deq_rdy                   := ctrl_vec_store.io.stcmdq_deq_rdy;

    ctrl_vec_store.io.sdq_deq           <> io.vsdq_deq;
    ctrl_vec_store.io.vsrq              <> io.vsrq;

    // ctrl_vec_sequencer
    ctrl_vec_seq.io.vlrq_val    := ctrl_vec_load_issue.io.vlrq.valid;
    ctrl_vec_seq.io.vlrq_rdy    := io.vlrq.rdy;
    ctrl_vec_seq.io.vmldq_val   := io.vmldq.valid;
    ctrl_vec_seq.io.vmldq_rdy   := ctrl_vec_load_wb.io.vmldq.rdy;
    ctrl_vec_seq.io.vsrq_val    := ctrl_vec_store.io.vsrq.valid;
    ctrl_vec_seq.io.vsrq_rdy    := io.vsrq.rdy;
    ctrl_vec_seq.io.vsackq_val  := Bool(true); // FIXME!
    ctrl_vec_seq.io.vsackq_rdy  := Bool(true); // FIXME!
  }
}

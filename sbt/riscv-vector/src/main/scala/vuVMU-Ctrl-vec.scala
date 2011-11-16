package riscvVector {
  import Chisel._
  import Node._
  import Interface._

  class vuVMU_Ctrl_vecIO extends Bundle
  {
    val vmcmdq_bits		= UFix(VMCMD_SZ, 'input);
    val vmcmdq_val		= Bool('input);
    val vmcmdq_rdy		= Bool('output);

    val vmimmq_bits		= UFix(VMIMM_SZ, 'input);
    val vmimmq_val		= Bool('input);
    val vmimmq_rdy		= Bool('output);

    val vmstrideq_bits		= UFix(VMSTRIDE_SZ, 'input);
    val vmstrideq_val		= Bool('input);
    val vmstrideq_rdy		= Bool('output);

    val vmrespq_bits		= UFix(VMRESP_SZ, 'output);
    val vmrespq_val		= Bool('output);
    val vmrespq_rdy		= Bool('input);

    // load data queue interface
    val vldq_enq_val		= Bool('output);
    val vldq_enq_rdy		= Bool('input);
    val vldq_enq_bits		= UFix(65, 'output);
    val vldq_deq_rdy		= Bool('input);
    val vldq_wb_done		= Bool('output);

    // store data queue interface
    val vsdq_deq_val		= Bool('input);
    val vsdq_deq_rdy		= Bool('output);
    val vsdq_deq_bits		= UFix(65, 'input);

    // D$ interface
    // request
    val dcachereq_addr		= UFix(28, 'output);
    val dcachereq_tag		= UFix(12, 'output);
    val dcachereq_data		= UFix(128, 'output);
    val dcachereq_wmask		= UFix(16, 'output);
    val dcachereq_op		= UFix(4, 'output);
    val dcachereq_val		= Bool('output);
    val dcachereq_rdy		= Bool('input);

    // response
    val dcacheresp_data		= UFix(128, 'input);
    val dcacheresp_tag		= UFix(12, 'input);
    val dcacheresp_val		= Bool('input);
    // Store control
    // 128 bits data + 28 bits address + 16 bits write mask
    // stores are given priority over loads
  }

  class vuVMU_Ctrl_vec extends Bundle
  {
    val io = new vuVMU_Ctrl_vecIO();
    val ctrl_vec_top = new vuVMU_Ctrl_vec_top;
    ctrl_vec_top.io.vmcmdq_bits := io.vmcmdq_bits;
    ctrl_vec_top.io.vmcmdq_val := io.vmcmdq_rdy;
    ctrl_vec_top.io.vmcmdq_rdy := io.vmcmdq_rdy;
  }
}

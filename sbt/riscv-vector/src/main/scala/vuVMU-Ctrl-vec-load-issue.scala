package riscvVector {
  import Chisel._
  import Node._

  class vuVMU_Ctrl_vec_load_issueIO extends Bundle {
    val iscmdq_deq_bits		= UFix(VM_ISCMD_SZ, 'input);
    val iscmdq_deq_val		= Bool('input);
    val iscmdq_deq_rdy		= Bool('output);
    
    // interface to load request queue
    // 28 bits for address, 8 bits for tag
    val lrq_enq_bits		= UFix(36, 'output);
    val lrq_enq_rdy		= Bool('input);
    val lrq_enq_val		= Bool('output);
    
    // interface to reorder queue (256 entries)
    val roq_deq_tag_bits		= UFix(8, 'input);
    val roq_deq_tag_val		= Bool('input);
    val roq_deq_tag_rdy		= Bool('output);
  }

  class vuVMU_Ctrl_vec_load_issue extends Component {
    val io = new vuVMU_Ctrl_vec_load_issueIO();

    val VMU_Ctrl_Idle = UFix(0, 2);
    val VMU_Ctrl_IssueShort = UFix(1, 2);
    val VMU_Ctrl_IssueLong = UFix(2, 2);

    val state = Reg(resetVal = VMU_Ctrl_Idle);
    val addr_reg = Reg(resetVal = UFix(0, 32));
    val last_addr_reg = Reg(resetVal = UFix(0, 32));
    val stride_reg = Reg(resetVal = UFix(0, 32));
    
    val vlen = io.iscmdq_deq_bits(interface.VMCMD_VLEN_SZ-1, 0) 
    val addr = io.iscmdq_deq_bits(interface.VMIMM_SZ+interface.VMCMD_VLEN_SZ-1, interface.VMCMD_VLEN_SZ);
    val stride = io.iscmdq_deq_bits(interface.VMSTRIDE_SZ+interface.VMIMM_SZ+interface.VMCMD_VLEN_SZ-1, interface.VMIMM_SZ+interface.VMCMD_VLEN_SZ);
    val addr_cacheline = Cat(addr_reg(31,4), UFix(0,4));

    io.lrq_enq_bits := Cat(addr_reg(31,4), io.roq_deq_tag_bits);

    val lrq_enq_val       = Wire() {Bool()};
    val iscmdq_deq_rdy    = Wire() {Bool()};
    val roq_deq_tag_rdy   = Wire() {Bool()};

    io.lrq_enq_val      := lrq_enq_val;
    io.iscmdq_deq_rdy   := iscmdq_deq_rdy;
    io.roq_deq_tag_rdy  := io.roq_deq_tag_rdy;

    switch(state) {
      is(VMU_Ctrl_Idle) {
        iscmdq_deq_rdy <== Bool(true);
        when( io.iscmdq_deq_rdy ) {
          last_addr_reg <== addr + (vlen * stride);
          when( stride < UFix(16, interface.VMSTRIDE_SZ) ) {
            state <== VMU_Ctrl_IssueShort;
          }
          otherwise {
            state <== VMU_Ctrl_IssueLong;
          }
        }
      }
      is(VMU_Ctrl_IssueShort) {
        when( addr_cacheline > last_addr_reg ) {
          state <== VMU_Ctrl_Idle;
        }
        otherwise {
          roq_deq_tag_rdy <== io.lrq_enq_rdy;
          lrq_enq_val     <== io.roq_deq_tag_val;
          when( io.roq_deq_tag_val & io.lrq_enq_rdy ) {
            addr_reg <== addr_reg + UFix(16, 32);
          }
        }
      }
      is(VMU_Ctrl_IssueLong) {
        when( addr_cacheline > last_addr_reg ) {
          state <== VMU_Ctrl_Idle;
        }
        otherwise {
          roq_deq_tag_rdy <== io.lrq_enq_rdy;
          lrq_enq_val     <== io.roq_deq_tag_val;
          when( io.roq_deq_tag_val & io.lrq_enq_rdy ) {
            addr_reg <== addr_reg + stride_reg;
          }
        }
      }
    }
  }

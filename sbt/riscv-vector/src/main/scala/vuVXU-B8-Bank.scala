package riscvVector {
import Chisel._
import Node._

class BankToBankIO extends Bundle {
  val ren    = Bool('input)
  val rlast  = Bool('input)
  val rct    = Bits(DEF_BVLEN, 'input);
  val raddr  = Bits(DEF_BREGLEN, 'input);
  val roplen = Bits(DEF_BOPL, 'input);
  val rblen  = Bits(DEF_BRPORT, 'input);

  val wen   = Bool('input)
  val wlast = Bool('input)
  val wct   = Bits(DEF_BVLEN, 'input);
  val waddr = Bits(DEF_BREGLEN, 'input);
  val wsel  = Bits(DEF_BWPORT, 'input);

  val viu_val   = Bool('input)
  val viu_fn    = Bits(DEF_VIU_FN, 'input);
  val viu_utidx = Bits(DEF_VLEN, 'input);
  val viu_imm   = Bits(DEF_DATA, 'input);
}

class BankRWIO extends Bundle {
  val rblen = Bits(DEF_BRPORT, 'output);
  val rdata = Bits(DEF_DATA, 'ouput);
  val ropl0 = Bits(DEF_DATA, 'output);
  val ropl1 = Bits(DEF_DATA, 'output);

  val wbl0 = Bits(DEF_DATA, 'input);
  val wbl1 = Bits(DEF_DATA, 'input);
  val wbl2 = Bits(DEF_DATA, 'input);
  val wbl3 = Bits(DEF_DATA, 'input);
}

class vuVXU_Banked8_BankIO extends Bundle {
  val active = Bool('input)
  
  val in  = new BankToBankIO();
  val out = new BankToBankIO().flip();
  
  val rw  = new BankRWIO(); 
}

class vuVXU_Banked8_Bank extends Component {
  val io = vuVXU_Banked8_BankIO();

  val rpass = io.in.rcnt.orR();
  val wpass = io.in.wcnt.orR();

  val reg_rn     = Reg(rpass & io.in.ren);
  val reg_rlast  = Reg(io.in.rlast);
  val reg_rcnt   = Reg(Mux(rpass, io.in.rcnt - UFix(1), UFix(0)));
  val reg_raddr  = Reg(io.in.raddr);
  val reg_roplen = Reg(io.in.roplen);
  val reg_rblen  = Reg(io.in.rblen); 

  val reg_wen   = Reg(wpass & io.in.wen);
  val reg_wlast = Reg(io.in.wlast);
  val reg_wcnt  = Reg(Mux(wpass, io.in.wcnt - UFix(1), UFix(0)));
  val reg_waddr = Reg(io.in.waddr);
  val reg_wsel  = Reg(io.in.wsel);

  val reg_viu_val   = Reg(rpass & io.in.viu_val);
  val reg_viu_fn    = Reg(io.in.viu_fn);
  val reg_viu_utidx = Reg(io.in.viu_utidx + UFix(1));
  val reg_viu_imm   = Reg(io.in.viu_imm);

  // every signal related to the read port is delayed by one cycle 
  // because of the register file is an sram

  val delay_roplen = reg_roplen & Fill(io.active, SZ_BOPL);
  val delay_rblen  = reg_rblen & Fill(io.active, SZ_BRPORT);

  val delay_viu_fn = reg_viu_fn;
  
  val delay_viu_val   = Reg(io.in.viu_val & io.active);
  val delay_viu_utidx = Reg(io.in.viu_utidx); 

  io.rw.rblen := delay_rblen;

  val rfile = new vuVXU_Banked8_Bank_Regfile();
  val alu = new vuVXU_Banked8_FU_alu();

  rfile.io.ren    := io.in.ren & io.active;
  rfile.io.raddr  := io.in.raddr;
  rfile.io.roplen := delay_roplen;
  
  rfile.io.wen    := alu.io.wen_masked & io.active;
  rfile.io.waddr  := io.in.waddr;
  rfile.io.wsel   := io.in.wsel;

  io.rw.rdata     := rfile.io.rdata;
  io.rw.ropl0     := rfile.io.ropl0;
  io.rw.ropl1     := rfile.io.ropl1;

  rfile.io.wbl0   := io.rw.wbl0;
  rfile.io.wbl1   := io.rw.wbl1;
  rfile.io.wbl2   := io.rw.wbl2;
  rfile.io.wbl3   := io.rw.wbl3;

  val viu_rdata       = rfile.io.viu_rdata;
  val viu_ropl        = rfile.io.viu_ropl;
  rfile.io.viu_wdata := alu.io.out;

  val viu_in0 = MuxLookup(delay_viu_fn(RG_VIU_T0), UFix(0, SZ_DATA), Array(
    M0 -> UFix(0, SZ_DATA),
    ML -> viu_ropl,
    MR -> viu_rdata));

  val viu_in1 = MuxLookup(delay_viu_fn(RG_VIU_T1), UFix(0, SZ_DATA), Array(
    M0 -> UFix(0, SZ_DATA),
    MR -> viu_rdata,
    MI -> delay_viu_imm));

  alu.io.valid      := delay_viu_val;
  alu.io.wen        := io.in.wen;
  alu.io.fn         := delay_viu_fn;
  alu.io.utidx      := delay_viu_utidx;
  alu.io.in0        := viu_in0;
  alu.io.in1        := viu_in1;

  io.out.ren    := Mux(active, reg_ren, io.in.ren);
  io.out.rlast  := Mux(active, reg_rlast, io.in.rlast);
  io.out.rcnt   := Mux(active, reg_rcnt, UFix(0))) , io.in.rcnt);
  io.out.raddr  := Mux(active, reg_raddr, io.in.raddr);
  io.out.roplen := Mux(active, reg_roplen, io.in.roplen);
  io.out.rblen  := Mux(active, reg_rblen, io.in.rblen);

  io.out.wen   := Mux(active, reg_wen, io.in.wen);
  io.out.wlast := Mux(active, reg_wlast, io.in.wlast);
  io.out.wcnt  := Mux(active, reg_wcnt, io.in.wcnt);
  io.out.waddr := Mux(active, reg_waddr, io.in.waddr);
  io.out.wsel  := Mux(active, reg_wsel, io.in.wsel);

  io.out.viu_val   := Mux(active, reg_viu_val, io.in.viu_val);
  io.out.viu_fn    := Mux(active, reg_viu_fn, io.in.viu_fn);
  io.out.viu_utidx := Mux(active, reg_viu_utidx, io.in.viu_utidx);
  io.out.viu_imm   := Mux(active, reg_viu_imm, io.in.viu_imm);

}
}

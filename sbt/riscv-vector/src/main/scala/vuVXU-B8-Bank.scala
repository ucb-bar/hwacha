package riscvVector
{

import Chisel._
import Node._
import Config._

class BankToBankIO extends Bundle
{
  val ren    = Bool(INPUT)
  val rlast  = Bool(INPUT)
  val rcnt   = Bits(DEF_BVLEN, INPUT);
  val raddr  = Bits(DEF_BREGLEN, INPUT);
  val roplen = Bits(DEF_BOPL, INPUT);
  val rblen  = Bits(DEF_BRPORT, INPUT);

  val wen   = Bool(INPUT)
  val wlast = Bool(INPUT)
  val wcnt  = Bits(DEF_BVLEN, INPUT);
  val waddr = Bits(DEF_BREGLEN, INPUT);
  val wsel  = Bits(DEF_BWPORT, INPUT);

  val viu_val   = Bool(INPUT)
  val viu_fn    = Bits(DEF_VIU_FN, INPUT);
  val viu_utidx = Bits(DEF_VLEN, INPUT);
  val viu_imm   = Bits(DEF_DATA, INPUT);
}

class BankRWIO extends Bundle
{
  val rblen = Bits(DEF_BRPORT, OUTPUT);
  val rdata = Bits(DEF_DATA, OUTPUT);
  val ropl0 = Bits(DEF_DATA, OUTPUT);
  val ropl1 = Bits(DEF_DATA, OUTPUT);

  val wbl0 = Bits(DEF_DATA, INPUT);
  val wbl1 = Bits(DEF_DATA, INPUT);
  val wbl2 = Bits(DEF_DATA, INPUT);
  val wbl3 = Bits(DEF_DATA, INPUT);
}

class vuVXU_Banked8_BankIO extends Bundle
{
  val active = Bool(INPUT)
  
  val in  = new BankToBankIO();
  val out = new BankToBankIO().flip();
  
  val rw  = new BankRWIO(); 
}

class vuVXU_Banked8_Bank extends Component
{
  val io = new vuVXU_Banked8_BankIO();

  val rpass = io.in.rcnt.orR();
  val wpass = io.in.wcnt.orR();

  val reg_ren    = Reg(rpass & io.in.ren);
  val reg_rlast  = Reg(io.in.rlast);
  val reg_rcnt   = Reg(Mux(rpass, io.in.rcnt.toUFix - UFix(1), UFix(0)));
  val reg_raddr  = Reg(io.in.raddr);
  val reg_roplen = Reg(io.in.roplen);
  val reg_rblen  = Reg(io.in.rblen); 

  val reg_wen   = Reg(wpass & io.in.wen);
  val reg_wlast = Reg(io.in.wlast);
  val reg_wcnt  = Reg(Mux(wpass, io.in.wcnt.toUFix - UFix(1), UFix(0)));
  val reg_waddr = Reg(io.in.waddr);
  val reg_wsel  = Reg(io.in.wsel);

  val reg_viu_val   = Reg(rpass & io.in.viu_val);
  val reg_viu_fn    = Reg(io.in.viu_fn);
  val reg_viu_utidx = Reg(io.in.viu_utidx.toUFix + UFix(1));
  val reg_viu_imm   = Reg(io.in.viu_imm);

  // every signal related to the read port is delayed by one cycle 
  // because of the register file is an sram

  val delay_roplen = reg_roplen & Fill(SZ_BOPL, io.active);
  val delay_rblen  = reg_rblen & Fill(SZ_BRPORT, io.active);

  val delay_viu_fn  = reg_viu_fn;
  val delay_viu_imm = reg_viu_imm;
  
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

  val viu_in0 = MuxLookup(
    delay_viu_fn(RG_VIU_T0), UFix(0, SZ_DATA), Array(
      M0 -> UFix(0, SZ_DATA),
      ML -> viu_ropl,
      MR -> viu_rdata
    ));

  val viu_in1 = MuxLookup(
    delay_viu_fn(RG_VIU_T1), UFix(0, SZ_DATA), Array(
      M0 -> UFix(0, SZ_DATA),
      MR -> viu_rdata,
      MI -> delay_viu_imm
    ));

  alu.io.valid := delay_viu_val;
  alu.io.wen   := io.in.wen;
  alu.io.fn    := delay_viu_fn;
  alu.io.utidx := delay_viu_utidx;
  alu.io.in0   := viu_in0;
  alu.io.in1   := viu_in1;

  io.out.ren    := Mux(io.active, reg_ren, io.in.ren);
  io.out.rlast  := Mux(io.active, reg_rlast, io.in.rlast);
  io.out.rcnt   := Mux(io.active, reg_rcnt, io.in.rcnt);
  io.out.raddr  := Mux(io.active, reg_raddr, io.in.raddr);
  io.out.roplen := Mux(io.active, reg_roplen, io.in.roplen);
  io.out.rblen  := Mux(io.active, reg_rblen, io.in.rblen);

  io.out.wen   := Mux(io.active, reg_wen, io.in.wen);
  io.out.wlast := Mux(io.active, reg_wlast, io.in.wlast);
  io.out.wcnt  := Mux(io.active, reg_wcnt, io.in.wcnt);
  io.out.waddr := Mux(io.active, reg_waddr, io.in.waddr);
  io.out.wsel  := Mux(io.active, reg_wsel, io.in.wsel);

  io.out.viu_val   := Mux(io.active, reg_viu_val, io.in.viu_val);
  io.out.viu_fn    := Mux(io.active, reg_viu_fn, io.in.viu_fn);
  io.out.viu_utidx := Mux(io.active, reg_viu_utidx, io.in.viu_utidx);
  io.out.viu_imm   := Mux(io.active, reg_viu_imm, io.in.viu_imm);
}

}

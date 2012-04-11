package hwacha

import Chisel._
import Node._
import Constants._

class BankToBankIO extends Bundle
{
  val ren    = Bool(INPUT)
  val rlast  = Bool(INPUT)
  val rcnt   = Bits(SZ_BVLEN, INPUT)
  val raddr  = Bits(SZ_BREGLEN, INPUT)
  val roplen = Bits(SZ_BOPL, INPUT)
  val rblen  = Bits(SZ_BRPORT, INPUT)
  val rmask  = Bits(SZ_BANK, INPUT)

  val wen        = Bool(INPUT)
  val wlast      = Bool(INPUT)
  val wcnt       = Bits(SZ_BVLEN, INPUT)
  val waddr      = Bits(SZ_BREGLEN, INPUT)
  val wsel       = Bits(SZ_BWPORT, INPUT)
  val wmask      = Bits(SZ_BANK, INPUT)
  val wen_mask   = Bool(INPUT)
  val wlast_mask = Bool(INPUT)
  val waddr_mask = Bits(SZ_BMASK, INPUT)

  val viu       = Bool(INPUT)
  val viu_fn    = Bits(SZ_VIU_FN, INPUT)
  val viu_utidx = Bits(SZ_VLEN, INPUT)
  val viu_imm   = Bits(SZ_DATA, INPUT)
}

class BankRWIO extends Bundle
{
  val rblen = Bits(SZ_BRPORT, OUTPUT)
  val rdata = Bits(SZ_DATA, OUTPUT)
  val ropl0 = Bits(SZ_DATA, OUTPUT)
  val ropl1 = Bits(SZ_DATA, OUTPUT)

  val wbl0 = Bits(SZ_DATA, INPUT)
  val wbl1 = Bits(SZ_DATA, INPUT)
  val wbl2 = Bits(SZ_DATA, INPUT)
  val wbl3 = Bits(SZ_DATA, INPUT)
}

class vuVXU_Banked8_BankIO extends Bundle
{
  val active = Bool(INPUT)
  
  val in  = new BankToBankIO()
  val out = new BankToBankIO().flip
  
  val rw  = new BankRWIO()

  val branch_resolution_mask = Bits(WIDTH_BMASK, OUTPUT)
}

class vuVXU_Banked8_Bank extends Component
{
  val io = new vuVXU_Banked8_BankIO()

  val rpass = io.in.rcnt.orR
  val wpass = io.in.wcnt.orR

  val reg_ren    = Reg(rpass & io.in.ren)
  val reg_rlast  = Reg(io.in.rlast)
  val reg_rcnt   = Reg(Mux(rpass, io.in.rcnt.toUFix - io.in.rmask(0), UFix(0)))
  val reg_raddr  = Reg(io.in.raddr)
  val reg_roplen = Reg(io.in.roplen)
  val reg_rblen  = Reg(io.in.rblen) 
  val reg_rmask  = Reg(io.in.rmask >> UFix(1))

  val reg_wen        = Reg(wpass & io.in.wen)
  val reg_wlast      = Reg(io.in.wlast)
  val reg_wcnt       = Reg(Mux(wpass, io.in.wcnt.toUFix - io.in.wmask(0), UFix(0)))
  val reg_waddr      = Reg(io.in.waddr)
  val reg_wsel       = Reg(io.in.wsel)
  val reg_wmask      = Reg(io.in.wmask >> UFix(1))
  val reg_wen_mask   = Reg(wpass & io.in.wen_mask)
  val reg_wlast_mask = Reg(io.in.wlast_mask)
  val reg_waddr_mask = Reg(io.in.waddr_mask)

  val reg_viu_val   = Reg(rpass & io.in.viu)
  val reg_viu_fn    = Reg(io.in.viu_fn)
  val reg_viu_utidx = Reg(io.in.viu_utidx.toUFix + UFix(1))
  val reg_viu_imm   = Reg(io.in.viu_imm)

  // every signal related to the read port is delayed by one cycle 
  // because of the register file is an sram

  val delay_roplen = reg_roplen & Fill(SZ_BOPL, io.active) & Fill(SZ_BOPL, io.in.rmask(0))
  val delay_rblen  = reg_rblen & Fill(SZ_BRPORT, io.active) & Fill(SZ_BRPORT, io.in.rmask(0))

  val delay_viu_fn  = reg_viu_fn
  val delay_viu_imm = reg_viu_imm
  
  val delay_viu_val   = Reg(io.in.viu & io.active)
  val delay_viu_utidx = Reg(io.in.viu_utidx) 

  io.rw.rblen := delay_rblen

  val rfile = new vuVXU_Banked8_Bank_Regfile()
  val alu = new vuVXU_Banked8_FU_alu()

  rfile.io.ren    := io.in.ren & io.active & io.in.rmask(0)
  rfile.io.raddr  := io.in.raddr
  rfile.io.roplen := delay_roplen
  
  rfile.io.wen    := alu.io.wen_masked & io.active & io.in.wmask(0)
  rfile.io.waddr  := io.in.waddr
  rfile.io.wsel   := io.in.wsel

  io.rw.rdata     := rfile.io.rdata
  io.rw.ropl0     := rfile.io.ropl0
  io.rw.ropl1     := rfile.io.ropl1

  rfile.io.wbl0   := io.rw.wbl0
  rfile.io.wbl1   := io.rw.wbl1
  rfile.io.wbl2   := io.rw.wbl2
  rfile.io.wbl3   := io.rw.wbl3

  val viu_rdata       = rfile.io.viu_rdata
  val viu_ropl        = rfile.io.viu_ropl
  rfile.io.viu_wdata := alu.io.out

  val viu_in0 = MuxLookup(
    delay_viu_fn(RG_VIU_T0), UFix(0, SZ_DATA), Array(
      M0 -> UFix(0, SZ_DATA),
      ML -> viu_ropl,
      MR -> viu_rdata
    ))

  val viu_in1 = MuxLookup(
    delay_viu_fn(RG_VIU_T1), UFix(0, SZ_DATA), Array(
      M0 -> UFix(0, SZ_DATA),
      MR -> viu_rdata,
      MI -> delay_viu_imm
    ))

  val branch_resolution_register = Vec(WIDTH_BMASK){ Reg(){ Bool() } }
  when (io.wen_mask && io.wmask(0)){ branch_resolution_register(io.waddr_mask) := alu.io.branch_result  }
  io.branch_resolution_mask := branch_resolution_register.toBits

  alu.io.valid := delay_viu_val
  alu.io.wen   := io.in.wen
  alu.io.fn    := delay_viu_fn
  alu.io.utidx := delay_viu_utidx
  alu.io.in0   := viu_in0
  alu.io.in1   := viu_in1

  io.out.ren    := Mux(io.active, reg_ren, io.in.ren)
  io.out.rlast  := Mux(io.active, reg_rlast, io.in.rlast)
  io.out.rcnt   := Mux(io.active, reg_rcnt, io.in.rcnt)
  io.out.raddr  := Mux(io.active, reg_raddr, io.in.raddr)
  io.out.roplen := Mux(io.active, reg_roplen, io.in.roplen)
  io.out.rblen  := Mux(io.active, reg_rblen, io.in.rblen)
  io.out.rmask  := Mux(io.active, reg_rmask, io.in.rmask)

  io.out.wen        := Mux(io.active, reg_wen, io.in.wen)
  io.out.wlast      := Mux(io.active, reg_wlast, io.in.wlast)
  io.out.wcnt       := Mux(io.active, reg_wcnt, io.in.wcnt)
  io.out.waddr      := Mux(io.active, reg_waddr, io.in.waddr)
  io.out.wsel       := Mux(io.active, reg_wsel, io.in.wsel)
  io.out.wmask      := Mux(io.active, reg_wmask, io.in.wmask)
  io.out.wen_mask   := Mux(io.active, reg_wen_mask, io.in.wen_mask)
  io.out.wlast_mask := Mux(io.active, reg_wlast_mask, io.in.wlast_mask)
  io.out.waddr_mask := Mux(io.active, reg_waddr_mask, io.in.waddr_mask)

  io.out.viu   := Mux(io.active, reg_viu_val, io.in.viu)
  io.out.viu_fn    := Mux(io.active, reg_viu_fn, io.in.viu_fn)
  io.out.viu_utidx := Mux(io.active, reg_viu_utidx, io.in.viu_utidx)
  io.out.viu_imm   := Mux(io.active, reg_viu_imm, io.in.viu_imm)
}

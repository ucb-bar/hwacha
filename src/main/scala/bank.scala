package hwacha

import Chisel._
import Node._
import Constants._

class BankToBankIO extends Bundle
{
  val ren    = Bool(INPUT)
  val rlast  = Bool(INPUT)
  val rcnt   = Bits(INPUT, SZ_BVLEN)
  val raddr  = Bits(INPUT, SZ_BREGLEN)
  val roplen = Bits(INPUT, SZ_BOPL)
  val rblen  = Bits(INPUT, SZ_BRPORT)
  val rmask  = Bits(INPUT, SZ_BANK)

  val wen        = Bool(INPUT)
  val wlast      = Bool(INPUT)
  val wcnt       = Bits(INPUT, SZ_BVLEN)
  val waddr      = Bits(INPUT, SZ_BREGLEN)
  val wsel       = Bits(INPUT, SZ_BWPORT)
  val wmask      = Bits(INPUT, SZ_BANK)

  val wen_mask   = Bool(INPUT)
  val wlast_mask = Bool(INPUT)
  val wcnt_mask  = Bits(INPUT, SZ_BVLEN)
  val wmask_mask = Bits(INPUT, SZ_BANK)
  val waddr_mask = Bits(INPUT, SZ_BMASK)
  val pvfb_tag   = Bits(INPUT, SZ_PVFB_TAG)

  val viu       = Bool(INPUT)
  val viu_fn    = Bits(INPUT, SZ_VIU_FN)
  val viu_utidx = Bits(INPUT, SZ_VLEN)
  val viu_imm   = Bits(INPUT, SZ_DATA)
}

class Bank extends Module
{
  val io = new Bundle {
    val active = Bool(INPUT)
    
    val in  = new BankToBankIO()
    val out = new BankToBankIO().flip
    
    val rw  = new Bundle {
      val rblen = Bits(OUTPUT, SZ_BRPORT)
      val rdata = Bits(OUTPUT, SZ_DATA)
      val ropl0 = Bits(OUTPUT, SZ_DATA)
      val ropl1 = Bits(OUTPUT, SZ_DATA)

      val wbl0 = Bits(INPUT, SZ_DATA)
      val wbl1 = Bits(INPUT, SZ_DATA)
      val wbl2 = Bits(INPUT, SZ_DATA)
      val wbl3 = Bits(INPUT, SZ_DATA)
    }

    val branch_resolution_mask = Bits(OUTPUT, WIDTH_BMASK)

    val prec = Bits(INPUT, SZ_PREC)
  }

  val rpass = io.in.rcnt.orR
  val wpass = io.in.wcnt.orR

  val reg_ren    = Reg(next=rpass & io.in.ren)
  val reg_rlast  = Reg(next=io.in.rlast)
  val reg_rcnt   = Reg(next=Mux(rpass, io.in.rcnt.toUInt - UInt(1), UInt(0)))
  val reg_raddr  = Reg(next=io.in.raddr)
  val reg_roplen = Reg(next=io.in.roplen)
  val reg_rblen  = Reg(next=io.in.rblen) 
  val reg_rmask  = Reg(next=io.in.rmask >> UInt(1))

  val reg_wen        = Reg(next=wpass & io.in.wen)
  val reg_wlast      = Reg(next=io.in.wlast)
  val reg_wcnt       = Reg(next=Mux(wpass, io.in.wcnt.toUInt - UInt(1), UInt(0)))
  val reg_waddr      = Reg(next=io.in.waddr)
  val reg_wsel       = Reg(next=io.in.wsel)
  val reg_wmask      = Reg(next=io.in.wmask >> UInt(1))


  val wpass_mask = io.in.wcnt_mask.orR

  val reg_wen_mask   = Reg(next=wpass_mask & io.in.wen_mask)
  val reg_wlast_mask = Reg(next=io.in.wlast_mask)
  val reg_wcnt_mask  = Reg(next=Mux(wpass_mask, io.in.wcnt_mask.toUInt - UInt(1), UInt(0)))
  val reg_wmask_mask = Reg(next=io.in.wmask_mask >> UInt(1))
  val reg_waddr_mask = Reg(next=io.in.waddr_mask)
  val reg_pvfb_tag   = Reg(next=io.in.pvfb_tag)

  val reg_viu_val   = Reg(next=rpass & io.in.viu)
  val reg_viu_fn    = Reg(next=io.in.viu_fn)
  val reg_viu_utidx = Reg(next=io.in.viu_utidx.toUInt + UInt(1))
  val reg_viu_imm   = Reg(next=io.in.viu_imm)

  // every signal related to the read port is delayed by one cycle 
  // because of the register file is an sram

  val rmask0 = if(HAVE_PVFB) io.in.rmask(0) else Bool(true)
  val wmask0 = if(HAVE_PVFB) io.in.wmask(0) else Bool(true)

  val delay_roplen = reg_roplen & Fill(SZ_BOPL, io.active) & Fill(SZ_BOPL, Reg(next=rmask0))
  val delay_rblen  = reg_rblen & Fill(SZ_BRPORT, io.active) & Fill(SZ_BRPORT, Reg(next=rmask0))

  val delay_viu_fn  = reg_viu_fn
  val delay_viu_imm = reg_viu_imm
  
  val delay_viu_val   = Reg(next=io.in.viu & io.active)
  val delay_viu_utidx = Reg(next=io.in.viu_utidx) 

  io.rw.rblen := delay_rblen

  val rfile = Module(new BankRegfile)
  val alu = Module(new BankALU)

  rfile.io.ren    := io.in.ren & io.active & rmask0
  rfile.io.raddr  := io.in.raddr
  rfile.io.roplen := delay_roplen
  
  rfile.io.wen    := alu.io.wen_masked & io.active & wmask0
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
    delay_viu_fn(RG_VIU_T0), UInt(0, SZ_DATA), Array(
      M0 -> UInt(0, SZ_DATA),
      ML -> viu_ropl,
      MR -> viu_rdata
    ))

  val viu_in1 = MuxLookup(
    delay_viu_fn(RG_VIU_T1), UInt(0, SZ_DATA), Array(
      M0 -> UInt(0, SZ_DATA),
      MR -> viu_rdata,
      MI -> delay_viu_imm
    ))

  val branch_resolution_register = Reg(init=Bits(0, WIDTH_BMASK))
  when (io.in.wen_mask && io.in.wmask_mask(0))
  { 
    branch_resolution_register := branch_resolution_register.bitSet(io.in.waddr_mask, alu.io.branch_result)
  }
  io.branch_resolution_mask := branch_resolution_register

  rfile.io.prec := io.prec

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
  io.out.wcnt_mask  := Mux(io.active, reg_wcnt_mask, io.in.wcnt_mask)
  io.out.wmask_mask := Mux(io.active, reg_wmask_mask, io.in.wmask_mask)
  io.out.waddr_mask := Mux(io.active, reg_waddr_mask, io.in.waddr_mask)
  io.out.pvfb_tag   := Mux(io.active, reg_pvfb_tag, io.in.pvfb_tag)

  io.out.viu   := Mux(io.active, reg_viu_val, io.in.viu)
  io.out.viu_fn    := Mux(io.active, reg_viu_fn, io.in.viu_fn)
  io.out.viu_utidx := Mux(io.active, reg_viu_utidx, io.in.viu_utidx)
  io.out.viu_imm   := Mux(io.active, reg_viu_imm, io.in.viu_imm)
}

class BankBufferedWrite extends Module
{
  val io = new Bundle {
    val prec = Bits(INPUT, SZ_PREC)
    val active = Bool(INPUT)

    val position = Bits(INPUT, SZ_BUF_PREC)
    val in = Bits(INPUT, SZ_DATA)

    val out = Bits(OUTPUT, SZ_DATA)
  }

  val h = Vec.fill(4){ Reg(Bits(width = 16), init = UInt(0, 16)) }
  val s = Vec.fill(2){ Reg(Bits(width = 33), init = UInt(0, 33)) }
  val d = Vec.fill(1){ Reg(Bits(width = 65), init = UInt(0, 65)) }

  switch (io.position) {
    is (PREC_BUF_NULL) { }
    is (PREC_HALF_0) { h(0) := io.in(15,0) }
    is (PREC_HALF_1) { h(1) := io.in(15,0) }
    is (PREC_HALF_2) { h(2) := io.in(15,0) }
    is (PREC_HALF_3) { h(3) := io.in(15,0) }
    is (PREC_SINGLE_0) { s(0) := io.in(32,0) }
    is (PREC_SINGLE_1) { s(1) := io.in(32,0) }
    is (PREC_DOUBLE_0) { d(0) := io.in(64,0) }
  }

  val buffered = MuxLookup(io.prec, UInt(0, 66), Array (
    PREC_DOUBLE -> Cat(Bits(0), d(0)),
    PREC_SINGLE -> Cat(s(1), s(0)),
    PREC_HALF   -> Cat(Bits(0), h(3), h(2), Bits(0), h(1), h(0))
  ))

  io.out := Mux(io.active, buffered, io.in)
}

class BankRegfile extends Module
{
  val io = new Bundle {
    val ren    = Bool(INPUT)
    val raddr  = Bits(INPUT, SZ_BREGLEN)
    val roplen = Bits(INPUT, SZ_BOPL)

    val wen   = Bool(INPUT)
    val waddr = Bits(INPUT, SZ_BREGLEN)
    val wsel  = Bits(INPUT, SZ_BWPORT)

    val rdata = Bits(OUTPUT, SZ_DATA)
    val ropl0 = Bits(OUTPUT, SZ_DATA)
    val ropl1 = Bits(OUTPUT, SZ_DATA)

    val wbl0 = Bits(INPUT, SZ_DATA)
    val wbl1 = Bits(INPUT, SZ_DATA)
    val wbl2 = Bits(INPUT, SZ_DATA)
    val wbl3 = Bits(INPUT, SZ_DATA)

    val viu_rdata = Bits(OUTPUT, SZ_DATA)
    val viu_ropl  = Bits(OUTPUT, SZ_DATA)
    val viu_wdata = Bits(INPUT, SZ_DATA)

    val prec = Bits(INPUT, SZ_PREC)
  }

  val wdata = MuxLookup(
    io.wsel, Bits(0, SZ_DATA), Array(
      Bits(0, SZ_BWPORT) -> io.wbl0,
      Bits(1, SZ_BWPORT) -> io.wbl1,
      Bits(2, SZ_BWPORT) -> io.wbl2,
      Bits(3, SZ_BWPORT) -> io.wbl3,
      Bits(4, SZ_BWPORT) -> io.viu_wdata
    ))

  val buffer = Module(new BankBufferedWrite)
  buffer.io.prec := io.prec
  buffer.io.active := Bool(false)
  buffer.io.position := PREC_BUF_NULL
  buffer.io.in := wdata
  val buffered_wdata = buffer.io.out

  val rfile = Mem(Bits(width = SZ_DATA), 256, seqRead = true)
  val raddr = Reg(Bits())
  when (io.wen) { rfile(io.waddr) := buffered_wdata }
  when (io.ren) { raddr := io.raddr }
  val rdata_rf = Mux(Reg(next=io.ren), rfile(raddr), Bits(0)) 
  io.rdata := rdata_rf

  val ropl0Reg = Reg(Bits(width = SZ_DATA))
  val ropl1Reg = Reg(Bits(width = SZ_DATA))
  when (io.roplen(0)) { ropl0Reg := rdata_rf }
  when (io.roplen(1)) { ropl1Reg := rdata_rf }

  io.ropl0 := ropl0Reg
  io.ropl1 := ropl1Reg

  io.viu_rdata := rdata_rf
  io.viu_ropl := io.ropl0
}

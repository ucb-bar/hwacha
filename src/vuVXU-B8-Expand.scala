package hwacha

import Chisel._
import Node._
import Constants._

class io_vxu_expand_read extends Bundle
{
  val ren = Bool()
  val rlast = Bool()
  val rcnt = Bits(width = SZ_BVLEN)
  val raddr = Bits(width = SZ_BREGLEN)
  val roplen = Bits(width = SZ_BOPL)
  val rblen = Bits(width = SZ_BRPORT)
  val rmask = Bits(width = SZ_BANK)
}

class io_vxu_expand_write extends Bundle
{
  val wen = Bool()
  val wlast = Bool()
  val wcnt = Bits(width = SZ_BVLEN)
  val waddr = Bits(width = SZ_BREGLEN)
  val wsel = Bits(width = SZ_BWPORT)
  val wmask = Bits(width = SZ_BANK)

  val wen_mask = Bool()
  val wlast_mask = Bool()
  val wcnt_mask = Bits(width = SZ_BVLEN)
  val wmask_mask = Bits(width = SZ_BANK)
  val waddr_mask = Bits(width = SZ_BMASK)
  val pvfb_tag = Bits(width = SZ_PVFB_TAG)
}

class io_vxu_expand_fu_fn extends Bundle
{
  val viu = Bool()
  val viu_fn = Bits(width = SZ_VIU_FN)
  val viu_utidx = Bits(width = SZ_VLEN)
  val viu_imm = Bits(width = SZ_DATA)
}

class io_vxu_expand_lfu_fn extends Bundle
{
  val vau0 = Bool()
  val vau0_fn = Bits(width = SZ_VAU0_FN)
  val vau1 = Bool()
  val vau1_fn = Bits(width = SZ_VAU1_FN)
  val vau2 = Bool()
  val vau2_fn = Bits(width = SZ_VAU2_FN)
  val mem = new io_vxu_mem_cmd()
  val imm = Bits(width = SZ_DATA)
  val imm2 = Bits(width = SZ_XIMM2)
  val vaq = Bool()
  val vaq_mask = Bits(width=SZ_BANK)
  val vaq_pop_cnt = UFix(width=SZ_LGBANK1)
  val vldq = Bool()
  val vldq_mask = Bits(width=SZ_BANK)
  val vsdq = Bool()
  val vsdq_mask = Bits(width=SZ_BANK)
  val utmemop = Bool()
}

class io_vxu_expand_to_hazard extends Bundle
{
  val ren = Bool()
  val wen = Bool()
  val wen_mask = Bool()
}

class io_expand_to_xcpt_handler extends Bundle
{
  val empty = Bool(OUTPUT)
}

class io_vxu_expand extends Bundle
{
  val seq_to_expand = new io_vxu_seq_to_expand().asInput
  val expand_to_hazard = new io_vxu_expand_to_hazard().asOutput

  val seq = new io_vxu_seq_fu().asInput
  val seq_fn = new io_vxu_seq_fn().asInput
  val seq_regid_imm = new io_vxu_seq_regid_imm().asInput

  val expand_read = new io_vxu_expand_read().asOutput
  val expand_write = new io_vxu_expand_write().asOutput
  val expand_fu_fn = new io_vxu_expand_fu_fn().asOutput
  val expand_lfu_fn = new io_vxu_expand_lfu_fn().asOutput

  val expand_to_xcpt = new io_expand_to_xcpt_handler()
}

class vuVXU_Banked8_Expand extends Component 
{
  val io = new io_vxu_expand

  val next_ren = Vec(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_rlast = Vec(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_rcnt = Vec(SHIFT_BUF_READ){ Wire(){Bits(width=SZ_BVLEN)} }
  val next_raddr = Vec(SHIFT_BUF_READ){ Wire(){Bits(width=SZ_BREGLEN)} }
  val next_roplen = Vec(SHIFT_BUF_READ){ Wire(){Bits(width=SZ_BOPL)} }
  val next_rblen = VecBuf(SHIFT_BUF_READ){ Vec(SZ_BRPORT){ Wire(){Bool()} } }
  val next_rmask = Vec(SHIFT_BUF_READ){ Wire(){ Bits(width=SZ_BANK) } }

  val reg_ren = Vec(SHIFT_BUF_READ){ Reg(resetVal=Bool(false))}
  val reg_rlast = Vec(SHIFT_BUF_READ){ Reg(){ Bool() } }
  val reg_rcnt = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_BVLEN)} }
  val reg_raddr = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_BREGLEN)} }
  val reg_roplen = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_BOPL)} }
  val reg_rblen = VecBuf(SHIFT_BUF_READ){ Vec(SZ_BRPORT){ Reg(){Bool()} } }
  val reg_rmask = Vec(SHIFT_BUF_READ){ Reg(){ Bits(width=SZ_BANK) } }

  for (i <- 0 until SHIFT_BUF_READ){
    reg_ren(i) := next_ren(i)
    reg_rlast(i) := next_rlast(i)
    reg_rcnt(i) := next_rcnt(i)
    reg_raddr(i) := next_raddr(i)
    reg_roplen(i) := next_roplen(i)
    for(j <- 0 until SZ_BRPORT)
      reg_rblen(i)(j) := next_rblen(i)(j)
    reg_rmask(i) := next_rmask(i)
  }

  for (i <- 0 until SHIFT_BUF_READ-1)
  {
    next_ren(i) := reg_ren(i+1)
    next_rlast(i) := reg_rlast(i+1)
    next_rcnt(i) := reg_rcnt(i+1)
    next_raddr(i) := reg_raddr(i+1)
    next_roplen(i) := reg_roplen(i+1)
    for(j <- 0 until SZ_BRPORT)
      next_rblen(i)(j) := reg_rblen(i+1)(j)
    next_rmask(i) := reg_rmask(i+1)
  }

  next_ren(SHIFT_BUF_READ-1) := Bool(false)
  next_rlast(SHIFT_BUF_READ-1) := Bool(false)
  next_rcnt(SHIFT_BUF_READ-1) := Bits("d0", 3)
  next_raddr(SHIFT_BUF_READ-1) := Bits("d0", 8)
  next_roplen(SHIFT_BUF_READ-1) := Bits("d0", 2)
  for(i <- 0 until SZ_BRPORT)
    next_rblen(SHIFT_BUF_READ-1)(i) := Bool(false)
  next_rmask(SHIFT_BUF_READ-1) := Bits(0,SZ_BANK)

  when (io.seq.viu) 
  {
    next_ren(0) := Bool(true)
    next_rlast(0) := io.seq_to_expand.last
    next_rcnt(0) := io.seq_regid_imm.cnt
    next_raddr(0) := io.seq_regid_imm.vs
    next_roplen(0) := Bits("b01", 2)
    for(i <- 0 until SZ_BRPORT)
      next_rblen(0)(i) := Bool(false)
    next_rmask(0) := io.seq_regid_imm.mask

    when (io.seq_fn.viu(RG_VIU_T) === Cat(ML,MR))
    {
      next_ren(1) := Bool(true)
      next_rlast(1) := io.seq_to_expand.last
      next_rcnt(1) := io.seq_regid_imm.cnt
      next_raddr(1) := io.seq_regid_imm.vt
      next_roplen(1) := Bits("b00", 2)
      for(i <- 0 until SZ_BRPORT)
        next_rblen(1)(i) := Bool(false)
      next_rmask(1) := io.seq_regid_imm.mask
    }
    when (io.seq_fn.viu(RG_VIU_T) === Cat(M0,MR))
    {
      next_raddr(0) := io.seq_regid_imm.vt
    }
  }
  when (io.seq.vau0) 
  {
    next_ren(0) := Bool(true)
    next_rlast(0) := io.seq_to_expand.last
    next_rcnt(0) := io.seq_regid_imm.cnt
    next_raddr(0) := io.seq_regid_imm.vs
    next_roplen(0) := Bits("b01", 2)
    for(i <- 0 until SZ_BRPORT)
      next_rblen(0)(i) := Bool(false)
    next_rmask(0) := io.seq_regid_imm.mask
    
    next_ren(1) := Bool(true)
    next_rlast(1) := io.seq_to_expand.last
    next_rcnt(1) := io.seq_regid_imm.cnt
    next_raddr(1) := io.seq_regid_imm.vt
    next_roplen(1) := Bits("b00", 2)
    for(i <- 0 until SZ_BRPORT)
      if(i == 1 || i == 0) 
        next_rblen(1)(i) := Bool(true)
      else
        next_rblen(1)(i) := Bool(false)
    next_rmask(1) := io.seq_regid_imm.mask

    when (io.seq_regid_imm.vs_zero) { next_rblen(1)(0) := Bool(false) }
    when (io.seq_regid_imm.vt_zero) { next_rblen(1)(1) := Bool(false) }
  }
  when (io.seq.vau1)
  {
    when (FN_VAU1_FMA(io.seq_fn.vau1))
    {
      next_ren(0) := Bool(true)
      next_rlast(0) := io.seq_to_expand.last
      next_rcnt(0) := io.seq_regid_imm.cnt
      next_raddr(0) := io.seq_regid_imm.vs
      next_roplen(0) := Bits("b10", 2)
      for(i <- 0 until SZ_BRPORT)
        next_rblen(0)(i) := Bool(false)
      next_rmask(0) := io.seq_regid_imm.mask
        
      next_ren(1) := Bool(true)
      next_rlast(1) := io.seq_to_expand.last
      next_rcnt(1) := io.seq_regid_imm.cnt
      next_raddr(1) := io.seq_regid_imm.vt
      next_roplen(1) := Bits("b01", 2)
      for(i <- 0 until SZ_BRPORT)
        next_rblen(1)(i) := Bool(false)
      next_rmask(1) := io.seq_regid_imm.mask
      
      next_ren(2) := Bool(true)
      next_rlast(2) := io.seq_to_expand.last
      next_rcnt(2) := io.seq_regid_imm.cnt
      next_raddr(2) := io.seq_regid_imm.vr
      next_roplen(2) := Bits("b00", 2)
      for(i <- 0 until SZ_BRPORT)
        if(i == 2 || i == 3 || i == 4) 
          next_rblen(2)(i) := Bool(true)
        else
          next_rblen(2)(i) := Bool(false)
      next_rmask(2) := io.seq_regid_imm.mask


      when (io.seq_regid_imm.vs_zero) { next_rblen(2)(2) := Bool(false) }
      when (io.seq_regid_imm.vt_zero) { next_rblen(2)(3) := Bool(false) }
      when (io.seq_regid_imm.vr_zero) { next_rblen(2)(4) := Bool(false) }
    }
    .otherwise
    {
      next_ren(0) := Bool(true)
      next_rlast(0) := io.seq_to_expand.last
      next_rcnt(0) := io.seq_regid_imm.cnt
      next_raddr(0) := io.seq_regid_imm.vs
      next_roplen(0) := Bits("b10", 2)
      for(i <- 0 until SZ_BRPORT)
        next_rblen(0)(i) := Bool(false)
      next_rmask(0) := io.seq_regid_imm.mask
      
      next_ren(1) := Bool(true)
      next_rlast(1) := io.seq_to_expand.last
      next_rcnt(1) := io.seq_regid_imm.cnt
      next_raddr(1) := io.seq_regid_imm.vt
      next_roplen(1) := Bits("b00", 2)
      for(i <- 0 until SZ_BRPORT){
        if(i == 2 || i == 4)
          next_rblen(1)(i) := Bool(true)
        else
          next_rblen(1)(i) := Bool(false)
      }
      next_rmask(1) := io.seq_regid_imm.mask

      when (io.seq_regid_imm.vs_zero) { next_rblen(2)(2) := Bool(false) }
      when (io.seq_regid_imm.vt_zero) { next_rblen(2)(4) := Bool(false) }
    }
  }
  when (io.seq.vau2)
  {
    next_ren(0) := Bool(true)
    next_rlast(0) := io.seq_to_expand.last
    next_rcnt(0) := io.seq_regid_imm.cnt
    next_raddr(0) := io.seq_regid_imm.vs
    next_roplen(0) := Bits("b00", 2)
    for(i <- 0 until SZ_BRPORT)
      if(i == 5)
        next_rblen(0)(i) := Bool(true)
      else
        next_rblen(0)(i) := Bool(false) 
    next_rmask(0) := io.seq_regid_imm.mask

    when (io.seq_regid_imm.vs_zero) { next_rblen(0)(5) := Bool(false) }
  }
  when (io.seq.vaq)
  {
    next_ren(0) := Bool(true)
    next_rlast(0) := io.seq_to_expand.last
    next_rcnt(0) := io.seq_regid_imm.cnt
    when (io.seq_regid_imm.utmemop)
    {
      next_raddr(0) := io.seq_regid_imm.vs
      next_roplen(0) := Bits("b00", 2)
      for(i <- 0 until SZ_BRPORT)
        if(i == 6)
        next_rblen(0)(i) := Bool(true)
      else
        next_rblen(0)(i) := Bool(false)

      when (io.seq_regid_imm.vs_zero) { next_rblen(0)(6) := Bool(false) }
    }
    next_rmask(0) := io.seq_regid_imm.mask
  }
  when (io.seq.vsdq)
  {
    next_ren(0) := Bool(true)
    next_rlast(0) := io.seq_to_expand.last
    next_rcnt(0) := io.seq_regid_imm.cnt
    next_raddr(0) := io.seq_regid_imm.vt
    next_roplen(0) := Bits("b00", 2)
    for(i <- 0 until SZ_BRPORT)
      if(i == 7)
        next_rblen(0)(i) := Bool(true)
      else
        next_rblen(0)(i) := Bool(false)
    next_rmask(0) := io.seq_regid_imm.mask

    when (io.seq_regid_imm.vt_zero) { next_rblen(0)(7) := Bool(false) }
  }

  val next_wen = Vec(SHIFT_BUF_WRITE){ Wire(){ Bool() } }
  val next_wlast = Vec(SHIFT_BUF_WRITE){ Wire(){ Bool() } }
  val next_wcnt = Vec(SHIFT_BUF_WRITE){ Wire(){Bits(width=SZ_BVLEN)} }
  val next_waddr = Vec(SHIFT_BUF_WRITE){ Wire(){Bits(width=SZ_BREGLEN)} }
  val next_wsel = Vec(SHIFT_BUF_WRITE){ Wire(){Bits(width=SZ_BWPORT)} }
  val next_wmask = Vec(SHIFT_BUF_WRITE){ Wire(){Bits(width=SZ_BANK) } }

  val next_wen_mask = Vec(SHIFT_BUF_WRITE){ Wire(){ Bool() } }
  val next_wlast_mask = Vec(SHIFT_BUF_WRITE){ Wire(){ Bool() } }
  val next_wcnt_mask = Vec(SHIFT_BUF_WRITE){ Wire(){Bits(width=SZ_BVLEN)} }
  val next_wmask_mask = Vec(SHIFT_BUF_WRITE){ Wire(){Bits(width=SZ_BANK) } }
  val next_waddr_mask = Vec(SHIFT_BUF_WRITE){ Wire(){ Bits(width=SZ_BMASK) } }
  val next_pvfb_tag = Vec(SHIFT_BUF_WRITE){ Wire(){ Bits(width=SZ_PVFB_TAG) } }

  val reg_wen = Vec(SHIFT_BUF_WRITE){ Reg(resetVal=Bool(false)) }
  val reg_wlast = Vec(SHIFT_BUF_WRITE){ Reg(){ Bool() } }
  val reg_wcnt = Vec(SHIFT_BUF_WRITE){ Reg(){Bits(width=SZ_BVLEN)} }
  val reg_waddr = Vec(SHIFT_BUF_WRITE){ Reg(){Bits(width=SZ_BREGLEN)} }
  val reg_wsel = Vec(SHIFT_BUF_WRITE){ Reg(){Bits(width=SZ_BWPORT)} }
  val reg_wmask = Vec(SHIFT_BUF_WRITE){ Reg(){Bits(width=SZ_BANK) } }

  val reg_wen_mask = Vec(SHIFT_BUF_WRITE){ Reg(){ Bool() } }
  val reg_wlast_mask = Vec(SHIFT_BUF_WRITE){ Reg(){ Bool() } }
  val reg_wcnt_mask = Vec(SHIFT_BUF_WRITE){ Reg(){Bits(width=SZ_BVLEN)} }
  val reg_wmask_mask = Vec(SHIFT_BUF_WRITE){ Reg(){Bits(width=SZ_BANK) } }
  val reg_waddr_mask = Vec(SHIFT_BUF_WRITE){ Reg(){ Bits(width=SZ_BMASK) } }
  val reg_pvfb_tag = Vec(SHIFT_BUF_WRITE){ Reg(){ Bits(width=SZ_PVFB_TAG) } }

  for (i <- 0 until SHIFT_BUF_WRITE)
  {
    reg_wen(i) := next_wen(i)
    reg_wlast(i) := next_wlast(i)
    reg_wcnt(i) := next_wcnt(i)
    reg_waddr(i) := next_waddr(i)
    reg_wsel(i) := next_wsel(i)
    reg_wmask(i) := next_wmask(i)

    reg_wen_mask(i) := next_wen_mask(i)
    reg_wlast_mask(i) := next_wlast_mask(i)
    reg_wcnt_mask(i) := next_wcnt_mask(i)
    reg_wmask_mask(i) := next_wmask_mask(i)
    reg_waddr_mask(i) := next_waddr_mask(i)
    reg_pvfb_tag(i) := next_pvfb_tag(i)
  }

  val viu_wptr = 
    Mux((io.seq_fn.viu(RG_VIU_T) === Cat(ML,MR)) , Bits(INT_STAGES + 2, SZ_BPTR1)
    , Bits(INT_STAGES + 1, SZ_BPTR1))

  val vau0_wptr =
    Bits(IMUL_STAGES + 2, SZ_BPTR1)

  val vau1_wptr =
    Mux(FN_VAU1_FMA(io.seq_fn.vau1), Bits(FMA_STAGES + 3, SZ_BPTR1)
    , Bits(FMA_STAGES + 2, SZ_BPTR1))

  val vau2_wptr = 
    Bits(FCONV_STAGES + 1,  SZ_BPTR1)

  for (i <- 0 until SHIFT_BUF_WRITE-1)
  {
    next_wen(i) := reg_wen(i+1)
    next_wlast(i) := reg_wlast(i+1)
    next_wcnt(i) := reg_wcnt(i+1)
    next_waddr(i) := reg_waddr(i+1)
    next_wsel(i) := reg_wsel(i+1)
    next_wmask(i) := reg_wmask(i+1)

    next_wen_mask(i) := reg_wen_mask(i+1)
    next_wlast_mask(i) := reg_wlast_mask(i+1)
    next_wcnt_mask(i) := reg_wcnt_mask(i+1)
    next_wmask_mask(i) := reg_wmask_mask(i+1)
    next_waddr_mask(i) := reg_waddr_mask(i+1)
    next_pvfb_tag(i) := reg_pvfb_tag(i+1)
  }

  next_wen(SHIFT_BUF_WRITE-1) := Bool(false)
  next_wlast(SHIFT_BUF_WRITE-1) := Bool(false)
  next_wcnt(SHIFT_BUF_WRITE-1) := Bits("d0", 3)
  next_waddr(SHIFT_BUF_WRITE-1) := Bits("d0", 8)
  next_wsel(SHIFT_BUF_WRITE-1) := Bits("d0", 3)
  next_wmask(SHIFT_BUF_WRITE-1) := Bits(0,SZ_BANK)

  next_wen_mask(SHIFT_BUF_WRITE-1) := Bool(false)
  next_wlast_mask(SHIFT_BUF_WRITE-1) := Bool(false)
  next_wcnt_mask(SHIFT_BUF_WRITE-1) := Bits("d0", 3)
  next_wmask_mask(SHIFT_BUF_WRITE-1) := Bits(0, SZ_BANK)
  next_waddr_mask(SHIFT_BUF_WRITE-1) := Bits(0, SZ_BMASK)
  next_pvfb_tag(SHIFT_BUF_WRITE-1) := Bits(0, SZ_PVFB_TAG)
      
  when (io.seq.viu)
  {
    when (isVIUBranch(io.seq_fn.viu(RG_VIU_FN)))
    {
      next_wen_mask.write(viu_wptr, Bool(true))
      next_wlast_mask.write(viu_wptr, io.seq_to_expand.last)
      next_wcnt_mask.write(viu_wptr, io.seq_regid_imm.cnt)
      next_wmask_mask.write(viu_wptr, io.seq_regid_imm.mask)
      next_waddr_mask.write(viu_wptr, io.seq_regid_imm.vm)
      next_pvfb_tag.write(viu_wptr, io.seq_regid_imm.pvfb_tag)
    }
    . otherwise
    {
      next_wen.write(viu_wptr, Bool(true))
      next_wlast.write(viu_wptr, io.seq_to_expand.last)
      next_wcnt.write(viu_wptr, io.seq_regid_imm.cnt)
      next_waddr.write(viu_wptr, io.seq_regid_imm.vd)
      next_wsel.write(viu_wptr, Bits("d4", 3))
      next_wmask.write(viu_wptr, io.seq_regid_imm.mask)
    }
  }
  when (io.seq.vau0)
  {
    next_wen.write(vau0_wptr, Bool(true))
    next_wlast.write(vau0_wptr, io.seq_to_expand.last)
    next_wcnt.write(vau0_wptr, io.seq_regid_imm.cnt)
    next_waddr.write(vau0_wptr, io.seq_regid_imm.vd)
    next_wsel.write(vau0_wptr, Bits("d0", 3))
    next_wmask.write(vau0_wptr, io.seq_regid_imm.mask)
  }
  when (io.seq.vau1)
  {
    next_wen.write(vau1_wptr, Bool(true))
    next_wlast.write(vau1_wptr, io.seq_to_expand.last)
    next_wcnt.write(vau1_wptr, io.seq_regid_imm.cnt)
    next_waddr.write(vau1_wptr, io.seq_regid_imm.vd)
    next_wsel.write(vau1_wptr, Bits("d1", 3))
    next_wmask.write(vau1_wptr, io.seq_regid_imm.mask)
  }
  when (io.seq.vau2)
  {
    next_wen.write(vau2_wptr, Bool(true))
    next_wlast.write(vau2_wptr, io.seq_to_expand.last)
    next_wcnt.write(vau2_wptr, io.seq_regid_imm.cnt)
    next_waddr.write(vau2_wptr, io.seq_regid_imm.vd)
    next_wsel.write(vau2_wptr, Bits("d2", 3))
    next_wmask.write(vau2_wptr, io.seq_regid_imm.mask)
  }
  when (io.seq.vldq)
  {
    next_wen(0) := Bool(true)
    next_wlast(0) := io.seq_to_expand.last
    next_wcnt(0) := io.seq_regid_imm.cnt
    next_waddr(0) := io.seq_regid_imm.vd
    next_wsel(0) := Bits("d3", 3)
    next_wmask(0) := io.seq_regid_imm.mask
  }

  val next_viu = Vec(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_viu_fn = Vec(SHIFT_BUF_READ){ Wire(){Bits(width=SZ_VIU_FN)} }
  val next_viu_utidx = Vec(SHIFT_BUF_READ){ Wire(){Bits(width=SZ_VLEN)} }
  val next_viu_imm = Vec(SHIFT_BUF_READ){ Wire(){Bits(width=SZ_DATA)} }

  val next_vau0 = Vec(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_vau0_fn = Vec(SHIFT_BUF_READ){ Wire(){Bits(width=SZ_VAU0_FN)} }

  val next_vau1 = Vec(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_vau1_fn = Vec(SHIFT_BUF_READ){ Wire(){Bits(width=SZ_VAU1_FN)} }

  val next_vau2 = Vec(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_vau2_fn = Vec(SHIFT_BUF_READ){ Wire(){Bits(width=SZ_VAU2_FN)} }

  val next_mem = Vec(SHIFT_BUF_READ){ Wire(){ new io_vxu_mem_cmd() } }
  val next_imm = Vec(SHIFT_BUF_READ){ Wire(){ Bits(width=SZ_DATA) } }
  val next_imm2 = Vec(SHIFT_BUF_READ){ Wire(){ Bits(width=SZ_XIMM2) } }
  val next_vaq = Vec(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_vaq_mask = Vec(SHIFT_BUF_READ){ Wire(){Bits(width=SZ_BANK)} }
  val next_vaq_pop_cnt = Vec(SHIFT_BUF_READ){ Wire(){ UFix(width=SZ_LGBANK1) } }
  val next_vldq = Vec(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_vldq_mask =Vec(SHIFT_BUF_READ){ Wire(){Bits(width=SZ_BANK)} }
  val next_vsdq = Vec(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_vsdq_mask = Vec(SHIFT_BUF_READ){ Wire(){Bits(width=SZ_BANK)} }
  val next_utmemop = Vec(SHIFT_BUF_READ){ Wire(){ Bool() } }

  val reg_viu = Vec(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_viu_fn = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_VIU_FN)} }
  val reg_viu_utidx = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_VLEN)} }
  val reg_viu_imm = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_DATA)} }

  val reg_vau0 = Vec(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_vau0_fn = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_VAU0_FN)} }

  val reg_vau1 = Vec(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_vau1_fn = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_VAU1_FN)} }

  val reg_vau2 = Vec(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_vau2_fn = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_VAU2_FN)} }

  val reg_mem = Vec(SHIFT_BUF_READ){ Reg(){ new io_vxu_mem_cmd() } }
  val reg_imm = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_DATA)} }
  val reg_imm2 = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_XIMM2)} }
  val reg_vaq = Vec(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_vaq_mask = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_BANK) } }
  val reg_vaq_pop_cnt = Vec(SHIFT_BUF_READ){ Reg(){UFix(width=SZ_LGBANK1)} }
  val reg_vldq = Vec(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_vldq_mask = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_BANK) } }
  val reg_vsdq = Vec(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_vsdq_mask = Vec(SHIFT_BUF_READ){ Reg(){Bits(width=SZ_BANK) } }
  val reg_utmemop = Vec(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }

  for (i <- 0 until SHIFT_BUF_READ)
  {
    reg_viu(i) := next_viu(i)
    reg_viu_fn(i) := next_viu_fn(i)
    reg_viu_utidx(i) := next_viu_utidx(i)
    reg_viu_imm(i) := next_viu_imm(i)
    reg_vau0(i) := next_vau0(i)
    reg_vau0_fn(i) := next_vau0_fn(i)
    reg_vau1(i) := next_vau1(i)
    reg_vau1_fn(i) := next_vau1_fn(i)
    reg_vau2(i) := next_vau2(i)
    reg_vau2_fn(i) := next_vau2_fn(i)
    reg_mem(i) := next_mem(i)
    reg_imm(i) := next_imm(i)
    reg_imm2(i) := next_imm2(i)
    reg_vaq(i) := next_vaq(i)
    reg_vaq_mask(i) := next_vaq_mask(i)
    reg_vaq_pop_cnt(i) := next_vaq_pop_cnt(i)
    reg_vldq(i) := next_vldq(i)
    reg_vldq_mask(i) := next_vldq_mask(i)
    reg_vsdq(i) := next_vsdq(i)
    reg_vsdq_mask(i) := next_vsdq_mask(i)
    reg_utmemop(i) := next_utmemop(i)
  }

  for(i <- 0 until SHIFT_BUF_READ-1)
  {
    next_viu(i) := reg_viu(i+1)
    next_viu_fn(i) := reg_viu_fn(i+1)
    next_viu_utidx(i) := reg_viu_utidx(i+1)
    next_viu_imm(i) := reg_viu_imm(i+1)
    next_vau0(i) := reg_vau0(i+1)
    next_vau0_fn(i) := reg_vau0_fn(i+1)
    next_vau1(i) := reg_vau1(i+1)
    next_vau1_fn(i) := reg_vau1_fn(i+1)
    next_vau2(i) := reg_vau2(i+1)
    next_vau2_fn(i) := reg_vau2_fn(i+1)
    next_mem(i) := reg_mem(i+1)
    next_imm(i) := reg_imm(i+1)
    next_imm2(i) := reg_imm2(i+1)
    next_vaq(i) := reg_vaq(i+1)
    next_vaq_mask(i) := reg_vaq_mask(i+1)
    next_vaq_pop_cnt(i) := reg_vaq_pop_cnt(i+1)
    next_vldq(i) := reg_vldq(i+1)
    next_vldq_mask(i) := reg_vldq_mask(i+1)
    next_vsdq(i) := reg_vsdq(i+1)
    next_vsdq_mask(i) := reg_vsdq_mask(i+1)
    next_utmemop(i) := reg_utmemop(i+1)
  }
  
  next_viu(SHIFT_BUF_READ-1) := Bool(false)
  next_viu_fn(SHIFT_BUF_READ-1) := Bits("d0", SZ_VIU_FN)
  next_viu_utidx(SHIFT_BUF_READ-1) := Bits("d0", SZ_VLEN)
  next_viu_imm(SHIFT_BUF_READ-1) := Bits("d0", SZ_DATA)
  next_vau0(SHIFT_BUF_READ-1) := Bool(false)
  next_vau0_fn(SHIFT_BUF_READ-1) := Bits("d0", SZ_VAU0_FN)
  next_vau1(SHIFT_BUF_READ-1) := Bool(false)
  next_vau1_fn(SHIFT_BUF_READ-1) := Bits("d0", SZ_VAU1_FN)
  next_vau2(SHIFT_BUF_READ-1) := Bool(false)
  next_vau2_fn(SHIFT_BUF_READ-1) := Bits("d0", SZ_VAU2_FN)
  next_mem(SHIFT_BUF_READ-1).cmd := Bits("d0", 4)
  next_mem(SHIFT_BUF_READ-1).typ := Bits("d0", 3)
  next_mem(SHIFT_BUF_READ-1).typ_float := Bits("d0", 1)
  next_imm(SHIFT_BUF_READ-1) := Bits("d0", SZ_DATA)
  next_imm2(SHIFT_BUF_READ-1) := Bits("d0", SZ_XIMM2)
  next_vaq(SHIFT_BUF_READ-1) := Bool(false)
  next_vaq_mask(SHIFT_BUF_READ-1) := Bits(0, SZ_BANK)
  next_vaq_pop_cnt(SHIFT_BUF_READ-1) := UFix(0, SZ_LGBANK1)
  next_vldq(SHIFT_BUF_READ-1) := Bool(false)
  next_vldq_mask(SHIFT_BUF_READ-1) := Bits(0, SZ_BANK)
  next_vsdq(SHIFT_BUF_READ-1) := Bool(false)
  next_vsdq_mask(SHIFT_BUF_READ-1) := Bits(0,SZ_BANK)
  next_utmemop(SHIFT_BUF_READ-1) := Bool(false)

  when (io.seq.viu)
  {
    when (io.seq_fn.viu(RG_VIU_T) === Cat(ML,MR))
    {
      next_viu(1) := Bool(true)
      when (io.seq_regid_imm.vs_zero)
      {
        when (io.seq_regid_imm.vt_zero) { next_viu_fn(1) := Cat(M0, M0, io.seq_fn.viu(6,0)) }
        .otherwise { next_viu_fn(1) := Cat(M0, io.seq_fn.viu(8,0)) }
      }
      .otherwise
      {
        when (io.seq_regid_imm.vt_zero) { next_viu_fn(1) := Cat(io.seq_fn.viu(10,9), M0, io.seq_fn.viu(6,0)) }
        .otherwise { next_viu_fn(1) := io.seq_fn.viu }
      }
    }
    .otherwise
    {
      next_viu(0) := Bool(true)
      next_viu_fn(0) := io.seq_fn.viu
      next_viu_utidx(0) := io.seq_regid_imm.utidx
      next_viu_imm(0) := io.seq_regid_imm.imm

      when (io.seq_regid_imm.vs_zero) { next_viu_fn(0) := Cat(M0, io.seq_fn.viu(8,0)) }
    }
  }
  when (io.seq.vau0)
  {
    next_vau0(1) := Bool(true)
    next_vau0_fn(1) := io.seq_fn.vau0
  }
  when (io.seq.vau1)
  {
    when (FN_VAU1_FMA(io.seq_fn.vau1))
    {
      next_vau1(2) := Bool(true)
      next_vau1_fn(2) := io.seq_fn.vau1
    }
    .otherwise
    {
      next_vau1(1) := Bool(true)
      next_vau1_fn(1) := io.seq_fn.vau1
    }
  }
  when (io.seq.vau2)
  {
    next_vau2(0) := Bool(true)
    next_vau2_fn(0) := io.seq_fn.vau2
  }
  when (io.seq.vaq)
  {
    next_vaq(0) := Bool(true)
    next_vaq_mask(0) := io.seq_regid_imm.mask
    next_vaq_pop_cnt(0) := io.seq_regid_imm.pop_count
    next_mem(0) := io.seq_regid_imm.mem
    next_imm(0) := io.seq_regid_imm.imm
    next_imm2(0) := io.seq_regid_imm.imm2
    next_utmemop(0) := io.seq_regid_imm.utmemop
  }
  when (io.seq.vldq)
  {
    next_vldq(0) := Bool(true)
    next_vldq_mask(0) := io.seq_regid_imm.mask
  }
  when (io.seq.vsdq)
  {
    next_vsdq(0) := Bool(true)
    next_vsdq_mask(0) := io.seq_regid_imm.mask
    next_mem(0) := io.seq_regid_imm.mem
  }

  io.expand_to_hazard.ren := reg_ren(0)
  io.expand_to_hazard.wen := reg_wen(0)
  io.expand_to_hazard.wen_mask := reg_wen_mask(0)

  io.expand_read.ren := reg_ren(0)
  io.expand_read.rlast := reg_rlast(0)
  io.expand_read.rcnt := reg_rcnt(0)
  io.expand_read.raddr := reg_raddr(0)
  io.expand_read.roplen := reg_roplen(0)
  io.expand_read.rblen := reg_rblen(0).toBits
  io.expand_read.rmask := reg_rmask(0).toBits

  io.expand_write.wen := reg_wen(0)
  io.expand_write.wlast := reg_wlast(0)
  io.expand_write.wcnt := reg_wcnt(0)
  io.expand_write.waddr := reg_waddr(0)
  io.expand_write.wsel := reg_wsel(0)
  io.expand_write.wmask := reg_wmask(0)

  io.expand_write.wen_mask := reg_wen_mask(0)
  io.expand_write.wlast_mask := reg_wlast_mask(0)
  io.expand_write.wcnt_mask := reg_wcnt_mask(0)
  io.expand_write.wmask_mask := reg_wmask_mask(0)
  io.expand_write.waddr_mask := reg_waddr_mask(0)
  io.expand_write.pvfb_tag := reg_pvfb_tag(0)

  io.expand_fu_fn.viu := reg_viu(0)
  io.expand_fu_fn.viu_fn := reg_viu_fn(0)
  io.expand_fu_fn.viu_utidx := reg_viu_utidx(0)
  io.expand_fu_fn.viu_imm := reg_viu_imm(0)
  io.expand_lfu_fn.vau0 := reg_vau0(0)
  io.expand_lfu_fn.vau0_fn := reg_vau0_fn(0)
  io.expand_lfu_fn.vau1 := reg_vau1(0)
  io.expand_lfu_fn.vau1_fn := reg_vau1_fn(0)
  io.expand_lfu_fn.vau2 := reg_vau2(0)
  io.expand_lfu_fn.vau2_fn := reg_vau2_fn(0)
  io.expand_lfu_fn.mem := reg_mem(0)
  io.expand_lfu_fn.imm := reg_imm(0)
  io.expand_lfu_fn.imm2 := reg_imm2(0)
  io.expand_lfu_fn.vaq := reg_vaq(0)
  io.expand_lfu_fn.vaq_mask := reg_vaq_mask(0)
  io.expand_lfu_fn.vaq_pop_cnt := reg_vaq_pop_cnt(0)
  io.expand_lfu_fn.vldq := reg_vldq(0)
  io.expand_lfu_fn.vldq_mask := reg_vldq_mask(0)
  io.expand_lfu_fn.vsdq := reg_vsdq(0)
  io.expand_lfu_fn.vsdq_mask := reg_vsdq_mask(0)
  io.expand_lfu_fn.utmemop := reg_utmemop(0)

  io.expand_to_xcpt.empty := !reg_ren.toBits().orR() && !reg_wen.toBits().orR && !reg_viu.toBits().orR() && !reg_wen_mask.toBits().orR()
}

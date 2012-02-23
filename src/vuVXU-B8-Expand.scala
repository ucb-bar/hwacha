package hwacha

import Chisel._
import Node._
import Config._
import Interface._

class vuVXU_Banked8_Expand extends Component 
{
  val io = new io_vxu_expand

  val next_ren = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_rlast = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_rcnt = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_BVLEN)} }
  val next_raddr = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_BREGLEN)} }
  val next_roplen = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_BOPL)} }
  val next_rblen = GenBuf(SHIFT_BUF_READ){ GenArray(DEF_BRPORT){ Wire(){Bool()} } }

  val reg_ren = GenArray(SHIFT_BUF_READ){ Reg(resetVal=Bool(false))}
  val reg_rlast = GenArray(SHIFT_BUF_READ){ Reg(){ Bool() } }
  val reg_rcnt = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_BVLEN)} }
  val reg_raddr = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_BREGLEN)} }
  val reg_roplen = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_BOPL)} }
  val reg_rblen = GenBuf(SHIFT_BUF_READ){ GenArray(DEF_BRPORT){ Reg(){Bool()} } }

  for (i <- 0 until SHIFT_BUF_READ){
    reg_ren(i) := next_ren(i)
    reg_rlast(i) := next_rlast(i)
    reg_rcnt(i) := next_rcnt(i)
    reg_raddr(i) := next_raddr(i)
    reg_roplen(i) := next_roplen(i)
    for(j <- 0 until DEF_BRPORT)
      reg_rblen(i)(j) := next_rblen(i)(j)
  }

  for (i <- 0 until SHIFT_BUF_READ-1)
  {
    next_ren(i) := reg_ren(i+1)
    next_rlast(i) := reg_rlast(i+1)
    next_rcnt(i) := reg_rcnt(i+1)
    next_raddr(i) := reg_raddr(i+1)
    next_roplen(i) := reg_roplen(i+1)
    for(j <- 0 until DEF_BRPORT)
      next_rblen(i)(j) := reg_rblen(i+1)(j)
  }

  next_ren(SHIFT_BUF_READ-1) := Bool(false)
  next_rlast(SHIFT_BUF_READ-1) := Bool(false)
  next_rcnt(SHIFT_BUF_READ-1) := Bits("d0", 3)
  next_raddr(SHIFT_BUF_READ-1) := Bits("d0", 8)
  next_roplen(SHIFT_BUF_READ-1) := Bits("d0", 2)
  for(i <- 0 until DEF_BRPORT)
    next_rblen(SHIFT_BUF_READ-1)(i) := Bool(false)

  when (io.seq.viu) 
  {
    next_ren(0) := Bool(true)
    next_rlast(0) := io.seq_to_expand.last
    next_rcnt(0) := io.seq_regid_imm.cnt
    next_raddr(0) := io.seq_regid_imm.vs
    next_roplen(0) := Bits("b01", 2)
    for(i <- 0 until DEF_BRPORT)
      next_rblen(0)(i) := Bool(false)

    when (io.seq_fn.viu(RG_VIU_T) === Cat(ML,MR))
    {
      next_ren(1) := Bool(true)
      next_rlast(1) := io.seq_to_expand.last
      next_rcnt(1) := io.seq_regid_imm.cnt
      next_raddr(1) := io.seq_regid_imm.vt
      next_roplen(1) := Bits("b00", 2)
      for(i <- 0 until DEF_BRPORT)
        next_rblen(1)(i) := Bool(false)
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
    for(i <- 0 until DEF_BRPORT)
      next_rblen(0)(i) := Bool(false)
    
    next_ren(1) := Bool(true)
    next_rlast(1) := io.seq_to_expand.last
    next_rcnt(1) := io.seq_regid_imm.cnt
    next_raddr(1) := io.seq_regid_imm.vt
    next_roplen(1) := Bits("b00", 2)
    for(i <- 0 until DEF_BRPORT)
      if(i == 1 || i == 0) 
        next_rblen(1)(i) := Bool(true)
      else
        next_rblen(1)(i) := Bool(false)

    when (io.seq_regid_imm.vs_zero) { next_rblen(1)(0) := Bool(false) }
    when (io.seq_regid_imm.vt_zero) { next_rblen(1)(1) := Bool(false) }
  }
  when (io.seq.vau1)
  {
    when (io.seq_fn.vau1(2).toBool)
    {
      next_ren(0) := Bool(true)
      next_rlast(0) := io.seq_to_expand.last
      next_rcnt(0) := io.seq_regid_imm.cnt
      next_raddr(0) := io.seq_regid_imm.vs
      next_roplen(0) := Bits("b10", 2)
      for(i <- 0 until DEF_BRPORT)
        next_rblen(0)(i) := Bool(false)
        
      next_ren(1) := Bool(true)
      next_rlast(1) := io.seq_to_expand.last
      next_rcnt(1) := io.seq_regid_imm.cnt
      next_raddr(1) := io.seq_regid_imm.vt
      next_roplen(1) := Bits("b01", 2)
      for(i <- 0 until DEF_BRPORT)
        next_rblen(1)(i) := Bool(false)
      
      next_ren(2) := Bool(true)
      next_rlast(2) := io.seq_to_expand.last
      next_rcnt(2) := io.seq_regid_imm.cnt
      next_raddr(2) := io.seq_regid_imm.vr
      next_roplen(2) := Bits("b00", 2)
      for(i <- 0 until DEF_BRPORT)
        if(i == 2 || i == 3 || i == 4) 
          next_rblen(2)(i) := Bool(true)
        else
          next_rblen(2)(i) := Bool(false)


      when (io.seq_regid_imm.vs_zero) { next_rblen(2)(2) := Bool(false) }
      when (io.seq_regid_imm.vt_zero) { next_rblen(2)(3) := Bool(false) }
      when (io.seq_regid_imm.vr_zero) { next_rblen(2)(4) := Bool(false) }
    }
    when (!io.seq_fn.vau1(2).toBool)
    {
      next_ren(0) := Bool(true)
      next_rlast(0) := io.seq_to_expand.last
      next_rcnt(0) := io.seq_regid_imm.cnt
      next_raddr(0) := io.seq_regid_imm.vs
      next_roplen(0) := Bits("b10", 2)
      for(i <- 0 until DEF_BRPORT)
        next_rblen(0)(i) := Bool(false)
      
      next_ren(1) := Bool(true)
      next_rlast(1) := io.seq_to_expand.last
      next_rcnt(1) := io.seq_regid_imm.cnt
      next_raddr(1) := io.seq_regid_imm.vt
      next_roplen(1) := Bits("b00", 2)
      for(i <- 0 until DEF_BRPORT){
        if(i == 2 || i == 4)
          next_rblen(1)(i) := Bool(true)
        else
          next_rblen(1)(i) := Bool(false)
      }

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
    for(i <- 0 until DEF_BRPORT)
      if(i == 5)
        next_rblen(0)(i) := Bool(true)
      else
        next_rblen(0)(i) := Bool(false) 

    when (io.seq_regid_imm.vs_zero) { next_rblen(0)(5) := Bool(false) }
  }
  when (io.seq.vaq)
  {
    when (io.seq_regid_imm.utmemop)
    {
      next_ren(0) := Bool(true)
      next_rlast(0) := io.seq_to_expand.last
      next_rcnt(0) := io.seq_regid_imm.cnt
      next_raddr(0) := io.seq_regid_imm.vs
      next_roplen(0) := Bits("b00", 2)
      for(i <- 0 until DEF_BRPORT)
        if(i == 6)
        next_rblen(0)(i) := Bool(true)
      else
        next_rblen(0)(i) := Bool(false)

      when (io.seq_regid_imm.vs_zero) { next_rblen(0)(6) := Bool(false) }
    }
    .otherwise
    {
      next_ren(0) := Bool(true)
      next_rlast(0) := io.seq_to_expand.last
      next_rcnt(0) := io.seq_regid_imm.cnt
    }
  }
  when (io.seq.vsdq)
  {
    next_ren(0) := Bool(true)
    next_rlast(0) := io.seq_to_expand.last
    next_rcnt(0) := io.seq_regid_imm.cnt
    next_raddr(0) := io.seq_regid_imm.vt
    next_roplen(0) := Bits("b00", 2)
    for(i <- 0 until DEF_BRPORT)
      if(i == 7)
        next_rblen(0)(i) := Bool(true)
      else
        next_rblen(0)(i) := Bool(false)

    when (io.seq_regid_imm.vt_zero) { next_rblen(0)(7) := Bool(false) }
  }

  val next_wen = GenArray(SHIFT_BUF_WRITE){ Wire(){ Bool() } }
  val next_wlast = GenArray(SHIFT_BUF_WRITE){ Wire(){ Bool() } }
  val next_wcnt = GenArray(SHIFT_BUF_WRITE){ Wire(){Bits(width=DEF_BVLEN)} }
  val next_waddr = GenArray(SHIFT_BUF_WRITE){ Wire(){Bits(width=DEF_BREGLEN)} }
  val next_wsel = GenArray(SHIFT_BUF_WRITE){ Wire(){Bits(width=DEF_BWPORT)} }

  val reg_wen = GenArray(SHIFT_BUF_WRITE){ Reg(resetVal=Bool(false)) }
  val reg_wlast = GenArray(SHIFT_BUF_WRITE){ Reg(){ Bool() } }
  val reg_wcnt = GenArray(SHIFT_BUF_WRITE){ Reg(){Bits(width=DEF_BVLEN)} }
  val reg_waddr = GenArray(SHIFT_BUF_WRITE){ Reg(){Bits(width=DEF_BREGLEN)} }
  val reg_wsel = GenArray(SHIFT_BUF_WRITE){ Reg(){Bits(width=DEF_BWPORT)} }

  for (i <- 0 until SHIFT_BUF_WRITE)
  {
    reg_wen(i) := next_wen(i)
    reg_wlast(i) := next_wlast(i)
    reg_wcnt(i) := next_wcnt(i)
    reg_waddr(i) := next_waddr(i)
    reg_wsel(i) := next_wsel(i)
  }

  val viu_wptr = 
    Mux((io.seq_fn.viu(RG_VIU_T) === Cat(ML,MR)) , Bits(INT_STAGES + 2, DEF_BPTR1)
    , Bits(INT_STAGES + 1, DEF_BPTR1))

  val vau0_wptr =
    Bits(IMUL_STAGES + 2, DEF_BPTR1)

  val vau1_wptr =
    Mux((io.seq_fn.vau1(2)) , Bits(FMA_STAGES + 3, DEF_BPTR1)
    , Bits(FMA_STAGES + 2, DEF_BPTR1))

  val vau2_wptr = 
    Bits(FCONV_STAGES + 1,  DEF_BPTR1)

  for (i <- 0 until SHIFT_BUF_WRITE-1)
  {
    next_wen(i) := reg_wen(i+1)
    next_wlast(i) := reg_wlast(i+1)
    next_wcnt(i) := reg_wcnt(i+1)
    next_waddr(i) := reg_waddr(i+1)
    next_wsel(i) := reg_wsel(i+1)
  }

  next_wen(SHIFT_BUF_WRITE-1) := Bool(false)
  next_wlast(SHIFT_BUF_WRITE-1) := Bool(false)
  next_wcnt(SHIFT_BUF_WRITE-1) := Bits("d0", 3)
  next_waddr(SHIFT_BUF_WRITE-1) := Bits("d0", 8)
  next_wsel(SHIFT_BUF_WRITE-1) := Bits("d0", 3)
      
  when (io.seq.viu)
  {
    next_wen.write(viu_wptr, Bool(true))
    next_wlast.write(viu_wptr, io.seq_to_expand.last)
    next_wcnt.write(viu_wptr, io.seq_regid_imm.cnt)
    next_waddr.write(viu_wptr, io.seq_regid_imm.vd)
    next_wsel.write(viu_wptr, Bits("d4", 3))
  }
  when (io.seq.vau0)
  {
    next_wen.write(vau0_wptr, Bool(true))
    next_wlast.write(vau0_wptr, io.seq_to_expand.last)
    next_wcnt.write(vau0_wptr, io.seq_regid_imm.cnt)
    next_waddr.write(vau0_wptr, io.seq_regid_imm.vd)
    next_wsel.write(vau0_wptr, Bits("d0", 3))
  }
  when (io.seq.vau1)
  {
    next_wen.write(vau1_wptr, Bool(true))
    next_wlast.write(vau1_wptr, io.seq_to_expand.last)
    next_wcnt.write(vau1_wptr, io.seq_regid_imm.cnt)
    next_waddr.write(vau1_wptr, io.seq_regid_imm.vd)
    next_wsel.write(vau1_wptr, Bits("d1", 3))
  }
  when (io.seq.vau2)
  {
    next_wen.write(vau2_wptr, Bool(true))
    next_wlast.write(vau2_wptr, io.seq_to_expand.last)
    next_wcnt.write(vau2_wptr, io.seq_regid_imm.cnt)
    next_waddr.write(vau2_wptr, io.seq_regid_imm.vd)
    next_wsel.write(vau2_wptr, Bits("d2", 3))
  }
  when (io.seq.vldq)
  {
    next_wen(0) := Bool(true)
    next_wlast(0) := io.seq_to_expand.last
    next_wcnt(0) := io.seq_regid_imm.cnt
    next_waddr(0) := io.seq_regid_imm.vd
    next_wsel(0) := Bits("d3", 3)
  }

  val next_viu = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_viu_fn = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_VIU_FN)} }
  val next_viu_utidx = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_VLEN)} }
  val next_viu_imm = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_DATA)} }
  val next_vau0 = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_vau0_fn = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_VAU0_FN)} }
  val next_vau1 = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_vau1_fn = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_VAU1_FN)} }
  val next_vau2 = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_vau2_fn = GenArray(SHIFT_BUF_READ){ Wire(){Bits(width=DEF_VAU2_FN)} }
  val next_mem_cmd = GenArray(SHIFT_BUF_READ){ Wire(){ Bits(width=4) } }
  val next_mem_typ = GenArray(SHIFT_BUF_READ){ Wire(){ Bits(width=3) } }
  val next_mem_typ_float = GenArray(SHIFT_BUF_READ){ Wire(){ Bits(width=3) } }
  val next_imm = GenArray(SHIFT_BUF_READ){ Wire(){ Bits(width=DEF_DATA) } }
  val next_imm2 = GenArray(SHIFT_BUF_READ){ Wire(){ Bits(width=DEF_VXU_IMM2Q) } }
  val next_vaq = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_vldq = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_vsdq = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } }
  val next_utmemop = GenArray(SHIFT_BUF_READ){ Wire(){ Bool() } }

  val reg_viu = GenArray(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_viu_fn = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_VIU_FN)} }
  val reg_viu_utidx = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_VLEN)} }
  val reg_viu_imm = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_DATA)} }
  val reg_vau0 = GenArray(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_vau0_fn = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_VAU0_FN)} }
  val reg_vau1 = GenArray(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_vau1_fn = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_VAU1_FN)} }
  val reg_vau2 = GenArray(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_vau2_fn = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_VAU2_FN)} }
  val reg_mem_cmd = GenArray(SHIFT_BUF_READ){ Reg(){ Bits(width=4) } }
  val reg_mem_typ = GenArray(SHIFT_BUF_READ){ Reg(){ Bits(width=3) } }
  val reg_mem_typ_float = GenArray(SHIFT_BUF_READ){ Reg(){ Bits(width=1) } }
  val reg_imm = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_DATA)} }
  val reg_imm2 = GenArray(SHIFT_BUF_READ){ Reg(){Bits(width=DEF_VXU_IMM2Q)} }
  val reg_vaq = GenArray(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_vldq = GenArray(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_vsdq = GenArray(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }
  val reg_utmemop = GenArray(SHIFT_BUF_READ){ Reg(resetVal=Bool(false)) }

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
    reg_mem_cmd(i) := next_mem_cmd(i)
    reg_mem_typ(i) := next_mem_typ(i)
    reg_mem_typ_float(i) := next_mem_typ_float(i)
    reg_imm(i) := next_imm(i)
    reg_imm2(i) := next_imm2(i)
    reg_vaq(i) := next_vaq(i)
    reg_vldq(i) := next_vldq(i)
    reg_vsdq(i) := next_vsdq(i)
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
    next_mem_cmd(i) := reg_mem_cmd(i+1)
    next_mem_typ(i) := reg_mem_typ(i+1)
    next_mem_typ_float(i) := reg_mem_typ_float(i+1)
    next_imm(i) := reg_imm(i+1)
    next_imm2(i) := reg_imm2(i+1)
    next_vaq(i) := reg_vaq(i+1)
    next_vldq(i) := reg_vldq(i+1)
    next_vsdq(i) := reg_vsdq(i+1)
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
  next_mem_cmd(SHIFT_BUF_READ-1) := Bits("d0", 4)
  next_mem_typ(SHIFT_BUF_READ-1) := Bits("d0", 3)
  next_mem_typ_float(SHIFT_BUF_READ-1) := Bits("d0", 1)
  next_imm(SHIFT_BUF_READ-1) := Bits("d0", SZ_DATA)
  next_imm2(SHIFT_BUF_READ-1) := Bits("d0", DEF_VXU_IMM2Q)
  next_vaq(SHIFT_BUF_READ-1) := Bool(false)
  next_vldq(SHIFT_BUF_READ-1) := Bool(false)
  next_vsdq(SHIFT_BUF_READ-1) := Bool(false)
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
    when (io.seq_fn.vau1(2).toBool)
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
    next_mem_cmd(0) := io.seq_regid_imm.mem.cmd
    next_mem_typ(0) := io.seq_regid_imm.mem.typ
    next_mem_typ_float(0) := io.seq_regid_imm.mem.typ_float
    next_imm(0) := io.seq_regid_imm.imm
    next_imm2(0) := io.seq_regid_imm.imm2
    next_utmemop(0) := io.seq_regid_imm.utmemop
  }
  when (io.seq.vldq)
  {
    next_vldq(0) := Bool(true)
  }
  when (io.seq.vsdq)
  {
    next_vsdq(0) := Bool(true)
    when (io.seq_regid_imm.utmemop)
    {
      next_mem_cmd(0) := io.seq_regid_imm.mem.cmd
      next_mem_typ(0) := io.seq_regid_imm.mem.typ
      next_mem_typ_float(0) := io.seq_regid_imm.mem.typ_float
    }
  }

  io.expand_to_hazard.ren := reg_ren(0)
  io.expand_to_hazard.wen := reg_wen(0)

  io.expand_read.ren := reg_ren(0)
  io.expand_read.rlast := reg_rlast(0)
  io.expand_read.rcnt := reg_rcnt(0)
  io.expand_read.raddr := reg_raddr(0)
  io.expand_read.roplen := reg_roplen(0)
  io.expand_read.rblen := reg_rblen(0).flatten()

  io.expand_write.wen := reg_wen(0)
  io.expand_write.wlast := reg_wlast(0)
  io.expand_write.wcnt := reg_wcnt(0)
  io.expand_write.waddr := reg_waddr(0)
  io.expand_write.wsel := reg_wsel(0)

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
  io.expand_lfu_fn.mem.cmd := reg_mem_cmd(0)
  io.expand_lfu_fn.mem.typ := reg_mem_typ(0)
  io.expand_lfu_fn.mem.typ_float := reg_mem_typ_float(0)
  io.expand_lfu_fn.imm := reg_imm(0)
  io.expand_lfu_fn.imm2 := reg_imm2(0)
  io.expand_lfu_fn.vaq := reg_vaq(0)
  io.expand_lfu_fn.vldq := reg_vldq(0)
  io.expand_lfu_fn.vsdq := reg_vsdq(0)
  io.expand_lfu_fn.utmemop := reg_utmemop(0)
}

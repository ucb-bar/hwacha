package hwacha

import Chisel._
import Config._
import Interface._

class ExpanderToLFUIO extends Bundle
{
  val vau0    = Bool(OUTPUT)
  val vau0_fn = Bits(DEF_VAU0_FN, OUTPUT)
  val vau1    = Bool(OUTPUT)
  val vau1_fn = Bits(DEF_VAU1_FN, OUTPUT)
  val vau2    = Bool(OUTPUT)
  val vau2_fn = Bits(DEF_VAU2_FN, OUTPUT)
  val mem     = new io_vxu_mem_cmd().asOutput
  val imm     = Bits(DEF_DATA, OUTPUT)
  val imm2    = Bits(DEF_VXU_IMM2Q, OUTPUT)
  val vaq     = Bool(OUTPUT)
  val vldq    = Bool(OUTPUT)
  val vsdq    = Bool(OUTPUT)
  val utaq    = Bool(OUTPUT)
}

class LFUIO extends Bundle 
{
  val expand_rcnt = UFix(DEF_BVLEN, INPUT)
  val expand_wcnt = UFix(DEF_BVLEN, INPUT)
  
  val expand = new ExpanderToLFUIO().flip()

  val vau0_val  = Bool(OUTPUT)
  val vau0_fn   = Bits(DEF_VAU0_FN, OUTPUT)
  val vau1_val  = Bool(OUTPUT)
  val vau1_fn   = Bits(DEF_VAU1_FN, OUTPUT)
  val vau2_val  = Bool(OUTPUT)
  val vau2_fn   = Bits(DEF_VAU2_FN, OUTPUT)
  val mem = new io_vxu_mem_cmd().asOutput
  val imm = Bits(DEF_DATA, OUTPUT)
  val vaq_val   = Bool(OUTPUT)
  val vldq_rdy  = Bool(OUTPUT)
  val vsdq_val  = Bool(OUTPUT)
  val utaq_val  = Bool(OUTPUT)
}

class vuVXU_Banked8_Lane_LFU extends Component 
{
  val io = new LFUIO()

  val next_vau0_cnt = Wire(){ UFix(width = DEF_BVLEN) }
  val next_vau1_cnt = Wire(){ UFix(width = DEF_BVLEN) }
  val next_vau2_cnt = Wire(){ UFix(width = DEF_BVLEN) }
  val next_vgu_cnt = Wire(){ UFix(width = DEF_BVLEN) }
  val next_vlu_cnt = Wire(){ UFix(width = DEF_BVLEN) }
  val next_vsu_cnt = Wire(){ UFix(width = DEF_BVLEN) }

  val reg_vau0_cnt = Reg(resetVal = UFix(0, SZ_BVLEN))
  val reg_vau1_cnt = Reg(resetVal = UFix(0, SZ_BVLEN))
  val reg_vau2_cnt = Reg(resetVal = UFix(0, SZ_BVLEN))
  val reg_vgu_cnt  = Reg(resetVal = UFix(0, SZ_BVLEN))
  val reg_vlu_cnt  = Reg(resetVal = UFix(0, SZ_BVLEN))
  val reg_vsu_cnt  = Reg(resetVal = UFix(0, SZ_BVLEN))

  reg_vau0_cnt := next_vau0_cnt
  reg_vau1_cnt := next_vau1_cnt
  reg_vau2_cnt := next_vau2_cnt
  reg_vgu_cnt  := next_vgu_cnt
  reg_vlu_cnt  := next_vlu_cnt
  reg_vsu_cnt  := next_vsu_cnt

  next_vau0_cnt := UFix(0, SZ_BVLEN)
  next_vau1_cnt := UFix(0, SZ_BVLEN)
  next_vau2_cnt := UFix(0, SZ_BVLEN)
  next_vgu_cnt  := UFix(0, SZ_BVLEN)
  next_vlu_cnt  := UFix(0, SZ_BVLEN)
  next_vsu_cnt  := UFix(0, SZ_BVLEN)

  when (io.expand.vau0) { next_vau0_cnt := io.expand_rcnt}
  when (io.expand.vau1) { next_vau1_cnt := io.expand_rcnt}
  when (io.expand.vau2) { next_vau2_cnt := io.expand_rcnt}
  when (io.expand.vaq || io.expand.utaq) { next_vgu_cnt := io.expand_rcnt}
  when (io.expand.vldq) { next_vlu_cnt := io.expand_wcnt}
  when (io.expand.vsdq) { next_vsu_cnt := io.expand_rcnt}
  
  when (reg_vau0_cnt.orR) { next_vau0_cnt := reg_vau0_cnt - UFix(1,1)}
  when (reg_vau1_cnt.orR) { next_vau1_cnt := reg_vau1_cnt - UFix(1,1)}
  when (reg_vau2_cnt.orR) { next_vau2_cnt := reg_vau2_cnt - UFix(1,1)}
  when (reg_vgu_cnt.orR) { next_vgu_cnt := reg_vgu_cnt - UFix(1,1)}
  when (reg_vlu_cnt.orR) { next_vlu_cnt := reg_vlu_cnt - UFix(1,1)}
  when (reg_vsu_cnt.orR) { next_vsu_cnt := reg_vsu_cnt - UFix(1,1)}

  val reg_vau0    = Reg(resetVal = Bool(false))
  val reg_vau0_fn = Reg(){ Bits(width = DEF_VAU0_FN) }
  val reg_vau1    = Reg(resetVal = Bool(false))
  val reg_vau1_fn = Reg(){ Bits(width = DEF_VAU1_FN) }
  val reg_vau2    = Reg(resetVal = Bool(false))
  val reg_vau2_fn = Reg(){ Bits(width = DEF_VAU2_FN) }
  val reg_mem     = Reg(){ new io_vxu_mem_cmd() }
  val reg_imm     = Reg(){ Bits(width = DEF_DATA) }
  val reg_imm2    = Reg(){ Bits(width = DEF_VXU_IMM2Q) }
  val reg_vaq     = Reg(resetVal = Bool(false))
  val reg_vldq    = Reg(resetVal = Bool(false))
  val reg_vsdq    = Reg(resetVal = Bool(false))
  val reg_utaq    = Reg(resetVal = Bool(false))

  when (io.expand.vau0)
  {
    reg_vau0 := Bool(true)
    reg_vau0_fn := io.expand.vau0_fn
  }
  when (!io.expand.vau0 && ~(reg_vau0_cnt.orR))
  {
    reg_vau0 := Bool(false)
  }

  when (io.expand.vau1)
  {
    reg_vau1 := Bool(true)
    reg_vau1_fn := io.expand.vau1_fn
  }
  when (!io.expand.vau1 && ~(reg_vau1_cnt.orR))
  {
    reg_vau1 := Bool(false)
  }
  
  when (io.expand.vau2)
  {
    reg_vau2 := Bool(true)
    reg_vau2_fn := io.expand.vau2_fn
  }
  when (!io.expand.vau2 && ~(reg_vau2_cnt.orR))
  {
    reg_vau2 := Bool(false)
  }

  when (io.expand.utaq)
  {
    reg_utaq := Bool(true)
    reg_mem := io.expand.mem
    reg_imm := io.expand.imm
  }
  when (!io.expand.utaq && ~(reg_vgu_cnt.orR))
  {
    reg_utaq := Bool(false)
  }

  when (io.expand.vaq)
  {
    reg_vaq := Bool(true)
    reg_mem := io.expand.mem
    reg_imm := io.expand.imm
    reg_imm2 := io.expand.imm2
  }
  when (reg_vaq)
  {
    reg_imm := reg_imm.toUFix + reg_imm2.toUFix
  }
  when (!io.expand.vaq && ~(reg_vgu_cnt.orR))
  {
    reg_vaq := Bool(false)
  }

  when ((io.expand.vldq) && (io.expand_wcnt.orR))
  {
    reg_vldq := io.expand.vldq
  }
  when (!((io.expand.vldq) && (io.expand_wcnt.orR)) && ~(next_vlu_cnt.orR))
  {
    reg_vldq := Bool(false)
  }

  when (io.expand.vsdq)
  {
    reg_vsdq := io.expand.vsdq
  }
  when (!(io.expand.vsdq) && ~(reg_vsu_cnt.orR))
  {
    reg_vsdq := Bool(false)
  }

  // every signal related to the read port is delayed by one cycle 
  // because of the register file is an sram
  // for this reason vldq_rdy needs to be bypassed
  // and count down using the next_vlu_cnt signal

  io.vau0_val  := reg_vau0
  io.vau0_fn   := reg_vau0_fn
  io.vau1_val  := reg_vau1
  io.vau1_fn   := reg_vau1_fn
  io.vau2_val  := reg_vau2
  io.vau2_fn   := reg_vau2_fn
  io.mem <> reg_mem
  io.imm := reg_imm
  io.utaq_val  := reg_utaq
  io.vaq_val   := reg_vaq
  io.vldq_rdy  := io.expand.vldq | reg_vldq
  io.vsdq_val  := reg_vsdq
}

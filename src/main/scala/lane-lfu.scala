package hwacha

import Chisel._
import Constants._

class LaneLFU extends Module 
{
  val io = new Bundle {
    val uop = new LfuncUopIO().flip

    val vau0_val = Bool(OUTPUT)
    val vau0_fn = Bits(OUTPUT, SZ_VAU0_FN)
    val vau1_val = Bool(OUTPUT)
    val vau1_fn = Bits(OUTPUT, SZ_VAU1_FN)
    val vau2_val = Bool(OUTPUT)
    val vau2_fn = Bits(OUTPUT, SZ_VAU2_FN)
    val vaq_val = Bool(OUTPUT)
    val vaq_check = new io_vxu_mem_check().asOutput
    val vaq_mem = new io_vxu_mem_cmd().asOutput
    val vaq_imm = Bits(OUTPUT, SZ_DATA)
    val vaq_utmemop = Bool(OUTPUT)
    val vldq_rdy = Bool(OUTPUT)
    val vsdq_val = Bool(OUTPUT)
    val vsdq_mem = new io_vxu_mem_cmd().asOutput
  }

  val vau0_uop = Reg(Valid(new LfuncUopVAU0).asDirectionless)
  when (vau0_uop.bits.cnt.orR) {
    vau0_uop.bits.cnt := vau0_uop.bits.cnt - UInt(1)
  }
  when (vau0_uop.bits.cnt === UInt(1)) {
    vau0_uop.valid := Bool(false)
  }
  when (io.uop.vau0.valid) {
    vau0_uop.valid := Bool(true)
    vau0_uop.bits.cnt := io.uop.vau0.bits.cnt
    vau0_uop.bits.fn := io.uop.vau0.bits.fn
  }

  val vau1_uop = Reg(Valid(new LfuncUopVAU1).asDirectionless)
  when (vau1_uop.bits.cnt.orR) {
    vau1_uop.bits.cnt := vau1_uop.bits.cnt - UInt(1)
  }
  when (vau1_uop.bits.cnt === UInt(1)) {
    vau1_uop.valid := Bool(false)
  }
  when (io.uop.vau1.valid) {
    vau1_uop.valid := Bool(true)
    vau1_uop.bits.cnt := io.uop.vau1.bits.cnt
    vau1_uop.bits.fn := io.uop.vau1.bits.fn
  }

  val vau2_uop = Reg(Valid(new LfuncUopVAU2).asDirectionless)
  when (vau2_uop.bits.cnt.orR) {
    vau2_uop.bits.cnt := vau2_uop.bits.cnt - UInt(1)
  }
  when (vau2_uop.bits.cnt === UInt(1)) {
    vau2_uop.valid := Bool(false)
  }
  when (io.uop.vau2.valid) {
    vau2_uop.valid := Bool(true)
    vau2_uop.bits.cnt := io.uop.vau2.bits.cnt
    vau2_uop.bits.fn := io.uop.vau2.bits.fn
  }

  val vgu_checkcnt = Reg(init=Bool(false))
  val vgu_cnt = Reg(UInt(width = SZ_BCNT))
  val vgu_uop = Reg(Valid(new LfuncUopVGU).asDirectionless)
  when (vgu_uop.bits.cnt.orR) {
    vgu_uop.bits.cnt := vgu_uop.bits.cnt - UInt(1)
  }
  when (vgu_uop.bits.cnt === UInt(1)) {
    vgu_uop.valid := Bool(false)
  }
  when (vgu_uop.valid && !vgu_uop.bits.utmemop) {
    vgu_uop.bits.imm := vgu_uop.bits.imm + vgu_uop.bits.imm2
  }
  vgu_checkcnt := Bool(false)
  when (io.uop.vgu.valid) {
    vgu_uop.valid := Bool(true)
    vgu_uop.bits.cnt := io.uop.vgu.bits.cnt
    vgu_uop.bits.mem := io.uop.vgu.bits.mem
    vgu_uop.bits.imm := io.uop.vgu.bits.imm
    vgu_uop.bits.imm2 := io.uop.vgu.bits.imm2
    vgu_uop.bits.utmemop := io.uop.vgu.bits.utmemop
    vgu_checkcnt := Bool(true)
    vgu_cnt := io.uop.vgu.bits.cnt
  }

  val vlu_uop = Reg(Valid(new LfuncUopVLU).asDirectionless)
  when (vlu_uop.bits.cnt.orR) {
    vlu_uop.bits.cnt := vlu_uop.bits.cnt - UInt(1)
  }
  //FIXME
  when (vlu_uop.bits.cnt <= UInt(2)) {
    vlu_uop.valid := Bool(false)
  }
  when (io.uop.vlu.valid & io.uop.vlu.bits.cnt.orR) {
    vlu_uop.valid := Bool(true)
  }
  when (io.uop.vlu.valid) {
    vlu_uop.bits.cnt := io.uop.vlu.bits.cnt
  }

  val vsu_uop = Reg(Valid(new LfuncUopVSU).asDirectionless)
  when (vsu_uop.bits.cnt.orR) {
    vsu_uop.bits.cnt := vsu_uop.bits.cnt - UInt(1)
  }
  when (vsu_uop.bits.cnt === UInt(1)) {
    vsu_uop.valid := Bool(false)
  }
  when (io.uop.vsu.valid) {
    vsu_uop.valid := Bool(true)
    vsu_uop.bits.cnt := io.uop.vsu.bits.cnt
    vsu_uop.bits.mem := io.uop.vsu.bits.mem
  }

  when (this.reset) {
    vau0_uop.valid := Bool(false)
    vau1_uop.valid := Bool(false)
    vau2_uop.valid := Bool(false)
    vgu_uop.valid := Bool(false)
    vlu_uop.valid := Bool(false)
    vsu_uop.valid := Bool(false)
  }

  // every signal related to the read port is delayed by one cycle 
  // because of the register file is an sram
  // for this reason vldq_rdy needs to be bypassed
  // and count down using the next_vlu_cnt signal

  io.vau0_val := vau0_uop.valid
  io.vau0_fn := vau0_uop.bits.fn
  io.vau1_val := vau1_uop.valid
  io.vau1_fn := vau1_uop.bits.fn
  io.vau2_val := vau2_uop.valid
  io.vau2_fn := vau2_uop.bits.fn
  io.vaq_val := vgu_uop.valid
  io.vaq_check.checkcnt := vgu_checkcnt
  io.vaq_check.cnt := vgu_cnt
  io.vaq_mem <> vgu_uop.bits.mem
  io.vaq_imm := vgu_uop.bits.imm
  io.vaq_utmemop := vgu_uop.bits.utmemop
  io.vldq_rdy := io.uop.vlu.valid | vlu_uop.valid
  io.vsdq_val := vsu_uop.valid
  io.vsdq_mem <> vsu_uop.bits.mem
}

package hwacha

import Chisel._
import Constants._

class LaneLFU extends Module 
{
  val io = new Bundle {
    val uop = new LaneFUOpIO().flip

    val vau0_val = Bool(OUTPUT)
    val vau0_fn = Bits(OUTPUT, SZ_VAU0_FN)
    val vau1_val = Bool(OUTPUT)
    val vau1_fn = Bits(OUTPUT, SZ_VAU1_FN)
    val vau2_val = Bool(OUTPUT)
    val vau2_fn = Bits(OUTPUT, SZ_VAU2_FN)

    val memop = new LaneMemOpIO
  }

  val vau0_uop = Reg(Valid(new VAU0LaneFUOp).asDirectionless)
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

  val vau1_uop = Reg(Valid(new VAU1LaneFUOp).asDirectionless)
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

  val vau2_uop = Reg(Valid(new VAU2LaneFUOp).asDirectionless)
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

  val vgu_uop = Reg(Valid(new VGULaneFUOp).asDirectionless)
  when (vgu_uop.bits.cnt.orR) {
    vgu_uop.bits.cnt := vgu_uop.bits.cnt - UInt(1)
  }
  when (vgu_uop.bits.cnt === UInt(1)) {
    vgu_uop.valid := Bool(false)
  }
  when (vgu_uop.valid && !vgu_uop.bits.utmemop) {
    vgu_uop.bits.imm := vgu_uop.bits.imm + vgu_uop.bits.imm2
  }
  vgu_uop.bits.check.checkcnt := Bool(false)
  when (io.uop.vgu.valid) {
    vgu_uop.valid := Bool(true)
    vgu_uop.bits.cnt := io.uop.vgu.bits.cnt
    vgu_uop.bits.mem := io.uop.vgu.bits.mem
    vgu_uop.bits.imm := io.uop.vgu.bits.imm
    vgu_uop.bits.imm2 := io.uop.vgu.bits.imm2
    vgu_uop.bits.utmemop := io.uop.vgu.bits.utmemop
    vgu_uop.bits.check.checkcnt := Bool(true)
    vgu_uop.bits.check.cnt := io.uop.vgu.bits.cnt
  }

  val vlu_uop = Reg(Valid(new VLULaneFUOp).asDirectionless)
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
    vlu_uop.bits.mem := io.uop.vlu.bits.mem
  }

  val vsu_uop = Reg(Valid(new VSULaneFUOp).asDirectionless)
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

  io.memop.vgu <> vgu_uop
  io.memop.vlu.valid := io.uop.vlu.valid | vlu_uop.valid
  io.memop.vlu.bits.mem := Mux(io.uop.vlu.valid, io.uop.vlu.bits.mem, vlu_uop.bits.mem)
  io.memop.vsu <> vsu_uop
}

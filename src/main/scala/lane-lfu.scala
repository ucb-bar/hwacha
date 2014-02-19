package hwacha

import Chisel._
import Constants._

class LaneLFU extends Module 
{
  val io = new Bundle {
    val op = new LaneFUOpIO().flip

    val vau0_val = Bool(OUTPUT)
    val vau0_fn = Bits(OUTPUT, SZ_VAU0_FN)
    val vau1_val = Bool(OUTPUT)
    val vau1_fn = Bits(OUTPUT, SZ_VAU1_FN)
    val vau2_val = Bool(OUTPUT)
    val vau2_fn = Bits(OUTPUT, SZ_VAU2_FN)

    val memop = new LaneMemOpIO
  }

  val vau0_op = Reg(Valid(new VAU0Op).asDirectionless)
  when (vau0_op.bits.cnt.orR) {
    vau0_op.bits.cnt := vau0_op.bits.cnt - UInt(1)
  }
  when (vau0_op.bits.cnt === UInt(1)) {
    vau0_op.valid := Bool(false)
  }
  when (io.op.vau0.valid) {
    vau0_op.valid := Bool(true)
    vau0_op.bits.cnt := io.op.vau0.bits.cnt
    vau0_op.bits.fn := io.op.vau0.bits.fn
  }

  val vau1_op = Reg(Valid(new VAU1Op).asDirectionless)
  when (vau1_op.bits.cnt.orR) {
    vau1_op.bits.cnt := vau1_op.bits.cnt - UInt(1)
  }
  when (vau1_op.bits.cnt === UInt(1)) {
    vau1_op.valid := Bool(false)
  }
  when (io.op.vau1.valid) {
    vau1_op.valid := Bool(true)
    vau1_op.bits.cnt := io.op.vau1.bits.cnt
    vau1_op.bits.fn := io.op.vau1.bits.fn
  }

  val vau2_op = Reg(Valid(new VAU2Op).asDirectionless)
  when (vau2_op.bits.cnt.orR) {
    vau2_op.bits.cnt := vau2_op.bits.cnt - UInt(1)
  }
  when (vau2_op.bits.cnt === UInt(1)) {
    vau2_op.valid := Bool(false)
  }
  when (io.op.vau2.valid) {
    vau2_op.valid := Bool(true)
    vau2_op.bits.cnt := io.op.vau2.bits.cnt
    vau2_op.bits.fn := io.op.vau2.bits.fn
  }

  val vgu_op = Reg(Valid(new VGUOp).asDirectionless)
  when (vgu_op.bits.cnt.orR) {
    vgu_op.bits.cnt := vgu_op.bits.cnt - UInt(1)
  }
  when (vgu_op.bits.cnt === UInt(1)) {
    vgu_op.valid := Bool(false)
  }
  when (vgu_op.valid && !vgu_op.bits.utmemop) {
    vgu_op.bits.imm := vgu_op.bits.imm + vgu_op.bits.imm2
  }
  vgu_op.bits.check.checkcnt := Bool(false)
  when (io.op.vgu.valid) {
    vgu_op.valid := Bool(true)
    vgu_op.bits.cnt := io.op.vgu.bits.cnt
    vgu_op.bits.mem := io.op.vgu.bits.mem
    vgu_op.bits.imm := io.op.vgu.bits.imm
    vgu_op.bits.imm2 := io.op.vgu.bits.imm2
    vgu_op.bits.utmemop := io.op.vgu.bits.utmemop
    vgu_op.bits.check.checkcnt := Bool(true)
    vgu_op.bits.check.cnt := io.op.vgu.bits.cnt
  }

  val vlu_op = Reg(Valid(new VLUOp).asDirectionless)
  when (vlu_op.bits.cnt.orR) {
    vlu_op.bits.cnt := vlu_op.bits.cnt - UInt(1)
  }
  //FIXME
  when (vlu_op.bits.cnt <= UInt(2)) {
    vlu_op.valid := Bool(false)
  }
  when (io.op.vlu.valid & io.op.vlu.bits.cnt.orR) {
    vlu_op.valid := Bool(true)
  }
  when (io.op.vlu.valid) {
    vlu_op.bits.cnt := io.op.vlu.bits.cnt
    vlu_op.bits.mem := io.op.vlu.bits.mem
  }

  val vsu_op = Reg(Valid(new VSUOp).asDirectionless)
  when (vsu_op.bits.cnt.orR) {
    vsu_op.bits.cnt := vsu_op.bits.cnt - UInt(1)
  }
  when (vsu_op.bits.cnt === UInt(1)) {
    vsu_op.valid := Bool(false)
  }
  when (io.op.vsu.valid) {
    vsu_op.valid := Bool(true)
    vsu_op.bits.cnt := io.op.vsu.bits.cnt
    vsu_op.bits.mem := io.op.vsu.bits.mem
  }

  when (this.reset) {
    vau0_op.valid := Bool(false)
    vau1_op.valid := Bool(false)
    vau2_op.valid := Bool(false)
    vgu_op.valid := Bool(false)
    vlu_op.valid := Bool(false)
    vsu_op.valid := Bool(false)
  }

  // every signal related to the read port is delayed by one cycle 
  // because of the register file is an sram
  // for this reason vldq_rdy needs to be bypassed
  // and count down using the next_vlu_cnt signal

  io.vau0_val := vau0_op.valid
  io.vau0_fn := vau0_op.bits.fn
  io.vau1_val := vau1_op.valid
  io.vau1_fn := vau1_op.bits.fn
  io.vau2_val := vau2_op.valid
  io.vau2_fn := vau2_op.bits.fn

  io.memop.vgu <> vgu_op
  io.memop.vlu.valid := io.op.vlu.valid | vlu_op.valid
  io.memop.vlu.bits.mem := Mux(io.op.vlu.valid, io.op.vlu.bits.mem, vlu_op.bits.mem)
  io.memop.vsu <> vsu_op
}

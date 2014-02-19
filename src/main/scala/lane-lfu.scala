package hwacha

import Chisel._
import Constants._

class LaneLFU extends Module 
{
  val io = new Bundle {
    val op = new LaneFUOpIO().flip

    val vau0 = Valid(new VAU0Op)
    val vau1 = Valid(new VAU1Op)
    val vau2 = Valid(new VAU2Op)
    val mem = new LaneMemOpIO
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
    vau0_op.bits := io.op.vau0.bits
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
    vau1_op.bits := io.op.vau1.bits
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
    vau2_op.bits := io.op.vau2.bits
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
    vgu_op.bits.fn := io.op.vgu.bits.fn
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
  when (vlu_op.bits.cnt === UInt(1)) {
    vlu_op.valid := Bool(false)
  }
  // slightly different because VLUOp is bypassed
  // every signal related to the read port is delayed by one cycle 
  // because of the register file is an sram
  when (io.op.vlu.valid && io.op.vlu.bits.cnt > UInt(1)) {
    vlu_op.valid := Bool(true)
    vlu_op.bits.cnt := io.op.vlu.bits.cnt - UInt(1)
    vlu_op.bits.fn := io.op.vlu.bits.fn
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
    vsu_op.bits := io.op.vsu.bits
  }

  when (this.reset) {
    vau0_op.valid := Bool(false)
    vau1_op.valid := Bool(false)
    vau2_op.valid := Bool(false)
    vgu_op.valid := Bool(false)
    vlu_op.valid := Bool(false)
    vsu_op.valid := Bool(false)
  }

  io.vau0 <> vau0_op
  io.vau1 <> vau1_op
  io.vau2 <> vau2_op
  io.mem.vgu <> vgu_op
  io.mem.vlu.valid := io.op.vlu.valid | vlu_op.valid
  io.mem.vlu.bits.fn := Mux(io.op.vlu.valid, io.op.vlu.bits.fn, vlu_op.bits.fn)
  io.mem.vsu <> vsu_op
}

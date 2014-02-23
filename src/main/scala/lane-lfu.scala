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
  when (io.op.vau0.valid && io.op.vau0.bits.cnt > UInt(1)) {
    vau0_op.valid := Bool(true)
    vau0_op.bits.cnt := io.op.vau0.bits.cnt - UInt(1)
    vau0_op.bits.fn := io.op.vau0.bits.fn
  }

  val vau1_op = Reg(Valid(new VAU1Op).asDirectionless)
  when (vau1_op.bits.cnt.orR) {
    vau1_op.bits.cnt := vau1_op.bits.cnt - UInt(1)
  }
  when (vau1_op.bits.cnt === UInt(1)) {
    vau1_op.valid := Bool(false)
  }
  when (io.op.vau1.valid && io.op.vau1.bits.cnt > UInt(1)) {
    vau1_op.valid := Bool(true)
    vau1_op.bits.cnt := io.op.vau1.bits.cnt - UInt(1)
    vau1_op.bits.fn := io.op.vau1.bits.fn
  }

  val vau2_op = Reg(Valid(new VAU2Op).asDirectionless)
  when (vau2_op.bits.cnt.orR) {
    vau2_op.bits.cnt := vau2_op.bits.cnt - UInt(1)
  }
  when (vau2_op.bits.cnt === UInt(1)) {
    vau2_op.valid := Bool(false)
  }
  when (io.op.vau2.valid && io.op.vau2.bits.cnt > UInt(1)) {
    vau2_op.valid := Bool(true)
    vau2_op.bits.cnt := io.op.vau2.bits.cnt - UInt(1)
    vau2_op.bits.fn := io.op.vau2.bits.fn
  }

  val vgu_base = Bits()
  val vgu_stride = Bits()
  vgu_base := io.op.vgu.bits.base
  vgu_stride := Bits(0)

  val vgu_op = Reg(Valid(new VGUOp).asDirectionless)
  when (vgu_op.bits.cnt.orR) {
    vgu_op.bits.cnt := vgu_op.bits.cnt - UInt(1)
  }
  when (vgu_op.bits.cnt === UInt(1)) {
    vgu_op.valid := Bool(false)
  }
  when (vgu_op.valid && !vgu_op.bits.fn.utmemop()) {
    vgu_base := vgu_op.bits.base
    vgu_stride := vgu_op.bits.stride
  }
  when (io.op.vgu.valid && io.op.vgu.bits.cnt > UInt(1)) {
    vgu_op.valid := Bool(true)
    vgu_op.bits.cnt := io.op.vgu.bits.cnt - UInt(1)
    vgu_op.bits.fn := io.op.vgu.bits.fn
    when (!io.op.vgu.bits.fn.utmemop()) { vgu_stride := io.op.vgu.bits.stride }
    vgu_op.bits.stride := io.op.vgu.bits.stride
    vgu_op.bits.check.checkcnt := Bool(false)
    vgu_op.bits.check.cnt := io.op.vgu.bits.cnt
  }
  vgu_op.bits.base := vgu_base + vgu_stride

  val vlu_op = Reg(Valid(new VLUOp).asDirectionless)
  when (vlu_op.bits.cnt.orR) {
    vlu_op.bits.cnt := vlu_op.bits.cnt - UInt(1)
  }
  when (vlu_op.bits.cnt === UInt(1)) {
    vlu_op.valid := Bool(false)
  }
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
  when (io.op.vsu.valid && io.op.vsu.bits.cnt > UInt(1)) {
    vsu_op.valid := Bool(true)
    vsu_op.bits.cnt := io.op.vsu.bits.cnt - UInt(1)
    vsu_op.bits.fn := io.op.vsu.bits.fn
  }

  when (this.reset) {
    vau0_op.valid := Bool(false)
    vau1_op.valid := Bool(false)
    vau2_op.valid := Bool(false)
    vgu_op.valid := Bool(false)
    vlu_op.valid := Bool(false)
    vsu_op.valid := Bool(false)
  }

  def bypass[T<:Data](result: ValidIO[T], op: ValidIO[T], reg_op: ValidIO[T]) = {
    result.valid := op.valid | reg_op.valid
    result.bits := Mux(op.valid, op.bits, reg_op.bits)
  }

  bypass(io.vau0, io.op.vau0, vau0_op)
  bypass(io.vau1, io.op.vau1, vau1_op)
  bypass(io.vau2, io.op.vau2, vau2_op)
  bypass(io.mem.vgu, io.op.vgu, vgu_op)
  bypass(io.mem.vlu, io.op.vlu, vlu_op)
  bypass(io.mem.vsu, io.op.vsu, vsu_op)
}

package hwacha

import Chisel._
import Constants._

class LaneLFU extends Module 
{
  val io = new Bundle {
    val op = new LaneFUOpIO().flip
    val vau0 = Valid(new VAU0Op)
    val vau1t = Valid(new VAU1Op)
    val vau1f = Valid(new VAU1Op)
    val vau2t = Valid(new VAU2Op)
    val vau2f = Valid(new VAU2Op)
    val vgu = Valid(new VGUOp)
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

  val vau1t_op = Reg(Valid(new VAU1Op).asDirectionless)
  when (vau1t_op.bits.cnt.orR) {
    vau1t_op.bits.cnt := vau1t_op.bits.cnt - UInt(1)
  }
  when (vau1t_op.bits.cnt === UInt(1)) {
    vau1t_op.valid := Bool(false)
  }
  when (io.op.vau1t.valid && io.op.vau1t.bits.cnt > UInt(1)) {
    vau1t_op.valid := Bool(true)
    vau1t_op.bits.cnt := io.op.vau1t.bits.cnt - UInt(1)
    vau1t_op.bits.fn := io.op.vau1t.bits.fn
  }

  val vau1f_op = Reg(Valid(new VAU1Op).asDirectionless)
  when (vau1f_op.bits.cnt.orR) {
    vau1f_op.bits.cnt := vau1f_op.bits.cnt - UInt(1)
  }
  when (vau1f_op.bits.cnt === UInt(1)) {
    vau1f_op.valid := Bool(false)
  }
  when (io.op.vau1f.valid && io.op.vau1f.bits.cnt > UInt(1)) {
    vau1f_op.valid := Bool(true)
    vau1f_op.bits.cnt := io.op.vau1f.bits.cnt - UInt(1)
    vau1f_op.bits.fn := io.op.vau1f.bits.fn
  }

  val vau2t_op = Reg(Valid(new VAU2Op).asDirectionless)
  when (vau2t_op.bits.cnt.orR) {
    vau2t_op.bits.cnt := vau2t_op.bits.cnt - UInt(1)
  }
  when (vau2t_op.bits.cnt === UInt(1)) {
    vau2t_op.valid := Bool(false)
  }
  when (io.op.vau2t.valid && io.op.vau2t.bits.cnt > UInt(1)) {
    vau2t_op.valid := Bool(true)
    vau2t_op.bits.cnt := io.op.vau2t.bits.cnt - UInt(1)
    vau2t_op.bits.fn := io.op.vau2t.bits.fn
    vau2t_op.bits.utidx := io.op.vau2t.bits.utidx
  }

  val vau2f_op = Reg(Valid(new VAU2Op).asDirectionless)
  when (vau2f_op.bits.cnt.orR) {
    vau2f_op.bits.cnt := vau2f_op.bits.cnt - UInt(1)
  }
  when (vau2f_op.bits.cnt === UInt(1)) {
    vau2f_op.valid := Bool(false)
  }
  when (io.op.vau2f.valid && io.op.vau2f.bits.cnt > UInt(1)) {
    vau2f_op.valid := Bool(true)
    vau2f_op.bits.cnt := io.op.vau2f.bits.cnt - UInt(1)
    vau2f_op.bits.fn := io.op.vau2f.bits.fn
    vau2f_op.bits.utidx := UInt(0) // FIXME
  }

  val vgu_op = Reg(Valid(new VGUOp).asDirectionless)
  when (vgu_op.bits.cnt.orR) {
    vgu_op.bits.cnt := vgu_op.bits.cnt - UInt(1)
  }
  when (vgu_op.bits.cnt === UInt(1)) {
    vgu_op.valid := Bool(false)
  }
  when (io.op.vgu.valid && io.op.vgu.bits.cnt > UInt(1)) {
    vgu_op.valid := Bool(true)
    vgu_op.bits.cnt := io.op.vgu.bits.cnt - UInt(1)
    vgu_op.bits.fn := io.op.vgu.bits.fn
    vgu_op.bits.base := io.op.vgu.bits.base
  }

  when (this.reset) {
    vau0_op.valid := Bool(false)
    vau1t_op.valid := Bool(false)
    vau1f_op.valid := Bool(false)
    vau2t_op.valid := Bool(false)
    vau2f_op.valid := Bool(false)
    vgu_op.valid := Bool(false)
  }

  def bypass[T<:Data](result: ValidIO[T], op: ValidIO[T], reg_op: ValidIO[T]) = {
    result.valid := op.valid | reg_op.valid
    result.bits := Mux(op.valid, op.bits, reg_op.bits)
  }

  bypass(io.vau0, io.op.vau0, vau0_op)
  bypass(io.vau1t, io.op.vau1t, vau1t_op)
  bypass(io.vau1f, io.op.vau1f, vau1f_op)
  bypass(io.vau2t, io.op.vau2t, vau2t_op)
  bypass(io.vau2f, io.op.vau2f, vau2f_op)
  bypass(io.vgu, io.op.vgu, vgu_op)
}

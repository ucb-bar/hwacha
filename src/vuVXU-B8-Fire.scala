package hwacha

import Chisel._
import Node._
import Constants._

class vuVXU_Banked8_Fire extends Component
{
  val io = new io_vxu_fire()

  val fire_tvec_viu = ~io.tvec_regid_imm.vd_zero & io.tvec_valid.viu & io.tvec_ready
  val fire_tvec_vld = ~io.tvec_regid_imm.vd_zero & io.tvec_valid.vld & io.tvec_ready
  val fire_tvec_vst = ~io.tvec_regid_imm.vd_zero & io.tvec_valid.vst & io.tvec_ready
  val fire_tvec = fire_tvec_viu | fire_tvec_vld | fire_tvec_vst

  val fire_vt_viu = ~io.vt_regid_imm.vd_zero & io.vt_valid.viu & io.vt_ready
  val fire_vt_vau0 = ~io.vt_regid_imm.vd_zero & io.vt_valid.vau0 & io.vt_ready
  val fire_vt_vau1 = ~io.vt_regid_imm.vd_zero & io.vt_valid.vau1 & io.vt_ready
  val fire_vt_vau2 = ~io.vt_regid_imm.vd_zero & io.vt_valid.vau2 & io.vt_ready
  val fire_vt_amo = ~io.vt_regid_imm.vd_zero & io.vt_valid.amo & io.vt_ready
  val fire_vt_utld = ~io.vt_regid_imm.vd_zero & io.vt_valid.utld & io.vt_ready
  val fire_vt_utst = ~io.vt_regid_imm.vd_zero & io.vt_valid.utst & io.vt_ready

  io.fire.viu := fire_tvec_viu | fire_vt_viu
  io.fire.vau0 := fire_vt_vau0
  io.fire.vau1 := fire_vt_vau1
  io.fire.vau2 := fire_vt_vau2
  io.fire.amo := fire_vt_amo
  io.fire.utld := fire_vt_utld
  io.fire.utst := fire_vt_utst
  io.fire.vld := fire_tvec_vld
  io.fire.vst := fire_tvec_vst

  io.fire_fn.viu := Mux(fire_tvec, io.tvec_fn.viu, io.vt_fn.viu)
  io.fire_fn.vau0 := io.vt_fn.vau0
  io.fire_fn.vau1 := io.vt_fn.vau1
  io.fire_fn.vau2 := io.vt_fn.vau2

  io.fire_regid_imm.vs_zero := io.vt_regid_imm.vs_zero
  io.fire_regid_imm.vt_zero := Mux(fire_tvec, io.tvec_regid_imm.vt_zero, io.vt_regid_imm.vt_zero)
  io.fire_regid_imm.vr_zero := io.vt_regid_imm.vr_zero
  io.fire_regid_imm.vs := io.vt_regid_imm.vs
  io.fire_regid_imm.vt := Mux(fire_tvec, io.tvec_regid_imm.vt, io.vt_regid_imm.vt)
  io.fire_regid_imm.vr := io.vt_regid_imm.vr
  io.fire_regid_imm.vd := Mux(fire_tvec, io.tvec_regid_imm.vd, io.vt_regid_imm.vd)
  io.fire_regid_imm.mem <> Mux(fire_tvec, io.tvec_regid_imm.mem, io.vt_regid_imm.mem)
  io.fire_regid_imm.imm := Mux(fire_tvec, io.tvec_regid_imm.imm, io.vt_regid_imm.imm)
  io.fire_regid_imm.imm2 := io.tvec_regid_imm.imm2
  io.fire_regid_imm.irb.imm1_rtag := Mux(fire_tvec, io.tvec_regid_imm.irb.imm1_rtag, io.tvec_regid_imm.irb.imm1_rtag)
  io.fire_regid_imm.irb.cnt_rtag := Mux(fire_tvec, io.tvec_regid_imm.irb.cnt_rtag, io.tvec_regid_imm.irb.cnt_rtag)
  io.fire_regid_imm.irb.pc_next := io.vt_regid_imm.irb.pc_next
  io.fire_regid_imm.irb.update_imm1 := io.tvec_regid_imm.irb.update_imm1
}

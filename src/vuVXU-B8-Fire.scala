package riscvVector

import Chisel._
import Node._
import Config._

class vuVXU_Banked8_Fire extends Component
{
  val io = new io_vxu_fire();

  val fire_tvec_viu = ~io.tvec_regid_imm.vd_zero & io.tvec_valid.viu & io.tvec_ready;
  val fire_tvec_vlu = ~io.tvec_regid_imm.vd_zero & io.tvec_valid.vlu & io.tvec_ready;
  val fire_tvec_vsu = ~io.tvec_regid_imm.vd_zero & io.tvec_valid.vsu & io.tvec_ready;
  val fire_tvec = fire_tvec_viu | fire_tvec_vlu | fire_tvec_vsu;

  val fire_vt_viu = ~io.vt_regid_imm.vd_zero & io.vt_valid.viu & io.vt_ready;
  val fire_vt_vau0 = ~io.vt_regid_imm.vd_zero & io.vt_valid.vau0 & io.vt_ready;
  val fire_vt_vau1 = ~io.vt_regid_imm.vd_zero & io.vt_valid.vau1 & io.vt_ready;
  val fire_vt_vau2 = ~io.vt_regid_imm.vd_zero & io.vt_valid.vau2 & io.vt_ready;
  val fire_vt_vgslu = ~io.vt_regid_imm.vd_zero & io.vt_valid.vgslu & io.vt_ready;
  val fire_vt_vglu = ~io.vt_regid_imm.vd_zero & io.vt_valid.vglu & io.vt_ready;
  val fire_vt_vgsu = ~io.vt_regid_imm.vd_zero & io.vt_valid.vgsu & io.vt_ready;

  io.fire.viu := fire_tvec_viu | fire_vt_viu;
  io.fire.vau0 := fire_vt_vau0;
  io.fire.vau1 := fire_vt_vau1;
  io.fire.vau2 := fire_vt_vau2;
  io.fire.vgslu := fire_vt_vgslu;
  io.fire.vglu := fire_vt_vglu;
  io.fire.vgsu := fire_vt_vgsu;
  io.fire.vlu := fire_tvec_vlu;
  io.fire.vsu := fire_tvec_vsu;

  io.fire_fn.viu := Mux(fire_tvec, io.tvec_fn.viu, io.vt_fn.viu);
  io.fire_fn.vau0 := io.vt_fn.vau0;
  io.fire_fn.vau1 := io.vt_fn.vau1;
  io.fire_fn.vau2 := io.vt_fn.vau2;

  io.fire_regid_imm.vs_zero := io.vt_regid_imm.vs_zero;
  io.fire_regid_imm.vt_zero := Mux(fire_tvec, io.tvec_regid_imm.vt_zero, io.vt_regid_imm.vt_zero);
  io.fire_regid_imm.vr_zero := io.vt_regid_imm.vr_zero;
  io.fire_regid_imm.vs := io.vt_regid_imm.vs;
  io.fire_regid_imm.vt := Mux(fire_tvec, io.tvec_regid_imm.vt, io.vt_regid_imm.vt);
  io.fire_regid_imm.vr := io.vt_regid_imm.vr;
  io.fire_regid_imm.vd := Mux(fire_tvec, io.tvec_regid_imm.vd, io.vt_regid_imm.vd);
  io.fire_regid_imm.imm := Mux(fire_tvec, io.tvec_regid_imm.imm, io.vt_regid_imm.imm);
  io.fire_regid_imm.imm2 := io.tvec_regid_imm.imm2;
}

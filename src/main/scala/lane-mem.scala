package hwacha

import Chisel._
import Node._
import Constants._
import Compaction._
import uncore.constants.AddressConstants._
import uncore.constants.MemoryOpConstants._

class LaneMem extends Module
{
  val io = new Bundle {
    val op = new LaneMemOpIO().flip
    val data = new LaneMemDataIO().flip
    val vmu = new VMUIO
  }

  // VGU
  val s1_vgu_op = Reg(Valid(new VGUOp).asDirectionless)
  s1_vgu_op.valid := io.op.vgu.valid
  when (io.op.vgu.valid) {
    s1_vgu_op.bits := io.op.vgu.bits
  }
  val s1_paddr = RegEnable(Mux(io.op.vgu.bits.fn.utmemop(), io.data.paddr, Bits(0)), io.op.vgu.valid)
  val s1_addr = s1_vgu_op.bits.base + s1_paddr

  io.vmu.vaq.valid := s1_vgu_op.valid
  io.vmu.vaq.bits <> s1_vgu_op.bits.check
  io.vmu.vaq.bits.cmd <> s1_vgu_op.bits.fn.cmd
  io.vmu.vaq.bits.typ <> s1_vgu_op.bits.fn.typ
  io.vmu.vaq.bits.typ_float <> s1_vgu_op.bits.fn.float
  io.vmu.vaq.bits.idx := s1_addr(PGIDX_BITS-1, 0)
  io.vmu.vaq.bits.vpn := s1_addr(VADDR_BITS, PGIDX_BITS)

  // FIXME
  io.vmu.evaq.valid := io.op.vgu.valid
  io.vmu.evaq.bits.cmd := io.op.vgu.bits.fn.cmd

  // VSU
  val s1_vsu_op = Reg(Valid(new VSUOp).asDirectionless)
  s1_vsu_op.valid := io.op.vsu.valid
  when (io.op.vsu.valid) {
    s1_vsu_op.bits := io.op.vsu.bits
  }
  val s1_sdata = RegEnable(io.data.sdata, io.op.vsu.valid) 

  val rf64f64 = Module(new hardfloat.recodedFloat64ToFloat64)
  rf64f64.io.in := unpack_float_d(s1_sdata, 0)
  val s1_sdata_f_dp = rf64f64.io.out

  val rf32f32 = Module(new hardfloat.recodedFloat32ToFloat32)
  rf32f32.io.in := unpack_float_s(s1_sdata, 0)
  val s1_sdata_f_sp = rf32f32.io.out

  val s1_sdata_f_hp = unpack_float_h(s1_sdata, 0)

  val s1_store_fp = s1_vsu_op.bits.fn.float
  val s1_store_fp_d = s1_store_fp && s1_vsu_op.bits.fn.typ === MT_D
  val s1_store_fp_w = s1_store_fp && s1_vsu_op.bits.fn.typ === MT_W
  val s1_store_fp_h = s1_store_fp && s1_vsu_op.bits.fn.typ === MT_H

  io.vmu.vsdq.valid := s1_vsu_op.valid
  io.vmu.vsdq.bits := MuxCase(
    s1_sdata(63,0), Array(	
      (s1_store_fp_d) -> s1_sdata_f_dp,
      (s1_store_fp_w) -> s1_sdata_f_sp,
      (s1_store_fp_h) -> s1_sdata_f_hp
    ))

  // FIXME
  io.vmu.evsdq.valid := io.op.vsu.valid

  // VLU
  val s1_vlu_op = Reg(Valid(new VLUOp).asDirectionless)
  s1_vlu_op.valid := io.op.vlu.valid
  when (io.op.vlu.valid) {
    s1_vlu_op.bits := io.op.vlu.bits
  }
  val s1_ldata = RegEnable(io.vmu.vldq.bits, io.op.vlu.valid) 

  val f64rf64 = Module(new hardfloat.float64ToRecodedFloat64)
  f64rf64.io.in := s1_ldata
  val s1_ldata_rf_dp = f64rf64.io.out

  val f32rf32 = Module(new hardfloat.float32ToRecodedFloat32)
  f32rf32.io.in := s1_ldata
  val s1_ldata_rf_sp = f32rf32.io.out

  val s1_load_fp = s1_vlu_op.bits.fn.float
  val s1_load_fp_d = s1_load_fp && s1_vlu_op.bits.fn.typ === MT_D
  val s1_load_fp_w = s1_load_fp && s1_vlu_op.bits.fn.typ === MT_W
  val s1_load_fp_h = s1_load_fp && s1_vlu_op.bits.fn.typ === MT_H

  io.vmu.vldq.ready := io.op.vlu.valid
  io.data.ldata := MuxCase(
    s1_ldata, Array(
      (s1_load_fp_d) -> pack_float_d(s1_ldata_rf_dp, 0),
      (s1_load_fp_w) -> pack_float_s(s1_ldata_rf_sp, 0),
      (s1_load_fp_h) -> pack_float_h(s1_ldata, 0)
    ))
}

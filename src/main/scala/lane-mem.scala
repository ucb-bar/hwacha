package hwacha

import Chisel._
import Node._
import Constants._
import Compaction._
import uncore.constants.AddressConstants._
import uncore.constants.MemoryOpConstants._

class LaneMem(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val op = new LaneMemOpIO().flip
    val data = new LaneMemDataIO().flip
    val vmu = new VMUIO
    val lret = new MRTLoadRetireIO
  }

  // VGU
  // Timing Diagram
  // | Seq | Exp/SRAM-Addr-Setup | SRAM-Clock-Q | XBar/Addr-Gen/VAQ-Setup |
  //                                              ^ io.op.vgu starts here
  val paddr = Mux(io.op.vgu.bits.fn.utmemop(), io.data.paddr, Bits(0))
  val addr = io.op.vgu.bits.base + paddr

  io.vmu.addr.q.valid := io.op.vgu.valid
  io.vmu.addr.q.bits.cmd <> vmu_op_mcmd(io.op.vgu.bits.fn.op)
  io.vmu.addr.q.bits.typ <> io.op.vgu.bits.fn.typ
  io.vmu.addr.q.bits.idx := addr(PGIDX_BITS-1, 0)
  io.vmu.addr.q.bits.vpn := addr(VADDR_BITS, PGIDX_BITS)

  // VSU
  // Timing Diagram
  // | Seq | Exp/SRAM-Addr-Setup | SRAM-Clock-Q | XBar/Recoding/VSDQ-Setup |
  //                                              ^ io.op.vsu starts here
  val rf64f64 = Module(new hardfloat.recodedFloat64ToFloat64)
  rf64f64.io.in := unpack_float_d(io.data.sdata, 0)
  val sdata_f_dp = rf64f64.io.out

  val rf32f32 = Module(new hardfloat.recodedFloat32ToFloat32)
  rf32f32.io.in := unpack_float_s(io.data.sdata, 0)
  val sdata_f_sp = rf32f32.io.out

  val sdata_f_hp = unpack_float_h(io.data.sdata, 0)

  val store_fp = io.op.vsu.bits.fn.float
  val store_fp_d = store_fp && io.op.vsu.bits.fn.typ === MT_D
  val store_fp_w = store_fp && io.op.vsu.bits.fn.typ === MT_W
  val store_fp_h = store_fp && io.op.vsu.bits.fn.typ === MT_H

  io.vmu.sdata.q.valid := io.op.vsu.valid
  io.vmu.sdata.q.bits := MuxCase(
    io.data.sdata(63,0), Array(
      (store_fp_d) -> sdata_f_dp,
      (store_fp_w) -> sdata_f_sp,
      (store_fp_h) -> sdata_f_hp
    ))

  // VLU
  val s1_vlu_op = Reg(Valid(new VLUOp).asDirectionless)
  s1_vlu_op.valid := io.op.vlu.valid
  when (io.op.vlu.valid) {
    s1_vlu_op.bits := io.op.vlu.bits
  }
  val s1_ldata = RegEnable(io.vmu.ldata.q.bits, io.op.vlu.valid)

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

  io.vmu.ldata.q.ready := io.op.vlu.valid
  io.data.ldata := MuxCase(
    s1_ldata, Array(
      (s1_load_fp_d) -> pack_float_d(s1_ldata_rf_dp, 0),
      (s1_load_fp_w) -> pack_float_s(s1_ldata_rf_sp, 0),
      (s1_load_fp_h) -> pack_float_h(s1_ldata, 0)
    ))

  io.lret.update := s1_vlu_op.valid
}

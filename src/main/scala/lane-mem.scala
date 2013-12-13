package hwacha

import Chisel._
import Node._
import Constants._
import uncore.constants.AddressConstants._
import uncore.constants.MemoryOpConstants._

class LaneMem extends Module
{
  val io = new Bundle {
    val lane_vaq_valid = Bool(INPUT)
    val lane_vaq_check = new io_vxu_mem_check().asInput
    val lane_vaq_mem = new io_vxu_mem_cmd().asInput
    val lane_vaq_imm = Bits(INPUT, SZ_DATA)
    val lane_vaq_utmemop = Bool(INPUT)
    val lane_vaq_rf = Bits(INPUT, SZ_DATA)
    
    val vmu_vaq_valid = Bool(OUTPUT)
    val vmu_vaq_bits = new io_vvaq_bundle().asOutput
    
    val lane_vsdq_valid = Bool(INPUT)
    val lane_vsdq_mem = new io_vxu_mem_cmd().asInput 
    val lane_vsdq_bits = Bits(INPUT, SZ_DATA) 
    
    val vmu_vsdq_valid = Bool(OUTPUT)
    val vmu_vsdq_bits = Bits(OUTPUT, SZ_DATA) 
  }

  val reg_lane_vaq_valid = Reg(next=io.lane_vaq_valid)
  val reg_lane_vaq_check = Reg(next=io.lane_vaq_check)
  val reg_lane_vaq_mem = Reg(next=io.lane_vaq_mem)
  val reg_lane_vaq_imm = Reg(next=io.lane_vaq_imm)
  val reg_lane_vaq_rf = Reg(next=Mux(io.lane_vaq_utmemop, io.lane_vaq_rf, Bits(0,SZ_DATA)))

  val addr = reg_lane_vaq_imm + reg_lane_vaq_rf
  io.vmu_vaq_valid := reg_lane_vaq_valid
  io.vmu_vaq_bits <> reg_lane_vaq_check
  io.vmu_vaq_bits <> reg_lane_vaq_mem
  io.vmu_vaq_bits.idx := addr(PGIDX_BITS-1, 0)
  io.vmu_vaq_bits.vpn := addr(VADDR_BITS, PGIDX_BITS)

  val store_fp = io.lane_vsdq_mem.typ_float
  val store_fp_d = store_fp && io.lane_vsdq_mem.typ === MT_D
  val store_fp_w = store_fp && io.lane_vsdq_mem.typ === MT_W
  val store_fp_h = store_fp && io.lane_vsdq_mem.typ === MT_H

  val reg_lane_vsdq_valid = Reg(next=io.lane_vsdq_valid)
  val reg_lane_vsdq_bits = Reg(next=io.lane_vsdq_bits)

  val rf32f32_lo  = Module(new hardfloat.recodedFloat32ToFloat32)
  rf32f32_lo.io.in := reg_lane_vsdq_bits(32,0)
  val vsdq_deq_sp_lo = rf32f32_lo.io.out

  val rf32f32_hi  = Module(new hardfloat.recodedFloat32ToFloat32)
  rf32f32_hi.io.in := reg_lane_vsdq_bits(65,33)
  val vsdq_deq_sp_hi = rf32f32_hi.io.out

  val vsdq_deq_sp = Cat(vsdq_deq_sp_hi, vsdq_deq_sp_lo)

  val rf64f64  = Module(new hardfloat.recodedFloat64ToFloat64)
  rf64f64.io.in := reg_lane_vsdq_bits
  val vsdq_deq_dp = rf64f64.io.out

  val vsdq_deq_hp = Cat(reg_lane_vsdq_bits(64,33),reg_lane_vsdq_bits(31,0))
 
  io.vmu_vsdq_valid := reg_lane_vsdq_valid
  io.vmu_vsdq_bits := MuxCase(
      reg_lane_vsdq_bits(63,0), Array(	
        (store_fp_d) -> vsdq_deq_dp,
        (store_fp_w) -> vsdq_deq_sp,
        (store_fp_h) -> vsdq_deq_hp
      ))
}

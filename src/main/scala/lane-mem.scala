package hwacha

import Chisel._
import Node._
import Constants._
import hardfloat._
import uncore.constants.MemoryOpConstants._

class io_vxu_mem extends Bundle
{
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

class vuVXU_Banked8_Mem extends Module
{
  val io = new io_vxu_mem()

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

  val reg_lane_vsdq_valid = Reg(next=io.lane_vsdq_valid)
  val reg_lane_vsdq_bits = Reg(next=io.lane_vsdq_bits)

  val rf32f32  = Module(new recodedFloat32ToFloat32)
  rf32f32.io.in := reg_lane_vsdq_bits(32,0)
  val vsdq_deq_sp = rf32f32.io.out

  val rf64f64  = Module(new recodedFloat64ToFloat64)
  rf64f64.io.in := reg_lane_vsdq_bits
  val vsdq_deq_dp = rf64f64.io.out
 
  io.vmu_vsdq_valid := reg_lane_vsdq_valid
  io.vmu_vsdq_bits := MuxCase(
      reg_lane_vsdq_bits(63,0), Array(	
	      (store_fp_d) -> vsdq_deq_dp,
        (store_fp_w) -> Fill(2, vsdq_deq_sp)
      ))
}

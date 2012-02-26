package hwacha

import Chisel._
import Node._
import Constants._
import queues._
import hardfloat._

class vuVXU_Banked8_Mem extends Component
{
  val io = new io_vxu_mem()

  val reg_lane_vaq_valid = Reg(io.lane_vaq_valid)
  val reg_lane_vaq_mem = Reg(io.lane_vaq_mem)
  val reg_lane_vaq_imm = Reg(io.lane_vaq_imm)
  val reg_lane_vaq_rf = Reg(Mux(io.lane_vaq_utmemop, io.lane_vaq_rf, Bits(0,SZ_DATA)))

  val addr = reg_lane_vaq_imm + reg_lane_vaq_rf
  io.vmu_vaq_valid := reg_lane_vaq_valid
  io.vmu_vaq_bits <> reg_lane_vaq_mem
  io.vmu_vaq_bits.idx := addr(PGIDX_BITS-1, 0)
  io.vmu_vaq_bits.vpn := addr(VADDR_BITS, PGIDX_BITS)

  val store_fp = io.lane_vsdq_mem.typ_float
  val store_fp_d = store_fp && io.lane_vsdq_mem.typ === MT_D
  val store_fp_w = store_fp && io.lane_vsdq_mem.typ === MT_W

  val reg_lane_vsdq_valid = Reg(io.lane_vsdq_valid)
  val reg_lane_vsdq_bits = Reg(io.lane_vsdq_bits)

  val rf32f32  = new recodedFloat32ToFloat32()
  rf32f32.io.in := reg_lane_vsdq_bits(32,0)
  val vsdq_deq_sp = rf32f32.io.out

  val rf64f64  = new recodedFloat64ToFloat64()
  rf64f64.io.in := reg_lane_vsdq_bits
  val vsdq_deq_dp = rf64f64.io.out
 
  io.vmu_vsdq_valid := reg_lane_vsdq_valid
  io.vmu_vsdq_bits := MuxCase(
      reg_lane_vsdq_bits(63,0), Array(	
	      (store_fp_d) -> vsdq_deq_dp,
        (store_fp_w) -> Fill(2, vsdq_deq_sp)
      ))
}

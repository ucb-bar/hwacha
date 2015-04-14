package hwacha

import Chisel._
import Node._
import uncore._
import Constants._

class FrontendReq extends rocket.CoreBundle {
  val pc = UInt(width = vaddrBits+1)
}

class FrontendIO extends Bundle {
  val active = Bool(OUTPUT)
  val req = Valid(new FrontendReq)
  val resp = Decoupled(new rocket.FrontendResp).flip
  val invalidate = Bool(OUTPUT)
}

class HwachaFrontend extends HwachaModule with rocket.FrontendParameters
{
  val io = new Bundle {
    val vxu = new FrontendIO().flip
    val vru = new FrontendIO().flip
    val ptw = new rocket.TLBPTWIO()
    val mem = new HeaderlessUncachedTileLinkIO
  }

  val icache = Module(new rocket.ICache)
  val tlb = Module(new rocket.TLB)

  // COLIN FIXME: v[x|r]u* signals look bad and are prone to errors, collating to array my be useful?
  val vxu_s1_pc_ = Reg(UInt())
  val vxu_s1_pc = vxu_s1_pc_ & SInt(-2) // discard LSB of PC (throughout the pipeline)
  val vru_s1_pc_ = Reg(UInt())
  val vru_s1_pc = vru_s1_pc_ & SInt(-2) // discard LSB of PC (throughout the pipeline)
  val s1_pc_ = Reg(UInt())
  val s1_pc = s1_pc_ & SInt(-2) // discard LSB of PC (throughout the pipeline)
  val s1_same_block = Reg(Bool())
  val s1_valid = Reg(init=Bool(false))
  val vxu_s1_valid = Reg(init=Bool(false))
  val vru_s1_valid = Reg(init=Bool(false))
  val s1_type = Reg(Bool())

  val vxu_s2_pc = Reg(init=UInt(0x100))
  val vru_s2_pc = Reg(init=UInt(0x100))
  val s2_pc = Reg(init=UInt(0x100))
  val s2_xcpt_if = Reg(init=Bool(false))
  val s2_same_block = Reg(Bool())
  val s2_valid = Reg(init=Bool(false))
  val vxu_s2_valid = Reg(init=Bool(false))
  val vru_s2_valid = Reg(init=Bool(false))
  val s2_type = Reg(Bool())

  val vxu_line_val = Reg(init=Bool(false))
  val vru_line_val = Reg(init=Bool(false))
  val vxu_line = Reg(init=Bits(0, width=rowBits))
  val vru_line = Reg(init=Bits(0, width=rowBits))
  val vxu_line_pc = Reg(UInt())
  val vru_line_pc = Reg(UInt())

  val icmiss = s2_valid && !icache.io.resp.valid
  val vxu_icmiss = icmiss && s2_type
  val vru_icmiss = icmiss && !s2_type

  //avoid firing the icache if we have the same line in a line buffer or further down the pipeline
  val vxu_req_pc = Mux(vxu_icmiss, vxu_s2_pc, 
                       Mux(io.vxu.req.valid, io.vxu.req.bits.pc, 
                       Mux(!vxu_s1_valid,vxu_s1_pc,vxu_s1_pc+UInt(8))))

  val vxu_req_pc_rowbyte = vxu_req_pc & SInt(-rowBytes)

  val vxu_line_rowbyte = vxu_line_pc & SInt(-rowBytes)
  val vru_line_rowbyte = vru_line_pc & SInt(-rowBytes)
  val s1_pc_rowbyte = s1_pc & SInt(-rowBytes)
  val vxu_s1_pc_rowbyte = vxu_s1_pc & SInt(-rowBytes)
  val vru_s1_pc_rowbyte = vru_s1_pc & SInt(-rowBytes)
  val s2_pc_rowbyte = s2_pc & SInt(-rowBytes)


  val vxu_s0_n_match = s1_type && s1_valid &&
                     (vxu_req_pc_rowbyte === s1_pc_rowbyte)
  val vxu_s1_n_match = s2_type && s2_valid && !vxu_icmiss && 
                     (vxu_s1_pc_rowbyte === s2_pc_rowbyte)
  //next next req matches
  val vxu_s0_inval_nn_match = s1_type && s1_valid && (vxu_req_pc_rowbyte != s1_pc_rowbyte)
  val vxu_s0_nn_match = s2_type && s2_valid && !vxu_icmiss && 
                          (vxu_req_pc_rowbyte === s2_pc_rowbyte) &&
                          !vxu_s0_inval_nn_match
  //line buffer matches
  val vxu_s0_inval_nnn_match = s2_type && s2_valid && (vxu_req_pc_rowbyte != s2_pc_rowbyte)
  val vxu_s0_nnn_match = vxu_line_val && 
                          (vxu_req_pc_rowbyte === vxu_line_rowbyte) &&
                          !vxu_s0_inval_nn_match && !vxu_s0_inval_nnn_match

  val vxu_s1_nn_match = Reg(init=Bool(false))
  val vxu_s1_nnn_match = Reg(init=Bool(false))
  val vxu_s2_same_block = Reg(init=Bool(false))
  //overall match
  val vxu_s0_same_block = vxu_s0_n_match || vxu_s0_nn_match || vxu_s0_nnn_match
  val vxu_s1_same_block = vxu_s1_n_match || vxu_s1_nn_match || vxu_s1_nnn_match

  val vru_req_pc = Mux(vru_icmiss, vru_s2_pc, 
                       Mux(io.vru.req.valid, io.vru.req.bits.pc,
                       Mux(!vru_s1_valid, vru_s1_pc,
                           vru_s1_pc+UInt(8))))

  val vru_req_pc_rowbyte = vru_req_pc & SInt(-rowBytes)
  //next req matches
  val vru_s0_n_match = !s1_type && s1_valid &&
                     (vru_req_pc_rowbyte === s1_pc_rowbyte)
  val vru_s1_n_match = !s2_type && s2_valid && !vru_icmiss && 
                     (vru_s1_pc_rowbyte === s2_pc_rowbyte)
  //next next req matches
  val vru_s0_inval_nn_match = !s1_type && s1_valid && (vru_req_pc_rowbyte != s1_pc_rowbyte)
  val vru_s0_nn_match = !s2_type && s2_valid && !vru_icmiss && 
                          (vru_req_pc_rowbyte === s2_pc_rowbyte) &&
                          !vru_s0_inval_nn_match
  //line buffer matches
  val vru_s0_inval_nnn_match = !s2_type && s2_valid && (vru_req_pc_rowbyte != s2_pc_rowbyte)
  val vru_s0_nnn_match = vru_line_val && 
                          (vru_req_pc_rowbyte === vru_line_rowbyte) &&
                          !vru_s0_inval_nn_match && !vru_s0_inval_nnn_match

  val vru_s1_nn_match = Reg(init=Bool(false))
  val vru_s1_nnn_match = Reg(init=Bool(false))
  val vru_s2_same_block = Reg(init=Bool(false))
  //overall match
  val vru_s0_same_block = vru_s0_n_match || vru_s0_nn_match || vru_s0_nnn_match
  val vru_s1_same_block = vru_s1_n_match || vru_s1_nn_match || vru_s1_nnn_match
              
  //arbitrate between vxu and vru
  val vxu_active = io.vxu.active
  val vru_active = io.vru.active
  val redirect = io.vxu.req.valid || io.vru.req.valid
  val req_val = redirect || (vxu_active && !vxu_s0_same_block) || (vru_active && !vru_s0_same_block)
  val req_pc   = Mux(!vxu_s0_same_block, vxu_req_pc, vru_req_pc)
  val req_type = !vxu_s0_same_block
  val vxu_make_req = req_type && (!vxu_s0_same_block && vxu_active)
  val vru_make_req = !req_type && (!vru_s0_same_block && vru_active)

  val stall = req_val && (io.vxu.resp.valid && !io.vxu.resp.ready ||
                         io.vru.resp.valid && !io.vru.resp.ready)

  val valid_req = (s1_type && vxu_s1_valid && !vxu_s1_same_block) ||
                    (!s1_type && vru_s1_valid && !vru_s1_same_block)


  when (!stall) {
    //stage 1
    vxu_s1_pc_ := vxu_req_pc
    vru_s1_pc_ := vru_req_pc
    s1_pc_ := req_pc
    s1_valid := vxu_make_req || vru_make_req
    vxu_s1_valid := vxu_active &&  (req_type && !vxu_s0_same_block || vxu_s0_same_block)
    vru_s1_valid := vru_active && (!req_type && !vru_s0_same_block || vru_s0_same_block)
    s1_type := req_type
    vxu_s1_nn_match := vxu_s0_nn_match
    vru_s1_nn_match := vru_s0_nn_match
    vxu_s1_nnn_match := vxu_s0_nnn_match
    vru_s1_nnn_match := vru_s0_nnn_match
    //stage 2
    vxu_s2_same_block := vxu_s1_same_block
    vru_s2_same_block := vru_s1_same_block
    s2_valid := s1_valid
    vxu_s2_valid := vxu_s1_valid
    vru_s2_valid := vru_s1_valid
    s2_type := s1_type
    when (!icmiss) {
      s2_pc := s1_pc
      s2_xcpt_if := tlb.io.resp.xcpt_if
    }
    when (!vxu_icmiss) {
      vxu_s2_pc := vxu_s1_pc
    }
    when (!vru_icmiss) {
      vru_s2_pc := vru_s1_pc
    }
  }

  tlb.io.ptw <> io.ptw
  tlb.io.req.valid := !stall && !icmiss
  tlb.io.req.bits.vpn := s1_pc >> UInt(pgIdxBits)
  tlb.io.req.bits.asid := UInt(0)
  tlb.io.req.bits.passthrough := Bool(false)
  tlb.io.req.bits.instruction := Bool(true)
  tlb.io.req.bits.store := Bool(false)

  icache.io.mem <> io.mem
  icache.io.req.valid := !stall && (vxu_make_req || vru_make_req)
  icache.io.req.bits.idx := req_pc
  icache.io.invalidate := io.vxu.invalidate//only vxu invalidates
  icache.io.req.bits.ppn := tlb.io.resp.ppn
  icache.io.req.bits.kill := tlb.io.resp.miss || io.ptw.invalidate
  icache.io.resp.ready := !stall && valid_req

  val resp_valid = s2_valid && (s2_xcpt_if || icache.io.resp.valid)
  io.vxu.resp.valid := vxu_s2_valid && ((s2_type && resp_valid) || vxu_s2_same_block)
  io.vru.resp.valid := vru_s2_valid && ((!s2_type && resp_valid) || vru_s2_same_block)
  io.vxu.resp.bits.pc := vxu_s2_pc & SInt(-coreInstBytes) // discard PC LSBs
  io.vru.resp.bits.pc := vru_s2_pc & SInt(-coreInstBytes) // discard PC LSBs

  when(s2_valid && icache.io.resp.valid) {
    when(s2_type){
      when(!vxu_s2_same_block){
        vxu_line := icache.io.resp.bits.datablock
        vxu_line_val := Bool(true)
        vxu_line_pc := vxu_s2_pc
      }
    }.otherwise {
      when(!vru_s2_same_block){
        vru_line := icache.io.resp.bits.datablock
        vru_line_val := Bool(true)
        vru_line_pc := vru_s2_pc
      }
    }
  }
  val vxu_datablock = Mux(vxu_s2_same_block, vxu_line, icache.io.resp.bits.datablock)
  val vru_datablock = Mux(vru_s2_same_block, vru_line, icache.io.resp.bits.datablock)
  val vxu_fetch_data = vxu_datablock >> (vxu_s2_pc(log2Up(rowBytes)-1,log2Up(coreFetchWidth*coreInstBytes)) << UInt(log2Up(coreFetchWidth*coreInstBits)))
  val vru_fetch_data = vru_datablock >> (vru_s2_pc(log2Up(rowBytes)-1,log2Up(coreFetchWidth*coreInstBytes)) << UInt(log2Up(coreFetchWidth*coreInstBits)))
  for (i <- 0 until coreFetchWidth) {
    io.vxu.resp.bits.data(i) := vxu_fetch_data(i*coreInstBits+coreInstBits-1, i*coreInstBits)
    io.vru.resp.bits.data(i) := vru_fetch_data(i*coreInstBits+coreInstBits-1, i*coreInstBits)
  }

  val all_ones = UInt((1 << (coreFetchWidth+1))-1)
  io.vxu.resp.bits.mask := all_ones//not using btb
  io.vru.resp.bits.mask := all_ones//not using btb

  io.vxu.resp.bits.xcpt_if := s2_xcpt_if
  io.vru.resp.bits.xcpt_if := s2_xcpt_if

}

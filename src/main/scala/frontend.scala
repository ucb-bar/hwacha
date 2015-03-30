package hwacha

import Chisel._
import Node._
import uncore._
import Constants._

class FrontendReq extends rocket.CoreBundle {
  val pc = UInt(width = vaddrBits+1)
  val npc = UInt(width = vaddrBits+1)
  val nnpc = UInt(width = vaddrBits+1)
}

class FrontendIO extends Bundle {
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

  val s1_pc_ = Reg(UInt())
  val s1_pc = s1_pc_ & SInt(-2) // discard LSB of PC (throughout the pipeline)
  val s1_same_block = Reg(Bool())
  val s1_valid = Reg(init=Bool(false))
  val s1_type = Reg(Bool())

  val s2_pc = Reg(init=UInt(0x100))
  val s2_xcpt_if = Reg(init=Bool(false))
  val s2_same_block = Reg(Bool())
  val s2_valid = Reg(init=Bool(false))
  val s2_type = Reg(Bool())

  val vxu_line_val = Reg(init=Bool(false))
  val vru_line_val = Reg(init=Bool(false))
  val vxu_line = Reg(init=Bits(0, width=rowBits))
  val vru_line = Reg(init=Bits(0, width=rowBits))
  val vxu_line_pc = Reg(UInt())
  val vru_line_pc = Reg(UInt())

  val icmiss = !s2_same_block && !icache.io.resp.valid
  val prev_icmiss = Reg(next=icmiss)
  //avoid firing the icache if we have the same line in our buffer? (opt: either buffer)
  //since we have no fine-grained invalidation we assume the line buffer is up-to-date unless we recieve an invalidate

  val req_val = io.vxu.req.valid || io.vru.req.valid;
  val req_pc = Mux(req_val && !prev_icmiss && !icmiss,
               Mux(io.vxu.req.valid, io.vxu.req.bits.nnpc, io.vru.req.bits.nnpc),
               Mux(req_val && !icmiss,
               Mux(io.vxu.req.valid, io.vxu.req.bits.npc, io.vru.req.bits.npc),
               Mux(io.vxu.req.valid, io.vxu.req.bits.pc, io.vru.req.bits.pc)))
  val req_type = io.vxu.req.valid //1===vxu_req 

  //line buffer has the pc and won't be overwritten
  val vxu_same_block = vxu_line_val && 
                     ((vxu_line_pc & Bits(rowBytes)) === (req_pc & Bits(rowBytes))) &&
                     (!s1_type || !s1_valid || s1_same_block)
                     (!s2_type || !s2_valid || s2_same_block)
              
   val vru_same_block = vru_line_val && 
                      ((vru_line_pc & Bits(rowBytes)) === (req_pc & Bits(rowBytes))) &&
                      (s1_type || !s1_valid || s1_same_block)
                      (s2_type || !s2_valid || s2_same_block)
  //or the s1_req has the data
  val req_same_block = !icmiss && ((s1_pc & Bits(rowBytes)) === (req_pc & Bits(rowBytes)))
  ///Colin FIXME: is there a case where the line buffers will be overwritten before we read it?
  val s0_same_block = vxu_same_block || vru_same_block | req_same_block


  val stall = req_val && (io.vxu.resp.valid && !io.vxu.resp.ready ||
                         io.vru.resp.valid && !io.vru.resp.ready)

  when(!stall) {
    s1_same_block := s0_same_block && !tlb.io.resp.miss && req_val
    s2_same_block := s1_same_block
    s2_valid := s1_valid && (!icmiss || s1_same_block)
    s2_type := s1_type
    //Colin FIXME: we don't predicate this on a non-cache miss anymore
    s2_pc := s1_pc
    s2_xcpt_if := tlb.io.resp.xcpt_if
  }
  s1_valid := Bool(false)
  when(req_val){
    s1_pc_ := req_pc
    s1_valid := Bool(true)
    s1_type := req_type
  }

  tlb.io.ptw <> io.ptw
  tlb.io.req.valid := !stall && !icmiss
  tlb.io.req.bits.vpn := s1_pc >> UInt(pgIdxBits)
  tlb.io.req.bits.asid := UInt(0)
  tlb.io.req.bits.passthrough := Bool(false)
  tlb.io.req.bits.instruction := Bool(true)
  tlb.io.req.bits.store := Bool(false)

  icache.io.mem <> io.mem
  icache.io.req.valid := !stall && !s0_same_block && req_val
  icache.io.req.bits.idx := req_pc
  icache.io.invalidate := io.vxu.invalidate//only vxu invalidates
  icache.io.req.bits.ppn := tlb.io.resp.ppn
  //Colin FIXME: are we required to kill req on miss?
  icache.io.req.bits.kill := tlb.io.resp.miss || io.ptw.invalidate
  icache.io.resp.ready := !stall && !s1_same_block

  val resp_valid = s2_valid && (s2_xcpt_if || icache.io.resp.valid)
  io.vxu.resp.valid := s2_type && resp_valid
  io.vru.resp.valid := !s2_type && resp_valid
  io.vxu.resp.bits.pc := s2_pc & SInt(-coreInstBytes) // discard PC LSBs
  io.vru.resp.bits.pc := s2_pc & SInt(-coreInstBytes) // discard PC LSBs

  when(s2_valid && icache.io.resp.valid) {
    when(s2_type){
      vxu_line := icache.io.resp.bits.datablock
      vxu_line_val := Bool(true)
      vxu_line_pc := s2_pc
    }.otherwise {
      vru_line := icache.io.resp.bits.datablock
      vru_line_val := Bool(true)
      vru_line_pc := s2_pc
    }
  }
  //Colin FIXME: this looks ugly
  val vxu_datablock = Mux(s2_same_block, vxu_line, icache.io.resp.bits.datablock)
  val vru_datablock = Mux(s2_same_block, vru_line, icache.io.resp.bits.datablock)
  val vxu_fetch_data = vxu_datablock >> (s2_pc(log2Up(rowBytes)-1,log2Up(coreFetchWidth*coreInstBytes)) << UInt(log2Up(coreFetchWidth*coreInstBits)))
  val vru_fetch_data = vru_datablock >> (s2_pc(log2Up(rowBytes)-1,log2Up(coreFetchWidth*coreInstBytes)) << UInt(log2Up(coreFetchWidth*coreInstBits)))
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

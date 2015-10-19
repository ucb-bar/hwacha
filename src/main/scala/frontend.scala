package hwacha

import Chisel._

class FrontendReq(implicit p: Parameters) extends rocket.CoreBundle()(p) {
  val pc = UInt(width = vaddrBits+1)
}

class FrontendIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val active = Bool(OUTPUT)
  val req = Valid(new FrontendReq)
  val resp = Decoupled(new rocket.FrontendResp).flip
  val invalidate = Bool(OUTPUT)
}

class HwachaFrontend(implicit p: Parameters) extends HwachaModule()(p) with rocket.HasL1CacheParameters {
  val io = new Bundle {
    val vxu = new FrontendIO().flip
    val vru = new FrontendIO().flip
    val ptw = new rocket.TLBPTWIO()
    val mem = new uncore.ClientUncachedTileLinkIO
  }

  val icache = Module(new rocket.ICache)
  val tlb = Module(new rocket.TLB)

  // COLIN FIXME: v[x|r]u* signals look bad and are prone to errors, collating to array my be useful?
  val s1_vxu_pc_ = Reg(UInt())
  val s1_vxu_pc = s1_vxu_pc_ & SInt(-2) // discard LSB of PC (throughout the pipeline)
  val s1_vru_pc_ = Reg(UInt())
  val s1_vru_pc = s1_vru_pc_ & SInt(-2) // discard LSB of PC (throughout the pipeline)
  val s1_pc_ = Reg(UInt())
  val s1_pc = s1_pc_ & SInt(-2) // discard LSB of PC (throughout the pipeline)
  val s1_same_block = Reg(Bool())
  val s1_valid = Reg(init=Bool(false))
  val s1_vxu_valid = Reg(init=Bool(false))
  val s1_vru_valid = Reg(init=Bool(false))
  val s1_vxu_kill = Bool()
  val s1_vru_kill = Bool()
  val s1_vxu_stall = Bool()
  val s1_vru_stall = Bool()
  val s1_type = Reg(Bool())
  val s1_vxu_same_block = Reg(init=Bool(false))
  val s1_vru_same_block = Reg(init=Bool(false))

  val s2_vxu_pc = Reg(init=UInt(0x100))
  val s2_vru_pc = Reg(init=UInt(0x100))
  val s2_pc = Reg(init=UInt(0x100))
  val s2_xcpt_if = Reg(init=Bool(false))
  val s2_same_block = Reg(Bool())
  val s2_valid = Reg(init=Bool(false))
  val s2_valid_vxu = Reg(init=Bool(false))
  val s2_valid_vru = Reg(init=Bool(false))
  val s2_type = Reg(Bool())
  val s2_vxu_same_block = Reg(init=Bool(false))
  val s2_vru_same_block = Reg(init=Bool(false))

  val vxu_line_val = Reg(init=Bool(false))
  val vru_line_val = Reg(init=Bool(false))
  val vxu_line = Module(new Queue(new rocket.ICacheResp, 1, pipe=true))
  val vru_line = Module(new Queue(new rocket.ICacheResp, 1, pipe=true))

  val vxu_icmiss = s2_valid && s2_type && !vxu_line.io.deq.valid
  val vru_icmiss = s2_valid && !s2_type && !vru_line.io.deq.valid
  val icmiss = vxu_icmiss || vru_icmiss

  def end_of_line(pc: UInt) = {
    val instBytes = coreInstBits/8
    val instPerRow = rowBytes/instBytes
    pc(log2Up(rowBytes)-1, log2Up(instBytes)) === Bits((instPerRow-1))
  }

  // Stage 0
  // vxu pc gen
  val vxu_req_pc = Mux(io.vxu.req.valid, io.vxu.req.bits.pc, 
                     Mux(vxu_icmiss, s2_vxu_pc, 
                     Mux(!s1_vxu_valid || s1_vxu_kill, s1_vxu_pc, s1_vxu_pc+UInt(8))))

  val vxu_req_pc_rowbyte = vxu_req_pc & SInt(-rowBytes)
  val s1_vxu_pc_rowbyte = s1_vxu_pc & SInt(-rowBytes)
  val s2_vxu_pc_rowbyte = s2_vxu_pc & SInt(-rowBytes)
  val s1_pc_rowbyte = s1_pc & SInt(-rowBytes)
  val s2_pc_rowbyte = s2_pc & SInt(-rowBytes)

  val s0_vxu_n_match = s1_type && s1_valid &&
                     (vxu_req_pc_rowbyte === s1_pc_rowbyte)
  // line buffer matches
  val s0_vxu_inval_nn_match = s1_type && s1_valid && (vxu_req_pc_rowbyte != s1_pc_rowbyte)
  val s0_vxu_nn_match = vxu_line_val && !vxu_icmiss &&
                          (vxu_req_pc_rowbyte === s2_vxu_pc_rowbyte) &&
                          !s0_vxu_inval_nn_match

  //overall match
  val s0_vxu_same_block = s0_vxu_n_match || s0_vxu_nn_match

  // vru pc gen
  val vru_req_pc = Mux(io.vru.req.valid, io.vru.req.bits.pc,
                     Mux(vru_icmiss, s2_vru_pc, 
                     Mux(!s1_vru_valid || s1_vru_kill, s1_vru_pc, s1_vru_pc+UInt(8))))

  val vru_req_pc_rowbyte = vru_req_pc & SInt(-rowBytes)
  val s1_vru_pc_rowbyte = s1_vru_pc & SInt(-rowBytes)
  val s2_vru_pc_rowbyte = s2_vru_pc & SInt(-rowBytes)
  //next req matches
  val s0_vru_n_match = !s1_type && s1_valid &&
                     (vru_req_pc_rowbyte === s1_pc_rowbyte)
  //line buffer matches
  val s0_vru_inval_nn_match = !s1_type && s1_valid && (vru_req_pc_rowbyte != s1_pc_rowbyte)
  val s0_vru_nn_match = vru_line_val && !vru_icmiss &&
                          (vru_req_pc_rowbyte === s2_vru_pc_rowbyte) &&
                          !s0_vru_inval_nn_match

  //overall match
  val s0_vru_same_block = s0_vru_n_match || s0_vru_nn_match
              
  // COLIN FIXME: make sure a loss of arb during redirect never loses request
  // arbitrate between vxu and vru
  val vxu_active = io.vxu.active
  val vru_active = io.vru.active
  val req_pc   = Mux(!s0_vxu_same_block && !s1_vxu_stall, vxu_req_pc, vru_req_pc)
  val req_type = !s0_vxu_same_block && !s1_vxu_stall
  val redirect = req_type && io.vxu.req.valid || !req_type && io.vru.req.valid
  val vxu_make_req = req_type && (!s0_vxu_same_block && vxu_active)
  val vru_make_req = !req_type && (!s0_vru_same_block && vru_active)
  val req_val = redirect || vxu_make_req || vru_make_req

  val vxu_stall = io.vxu.resp.valid && !io.vxu.resp.ready
  val vru_stall = io.vru.resp.valid && !io.vru.resp.ready
  val s0_stall = req_type && vxu_stall || !req_type && vru_stall
  val s1_stall = s1_type && vxu_stall || !s1_type && vru_stall

  val s1_valid_req = (s1_type && s1_vxu_valid && !s1_vxu_same_block) ||
                    (!s1_type && s1_vru_valid && !s1_vru_same_block)

  s1_vxu_stall := vxu_stall && !vxu_icmiss && s1_vxu_same_block
  s1_vru_stall := vru_stall && !vru_icmiss && s1_vru_same_block
  s1_vxu_kill := vxu_icmiss || (vxu_stall && !vxu_icmiss) || end_of_line(s2_vxu_pc) && !s1_type || io.vxu.req.valid
  s1_vru_kill := vru_icmiss || (vru_stall && !vru_icmiss) || end_of_line(s2_vru_pc) && s1_type || io.vru.req.valid
  val kill_s1 = s1_type && s1_vxu_kill || !s1_type && s1_vru_kill 

  icache.io.mem <> io.mem
  icache.io.req.valid := !s0_stall && req_val
  icache.io.req.bits.idx := req_pc
  icache.io.invalidate := io.vxu.invalidate//only vxu invalidates
  icache.io.req.bits.ppn := tlb.io.resp.ppn
  icache.io.req.bits.kill := tlb.io.resp.miss || io.ptw.invalidate || kill_s1
  icache.io.resp.ready := !s1_stall && s1_valid_req

  // stage 1
  s1_vxu_pc_ := vxu_req_pc
  s1_vxu_valid := vxu_active && (req_type && !s0_vxu_same_block || s0_vxu_same_block)
  s1_vxu_same_block := s0_vxu_same_block

  s1_vru_pc_ := vru_req_pc
  s1_vru_valid := vru_active && (!req_type && !s0_vru_same_block || s0_vru_same_block)
  s1_vru_same_block := s0_vru_same_block

  s1_pc_ := req_pc
  s1_valid := vxu_make_req || vru_make_req
  s1_type := req_type

  tlb.io.ptw <> io.ptw
  tlb.io.req.valid := !s1_stall && !icmiss && !kill_s1
  tlb.io.req.bits.vpn := s1_pc >> UInt(pgIdxBits)
  tlb.io.req.bits.asid := UInt(0)
  tlb.io.req.bits.passthrough := Bool(false)
  tlb.io.req.bits.instruction := Bool(true)
  tlb.io.req.bits.store := Bool(false)

  vxu_line.io.enq.bits := icache.io.resp.bits
  vxu_line.io.enq.valid := icache.io.resp.valid && s1_type
  vru_line.io.enq.bits := icache.io.resp.bits
  vru_line.io.enq.valid := icache.io.resp.valid && !s1_type
  icache.io.resp.ready := (vxu_line.io.enq.ready && s1_type) ||
                          (vru_line.io.enq.ready && !s1_type)
  
  // stage 2
  when (io.vxu.invalidate || vxu_line.io.count === UInt(0)) {
    vxu_line_val := Bool(false)
  }
  // only vxu invalidates
  when (io.vxu.invalidate || vru_line.io.count === UInt(0)) {
    vru_line_val := Bool(false)
  }
  when (!(vxu_stall && s1_vxu_kill)) {
    s2_vxu_same_block := s1_vxu_same_block
    s2_valid_vxu := s1_vxu_valid && !io.vxu.req.valid && !(s1_vxu_pc === s2_vxu_pc && io.vxu.resp.fire())
    when (!vxu_icmiss) {
      s2_vxu_pc := s1_vxu_pc
      vxu_line_val := Bool(true)
    }
  }
  when (!(vru_stall && s1_vru_kill)) {
    s2_vru_same_block := s1_vru_same_block
    s2_valid_vru := s1_vru_valid && !io.vru.req.valid && !(s1_vru_pc === s2_vru_pc && io.vru.resp.fire())
    when (!vru_icmiss) {
      s2_vru_pc := s1_vru_pc
      vru_line_val := Bool(true)
    }
  }
    s2_valid := s1_valid && !kill_s1
    s2_type := s1_type
  when (!s1_stall) {
    when (!icmiss) {
      s2_pc := s1_pc
      s2_xcpt_if := tlb.io.resp.xcpt_if
    }
  }

  io.vxu.resp.valid := s2_valid_vxu && vxu_line.io.deq.valid
  io.vru.resp.valid := s2_valid_vru && vru_line.io.deq.valid
  io.vxu.resp.bits.pc := s2_vxu_pc & SInt(-coreInstBytes) // discard PC LSBs
  io.vru.resp.bits.pc := s2_vru_pc & SInt(-coreInstBytes) // discard PC LSBs

  vxu_line.io.deq.ready := io.vxu.resp.fire() && end_of_line(s2_vxu_pc)
  vru_line.io.deq.ready := io.vru.resp.fire() && end_of_line(s2_vru_pc)

  val vxu_datablock = vxu_line.io.deq.bits.datablock
  val vru_datablock = vru_line.io.deq.bits.datablock
  val vxu_fetch_data = vxu_datablock >> (s2_vxu_pc(log2Up(rowBytes)-1,log2Up(fetchWidth*coreInstBytes)) << UInt(log2Up(fetchWidth*coreInstBits)))
  val vru_fetch_data = vru_datablock >> (s2_vru_pc(log2Up(rowBytes)-1,log2Up(fetchWidth*coreInstBytes)) << UInt(log2Up(fetchWidth*coreInstBits)))
  for (i <- 0 until fetchWidth) {
    io.vxu.resp.bits.data(i) := vxu_fetch_data(i*coreInstBits+coreInstBits-1, i*coreInstBits)
    io.vru.resp.bits.data(i) := vru_fetch_data(i*coreInstBits+coreInstBits-1, i*coreInstBits)
  }

  val all_ones = UInt((1 << (fetchWidth+1))-1)
  io.vxu.resp.bits.mask := all_ones//not using btb
  io.vru.resp.bits.mask := all_ones//not using btb

  io.vxu.resp.bits.xcpt_if := s2_xcpt_if
  io.vru.resp.bits.xcpt_if := s2_xcpt_if

}

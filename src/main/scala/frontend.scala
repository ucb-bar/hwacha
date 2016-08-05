package hwacha

import Chisel._
import cde.Parameters

class FrontendReq(implicit p: Parameters) extends rocket.CoreBundle()(p) {
  val pc = UInt(width = vaddrBits+1)
}

class FrontendIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val active = Bool(OUTPUT)
  val req = Valid(new FrontendReq)
  val resp = Decoupled(new rocket.FrontendResp).flip
  val invalidate = Bool(OUTPUT)
}

class MiniFrontend(implicit p: Parameters) extends HwachaModule()(p) with rocket.HasL1CacheParameters {
  val io = new Bundle {
    val front = new FrontendIO().flip
    val back = new Bundle {
      val s0_req = Decoupled(new FrontendReq)
      val s1_kill = Bool(OUTPUT)
      val s1_resp = Valid(new rocket.ICacheResp).flip
      val s1_tlb = new rocket.TLBResp().flip
    }
  }

  val s1_valid = Reg(init=Bool(false))
  val s1_pc_ = Reg(UInt())
  val s1_pc = ~(~s1_pc_ | UInt(coreInstBytes-1)) // discard PC LSBS (this propagates down the pipeline)
  val s1_same_block = Reg(init=Bool(false))
  val s1_req_valid = Reg(init=Bool(false))

  val s2_valid = Reg(init=Bool(false))
  val s2_pc = Reg(UInt())
  val s2_xcpt_if = Reg(init=Bool(false))
  val s2_line = Module(new Queue(new rocket.ICacheResp, 1, pipe=true))

  s1_req_valid := io.back.s0_req.fire()

  val icmiss = s2_valid && !s2_line.io.deq.valid
  val s2_replay = icmiss
  val s1_replay = s1_valid && !s1_same_block && !s1_req_valid

  val stall = !io.front.req.valid && (io.front.resp.valid && !io.front.resp.ready || !io.front.active)
  val s1_kill = io.front.req.valid || icmiss || s1_replay
  val s0_npc = s1_pc + UInt(coreInstBytes)
  val s0_same_block =
    !s1_kill && ((s0_npc & UInt(rowBytes)) === (s1_pc & UInt(rowBytes)))
  val s0_req_valid = !stall && !s0_same_block

  io.back.s0_req.valid := s0_req_valid
  io.back.s0_req.bits.pc :=
    Mux(io.front.req.valid, io.front.req.bits.pc,
      Mux(s2_replay, s2_pc,
        Mux(s1_replay, s1_pc,
          s0_npc)))
  io.back.s1_kill := s1_req_valid && s1_kill

  when (!stall) {
    s1_valid := s0_req_valid
    s1_pc_ := io.back.s0_req.bits.pc
    s1_same_block := s0_same_block && !(s1_req_valid && io.back.s1_tlb.miss)

    s2_valid := !s1_kill
    when (!s1_kill) {
      s2_pc := s1_pc
      s2_xcpt_if := s1_req_valid && io.back.s1_tlb.xcpt_if
    }
  }

  s2_line.io.enq.bits := io.back.s1_resp.bits
  s2_line.io.enq.valid := s1_req_valid && io.back.s1_resp.valid
  s2_line.io.deq.ready := !stall && !(s1_valid && s1_same_block)

  io.front.resp.valid := s2_valid && (s2_line.io.deq.valid || s2_xcpt_if)
  io.front.resp.bits.pc := s2_pc
  io.front.resp.bits.xcpt_if := s2_xcpt_if

  require(fetchWidth * coreInstBytes <= rowBytes)
  val fetch_data =
    if (fetchWidth * coreInstBytes == rowBytes) s2_line.io.deq.bits.datablock
    else s2_line.io.deq.bits.datablock >> (s2_pc(log2Up(rowBytes)-1,log2Up(fetchWidth*coreInstBytes)) << log2Up(fetchWidth*coreInstBits))

    io.front.resp.bits.data := fetch_data
}

class HwachaFrontend(implicit p: Parameters) extends HwachaModule()(p) with rocket.HasL1CacheParameters {
  val io = new Bundle {
    val vxu = new FrontendIO().flip
    val vru = new FrontendIO().flip
    val ptw = new rocket.TLBPTWIO()
    val mem = new uncore.tilelink.ClientUncachedTileLinkIO
  }

  val icache = Module(new rocket.ICache(latency = 1))
  val tlb = Module(new rocket.TLB)
  val vxu = Module(new MiniFrontend)
  val vru = Module(new MiniFrontend)
  val req_arb = Module(new Arbiter(new FrontendReq, 2))

  vxu.io.front <> io.vxu
  vru.io.front <> io.vru

  req_arb.io.in(1) <> vxu.io.back.s0_req
  req_arb.io.in(0) <> vru.io.back.s0_req
  private val req = req_arb.io.out

  icache.io.req.valid := req.valid
  icache.io.req.bits.addr := req.bits.pc
  icache.io.invalidate := Bool(false)
  icache.io.s1_ppn := tlb.io.resp.ppn
  icache.io.s1_kill :=
    vxu.io.back.s1_kill || vru.io.back.s1_kill ||
    tlb.io.resp.miss || tlb.io.resp.xcpt_if || io.ptw.invalidate
  icache.io.s2_kill := Bool(false)

  tlb.io.req.valid := Reg(next=req.valid)
  tlb.io.req.bits.vpn := RegEnable(req.bits.pc, req.valid) >> UInt(pgIdxBits)
  tlb.io.req.bits.passthrough := Bool(false)
  tlb.io.req.bits.instruction := Bool(true)
  tlb.io.req.bits.store := Bool(false)

  req.ready := Bool(true)
  icache.io.resp.ready := Bool(true)

  vxu.io.back.s1_resp <> icache.io.resp
  vru.io.back.s1_resp <> icache.io.resp
  vxu.io.back.s1_tlb <> tlb.io.resp
  vru.io.back.s1_tlb <> tlb.io.resp

  io.ptw <> tlb.io.ptw
  io.mem <> icache.io.mem
}

package hwacha

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket.{ICacheParams, MStatus, TLBConfig, TLBResp}

class FrontendResp(icacheParams: ICacheParams)(implicit p: Parameters) extends HwachaBundle()(p) {
  val pc = UInt(vaddrBitsExtended.W)  // ID stage PC
  val data = UInt((icacheParams.fetchBytes * 8).W)
  val pf = Bool()
}

class FrontendReq(implicit p: Parameters) extends freechips.rocketchip.tile.CoreBundle()(p) {
  val pc = UInt((vaddrBits+1).W)
  val status = new MStatus
}

class FrontendIO(cacheParams: ICacheParams)(implicit p: Parameters) extends HwachaBundle()(p) {
  val active = Output(Bool())
  val req = Valid(new FrontendReq)
  val resp = Flipped(Decoupled(new FrontendResp(cacheParams)))
  val invalidate = Output(Bool())

}

class MiniFrontend(val cacheParams: ICacheParams)(implicit p: Parameters) extends HwachaModule()(p) with freechips.rocketchip.tile.HasL1CacheParameters {
  val io = IO(new Bundle {
    val front = Flipped(new FrontendIO(cacheParams))
    val back = new Bundle {
      val s0_req = Decoupled(new FrontendReq)
      val s1_kill = Output(Bool())
      //TODO: make sure we dont need double Valid nesting
      val s1_resp = Input(Valid(UInt((cacheParams.fetchBytes * 8).W)))
      val s1_tlb = Flipped(new TLBResp())
    }
  })

  val s1_valid = RegInit(false.B)
  val s1_pc_ = Reg(UInt())
  val s1_status = Reg(new MStatus)
  val s1_pc = ~(~s1_pc_ | (HwachaElementInstBytes-1).U) // discard PC LSBS (this propagates down the pipeline)
  val s1_same_block = RegInit(false.B)
  val s1_req_valid = RegInit(false.B)

  val s2_valid = RegInit(false.B)
  val s2_pc = Reg(UInt())
  val s2_status = Reg(new MStatus)
  val s2_xcpt_if = RegInit(false.B)
  val s2_line = Module(new Queue(UInt((cacheParams.fetchBytes * 8).W), 1, pipe=true))

  s1_req_valid := io.back.s0_req.fire

  val icmiss = s2_valid && !s2_line.io.deq.valid
  val s2_replay = icmiss
  val s1_replay = s1_valid && !s1_same_block && !s1_req_valid

  val stall = !io.front.req.valid && (io.front.resp.valid && !io.front.resp.ready || !io.front.active)
  val s1_kill = io.front.req.valid || icmiss || s1_replay
  val s0_npc = s1_pc + HwachaElementInstBytes.U
  val s0_same_block =
    !s1_kill && ((s0_npc & rowBytes.U) === (s1_pc & rowBytes.U))
  val s0_req_valid = !stall && !s0_same_block

  io.back.s0_req.valid := s0_req_valid
  io.back.s0_req.bits.status :=
    Mux(io.front.req.valid, io.front.req.bits.status,
      Mux(s2_replay, s2_status,
        Mux(s1_replay, s1_status,
          s1_status)))// next status is same as last status w/o new req
  io.back.s0_req.bits.pc :=
    Mux(io.front.req.valid, io.front.req.bits.pc,
      Mux(s2_replay, s2_pc,
        Mux(s1_replay, s1_pc,
          s0_npc)))
  io.back.s1_kill := s1_req_valid && s1_kill

  when (!stall) {
    s1_valid := s0_req_valid
    s1_pc_ := io.back.s0_req.bits.pc
    s1_status := io.back.s0_req.bits.status
    s1_same_block := s0_same_block && !(s1_req_valid && io.back.s1_tlb.miss)

    s2_valid := !s1_kill
    when (!s1_kill) {
      s2_pc := s1_pc
      s2_status := s1_status
      s2_xcpt_if := s1_req_valid && io.back.s1_tlb.pf.inst
    }
  }

  s2_line.io.enq.bits := io.back.s1_resp.bits
  s2_line.io.enq.valid := s1_req_valid && io.back.s1_resp.valid
  s2_line.io.deq.ready := !stall && !(s1_valid && s1_same_block)

  io.front.resp.valid := s2_valid && (s2_line.io.deq.valid || s2_xcpt_if)
  io.front.resp.bits.pc := s2_pc
  io.front.resp.bits.pf := s2_xcpt_if

  require(cacheParams.fetchBytes <= rowBytes)
  val fetch_data =
    if (cacheParams.fetchBytes == rowBytes) s2_line.io.deq.bits
    else s2_line.io.deq.bits >> (s2_pc(log2Up(rowBytes)-1,log2Up(cacheParams.fetchBytes)) << log2Up(cacheParams.fetchBytes*8))

    io.front.resp.bits.data := fetch_data
}

class HwachaFrontend(implicit p : Parameters) extends LazyModule {
  lazy val module = new HwachaFrontendModule(this)
  val cacheParams = p(HwachaIcacheKey)

  val icache = LazyModule(new freechips.rocketchip.rocket.ICache(cacheParams, staticIdForMetadataUseOnly = 0))

  val masterNode = icache.masterNode
}

class HwachaFrontendModule(outer: HwachaFrontend)(implicit p: Parameters) extends LazyModuleImp(outer)
  with freechips.rocketchip.tile.HasL1CacheParameters with UsesHwachaParameters {
  implicit val edge = outer.masterNode.edges.out.head
  val cacheParams = outer.cacheParams

  val io = IO(new Bundle {
    val vxu = Flipped(new FrontendIO(cacheParams))
    val vru = Flipped(new FrontendIO(cacheParams))
    val ptw = new freechips.rocketchip.rocket.TLBPTWIO()
  })
  val icache = outer.icache.module
  val tlb = Module(new freechips.rocketchip.rocket.TLB(instruction = true, lgMaxSize = log2Ceil(cacheParams.fetchBytes), TLBConfig(nSets=nptlb, nWays=1, nSectors=1))(edge, p))
  val vxu = Module(new MiniFrontend(cacheParams))
  val vru = Module(new MiniFrontend(cacheParams))
  val req_arb = Module(new Arbiter(new FrontendReq, 2))

  vxu.io.front <> io.vxu
  vru.io.front <> io.vru

  req_arb.io.in(1) <> vxu.io.back.s0_req
  req_arb.io.in(0) <> vru.io.back.s0_req
  private val req = req_arb.io.out

  val s1_pc = RegEnable(req.bits.pc, req.valid)
  val s2_pc = Reg(s1_pc)

  icache.io.req.valid := req.valid
  icache.io.req.bits.addr := req.bits.pc
  icache.io.invalidate := false.B
  icache.io.s1_paddr := tlb.io.resp.paddr
  icache.io.s2_vaddr := s2_pc
  icache.io.s1_kill :=
    vxu.io.back.s1_kill || vru.io.back.s1_kill ||
    tlb.io.resp.miss || tlb.io.resp.pf.inst
    //TODO: check ptw result
  icache.io.s2_kill := false.B
  icache.io.s2_cacheable := false.B
  icache.io.s2_prefetch := false.B
  icache.io.clock_enabled := true.B

  tlb.io.req.valid := RegNext(req.valid)
  tlb.io.req.bits.vaddr := s1_pc
  tlb.io.req.bits.passthrough := false.B
  tlb.io.req.bits.size := log2Ceil(cacheParams.fetchBytes).U
  tlb.io.sfence.valid := false.B

  req.ready := true.B

  vxu.io.back.s1_resp.valid <> icache.io.resp.valid
  vru.io.back.s1_resp.valid <> icache.io.resp.valid
  vxu.io.back.s1_resp.bits <> icache.io.resp.bits.data
  vru.io.back.s1_resp.bits <> icache.io.resp.bits.data
  vxu.io.back.s1_tlb <> tlb.io.resp
  vru.io.back.s1_tlb <> tlb.io.resp

  io.ptw <> tlb.io.ptw
  tlb.io.ptw.status := req.bits.status
}

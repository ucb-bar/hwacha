package hwacha

import chisel3._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._

/*
 * TODO:
 * - when the decoded mem op queue overflows, should we discard what's at the
 * head or at the tail?
 * - counter overflows?
 * - vmcs assert
 */

class VRUCtrlSigs(implicit p: Parameters) extends HwachaBundle()(p) {
  val opwidth = UInt(3.W)
  val ls = Bool()
  val prefetchable = Bool()
  val stop = Bool()

  def decode(inst: UInt) = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, VRUDecodeTable.default, VRUDecodeTable.table)
    val sigs = Seq(opwidth, ls, prefetchable, stop)
    sigs zip decoder map {case(s,d) => s := d}

    this
  }
}

object VRUDecodeTable {
  import HwachaElementInstructions._
  import freechips.rocketchip.util._ //implicit uint to bitpat

  /* list contains:
   * opwidth (2 bits, stored as 2^opwidth)
   * store (false for load, true for store)
   * prefetchable (0 for VRU ignore, 1 for VRU use)
   * STOP (end of vf block)
   */
  val default: List[BitPat] = List(0.U, N, N, N)

  val table: Array[(BitPat, List[BitPat])] = Array(
    VSTOP -> List(X, X, N, Y),
    VLD   -> List(UInt(3), N, Y, N),
    VSD   -> List(UInt(3), Y, Y, N),
    VLW   -> List(2.U, N, Y, N),
    VLWU  -> List(2.U, N, Y, N),
    VSW   -> List(2.U, Y, Y, N),
    VLH   -> List(1.U, N, Y, N),
    VLHU  -> List(1.U, N, Y, N),
    VSH   -> List(1.U, Y, Y, N),
    VLB   -> List(0.U, N, Y, N),
    VLBU  -> List(0.U, N, Y, N),
    VSB   -> List(0.U, Y, Y, N)
  )
}

class DecodedMemOp(implicit p: Parameters) extends HwachaBundle()(p) {
  // 64 is hardcoded in scalar-unit
  val addr = UInt(width = 64)
  val curr_vlen = UInt(width = bMLVLen)
  val opwidth = UInt(width=2) // byte = 0, half = 1, word = 2, double = 3
  val ls = UInt(width = 1) // load = 0, store = 1
}

/*
 * Formerly called ThrottleManager
 *
 * This module regulates the amount that the VRU can runahead by stalling
 * the decoding of VF blocks.
 *
 * It also measures the number of vf blocks that we are ahead/behind to
 * facilitate skipping vf blocks to purposefully get ahead
 *
 * To deal with predication, runahead is always tracked at VF block granularity.
 * The runahead counter is always brought back into sync at the end of each
 * vf block, even with predication.
 */
class RunaheadManager(resetSignal: Bool = null)(implicit p: Parameters) extends HwachaModule(_reset = resetSignal)(p) {
  val entrywidth = 20 // width of per-vf-block bytes loaded/stored counter
  val throttleQueueDepth = 10

  val io = IO(new Bundle {
    // runahead distance throttling
    val enq = Flipped(Decoupled(UInt(entrywidth.W)))
    val vf_done_vxu = Input(Bool())
    val stall_prefetch = Output(Bool())

    // vf block skipping
    val vf_fire = Input(Bool())
    val vf_skip = Output(Bool())
  })

  val skipamt = p(HwachaVRUEarlyIgnore)
  val runahead_vf_count = RegInit(0.S(32.W))

  io.vf_skip := runahead_vf_count < SInt(skipamt)

  // number of bytes the VRU can runahead
  val MAX_RUNAHEAD = p(HwachaVRUMaxRunaheadBytes)

  // this queue tracks the number of bytes loaded/stored per vf block
  // in flight (where in-flight = sent to prefetch stage, but not acked by
  // vxu)
  val bytesq = Module(new Queue(UInt(width=entrywidth), throttleQueueDepth))
  bytesq.suggestName("bytesqInst")

  // the number of bytes the prefetcher is ahead of the vxu
  val runahead_bytes_count = RegInit(UInt(0, width=32))

  // signal to the decode stage that we should stop decoding VF blocks because
  // we have run too far ahead
  io.stall_prefetch := runahead_bytes_count > UInt(MAX_RUNAHEAD)

  // queue does not need to worry about ignoring entries when we fall behind,
  // because the vf blocks will never enter decode
  bytesq.io.enq.valid := io.enq.valid && !(io.vf_done_vxu && !bytesq.io.deq.valid)
  io.enq.ready := bytesq.io.enq.ready
  bytesq.io.enq.bits := io.enq.bits

  // only one of these can be true at a time
  val skipped_block = io.vf_fire && io.vf_skip
  val accepted_block = io.enq.fire
  assert(!(skipped_block && accepted_block), "VRU attempted to simultaneously enqueue and skip VF block")
  val increment_vf_count = skipped_block || accepted_block

  runahead_vf_count := runahead_vf_count + increment_vf_count.zext - io.vf_done_vxu.zext

  val increment_bytes_necessary = bytesq.io.enq.valid && bytesq.io.enq.ready
  val decrement_bytes_necessary = bytesq.io.deq.valid && io.vf_done_vxu

  bytesq.io.deq.ready := io.vf_done_vxu

  val next_increment_bytes_value = Mux(increment_bytes_necessary, bytesq.io.enq.bits, 0.U)
  val next_decrement_bytes_value = Mux(decrement_bytes_necessary, bytesq.io.deq.bits, 0.U)
  runahead_bytes_count := runahead_bytes_count + next_increment_bytes_value - next_decrement_bytes_value
}

/*
 * The PrefetchUnit is actually responsible for sending out prefetches from
 * the decodedMemOpQueue.
 *
 * The only throttling applied here is based on the allowed number of
 * outstanding requests
 */
class PrefetchUnit(edge: TLEdgeOut, resetSignal: Bool = null)(implicit p: Parameters) extends HwachaModule(_reset = resetSignal)(p)
  with MemParameters {

  val io = IO(new Bundle {
    val memop = Flipped(Decoupled(new DecodedMemOp))
    val dmem = TLBundle(edge.bundle)
  })
  val tag_count = RegInit(5.W)//tlMasterXactIdBits))

  val tlBlockAddrOffset =  tlByteAddrBits
  //val tlBlockAddrOffset = tlBeatAddrBits + tlByteAddrBits
  val req_addr = io.memop.bits.addr(bPAddr-1, tlBlockAddrOffset)
  val req_vlen = io.memop.bits.curr_vlen // vector len
  val req_opwidth = io.memop.bits.opwidth // byte = 0 ... double = 3
  val req_ls = io.memop.bits.ls // load = 0, store = 1
  val prefetch_ip = RegInit(false.B)

  val vec_len_bytes = req_vlen << req_opwidth
  val num_blocks_pf = vec_len_bytes >> tlBlockAddrOffset

  // TODO: calculate width
  val pf_ip_counter = RegInit(UInt(0, width=20))

  // TODO: log2Up for width?
  val MAX_OUTSTANDING_PREFETCHES = UInt(p(HwachaVRUMaxOutstandingPrefetches), width=10) // approximately 1/3 of the units
  val remaining_allowed_prefetches = RegInit(MAX_OUTSTANDING_PREFETCHES)

  assert(remaining_allowed_prefetches <= MAX_OUTSTANDING_PREFETCHES, "VRU: THROTTLE TOO LARGE\n")

//TODO: fix-up once TL2 supports prefetches (Intent message type)
/*
  io.dmem.a.bits := Mux(req_ls === 0.U,
    edge.GetPrefetch(tag_count, req_addr+pf_ip_counter),
    edge.PutPrefetch(tag_count, req_addr+pf_ip_counter))
*/

  io.dmem.a.valid := false.B
  io.memop.ready := false.B

  when (io.memop.valid && !prefetch_ip && io.dmem.a.ready) {
    prefetch_ip := true.B
    pf_ip_counter := 0.U
  }

  val movecond1 = prefetch_ip &&
    pf_ip_counter < (num_blocks_pf-1.U) && io.dmem.a.ready &&
    remaining_allowed_prefetches > 0.U


  when (movecond1) {
    pf_ip_counter := pf_ip_counter + 1.U
    tag_count := tag_count + 1.U
    when (!io.dmem.d.valid) {
      remaining_allowed_prefetches := remaining_allowed_prefetches - 1.U
    }
  }

  val movecond2 = prefetch_ip &&
    pf_ip_counter === (num_blocks_pf - 1.U) && io.dmem.a.ready &&
    remaining_allowed_prefetches > 0.U

  when (movecond2) {
    pf_ip_counter := 0.U
    prefetch_ip := false.B
    tag_count := tag_count + 1.U
    io.memop.ready := true.B
    when (!io.dmem.d.valid) {
      remaining_allowed_prefetches := remaining_allowed_prefetches - 1.U
    }
  }

  io.dmem.a.valid := prefetch_ip && remaining_allowed_prefetches > 0.U
  io.dmem.d.ready := true.B
  when (io.dmem.d.valid) {
    when (!(movecond1 || movecond2)) {
      remaining_allowed_prefetches := remaining_allowed_prefetches + 1.U
    }
  }
}


/*
 * This module is notified when a vf command is received, then:
 *
 * 1) gets the instructions in the VF block from the icache
 *
 * 2) determines the total number of bytes loaded/stored in the VF block
 * (repeat loads/stores to the same address will be double counted)
 *
 * 3) As it sees loads/stores, puts them into the decoded memop queue for use
 * by the PrefetchUnit. There is no backpressure here, we drop entries if
 * we don't have space to remember them
 *
 * 4) If the runahead manager determines that we aren't too far ahead, it
 * enqueues tracking information in the runahead manager's tracking queue,
 * otherwise, we halt vf decode (and thus stop accepting any more rocc commands)
 * until the runahead manager determines that it is okay to continue
 *
 */
class VRUFrontend(resetSignal: Bool = null)(implicit p: Parameters) extends HwachaModule(_reset = resetSignal)(p) {

  val io = IO(new Bundle {
    val imem = new FrontendIO(p(HwachaIcacheKey))

    val fire_vf = Input(Bool())
    val fetch_pc = Input(UInt())
    val fetch_status = Input(new freechips.rocketchip.rocket.MStatus())
    val vf_active = Output(Bool())
    val vf_complete_ack = Input(Bool())

    val memop = Decoupled(new DecodedMemOp)
    val vlen = Input(UInt())

    val aaddr = Output(UInt())
    val adata = Input(UInt())
  })

  val vf_active = RegInit(false.B)
  io.vf_active := vf_active

  val runaheadman = Module(new RunaheadManager)
  runaheadman.suggestName("runaheadmanInst")
  runaheadman.io.vf_done_vxu := io.vf_complete_ack
  runaheadman.io.vf_fire := io.fire_vf

  /* Process the VF block if the runaheadmanager has not told us to skip it
   * For example at the start of a program or if there has been no prefetching
   * for a while and the prefetcher is all caught up
   */
  when (io.fire_vf && !runaheadman.io.vf_skip) {
    vf_active := true.B
  }

 /* pause if we get to a VSTOP but the runaheadman is not ready to accept
  * new entries in the vf block load/store byte tracking queue or if
  * the throttle manager says to throttle

  * it is okay to stop instruction decode when stop_at_VSTOP is true without
  * causing a lockup because either:

  * 1) the runaheadman queue is full, which means the vxu is behind anyway
  * 2) the vru is very far ahead in terms of runahead distance, which means
  * vxu is behind
  */
  val stop_at_VSTOP = !runaheadman.io.enq.ready || runaheadman.io.stall_prefetch

  // do a fetch
  io.imem.req.valid := io.fire_vf && !runaheadman.io.vf_skip //fixed
  io.imem.req.bits.pc := io.fetch_pc
  io.imem.req.bits.status := io.fetch_status
  io.imem.active := vf_active
  io.imem.invalidate := false.B

  val loaded_inst = io.imem.resp.bits.data; require(fetchWidth == 1)
  io.aaddr := loaded_inst(28, 24)
  io.memop.bits.addr := io.adata
  io.memop.bits.curr_vlen := io.vlen

  // decode logic from table
  val ctrl = Wire(new VRUCtrlSigs()).decode(loaded_inst)

  val imem_use_resp = io.imem.resp.valid && vf_active

  // as long as we're not stopped at a VSTOP (see above), accept instruction
  io.imem.resp.ready := !ctrl.stop || (ctrl.stop && !stop_at_VSTOP)

  io.memop.bits.opwidth := ctrl.opwidth
  io.memop.bits.ls := ctrl.ls
  io.memop.valid := ctrl.prefetchable && imem_use_resp

  val current_ls_bytes = RegInit(UInt(0, width=runaheadman.entrywidth))
  current_ls_bytes := Mux(ctrl.prefetchable && imem_use_resp,
    current_ls_bytes + (io.vlen << ctrl.opwidth),
    Mux(ctrl.stop && !stop_at_VSTOP && imem_use_resp, 0.U, current_ls_bytes)
  )

  runaheadman.io.enq.bits := current_ls_bytes
  runaheadman.io.enq.valid := ctrl.stop && !runaheadman.io.stall_prefetch && imem_use_resp

  when (ctrl.stop && !stop_at_VSTOP && imem_use_resp) {
      vf_active := false.B
  }
}


class VRURoCCUnit(implicit p: Parameters) extends HwachaModule()(p) {
  import Commands._

  val io = IO(new Bundle {
    val cmdq = Flipped(new CMDQIO())
    val fire_vf = Output(Bool())
    val vf_active = Input(Bool())
    val vlen = Output(UInt())
    val aaddr = Input(UInt())
    val adata = Output(UInt())
  })

  // addr regfile
  val arf = Mem(32, UInt(64.W))
  val vlen = RegInit(0.U(bMLVLen.W))
  io.vlen := vlen
  io.adata := arf(io.aaddr)

  val decode_vmca    = io.cmdq.cmd.bits === CMD_VMCA
  val decode_vsetcfg = io.cmdq.cmd.bits === CMD_VSETCFG
  val decode_vsetvl  = io.cmdq.cmd.bits === CMD_VSETVL
  val decode_vf      = io.cmdq.cmd.bits === CMD_VF
  val decode_vft     = io.cmdq.cmd.bits === CMD_VFT

  val deq_imm = decode_vmca || decode_vf || decode_vft || decode_vsetvl || decode_vsetcfg
  val deq_rd  = decode_vmca
  val deq_status = decode_vf || decode_vft

  val mask_imm_valid = !deq_imm || io.cmdq.imm.valid
  val mask_rd_valid  = !deq_rd  || io.cmdq.rd.valid
  val mask_status_valid  = !deq_status  || io.cmdq.status.valid

  def fire_cmdq(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      !io.vf_active,
      io.cmdq.cmd.valid,
      mask_imm_valid,
      mask_rd_valid,
      mask_status_valid
    )
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  io.fire_vf := fire_cmdq(null, decode_vf)

  io.cmdq.cmd.ready := fire_cmdq(io.cmdq.cmd.valid)
  io.cmdq.imm.ready := fire_cmdq(mask_imm_valid, deq_imm)
  io.cmdq.rd.ready := fire_cmdq(mask_rd_valid, deq_rd)
  io.cmdq.status.ready := fire_cmdq(mask_status_valid, deq_status)

  when (fire_cmdq(null, decode_vmca)) {
    arf(io.cmdq.rd.bits) := io.cmdq.imm.bits
  }

  when (fire_cmdq(null, decode_vsetcfg)) {
    vlen := 0.U
  }

  when (fire_cmdq(null, decode_vsetvl)) {
    vlen := io.cmdq.imm.bits
  }

}

class VRUCounterIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val memOps = UInt(width = log2Up(10))
}

class VRU(implicit p: Parameters) extends LazyModule {
  lazy val module = new VRUModule(this)
  val masterNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = "HwachaVRU", sourceId = IdRange(0,5))))))
}
class VRUModule(outer: VRU)(implicit p: Parameters) extends LazyModuleImp(outer)
  with MemParameters {
  import Commands._
  val (dmem, edge) = outer.masterNode.out.head
  val io = IO(new Bundle {
    // to is implicit, -> imem
    val imem = new FrontendIO(p(HwachaIcacheKey))
    val cmdq = Flipped(new CMDQIO())
    // shorten names
    val vf_complete_ack = Input(Bool())

    val counters = new VRUCounterIO
  })

  val vru_frontend = Module(new VRUFrontend)
  vru_frontend.suggestName("vru_frontendInst")
  val vru_rocc_unit = Module(new VRURoCCUnit)
  vru_rocc_unit.suggestName("vru_rocc_unitInst")

  // queue of load/store/addr/len to L2 prefetching stage
  val decodedMemOpQueue = Module(new Queue(new DecodedMemOp, 10))
  decodedMemOpQueue.suggestName("decodedMemOpQueueInst")

  io.counters.memOps := decodedMemOpQueue.io.count

  // wire up vru_rocc_unit, vec inst decode unit
  vru_rocc_unit.io.cmdq <> io.cmdq
  vru_rocc_unit.io.aaddr := vru_frontend.io.aaddr
  vru_rocc_unit.io.vf_active := vru_frontend.io.vf_active
  io.imem <> vru_frontend.io.imem
  vru_frontend.io.fire_vf := vru_rocc_unit.io.fire_vf
  vru_frontend.io.fetch_pc := io.cmdq.imm.bits
  vru_frontend.io.fetch_status := io.cmdq.status.bits
  decodedMemOpQueue.io.enq <> vru_frontend.io.memop
  vru_frontend.io.vf_complete_ack := io.vf_complete_ack
  vru_frontend.io.vlen := vru_rocc_unit.io.vlen
  vru_frontend.io.adata := vru_rocc_unit.io.adata

  // prefetch unit
  val prefetch_unit = Module(new PrefetchUnit(edge))
  prefetch_unit.suggestName("prefetch_unitInst")
  prefetch_unit.io.memop <> decodedMemOpQueue.io.deq
  dmem <> prefetch_unit.io.dmem

  //Tie off unused channels
  dmem.b.ready := true.B
  dmem.c.valid := false.B
  dmem.e.valid := false.B
}

package hwacha

import Chisel._
import Node._
import Constants._

class io_vmu_to_xcpt_handler extends Bundle 
{
  val no_pending_load_store = Bool(OUTPUT)
}

class VMU(resetSignal: Bool = null)(implicit conf: HwachaConfiguration) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val pf_vvaq = new io_vvaq().flip

    val lane_vvaq = new io_vvaq().flip
    val evac_vvaq = new io_vvaq().flip

    val lane_vsdq = new io_vsdq().flip
    val evac_vsdq = new io_vsdq().flip

    val lane_vldq = new io_vldq()

    val vxu_to_vmu = new io_vxu_to_vmu().flip

    val qcntp1 = UInt(INPUT, SZ_QCNT)
    val qcntp2 = UInt(INPUT, SZ_QCNT)

    val pending_store = Bool(OUTPUT)

    val dmem = new rocket.HellaCacheIO()(conf.dcache)

    val vtlb = new TLBIO
    val vpftlb = new TLBIO

    val xcpt_to_vmu = new io_xcpt_handler_to_vmu().flip()
    val evac_to_vmu = new io_evac_to_vmu().flip
    val vmu_to_xcpt  = new io_vmu_to_xcpt_handler()

    val irq = new io_vmu_to_irq_handler()

    val prec = Bits(INPUT, SZ_PREC)
  }

  val addr = Module(new VMUAddress)
  val ldata = Module(new VMULoadData)
  val sdata = Module(new VMUStoreData)
  val counters = Module(new VMUCounters)
  val memif = Module(new MemIF)
  val unpack = Module(new UnpackStore)


  // address unit
  addr.io.vvaq_pf <> io.pf_vvaq
  addr.io.vvaq_lane <> io.lane_vvaq
  addr.io.vvaq_evac <> io.evac_vvaq

  io.vtlb <> addr.io.vtlb
  io.vpftlb <> addr.io.vpftlb

  memif.io.vaq <> unpack.io.out.vaq
  unpack.io.in.vaq <> addr.io.vaq

  addr.io.irq <> io.irq

  addr.io.vvaq_lane_dec := io.vxu_to_vmu.vaq_valid
  // vvaq counts available space
  counters.io.vvaq_dec := addr.io.vvaq_do_enq
  counters.io.vvaq_inc := addr.io.vvaq_do_deq
  // vpaq counts occupied space
  counters.io.vpaq_inc := addr.io.vpaq_do_enq
  counters.io.vpaq_dec := addr.io.vpaq_do_deq
  // vpasdq counts occupied space
  counters.io.vpasdq_inc := addr.io.vpaq_do_enq_vsdq
  counters.io.vpaq_qcnt := addr.io.vpaq_qcnt
  addr.io.vvaq_watermark := counters.io.vvaq_watermark
  addr.io.vpaq_watermark := counters.io.vpaq_watermark
  addr.io.vsreq_watermark := counters.io.vsreq_watermark
  addr.io.vlreq_watermark := counters.io.vlreq_watermark


  // load data unit
  io.lane_vldq <> ldata.io.vldq_lane

  ldata.io.vldq <> memif.io.vldq
  memif.io.vldq_rtag <> ldata.io.vldq_rtag

  ldata.io.qcnt := io.qcntp1
  // vlreq counts available space
  counters.io.vlreq_inc := io.lane_vldq.ready
  counters.io.vlreq_dec :=
    io.vxu_to_vmu.vaq_valid && (
      is_mcmd_load(io.vxu_to_vmu.vaq_cmd) ||
      is_mcmd_amo(io.vxu_to_vmu.vaq_cmd)
    )


  // store data unit
  sdata.io.vsdq_lane <> io.lane_vsdq
  sdata.io.vsdq_evac <> io.evac_vsdq

  memif.io.vsdq <> unpack.io.out.vsdq
  unpack.io.in.vsdq <> sdata.io.vsdq

  sdata.io.vsdq_lane_dec := io.vxu_to_vmu.vsdq_valid
  // vsdq counts available space
  counters.io.vsdq_inc := sdata.io.vsdq_do_deq
  counters.io.vsdq_dec := sdata.io.vsdq_do_enq
  // vsreq counts available space
  counters.io.vsreq_inc := memif.io.store_ack
  counters.io.vsreq_dec :=
    Mux(io.evac_to_vmu.evac_mode, io.evac_vvaq.fire(),
        io.vxu_to_vmu.vaq_valid && is_mcmd_store(io.vxu_to_vmu.vaq_cmd))
  // vpasdq counts occupied space
  counters.io.vpasdq_dec := sdata.io.vsdq_do_enq
  sdata.io.vpasdq_watermark := counters.io.vpasdq_watermark
  sdata.io.vsdq_watermark := counters.io.vsdq_watermark


  // counters
  counters.io.qcnt := io.qcntp2
  io.pending_store := counters.io.pending_store


  // memif interface
  io.dmem <> memif.io.dmem

  memif.io.prec := io.prec
  unpack.io.prec := io.prec

  // exception handler
  addr.io.evac_to_vmu <> io.evac_to_vmu
  addr.io.stall := io.xcpt_to_vmu.tlb.stall
  sdata.io.evac_to_vmu <> io.evac_to_vmu

  io.vmu_to_xcpt.no_pending_load_store :=
    !counters.io.pending_load && !counters.io.pending_store &&
    !addr.io.vpaq_to_xcpt.vpaq_valid
}

class UnpackStore(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val prec = Bits(INPUT, SZ_PREC)

    val in = new Bundle {
      val vaq = Decoupled(new io_vpaq_bundle()).flip
      val vsdq = Decoupled(Bits(width = 65)).flip
    }

    val out = new Bundle {
      val vaq = Decoupled(new io_vpaq_bundle())
      val vsdq = Decoupled(Bits(width = 65))
    }
  }

  val store = is_mcmd_store(io.in.vaq.bits.cmd) && io.in.vaq.bits.typ_float

  val minor = Reg(init = UInt(0, 2))

  val minor_next = MuxLookup(
    io.prec, Bits(0, 2),
    Array(
      (PREC_SINGLE) -> Cat(~minor(1), Bits(0, 1)),
      (PREC_HALF)   -> (minor + UInt(1))
    ))

  val unpack_finishing = (minor_next === UInt(0))

  io.in.vaq.ready := io.out.vaq.ready
  io.in.vsdq.ready := io.out.vsdq.ready && Mux(store, unpack_finishing, Bool(true))
  io.out.vaq.valid := io.in.vaq.valid
  io.out.vsdq.valid := io.in.vsdq.valid

  // d'oh
  io.out.vaq.bits := io.in.vaq.bits
  io.out.vsdq.bits := io.in.vsdq.bits

  val shift = minor << UInt(4)

  when (store && io.in.vaq.valid && io.in.vsdq.valid && io.out.vaq.ready && io.out.vsdq.ready) {
    minor := minor_next
  }

  when (store) {
    io.out.vsdq.bits := MuxLookup(io.prec, io.in.vsdq.bits, Array(
      PREC_SINGLE -> ((io.in.vsdq.bits >> shift) & Bits("hFFFFFFFF", 32)),
      PREC_HALF   -> ((io.in.vsdq.bits >> shift) & Bits("hFFFF", 16))
    ))
  }
}

class VMUStoreData(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val vsdq_lane = new io_vsdq().flip
    val vsdq_evac = new io_vsdq().flip

    val vsdq = new io_vsdq()

    val vsdq_lane_dec = Bool(INPUT)

    val vsdq_do_enq = Bool(OUTPUT)
    val vsdq_do_deq = Bool(OUTPUT)
    val vpasdq_watermark = Bool(INPUT)
    val vsdq_watermark = Bool(INPUT)

    val evac_to_vmu = new io_evac_to_vmu().flip
  }

  val vsdq_arb = Module(new Arbiter(Bits(width = 65), 2))
  val vsdq = Module(new Queue(Bits(width = 65), conf.nvsdq))

  vsdq_arb.io.in(0) <> io.vsdq_lane
  vsdq_arb.io.in(1) <> io.vsdq_evac
  vsdq_arb.io.out.ready :=
    Mux(io.evac_to_vmu.evac_mode, vsdq.io.enq.ready,
        io.vsdq_watermark && io.vpasdq_watermark)

  vsdq.io.enq.valid := vsdq_arb.io.out.valid
  vsdq.io.enq.bits := vsdq_arb.io.out.bits

  io.vsdq <> vsdq.io.deq

  io.vsdq_do_enq :=
    Mux(io.evac_to_vmu.evac_mode, vsdq.io.enq.ready && io.vsdq_evac.valid,
        io.vsdq_lane_dec)
  io.vsdq_do_deq := io.vsdq.ready && vsdq.io.deq.valid
}

class VMULoadData(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val vldq_lane = new io_vldq()

    val vldq = Valid(new VLDQEnqBundle(66, 4, log2Up(conf.nvldq))).flip
    val vldq_rtag = Decoupled(Bits(width = log2Up(conf.nvldq)))

    val qcnt = UInt(INPUT, SZ_QCNT)
  }

  // needs to make sure log2Up(vldq_entries)+1 <= CPU_TAG_BITS-3
  val vldq = Module(new VLDQ(66, 4, conf.nvldq, 9))

  vldq.io.deq_data.ready := io.vldq_lane.ready
  io.vldq_lane.valid := vldq.io.watermark // vldq.deq_data.valid
  io.vldq_lane.bits := vldq.io.deq_data.bits

  vldq.io.enq <> io.vldq
  io.vldq_rtag <> vldq.io.deq_rtag

  // vldq has an embedded counter
  // vldq counts occupied space
  // vldq occupies an entry, when it accepts an entry from the memory system
  // vldq frees an entry, when the lane consumes it
  vldq.io.qcnt := io.qcnt
}

class VMUCounters(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val vvaq_inc = Bool(INPUT)
    val vvaq_dec = Bool(INPUT)
    val vpaq_inc = Bool(INPUT)
    val vpaq_dec = Bool(INPUT)
    val vsdq_inc = Bool(INPUT)
    val vsdq_dec = Bool(INPUT)
    val vpasdq_inc = Bool(INPUT)
    val vpasdq_dec = Bool(INPUT)
    val vlreq_inc = Bool(INPUT)
    val vlreq_dec = Bool(INPUT)
    val vsreq_inc = Bool(INPUT)
    val vsreq_dec = Bool(INPUT)

    val qcnt = UInt(INPUT, SZ_QCNT)
    val vvaq_watermark = Bool(OUTPUT)
    val vsdq_watermark = Bool(OUTPUT)
    val vpasdq_watermark = Bool(OUTPUT)
    val vlreq_watermark = Bool(OUTPUT)
    val vsreq_watermark = Bool(OUTPUT)

    val vpaq_qcnt = UInt(INPUT, SZ_QCNT)
    val vpaq_watermark = Bool(OUTPUT)

    val pending_load = Bool(OUTPUT)
    val pending_store = Bool(OUTPUT)
  }

  val vvaq_count = Module(new qcnt(conf.nvvaq, conf.nvvaq))
  val vpaq_count = Module(new qcnt(0, conf.nvpaq))
  val vsdq_count = Module(new qcnt(conf.nvsdq, conf.nvsdq))
  val vpasdq_count = Module(new qcnt(0, conf.nvpasdq))
  val vsreq_count = Module(new qcnt(conf.nvsreq, conf.nvsreq)) // vector stores in flight
  val vlreq_count = Module(new qcnt(conf.nvlreq, conf.nvlreq)) // vector loads in flight

  // vvaq counts available space
  vvaq_count.io.inc := io.vvaq_inc
  vvaq_count.io.dec := io.vvaq_dec
  vvaq_count.io.qcnt := io.qcnt
  io.vvaq_watermark := vvaq_count.io.watermark

  // vpaq counts occupied space
  vpaq_count.io.inc := io.vpaq_inc
  vpaq_count.io.dec := io.vpaq_dec
  vpaq_count.io.qcnt := io.vpaq_qcnt
  io.vpaq_watermark := vpaq_count.io.watermark

  // vsdq counts available space
  vsdq_count.io.inc := io.vsdq_inc
  vsdq_count.io.dec := io.vsdq_dec
  vsdq_count.io.qcnt := io.qcnt
  io.vsdq_watermark := vsdq_count.io.watermark

  // vpasdq counts occupied space
  vpasdq_count.io.inc := io.vpasdq_inc
  vpasdq_count.io.dec := io.vpasdq_dec
  vpasdq_count.io.qcnt := io.qcnt
  io.vpasdq_watermark := vpasdq_count.io.watermark

  // vlreq counts available space
  vlreq_count.io.inc := io.vlreq_inc
  vlreq_count.io.dec := io.vlreq_dec
  vlreq_count.io.qcnt := io.qcnt
  io.vlreq_watermark := vlreq_count.io.watermark

  // vsreq counts available space
  vsreq_count.io.inc := io.vsreq_inc
  vsreq_count.io.dec := io.vsreq_dec
  vsreq_count.io.qcnt := io.qcnt
  io.vsreq_watermark := vsreq_count.io.watermark

  // there is no loads in flight, when the counter is full
  io.pending_load := !vlreq_count.io.full
  // there is no stores in flight, when the counter is full
  io.pending_store := !vsreq_count.io.full
}

package hwacha

import Chisel._
import Constants._
import uncore._

class VVAQ extends HwachaModule
{
  val io = new Bundle {
    val xcpt = new XCPTIO().flip
    val lane = new VAQLaneIO().flip
    val evac = new VVAQIO().flip
    val deq = new VVAQIO
  }

  val arb = Module(new Arbiter(UInt(width = confvmu.sz_addr), 2))
  val q = Module(new Queue(UInt(width = confvmu.sz_addr), confvmu.nvvaq))

  arb.io.in(0) <> io.lane.q
  arb.io.in(1) <> io.evac
  q.io.enq <> arb.io.out
  io.deq <> q.io.deq

  val lacntr = Module(new LookAheadCounter(confvmu.nvvaq, confvmu.nvvaq))
  lacntr.io.la <> io.lane.vala
  lacntr.io.inc.cnt := UInt(1)
  lacntr.io.inc.update := io.deq.fire()
  lacntr.io.dec.cnt := UInt(1)
  lacntr.io.dec.update := io.xcpt.prop.vmu.drain && io.evac.fire()
}

class AddressGen extends HwachaModule
{
  val io = new Bundle {
    val xcpt = new XCPTIO().flip
    val ctrl = new VMUBackendIO().flip
    val vvaq = new VVAQIO().flip
    val vatq = new VATQIO()
  }

  val op_tvec = io.ctrl.op.tvec

  val addr = Reg(UInt(width = SZ_ADDR))
  val addr_next = addr + io.ctrl.op.stride

  val utidx = Reg(UInt(width = SZ_VLEN))
  val utidx_next = utidx + UInt(1)

  io.vatq.bits.addr := Mux(!op_tvec || io.xcpt.prop.vmu.drain, io.vvaq.bits, addr)
  io.vatq.bits.meta.utidx := utidx
  io.vatq.bits.meta.utcnt := UInt(1)
  io.vatq.bits.meta.offset := UInt(0)
  io.vatq.bits.cmd := Mux(io.xcpt.prop.vmu.drain, M_XWR, io.ctrl.op.cmd.raw)
  io.vatq.bits.typ := Mux(io.xcpt.prop.vmu.drain, MT_D, io.ctrl.op.typ.raw)

  io.ctrl.busy := Bool(false)
  io.vvaq.ready := Bool(false)
  io.vatq.valid := Bool(false)

  val s_idle :: s_busy :: Nil = Enum(UInt(), 2)
  val state = Reg(init = s_idle)

  switch (state) {
    is (s_idle) {
      when (io.ctrl.fire) {
        addr := io.ctrl.op.base
        utidx := UInt(0)
        state := s_busy
      }
    }

    is (s_busy) {
      io.ctrl.busy := Bool(true)
      io.vvaq.ready := !op_tvec && io.vatq.ready
      io.vatq.valid := op_tvec || io.vvaq.valid

      when (io.vatq.fire()) {
        addr := addr_next
        utidx := utidx_next
        when (utidx_next === io.ctrl.op.vlen) {
          state := s_idle
        }
      }
    }
  }

  when (io.xcpt.prop.vmu.drain) {
    io.vvaq.ready := io.vatq.ready
    io.vatq.valid := io.vvaq.valid
  }
}

class AddressTLB extends HwachaModule
{
  def vpn(addr: UInt) = addr(params(VAddrBits)-1, params(PgIdxBits))
  def idx(addr: UInt) = addr(params(PgIdxBits)-1, 0)

  val io = new Bundle {
    val enq = new VATQIO().flip
    val deq = new VPAQIO
    val tlb = new TLBIO

    val stall = Bool(INPUT)
    val irq = new IRQIO
  }

  val stall_hold = Reg(init = Bool(false))
  val stall = io.stall || stall_hold

  io.tlb.req.valid := !stall && io.enq.valid && io.deq.ready
  io.tlb.req.bits.asid := UInt(0)
  io.tlb.req.bits.vpn := vpn(io.enq.bits.addr)
  io.tlb.req.bits.passthrough := Bool(false)
  io.tlb.req.bits.instruction := Bool(false)

  val mcmd_amo = is_mcmd_amo(io.enq.bits.cmd)
  val mcmd_ld = is_mcmd_load(io.enq.bits.cmd) || mcmd_amo
  val mcmd_st = is_mcmd_store(io.enq.bits.cmd) || mcmd_amo
  val mcmd_pfr = is_mcmd_pfr(io.enq.bits.cmd)
  val mcmd_pfw = is_mcmd_pfw(io.enq.bits.cmd)

  val ma_type_h = is_mtype_halfword(io.enq.bits.typ) && (io.enq.bits.addr(0) != UInt(0))
  val ma_type_w = is_mtype_word(io.enq.bits.typ) && (io.enq.bits.addr(1,0) != UInt(0))
  val ma_type_d = is_mtype_doubleword(io.enq.bits.typ) && (io.enq.bits.addr(2,0) != UInt(0))
  val ma_addr = ma_type_h || ma_type_w || ma_type_d
  val ma_ld = ma_addr && mcmd_ld
  val ma_st = ma_addr && mcmd_st

  val xcpt_ld = io.tlb.resp.xcpt_ld && mcmd_ld
  val xcpt_st = io.tlb.resp.xcpt_st && mcmd_st
  val xcpt_pf = (io.tlb.resp.xcpt_ld && mcmd_pfr) || (io.tlb.resp.xcpt_st && mcmd_pfw)
  val xcpt_stall = ma_addr || xcpt_ld || xcpt_st
  val xcpt = xcpt_stall || xcpt_pf

  io.enq.ready := !stall && io.deq.ready && io.tlb.req.ready && !io.tlb.resp.miss && !xcpt_stall

  io.deq.valid := io.tlb.req.fire() && !io.tlb.resp.miss && !xcpt
  io.deq.bits.cmd := io.enq.bits.cmd
  io.deq.bits.typ := io.enq.bits.typ
  io.deq.bits.addr := Cat(io.tlb.resp.ppn, idx(io.enq.bits.addr))
  io.deq.bits.meta := io.enq.bits.meta

  when (io.tlb.req.fire() && xcpt_stall) {
    stall_hold := Bool(true)
  }

  io.irq.vmu.ma_ld := io.tlb.req.fire() && ma_ld
  io.irq.vmu.ma_st := io.tlb.req.fire() && ma_st
  io.irq.vmu.faulted_ld := io.tlb.req.fire() && xcpt_ld
  io.irq.vmu.faulted_st := io.tlb.req.fire() && xcpt_st
  io.irq.vmu.aux := io.enq.bits.addr
}

class VPAQ extends HwachaModule
{
  val sz = log2Down(confvmu.nvpaq) + 1
  val io = new Bundle {
    val enq = new VPAQIO().flip
    val deq = new VPAQIO

    val la = new LookAheadPortIO(sz).flip
    val xcpt = new XCPTIO().flip
  }

  val q = Module(new Queue(new VPAQEntry, confvmu.nvpaq))
  q.io.enq <> io.enq

  val lacntr = Module(new LookAheadCounter(0, confvmu.nvpaq))
  // TODO: Support utcnt != 1
  lacntr.io.inc.cnt := UInt(1)
  lacntr.io.inc.update := q.io.enq.fire()
  lacntr.io.dec.cnt := UInt(1)
  lacntr.io.dec.update := q.io.deq.fire() && io.xcpt.prop.vmu.drain
  lacntr.io.la <> io.la

  // Throttle counter
  val count = Reg(init = UInt(0, sz))
  val summand = Fill(sz, io.la.reserve) & io.la.cnt
  val minuend = Fill(sz, io.deq.fire()) & io.deq.bits.meta.utcnt
  val throttle = !io.xcpt.prop.vmu.drain
  when (throttle) {
    count := count + summand - minuend
  }
  val stall = (count < q.io.deq.bits.meta.utcnt) && throttle

  q.io.deq.ready := io.deq.ready && !stall
  io.deq.valid := q.io.deq.valid && !stall
  io.deq.bits := q.io.deq.bits
}

class MetadataAlloc extends HwachaModule
{
  val io = new Bundle {
    val vpaq = new VPAQIO().flip
    val vmdb = new VMDBIO
    val memif = new VPAQMemIO
  }

  val load = is_mcmd_load(io.vpaq.bits.cmd) || is_mcmd_amo(io.vpaq.bits.cmd)

  io.vmdb.info.valid := load && io.vpaq.valid && io.memif.ready
  io.vmdb.info.bits := io.vpaq.bits.meta

  val vmdb_ready = !load || io.vmdb.info.ready

  io.vpaq.ready := io.memif.ready && vmdb_ready
  io.memif.valid := io.vpaq.valid && vmdb_ready
  io.memif.bits <> io.vpaq.bits
  io.memif.bits.tag := io.vmdb.tag
}

class AddressUnit extends HwachaModule
{
  val io = new Bundle {
    val ctrl = new VMUBackendIO().flip

    val irq = new IRQIO
    val xcpt = new XCPTIO().flip

    val lane = new VAQLaneIO().flip
    val evac = new VVAQIO().flip

    val memif = new VPAQMemIO
    val vmdb = new VMDBIO
    val tlb = new TLBIO
  }

  val vvaq = Module(new VVAQ)
  val agu = Module(new AddressGen)
  val atu = Module(new AddressTLB)
  val vpaq = Module(new VPAQ)
  val vmda = Module(new MetadataAlloc)

  vvaq.io.lane <> io.lane
  vvaq.io.evac <> io.evac
  vvaq.io.xcpt <> io.xcpt
  agu.io.vvaq <> vvaq.io.deq
  agu.io.ctrl <> io.ctrl
  agu.io.xcpt <> io.xcpt

  atu.io.enq <> agu.io.vatq
  atu.io.irq <> io.irq
  atu.io.stall := io.xcpt.prop.vmu.stall
  io.tlb <> atu.io.tlb

  vpaq.io.enq <> atu.io.deq
  vpaq.io.la <> io.lane.pala
  vpaq.io.xcpt <> io.xcpt

  vmda.io.vpaq <> vpaq.io.deq
  io.vmdb <> vmda.io.vmdb
  io.memif <> vmda.io.memif
}

class PrefetchUnit extends HwachaModule
{
  val io = new Bundle {
    val xcpt = new XCPTIO().flip

    val vaq = new VVAPFQIO().flip
    val memif = new VPAQMemIO
    val tlb = new TLBIO
  }

  val vvaq = Module(new Queue(new VVAPFQEntry, confvmu.nvvapfq))
  val atu = Module(new AddressTLB)
  val vpaq = Module(new Queue(new VPAQEntry, confvmu.nvpapfq))

  vvaq.io.enq <> io.vaq

  atu.io.enq <> vvaq.io.deq
  atu.io.stall := io.xcpt.prop.vmu.stall
  io.tlb <> atu.io.tlb

  vpaq.io.enq <> atu.io.deq
  io.memif <> MaskStall(vpaq.io.deq, io.xcpt.prop.vmu.stall)
  io.memif.bits.tag := UInt(0)
}

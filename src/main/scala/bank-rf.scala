package hwacha

import Chisel._
import DataGating._

class RFWritePort extends VXUBundle with BankData with BankMask {
  val addr = UInt(width = log2Up(nSRAM))
}

class BankRegfile extends VXUModule {
  val io = new Bundle {
    val op = new BankOpIO().flip
    val global = new BankRWIO
    val local = new Bundle {
      val pdl = Vec.fill(nLPDL){new BankPredEntry().asOutput}
      val opl = Vec.fill(nLOPL){new BankDataEntry().asOutput}
      val ppred = new BankPredEntry().asOutput
      val rpred = Vec.fill(nPredRPorts){new BankPredEntry().asOutput}
      val wpred = Vec.fill(2){new BankPredEntry().asInput}
      val wdata = new BankDataPredEntry().asInput
    }
  }

  val sram_rf = SeqMem(Bits(width = wBank), nSRAM)
  val ff_rf = Mem(Bits(width = wBank), nFF)
  val pred_rf = Mem(Bits(width = nSlices), nPred)

  val gopl = Mem(Bits(width = wBank), nGOPL)
  val lopl = Mem(Bits(width = wBank), nLOPL)
  val gpdl = Mem(Bits(width = nSlices), nGPDL)
  val lpdl = Mem(Bits(width = nSlices), nLPDL)

  def read_gpdl(addr: UInt) = {
    new BankPredEntry().fromBits(gpdl(addr))
  }

  // Predicate RF gated read port
  val pred_gated_op = IndexedSeq(io.op.pred.gread, io.op.pred.pread)
  val pred_gated_raddr = pred_gated_op map { op => dgate(op.valid, op.bits.addr) }
  val pred_gated_rdata_raw = pred_gated_raddr map { addr => pred_rf(addr) }
  val pred_gated_rdata = (pred_gated_op zip pred_gated_rdata_raw) map { case (op, rdata) =>
    new BankPredEntry().fromBits(Mux(op.bits.off, op.bits.pred, Mux(op.bits.neg, ~rdata, rdata))) }
  val gpred = pred_gated_rdata(0)
  val s1_gpred = RegEnable(gpred, io.op.pred.gread.valid)
  io.local.ppred := pred_gated_rdata(1)

  // Predicate RF read port
  val pred_raddr = io.op.pred.read map { op => dgate(op.valid, op.bits.addr) }
  val pred_rdata = pred_raddr map { addr => pred_rf(addr) }
  (io.local.rpred zip pred_rdata) map { case (rpred, rdata) =>
    rpred := new BankPredEntry().fromBits(rdata) }

  // Predicate RF write port
  when (io.op.pred.write.valid) {
    pred_rf.write(
      io.op.pred.write.bits.addr,
      Mux(io.op.pred.write.bits.selg, io.global.wpred.pred,
        Mux(io.op.pred.write.bits.plu, io.local.wpred(1).pred, io.local.wpred(0).pred)))
  }

  // SRAM RF read port
  val sram_rdata = sram_rf.read(io.op.sram.read.bits.addr, io.op.sram.read.valid && gpred.active())

  // SRAM RF write port
  val sram_warb = Module(new Arbiter(new RFWritePort, 3))

  val sram_wdata = new BankDataPredEntry().fromBits(
    Mux(io.op.sram.write.bits.selg,
      MuxLookup(io.op.sram.write.bits.wsel, Bits(0), (0 until nWSel) map {
        i => UInt(i) -> io.global.wdata(i).toBits }),
      io.local.wdata.toBits))

  sram_warb.io.in(0).valid := io.op.sram.write.valid && sram_wdata.active()
  sram_warb.io.in(0).bits.addr := io.op.sram.write.bits.addr
  sram_warb.io.in(0).bits.data := sram_wdata.data
  sram_warb.io.in(0).bits.mask := FillInterleaved(regLen/8, io.op.sram.write.bits.pred & sram_wdata.pred)
  assert(!io.op.sram.write.valid || sram_warb.io.in(0).ready, "this sram write port should always be ready")

  sram_warb.io.in(1).valid := io.global.bwq.mem.valid && !io.global.bwq.mem.bits.selff
  sram_warb.io.in(1).bits.addr := io.global.bwq.mem.bits.saddr()
  sram_warb.io.in(1).bits.data := io.global.bwq.mem.bits.data
  sram_warb.io.in(1).bits.mask := io.global.bwq.mem.bits.mask

  sram_warb.io.in(2).valid := io.global.bwq.fu.valid && !io.global.bwq.fu.bits.selff
  sram_warb.io.in(2).bits.addr := io.global.bwq.fu.bits.saddr()
  sram_warb.io.in(2).bits.data := io.global.bwq.fu.bits.data
  sram_warb.io.in(2).bits.mask := io.global.bwq.fu.bits.mask

  sram_warb.io.out.ready := Bool(true) // can always write the register file
  when (sram_warb.io.out.valid) {
    sram_rf.write(
      sram_warb.io.out.bits.addr,
      sram_warb.io.out.bits.data,
      FillInterleaved(8, sram_warb.io.out.bits.mask))
  }

  // FF RF read port
  val ff_raddr = io.op.ff.read map { op => dgate(op.valid && gpred.active(), op.bits.addr) }
  val ff_rdata = ff_raddr map { addr => ff_rf(addr) }

  // FF RF write port
  val ff_warb = Module(new Arbiter(new RFWritePort, 3))

  val ff_wdata = new BankDataPredEntry().fromBits(
    Mux(io.op.ff.write.bits.selg,
      MuxLookup(io.op.ff.write.bits.wsel, Bits(0), (0 until nWSel) map {
        i => UInt(i) -> io.global.wdata(i).toBits }),
      io.local.wdata.toBits))

  ff_warb.io.in(0).valid := io.op.ff.write.valid && ff_wdata.active()
  ff_warb.io.in(0).bits.addr := io.op.ff.write.bits.addr
  ff_warb.io.in(0).bits.data := ff_wdata.data
  ff_warb.io.in(0).bits.mask := FillInterleaved(regLen/8, io.op.ff.write.bits.pred & ff_wdata.pred)
  assert(!io.op.ff.write.valid || ff_warb.io.in(0).ready, "this ff write port should always be ready")

  ff_warb.io.in(1).valid := io.global.bwq.mem.valid && io.global.bwq.mem.bits.selff
  ff_warb.io.in(1).bits.addr := io.global.bwq.mem.bits.faddr()
  ff_warb.io.in(1).bits.data := io.global.bwq.mem.bits.data
  ff_warb.io.in(1).bits.mask := io.global.bwq.mem.bits.mask

  ff_warb.io.in(2).valid := io.global.bwq.fu.valid && io.global.bwq.fu.bits.selff
  ff_warb.io.in(2).bits.addr := io.global.bwq.fu.bits.faddr()
  ff_warb.io.in(2).bits.data := io.global.bwq.fu.bits.data
  ff_warb.io.in(2).bits.mask := io.global.bwq.fu.bits.mask

  ff_warb.io.out.ready := Bool(true) // can always write the register file
  when (ff_warb.io.out.valid) {
    ff_rf.write(
      ff_warb.io.out.bits.addr,
      ff_warb.io.out.bits.data,
      FillInterleaved(8, ff_warb.io.out.bits.mask))
  }

  // BWQ
  io.global.bwq.mem.ready :=
    !io.global.bwq.mem.bits.selff && sram_warb.io.in(1).ready ||
     io.global.bwq.mem.bits.selff && ff_warb.io.in(1).ready

  io.global.bwq.fu.ready :=
    !io.global.bwq.fu.bits.selff && sram_warb.io.in(2).ready ||
     io.global.bwq.fu.bits.selff && ff_warb.io.in(2).ready

  // Operand Latches (OPL)
  (0 until nGOPL) foreach { i =>
    when (io.op.opl.global(i).valid && s1_gpred.active()) {
      gopl.write(
        UInt(i),
        Mux(io.op.opl.global(i).bits.selff, ff_rdata(i % nFFRPorts), sram_rdata),
        FillInterleaved(regLen, io.op.opl.global(i).bits.pred & s1_gpred.pred))
    }
    io.global.opl(i).data :=
      dgate(io.op.xbar(i).valid && read_gpdl(io.op.xbar(i).bits.pdladdr).active(), gopl(i))
  }
  (0 until nLOPL) foreach { i =>
    when (io.op.opl.local(i).valid && s1_gpred.active()) {
      lopl.write(
        UInt(i),
        Mux(io.op.opl.local(i).bits.selff, ff_rdata(i % nFFRPorts), sram_rdata),
        FillInterleaved(regLen, io.op.opl.local(i).bits.pred & s1_gpred.pred))
    }
    io.local.opl(i).data := lopl(i)
  }

  // Predicate Latches (PDL)
  (0 until nGPDL) foreach { i =>
    when (io.op.pdl.global(i).valid) {
      gpdl.write(UInt(i), s1_gpred.pred)
    }
    io.global.pdl(i).pred := dgate(io.op.pxbar(i).valid, gpdl(i))
  }
  (0 until nLPDL) foreach { i =>
    when (io.op.pdl.local(i).valid) {
      lpdl.write(UInt(i), s1_gpred.pred)
    }
    io.local.pdl(i).pred := lpdl(i)
  }
}

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

  // Predicate RF read port
  val pred_raddr = (0 until nPredRPorts) map { i => dgate(io.op.pred.read(i).valid, io.op.pred.read(i).bits.addr) }
  val pred_rdata_raw = (0 until nPredRPorts) map { i => pred_rf(pred_raddr(i)) }
  val pred_rdata = (0 until nPredRPorts) map { i =>
    new BankPredEntry().fromBits(
      Mux(io.op.pred.read(i).bits.off, io.op.pred.read(i).bits.pred,
        Mux(io.op.pred.read(i).bits.neg, ~pred_rdata_raw(i), pred_rdata_raw(i)))) }
  val pdl_rdata = RegEnable(pred_rdata(0), io.op.pred.read(0).valid)
  (io.local.rpred zip pred_rdata) map { case (rpred, rdata) => rpred := rdata }

  // Predicate RF write port
  when (io.op.pred.write.valid) {
    pred_rf.write(
      io.op.pred.write.bits.addr,
      Mux(io.op.pred.write.bits.selg, io.global.wpred.pred,
        Mux(io.op.pred.write.bits.plu, io.local.wpred(1).pred, io.local.wpred(0).pred)))
  }

  def gop_pred[T <: GatedOp](op: ValidIO[T]) = {
    new BankPredEntry().fromBits(gpdl(op.bits.pdladdr))
  }
  def gop_valid[T <: GatedOp](op: ValidIO[T]) = {
    op.valid && gop_pred(op).active()
  }

  // SRAM RF read port
  val sram_rdata = sram_rf.read(io.op.sram.read.bits.addr, io.op.sram.read.valid && pred_rdata(0).active())

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
  val ff_raddr = (0 until nFFRPorts) map { i => dgate(io.op.ff.read(i).valid && pred_rdata(0).active(), io.op.ff.read(i).bits.addr) }
  val ff_rdata = (0 until nFFRPorts) map { i => ff_rf(ff_raddr(i)) }

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
    when (io.op.opl.global(i).valid && pdl_rdata.active()) {
      gopl.write(
        UInt(i),
        Mux(io.op.opl.global(i).bits.selff, ff_rdata(i % nFFRPorts), sram_rdata),
        FillInterleaved(regLen, io.op.opl.global(i).bits.pred & pdl_rdata.pred))
    }
    io.global.opl(i).data := dgate(gop_valid(io.op.xbar(i)), gopl(i))
  }
  (0 until nLOPL) foreach { i =>
    when (io.op.opl.local(i).valid && pdl_rdata.active()) {
      lopl.write(
        UInt(i),
        Mux(io.op.opl.local(i).bits.selff, ff_rdata(i % nFFRPorts), sram_rdata),
        FillInterleaved(regLen, io.op.opl.local(i).bits.pred & pdl_rdata.pred))
    }
    io.local.opl(i).data := lopl(i)
  }

  // Predicate Latches (PDL)
  (0 until nGPDL) foreach { i =>
    when (io.op.pdl.global(i).valid) {
      gpdl.write(UInt(i), pdl_rdata.pred)
    }
    io.global.pdl(i).pred := dgate(io.op.pxbar(i).valid, gpdl(i))
  }
  (0 until nLPDL) foreach { i =>
    when (io.op.pdl.local(i).valid) {
      lpdl.write(UInt(i), pdl_rdata.pred)
    }
    io.local.pdl(i).pred := lpdl(i)
  }
}

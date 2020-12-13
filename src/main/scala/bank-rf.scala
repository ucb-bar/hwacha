package hwacha

import Chisel._
import freechips.rocketchip.config._
import DataGating._

class RFWritePort(implicit p: Parameters) extends VXUBundle()(p) with BankData with BankMask {
  val addr = UInt(width = log2Up(nSRAM))
}

class BankRegfile(bid: Int)(implicit p: Parameters) extends VXUModule()(p) with PackLogic {
  val io = new Bundle {
    val lid = UInt(INPUT)
    val op = new BankOpIO().flip
    val global = new BankRWIO
    val local = new Bundle {
      val pdl = Vec(nLPDL, new BankPredEntry()).asOutput
      val opl = Vec(nLOPL, new BankDataEntry()).asOutput
      val ppred = new BankPredEntry().asOutput
      val rpred = Vec(nPredRPorts, new BankPredEntry()).asOutput
      val wpred = Vec(2, new BankPredMaskEntry()).asInput
      val wdata = new BankDataPredEntry().asInput
    }
  }
  val sram_rf = SeqMem(nSRAM, Vec(wBank/8, Bits(width = 8)))
  sram_rf.suggestName("HwSRAMRF")
  val ff_rf = Mem(nFF, Vec(wBank/8, Bits(width = 8)))
  val pred_rf = Mem(nPred, Vec(wPred, Bool()))

  val gopl = Mem(nGOPL, Vec(nSlices, Bits(width = regLen)))
  val lopl = Mem(nLOPL, Vec(nSlices, Bits(width = regLen)))
  val gpdl = Mem(nGPDL, Bits(width = wPred))
  val lpdl = Mem(nLPDL, Bits(width = wPred))

  def read_gpdl(addr: UInt) = new BankPredEntry().fromBits(gpdl(addr))

  def toBytes(bits: UInt) = Vec.tabulate(wBank/8)(i => bits(8*(i+1)-1, 8*i))
  def toDWords(bits: UInt) = Vec.tabulate(nSlices)(i => bits(regLen*(i+1)-1, regLen*i))

  def pred_shift(idx: UInt) = if (confprec) Cat(idx, UInt(0, bSlices)) else UInt(0)
  def pred_read(op: ValidIO[PredRFReadOp]) = {
    val addr = dgate(op.valid, op.bits.addr)
    val idx = dgate(op.valid, op.bits.pack.idx)
    pred_rf(addr).asUInt >> pred_shift(idx)
  }

  // Predicate RF gated read port
  val pred_gated_op = IndexedSeq(io.op.pred.gread, io.op.pred.pread)
  val pred_gated_rdata_raw = pred_gated_op.map(op => pred_read(op))
  val pred_gated_rdata = (pred_gated_op zip pred_gated_rdata_raw) map { case (op, rdata) =>
    new BankPredEntry().fromBits(
      Mux(op.bits.off, op.bits.pred, op.bits.pred & Mux(op.bits.neg, ~rdata, rdata))) }
  val gpred = pred_gated_rdata(0)
  val ppred = pred_gated_rdata(1)
  val s1_gpred = RegEnable(gpred, io.op.pred.gread.valid)
  io.local.ppred := RegEnable(ppred, io.op.pred.pread.valid)

  // Predicate RF read port
  val pred_rdata = io.op.pred.read.map(op => pred_read(op))
  (io.op.pred.read zip io.local.rpred zip pred_rdata) map { case ((op, rpred), rdata) =>
    rpred := RegEnable(new BankPredEntry().fromBits(op.bits.pred & rdata), op.valid) }

  // Predicate RF write port
  when (io.op.pred.write.valid) {
    val waddr = io.op.pred.write.bits.addr
    val shift = pred_shift(io.op.pred.write.bits.pack.idx)
    val wpred =
      Mux(io.op.pred.write.bits.selg, io.global.wpred,
        Mux(io.op.pred.write.bits.plu, io.local.wpred(1), io.local.wpred(0)))
    val wdata_base = repack_pred(wpred.pred, io.op.pred.write.bits.rate)
    val wdata = (wdata_base << shift)(wPred-1, 0)
    val wmask_base = io.op.pred.write.bits.pred & wpred.mask
    val wmask = (wmask_base << shift)(wPred-1, 0)

    pred_rf.write(waddr, Vec(((wdata & wmask) | (pred_rf(waddr).asUInt & ~wmask)).asBools))

    if (commit_log) {
      (0 until wPred) foreach { case i =>
        when (wmask(i)) {
          printf("H: write_prf %d %d %d %d %d\n", io.lid, UInt(bid), waddr, UInt(i), wdata(i))
        }
      }
    }
  }

  // SRAM RF read port
  val sram_raddr = io.op.sram.read.bits.addr
  val sram_rdata = sram_rf.read(sram_raddr, io.op.sram.read.valid && gpred.active()).asUInt
  val sram_rpack = unpack_bank(Reg(next = io.op.sram.read.bits), sram_rdata)

  // SRAM RF write port
  val sram_warb = Module(new Arbiter(new RFWritePort, 3))
  sram_warb.suggestName("sram_warbInst")

  val sram_wdata = new BankDataPredEntry().fromBits(
    Mux(io.op.sram.write.bits.selg,
      MuxLookup(io.op.sram.write.bits.wsel, Bits(0), (0 until nWSel) map {
        i => UInt(i) -> io.global.wdata(i).asUInt }),
      io.local.wdata.asUInt))
  val sram_wpack = repack_bank(io.op.sram.write.bits, sram_wdata)

  sram_warb.io.in(0).valid := io.op.sram.write.valid && sram_wdata.active()
  sram_warb.io.in(0).bits.addr := io.op.sram.write.bits.addr
  sram_warb.io.in(0).bits.data := sram_wpack.data
  sram_warb.io.in(0).bits.mask := sram_wpack.mask
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
    val waddr = sram_warb.io.out.bits.addr
    val wdata = toBytes(sram_warb.io.out.bits.data)
    val wmask = sram_warb.io.out.bits.mask.asBools

    sram_rf.write(waddr, wdata, wmask)

    if (commit_log) {
      val wdata = toDWords(sram_warb.io.out.bits.data) // FIXME
      (0 until nSlices) foreach { case i =>
        when (wmask(8*i)) {
          printf("H: write_vrf %d %d %d %d %x\n", io.lid, UInt(bid), waddr, UInt(i), wdata(i))
        }
      }
    }
  }

  // FF RF read port
  val ff_raddr = io.op.ff.read map { op => dgate(op.valid && gpred.active(), op.bits.addr) }
  val ff_rdata = ff_raddr map { addr => ff_rf(addr).asUInt }

  // FF RF write port
  val ff_warb = Module(new Arbiter(new RFWritePort, 3))
  ff_warb.suggestName("ff_warbInst")

  val ff_wdata = new BankDataPredEntry().fromBits(
    Mux(io.op.ff.write.bits.selg,
      MuxLookup(io.op.ff.write.bits.wsel, Bits(0), (0 until nWSel) map {
        i => UInt(i) -> io.global.wdata(i).asUInt }),
      io.local.wdata.asUInt))

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
      toBytes(ff_warb.io.out.bits.data),
      ff_warb.io.out.bits.mask.asBools)
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
        toDWords(Mux(io.op.opl.global(i).bits.selff, ff_rdata(i % nFFRPorts), sram_rpack.data)),
        (io.op.opl.global(i).bits.pred & s1_gpred.pred)(1,0).asBools)
    }
    io.global.opl(i).data :=
      dgate(io.op.xbar(i).valid && read_gpdl(io.op.xbar(i).bits.pdladdr).active(), gopl(i).asUInt)
  }
  (0 until nLOPL) foreach { i =>
    when (io.op.opl.local(i).valid && s1_gpred.active()) {
      lopl.write(
        UInt(i),
        toDWords(Mux(io.op.opl.local(i).bits.selff, ff_rdata(i % nFFRPorts), sram_rpack.data)),
        (io.op.opl.local(i).bits.pred & s1_gpred.pred)(1,0).asBools)
    }
    io.local.opl(i).data := lopl(i).asUInt
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

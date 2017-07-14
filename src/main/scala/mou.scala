package hwacha

import Chisel._
import freechips.rocketchip.config._

class MOCheck(implicit p: Parameters) extends HwachaBundle()(p) {
  val load = Bool()
  val store = Bool()
}

class MemOrderingUnit(implicit p: Parameters) extends HwachaModule()(p) with SeqLogic {
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val mseq = new MasterSequencerState().asInput
    val pending = new Bundle {
      val su = new MRTPending().asInput
      val vus = Vec(nLanes, new MRTPending).asInput
    }
    val check = new Bundle {
      val su = new MOCheck().asOutput
      val vus = Vec(nLanes, Vec(nSeq, new MOCheck)).asOutput
    }
  }

  // Treat addr as limiter on head advancement for comparisons
  val latchedHead = Reg(init = 0.U(log2Up(nSeq).W))
  val headMatchAddr = io.pending.vus.map(_.addr).map{ case a =>
    a.valid && a.bits === latchedHead }.foldLeft(Bool(false))(_ || _)
  when(!headMatchAddr) { latchedHead := io.mseq.head }
  // if the vmu is sending out acquires for a vlu/vsu between
  // the head and our mseq entry we need to wait
  def pending_addr(i: Int) = {
    val myIdx = i.U(log2Up(nSeq).W)
    // comparisons between head and aidx should be inclusive
    io.pending.vus.map(_.addr).map{ case a => a.valid &&
      (latchedHead < myIdx && latchedHead <= a.bits && a.bits < myIdx) ||
      (latchedHead > myIdx && // check for wrap-around
        ((a.bits >= latchedHead && a.bits > myIdx) ||
        (a.bits < myIdx && a.bits <= latchedHead)))
    }.foldLeft(false.B)(_ || _)
  }

  def vus_pending_store(exclude: Bool) =
    io.pending.vus.map(_.store).filter(_ ne exclude).foldLeft(Bool(false))(_ || _)
  def vus_pending_all(exclude: Bool) =
    io.pending.vus.map(_.all).filter(_ ne exclude).foldLeft(Bool(false))(_ || _)

  // scalar loads can go through when memory ordering is relaxed or
  // when no pending vector stores
  // scalar stores can go through when memory orderig is relaxed or
  // when no pending vector loads & stores

  io.check.su.load := (io.cfg.morelax || !vus_pending_store(null))
  io.check.su.store := (io.cfg.morelax || !vus_pending_all(null))

  // vector loads can go through when memory ordering is relaxed or
  // when no pending scalar stores and when either of these conditions are met
  //  1) it's the first vector memory op
  //  2) no pending vector stores from other lanes than the one examined
  // vector stores can go through when memory ordering is relaxed or
  // when no pending scalar laods & stores and when either of these conditions are met
  //  1) it's the first vector memory op
  //  2) no pending vector loads & stores from other lanes than the one examined
  // First is defined as being the first vcu op after any previous memory ops have
  // finished sending out the aquires (vu_pending_addr)

  val vu_pending_addr = (0 until nSeq).map{ case i => pending_addr(i) }
  val first = find_first(io.mseq.valid, io.mseq.head, (i: Int) => io.mseq.e(i).active.vcu)
    .zip(vu_pending_addr).map{ case (f, a) => f && !a }

  (0 until nLanes) map { l => (0 until nSeq) map { s =>
    io.check.vus(l)(s).load :=
      io.cfg.morelax || !io.pending.su.store &&
      (first(s) || !vus_pending_store(io.pending.vus(l).store))
    io.check.vus(l)(s).store :=
      io.cfg.morelax || !io.pending.su.all &&
      (first(s) || !vus_pending_all(io.pending.vus(l).all))
  } }
}

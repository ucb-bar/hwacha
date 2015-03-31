package hwacha

import Chisel._
import Node._

class VXU extends HwachaModule
{
  val io = new Bundle {
    val seqop = new SequencerOpIO().flip
    val vmu = new VMUIO
  }

  val seq = Module(new Sequencer)
  val lane = Module(new Lane)
  val deck = Module(new Deck, {case HwachaNBanks => 8})

  seq.io.seqop <> io.seqop
  seq.io.laneack <> lane.io.ack

  lane.io.op <> seq.io.laneop

  // TODO: deck assumes 8 banks, lane assumes 4 banks
  for (i <- 0 until nbanks) {
    lane.io.bwqs(i) <> deck.io.bwqs(i)
    deck.io.brqs(i) <> lane.io.brqs(i)
  }

  io.vmu <> lane.io.vmu
  io.vmu <> deck.io.vmu

  // TODO: this is here to make sure things get instantiated
  io.vmu.issue.cmd.valid := io.seqop.valid
  io.vmu.issue.cmd.bits.fn := io.seqop.bits.inst
  io.vmu.issue.cmd.bits.mt := io.seqop.bits.inst
  io.vmu.issue.cmd.bits.vlen := io.seqop.bits.inst

  io.vmu.issue.addr.valid := io.seqop.valid
  io.vmu.issue.addr.bits.base := io.seqop.bits.inst
  io.vmu.issue.addr.bits.stride := io.seqop.bits.inst

  deck.io.cfg.prec := io.seqop.bits.inst
  deck.io.cfg.bactive := io.seqop.bits.inst
  deck.io.cfg.bcnt := io.seqop.bits.inst
  deck.io.cfg.nxregs := io.seqop.bits.inst
  deck.io.cfg.nfregs := io.seqop.bits.inst
  deck.io.cfg.xstride := io.seqop.bits.inst
  deck.io.cfg.fstride := io.seqop.bits.inst
  deck.io.cfg.xfsplit := io.seqop.bits.inst

  deck.io.op.valid := io.seqop.valid
  deck.io.op.bits.fn := io.seqop.bits.inst
  deck.io.op.bits.mt := io.seqop.bits.inst
  deck.io.op.bits.vlen := io.seqop.bits.inst
  deck.io.op.bits.utidx := io.seqop.bits.inst
  deck.io.op.bits.float := io.seqop.bits.inst(13)
  deck.io.op.bits.reg.vs.zero := io.seqop.bits.inst(13)
  deck.io.op.bits.reg.vs.float := io.seqop.bits.inst(13)
  deck.io.op.bits.reg.vs.id := io.seqop.bits.inst
  deck.io.op.bits.reg.vt.zero := io.seqop.bits.inst(13)
  deck.io.op.bits.reg.vt.float := io.seqop.bits.inst(13)
  deck.io.op.bits.reg.vt.id := io.seqop.bits.inst
  deck.io.op.bits.reg.vr.zero := io.seqop.bits.inst(13)
  deck.io.op.bits.reg.vr.float := io.seqop.bits.inst(13)
  deck.io.op.bits.reg.vr.id := io.seqop.bits.inst
  deck.io.op.bits.reg.vd.zero := io.seqop.bits.inst(13)
  deck.io.op.bits.reg.vd.float := io.seqop.bits.inst(13)
  deck.io.op.bits.reg.vd.id := io.seqop.bits.inst

  deck.io.lla.cnt := io.seqop.bits.inst
  deck.io.lla.reserve := io.seqop.bits.inst(13)

  deck.io.sla.cnt := io.seqop.bits.inst
  deck.io.sla.reserve := io.seqop.bits.inst(13)

  deck.io.xcpt.prop.vmu.stall := Bool(false)
}

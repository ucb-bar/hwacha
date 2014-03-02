package hwacha

import Chisel._
import Node._
import Constants._
import Commands._

class AIWCmdQIO extends DecoupledIO(Bits(width = SZ_VCMD))
class AIWImm1QIO extends DecoupledIO(Bits(width = SZ_VIMM))
class AIWImm2QIO extends DecoupledIO(Bits(width = SZ_VSTRIDE))
class AIWCntQIO extends DecoupledIO(Bits(width = SZ_VLEN))
class AIWNumCntQIO extends DecoupledIO(Bits(width = 1))

class AIWVXUIO extends Bundle
{
  val issue = new Bundle {
    val enq = new Bundle {
      val cmdb = new AIWCmdQIO
      val imm1b = new AIWImm1QIO
      val imm2b = new AIWImm2QIO
      val cntb = new AIWCntQIO
      val numcntb = new AIWNumCntQIO
    }
    val rtag = new Bundle {
      val imm1 = Bits(INPUT, SZ_AIW_IMM1)
      val cnt = Bits(INPUT, SZ_AIW_CNT)
      val numcnt = Bits(INPUT, SZ_AIW_NUMCNT)
    }
    val marklast = Bool(OUTPUT)
    val update = new Bundle {
      val numcnt = new AIWUpdateCounterVecIO
    }
  }
  val seq = new Bundle {
    val update = new Bundle {
      val imm1 = Valid(new AIWUpdateImm1Op)
      val cnt = Valid(new AIWUpdateCntOp)
      val numcnt = Valid(new AIWUpdateNumCntOp)
    }
  }
}

class AIWEvacIO extends Bundle
{
  val deq = new Bundle {
    val cmdb = new AIWCmdQIO().flip
    val imm1b = new AIWImm1QIO().flip
    val imm2b = new AIWImm2QIO().flip
    val cntb = new AIWCntQIO().flip
    val numcntb = new AIWNumCntQIO().flip
    val numcntb_last = Bool(INPUT)
  }
  val update = new Bundle {
    val numcnt = new AIWUpdateCounterVecIO
  }
}

class AIW(resetSignal: Bool = null) extends Module(_reset = resetSignal)
{
  val io = new Bundle {
    val vxu = new AIWVXUIO().flip
    val evac = new AIWEvacIO().flip
  }

  val ircmdb = Module(new Queue(Bits(width = SZ_VCMD), AIW_CMD_DEPTH))
  val irimm1b = Module(new Buffer(SZ_VIMM, AIW_IMM1_DEPTH))
  val irimm2b = Module(new Queue(Bits(width = SZ_VSTRIDE), AIW_IMM2_DEPTH))
  val ircntb = Module(new Buffer(SZ_VLEN, AIW_CNT_DEPTH))
  val irNumCntB = Module(new CounterVec(AIW_NUMCNT_DEPTH))

  ircmdb.io.enq <> io.vxu.issue.enq.cmdb

  irimm1b.io.enq <> io.vxu.issue.enq.imm1b
  irimm1b.io.update.valid := io.vxu.seq.update.imm1.valid
  irimm1b.io.update.bits.addr := io.vxu.seq.update.imm1.bits.rtag
  irimm1b.io.update.bits.data :=
    Mux(io.vxu.seq.update.imm1.bits.ldst, io.vxu.seq.update.imm1.bits.base, io.vxu.seq.update.imm1.bits.pc_next)
  irimm1b.io.rtag <> io.vxu.issue.rtag.imm1

  irimm2b.io.enq <> io.vxu.issue.enq.imm2b

  ircntb.io.enq <> io.vxu.issue.enq.cntb
  ircntb.io.update.valid := io.vxu.seq.update.cnt.valid
  ircntb.io.update.bits.addr := io.vxu.seq.update.cnt.bits.rtag
  ircntb.io.update.bits.data := io.vxu.seq.update.cnt.bits.utidx
  ircntb.io.rtag <> io.vxu.issue.rtag.cnt

  irNumCntB.io.enq <> io.vxu.issue.enq.numcntb
  irNumCntB.io.update_from_issue <> io.vxu.issue.update.numcnt
  irNumCntB.io.update_from_seq.valid := io.vxu.seq.update.numcnt.valid
  irNumCntB.io.update_from_seq.bits := io.vxu.seq.update.numcnt.bits.rtag
  irNumCntB.io.update_from_evac <> io.evac.update.numcnt
  irNumCntB.io.markLast <> io.vxu.issue.marklast
  irNumCntB.io.rtag <> io.vxu.issue.rtag.numcnt

  val cmd = new HwachaCommand().fromBits(ircmdb.io.deq.bits).cmcode
  val n = Bits(0,1)
  val y = Bits(1,1)

  val cs =
    ListLookup(cmd,
                     //   vf
                     //   |  deq_ircmdb
                     //   |  |  deq_irimm1b
                     //   |  |  |  deq_irimm2b
                     //   |  |  |  |  deq_ircntb
                     //   |  |  |  |  |
                     List(n, n, n, n, n), Array(
    CMD_VF->         List(y, n, y, n, y),

    CMD_VMVV->       List(n, y, n, n, y),
    CMD_VMSV->       List(n, y, y, n, y),
    CMD_VFMVV->      List(n, y, n, n, y),
    CMD_VFMSV_S->    List(n, y, y, n, y),
    CMD_VFMSV_D->    List(n, y, y, n, y),

    CMD_VLD       -> List(n, y, y, n, y),
    CMD_VLW       -> List(n, y, y, n, y),
    CMD_VLWU      -> List(n, y, y, n, y),
    CMD_VLH       -> List(n, y, y, n, y),
    CMD_VLHU      -> List(n, y, y, n, y),
    CMD_VLB       -> List(n, y, y, n, y),
    CMD_VLBU      -> List(n, y, y, n, y),
    CMD_VSD       -> List(n, y, y, n, y),
    CMD_VSW       -> List(n, y, y, n, y),
    CMD_VSH       -> List(n, y, y, n, y),
    CMD_VSB       -> List(n, y, y, n, y),
                     
    CMD_VFLD      -> List(n, y, y, n, y),
    CMD_VFLW      -> List(n, y, y, n, y),
    CMD_VFSD      -> List(n, y, y, n, y),
    CMD_VFSW      -> List(n, y, y, n, y),
                     
    CMD_VLSTD     -> List(n, y, y, y, y),
    CMD_VLSTW     -> List(n, y, y, y, y),
    CMD_VLSTWU    -> List(n, y, y, y, y),
    CMD_VLSTH     -> List(n, y, y, y, y),
    CMD_VLSTHU    -> List(n, y, y, y, y),
    CMD_VLSTB     -> List(n, y, y, y, y),
    CMD_VLSTBU    -> List(n, y, y, y, y),
    CMD_VSSTD     -> List(n, y, y, y, y),
    CMD_VSSTW     -> List(n, y, y, y, y),
    CMD_VSSTH     -> List(n, y, y, y, y),
    CMD_VSSTB     -> List(n, y, y, y, y),

    CMD_VFLSTD    -> List(n, y, y, y, y),
    CMD_VFLSTW    -> List(n, y, y, y, y),
    CMD_VFSSTD    -> List(n, y, y, y, y),
    CMD_VFSSTW    -> List(n, y, y, y, y)
  ))

  val vf :: deq_ircmdb :: deq_irimm1b :: deq_irimm2b :: deq_ircntb :: Nil = cs.map(_.toBool)
  val decode_vf = vf
  val decode_deq_ircmdb = deq_ircmdb
  val decode_deq_irimm1b = deq_irimm1b
  val decode_deq_irimm2b = deq_irimm2b
  val decode_deq_ircntb = deq_ircntb

  // count buffer is dequeued whenever sequencer or evacuator says so
  ircntb.io.deq.ready := io.vxu.seq.update.numcnt.valid && io.vxu.seq.update.numcnt.bits.last | io.evac.deq.cntb.ready

  val do_deq = irNumCntB.io.deq.bits.toBool && irNumCntB.io.deq.valid && irNumCntB.io.deq_last

  // other buffers are dequeued based on NumCntB or when evacuator says so
  ircmdb.io.deq.ready :=  do_deq || io.evac.deq.cmdb.ready
  irimm1b.io.deq.ready := do_deq && decode_deq_irimm1b || io.evac.deq.imm1b.ready
  irimm2b.io.deq.ready := do_deq && decode_deq_irimm2b || io.evac.deq.imm2b.ready
  irNumCntB.io.deq.ready := irNumCntB.io.deq.bits.toBool && irNumCntB.io.deq_last || io.evac.deq.numcntb.ready
    
  io.evac.deq.cmdb.bits := ircmdb.io.deq.bits
  io.evac.deq.imm1b.bits := irimm1b.io.deq.bits
  io.evac.deq.imm2b.bits := irimm2b.io.deq.bits
  io.evac.deq.cntb.bits := ircntb.io.deq.bits
  io.evac.deq.numcntb.bits := irNumCntB.io.deq.bits
  io.evac.deq.numcntb_last := irNumCntB.io.deq_last

  io.evac.deq.cmdb.valid := ircmdb.io.deq.valid
  io.evac.deq.imm1b.valid := irimm1b.io.deq.valid
  io.evac.deq.imm2b.valid := irimm2b.io.deq.valid
  io.evac.deq.cntb.valid := ircntb.io.deq.valid
  io.evac.deq.numcntb.valid := irNumCntB.io.deq.valid
}

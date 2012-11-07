package hwacha

import Chisel._
import Node._
import Constants._
import Commands._

class io_aiw_to_issue extends Bundle 
{
  val imm1_rtag = Bits(OUTPUT, SZ_AIW_IMM1)
  val numCnt_rtag = Bits(OUTPUT, SZ_AIW_NUMCNT)
  val cnt_rtag = Bits(OUTPUT, SZ_AIW_CNT)
}

class io_vu_aiw extends Bundle 
{
  val aiw_enq_cmdb = new io_vxu_cmdq().flip
  val aiw_enq_imm1b = new io_vxu_immq().flip
  val aiw_enq_imm2b = new io_vxu_imm2q().flip
  val aiw_enq_cntb = new io_vxu_cntq().flip
  val aiw_enq_numCntB = new io_vxu_numcntq().flip

  val issue_to_aiw = new io_issue_to_aiw().flip
  val aiw_to_issue = new io_aiw_to_issue()

  val seq_to_aiw = new io_seq_to_aiw().flip

  val aiw_deq_cmdb = new io_vxu_cmdq()
  val aiw_deq_imm1b = new io_vxu_immq()
  val aiw_deq_imm2b = new io_vxu_imm2q()
  val aiw_deq_cntb = new io_vxu_cntq()
  val aiw_deq_numCntB = new io_vxu_numcntq()
  val aiw_deq_numCntB_last = Bool(OUTPUT)

  val evac_to_aiw = new io_evac_to_aiw().flip
}

class vuAIW(resetSignal: Bool = null) extends Component(resetSignal)
{
  val io = new io_vu_aiw()

  val ircmdb = new Queue(AIW_CMD_DEPTH)(Bits(width = SZ_VCMD))
  val irimm1b = new Buffer(SZ_VIMM, AIW_IMM1_DEPTH)
  val irimm2b = new Queue(AIW_IMM2_DEPTH)(Bits(width = SZ_VSTRIDE))
  val ircntb = new Buffer(SZ_VLEN, AIW_CNT_DEPTH)
  val irNumCntB = new CounterVec(AIW_NUMCNT_DEPTH)

  ircmdb.io.enq <> io.aiw_enq_cmdb

  irimm1b.io.enq <> io.aiw_enq_imm1b
  irimm1b.io.update <> io.seq_to_aiw.update_imm1
  irimm1b.io.rtag <> io.aiw_to_issue.imm1_rtag

  irimm2b.io.enq <> io.aiw_enq_imm2b

  ircntb.io.enq <> io.aiw_enq_cntb
  ircntb.io.update <> io.seq_to_aiw.update_cnt
  ircntb.io.rtag <> io.aiw_to_issue.cnt_rtag

  irNumCntB.io.enq <> io.aiw_enq_numCntB
  irNumCntB.io.update_from_issue <> io.issue_to_aiw.update_numCnt
  irNumCntB.io.update_from_seq <> io.seq_to_aiw.update_numCnt
  irNumCntB.io.update_from_evac <> io.evac_to_aiw.update_numCnt
  irNumCntB.io.markLast <> io.issue_to_aiw.markLast
  irNumCntB.io.rtag <> io.aiw_to_issue.numCnt_rtag

  val cmd = ircmdb.io.deq.bits(RG_XCMD_CMCODE)
  val n = Bool(false)
  val y = Bool(true)

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

  val vf :: deq_ircmdb :: deq_irimm1b :: deq_irimm2b :: deq_ircntb :: Nil = cs
  val decode_vf = vf
  val decode_deq_ircmdb = deq_ircmdb
  val decode_deq_irimm1b = deq_irimm1b
  val decode_deq_irimm2b = deq_irimm2b
  val decode_deq_ircntb = deq_ircntb

  // count buffer is dequeued whenever sequencer or evacuator says so
  ircntb.io.deq.ready  := io.seq_to_aiw.last | io.aiw_deq_cntb.ready

  val do_deq = irNumCntB.io.deq.bits && irNumCntB.io.deq.valid && irNumCntB.io.deq_last

  // other buffers are dequeued based on NumCntB or when evacuator says so
  ircmdb.io.deq.ready :=  do_deq || io.aiw_deq_cmdb.ready
  irimm1b.io.deq.ready := do_deq && decode_deq_irimm1b || io.aiw_deq_imm1b.ready
  irimm2b.io.deq.ready := do_deq && decode_deq_irimm2b || io.aiw_deq_imm2b.ready
  irNumCntB.io.deq.ready := irNumCntB.io.deq.bits && irNumCntB.io.deq_last || io.aiw_deq_numCntB.ready
    
  io.aiw_deq_cmdb.bits := ircmdb.io.deq.bits
  io.aiw_deq_imm1b.bits := irimm1b.io.deq.bits
  io.aiw_deq_imm2b.bits := irimm2b.io.deq.bits
  io.aiw_deq_cntb.bits := ircntb.io.deq.bits
  io.aiw_deq_numCntB.bits := irNumCntB.io.deq.bits
  io.aiw_deq_numCntB_last := irNumCntB.io.deq_last

  io.aiw_deq_cmdb.valid := ircmdb.io.deq.valid
  io.aiw_deq_imm1b.valid := irimm1b.io.deq.valid
  io.aiw_deq_imm2b.valid := irimm2b.io.deq.valid
  io.aiw_deq_cntb.valid := ircntb.io.deq.valid
  io.aiw_deq_numCntB.valid := irNumCntB.io.deq.valid
}

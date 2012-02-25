package hwacha

import Chisel._
import Node._
import Constants._
import Instructions._
import queues._

class io_vu_irb extends Bundle 
{
  val irb_cmdb = new io_vxu_cmdq().flip()
  val irb_imm1b = new io_vxu_immq().flip()
  val irb_imm2b = new io_vxu_imm2q().flip()
  val irb_cntb = new io_vxu_cntq().flip()

  val issue_to_irb = new io_issue_to_irb().flip()
  val irb_to_issue = new io_irb_to_issue()

  val seq_to_irb = new io_seq_to_irb().flip()

  val mem = new io_irb_sdb()
}

class vuIRB extends Component 
{
  val io = new io_vu_irb()

  val ircmdb = new queueSimplePF(IRB_CMD_DEPTH)({Bits(width=SZ_VCMD)})
  val irimm1b = new Buffer(SZ_VIMM, IRB_IMM1_DEPTH)
  val irimm2b = new queueSimplePF(IRB_IMM2_DEPTH)({Bits(width=SZ_VSTRIDE)})
  val ircntb = new Buffer(SZ_VLEN, IRB_CNT_DEPTH, true)

  ircmdb.io.enq <> io.irb_cmdb

  irimm1b.io.enq <> io.irb_imm1b
  irimm1b.io.update <> io.seq_to_irb.update_imm1
  irimm1b.io.rtag <> io.irb_to_issue.imm1_rtag

  irimm2b.io.enq <> io.irb_imm2b
  
  ircntb.io.enq <> io.irb_cntb
  ircntb.io.update <> io.seq_to_irb.update_cnt
  ircntb.io.markLast <> io.issue_to_irb.markLast
  ircntb.io.rtag <> io.irb_to_issue.cnt_rtag

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

    //CMD_VMVV->       List(Bits("b001",3),Bits("b11",2),Bits("b00",2),Bits("b001",3),MR,n,n,y,n,n,n,n,n,Bits(0,4),MTF_X,MT_X,M_X),
    //CMD_VMSV->       List(Bits("b001",3),Bits("b10",2),Bits("b00",2),Bits("b001",3),MI,n,n,y,n,n,n,y,n,Bits(0,4),MTF_X,MT_X,M_X),
    //CMD_VFMVV->      List(Bits("b001",3),Bits("b11",2),Bits("b00",2),Bits("b001",3),MR,n,n,y,n,n,n,n,n,Bits(0,4),MTF_X,MT_X,M_X),

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
                     
    CMD_VLSTD     -> List(n, y, y, n, y),
    CMD_VLSTW     -> List(n, y, y, n, y),
    CMD_VLSTWU    -> List(n, y, y, n, y),
    CMD_VLSTH     -> List(n, y, y, n, y),
    CMD_VLSTHU    -> List(n, y, y, n, y),
    CMD_VLSTB     -> List(n, y, y, n, y),
    CMD_VLSTBU    -> List(n, y, y, n, y),
    CMD_VSSTD     -> List(n, y, y, n, y),
    CMD_VSSTW     -> List(n, y, y, n, y),
    CMD_VSSTH     -> List(n, y, y, n, y),
    CMD_VSSTB     -> List(n, y, y, n, y),

    CMD_VFLSTD    -> List(n, y, y, y, y),
    CMD_VFLSTW    -> List(n, y, y, y, y),
    CMD_VFSSTD    -> List(n, y, y, y, y),
    CMD_VFSSTW    -> List(n, y, y, y, y)
  ))

  val vf :: deq_ircmdb :: deq_irimm1b :: deq_irimm2b :: deq_ircntb :: Nil = cs
  val decode_vf = vf
  val decode_deq_ircmdb = deq_ircmdb.toBool
  val decode_deq_irimm1b = deq_irimm1b.toBool
  val decode_deq_irimm2b = deq_irimm2b.toBool
  val decode_deq_ircntb = deq_ircntb.toBool

  ircmdb.io.deq.ready  := io.seq_to_irb.last & ((decode_vf & ircntb.io.deq_last) | decode_deq_ircmdb)
  irimm1b.io.deq.ready := io.seq_to_irb.last & decode_deq_irimm1b
  irimm2b.io.deq.ready := io.seq_to_irb.last & decode_deq_irimm2b
  ircntb.io.deq.ready  := io.seq_to_irb.last & decode_deq_ircntb
    
  io.mem.valid := ircmdb.io.deq.valid || irimm1b.io.deq.valid || irimm2b.io.deq.valid || ircntb.io.deq.valid
  io.mem.bits := 
    MuxCase(Bits(0), Array(
      ircmdb.io.deq.valid -> ircmdb.io.deq.bits,
      irimm1b.io.deq.valid -> irimm1b.io.deq.bits,
      irimm2b.io.deq.valid -> irimm2b.io.deq.bits,
      ircntb.io.deq.valid -> ircntb.io.deq.bits
    ))
}

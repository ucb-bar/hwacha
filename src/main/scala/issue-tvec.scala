package hwacha

import Chisel._
import Node._
import Constants._
import Commands._
import uncore.constants.MemoryOpConstants._

class io_issue_tvec_to_irq_handler extends Bundle
{
  val illegal = Bool(OUTPUT)
  val cmd = Bits(OUTPUT, SZ_XCMD)
}

class ioIssueTVECToPC extends Bundle
{
  val pc = Bits(OUTPUT, SZ_ADDR)
  val vlen = Bits(OUTPUT, SZ_VLEN)
  val fire = Bool(OUTPUT)
}

class IssueTVEC extends Module
{
  val io = new Bundle {
    val irq = new io_issue_tvec_to_irq_handler()

    val vf = new io_vf()
    val active = Bool(OUTPUT)

    val issue_to_hazard = new io_vxu_issue_to_hazard().asOutput
    val issue_to_seq = new io_vxu_issue_to_seq().asOutput
    val issue_to_lane = new io_vxu_issue_to_lane().asOutput

    val vcmdq = new VCMDQIO().flip
    
    val valid = new io_vxu_issue_fire().asOutput
    val ready = Bool(INPUT)
    val dhazard = new io_vxu_issue_reg().asOutput
    val shazard = new io_vxu_issue_fu().asOutput
    val bhazard = new io_vxu_issue_op().asOutput
    val fn = new io_vxu_issue_fn().asOutput
    val decoded = new io_vxu_issue_regid_imm().asOutput

    val aiw_cmdb = new io_vxu_cmdq()
    val aiw_imm1b = new io_vxu_immq()
    val aiw_imm2b = new io_vxu_imm2q()
    val aiw_cntb = new io_vxu_cntq()
    val aiw_numCntB = new io_vxu_numcntq()
    val aiw_to_issue = new io_aiw_to_issue().flip
    val issue_to_aiw = new io_issue_to_aiw()

    val xcpt_to_issue = new io_xcpt_handler_to_issue().flip()

    val prec = Bits(OUTPUT, SZ_PREC)
  }

  val ISSUE_TVEC = Bits(0,1)
  val ISSUE_VT = Bits(1,1)

  val next_state = Bits(width = 1)
  val reg_state = Reg(next = next_state, init = ISSUE_TVEC)

  val tvec_active = (reg_state === ISSUE_TVEC)
  io.active := tvec_active    


//-------------------------------------------------------------------------\\
// DECODE                                                                  \\
//-------------------------------------------------------------------------\\

  val cmd = io.vcmdq.cmd.bits.cmcode
  val vd = io.vcmdq.cmd.bits.vd
  val vt = io.vcmdq.cmd.bits.vt
  val imm1 = new HwachaImm1().fromBits(io.vcmdq.imm1.bits)
  val imm2 = io.vcmdq.imm2.bits

  val n = Bool(false)
  val y = Bool(true)

  val stall_sticky = Reg(init=Bool(false))
  val stall = io.irq.illegal || stall_sticky || io.xcpt_to_issue.stall

  when (io.irq.illegal) { stall_sticky := Bool(true) }
  

  val cs =
  ListLookup(cmd,
                     //                                                                 vd_valid
                     //                                                                 | vd_active
                     //                                                                 | | vt_active
                     //                                                                 | | | decode_vcfg
                     //                                                                 | | | | decode_setvl
                     //                                                                 | | | | | decode_vf
                     //                                                                 | | | | | | deq_vcmdq_imm1
                     //         val            dhazard       shazard        bhazard msrc| | | | | | | deq_vcmdq_imm2
                     //         |              |             |              |        |  | | | | | | | | deq_vxu_cnt
                     //         |              |             |              |        |  | | | | | | | | | stride  mem_type_float
                     //         |              |             |              |        |  | | | | | | | | | |         |   mem_type mem_cmd
                     //         |              |             |              |        |  | | | | | | | | | |         |     |      |
                     List(Bits("b000",3),Bits("b00",2),Bits("b000",3),Bits("b000",3),M0,n,n,n,n,n,n,n,n,n,Bits(0,4),MTF_X,MT_X,M_X),Array(
    CMD_VVCFGIVL->   List(Bits("b000",3),Bits("b00",2),Bits("b000",3),Bits("b000",3),M0,n,n,n,y,y,n,y,n,n,Bits(0,4),MTF_X,MT_X,M_X),
    CMD_VSETVL->     List(Bits("b000",3),Bits("b00",2),Bits("b000",3),Bits("b000",3),M0,n,n,n,n,y,n,y,n,n,Bits(0,4),MTF_X,MT_X,M_X),
    CMD_VF->         List(Bits("b000",3),Bits("b00",2),Bits("b000",3),Bits("b000",3),M0,n,n,n,n,n,y,y,n,n,Bits(0,4),MTF_X,MT_X,M_X),

    CMD_VMVV->       List(Bits("b001",3),Bits("b11",2),Bits("b000",3),Bits("b001",3),MR,y,y,y,n,n,n,n,n,y,Bits(0,4),MTF_X,MT_X,M_X),
    CMD_VMSV->       List(Bits("b001",3),Bits("b10",2),Bits("b000",3),Bits("b001",3),MI,y,y,n,n,n,n,y,n,y,Bits(0,4),MTF_X,MT_X,M_X),
    CMD_VFMVV->      List(Bits("b001",3),Bits("b11",2),Bits("b000",3),Bits("b001",3),MR,y,y,y,n,n,n,n,n,y,Bits(0,4),MTF_X,MT_X,M_X),

    CMD_VLD       -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,n,y,Bits(8,4),MTF_X,MT_D,M_XRD),
    CMD_VLW       -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,n,y,Bits(4,4),MTF_X,MT_W,M_XRD),
    CMD_VLWU      -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,n,y,Bits(4,4),MTF_X,MT_WU,M_XRD),
    CMD_VLH       -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,n,y,Bits(2,4),MTF_X,MT_H,M_XRD),
    CMD_VLHU      -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,n,y,Bits(2,4),MTF_X,MT_HU,M_XRD),
    CMD_VLB       -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,n,y,Bits(1,4),MTF_X,MT_B,M_XRD),
    CMD_VLBU      -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,n,y,Bits(1,4),MTF_X,MT_BU,M_XRD),
    CMD_VSD       -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,y,n,n,n,y,n,y,Bits(8,4),MTF_X,MT_D,M_XWR),
    CMD_VSW       -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,y,n,n,n,y,n,y,Bits(4,4),MTF_X,MT_W,M_XWR),
    CMD_VSH       -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,y,n,n,n,y,n,y,Bits(2,4),MTF_X,MT_H,M_XWR),
    CMD_VSB       -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,y,n,n,n,y,n,y,Bits(1,4),MTF_X,MT_B,M_XWR),

    CMD_VFLD      -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,n,y,Bits(8,4),MTF_Y,MT_D,M_XRD),
    CMD_VFLW      -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,n,y,Bits(4,4),MTF_Y,MT_W,M_XRD),
    CMD_VFSD      -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,y,n,n,n,y,n,y,Bits(8,4),MTF_Y,MT_D,M_XWR),
    CMD_VFSW      -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,y,n,n,n,y,n,y,Bits(4,4),MTF_Y,MT_W,M_XWR),

    CMD_VLSTD     -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,MT_D,M_XRD),
    CMD_VLSTW     -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,MT_W,M_XRD),
    CMD_VLSTWU    -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,MT_WU,M_XRD),
    CMD_VLSTH     -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,MT_H,M_XRD),
    CMD_VLSTHU    -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,MT_HU,M_XRD),
    CMD_VLSTB     -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,MT_B,M_XRD),
    CMD_VLSTBU    -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,MT_BU,M_XRD),
    CMD_VSSTD     -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,y,n,n,n,y,y,y,Bits(0,4),MTF_X,MT_D,M_XWR),
    CMD_VSSTW     -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,y,n,n,n,y,y,y,Bits(0,4),MTF_X,MT_W,M_XWR),
    CMD_VSSTH     -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,y,n,n,n,y,y,y,Bits(0,4),MTF_X,MT_H,M_XWR),
    CMD_VSSTB     -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,y,n,n,n,y,y,y,Bits(0,4),MTF_X,MT_B,M_XWR),

    CMD_VFLSTD    -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_Y,MT_D,M_XRD),
    CMD_VFLSTW    -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_Y,MT_W,M_XRD),
    CMD_VFSSTD    -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,y,n,n,n,y,y,y,Bits(0,4),MTF_Y,MT_D,M_XWR),
    CMD_VFSSTW    -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,y,n,n,n,y,y,y,Bits(0,4),MTF_Y,MT_W,M_XWR)
  ))

  val valid::dhazard::shazard::bhazard::vmsrc::cs0 = cs
  val vd_valid::vd_active::vt_active::decode_vcfg::decode_setvl::decode_vf::deq_vcmdq_imm1::deq_vcmdq_imm2::deq_vcmdq_cnt::cs1 = cs0
  val addr_stride::mem_type_float::mem_type::mem_cmd::Nil = cs1

  val decode_aiw_cmdb_valid = valid.orR || decode_vf.toBool
  val decode_aiw_imm1b_valid = decode_aiw_cmdb_valid && deq_vcmdq_imm1.toBool
  val decode_aiw_imm2b_valid = decode_aiw_cmdb_valid && deq_vcmdq_imm2.toBool
  val decode_aiw_cntb_valid = valid.orR
  val decode_aiw_numCntB_valid = valid.orR || decode_vf.toBool

//-------------------------------------------------------------------------\\
// FIRE & QUEUE LOGIC                                                      \\
//-------------------------------------------------------------------------\\

  val mask_issue_ready = !valid.orR || io.ready
  val mask_vxu_immq_valid = !deq_vcmdq_imm1 || io.vcmdq.imm1.valid
  val mask_vxu_imm2q_valid = !deq_vcmdq_imm2 || io.vcmdq.imm2.valid
  val mask_aiw_cmdb_ready = !decode_aiw_cmdb_valid || io.aiw_cmdb.ready
  val mask_aiw_imm1b_ready = !deq_vcmdq_imm1 || io.aiw_imm1b.ready
  val mask_aiw_imm2b_ready = !deq_vcmdq_imm2 || io.aiw_imm2b.ready
  val mask_aiw_cntb_ready = !decode_aiw_cntb_valid || io.aiw_cntb.ready
  val mask_aiw_numCntB_ready = !decode_aiw_numCntB_valid || io.aiw_numCntB.ready

  val valid_common =
    !stall &&
    tvec_active &&
    io.vcmdq.cmd.valid && mask_vxu_immq_valid && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  val fire_common = mask_issue_ready && valid_common

  val fire_vcfg = fire_common && decode_vcfg.toBool
  val fire_setvl = fire_common && decode_setvl.toBool
  val fire_vf = fire_common && decode_vf.toBool

  val queue_common = tvec_active && mask_issue_ready && !stall 

  io.vcmdq.cmd.ready := 
    queue_common && 
    Bool(true) && mask_vxu_immq_valid && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.vcmdq.imm1.ready := 
    queue_common &&
    io.vcmdq.cmd.valid && deq_vcmdq_imm1.toBool && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.vcmdq.imm2.ready := 
    queue_common &&
    io.vcmdq.cmd.valid && mask_vxu_immq_valid && deq_vcmdq_imm2.toBool &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.vcmdq.cnt.ready :=
    queue_common &&
    io.vcmdq.cmd.valid && mask_vxu_immq_valid && mask_vxu_imm2q_valid && deq_vcmdq_cnt.toBool &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.aiw_cmdb.valid := 
    queue_common &&
    io.vcmdq.cmd.valid && mask_vxu_immq_valid && mask_vxu_imm2q_valid &&
    decode_aiw_cmdb_valid && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.aiw_imm1b.valid :=
    queue_common &&
    io.vcmdq.cmd.valid && decode_aiw_imm1b_valid && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && deq_vcmdq_imm1.toBool && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.aiw_imm2b.valid :=
    queue_common &&
    io.vcmdq.cmd.valid && mask_vxu_immq_valid && decode_aiw_imm2b_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && deq_vcmdq_imm2.toBool && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.aiw_cntb.valid :=
    queue_common &&
    io.vcmdq.cmd.valid && mask_vxu_immq_valid && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && decode_aiw_cntb_valid && mask_aiw_numCntB_ready
  
  io.aiw_numCntB.valid := 
    queue_common &&
    io.vcmdq.cmd.valid && mask_vxu_immq_valid && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && decode_aiw_numCntB_valid

  io.issue_to_aiw.markLast := 
    queue_common && valid.orR &&
    io.vcmdq.cmd.valid && mask_vxu_immq_valid && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready
    

  io.aiw_cmdb.bits := io.vcmdq.cmd.bits.toBits
  io.aiw_imm1b.bits := io.vcmdq.imm1.bits
  io.aiw_imm2b.bits := io.vcmdq.imm2.bits
  io.aiw_numCntB.bits := Mux(decode_vf.toBool, Bits(0, 1), Bits(1, 1))


//-------------------------------------------------------------------------\\
// REGISTERS                                                               \\
//-------------------------------------------------------------------------\\

  val next_vlen = Bits(width = SZ_VLEN)
  val next_nxregs = Bits(width = SZ_REGCNT)
  val next_nfregs = Bits(width = SZ_REGCNT)
  val next_eff_nfregs = Bits(width = SZ_REGCNT)
  val next_bactive = Bits(width = SZ_BANK)
  val next_bcnt = Bits(width = SZ_BCNT)
  val next_xstride = Bits(width = SZ_REGLEN)
  val next_fstride = Bits(width = SZ_REGLEN)
  val next_xf_split = Bits(width = SZ_BANK)
  val next_precision = Bits(width = SZ_PREC)

  val reg_vlen = Reg(next = next_vlen, init = Bits(0,SZ_VLEN))
  val reg_nxregs = Reg(next = next_nxregs, init = Bits(32,SZ_REGCNT))
  val reg_nfregs = Reg(next = next_nfregs, init = Bits(32,SZ_REGCNT))
  val reg_eff_nfregs = Reg(next = next_eff_nfregs, init = Bits(32,SZ_REGCNT))
  val reg_bactive = Reg(next = next_bactive, init = Bits("b1111_1111",SZ_BANK))
  val reg_bcnt = Reg(next = next_bcnt, init = Bits(8,SZ_LGBANK1))
  val reg_xstride = Reg(next = next_xstride, init = Bits(63,SZ_REGLEN))
  val reg_fstride = Reg(next = next_fstride, init = Bits(63,SZ_REGLEN))
  val reg_xf_split = Reg(next = next_xf_split, init = Bits(0,SZ_BANK))
  val reg_precision = Reg(next = next_precision, init = PREC_DOUBLE)

  val cnt = Mux(io.vcmdq.cnt.valid, io.vcmdq.cnt.bits, Bits(0))
  val regid_xbase = (cnt >> UInt(3)) * reg_xstride
  val regid_fbase = ((cnt >> UInt(3)) * reg_fstride) + reg_xf_split

  io.aiw_cntb.bits := cnt

  next_state := reg_state
  next_vlen := reg_vlen
  next_nxregs := reg_nxregs
  next_nfregs := reg_nfregs
  next_eff_nfregs := reg_eff_nfregs
  next_bactive := reg_bactive
  next_bcnt := reg_bcnt
  next_xstride := reg_xstride
  next_fstride := reg_fstride
  next_xf_split := reg_xf_split
  next_precision := reg_precision

  next_eff_nfregs := MuxLookup(
    next_precision, next_nfregs, Array(
    PREC_DOUBLE -> (next_nfregs),
    PREC_SINGLE -> ((next_nfregs + UInt(1)) >> UInt(1)),
    PREC_HALF -> ((next_nfregs + UInt(3)) >> UInt(2))
  ))

  when (fire_vcfg)
  {
    next_vlen := imm1.vlen
    next_nxregs := imm1.nxregs
    next_nfregs := imm1.nfregs
    next_bactive := imm1.bactive
    next_bcnt := imm1.bcnt
    next_xstride := next_nxregs - UInt(1)
    next_fstride := next_eff_nfregs
    // location of X/F register split in bank: number of xregs times the number of uts per bank
    next_xf_split := imm1.xf_split // don't add back 1

    next_precision := MuxLookup(
      imm1.prec, PREC_DOUBLE, Array(
      UInt(2,2) -> PREC_HALF,
      UInt(1,2) -> PREC_SINGLE,
      UInt(0,2) -> PREC_DOUBLE))
  }
  when (fire_setvl)
  {
    next_vlen := imm1.vlen
  }
  when (fire_vf)
  {
    next_state := ISSUE_VT
  }
  when (io.vf.stop)
  {
    next_state := ISSUE_TVEC
  }



//-------------------------------------------------------------------------\\
// SIGNALS                                                                 \\
//-------------------------------------------------------------------------\\

  io.vf.active := (reg_state === ISSUE_VT)
  io.vf.fire := fire_vf
  io.vf.pc := io.vcmdq.imm1.bits(31,0).toUInt
  io.vf.nxregs := reg_nxregs
  io.vf.nfregs := reg_nfregs
  io.vf.imm1_rtag := io.aiw_to_issue.imm1_rtag
  io.vf.numCnt_rtag := io.aiw_to_issue.numCnt_rtag
  io.vf.xstride := reg_xstride
  io.vf.fstride := reg_fstride
  io.vf.xf_split := reg_xf_split
  io.vf.vlen := reg_vlen

  val rtype_vd = vd(5)
  val rtype_vt = vt(5)

  io.issue_to_hazard.bcnt := reg_bcnt
  io.issue_to_hazard.stride := Mux(rtype_vd, reg_fstride, reg_xstride)
  io.issue_to_seq.vlen := reg_vlen - cnt
  io.issue_to_seq.xf_split := reg_xf_split
  io.issue_to_seq.xstride := reg_xstride
  io.issue_to_seq.fstride := reg_fstride
  io.issue_to_seq.bcnt := reg_bcnt
  io.issue_to_lane.bactive := reg_bactive

  io.prec := reg_precision


//-------------------------------------------------------------------------\\
// ISSUE                                                                   \\
//-------------------------------------------------------------------------\\

  io.valid.viu := valid_common && valid(0)
  io.valid.vau0 := Bool(false)
  io.valid.vau1 := Bool(false)
  io.valid.vau2 := Bool(false)
  io.valid.amo := Bool(false)
  io.valid.utld := Bool(false)
  io.valid.utst := Bool(false)
  io.valid.vld := valid_common && valid(1)
  io.valid.vst := valid_common && valid(2)

  io.dhazard.vs := Bool(false)
  io.dhazard.vt := dhazard(0)
  io.dhazard.vr := Bool(false)
  io.dhazard.vd := dhazard(1)

  io.shazard.viu := Bool(false)
  io.shazard.vau0 := Bool(false)
  io.shazard.vau1 := Bool(false)
  io.shazard.vau2 := Bool(false)
  io.shazard.vgu := shazard(2)
  io.shazard.vlu := shazard(1)
  io.shazard.vsu := shazard(0)

  io.bhazard.r1w1 := bhazard(0)
  io.bhazard.r2w1 := Bool(false)
  io.bhazard.r3w1 := Bool(false)
  io.bhazard.amo := Bool(false)
  io.bhazard.utld := Bool(false)
  io.bhazard.utst := Bool(false)
  io.bhazard.vld := bhazard(1)
  io.bhazard.vst := bhazard(2)

  io.fn.vbr := Bits(0, SZ_VBR_FN)
  io.fn.viu := Cat(M0,vmsrc,DW64,FP_,viu_MOV)
  io.fn.vau0 := Bits(0,SZ_VAU0_FN)
  io.fn.vau1 := Bits(0,SZ_VAU1_FN)
  io.fn.vau2 := Bits(0,SZ_VAU2_FN)

  val vt_m1 = Cat(Bits(0,1),vt(4,0)) - UInt(1,1)
  val vd_m1 = Cat(Bits(0,1),vd(4,0)) - UInt(1,1)

  io.decoded.vlen := reg_vlen - cnt
  io.decoded.utidx := Bits(0)
  // nxregs includes the zero register, which makes things "work out"
  io.decoded.vd_base := Mux(rtype_vd, regid_fbase + vd(4,0), regid_xbase + vd_m1)
  io.decoded.vs := Bits(0,SZ_BREGLEN)
  io.decoded.vt := Mux(rtype_vt, regid_fbase + vt(4,0), regid_xbase + vt_m1)
  io.decoded.vr := Bits(0,SZ_BREGLEN)
  io.decoded.vd := Mux(rtype_vd, regid_fbase + vd(4,0), regid_xbase + vd_m1)
  io.decoded.vs_zero := Bool(true)
  io.decoded.vt_zero := vt === Bits(0,6)
  io.decoded.vr_zero := Bool(true)
  io.decoded.vd_zero := vd === Bits(0,6) && vd_valid.toBool
  io.decoded.vs_active := Bool(false)
  io.decoded.vt_active := vt_active.toBool
  io.decoded.vr_active := Bool(false)
  io.decoded.vd_active := vd_active.toBool
  io.decoded.rtype := Cat(Bool(false), rtype_vt, Bool(false), rtype_vd) // vs vt vr vd
  io.decoded.mem.cmd := mem_cmd
  io.decoded.mem.typ := mem_type
  io.decoded.mem.typ_float := mem_type_float.toBool
  io.decoded.imm := io.vcmdq.imm1.bits
  io.decoded.imm2 := Mux(io.vcmdq.imm2.ready, imm2, Cat(Bits(0,60), addr_stride))
  io.decoded.cnt_valid := io.vcmdq.cnt.valid
  io.decoded.cnt := cnt
  io.decoded.aiw.imm1_rtag := io.aiw_to_issue.imm1_rtag
  io.decoded.aiw.numCnt_rtag := io.aiw_to_issue.numCnt_rtag
  io.decoded.aiw.cnt_rtag := io.aiw_to_issue.cnt_rtag
  io.decoded.aiw.update_imm1 := !io.valid.viu

  val illegal_vd = vd_active.toBool && (vd(4,0) >= reg_nfregs && rtype_vd || vd(4,0) >= reg_nxregs && !rtype_vd)
  val illegal_vt = vt_active.toBool && (vt(4,0) >= reg_nfregs && rtype_vt || vt(4,0) >= reg_nxregs && !rtype_vt)
  
  io.irq.illegal := 
    io.vcmdq.cmd.valid && tvec_active && 
    (!valid.orR && !decode_vcfg && !decode_setvl && !decode_vf || illegal_vd || illegal_vt)
  io.irq.cmd := io.vcmdq.cmd.bits.toBits
}

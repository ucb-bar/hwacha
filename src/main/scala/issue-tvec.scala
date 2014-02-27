package hwacha

import Chisel._
import Node._
import Constants._
import Commands._
import uncore.constants.MemoryOpConstants._

class HwachaConfigIO extends Bundle
{
  val prec = Bits(OUTPUT, SZ_PREC)
  val bactive = Bits(OUTPUT, SZ_BANK)
  val bcnt = UInt(OUTPUT, SZ_BCNT)
  val nxregs = UInt(OUTPUT, SZ_REGCNT)
  val nfregs = UInt(OUTPUT, SZ_REGCNT)
  val xstride = UInt(OUTPUT, SZ_REGLEN)
  val fstride = UInt(OUTPUT, SZ_REGLEN)
  val xfsplit = UInt(OUTPUT, SZ_BREGLEN)
}

object TVECDecodeTable
{
                  //                                   deq_vcmdq_imm1
                  //                                   | deq_vcmdq_imm2
                  //                                   | | deq_vcmdq_cnt
                  //                                   | | | decode_vcfg
                  //                                   | | | | decode_vsetvl
                  //  vd vt VIUfn VMUfn                | | | | | decode_vf
                  //   |  |  |    |                    | | | | | |
  val default =   List(R_,R_,F,M0,F,MT_X, M_X,  VM_X,  F,F,F,F,F,F)
  val table = Array(
    CMD_VSETCFG-> List(R_,R_,F,M0,F,MT_X, M_X,  VM_X,  T,F,F,T,T,F),
    CMD_VSETVL->  List(R_,R_,F,M0,F,MT_X, M_X,  VM_X,  T,F,F,F,T,F),
    CMD_VF->      List(R_,R_,F,M0,F,MT_X, M_X,  VM_X,  T,F,F,F,F,T),

    CMD_VMVV->    List(RX,RX,T,MR,F,MT_X, M_X,  VM_X,  F,F,T,F,F,F),
    CMD_VMSV->    List(RX,R_,T,MI,F,MT_X, M_X,  VM_X,  T,F,T,F,F,F),
    CMD_VFMVV->   List(RF,RF,T,MR,F,MT_X, M_X,  VM_X,  F,F,T,F,F,F),

    CMD_VLD->     List(RX,R_,F,M0,T,MT_D, M_XRD,VM_VLD,T,F,T,F,F,F),
    CMD_VLW->     List(RX,R_,F,M0,T,MT_W, M_XRD,VM_VLD,T,F,T,F,F,F),
    CMD_VLWU->    List(RX,R_,F,M0,T,MT_WU,M_XRD,VM_VLD,T,F,T,F,F,F),
    CMD_VLH->     List(RX,R_,F,M0,T,MT_H, M_XRD,VM_VLD,T,F,T,F,F,F),
    CMD_VLHU->    List(RX,R_,F,M0,T,MT_HU,M_XRD,VM_VLD,T,F,T,F,F,F),
    CMD_VLB->     List(RX,R_,F,M0,T,MT_B, M_XRD,VM_VLD,T,F,T,F,F,F),
    CMD_VLBU->    List(RX,R_,F,M0,T,MT_BU,M_XRD,VM_VLD,T,F,T,F,F,F),
    CMD_VSD->     List(R_,RX,F,M0,T,MT_D, M_XWR,VM_VST,T,F,T,F,F,F),
    CMD_VSW->     List(R_,RX,F,M0,T,MT_W, M_XWR,VM_VST,T,F,T,F,F,F),
    CMD_VSH->     List(R_,RX,F,M0,T,MT_H, M_XWR,VM_VST,T,F,T,F,F,F),
    CMD_VSB->     List(R_,RX,F,M0,T,MT_B, M_XWR,VM_VST,T,F,T,F,F,F),

    CMD_VFLD->    List(RF,R_,F,M0,T,MT_D, M_XRD,VM_VLD,T,F,T,F,F,F),
    CMD_VFLW->    List(RF,R_,F,M0,T,MT_W, M_XRD,VM_VLD,T,F,T,F,F,F),
    CMD_VFSD->    List(R_,RF,F,M0,T,MT_D, M_XWR,VM_VST,T,F,T,F,F,F),
    CMD_VFSW->    List(R_,RF,F,M0,T,MT_W, M_XWR,VM_VST,T,F,T,F,F,F),

    CMD_VLSTD->   List(RX,R_,F,M0,T,MT_D, M_XRD,VM_VLD,T,T,T,F,F,F),
    CMD_VLSTW->   List(RX,R_,F,M0,T,MT_W, M_XRD,VM_VLD,T,T,T,F,F,F),
    CMD_VLSTWU->  List(RX,R_,F,M0,T,MT_WU,M_XRD,VM_VLD,T,T,T,F,F,F),
    CMD_VLSTH->   List(RX,R_,F,M0,T,MT_H, M_XRD,VM_VLD,T,T,T,F,F,F),
    CMD_VLSTHU->  List(RX,R_,F,M0,T,MT_HU,M_XRD,VM_VLD,T,T,T,F,F,F),
    CMD_VLSTB->   List(RX,R_,F,M0,T,MT_B, M_XRD,VM_VLD,T,T,T,F,F,F),
    CMD_VLSTBU->  List(RX,R_,F,M0,T,MT_BU,M_XRD,VM_VLD,T,T,T,F,F,F),
    CMD_VSSTD->   List(R_,RX,F,M0,T,MT_D, M_XWR,VM_VST,T,T,T,F,F,F),
    CMD_VSSTW->   List(R_,RX,F,M0,T,MT_W, M_XWR,VM_VST,T,T,T,F,F,F),
    CMD_VSSTH->   List(R_,RX,F,M0,T,MT_H, M_XWR,VM_VST,T,T,T,F,F,F),
    CMD_VSSTB->   List(R_,RX,F,M0,T,MT_B, M_XWR,VM_VST,T,T,T,F,F,F),

    CMD_VFLSTD->  List(RF,R_,F,M0,T,MT_D, M_XRD,VM_VLD,T,T,T,F,F,F),
    CMD_VFLSTW->  List(RF,R_,F,M0,T,MT_W, M_XRD,VM_VLD,T,T,T,F,F,F),
    CMD_VFSSTD->  List(R_,RF,F,M0,T,MT_D, M_XWR,VM_VST,T,T,T,F,F,F),
    CMD_VFSSTW->  List(R_,RF,F,M0,T,MT_W, M_XWR,VM_VST,T,T,T,F,F,F)
  )
}

class IssueTVEC extends Module
{
  val io = new Bundle {
    val cfg = new HwachaConfigIO
    val xcpt = new XCPTIO().flip

    val active = Bool(OUTPUT)
    val vf = new VFIO
    val vcmdq = new VCMDQIO().flip

    val ready = Bool(INPUT)
    val op = new IssueOpIO

    val aiw_cmdb = new io_vxu_cmdq()
    val aiw_imm1b = new io_vxu_immq()
    val aiw_imm2b = new io_vxu_imm2q()
    val aiw_cntb = new io_vxu_cntq()
    val aiw_numCntB = new io_vxu_numcntq()
    val aiw_to_issue = new io_aiw_to_issue().flip
    val issue_to_aiw = new io_issue_to_aiw()
  }

  val ISSUE_TVEC = UInt(0,1)
  val ISSUE_VT = UInt(1,1)

  val reg_state = Reg(init = ISSUE_TVEC)
  val tvec_active = (reg_state === ISSUE_TVEC)

  val reg_vlen = Reg(init = Bits(0,SZ_VLEN))
  val reg_nxregs = Reg(init = Bits(32,SZ_REGCNT))
  val reg_nfregs = Reg(init = Bits(32,SZ_REGCNT))
  val reg_bactive = Reg(init = Bits("b1111_1111",SZ_BANK))
  val reg_bcnt = Reg(init = Bits(8,SZ_LGBANK1))
  val reg_xstride = Reg(init = Bits(31,SZ_REGLEN))
  val reg_fstride = Reg(init = Bits(32,SZ_REGLEN))
  val reg_xf_split = Reg(init = Bits(31*4,SZ_BANK))
  val reg_precision = Reg(init = PREC_DOUBLE)

  val stall = io.xcpt.prop.issue.stall


//-------------------------------------------------------------------------\\
// DECODE                                                                  \\
//-------------------------------------------------------------------------\\

  val cmd = io.vcmdq.cmd.bits.cmcode
  val vd = io.vcmdq.cmd.bits.vd
  val vt = io.vcmdq.cmd.bits.vt
  val imm1 = new HwachaImm1().fromBits(io.vcmdq.imm1.bits)
  val imm2 = io.vcmdq.imm2.bits

  val cs = rocket.DecodeLogic(cmd, TVECDecodeTable.default, TVECDecodeTable.table)

  val vdi :: vti :: cs0 = cs
  val (viu_val: Bool) :: viu_t1 :: cs1 = cs0
  val (vmu_val: Bool) :: vmu_type :: vmu_cmd :: vmu_op :: cs2 = cs1
  val (deq_vcmdq_imm1: Bool) :: (deq_vcmdq_imm2: Bool) :: (deq_vcmdq_cnt: Bool) :: cs3 = cs2
  val (decode_vcfg: Bool) :: (decode_vsetvl: Bool) :: (decode_vf: Bool) :: Nil = cs3

  val vd_val :: vd_fp :: Nil = parse_rinfo(vdi)
  val vt_val :: vt_fp :: Nil = parse_rinfo(vti)

  val cnt = Mux(io.vcmdq.cnt.valid, io.vcmdq.cnt.bits.cnt, UInt(0))
  val regid_xbase = (cnt >> UInt(3)) * reg_xstride
  val regid_fbase = ((cnt >> UInt(3)) * reg_fstride) + reg_xf_split

  val vfu_val = viu_val || vmu_val
  val vd_zero = !vd_fp && vd === UInt(0) && vd_val
  val issue_op = !vd_zero && vfu_val
  val enq_aiw_cmdb = issue_op || decode_vf
  val enq_aiw_imm1b = enq_aiw_cmdb && deq_vcmdq_imm1
  val enq_aiw_imm2b = enq_aiw_cmdb && deq_vcmdq_imm2
  val enq_aiw_cntb = issue_op
  val enq_aiw_numCntB = issue_op || decode_vf


//-------------------------------------------------------------------------\\
// READY & VALID LOGIC                                                     \\
//-------------------------------------------------------------------------\\

  val mask_vxu_immq_valid = !deq_vcmdq_imm1 || io.vcmdq.imm1.valid
  val mask_vxu_imm2q_valid = !deq_vcmdq_imm2 || io.vcmdq.imm2.valid
  val mask_issue_ready = !issue_op || io.ready
  val mask_aiw_cmdb_ready = !enq_aiw_cmdb || io.aiw_cmdb.ready
  val mask_aiw_imm1b_ready = !enq_aiw_imm1b || io.aiw_imm1b.ready
  val mask_aiw_imm2b_ready = !enq_aiw_imm2b || io.aiw_imm2b.ready
  val mask_aiw_cntb_ready = !enq_aiw_cntb || io.aiw_cntb.ready
  val mask_aiw_numCntB_ready = !enq_aiw_numCntB || io.aiw_numCntB.ready

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Array(
      !stall, tvec_active,
      io.vcmdq.cmd.valid, mask_vxu_immq_valid, mask_vxu_imm2q_valid,
      mask_issue_ready,
      mask_aiw_cmdb_ready, mask_aiw_imm1b_ready, mask_aiw_imm2b_ready, mask_aiw_cntb_ready, mask_aiw_numCntB_ready)
    rvs.filter(_ != exclude).reduce(_&&_) && (Bool(true) :: include.toList).reduce(_&&_)
  }

  io.vcmdq.cmd.ready := fire(io.vcmdq.cmd.valid)
  io.vcmdq.imm1.ready := fire(mask_vxu_immq_valid, deq_vcmdq_imm1)
  io.vcmdq.imm2.ready := fire(mask_vxu_imm2q_valid, deq_vcmdq_imm2)
  io.vcmdq.cnt.ready := fire(null, deq_vcmdq_cnt)
  io.op.valid := fire(mask_issue_ready, issue_op)
  io.aiw_cmdb.valid := fire(mask_aiw_cmdb_ready, enq_aiw_cmdb)
  io.aiw_imm1b.valid := fire(mask_aiw_imm1b_ready, enq_aiw_imm1b)
  io.aiw_imm2b.valid := fire(mask_aiw_imm2b_ready, enq_aiw_imm2b)
  io.aiw_cntb.valid := fire(mask_aiw_cntb_ready, enq_aiw_cntb)
  io.aiw_numCntB.valid := fire(mask_aiw_numCntB_ready, enq_aiw_numCntB)
  io.issue_to_aiw.markLast := fire(null, issue_op)


//-------------------------------------------------------------------------\\
// REGISTERS                                                               \\
//-------------------------------------------------------------------------\\

  when (fire(null, decode_vcfg)) {
    reg_vlen := imm1.vlen
    reg_nxregs := imm1.nxregs
    reg_nfregs := imm1.nfregs
    reg_bactive := imm1.bactive
    reg_bcnt := imm1.bcnt
    reg_xstride := imm1.nxregs - UInt(1)
    reg_fstride := MuxLookup(
      imm1.prec, imm1.nfregs, Array(
        PREC_DOUBLE -> (imm1.nfregs),
        PREC_SINGLE -> ((imm1.nfregs + UInt(1)) >> UInt(1)),
        PREC_HALF -> ((imm1.nfregs + UInt(3)) >> UInt(2))
      ))
    reg_xf_split := imm1.xf_split // location of X/F register split in bank: number of xregs times the number of uts per bank
    reg_precision := imm1.prec
  }
  when (fire(null, decode_vsetvl)) {
    reg_vlen := imm1.vlen
  }
  when (fire(null, decode_vf)) {
    reg_state := ISSUE_VT
  }
  when (io.vf.stop) {
    reg_state := ISSUE_TVEC
  }


//-------------------------------------------------------------------------\\
// SIGNALS                                                                 \\
//-------------------------------------------------------------------------\\

  io.cfg.prec := reg_precision
  io.cfg.bactive := reg_bactive
  io.cfg.bcnt := reg_bcnt
  io.cfg.nxregs := reg_nxregs
  io.cfg.nfregs := reg_nfregs
  io.cfg.xstride := reg_xstride
  io.cfg.fstride := reg_fstride
  io.cfg.xfsplit := reg_xf_split

  io.active := tvec_active    
  io.vf.active := (reg_state === ISSUE_VT)
  io.vf.fire := fire(null, decode_vf)
  io.vf.pc := io.vcmdq.imm1.bits(31,0)
  io.vf.imm1_rtag := io.aiw_to_issue.imm1_rtag
  io.vf.numCnt_rtag := io.aiw_to_issue.numCnt_rtag
  io.vf.vlen := reg_vlen


//-------------------------------------------------------------------------\\
// DATAPATH                                                                \\
//-------------------------------------------------------------------------\\

  val vmu_op_vld = vmu_op === VM_VLD
  val vmu_op_vst = vmu_op === VM_VST

  io.op.bits.active.viu := viu_val
  io.op.bits.active.vau0 := Bool(false)
  io.op.bits.active.vau1 := Bool(false)
  io.op.bits.active.vau2 := Bool(false)
  io.op.bits.active.amo := Bool(false)
  io.op.bits.active.utld := Bool(false)
  io.op.bits.active.utst := Bool(false)
  io.op.bits.active.vld := vmu_val && vmu_op_vld
  io.op.bits.active.vst := vmu_val && vmu_op_vst

  io.op.bits.vlen := reg_vlen - cnt
  io.op.bits.utidx := cnt

  val vmu_float = vmu_op_vld && vd_fp || vmu_op_vst && vt_fp

  io.op.bits.fn.viu := new VIUFn().fromBits(Cat(M0, viu_t1, DW64, FPD, I_MOV))
  io.op.bits.fn.vmu := new VMUFn().fromBits(Cat(vmu_float, vmu_type, vmu_cmd, vmu_op))

  val vt_m1 = vt - UInt(1)
  val vd_m1 = vd - UInt(1)

  val addr_stride = MuxLookup(
    vmu_type, UInt(0,4), Array(
      MT_B->  UInt(1,4),
      MT_BU-> UInt(1,4),
      MT_H->  UInt(2,4),
      MT_HU-> UInt(2,4),
      MT_W->  UInt(4,4),
      MT_WU-> UInt(4,4),
      MT_D->  UInt(8,4)
    ))

  io.op.bits.reg.vs.active := Bool(false) // need to explicitly set these to false
  io.op.bits.reg.vt.active := vt_val
  io.op.bits.reg.vr.active := Bool(false) // since these affect hazard check results
  io.op.bits.reg.vd.active := vd_val
  io.op.bits.reg.vt.zero := !vt_fp && vt === UInt(0)
  io.op.bits.reg.vd.zero := !vd_fp && vd === UInt(0)
  io.op.bits.reg.vt.float := vt_fp
  io.op.bits.reg.vd.float := vd_fp
  io.op.bits.reg.vt.id := Mux(vt_fp, regid_fbase + vt, regid_xbase + vt_m1)
  io.op.bits.reg.vd.id := Mux(vd_fp, regid_fbase + vd, regid_xbase + vd_m1)

  io.op.bits.regcheck.vs.active := Bool(false) // need to explicitly set these to false
  io.op.bits.regcheck.vt.active := vt_val
  io.op.bits.regcheck.vr.active := Bool(false) // since these affect hazard check results
  io.op.bits.regcheck.vd.active := vd_val
  io.op.bits.regcheck.vt.base := vt
  io.op.bits.regcheck.vd.base := vd

  io.op.bits.imm.imm := io.vcmdq.imm1.bits
  io.op.bits.imm.stride := Mux(io.vcmdq.imm2.ready, imm2, Cat(Bits(0,60), addr_stride))

  io.op.bits.aiw.active.imm1 := vmu_val
  io.op.bits.aiw.active.cnt := Bool(true)
  io.op.bits.aiw.imm1.rtag := io.aiw_to_issue.imm1_rtag
  io.op.bits.aiw.cnt.rtag := io.aiw_to_issue.cnt_rtag
  io.op.bits.aiw.cnt.utidx := cnt
  io.op.bits.aiw.numcnt.rtag := io.aiw_to_issue.numCnt_rtag

  io.aiw_cmdb.bits := io.vcmdq.cmd.bits.toBits
  io.aiw_imm1b.bits := io.vcmdq.imm1.bits
  io.aiw_imm2b.bits := io.vcmdq.imm2.bits
  io.aiw_numCntB.bits := Mux(decode_vf, Bits(0), Bits(1))
  io.aiw_cntb.bits := cnt
}

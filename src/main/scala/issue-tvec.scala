package hwacha

import Chisel._
import Node._
import Constants._
import Commands._
import uncore.constants.MemoryOpConstants._

class io_issue_tvec_to_irq_handler extends Bundle
{
  val illegal = Bool(OUTPUT)
  val cmd = Bits(OUTPUT, SZ_VCMD)
}

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
                  //                                   | | deq_vxu_cnt
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

    val irq = new io_issue_tvec_to_irq_handler()

    val vf = new io_vf()
    val active = Bool(OUTPUT)

    val issue_to_hazard = new io_vxu_issue_to_hazard().asOutput
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
  val reg_xstride = Reg(init = Bits(63,SZ_REGLEN))
  val reg_fstride = Reg(init = Bits(63,SZ_REGLEN))
  val reg_xf_split = Reg(init = Bits(0,SZ_BANK))
  val reg_precision = Reg(init = PREC_DOUBLE)

  val stall_sticky = Reg(init=Bool(false))
  val stall = io.irq.illegal || stall_sticky || io.xcpt_to_issue.stall

  when (io.irq.illegal) { stall_sticky := Bool(true) }
  

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

  val valid = viu_val || vmu_val

  val decode_aiw_cmdb = valid || decode_vf
  val decode_aiw_imm1b = decode_aiw_cmdb && deq_vcmdq_imm1
  val decode_aiw_imm2b = decode_aiw_cmdb && deq_vcmdq_imm2
  val decode_aiw_cntb = valid
  val decode_aiw_numCntB = valid || decode_vf


//-------------------------------------------------------------------------\\
// FIRE & QUEUE LOGIC                                                      \\
//-------------------------------------------------------------------------\\

  val mask_issue_ready = !valid || io.ready
  val mask_vxu_immq_valid = !deq_vcmdq_imm1 || io.vcmdq.imm1.valid
  val mask_vxu_imm2q_valid = !deq_vcmdq_imm2 || io.vcmdq.imm2.valid
  val mask_aiw_cmdb_ready = !decode_aiw_cmdb || io.aiw_cmdb.ready
  val mask_aiw_imm1b_ready = !deq_vcmdq_imm1 || io.aiw_imm1b.ready
  val mask_aiw_imm2b_ready = !deq_vcmdq_imm2 || io.aiw_imm2b.ready
  val mask_aiw_cntb_ready = !decode_aiw_cntb || io.aiw_cntb.ready
  val mask_aiw_numCntB_ready = !decode_aiw_numCntB || io.aiw_numCntB.ready

  def construct_rv_blob(exclude: Bool, include: Bool*) = {
    val rvs = Array(
      io.vcmdq.cmd.valid, mask_vxu_immq_valid, mask_vxu_imm2q_valid,
      mask_aiw_cmdb_ready, mask_aiw_imm1b_ready, mask_aiw_imm2b_ready, mask_aiw_cntb_ready, mask_aiw_numCntB_ready)
    rvs.filter(_ != exclude).reduce(_&&_) && (Bool(true) :: include.toList).reduce(_&&_)
  }

  val valid_common = !stall && tvec_active && construct_rv_blob(null)

  val fire_common = valid_common && mask_issue_ready
  val fire_vcfg = fire_common && decode_vcfg
  val fire_vsetvl = fire_common && decode_vsetvl
  val fire_vf = fire_common && decode_vf

  val queue_common = !stall && tvec_active && mask_issue_ready
  io.vcmdq.cmd.ready := queue_common && construct_rv_blob(io.vcmdq.cmd.valid)
  io.vcmdq.imm1.ready := queue_common && construct_rv_blob(mask_vxu_immq_valid, deq_vcmdq_imm1)
  io.vcmdq.imm2.ready := queue_common && construct_rv_blob(mask_vxu_imm2q_valid, deq_vcmdq_imm2)
  io.vcmdq.cnt.ready := queue_common && construct_rv_blob(null, deq_vcmdq_cnt)
  io.aiw_cmdb.valid := queue_common && construct_rv_blob(mask_aiw_cmdb_ready, decode_aiw_cmdb)
  io.aiw_imm1b.valid := queue_common && construct_rv_blob(mask_aiw_imm1b_ready, decode_aiw_imm1b)
  io.aiw_imm2b.valid := queue_common && construct_rv_blob(mask_aiw_imm2b_ready, decode_aiw_imm2b)
  io.aiw_cntb.valid := queue_common && construct_rv_blob(mask_aiw_cntb_ready, decode_aiw_cntb)
  io.aiw_numCntB.valid := queue_common && construct_rv_blob(mask_aiw_numCntB_ready, decode_aiw_numCntB)
  io.issue_to_aiw.markLast := queue_common && valid && construct_rv_blob(null)

  val cnt = Mux(io.vcmdq.cnt.valid, io.vcmdq.cnt.bits, UInt(0))
  io.aiw_cmdb.bits := io.vcmdq.cmd.bits.toBits
  io.aiw_imm1b.bits := io.vcmdq.imm1.bits
  io.aiw_imm2b.bits := io.vcmdq.imm2.bits
  io.aiw_numCntB.bits := Mux(decode_vf, Bits(0), Bits(1))
  io.aiw_cntb.bits := cnt


//-------------------------------------------------------------------------\\
// REGISTERS                                                               \\
//-------------------------------------------------------------------------\\

  when (fire_vcfg) {
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
  when (fire_vsetvl) {
    reg_vlen := imm1.vlen
  }
  when (fire_vf) {
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
  io.vf.fire := fire_vf
  io.vf.pc := io.vcmdq.imm1.bits(31,0)
  io.vf.imm1_rtag := io.aiw_to_issue.imm1_rtag
  io.vf.numCnt_rtag := io.aiw_to_issue.numCnt_rtag
  io.vf.vlen := reg_vlen

  val vmu_op_vld = vmu_op === VM_VLD
  val vmu_op_vst = vmu_op === VM_VST
  val vmu_float = vmu_op_vld && vd_fp || vmu_op_vst && vt_fp
  val fcond = viu_val && vd_fp || vmu_val && vmu_float

  io.issue_to_hazard.stride := Mux(fcond, reg_fstride, reg_xstride)


//-------------------------------------------------------------------------\\
// ISSUE                                                                   \\
//-------------------------------------------------------------------------\\

  io.valid.viu := valid_common && viu_val
  io.valid.vau0 := Bool(false)
  io.valid.vau1 := Bool(false)
  io.valid.vau2 := Bool(false)
  io.valid.amo := Bool(false)
  io.valid.utld := Bool(false)
  io.valid.utst := Bool(false)
  io.valid.vld := valid_common && vmu_val && vmu_op_vld
  io.valid.vst := valid_common && vmu_val && vmu_op_vst

  io.dhazard.vs := Bool(false)
  io.dhazard.vt := vt_val
  io.dhazard.vr := Bool(false)
  io.dhazard.vd := vd_val

  io.shazard.viu := Bool(false)
  io.shazard.vau0 := Bool(false)
  io.shazard.vau1 := Bool(false)
  io.shazard.vau2 := Bool(false)
  io.shazard.vgu := vmu_val
  io.shazard.vlu := vmu_val && vmu_op_vld
  io.shazard.vsu := vmu_val && vmu_op_vst

  io.bhazard.r1w1 := viu_val
  io.bhazard.r2w1 := Bool(false)
  io.bhazard.r3w1 := Bool(false)
  io.bhazard.amo := Bool(false)
  io.bhazard.utld := Bool(false)
  io.bhazard.utst := Bool(false)
  io.bhazard.vld := vmu_val && vmu_op_vld
  io.bhazard.vst := vmu_val && vmu_op_vst

  io.fn.viu := new VIUFn().fromBits(Cat(M0, viu_t1, DW64, FPD, I_MOV))
  io.fn.vmu := new VMUFn().fromBits(Cat(vmu_float, vmu_type, vmu_cmd, vmu_op))

  val vt_m1 = vt(4,0) - UInt(1)
  val vd_m1 = vd(4,0) - UInt(1)

  val regid_xbase = (cnt >> UInt(3)) * reg_xstride
  val regid_fbase = ((cnt >> UInt(3)) * reg_fstride) + reg_xf_split

  val addr_stride = MuxLookup(
    vmu_type, UInt(0,4), Array(
      MT_B -> UInt(1,4),
      MT_BU -> UInt(1,4),
      MT_H -> UInt(2,4),
      MT_HU -> UInt(2,4),
      MT_W -> UInt(4,4),
      MT_WU -> UInt(4,4),
      MT_D -> UInt(8,4)
    ))

  io.decoded.vlen := reg_vlen - cnt
  io.decoded.utidx := Bits(0)
  io.decoded.vd_base := Mux(vd_fp, regid_fbase + vd(4,0), regid_xbase + vd_m1)
  io.decoded.vs := Bits(0)
  io.decoded.vt := Mux(vt_fp, regid_fbase + vt(4,0), regid_xbase + vt_m1)
  io.decoded.vr := Bits(0)
  io.decoded.vd := Mux(vd_fp, regid_fbase + vd(4,0), regid_xbase + vd_m1)
  io.decoded.vs_zero := Bool(true)
  io.decoded.vt_zero := !vt_fp && vt === Bits(0)
  io.decoded.vr_zero := Bool(true)
  io.decoded.vd_zero := !vd_fp && vd === Bits(0) && vd_val
  io.decoded.vs_active := Bool(false)
  io.decoded.vt_active := vt_val
  io.decoded.vr_active := Bool(false)
  io.decoded.vd_active := vd_val
  io.decoded.rtype := Cat(Bool(false), vt_fp, Bool(false), vd_fp) // vs vt vr vd
  io.decoded.imm := io.vcmdq.imm1.bits
  io.decoded.imm2 := Mux(io.vcmdq.imm2.ready, imm2, Cat(Bits(0,60), addr_stride))
  io.decoded.cnt_valid := io.vcmdq.cnt.valid
  io.decoded.cnt := cnt
  io.decoded.aiw.imm1_rtag := io.aiw_to_issue.imm1_rtag
  io.decoded.aiw.numCnt_rtag := io.aiw_to_issue.numCnt_rtag
  io.decoded.aiw.cnt_rtag := io.aiw_to_issue.cnt_rtag
  io.decoded.aiw.update_imm1 := !io.valid.viu

  val illegal_vd = vd_val && (vd(4,0) >= reg_nfregs && vd_fp || vd(4,0) >= reg_nxregs && !vd_fp)
  val illegal_vt = vt_val && (vt(4,0) >= reg_nfregs && vt_fp || vt(4,0) >= reg_nxregs && !vt_fp)
  
  io.irq.illegal := 
    io.vcmdq.cmd.valid && tvec_active && 
    (!valid.orR && !decode_vcfg && !decode_vsetvl && !decode_vf || illegal_vd || illegal_vt)
  io.irq.cmd := io.vcmdq.cmd.bits.toBits
}

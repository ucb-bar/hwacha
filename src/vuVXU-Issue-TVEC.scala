package hwacha

import Chisel._
import Node._
import Constants._
import Commands._

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

class io_vxu_issue_tvec extends Bundle
{
  val irq = new io_issue_tvec_to_irq_handler()

  val vf = new io_vf()
  val active = Bool(OUTPUT)

  val issue_to_hazard = new io_vxu_issue_to_hazard().asOutput
  val issue_to_seq = new io_vxu_issue_to_seq().asOutput
  val issue_to_lane = new io_vxu_issue_to_lane().asOutput
  val hazard_to_issue = new io_vxu_hazard_to_issue_tvec().asInput

  val vxu_cmdq = new io_vxu_cmdq().flip()
  val vxu_immq = new io_vxu_immq().flip()
  val vxu_imm2q = new io_vxu_imm2q().flip()
  val vxu_cntq = new io_vxu_cntq().flip()
  
  val valid = new io_vxu_issue_fire().asOutput
  val ready = Bool(INPUT)
  val dhazard = new io_vxu_issue_reg().asOutput
  val shazard = new io_vxu_issue_fu().asOutput
  val bhazard = new io_vxu_issue_op().asOutput
  val fn = new io_vxu_issue_fn().asOutput
  val decoded = new io_vxu_issue_regid_imm().asOutput

  val pending_store = Bool(INPUT)

  val aiw_cmdb = new io_vxu_cmdq()
  val aiw_imm1b = new io_vxu_immq()
  val aiw_imm2b = new io_vxu_imm2q()
  val aiw_cntb = new io_vxu_cntq()
  val aiw_numCntB = new io_vxu_numcntq()
  val aiw_to_issue = new io_aiw_to_issue().flip
  val issue_to_aiw = new io_issue_to_aiw()

  val flush = Bool(INPUT)
  val xcpt_to_issue = new io_xcpt_handler_to_issue().flip()
}

class vuVXU_Issue_TVEC extends Component
{
  val io = new io_vxu_issue_tvec()

  val ISSUE_TVEC = Bits(0,1)
  val ISSUE_VT = Bits(1,1)

  val next_state = Bits(width = 1)
  val reg_state = Reg(next_state, resetVal = ISSUE_TVEC)

  val tvec_active = (reg_state === ISSUE_TVEC)
  io.active := tvec_active    


//-------------------------------------------------------------------------\\
// DECODE                                                                  \\
//-------------------------------------------------------------------------\\

  val cmd = io.vxu_cmdq.bits(RG_XCMD_CMCODE)
  val vd = io.vxu_cmdq.bits(RG_XCMD_VD)
  val vt = io.vxu_cmdq.bits(RG_XCMD_VT)
  val imm = io.vxu_immq.bits
  val imm2 = io.vxu_imm2q.bits

  val n = Bool(false)
  val y = Bool(true)

  val stall_sticky = Reg(resetVal = Bool(false))
  val stall = io.irq.illegal || stall_sticky || io.xcpt_to_issue.stall

  when (io.irq.illegal) { stall_sticky := Bool(true) }
  when (io.flush) { stall_sticky := Bool(false) }
  

  val cs =
  ListLookup(cmd,
                     //                                                                 decode_fence_v
                     //                                                                 | vd_valid
                     //                                                                 | | vd_active
                     //                                                                 | | | vt_active
                     //                                                                 | | | | decode_vcfg
                     //                                                                 | | | | | decode_setvl
                     //                                                                 | | | | | | decode_vf
                     //                                                                 | | | | | | | deq_vxu_immq
                     //         val            dhazard       shazard        bhazard msrc| | | | | | | | deq_vxu_imm2q  
                     //         |              |             |              |        |  | | | | | | | | | deq_vxu_cnt  
                     //         |              |             |              |        |  | | | | | | | | | | stride  mem_type_float
                     //         |              |             |              |        |  | | | | | | | | | | |         |   mem_type mem_cmd
                     //         |              |             |              |        |  | | | | | | | | | | |         |     |      |
                     List(Bits("b000",3),Bits("b00",2),Bits("b000",3),Bits("b000",3),M0,n,n,n,n,n,n,n,n,n,n,Bits(0,4),MTF_X,mtyp_X,mcmd_X),Array(
    CMD_VVCFGIVL->   List(Bits("b000",3),Bits("b00",2),Bits("b000",3),Bits("b000",3),M0,n,n,n,n,y,y,n,y,n,n,Bits(0,4),MTF_X,mtyp_X,mcmd_X),
    CMD_VSETVL->     List(Bits("b000",3),Bits("b00",2),Bits("b000",3),Bits("b000",3),M0,n,n,n,n,n,y,n,y,n,n,Bits(0,4),MTF_X,mtyp_X,mcmd_X),
    CMD_VF->         List(Bits("b000",3),Bits("b00",2),Bits("b000",3),Bits("b000",3),M0,n,n,n,n,n,n,y,y,n,n,Bits(0,4),MTF_X,mtyp_X,mcmd_X),

    CMD_FENCE_L_V->  List(Bits("b000",3),Bits("b00",2),Bits("b000",3),Bits("b000",3),M0,y,n,n,n,n,n,n,n,n,n,Bits(0,4),MTF_X,mtyp_X,mcmd_X),
    CMD_FENCE_G_V->  List(Bits("b000",3),Bits("b00",2),Bits("b000",3),Bits("b000",3),M0,y,n,n,n,n,n,n,n,n,n,Bits(0,4),MTF_X,mtyp_X,mcmd_X),

    CMD_VMVV->       List(Bits("b001",3),Bits("b11",2),Bits("b000",3),Bits("b001",3),MR,n,y,y,y,n,n,n,n,n,y,Bits(0,4),MTF_X,mtyp_X,mcmd_X),
    CMD_VMSV->       List(Bits("b001",3),Bits("b10",2),Bits("b000",3),Bits("b001",3),MI,n,y,y,n,n,n,n,y,n,y,Bits(0,4),MTF_X,mtyp_X,mcmd_X),
    CMD_VFMVV->      List(Bits("b001",3),Bits("b11",2),Bits("b000",3),Bits("b001",3),MR,n,y,y,y,n,n,n,n,n,y,Bits(0,4),MTF_X,mtyp_X,mcmd_X),

    CMD_VLD       -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,n,y,Bits(8,4),MTF_X,mtyp_D,mcmd_XRD),
    CMD_VLW       -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,n,y,Bits(4,4),MTF_X,mtyp_W,mcmd_XRD),
    CMD_VLWU      -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,n,y,Bits(4,4),MTF_X,mtyp_WU,mcmd_XRD),
    CMD_VLH       -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,n,y,Bits(2,4),MTF_X,mtyp_H,mcmd_XRD),
    CMD_VLHU      -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,n,y,Bits(2,4),MTF_X,mtyp_HU,mcmd_XRD),
    CMD_VLB       -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,n,y,Bits(1,4),MTF_X,mtyp_B,mcmd_XRD),
    CMD_VLBU      -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,n,y,Bits(1,4),MTF_X,mtyp_BU,mcmd_XRD),
    CMD_VSD       -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,n,y,n,n,n,y,n,y,Bits(8,4),MTF_X,mtyp_D,mcmd_XWR),
    CMD_VSW       -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,n,y,n,n,n,y,n,y,Bits(4,4),MTF_X,mtyp_W,mcmd_XWR),
    CMD_VSH       -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,n,y,n,n,n,y,n,y,Bits(2,4),MTF_X,mtyp_H,mcmd_XWR),
    CMD_VSB       -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,n,y,n,n,n,y,n,y,Bits(1,4),MTF_X,mtyp_B,mcmd_XWR),

    CMD_VFLD      -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,n,y,Bits(8,4),MTF_Y,mtyp_D,mcmd_XRD),
    CMD_VFLW      -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,n,y,Bits(4,4),MTF_Y,mtyp_W,mcmd_XRD),
    CMD_VFSD      -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,n,y,n,n,n,y,n,y,Bits(8,4),MTF_Y,mtyp_D,mcmd_XWR),
    CMD_VFSW      -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,n,y,n,n,n,y,n,y,Bits(4,4),MTF_Y,mtyp_W,mcmd_XWR),

    CMD_VLSTD     -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,mtyp_D,mcmd_XRD),
    CMD_VLSTW     -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,mtyp_W,mcmd_XRD),
    CMD_VLSTWU    -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,mtyp_WU,mcmd_XRD),
    CMD_VLSTH     -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,mtyp_H,mcmd_XRD),
    CMD_VLSTHU    -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,mtyp_HU,mcmd_XRD),
    CMD_VLSTB     -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,mtyp_B,mcmd_XRD),
    CMD_VLSTBU    -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_X,mtyp_BU,mcmd_XRD),
    CMD_VSSTD     -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,n,y,n,n,n,y,y,y,Bits(0,4),MTF_X,mtyp_D,mcmd_XWR),
    CMD_VSSTW     -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,n,y,n,n,n,y,y,y,Bits(0,4),MTF_X,mtyp_W,mcmd_XWR),
    CMD_VSSTH     -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,n,y,n,n,n,y,y,y,Bits(0,4),MTF_X,mtyp_H,mcmd_XWR),
    CMD_VSSTB     -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,n,y,n,n,n,y,y,y,Bits(0,4),MTF_X,mtyp_B,mcmd_XWR),

    CMD_VFLSTD    -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_Y,mtyp_D,mcmd_XRD),
    CMD_VFLSTW    -> List(Bits("b010",3),Bits("b10",2),Bits("b110",3),Bits("b010",3),M0,n,y,y,n,n,n,n,y,y,y,Bits(0,4),MTF_Y,mtyp_W,mcmd_XRD),
    CMD_VFSSTD    -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,n,y,n,n,n,y,y,y,Bits(0,4),MTF_Y,mtyp_D,mcmd_XWR),
    CMD_VFSSTW    -> List(Bits("b100",3),Bits("b01",2),Bits("b101",3),Bits("b100",3),M0,n,n,n,y,n,n,n,y,y,y,Bits(0,4),MTF_Y,mtyp_W,mcmd_XWR)
  ))

  val valid::dhazard::shazard::bhazard::vmsrc::decode_fence_v::cs0 = cs
  val vd_valid::vd_active::vt_active::decode_vcfg::decode_setvl::decode_vf::deq_vxu_immq::deq_vxu_imm2q::deq_vxu_cntq::cs1 = cs0
  val addr_stride::mem_type_float::mem_type::mem_cmd::Nil = cs1

  val decode_aiw_cmdb_valid = valid.orR || decode_vf
  val decode_aiw_imm1b_valid = decode_aiw_cmdb_valid && deq_vxu_immq
  val decode_aiw_imm2b_valid = decode_aiw_cmdb_valid && deq_vxu_imm2q
  val decode_aiw_cntb_valid = valid.orR
  val decode_aiw_numCntB_valid = valid.orR || decode_vf

  val tvec_active_fence_clear =
    tvec_active &&
    (!decode_fence_v || !io.hazard_to_issue.pending_memop && !io.pending_store)

//-------------------------------------------------------------------------\\
// FIRE & QUEUE LOGIC                                                      \\
//-------------------------------------------------------------------------\\

  val mask_issue_ready = !valid.orR || io.ready
  val mask_vxu_immq_valid = !deq_vxu_immq || io.vxu_immq.valid
  val mask_vxu_imm2q_valid = !deq_vxu_imm2q || io.vxu_imm2q.valid
  val mask_aiw_cmdb_ready = !decode_aiw_cmdb_valid || io.aiw_cmdb.ready
  val mask_aiw_imm1b_ready = !deq_vxu_immq || io.aiw_imm1b.ready
  val mask_aiw_imm2b_ready = !deq_vxu_imm2q || io.aiw_imm2b.ready
  val mask_aiw_cntb_ready = !decode_aiw_cntb_valid || io.aiw_cntb.ready
  val mask_aiw_numCntB_ready = !decode_aiw_numCntB_valid || io.aiw_numCntB.ready

  val valid_common =
    !stall &&
    tvec_active_fence_clear &&
    io.vxu_cmdq.valid && mask_vxu_immq_valid && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  val fire_common = mask_issue_ready && valid_common

  val fire_vcfg = fire_common && decode_vcfg
  val fire_setvl = fire_common && decode_setvl
  val fire_vf = fire_common && decode_vf

  val queue_common = tvec_active_fence_clear && mask_issue_ready && !stall 

  io.vxu_cmdq.ready := 
    queue_common && 
    Bool(true) && mask_vxu_immq_valid && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.vxu_immq.ready := 
    queue_common &&
    io.vxu_cmdq.valid && deq_vxu_immq && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.vxu_imm2q.ready := 
    queue_common &&
    io.vxu_cmdq.valid && mask_vxu_immq_valid && deq_vxu_imm2q &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.vxu_cntq.ready :=
    queue_common &&
    io.vxu_cmdq.valid && mask_vxu_immq_valid && mask_vxu_imm2q_valid && deq_vxu_cntq &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.aiw_cmdb.valid := 
    queue_common &&
    io.vxu_cmdq.valid && mask_vxu_immq_valid && mask_vxu_imm2q_valid &&
    decode_aiw_cmdb_valid && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.aiw_imm1b.valid :=
    queue_common &&
    io.vxu_cmdq.valid && decode_aiw_imm1b_valid && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && deq_vxu_immq && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.aiw_imm2b.valid :=
    queue_common &&
    io.vxu_cmdq.valid && mask_vxu_immq_valid && decode_aiw_imm2b_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && deq_vxu_imm2q && mask_aiw_cntb_ready && mask_aiw_numCntB_ready

  io.aiw_cntb.valid :=
    queue_common &&
    io.vxu_cmdq.valid && mask_vxu_immq_valid && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && decode_aiw_cntb_valid && mask_aiw_numCntB_ready
  
  io.aiw_numCntB.valid := 
    queue_common &&
    io.vxu_cmdq.valid && mask_vxu_immq_valid && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && decode_aiw_numCntB_valid

  io.issue_to_aiw.markLast := 
    queue_common && valid.orR &&
    io.vxu_cmdq.valid && mask_vxu_immq_valid && mask_vxu_imm2q_valid &&
    mask_aiw_cmdb_ready && mask_aiw_imm1b_ready && mask_aiw_imm2b_ready && mask_aiw_cntb_ready && mask_aiw_numCntB_ready
    

  io.aiw_cmdb.bits := io.vxu_cmdq.bits
  io.aiw_imm1b.bits := io.vxu_immq.bits
  io.aiw_imm2b.bits := io.vxu_imm2q.bits
  io.aiw_numCntB.bits := Mux(decode_vf, Bits(0, 1), Bits(1, 1))


//-------------------------------------------------------------------------\\
// REGISTERS                                                               \\
//-------------------------------------------------------------------------\\

  val next_vlen = Bits(width = SZ_VLEN)
  val next_nxregs = Bits(width = SZ_REGCNT)
  val next_nfregs = Bits(width = SZ_REGCNT)
  val next_bactive = Bits(width = SZ_BANK)
  val next_bcnt = Bits(width = SZ_BCNT)
  val next_stride = Bits(width = SZ_REGLEN)

  val reg_vlen = Reg(next_vlen, resetVal = Bits(0,SZ_VLEN))
  val reg_nxregs = Reg(next_nxregs, resetVal = Bits(32,SZ_REGCNT))
  val reg_nfregs = Reg(next_nfregs, resetVal = Bits(32,SZ_REGCNT))
  val reg_bactive = Reg(next_bactive, resetVal = Bits("b1111_1111",SZ_BANK))
  val reg_bcnt = Reg(next_bcnt, resetVal = Bits(8,SZ_LGBANK1))
  val reg_stride = Reg(next_stride, resetVal = Bits(63,SZ_REGLEN))

  val cnt = Mux(io.vxu_cntq.valid, io.vxu_cntq.bits, Bits(0))
  val regid_base = (cnt >> UFix(3)) * reg_stride

  io.aiw_cntb.bits := cnt

  next_state := reg_state
  next_vlen := reg_vlen
  next_nxregs := reg_nxregs
  next_nfregs := reg_nfregs
  next_bactive := reg_bactive
  next_bcnt := reg_bcnt
  next_stride := reg_stride

  when (fire_vcfg)
  {
    next_vlen := io.vxu_immq.bits(RG_XIMM1_VLEN)
    next_nxregs := io.vxu_immq.bits(RG_XIMM1_NXREGS)
    next_nfregs := io.vxu_immq.bits(RG_XIMM1_NFREGS)
    next_bactive := io.vxu_immq.bits(RG_XIMM1_BACTIVE)
    next_bcnt := io.vxu_immq.bits(RG_XIMM1_BCNT)
    next_stride := next_nxregs + next_nfregs - Bits(1,2)
  }
  when (fire_setvl)
  {
    next_vlen := io.vxu_immq.bits(RG_XIMM1_VLEN)
  }
  when (fire_vf)
  {
    next_state := ISSUE_VT
  }
  when (io.vf.stop)
  {
    next_state := ISSUE_TVEC
  }

  when (io.flush) 
  {
    next_state := ISSUE_TVEC
    next_vlen := Bits(0,SZ_VLEN)
    next_nxregs := Bits(32,SZ_REGCNT)
    next_nfregs := Bits(32,SZ_REGCNT)
    next_bactive := Bits("b1111_1111",SZ_BANK)
    next_bcnt := Bits(8,SZ_LGBANK1)
    next_stride := Bits(63,SZ_REGLEN)
  }



//-------------------------------------------------------------------------\\
// SIGNALS                                                                 \\
//-------------------------------------------------------------------------\\

  io.vf.active := (reg_state === ISSUE_VT)
  io.vf.fire := fire_vf
  io.vf.pc := io.vxu_immq.bits(31,0).toUFix
  io.vf.nxregs := reg_nxregs
  io.vf.nfregs := reg_nfregs
  io.vf.imm1_rtag := io.aiw_to_issue.imm1_rtag
  io.vf.numCnt_rtag := io.aiw_to_issue.numCnt_rtag
  io.vf.stride := reg_stride
  io.vf.vlen := reg_vlen

  io.issue_to_hazard.bcnt := reg_bcnt
  io.issue_to_hazard.stride := reg_stride
  io.issue_to_seq.vlen := reg_vlen - cnt
  io.issue_to_seq.stride := reg_stride
  io.issue_to_seq.bcnt := reg_bcnt
  io.issue_to_lane.bactive := reg_bactive


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

  io.bhazard.r2wm := Bool(false)
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

  val vt_m1 = Cat(Bits(0,1),vt(4,0)) - UFix(1,1)
  val vd_m1 = Cat(Bits(0,1),vd(4,0)) - UFix(1,1)
  val rtype_vd = vd(5)
  val rtype_vt = vt(5)

  io.decoded.vlen := reg_vlen - cnt
  io.decoded.utidx := Bits(0)
  io.decoded.vd_base := Mux(rtype_vd, vd_m1 + reg_nxregs, vd_m1)
  io.decoded.vs := Bits(0,SZ_BREGLEN)
  io.decoded.vt := Mux(rtype_vt, vt_m1 + reg_nxregs, vt_m1) + regid_base
  io.decoded.vr := Bits(0,SZ_BREGLEN)
  io.decoded.vd := Mux(rtype_vd, vd_m1 + reg_nxregs, vd_m1) + regid_base
  io.decoded.vm := Bits(0, SZ_BMASK)  
  io.decoded.vs_zero := Bool(true)
  io.decoded.vt_zero := vt === Bits(0,6)
  io.decoded.vr_zero := Bool(true)
  io.decoded.vd_zero := vd === Bits(0,6) && vd_valid
  io.decoded.vs_active := Bool(false)
  io.decoded.vt_active := vt_active
  io.decoded.vr_active := Bool(false)
  io.decoded.vd_active := vd_active
  io.decoded.mem.cmd := mem_cmd
  io.decoded.mem.typ := mem_type
  io.decoded.mem.typ_float := mem_type_float
  io.decoded.imm := imm
  io.decoded.imm2 := Mux(io.vxu_imm2q.ready, imm2, Cat(Bits(0,60), addr_stride))
  io.decoded.cnt_valid := io.vxu_cntq.valid
  io.decoded.cnt := cnt
  io.decoded.aiw.imm1_rtag := io.aiw_to_issue.imm1_rtag
  io.decoded.aiw.numCnt_rtag := io.aiw_to_issue.numCnt_rtag
  io.decoded.aiw.cnt_rtag := io.aiw_to_issue.cnt_rtag
  io.decoded.aiw.update_imm1 := !io.valid.viu
  io.decoded.active_mask := Bool(false)
  io.decoded.mask := Fill(WIDTH_PVFB, Bits(1,1))
  io.decoded.pvfb_tag := Bits(0, SZ_PVFB_TAG)

  val illegal_vd = vd_active && (vd(4,0) >= reg_nfregs && rtype_vd || vd(4,0) >= reg_nxregs && !rtype_vd)
  val illegal_vt = vt_active && (vt(4,0) >= reg_nfregs && rtype_vt || vt(4,0) >= reg_nxregs && !rtype_vt)
  
  io.irq.illegal := 
    io.vxu_cmdq.valid && tvec_active && 
    (!valid.orR && !decode_fence_v && !decode_vcfg && !decode_setvl && !decode_vf || illegal_vd || illegal_vt)
  io.irq.cmd := io.vxu_cmdq.bits
}

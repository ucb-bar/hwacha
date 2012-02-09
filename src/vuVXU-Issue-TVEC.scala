package riscvVector

import Chisel._
import Node._
import Config._
import Commands._
import Interface._

class vuVXU_Issue_TVEC extends Component
{
  val io = new io_vxu_issue_tvec();

  val ISSUE_TVEC = Bits(0,1);
  val ISSUE_VT = Bits(1,1);

  val next_state = Wire(){Bits(width = 1)};
  val reg_state = Reg(next_state, resetVal = ISSUE_TVEC);

  val tvec_active = (reg_state === ISSUE_TVEC);


//-------------------------------------------------------------------------\\
// DECODE                                                                  \\
//-------------------------------------------------------------------------\\

  val cmd = io.vxu_cmdq.bits(XCMD_CMCODE);
  val vd = io.vxu_cmdq.bits(XCMD_VD);
  val vt = io.vxu_cmdq.bits(XCMD_VS);
  val imm = io.vxu_immq.bits;
  val imm2 = io.vxu_imm2q.bits;

  val n = Bits(0,1);
  val y = Bits(1,1);

  val cs =
  ListLookup(cmd,
                     //                                                                vd_valid
                     //                                                                | decode_vcfg
                     //                                                                | | decode_setvl
                     //                                                                | | | decode_vf
                     //                                                                | | | | deq_vxu_immq
                     //         val            dhazard       shazard       bhazard msrc| | | | | enq_vmu_utcmdq
                     //         |              |             |             |        |  | | | | | |
                     List(Bits("b000",3),Bits("b00",2),Bits("b00",2),Bits("b000",3),M0,n,n,n,n,n,n),Array(
    CMD_VVCFGIVL->   List(Bits("b000",3),Bits("b00",2),Bits("b00",2),Bits("b000",3),M0,n,y,y,n,y,n),
    CMD_VSETVL->     List(Bits("b000",3),Bits("b00",2),Bits("b00",2),Bits("b000",3),M0,n,n,y,n,y,n),
    CMD_VF->         List(Bits("b000",3),Bits("b00",2),Bits("b00",2),Bits("b000",3),M0,n,n,n,y,y,n),

    CMD_FENCE_L_V->  List(Bits("b000",3),Bits("b00",2),Bits("b00",2),Bits("b000",3),M0,n,n,n,n,n,y),
    CMD_FENCE_G_V->  List(Bits("b000",3),Bits("b00",2),Bits("b00",2),Bits("b000",3),M0,n,n,n,n,n,y),
    CMD_FENCE_L_CV-> List(Bits("b000",3),Bits("b00",2),Bits("b00",2),Bits("b000",3),M0,n,n,n,n,n,y),
    CMD_FENCE_G_CV-> List(Bits("b000",3),Bits("b00",2),Bits("b00",2),Bits("b000",3),M0,n,n,n,n,n,y),

    CMD_VMVV->       List(Bits("b001",3),Bits("b11",2),Bits("b00",2),Bits("b001",3),MR,y,n,n,n,n,n),
    CMD_VMSV->       List(Bits("b001",3),Bits("b10",2),Bits("b00",2),Bits("b001",3),MI,y,n,n,n,y,n),
    CMD_VFMVV->      List(Bits("b001",3),Bits("b11",2),Bits("b00",2),Bits("b001",3),MR,y,n,n,n,n,n),

    CMD_VLD       -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VLW       -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VLWU      -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VLH       -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VLHU      -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VLB       -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VLBU      -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VSD       -> List(Bits("b100",3),Bits("b01",2),Bits("b10",2),Bits("b100",3),M0,n,n,n,n,n,n),
    CMD_VSW       -> List(Bits("b100",3),Bits("b01",2),Bits("b10",2),Bits("b100",3),M0,n,n,n,n,n,n),
    CMD_VSH       -> List(Bits("b100",3),Bits("b01",2),Bits("b10",2),Bits("b100",3),M0,n,n,n,n,n,n),
    CMD_VSB       -> List(Bits("b100",3),Bits("b01",2),Bits("b10",2),Bits("b100",3),M0,n,n,n,n,n,n),

    CMD_VFLD      -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VFLW      -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VFSD      -> List(Bits("b100",3),Bits("b01",2),Bits("b10",2),Bits("b100",3),M0,n,n,n,n,n,n),
    CMD_VFSW      -> List(Bits("b100",3),Bits("b01",2),Bits("b10",2),Bits("b100",3),M0,n,n,n,n,n,n),

    CMD_VLSTD     -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VLSTW     -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VLSTWU    -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VLSTH     -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VLSTHU    -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VLSTB     -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VLSTBU    -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VSSTD     -> List(Bits("b100",3),Bits("b01",2),Bits("b10",2),Bits("b100",3),M0,n,n,n,n,n,n),
    CMD_VSSTW     -> List(Bits("b100",3),Bits("b01",2),Bits("b10",2),Bits("b100",3),M0,n,n,n,n,n,n),
    CMD_VSSTH     -> List(Bits("b100",3),Bits("b01",2),Bits("b10",2),Bits("b100",3),M0,n,n,n,n,n,n),
    CMD_VSSTB     -> List(Bits("b100",3),Bits("b01",2),Bits("b10",2),Bits("b100",3),M0,n,n,n,n,n,n),

    CMD_VFLSTD    -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VFLSTW    -> List(Bits("b010",3),Bits("b10",2),Bits("b01",2),Bits("b010",3),M0,y,n,n,n,n,n),
    CMD_VFSSTD    -> List(Bits("b100",3),Bits("b01",2),Bits("b10",2),Bits("b100",3),M0,n,n,n,n,n,n),
    CMD_VFSSTW    -> List(Bits("b100",3),Bits("b01",2),Bits("b10",2),Bits("b100",3),M0,n,n,n,n,n,n)
  ));

  val valid::dhazard::shazard::bhazard::vmsrc::cs0 = cs;
  val vd_valid::decode_vcfg::decode_setvl::decode_vf::deq_vxu_immq::enq_vmu_utcmdq::Nil = cs0;

  val fire_vcfg  = tvec_active & io.vxu_cmdq.valid & decode_vcfg;
  val fire_setvl = tvec_active & io.vxu_cmdq.valid & decode_setvl;
  val fire_vf    = tvec_active & io.vxu_cmdq.valid & decode_vf;


//-------------------------------------------------------------------------\\
// REGISTERS                                                               \\
//-------------------------------------------------------------------------\\

  val next_vlen = Wire(){Bits(width = DEF_VLEN)};
  val next_nxregs = Wire(){Bits(width = DEF_REGCNT)};
  val next_nfregs = Wire(){Bits(width = DEF_REGCNT)};
  val next_bactive = Wire(){Bits(width = DEF_BANK)};
  val next_bcnt = Wire(){Bits(width = DEF_BCNT)};
  val next_stride = Wire(){Bits(width = DEF_REGLEN)};

  val reg_vlen = Reg(next_vlen, resetVal = Bits(0,SZ_VLEN));
  val reg_nxregs = Reg(next_nxregs, resetVal = Bits(32,SZ_REGCNT));
  val reg_nfregs = Reg(next_nfregs, resetVal = Bits(32,SZ_REGCNT));
  val reg_bactive = Reg(next_bactive, resetVal = Bits("b1111_1111",SZ_BANK));
  val reg_bcnt = Reg(next_bcnt, resetVal = Bits(8,SZ_LGBANK1));
  val reg_stride = Reg(next_stride, resetVal = Bits(63,SZ_REGLEN));

  next_state <== reg_state;
  next_vlen <== reg_vlen;
  next_nxregs <== reg_nxregs;
  next_nfregs <== reg_nfregs;
  next_bactive <== reg_bactive;
  next_bcnt <== reg_bcnt;
  next_stride <== reg_stride;

  when (fire_vcfg.toBool)
  {
    next_vlen <== io.vxu_immq.bits(10,0);
    next_nxregs <== io.vxu_immq.bits(16,11);
    next_nfregs <== io.vxu_immq.bits(22,17);
    next_bactive <== io.vxu_immq.bits(30,23);
    next_bcnt <== io.vxu_immq.bits(34,31);
    next_stride <== next_nxregs.toUFix + next_nfregs.toUFix - Bits(1,2).toUFix;
  }
  when (fire_setvl.toBool)
  {
    next_vlen <== io.vxu_immq.bits(10,0);
  }
  when (fire_vf.toBool)
  {
    next_state <== ISSUE_VT;
  }
  when (io.vf.stop)
  {
    next_state <== ISSUE_TVEC;
  }


//-------------------------------------------------------------------------\\
// SIGNALS                                                                 \\
//-------------------------------------------------------------------------\\

  io.vf.active := (reg_state === ISSUE_VT);
  io.vf.fire := fire_vf.toBool;
  io.vf.pc := io.vxu_immq.bits(31,0);
  io.vf.nxregs := reg_nxregs;
  io.vf.vlen := reg_vlen;

  io.issue_to_hazard.bcnt := reg_bcnt;
  io.issue_to_seq.vlen := reg_vlen;
  io.issue_to_seq.stride := reg_stride;
  io.issue_to_seq.bcnt := reg_bcnt;
  io.issue_to_lane.bactive := reg_bactive;

  val mask_vxu_immq_valid = ~deq_vxu_immq | io.vxu_immq.valid;
  val mask_issue_ready = ~valid.orR | io.ready;
  val mask_vmu_utcmdq_ready = ~enq_vmu_utcmdq | io.vmu_utcmdq.ready;

  io.vxu_cmdq.ready := (tvec_active & Bool(true) & mask_vxu_immq_valid & mask_issue_ready & mask_vmu_utcmdq_ready).toBool;
  io.vxu_immq.ready := (tvec_active & io.vxu_cmdq.valid & deq_vxu_immq & mask_issue_ready & mask_vmu_utcmdq_ready).toBool;

  io.vmu_utcmdq.bits := Cat(cmd,Bits(0,UTMCMD_VLEN_SZ));
  io.vmu_utcmdq.valid := (tvec_active & io.vxu_cmdq.valid & mask_vxu_immq_valid & mask_issue_ready & enq_vmu_utcmdq).toBool;

  io.valid.viu := (valid(0) & tvec_active & io.vxu_cmdq.valid & mask_vxu_immq_valid & mask_vmu_utcmdq_ready).toBool;
  io.valid.vau0 := Bool(false);
  io.valid.vau1 := Bool(false);
  io.valid.vau2 := Bool(false);
  io.valid.vgslu := Bool(false);
  io.valid.vglu := Bool(false);
  io.valid.vgsu := Bool(false);
  io.valid.vgu := Bool(false);
  io.valid.vlu := (valid(1) & tvec_active & io.vxu_cmdq.valid & mask_vxu_immq_valid & mask_vmu_utcmdq_ready).toBool;
  io.valid.vsu := (valid(2) & tvec_active & io.vxu_cmdq.valid & mask_vxu_immq_valid & mask_vmu_utcmdq_ready).toBool;

  io.dhazard.vs := Bool(false);
  io.dhazard.vt := dhazard(0).toBool;
  io.dhazard.vr := Bool(false);
  io.dhazard.vd := dhazard(1).toBool;

  io.shazard.viu := Bool(false);
  io.shazard.vau0 := Bool(false);
  io.shazard.vau1 := Bool(false);
  io.shazard.vau2 := Bool(false);
  io.shazard.vgslu := Bool(false);
  io.shazard.vglu := Bool(false);
  io.shazard.vgsu := Bool(false);
  io.shazard.vgu := Bool(false);
  io.shazard.vlu := shazard(0).toBool;
  io.shazard.vsu := shazard(1).toBool;

  io.bhazard.r1w1 := bhazard(0).toBool;
  io.bhazard.r2w1 := Bool(false);
  io.bhazard.r3w1 := Bool(false);
  io.bhazard.vgslu := Bool(false);
  io.bhazard.vglu := Bool(false);
  io.bhazard.vgsu := Bool(false);
  io.bhazard.vlu := bhazard(1).toBool;
  io.bhazard.vsu := bhazard(2).toBool;

  io.fn.viu := Cat(M0,vmsrc,DW64,FP_,VIU_MOV);
  io.fn.vau0 := Bits(0,DEF_VAU0_FN);
  io.fn.vau1 := Bits(0,DEF_VAU1_FN);
  io.fn.vau2 := Bits(0,DEF_VAU2_FN);

  val vt_m1 = Cat(Bits(0,1),vt(4,0)).toUFix - UFix(1,1);
  val vd_m1 = Cat(Bits(0,1),vd(4,0)).toUFix - UFix(1,1);

  io.decoded.vs := Bits(0,DEF_REGLEN);
  io.decoded.vt := Mux(vt(5), vt_m1 + reg_nxregs.toUFix, vt_m1); 
  io.decoded.vr := Bits(0,DEF_REGLEN);
  io.decoded.vd := Mux(vd(5), vd_m1 + reg_nxregs.toUFix, vd_m1);
  io.decoded.vs_zero := Bool(true);
  io.decoded.vt_zero := vt === Bits(0,6);
  io.decoded.vr_zero := Bool(true);
  io.decoded.vd_zero := (vd === Bits(0,6) & vd_valid).toBool;
  io.decoded.cmd := cmd;
  io.decoded.imm := imm;
  io.decoded.imm2 := imm2;
}

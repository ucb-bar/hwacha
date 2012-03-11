package hwacha

import Chisel._
import Node._
import Constants._
import Commands._
import Instructions._

class io_vf extends Bundle
{
  val active = Bool(OUTPUT)
  val fire = Bool(OUTPUT)
  val stop = Bool(INPUT)
  val pc = Bits(SZ_ADDR, OUTPUT)
  val nxregs = Bits(SZ_REGCNT, OUTPUT)
  val imm1_rtag = Bits(SZ_IRB_IMM1, OUTPUT)
  val stride = Bits(SZ_REGLEN, OUTPUT)
}

class io_vxu_issue_vt extends Bundle
{
  val illegal = Bool(OUTPUT)

  val imem_req = new io_imem_req()
  val imem_resp = new io_imem_resp().flip

  val vf = new io_vf().flip

  val valid = new io_vxu_issue_fire().asOutput
  val ready = Bool(INPUT)
  val dhazard = new io_vxu_issue_reg().asOutput
  val shazard = new io_vxu_issue_fu().asOutput
  val bhazard = new io_vxu_issue_op().asOutput
  val fn = new io_vxu_issue_fn().asOutput
  val decoded = new io_vxu_issue_regid_imm().asOutput

  val vxu_cntq = new io_vxu_cntq().flip()
  val irb_cntb = new io_vxu_cntq()

  val issue_to_irb = new io_issue_to_irb()
  val irb_to_issue = new io_irb_to_issue().flip

  val flush = Bool(INPUT)
  val xcpt_to_issue = new io_xcpt_handler_to_issue().flip()
}

class vuVXU_Issue_VT extends Component
{
  val io = new io_vxu_issue_vt()

  val vt_ready = io.ready && io.irb_cntb.ready & !io.xcpt_to_issue.stall

  val stalld = !vt_ready
  val killf = !io.imem_resp.valid || !io.imem_req.ready
  val reg_killf = Reg(killf)

  val if_reg_pc = Reg(resetVal = Bits(0,SZ_ADDR))
  val if_next_pc = if_reg_pc + UFix(4)

  val imm1_rtag = Reg(resetVal = Bits(0,SZ_IRB_IMM1))

  when(io.flush) 
  {
    if_reg_pc := Bits(0,SZ_ADDR)
    imm1_rtag := Bits(0,SZ_IRB_IMM1)
  } 
  .elsewhen (io.vf.fire) 
  { 
    if_reg_pc := io.vf.pc 
    imm1_rtag := io.vf.imm1_rtag
  }
  .elsewhen (!killf && !reg_killf && !stalld)
  { 
    if_reg_pc := if_next_pc 
  }

  val req_pc = Mux(reg_killf || stalld, if_reg_pc, if_next_pc)

  io.imem_req.bits := req_pc
  io.imem_req.valid := io.vf.active

  val id_reg_inst = Reg(resetVal = Bits(0,SZ_INST))
  val id_pc_next = Reg(resetVal = Bits(0,SZ_ADDR))

  io.decoded.irb.imm1_rtag := imm1_rtag
  io.decoded.irb.cnt_rtag := io.irb_to_issue.cnt_rtag
  io.decoded.irb.pc_next := id_pc_next
  io.decoded.irb.update_imm1 := Bool(true)

  when (io.flush)
  {
    id_reg_inst := Bits(0,SZ_INST)
    id_pc_next := Bits(0,SZ_ADDR)
  }
  .elsewhen (io.vf.fire)
  {
    id_reg_inst := NOP
  }
  .elsewhen (!stalld)
  {
    id_reg_inst := io.imem_resp.bits
    id_pc_next := req_pc
    when (killf || reg_killf)
    { 
      id_reg_inst := NOP
    }
  }

  val n = Bits(0,1)
  val y = Bits(1,1)

  val cs =
  ListLookup(id_reg_inst,
                //                                                                                                                                                                   vd_valid
                //                                                                                                                                                                   | decode_stop
                //                                                                                                                                                                   | | mem_type_float
                //                                                                                                                                                                   | | |     mem_type
                //         val                  dhazard          shazard            bhazard      viufn                        vau0fn        vau1fn         vau2fn     vs vt vr vd i  | | |     |    mem_cmd
                //         |                    |                |                  |            |                            |             |              |          |  |  |  |  |  | | |     |    |
                List(Bits("b0_000_000",7),Bits("b000_0",4),Bits("b000_000",6),Bits("b000_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    R_,R_,R_,R_,I_,n,n,MTF_X,MT_X,M_X),Array(
    LB->        List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_N,MT_B,M_XRD),
    LH->        List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_N,MT_H,M_XRD),
    LW->        List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_N,MT_W,M_XRD),
    LD->        List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_N,MT_D,M_XRD),
    LBU->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_N,MT_BU,M_XRD),
    LHU->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_N,MT_HU,M_XRD),
    LWU->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_N,MT_WU,M_XRD),
    SB->        List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b000_001",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,R_,IB,n,n,MTF_N,MT_B,M_XWR),
    SH->        List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b000_001",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,R_,IB,n,n,MTF_N,MT_H,M_XWR),
    SW->        List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b000_001",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,R_,IB,n,n,MTF_N,MT_W,M_XWR),
    SD->        List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b000_001",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,R_,IB,n,n,MTF_N,MT_D,M_XWR),
    AMOADD_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_W,M_XA_ADD),
    AMOSWAP_W-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_W,M_XA_SWAP),
    AMOAND_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_W,M_XA_AND),
    AMOOR_W->   List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_W,M_XA_OR),
    AMOMIN_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_W,M_XA_MIN),
    AMOMAX_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_W,M_XA_MAX),
    AMOMINU_W-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_W,M_XA_MINU),
    AMOMAXU_W-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_W,M_XA_MAXU),
    AMOADD_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_D,M_XA_ADD),
    AMOSWAP_D-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_D,M_XA_SWAP),
    AMOAND_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_D,M_XA_AND),
    AMOOR_D->   List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_D,M_XA_OR),
    AMOMIN_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_D,M_XA_MIN),
    AMOMAX_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_D,M_XA_MAX),
    AMOMINU_D-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_D,M_XA_MINU),
    AMOMAXU_D-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I0,y,n,MTF_N,MT_D,M_XA_MAXU),
    FLW->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RF,II,y,n,MTF_Y,MT_W,M_XRD),
    FLD->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RF,II,y,n,MTF_Y,MT_D,M_XRD),
    FSW->       List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b000_001",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RF,R_,R_,IB,n,n,MTF_Y,MT_W,M_XWR),
    FSD->       List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b000_001",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RF,R_,R_,IB,n,n,MTF_Y,MT_D,M_XWR),

    UTIDX->     List(Bits("b1_000_000",7),Bits("b000_1",4),Bits("b000_000",6),Bits("b100_000",6),M0,M0,DW64,FP_,VIU_IDX, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    R_,R_,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    MOVZ->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_MOVZ,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    MOVN->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_MOVN,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FMOVZ->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_MOVZ,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FMOVN->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_MOVN,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),

    LUI->       List(Bits("b1_000_000",7),Bits("b000_1",4),Bits("b000_000",6),Bits("b100_000",6),M0,MI,DW64,FP_,VIU_MOV, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    R_,R_,R_,RX,IL,y,n,MTF_X,MT_X,M_X),
    ADDI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_ADD, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),
    SLLI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_SLL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),
    SLTI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_SLT, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),
    SLTIU->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_SLTU,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),
    XORI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_XOR, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),
    SRLI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_SRL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),
    SRAI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_SRA, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),
    ORI->       List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_OR,  DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),
    ANDI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_AND, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),

    ADD->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_ADD, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    SUB->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_SUB, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    SLL->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_SLL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    SLT->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_SLT, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    SLTU->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_SLTU,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    riscvXOR->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_XOR, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    SRL->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_SRL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    SRA->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_SRA, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    riscvOR->   List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_OR,  DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    riscvAND->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_AND, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),

    ADDIW->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW32,FP_,VIU_ADD, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),
    SLLIW->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW32,FP_,VIU_SLL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),
    SRLIW->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW32,FP_,VIU_SRL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),
    SRAIW->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW32,FP_,VIU_SRA, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,MTF_X,MT_X,M_X),

    ADDW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW32,FP_,VIU_ADD, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    SUBW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW32,FP_,VIU_SUB, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    SLLW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW32,FP_,VIU_SLL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    SRLW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW32,FP_,VIU_SRL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    SRAW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW32,FP_,VIU_SRA, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),

    FSGNJ_S->   List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FSJ, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FSGNJN_S->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FSJN,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FSGNJX_S->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FSJX,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FEQ_S->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FEQ, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FLT_S->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FLT, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FLE_S->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FLE, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FMIN_S->    List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FMIN,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FMAX_S->    List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FMAX,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FSGNJ_D->   List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FSJ, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FSGNJN_D->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FSJN,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FSGNJX_D->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FSJX,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FEQ_D->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FEQ, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FLT_D->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FLT, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FLE_D->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FLE, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FMIN_D->    List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FMIN,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FMAX_D->    List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FMAX,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),

    MUL->       List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW64,VAU0_M,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    MULH->      List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW64,VAU0_MH,  FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    MULHU->     List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW64,VAU0_MHU, FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    MULHSU->    List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW64,VAU0_MHSU,FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    MULW->      List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW32,VAU0_M,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,MTF_X,MT_X,M_X),

    FADD_S->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_ADD,  FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FSUB_S->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_SUB,  FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FMUL_S->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_MUL,  FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FMADD_S->   List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_MADD, FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,MTF_X,MT_X,M_X),
    FMSUB_S->   List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_MSUB, FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,MTF_X,MT_X,M_X),
    FNMSUB_S->  List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_NMSUB,FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,MTF_X,MT_X,M_X),
    FNMADD_S->  List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_NMADD,FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,MTF_X,MT_X,M_X),
    FADD_D->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_ADD,  FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FSUB_D->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_SUB,  FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FMUL_D->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_MUL,  FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FMADD_D->   List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_MADD, FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,MTF_X,MT_X,M_X),
    FMSUB_D->   List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_MSUB, FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,MTF_X,MT_X,M_X),
    FNMSUB_D->  List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_NMSUB,FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,MTF_X,MT_X,M_X),
    FNMADD_D->  List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_NMADD,FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,MTF_X,MT_X,M_X),

    FCVT_S_D->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CDTS, RF,R_,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_D_S->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CSTD, RF,R_,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_L_S->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CFTL, RF,R_,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_LU_S-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CFTLU,RF,R_,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_W_S->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CFTW, RF,R_,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_WU_S-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CFTWU,RF,R_,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_S_L->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CLTF, RX,R_,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_S_LU-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CLUTF,RX,R_,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_S_W->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CWTF, RX,R_,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_S_WU-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CWUTF,RX,R_,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    MXTF_S->    List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_MXTF, RX,R_,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    MFTX_S->    List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_MFTX, RF,R_,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_L_D->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CFTL, RF,R_,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_LU_D-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CFTLU,RF,R_,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_W_D->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CFTW, RF,R_,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_WU_D-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CFTWU,RF,R_,R_,RX,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_D_L->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CLTF, RX,R_,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_D_LU-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CLUTF,RX,R_,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_D_W->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CWTF, RX,R_,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    FCVT_D_WU-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CWUTF,RX,R_,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    MXTF_D->    List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_MXTF, RX,R_,R_,RF,I_,y,n,MTF_X,MT_X,M_X),
    MFTX_D->    List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_MFTX, RF,R_,R_,RX,I_,y,n,MTF_X,MT_X,M_X),

    STOP->      List(Bits("b0_000_000",7),Bits("b000_0",4),Bits("b000_000",6),Bits("b000_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    R_,R_,R_,R_,I_,n,y,MTF_X,MT_X,M_X)
  ))

  val valid::dhazard::shazard::bhazard::viu_t0::viu_t1::viu_dw::viu_fp::viu_fn::vau0_dw::vau0_fn::vau1_fp::vau1_fn::vau2_fp::vau2_fn::cs0 = cs
  val rtype_vs::rtype_vt::rtype_vr::rtype_vd::itype::vd_valid::decode_stop::mem_type_float::mem_type::mem_cmd::Nil = cs0

  val unmasked_valid_viu = valid(6)
  val unmasked_valid_vau0 = valid(5)
  val unmasked_valid_vau1 = valid(4)
  val unmasked_valid_vau2 = valid(3)
  val unmasked_valid_amo = valid(2)
  val unmasked_valid_utld = valid(1)
  val unmasked_valid_utst = valid(0)

  val vau1_rm = Wire(){Bits(width = 3)}
  val vau2_rm = Wire(){Bits(width = 3)}

  vau1_rm := id_reg_inst(11,9)
  vau2_rm := id_reg_inst(11,9)

  when (id_reg_inst(11,9) === Bits("b111",3))
  {
    vau1_rm := Bits(0,3)
    vau2_rm := Bits(0,3)
  }

  val unmasked_valid =
    unmasked_valid_viu ||
    unmasked_valid_vau0 || unmasked_valid_vau1 || unmasked_valid_vau2 ||
    unmasked_valid_amo || unmasked_valid_utld || unmasked_valid_utst

  io.vf.stop := decode_stop

  val imm = MuxLookup(
    itype, Bits(0,SZ_DATA), Array(
      I0 -> Bits(0,65),
      II -> Cat(Bits(0,1),Fill(52,id_reg_inst(21)),id_reg_inst(21,10)),
      IB -> Cat(Bits(0,1),Fill(52,id_reg_inst(31)),id_reg_inst(31,27),id_reg_inst(16,10)),
      IL -> Cat(Bits(0,1),Fill(32,id_reg_inst(26)),id_reg_inst(26,7),Bits(0,12))
    ))

  val cnt = Mux(io.vxu_cntq.valid, io.vxu_cntq.bits, Bits(0))
  val regid_base = (cnt >> UFix(3)) * io.vf.stride

  io.vxu_cntq.ready := io.vf.active & io.ready & unmasked_valid & !io.decoded.vd_zero & !io.xcpt_to_issue.stall

  io.irb_cntb.valid := io.vf.active & io.ready & unmasked_valid & !io.decoded.vd_zero & !io.xcpt_to_issue.stall
  io.irb_cntb.bits := cnt

  io.issue_to_irb.markLast := decode_stop

  val valid_common = io.vf.active & io.irb_cntb.ready & !io.xcpt_to_issue.stall

  io.valid.viu := valid_common && unmasked_valid_viu 
  io.valid.vau0 := valid_common && unmasked_valid_vau0
  io.valid.vau1 := valid_common && unmasked_valid_vau1
  io.valid.vau2 := valid_common && unmasked_valid_vau2
  io.valid.amo := valid_common && unmasked_valid_amo
  io.valid.utld := valid_common && unmasked_valid_utld
  io.valid.utst := valid_common && unmasked_valid_utst
  io.valid.vld := Bool(false)
  io.valid.vst := Bool(false)

  io.dhazard.vs := dhazard(3)
  io.dhazard.vt := dhazard(2)
  io.dhazard.vr := dhazard(1)
  io.dhazard.vd := dhazard(0)

  io.shazard.viu := Bool(false)
  io.shazard.vau0 := shazard(5)
  io.shazard.vau1 := shazard(4)
  io.shazard.vau2 := shazard(3)
  io.shazard.vgu := shazard(2)
  io.shazard.vlu := shazard(1)
  io.shazard.vsu := shazard(0)

  io.bhazard.r1w1 := bhazard(5)
  io.bhazard.r2w1 := bhazard(4)
  io.bhazard.r3w1 := bhazard(3)
  io.bhazard.amo := bhazard(2)
  io.bhazard.utld := bhazard(1)
  io.bhazard.utst := bhazard(0)
  io.bhazard.vld := Bool(false)
  io.bhazard.vst := Bool(false)

  io.fn.viu := Cat(viu_t0,viu_t1,viu_dw,viu_fp,viu_fn)
  io.fn.vau0 := Cat(vau0_dw,vau0_fn)
  io.fn.vau1 := Cat(vau1_fp,vau1_rm,vau1_fn)
  io.fn.vau2 := Cat(vau2_fp,vau2_rm,vau2_fn)

  val vs = Cat(rtype_vs,id_reg_inst(26,22))
  val vt = Cat(rtype_vt,id_reg_inst(21,17))
  val vr = Cat(rtype_vr,id_reg_inst(16,12))
  val vd = Cat(rtype_vd,id_reg_inst(31,27))

  val vs_m1 = Cat(Bits(0,1),vs(4,0)) - UFix(1,1)
  val vt_m1 = Cat(Bits(0,1),vt(4,0)) - UFix(1,1)
  val vr_m1 = Cat(Bits(0,1),vr(4,0)) - UFix(1,1)
  val vd_m1 = Cat(Bits(0,1),vd(4,0)) - UFix(1,1)

  io.decoded.utidx := Mux(io.vxu_cntq.valid, io.vxu_cntq.bits, Bits(0))
  io.decoded.vs := Mux(rtype_vs, vs_m1 + io.vf.nxregs, vs_m1) + regid_base
  io.decoded.vt := Mux(rtype_vt, vt_m1 + io.vf.nxregs, vt_m1) + regid_base
  io.decoded.vr := Mux(rtype_vr, vr_m1 + io.vf.nxregs, vr_m1) + regid_base
  io.decoded.vd := Mux(rtype_vd, vd_m1 + io.vf.nxregs, vd_m1) + regid_base
  io.decoded.vs_zero := vs === Bits(0,6)
  io.decoded.vt_zero := vt === Bits(0,6)
  io.decoded.vr_zero := vr === Bits(0,6)
  io.decoded.vd_zero := vd === Bits(0,6) && vd_valid
  io.decoded.mem.cmd := mem_cmd
  io.decoded.mem.typ := mem_type
  io.decoded.mem.typ_float := mem_type_float
  io.decoded.imm := imm
  io.decoded.cnt_valid := io.vxu_cntq.valid
  io.decoded.cnt := cnt

  io.illegal := Reg(io.vf.active && (~unmasked_valid && ~decode_stop), resetVal = Bool(false))
}

package hwacha

import Chisel._
import Node._
import Config._
import Commands._
import Instructions._

class vuVXU_Issue_VT extends Component
{
  val io = new io_vxu_issue_vt();

  val stalld = ~io.ready;
  val stallf = ~io.imem_req.ready | ~io.imem_resp.valid | stalld;
  val killf = ~io.imem_resp.valid;

  val if_reg_pc = Reg(resetVal = Bits(0,SZ_ADDR));
  val if_next_pc = if_reg_pc.toUFix() + UFix(4);

  when (io.vf.fire)
  {
    if_reg_pc <== io.vf.pc;
  }
  when (!io.vf.fire && !stallf)
  {
    if_reg_pc <== if_next_pc;
  }

  io.imem_req.bits := Mux(
    stallf, if_reg_pc,
    if_next_pc);
  io.imem_req.valid := io.vf.active;

  val id_reg_inst = Reg(resetVal = Bits(0,DEF_INST));

  when (io.vf.fire)
  {
    id_reg_inst <== NOP;
  }
  when (!io.vf.fire && !stalld)
  {
    id_reg_inst <== io.imem_resp.bits;
    when (killf)
    {
      id_reg_inst <== NOP;
    }
  }

  val n = Bits(0,1);
  val y = Bits(1,1);

  val cs =
  ListLookup(id_reg_inst,
                //                                                                                                                                                                   vd_valid
                //                                                                                                                                                                   | decode_stop
                //                                                                                                                                                                   | | enq_vmu_utcmdq
                //                                                                                                                                                                   | | | enq_vmu_utimmq
                //         val                  dhazard          shazard            bhazard      viufn                        vau0fn        vau1fn         vau2fn     vs vt vr vd i  | | | | utcmd
                //         |                    |                |                  |            |                            |             |              |          |  |  |  |  |  | | | | |
                List(Bits("b0_000_000",7),Bits("b000_0",4),Bits("b000_000",6),Bits("b000_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    R_,R_,R_,R_,I_,n,n,n,n,CMD_X),Array(
    LB->        List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,y,y,CMD_VLXB),
    LH->        List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,y,y,CMD_VLXH),
    LW->        List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,y,y,CMD_VLXW),
    LD->        List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,y,y,CMD_VLXD),
    LBU->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,y,y,CMD_VLXBU),
    LHU->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,y,y,CMD_VLXHU),
    LWU->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,y,y,CMD_VLXWU),
    SB->        List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b000_001",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,R_,IB,n,n,y,y,CMD_VSXB),
    SH->        List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b000_001",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,R_,IB,n,n,y,y,CMD_VSXH),
    SW->        List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b000_001",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,R_,IB,n,n,y,y,CMD_VSXW),
    SD->        List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b000_001",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,R_,IB,n,n,y,y,CMD_VSXD),
    AMOADD_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOADDW),
    AMOSWAP_W-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOSWAPW),
    AMOAND_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOANDW),
    AMOOR_W->   List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOORW),
    AMOMIN_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOMINW),
    AMOMAX_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOMAXW),
    AMOMINU_W-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOMINUW),
    AMOMAXU_W-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOMAXUW),
    AMOADD_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOADDD),
    AMOSWAP_D-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOSWAPD),
    AMOAND_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOANDD),
    AMOOR_D->   List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOORD),
    AMOMIN_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOMIND),
    AMOMAX_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOMAXD),
    AMOMINU_D-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOMINUD),
    AMOMAXU_D-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b000_100",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,y,n,CMD_VAMOMAXUD),
    FLW->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RF,II,y,n,y,y,CMD_VFLXW),
    FLD->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b000_010",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RF,II,y,n,y,y,CMD_VFLXD),
    FSW->       List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b000_001",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RF,R_,R_,IB,n,n,y,y,CMD_VFSXW),
    FSD->       List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b000_001",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RF,R_,R_,IB,n,n,y,y,CMD_VFSXD),

    UTIDX->     List(Bits("b1_000_000",7),Bits("b000_1",4),Bits("b000_000",6),Bits("b100_000",6),M0,M0,DW64,FP_,VIU_IDX, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    R_,R_,R_,RX,I_,y,n,n,n,CMD_X),
    MOVZ->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_MOVZ,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    MOVN->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_MOVN,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    FMOVZ->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_MOVZ,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FMOVN->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_MOVN,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RF,R_,RF,I_,y,n,n,n,CMD_X),

    LUI->       List(Bits("b1_000_000",7),Bits("b000_1",4),Bits("b000_000",6),Bits("b100_000",6),M0,MI,DW64,FP_,VIU_MOV, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    R_,R_,R_,RX,IL,y,n,n,n,CMD_X),
    ADDI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_ADD, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),
    SLLI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_SLL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),
    SLTI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_SLT, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),
    SLTIU->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_SLTU,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),
    XORI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_XOR, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),
    SRLI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_SRL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),
    SRAI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_SRA, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),
    ORI->       List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_OR,  DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),
    ANDI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW64,FP_,VIU_AND, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),

    ADD->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_ADD, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    SUB->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_SUB, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    SLL->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_SLL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    SLT->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_SLT, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    SLTU->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_SLTU,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    riscvXOR->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_XOR, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    SRL->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_SRL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    SRA->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_SRA, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    riscvOR->   List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_OR,  DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    riscvAND->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FP_,VIU_AND, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),

    ADDIW->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW32,FP_,VIU_ADD, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),
    SLLIW->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW32,FP_,VIU_SLL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),
    SRLIW->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW32,FP_,VIU_SRL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),
    SRAIW->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b100_000",6),MR,MI,DW32,FP_,VIU_SRA, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,R_,R_,RX,II,y,n,n,n,CMD_X),

    ADDW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW32,FP_,VIU_ADD, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    SUBW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW32,FP_,VIU_SUB, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    SLLW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW32,FP_,VIU_SLL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    SRLW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW32,FP_,VIU_SRL, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    SRAW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW32,FP_,VIU_SRA, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),

    FSGNJ_S->   List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FSJ, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FSGNJN_S->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FSJN,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FSGNJX_S->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FSJX,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FEQ_S->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FEQ, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RX,I_,y,n,n,n,CMD_X),
    FLT_S->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FLT, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RX,I_,y,n,n,n,CMD_X),
    FLE_S->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FLE, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RX,I_,y,n,n,n,CMD_X),
    FMIN_S->    List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FMIN,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FMAX_S->    List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPS,VIU_FMAX,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FSGNJ_D->   List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FSJ, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FSGNJN_D->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FSJN,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FSGNJX_D->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FSJX,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FEQ_D->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FEQ, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RX,I_,y,n,n,n,CMD_X),
    FLT_D->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FLT, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RX,I_,y,n,n,n,CMD_X),
    FLE_D->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FLE, DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RX,I_,y,n,n,n,CMD_X),
    FMIN_D->    List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FMIN,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FMAX_D->    List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b010_000",6),ML,MR,DW64,FPD,VIU_FMAX,DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),

    MUL->       List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW64,VAU0_M,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    MULH->      List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW64,VAU0_MH,  FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    MULHU->     List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW64,VAU0_MHU, FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    MULHSU->    List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW64,VAU0_MHSU,FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),
    MULW->      List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW32,VAU0_M,   FP_,VAU1_X,    FP_,VAU2_X,    RX,RX,R_,RX,I_,y,n,n,n,CMD_X),

    FADD_S->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_ADD,  FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FSUB_S->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_SUB,  FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FMUL_S->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_MUL,  FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FMADD_S->   List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_MADD, FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,n,n,CMD_X),
    FMSUB_S->   List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_MSUB, FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,n,n,CMD_X),
    FNMSUB_S->  List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_NMSUB,FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,n,n,CMD_X),
    FNMADD_S->  List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPS,VAU1_NMADD,FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,n,n,CMD_X),
    FADD_D->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_ADD,  FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FSUB_D->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_SUB,  FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FMUL_D->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b010_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_MUL,  FP_,VAU2_X,    RF,RF,R_,RF,I_,y,n,n,n,CMD_X),
    FMADD_D->   List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_MADD, FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,n,n,CMD_X),
    FMSUB_D->   List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_MSUB, FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,n,n,CMD_X),
    FNMSUB_D->  List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_NMSUB,FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,n,n,CMD_X),
    FNMADD_D->  List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b001_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FPD,VAU1_NMADD,FP_,VAU2_X,    RF,RF,RF,RF,I_,y,n,n,n,CMD_X),

    FCVT_S_D->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CDTS, RF,R_,R_,RF,I_,y,n,n,n,CMD_X),
    FCVT_D_S->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CSTD, RF,R_,R_,RF,I_,y,n,n,n,CMD_X),
    FCVT_L_S->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CFTL, RF,R_,R_,RX,I_,y,n,n,n,CMD_X),
    FCVT_LU_S-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CFTLU,RF,R_,R_,RX,I_,y,n,n,n,CMD_X),
    FCVT_W_S->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CFTW, RF,R_,R_,RX,I_,y,n,n,n,CMD_X),
    FCVT_WU_S-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CFTWU,RF,R_,R_,RX,I_,y,n,n,n,CMD_X),
    FCVT_S_L->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CLTF, RX,R_,R_,RF,I_,y,n,n,n,CMD_X),
    FCVT_S_LU-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CLUTF,RX,R_,R_,RF,I_,y,n,n,n,CMD_X),
    FCVT_S_W->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CWTF, RX,R_,R_,RF,I_,y,n,n,n,CMD_X),
    FCVT_S_WU-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_CWUTF,RX,R_,R_,RF,I_,y,n,n,n,CMD_X),
    MXTF_S->    List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_MXTF, RX,R_,R_,RF,I_,y,n,n,n,CMD_X),
    MFTX_S->    List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPS,VAU2_MFTX, RF,R_,R_,RX,I_,y,n,n,n,CMD_X),
    FCVT_L_D->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CFTL, RF,R_,R_,RX,I_,y,n,n,n,CMD_X),
    FCVT_LU_D-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CFTLU,RF,R_,R_,RX,I_,y,n,n,n,CMD_X),
    FCVT_W_D->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CFTW, RF,R_,R_,RX,I_,y,n,n,n,CMD_X),
    FCVT_WU_D-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CFTWU,RF,R_,R_,RX,I_,y,n,n,n,CMD_X),
    FCVT_D_L->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CLTF, RX,R_,R_,RF,I_,y,n,n,n,CMD_X),
    FCVT_D_LU-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CLUTF,RX,R_,R_,RF,I_,y,n,n,n,CMD_X),
    FCVT_D_W->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CWTF, RX,R_,R_,RF,I_,y,n,n,n,CMD_X),
    FCVT_D_WU-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_CWUTF,RX,R_,R_,RF,I_,y,n,n,n,CMD_X),
    MXTF_D->    List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_MXTF, RX,R_,R_,RF,I_,y,n,n,n,CMD_X),
    MFTX_D->    List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b100_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FPD,VAU2_MFTX, RF,R_,R_,RX,I_,y,n,n,n,CMD_X),

    STOP->      List(Bits("b0_000_000",7),Bits("b000_0",4),Bits("b000_000",6),Bits("b000_000",6),M0,M0,DW__,FP_,VIU_X,   DW__,VAU0_X,   FP_,VAU1_X,    FP_,VAU2_X,    R_,R_,R_,R_,I_,n,y,n,n,CMD_X)
  ));

  val valid::dhazard::shazard::bhazard::viu_t0::viu_t1::viu_dw::viu_fp::viu_fn::vau0_dw::vau0_fn::vau1_fp::vau1_fn::vau2_fp::vau2_fn::cs0 = cs;
  val rtype_vs::rtype_vt::rtype_vr::rtype_vd::itype::vd_valid::decode_stop::enq_vmu_utcmdq::enq_vmu_utimmq::utcmd::Nil = cs0;

  val unmasked_valid_viu = valid(6);
  val unmasked_valid_vau0 = valid(5);
  val unmasked_valid_vau1 = valid(4);
  val unmasked_valid_vau2 = valid(3);
  val unmasked_valid_vgslu = valid(2);
  val unmasked_valid_vglu = valid(1);
  val unmasked_valid_vgsu = valid(0);

  val vau1_rm = Wire(){Bits(width = 2)};
  val vau2_rm = Wire(){Bits(width = 2)};

  vau1_rm <== id_reg_inst(10,9);
  vau2_rm <== id_reg_inst(10,9);

  when (id_reg_inst(11,9) === Bits("b111",3))
  {
    vau1_rm <== Bits(0,2);
    vau2_rm <== Bits(0,2);
  }

  val unmasked_valid
    = unmasked_valid_viu |
    unmasked_valid_vau0 | unmasked_valid_vau1 | unmasked_valid_vau2 |
    unmasked_valid_vgslu | unmasked_valid_vglu | unmasked_valid_vgsu;

  io.vf.stop := decode_stop.toBool;

  val mask_issue_ready = io.ready; // always issue for memory ops
  val mask_vmu_utcmdq_ready = ~enq_vmu_utcmdq | io.vmu_utcmdq.ready;
  val mask_vmu_utimmq_ready = ~enq_vmu_utimmq | io.vmu_utimmq.ready;

  val imm = MuxLookup(
    itype, Bits(0,SZ_DATA), Array(
      II -> Cat(Bits(0,1),Fill(52,id_reg_inst(21)),id_reg_inst(21,10)),
      IB -> Cat(Bits(0,1),Fill(52,id_reg_inst(31)),id_reg_inst(31,27),id_reg_inst(16,10)),
      IL -> Cat(Bits(0,1),Fill(32,id_reg_inst(26)),id_reg_inst(26,7),Bits(0,12))
    ));

  io.vmu_utcmdq.bits := Cat(utcmd,io.vf.vlen);
  io.vmu_utimmq.bits := imm(31,0);

  io.vmu_utcmdq.valid := (io.vf.active & ~(vd_valid & ~rtype_vd & id_reg_inst(31,27) === Bits(0,5)) & mask_issue_ready & enq_vmu_utcmdq & mask_vmu_utimmq_ready).toBool;
  io.vmu_utimmq.valid := (io.vf.active & ~(vd_valid & ~rtype_vd & id_reg_inst(31,27) === Bits(0,5)) & mask_issue_ready & mask_vmu_utcmdq_ready & enq_vmu_utimmq).toBool;

  val valid_common = io.vf.active & mask_vmu_utcmdq_ready & mask_vmu_utimmq_ready;

  io.valid.viu := (valid_common & unmasked_valid_viu).toBool;
  io.valid.vau0 := (valid_common & unmasked_valid_vau0).toBool;
  io.valid.vau1 := (valid_common & unmasked_valid_vau1).toBool;
  io.valid.vau2 := (valid_common & unmasked_valid_vau2).toBool;
  io.valid.vgslu := (valid_common & unmasked_valid_vgslu).toBool;
  io.valid.vglu := (valid_common & unmasked_valid_vglu).toBool;
  io.valid.vgsu := (valid_common & unmasked_valid_vgsu).toBool;
  io.valid.vgu := Bool(false);
  io.valid.vlu := Bool(false);
  io.valid.vsu := Bool(false);

  io.dhazard.vs := dhazard(3).toBool;
  io.dhazard.vt := dhazard(2).toBool;
  io.dhazard.vr := dhazard(1).toBool;
  io.dhazard.vd := dhazard(0).toBool;

  io.shazard.viu := Bool(false);
  io.shazard.vau0 := shazard(5).toBool;
  io.shazard.vau1 := shazard(4).toBool;
  io.shazard.vau2 := shazard(3).toBool;
  io.shazard.vgslu := Bool(false);
  io.shazard.vglu := Bool(false);
  io.shazard.vgsu := Bool(false);
  io.shazard.vgu := shazard(2).toBool;
  io.shazard.vlu := shazard(1).toBool;
  io.shazard.vsu := shazard(0).toBool;

  io.bhazard.r1w1 := bhazard(5).toBool;
  io.bhazard.r2w1 := bhazard(4).toBool;
  io.bhazard.r3w1 := bhazard(3).toBool;
  io.bhazard.vgslu := bhazard(2).toBool;
  io.bhazard.vglu := bhazard(1).toBool;
  io.bhazard.vgsu := bhazard(0).toBool;
  io.bhazard.vlu := Bool(false);
  io.bhazard.vsu := Bool(false);

  io.fn.viu := Cat(viu_t0,viu_t1,viu_dw,viu_fp,viu_fn);
  io.fn.vau0 := Cat(vau0_dw,vau0_fn);
  io.fn.vau1 := Cat(vau1_fp,vau1_rm,vau1_fn);
  io.fn.vau2 := Cat(vau2_fp,vau2_rm,vau2_fn);

  val vs = Cat(rtype_vs,id_reg_inst(26,22));
  val vt = Cat(rtype_vt,id_reg_inst(21,17));
  val vr = Cat(rtype_vr,id_reg_inst(16,12));
  val vd = Cat(rtype_vd,id_reg_inst(31,27));

  val vs_m1 = Cat(Bits(0,1),vs(4,0)).toUFix - UFix(1,1);
  val vt_m1 = Cat(Bits(0,1),vt(4,0)).toUFix - UFix(1,1);
  val vr_m1 = Cat(Bits(0,1),vr(4,0)).toUFix - UFix(1,1);
  val vd_m1 = Cat(Bits(0,1),vd(4,0)).toUFix - UFix(1,1);

  io.decoded.vs := Mux(rtype_vs, vs_m1 + io.vf.nxregs.toUFix, vs_m1);
  io.decoded.vt := Mux(rtype_vt, vt_m1 + io.vf.nxregs.toUFix, vt_m1);
  io.decoded.vr := Mux(rtype_vr, vr_m1 + io.vf.nxregs.toUFix, vr_m1);
  io.decoded.vd := Mux(rtype_vd, vd_m1 + io.vf.nxregs.toUFix, vd_m1);
  io.decoded.vs_zero := vs === Bits(0,6);
  io.decoded.vt_zero := vt === Bits(0,6);
  io.decoded.vr_zero := vr === Bits(0,6);
  io.decoded.vd_zero := (vd === Bits(0,6) & vd_valid).toBool;
  io.decoded.imm := imm;

  io.illegal := Reg((io.vf.active & (~unmasked_valid & ~decode_stop)).toBool, resetVal = Bool(false));
}

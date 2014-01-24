package hwacha

import Chisel._
import Node._
import Constants._
import Commands._
import rocket.Instructions._
import Instructions._
import uncore.constants.MemoryOpConstants._

class PCBundle extends Bundle
{
  val taken = Bits(width=SZ_ADDR)
  val not_taken = Bits(width=SZ_ADDR)
}

class ioPCPipe extends ValidIO(new PCBundle() )

class ioIssueVTToPVFB extends Bundle
{
  val stop = Bool(OUTPUT)
  val pc = new ioPCPipe()
  val pvfb_tag = Bits(OUTPUT, SZ_PVFB_TAG)
}

class ReplayBundle extends Bundle
{
  val pc = Bits(width=SZ_ADDR)
  val tag = Bits(width=SZ_PVFB_TAG)
}

class ioIssueVTToPC extends Bundle
{
  val replay_pre_if = Valid(new ReplayBundle)
  val replay_if = Valid(new ReplayBundle)
  val replay_jump = Valid(new ReplayBundle)
  val replay_branch = Valid(new ReplayBundle)
  val replay_stop = Valid(new ReplayBundle)
  val replay_stalld = Valid(new ReplayBundle)
}

class io_vf extends Bundle
{
  val active = Bool(OUTPUT)
  val fire = Bool(OUTPUT)
  val stop = Bool(INPUT)
  val pc = UInt(OUTPUT, SZ_ADDR)
  val nxregs = Bits(OUTPUT, SZ_REGCNT)
  val nfregs = Bits(OUTPUT, SZ_REGCNT)
  val imm1_rtag = Bits(OUTPUT, SZ_AIW_IMM1)
  val numCnt_rtag = Bits(OUTPUT, SZ_AIW_NUMCNT)
  val xf_split = Bits(OUTPUT, SZ_BANK)
  val xstride = Bits(OUTPUT, SZ_REGLEN)
  val fstride = Bits(OUTPUT, SZ_REGLEN)
  val vlen = Bits(OUTPUT, SZ_VLEN)
}

class io_issue_vt_to_irq_handler extends Bundle
{
  val ma_inst = Bool(OUTPUT)
  val fault_inst = Bool(OUTPUT)
  val illegal = Bool(OUTPUT)
  val pc = Bits(OUTPUT, SZ_ADDR)
}

class IssueVT(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val irq = new io_issue_vt_to_irq_handler()

    val imem = new rocket.CPUFrontendIO()(conf.icache)

    val vf = new io_vf().flip

    val laneToIssue = new ioLaneToIssue().flip()

    val valid = new io_vxu_issue_fire().asOutput
    val ready = Bool(INPUT)
    val dhazard = new io_vxu_issue_reg().asOutput
    val shazard = new io_vxu_issue_fu().asOutput
    val bhazard = new io_vxu_issue_op().asOutput
    val fn = new io_vxu_issue_fn().asOutput
    val decoded = new io_vxu_issue_regid_imm().asOutput

    val vcmdq = new VCMDQIO().flip
    val aiw_cntb = new io_vxu_cntq()

    val issue_to_aiw = new io_issue_to_aiw()
    val aiw_to_issue = new io_aiw_to_issue().flip

    val xcpt_to_issue = new io_xcpt_handler_to_issue().flip()
  }

  val stall_sticky = Reg(init=Bool(false))
  val mask_stall = Bool()

  val stall_issue = stall_sticky || io.irq.illegal || io.xcpt_to_issue.stall
  val stall_frontend = stall_issue || !(io.ready && (mask_stall || io.aiw_cntb.ready)) || io.irq.ma_inst || io.irq.fault_inst

  when (io.irq.ma_inst || io.irq.fault_inst || io.irq.illegal) { stall_sticky := Bool(true) }

  io.imem.req.valid := io.vf.fire
  io.imem.req.bits.pc := io.vf.pc
  io.imem.req.bits.mispredict := Bool(false)
  io.imem.req.bits.taken := Bool(false)
  io.imem.invalidate := Bool(false)
  io.imem.resp.ready := io.vf.active && !stall_frontend

  val imm1_rtag = Reg(init=Bits(0,SZ_AIW_IMM1))
  val numCnt_rtag = Reg(init=Bits(0,SZ_AIW_CMD))

  when (io.vf.fire) 
  { 
    imm1_rtag := io.vf.imm1_rtag
    numCnt_rtag := io.vf.numCnt_rtag
  }

  io.decoded.aiw.imm1_rtag := imm1_rtag
  io.decoded.aiw.numCnt_rtag := numCnt_rtag
  io.decoded.aiw.cnt_rtag := io.aiw_to_issue.cnt_rtag
  io.decoded.aiw.pc_next := io.imem.resp.bits.pc + UInt(4, SZ_ADDR)
  io.decoded.aiw.update_imm1 := Bool(true)

  val n = Bits(0,1)
  val y = Bits(1,1)

  val cs =
  ListLookup(io.imem.resp.bits.data, 
                //         valid                                                    bhazard
                //         |                                                        |r2wm
                //         |viu                                  shazard            || r1w1                                                                                                vd_valid
                //         || vau0                               |viu               || |r2w1                                                                                               | decode_stop
                //         || |vau1             dhazard          ||vau0             || ||r3w1                                                                                              | | decode_jump
                //         || ||vau2            |vs              |||vau2            || ||| amo                                                                                             | | | mem_type_float
                //         || ||| amo           ||vt             |||| vgu           || ||| |utld                                                                                           | | | |
                //         || ||| |utld         |||vr            |||| |vlu          || ||| ||utst                                                                                          | | | |     mem_type
                //         || ||| ||utst        |||| vd          |||| ||vsu         || ||| |||     viufn                    vau0fn         VAU1fn         vau2fn         vs vt vr vd i     | | | |     |    mem_cmd
                //         || ||| |||           |||| |           |||| |||           || ||| |||     |                        |              |              |              |  |  |  |  |     | | | |     |    |
                List(Bits("b0_000_000",7),Bits("b000_0",4),Bits("b000_000",6),Bits("b0_000_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    R_,R_,R_,R_,imm_X,n,n,n,MTF_X,MT_X,M_X),Array(

    LB->        List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b0_000_010",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_N,MT_B,M_XRD),
    LH->        List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b0_000_010",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_N,MT_H,M_XRD),
    LW->        List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b0_000_010",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_N,MT_W,M_XRD),
    LD->        List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b0_000_010",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_N,MT_D,M_XRD),
    LBU->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b0_000_010",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_N,MT_BU,M_XRD),
    LHU->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b0_000_010",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_N,MT_HU,M_XRD),
    LWU->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b0_000_010",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_N,MT_WU,M_XRD),
    SB->        List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b0_000_001",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,R_,imm_S,n,n,n,MTF_N,MT_B,M_XWR),
    SH->        List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b0_000_001",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,R_,imm_S,n,n,n,MTF_N,MT_H,M_XWR),
    SW->        List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b0_000_001",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,R_,imm_S,n,n,n,MTF_N,MT_W,M_XWR),
    SD->        List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b0_000_001",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,R_,imm_S,n,n,n,MTF_N,MT_D,M_XWR),
    AMOADD_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_W,M_XA_ADD),
    AMOXOR_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_W,M_XA_XOR),
    AMOOR_W->   List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_W,M_XA_OR),
    AMOAND_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_W,M_XA_AND),
    AMOMIN_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_W,M_XA_MIN),
    AMOMAX_W->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_W,M_XA_MAX),
    AMOMINU_W-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_W,M_XA_MINU),
    AMOMAXU_W-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_W,M_XA_MAXU),
    AMOSWAP_W-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_W,M_XA_SWAP),
    AMOADD_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_D,M_XA_ADD),
    AMOXOR_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_D,M_XA_XOR),
    AMOOR_D->   List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_D,M_XA_OR),
    AMOAND_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_D,M_XA_AND),
    AMOMIN_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_D,M_XA_MIN),
    AMOMAX_D->  List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_D,M_XA_MAX),
    AMOMINU_D-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_D,M_XA_MINU),
    AMOMAXU_D-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_D,M_XA_MAXU),
    AMOSWAP_D-> List(Bits("b0_000_100",7),Bits("b110_1",4),Bits("b000_111",6),Bits("b0_000_100",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_0,y,n,n,MTF_N,MT_D,M_XA_SWAP),
    FLH->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b0_000_010",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RF,imm_I,y,n,n,MTF_Y,MT_H,M_XRD),
    FLW->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b0_000_010",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RF,imm_I,y,n,n,MTF_Y,MT_W,M_XRD),
    FLD->       List(Bits("b0_000_010",7),Bits("b100_1",4),Bits("b000_110",6),Bits("b0_000_010",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RF,imm_I,y,n,n,MTF_Y,MT_D,M_XRD),
    FSH->       List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b0_000_001",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RF,R_,R_,imm_S,n,n,n,MTF_Y,MT_H,M_XWR),
    FSW->       List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b0_000_001",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RF,R_,R_,imm_S,n,n,n,MTF_Y,MT_W,M_XWR),
    FSD->       List(Bits("b0_000_001",7),Bits("b110_0",4),Bits("b000_101",6),Bits("b0_000_001",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RF,R_,R_,imm_S,n,n,n,MTF_Y,MT_D,M_XWR),

    UTIDX->     List(Bits("b1_000_000",7),Bits("b000_1",4),Bits("b000_000",6),Bits("b0_100_000",7),M0,M0,DW64,FP_,viu_IDX,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    R_,R_,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    MOVZ->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_MOVZ, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    MOVN->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_MOVN, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMOVZ->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_MOVZ, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMOVN->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_MOVN, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),

    LUI->       List(Bits("b1_000_000",7),Bits("b000_1",4),Bits("b000_000",6),Bits("b0_100_000",7),M0,MI,DW64,FP_,viu_MOV,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    R_,R_,R_,RX,imm_U,y,n,n,MTF_X,MT_X,M_X),
    ADDI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW64,FP_,viu_ADD,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),
    SLLI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW64,FP_,viu_SLL,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),
    SLTI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW64,FP_,viu_SLT,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),
    SLTIU->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW64,FP_,viu_SLTU, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),
    XORI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW64,FP_,viu_XOR,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),
    SRLI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW64,FP_,viu_SRL,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),
    SRAI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW64,FP_,viu_SRA,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),
    ORI->       List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW64,FP_,viu_OR,   DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),
    ANDI->      List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW64,FP_,viu_AND,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),

    ADD->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_ADD,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    SUB->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_SUB,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    SLL->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_SLL,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    SLT->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_SLT,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    SLTU->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_SLTU, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    XOR->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_XOR,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    SRL->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_SRL,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    SRA->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_SRA,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    OR->        List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_OR,   DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    AND->       List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FP_,viu_AND,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),

    ADDIW->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW32,FP_,viu_ADD,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),
    SLLIW->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW32,FP_,viu_SLL,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),
    SRLIW->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW32,FP_,viu_SRL,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),
    SRAIW->     List(Bits("b1_000_000",7),Bits("b100_1",4),Bits("b000_000",6),Bits("b0_100_000",7),MR,MI,DW32,FP_,viu_SRA,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,R_,R_,RX,imm_I,y,n,n,MTF_X,MT_X,M_X),

    ADDW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW32,FP_,viu_ADD,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    SUBW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW32,FP_,viu_SUB,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    SLLW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW32,FP_,viu_SLL,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    SRLW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW32,FP_,viu_SRL,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    SRAW->      List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW32,FP_,viu_SRA,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),

    FSGNJ_S->   List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPS,viu_FSJ,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FSGNJN_S->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPS,viu_FSJN, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FSGNJX_S->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPS,viu_FSJX, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FEQ_S->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPS,viu_FEQ,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FLT_S->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPS,viu_FLT,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FLE_S->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPS,viu_FLE,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMIN_S->    List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPS,viu_FMIN, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMAX_S->    List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPS,viu_FMAX, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FSGNJ_D->   List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPD,viu_FSJ,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FSGNJN_D->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPD,viu_FSJN, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FSGNJX_D->  List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPD,viu_FSJX, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FEQ_D->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPD,viu_FEQ,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FLT_D->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPD,viu_FLT,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FLE_D->     List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPD,viu_FLE,  DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMIN_D->    List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPD,viu_FMIN, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMAX_D->    List(Bits("b1_000_000",7),Bits("b110_1",4),Bits("b000_000",6),Bits("b0_010_000",7),ML,MR,DW64,FPD,viu_FMAX, DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),

    MUL->       List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW64,vau0_M,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    MULH->      List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW64,vau0_MH,  FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    MULHU->     List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW64,vau0_MHU, FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    MULHSU->    List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW64,vau0_MHSU,FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    MULW->      List(Bits("b0_100_000",7),Bits("b110_1",4),Bits("b100_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW32,vau0_M,   FP_,VAU1_X,    FP_,vau2_X,    RX,RX,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),

    FADD_H->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPH,VAU1_ADD,  FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FSUB_H->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPH,VAU1_SUB,  FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMUL_H->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPH,VAU1_MUL,  FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FADD_S->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPS,VAU1_ADD,  FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FSUB_S->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPS,VAU1_SUB,  FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMUL_S->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPS,VAU1_MUL,  FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMADD_S->   List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b0_001_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPS,VAU1_MADD, FP_,vau2_X,    RF,RF,RF,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMSUB_S->   List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b0_001_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPS,VAU1_MSUB, FP_,vau2_X,    RF,RF,RF,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FNMSUB_S->  List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b0_001_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPS,VAU1_NMSUB,FP_,vau2_X,    RF,RF,RF,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FNMADD_S->  List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b0_001_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPS,VAU1_NMADD,FP_,vau2_X,    RF,RF,RF,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FADD_D->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPD,VAU1_ADD,  FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FSUB_D->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPD,VAU1_SUB,  FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMUL_D->    List(Bits("b0_010_000",7),Bits("b110_1",4),Bits("b010_000",6),Bits("b0_010_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPD,VAU1_MUL,  FP_,vau2_X,    RF,RF,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMADD_D->   List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b0_001_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPD,VAU1_MADD, FP_,vau2_X,    RF,RF,RF,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMSUB_D->   List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b0_001_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPD,VAU1_MSUB, FP_,vau2_X,    RF,RF,RF,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FNMSUB_D->  List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b0_001_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPD,VAU1_NMSUB,FP_,vau2_X,    RF,RF,RF,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FNMADD_D->  List(Bits("b0_010_000",7),Bits("b111_1",4),Bits("b010_000",6),Bits("b0_001_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FPD,VAU1_NMADD,FP_,vau2_X,    RF,RF,RF,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),

    FCVT_S_D->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPS,vau2_CDTS, RF,R_,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_D_S->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPD,vau2_CSTD, RF,R_,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_L_S->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPS,vau2_CFTL, RF,R_,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_LU_S-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPS,vau2_CFTLU,RF,R_,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_W_S->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPS,vau2_CFTW, RF,R_,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_WU_S-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPS,vau2_CFTWU,RF,R_,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_S_L->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPS,vau2_CLTF, RX,R_,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_S_LU-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPS,vau2_CLUTF,RX,R_,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_S_W->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPS,vau2_CWTF, RX,R_,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_S_WU-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPS,vau2_CWUTF,RX,R_,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMV_S_X->   List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPS,vau2_MXTF, RX,R_,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMV_X_S->   List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPS,vau2_MFTX, RF,R_,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_L_D->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPD,vau2_CFTL, RF,R_,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_LU_D-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPD,vau2_CFTLU,RF,R_,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_W_D->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPD,vau2_CFTW, RF,R_,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_WU_D-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPD,vau2_CFTWU,RF,R_,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_D_L->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPD,vau2_CLTF, RX,R_,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_D_LU-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPD,vau2_CLUTF,RX,R_,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_D_W->  List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPD,vau2_CWTF, RX,R_,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FCVT_D_WU-> List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPD,vau2_CWUTF,RX,R_,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMV_D_X->   List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPD,vau2_MXTF, RX,R_,R_,RF,imm_X,y,n,n,MTF_X,MT_X,M_X),
    FMV_X_D->   List(Bits("b0_001_000",7),Bits("b100_1",4),Bits("b001_000",6),Bits("b0_100_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FPD,vau2_MFTX, RF,R_,R_,RX,imm_X,y,n,n,MTF_X,MT_X,M_X),

    STOP->      List(Bits("b0_000_000",7),Bits("b000_0",4),Bits("b000_000",6),Bits("b0_000_000",7),M0,M0,DW__,FP_,viu_X,    DW__,vau0_X,   FP_,VAU1_X,    FP_,vau2_X,    R_,R_,R_,R_,imm_X,n,y,n,MTF_X,MT_X,M_X)
  ))

  val valid::dhazard::shazard::bhazard::viu_t0::viu_t1::viu_dw::viu_fp::viu_fn::vau0_dw::vau0_fn::vau1_fp::vau1_fn::vau2_fp::vau2_fn::cs0 = cs
  val rtype = cs0.slice(0, 4)
  val itype::vd_valid::decode_stop::decode_jump::mem_type_float::mem_type::mem_cmd::Nil = cs0.slice(4, cs0.length)

  def decode_rtype(x: Bits): Bool = x(1).toBool
  def active_rtype(x: Bits): Bool = x(0).toBool
  val rtype_vs :: rtype_vt :: rtype_vr :: rtype_vd :: Nil = rtype.map(x => decode_rtype(x))
  val vs_active :: vt_active :: vr_active :: vd_active :: Nil = rtype.map(x => active_rtype(x))

  val unmasked_valid_viu = io.imem.resp.valid && valid(6)
  val unmasked_valid_vau0 = io.imem.resp.valid && valid(5)
  val unmasked_valid_vau1 = io.imem.resp.valid && valid(4)
  val unmasked_valid_vau2 = io.imem.resp.valid && valid(3)
  val unmasked_valid_amo = io.imem.resp.valid && valid(2)
  val unmasked_valid_utld = io.imem.resp.valid && valid(1)
  val unmasked_valid_utst = io.imem.resp.valid && valid(0)

  io.vf.stop := io.vf.active && io.imem.resp.valid && decode_stop.toBool

  val vau1_rm = Bits(width = 3)
  val vau2_rm = Bits(width = 3)

  vau1_rm := io.imem.resp.bits.data(14,12)
  vau2_rm := io.imem.resp.bits.data(14,12)

  when (io.imem.resp.bits.data(14,12) === Bits("b111",3))
  {
    vau1_rm := Bits(0,3)
    vau2_rm := Bits(0,3)
  }

  val unmasked_valid =
    unmasked_valid_viu ||
    unmasked_valid_vau0 || unmasked_valid_vau1 || unmasked_valid_vau2 ||
    unmasked_valid_amo || unmasked_valid_utld || unmasked_valid_utst

  val imm = MuxLookup(
    itype, Bits(0,SZ_DATA), Array(
      imm_0 -> Bits(0,65),
      imm_I -> Cat(Bits(0,1),Fill(52,io.imem.resp.bits.data(31)),io.imem.resp.bits.data(31,20)),
      imm_S -> Cat(Bits(0,1),Fill(52,io.imem.resp.bits.data(31)),io.imem.resp.bits.data(31,25),io.imem.resp.bits.data(11,7)),
      imm_U -> Cat(Bits(0,1),Fill(32,io.imem.resp.bits.data(31)),io.imem.resp.bits.data(31,12),Bits(0,12))
    ))

  val cnt = Mux(io.vcmdq.cnt.valid, io.vcmdq.cnt.bits, Bits(0))
  val regid_xbase = (cnt >> UInt(3)) * io.vf.xstride
  val regid_fbase = ((cnt >> UInt(3)) * io.vf.fstride) + io.vf.xf_split

  io.vcmdq.cnt.ready := io.vf.active && io.ready && unmasked_valid && !io.decoded.vd_zero && !stall_issue
  io.aiw_cntb.valid := io.vf.active && io.ready && unmasked_valid && !io.decoded.vd_zero && !stall_issue
  io.aiw_cntb.bits := cnt

  io.issue_to_aiw.markLast := io.vf.stop
  io.issue_to_aiw.update_numCnt.valid := io.vf.active && io.ready && unmasked_valid && !io.decoded.vd_zero && !stall_issue
  io.issue_to_aiw.update_numCnt.bits := numCnt_rtag

  val valid_common = io.vf.active && io.aiw_cntb.ready && !stall_issue

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

  io.bhazard.r2wm := bhazard(6)
  io.bhazard.r1w1 := bhazard(5)
  io.bhazard.r2w1 := bhazard(4)
  io.bhazard.r3w1 := bhazard(3)
  io.bhazard.amo := bhazard(2)
  io.bhazard.utld := bhazard(1)
  io.bhazard.utst := bhazard(0)
  io.bhazard.vld := Bool(false)
  io.bhazard.vst := Bool(false)

  io.fn.vbr := Cat(viu_t0,viu_t1,viu_dw,viu_fp,viu_fn)
  io.fn.viu := Cat(viu_t0,viu_t1,viu_dw,viu_fp,viu_fn)
  io.fn.vau0 := Cat(vau0_dw,vau0_fn)
  io.fn.vau1 := Cat(vau1_fp,vau1_rm,vau1_fn)
  io.fn.vau2 := Cat(vau2_fp,vau2_rm,vau2_fn)

  val vs = Cat(rtype_vs, io.imem.resp.bits.data(19,15)) // rs1
  val vt = Cat(rtype_vt, io.imem.resp.bits.data(24,20)) // rs2
  val vr = Cat(rtype_vr, io.imem.resp.bits.data(31,27)) // rs3
  val vd = Cat(rtype_vd, io.imem.resp.bits.data(11, 7)) // rd

  val vs_m1 = Cat(Bits(0,1),vs(4,0)) - UInt(1,1)
  val vt_m1 = Cat(Bits(0,1),vt(4,0)) - UInt(1,1)
  val vr_m1 = Cat(Bits(0,1),vr(4,0)) - UInt(1,1)
  val vd_m1 = Cat(Bits(0,1),vd(4,0)) - UInt(1,1)

  mask_stall := decode_stop.toBool

  io.decoded.vlen := io.vf.vlen - cnt
  io.decoded.utidx := Mux(io.vcmdq.cnt.valid, io.vcmdq.cnt.bits, Bits(0))
  // nxregs counts the zero register so it "just works out"
  // io.decoded.vs_base := Mux(rtype_vs, vs_m1 + io.vf.nxregs, vs_m1)
  // io.decoded.vt_base := Mux(rtype_vt, vt_m1 + io.vf.nxregs, vt_m1)
  // io.decoded.vr_base := Mux(rtype_vr, vr_m1 + io.vf.nxregs, vr_m1)
  // io.decoded.vd_base := Mux(rtype_vd, vd_m1 + io.vf.nxregs, vd_m1)
  io.decoded.vs_base := Mux(rtype_vs, vs(4,0) + io.vf.xf_split, vs_m1)
  io.decoded.vt_base := Mux(rtype_vt, vt(4,0) + io.vf.xf_split, vt_m1)
  io.decoded.vr_base := Mux(rtype_vr, vr(4,0) + io.vf.xf_split, vr_m1)
  io.decoded.vd_base := Mux(rtype_vd, vd(4,0) + io.vf.xf_split, vd_m1)
  // io.decoded.vs := Mux(rtype_vs, vs_m1 + io.vf.nxregs, vs_m1) + regid_base
  // io.decoded.vt := Mux(rtype_vt, vt_m1 + io.vf.nxregs, vt_m1) + regid_base
  // io.decoded.vr := Mux(rtype_vr, vr_m1 + io.vf.nxregs, vr_m1) + regid_base
  // io.decoded.vd := Mux(rtype_vd, vd_m1 + io.vf.nxregs, vd_m1) + regid_base
  io.decoded.vs := Mux(rtype_vs, vs(4,0) + regid_fbase, vs_m1 + regid_xbase)
  io.decoded.vt := Mux(rtype_vt, vt(4,0) + regid_fbase, vt_m1 + regid_xbase)
  io.decoded.vr := Mux(rtype_vr, vr(4,0) + regid_fbase, vr_m1 + regid_xbase)
  io.decoded.vd := Mux(rtype_vd, vd(4,0) + regid_fbase, vd_m1 + regid_xbase)
  io.decoded.vs_zero := vs === Bits(0,6)
  io.decoded.vt_zero := vt === Bits(0,6)
  io.decoded.vr_zero := vr === Bits(0,6)
  io.decoded.vd_zero := vd === Bits(0,6) && vd_valid.toBool || mask_stall
  io.decoded.vs_active := vs_active
  io.decoded.vt_active := vt_active
  io.decoded.vr_active := vr_active
  io.decoded.vd_active := vd_active
  io.decoded.rtype := Cat(rtype_vs, rtype_vt, rtype_vr, rtype_vd)
  io.decoded.mem.cmd := mem_cmd
  io.decoded.mem.typ := mem_type
  io.decoded.mem.typ_float := mem_type_float.toBool
  io.decoded.imm := imm
  io.decoded.cnt_valid := io.vcmdq.cnt.valid
  io.decoded.cnt := cnt
  io.decoded.active_mask := Bool(true)
  io.decoded.mask := Fill(WIDTH_PVFB, Bits(1,1))
  io.decoded.pvfb_tag := Bits(0, SZ_PVFB_TAG)

  val illegal_vd = vd_active && (vd(4,0) >= io.vf.nfregs && rtype_vd || vd(4,0) >= io.vf.nxregs && !rtype_vd)
  val illegal_vt = vt_active && (vt(4,0) >= io.vf.nfregs && rtype_vt || vt(4,0) >= io.vf.nxregs && !rtype_vt)
  val illegal_vs = vs_active && (vs(4,0) >= io.vf.nfregs && rtype_vs || vs(4,0) >= io.vf.nxregs && !rtype_vs)
  val illegal_vr = vr_active && (vr(4,0) >= io.vf.nfregs && rtype_vr || vr(4,0) >= io.vf.nxregs && !rtype_vr)

  io.irq.ma_inst := io.vf.active && io.imem.resp.valid && io.imem.resp.bits.xcpt_ma
  io.irq.fault_inst := io.vf.active && io.imem.resp.valid && io.imem.resp.bits.xcpt_if
  io.irq.illegal := io.vf.active && io.imem.resp.valid && (!unmasked_valid && !mask_stall || illegal_vd || illegal_vt || illegal_vs || illegal_vr)
  io.irq.pc := io.imem.resp.bits.pc
}

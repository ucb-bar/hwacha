package hwacha

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import Commands._
import freechips.rocketchip.util._ //implicits for bitpats

class HwachaConfigIO(implicit p: Parameters) extends HwachaBundle()(p) with LaneParameters {
  val valid = Output(Bool())
  val morelax = Output(Bool())
  val unpred = Output(Bool())
  val lstrip = Output(UInt(bfLStrip.W))
  val lstride = Output(UInt(bLStride.W))
  val pstride = Output(UInt(bPredAddr.W))
  val vstride = new Bundle {
    val d = Output(UInt(bRFAddr.W))
    val w = Output(UInt(bRFAddr.W))
    val h = Output(UInt(bRFAddr.W))
  }
  val base = Output(new HwachaConfigRegBase())
  val id = Output(new HwachaConfigRegId())
}

class HwachaConfigRegBase(implicit p: Parameters) extends HwachaBundle()(p) with LaneParameters {
  val w = UInt(bRFAddr.W)
  val h = UInt(bRFAddr.W)
}

class HwachaConfigRegId(implicit p: Parameters) extends HwachaBundle()(p) {
  val vp = UInt(bfPRegs.W)
  val vd = UInt(bfVRegs.W)
  val vw = UInt(bfVRegs.W)
  val vh = UInt(bfVRegs.W)
}

class DecodeConfig(implicit p: Parameters) extends HwachaBundle()(p) {
  val nvvh = UInt(bfVRegs.W)
  val nvvw = UInt(bfVRegs.W)
  val nvp = UInt(bfPRegs.W)
  val nvvd = UInt(bfVRegs.W)
}
class CMDQIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val cmd = Decoupled(UInt(CMD_X.getWidth.W))
  val imm = Decoupled(UInt(regLen.W))
  val rd  = Decoupled(UInt(bSDest.W))
  val cnt = Decoupled(UInt(bMLVLen.W))
  val status = Decoupled(new freechips.rocketchip.rocket.MStatus())
}

class CMDQCounterIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val cmd = UInt(log2Up(confvcmdq.ncmd).W)
  val imm = UInt(log2Up(confvcmdq.nimm).W)
  val rd = UInt(log2Up(confvcmdq.nrd).W)
  val cnt = UInt(log2Up(confvcmdq.ncnt).W)
  val status = UInt(log2Up(confvcmdq.nstatus).W)
}

object QueueWithCount
{
  /** Create a queue and supply a DecoupledIO containing the product and the count. */
  def apply[T <: Data](
      enq: DecoupledIO[T],
      entries: Int = 2,
      pipe: Boolean = false,
      flow: Boolean = false): (DecoupledIO[T], UInt) = {
    val q = Module(new Queue(enq.bits.cloneType, entries, pipe, flow))
    q.io.enq.valid := enq.valid // not using <> so that override is allowed
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
    (q.io.deq, q.io.count)
  }
}

class CMDQ(resetSignal: Bool = null)(implicit p: Parameters) extends HwachaModule(_reset = resetSignal)(p) {
  val io = IO(new Bundle {
    val enq = Flipped(new CMDQIO())
    val deq = new CMDQIO()
    val counters = new CMDQCounterIO()
  })

  val (qcmd, ccmd) = QueueWithCount(io.enq.cmd, confvcmdq.ncmd)
  io.deq.cmd <> qcmd
  io.counters.cmd <> ccmd

  val (qimm, cimm) = QueueWithCount(io.enq.imm, confvcmdq.nimm)
  io.deq.imm <> qimm
  io.counters.imm <> cimm

  val (qrd, crd) = QueueWithCount(io.enq.rd, confvcmdq.nrd)
  io.deq.rd <> qrd
  io.counters.rd <> crd

  val (qcnt, ccnt) = QueueWithCount(io.enq.cnt, confvcmdq.ncnt)
  io.deq.cnt <> qcnt
  io.counters.cnt <> ccnt

  val (qstatus, cstatus) = QueueWithCount(io.enq.status, confvcmdq.nstatus)
  io.deq.status <> qstatus
  io.counters.status <> cstatus
}

class RoCCCounterIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val req = new CMDQCounterIO
  val resp = UInt(log2Up(2).W)
}

class RoCCCtrlSigs(implicit p: Parameters) extends HwachaBundle()(p) {
  val inst_val = Bool() // TODO: unused
  val inst_priv = Bool() // TODO: unused
  val enq_cmd_  = Bool()
  val sel_cmd = UInt(CMD_X.getWidth.W)
  val rd_type = UInt(VRT_X.getWidth.W)
  val sel_imm = UInt(RIMM_X.getWidth.W)
  val check_vl = Bool()
  val enq_rd_ = Bool()
  val enq_imm_ = Bool()
  val enq_vcnt_ = Bool() // TODO: unused
  val enq_status_ = Bool()
  val enq_resp_ = Bool()
  val sel_resp = UInt(RESP_X.getWidth.W)
  val decode_save = Bool() // TODO: unused
  val decode_rest = Bool() // TODO: unused
  val decode_kill = Bool() // TODO: unused

  def decode(inst: UInt) = {
    val decoder = freechips.rocketchip.rocket.DecodeLogic(inst, HwachaDecodeTable.default, HwachaDecodeTable.table)
    val sigs = Seq(inst_val, inst_priv, enq_cmd_, sel_cmd, rd_type, sel_imm,
        check_vl, enq_rd_, enq_imm_, enq_vcnt_, enq_status_, enq_resp_, sel_resp, decode_save,
        decode_rest, decode_kill)
    sigs zip decoder map {case(s,d) => s := d}

    this
  }
}

object HwachaDecodeTable {
  import HwachaInstructions._
  val default: List[BitPat] =
                // * means special case decode code below      checkvl?
                //     inst_val                                |                               save
                //     |  priv                                 | vrd?            resp?         | restore
                //     |  |  vmcd_val                          | | imm?          |             | |
                //     |  |  |  cmd          rtype  imm        | | | vcnt?       | resptype    | | kill
                //     |  |  |  |            |      |          | | | | status    | |           | | |
                  List(N, N, N, CMD_X,       VRT_X, RIMM_X,    N,N,N,N,N,        N,RESP_X,     N,N,N)
  val table: Array[(BitPat, List[BitPat])] = Array(
    // General instructions
    VSETCFG    -> List(Y, N, Y, CMD_VSETCFG, VRT_X, RIMM_VLEN, N,N,Y,N,N,    N,RESP_X,     N,N,N), //* set maxvl register
    VSETVL     -> List(Y, N, Y, CMD_VSETVL,  VRT_X, RIMM_VLEN, N,N,Y,N,N,    Y,RESP_NVL,   N,N,N), //* set vl register
    VGETCFG    -> List(Y, N, N, CMD_X,       VRT_X, RIMM_X,    N,N,N,N,N,    Y,RESP_CFG,   N,N,N),
    VGETVL     -> List(Y, N, N, CMD_X,       VRT_X, RIMM_X,    N,N,N,N,N,    Y,RESP_VL,    N,N,N),
    VF         -> List(Y, N, Y, CMD_VF,      VRT_X, RIMM_ADDR, Y,N,Y,N,Y,    N,RESP_X,     N,N,N),
    VFT        -> List(Y, N, Y, CMD_VFT,     VRT_X, RIMM_ADDR, Y,N,Y,N,Y,    N,RESP_X,     N,N,N),
    VMCA       -> List(Y, N, Y, CMD_VMCA,    VRT_A, RIMM_RS1,  N,Y,Y,N,N,    N,RESP_X,     N,N,N),
    VMCS       -> List(Y, N, Y, CMD_VMCS,    VRT_S, RIMM_RS1,  N,Y,Y,N,N,    N,RESP_X,     N,N,N),
    // Exception and save/restore instructions
    VXCPTCAUSE -> List(Y, Y, N, CMD_X,       VRT_X, RIMM_X,    N,N,N,N,N,    Y,RESP_CAUSE, N,N,N),
    VXCPTAUX   -> List(Y, Y, N, CMD_X,       VRT_X, RIMM_X,    N,N,N,N,N,    Y,RESP_AUX,   N,N,N),
    VXCPTSAVE  -> List(Y, Y, N, CMD_X,       VRT_X, RIMM_X,    N,N,N,N,N,    N,RESP_X,     Y,N,N),
    VXCPTRESTORE->List(Y, Y, N, CMD_X,       VRT_X, RIMM_X,    N,N,N,N,N,    N,RESP_X,     N,Y,N),
    VXCPTKILL  -> List(Y, Y, N, CMD_X,       VRT_X, RIMM_X,    N,N,N,N,N,    N,RESP_X,     N,N,Y)
  )
}

class RoCCUnit(implicit p: Parameters) extends HwachaModule()(p) with LaneParameters with MinMax{
  import HwachaDecodeTable._

  val io = new Bundle {
    val rocc = new freechips.rocketchip.tile.RoCCCoreIO

    val vf_active = Input(Bool())
    val pending = new Bundle {
      val mseq = Input(Bool())
      val mrt = Input(Bool())
    }

    val cfg = new HwachaConfigIO

    val cmdqs = new Bundle {
      val vu = new CMDQIO
      val vru = new CMDQIO
    }
    val counters = new RoCCCounterIO
  }

  // COLIN TODO FIXME: make these types play nice with chisel3
  //private def _reg[U <: Data](cond: Boolean, x: U)(y: U = x) =
    //if (cond) RegInit(x) else y

  // Configuration defaults
  val cfg_init = Wire(new DecodeConfig)
  cfg_init.nvvd := 256.I
  cfg_init.nvvw := 0.U
  cfg_init.nvvh := 0.U
  cfg_init.nvp := 16.U
  private val cfg_init_nvv_dw = cfg_init.nvvd + cfg_init.nvvw
  private val cfg_init_nvv_dwh = cfg_init_nvv_dw + cfg_init.nvvh

  // Configuration state
  val cfg_reg = RegInit(cfg_init)
  val cfg_reg_maxvl = RegInit(8.U(bMLVLen.W))
  val cfg_reg_vl = RegInit(0.U(bMLVLen.W))
  val cfg_reg_lstride = RegInit(0.U(bLStride.W))
  val cfg_reg_unpred = RegInit(false.B)
  val cfg_reg_nvv_dw = RegInit(cfg_init_nvv_dw)
  val cfg_reg_nvv_dwh = RegInit(cfg_init_nvv_dwh)
  val cfg_reg_base = if (confprec) RegInit(0.U.asTypeOf(new HwachaConfigRegBase())) else 0.U.asTypeOf(new HwachaConfigRegBase())

  io.cfg.morelax := false.B
  io.cfg.unpred := cfg_reg_unpred
  io.cfg.lstrip := UInt(nStrip) << io.cfg.lstride
  io.cfg.lstride := cfg_reg_lstride
  io.cfg.pstride := cfg_reg.nvp
  io.cfg.vstride.d := (if (confprec) cfg_reg.nvvd else cfg_reg_nvv_dwh)
  io.cfg.vstride.w := cfg_reg.nvvw
  io.cfg.vstride.h := cfg_reg.nvvh
  // Base physical reg addr for each type
  io.cfg.base := cfg_reg_base
  // Upper bound of register ids for each type
  io.cfg.id.vp := cfg_reg.nvp
  io.cfg.id.vd := cfg_reg.nvvd
  io.cfg.id.vw := cfg_reg_nvv_dw
  io.cfg.id.vh := cfg_reg_nvv_dwh

  // Decode
  val rocc_inst = io.rocc.cmd.bits.inst.asUInt
  val rocc_imm12 = rocc_inst(31, 20)
  val rocc_split_imm12 = Cat(rocc_inst(31, 25), rocc_inst(11, 7))
  val rocc_rd = rocc_inst(11, 7)
  val rocc_srd = Cat(rocc_inst(22, 20), rocc_rd)

  val ctrl = Wire(new RoCCCtrlSigs()).decode(rocc_inst)

  val flush_kill = this.reset
  val cmdq = Module(new CMDQ(resetSignal = flush_kill))
  cmdq.suggestName("cmdqInst")

  // TODO: probably want to change the length of queues in here
  val vrucmdq = Module(new CMDQ(resetSignal = flush_kill))
  vrucmdq.suggestName("vrucmdqInst")
  val respq = Module(new Queue(io.rocc.resp.bits, 2))
  respq.suggestName("respqInst")
  val vru_enable = RegInit(false.B)

  val decode_vsetcfg = ctrl.enq_cmd_ && (ctrl.sel_cmd === CMD_VSETCFG)
  val decode_vsetvl = ctrl.enq_cmd_ && (ctrl.sel_cmd === CMD_VSETVL)
  val decode_vf = ctrl.enq_cmd_ && (ctrl.sel_cmd === CMD_VF || ctrl.sel_cmd === CMD_VFT)

  val keepcfg = Wire(Bool())
  val mask_vsetcfg = !decode_vsetcfg || !keepcfg

  val mask_vl = !ctrl.check_vl || (cfg_reg_vl =/= 0.U)
  val enq_cmd = mask_vl && ctrl.enq_cmd_
  val enq_imm = mask_vl && ctrl.enq_imm_
  val enq_rd = mask_vl && ctrl.enq_rd_
  val enq_cnt = false.B
  val enq_status = mask_vl && ctrl.enq_status_
  val enq_resp = mask_vl && ctrl.enq_resp_

  val vru_insts_wanted = !(ctrl.sel_cmd === CMD_VMCS)
  val vru_enq_cmd = enq_cmd && vru_insts_wanted
  val vru_enq_imm = enq_imm && vru_insts_wanted
  val vru_enq_rd = enq_rd && vru_insts_wanted
  val vru_enq_cnt = false.B
  val vru_enq_status = enq_status && vru_insts_wanted

  val mask_vxu_cmd_ready = !enq_cmd || cmdq.io.enq.cmd.ready
  val mask_vxu_imm_ready = !enq_imm || cmdq.io.enq.imm.ready
  val mask_vxu_rd_ready = !enq_rd || cmdq.io.enq.rd.ready
  val mask_vxu_cnt_ready = !enq_cnt || cmdq.io.enq.cnt.ready
  val mask_vxu_status_ready = !enq_status || cmdq.io.enq.status.ready
  val mask_resp_ready = !enq_resp || respq.io.enq.ready

  val mask_vru_cmd_ready = !vru_enq_cmd || vrucmdq.io.enq.cmd.ready || !vru_enable
  val mask_vru_imm_ready = !vru_enq_imm || vrucmdq.io.enq.imm.ready || !vru_enable
  val mask_vru_rd_ready = !vru_enq_rd || vrucmdq.io.enq.rd.ready || !vru_enable
  val mask_vru_cnt_ready = !vru_enq_cnt || vrucmdq.io.enq.cnt.ready || !vru_enable
  val mask_vru_status_ready = !vru_enq_status || vrucmdq.io.enq.status.ready || !vru_enable

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      mask_vsetcfg,
      io.rocc.cmd.valid,
      mask_vxu_cmd_ready,
      mask_vxu_imm_ready,
      mask_vxu_rd_ready,
      mask_vxu_cnt_ready,
      mask_vxu_status_ready,
      mask_resp_ready,
      mask_vru_cmd_ready,
      mask_vru_imm_ready,
      mask_vru_rd_ready,
      mask_vru_cnt_ready,
      mask_vru_status_ready
    )
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }
  val _fire = fire(null)
  val fire_vsetcfg = _fire && decode_vsetcfg
  val fire_vsetvl = _fire && decode_vsetvl
  val fire_vf = _fire && decode_vf

  // Logic to handle vector length calculation
  val cfg = (rocc_imm12 | io.rocc.cmd.bits.rs1).asTypeOf(new DecodeConfig)
  val cfg_nvv_dw = cfg.nvvd + cfg.nvvw
  val cfg_nvv_dwh = cfg_nvv_dw + cfg.nvvh
  val cfg_nvv = if (confprec) ((cfg.nvvd << 2.U) + (cfg.nvvw << 1.U) + cfg.nvvh) else cfg_nvv_dwh

  // vector length lookup
  val lookup_tbl_nvv = (0 to (nVRegs << bPack)).toArray map { n =>
    (n.U, (if (n < 2) (nSRAM) else math.min((nSRAM << bPack) / n, maxVLen),
      log2Down(nSRAM)+bPack+1).U) }
  val lookup_tbl_nvp = (0 to nPRegs).toArray map { n =>
    (n.U, (if (n < 2) (nPred) else (nPred / n), log2Down(nPred)+1).U) }

  // epb: elements per bank
  val epb_nvv = MuxLookup(cfg_nvv, lookup_tbl_nvv.last._2)(lookup_tbl_nvv)
  val epb_nvp = MuxLookup(cfg.nvp, lookup_tbl_nvp.last._2)(lookup_tbl_nvp) << bPack
  val epb_base = min(epb_nvv, epb_nvp)
  val (epb, cfg_lstride) = if (confprec) {
    val sel = Seq(cfg.nvvh =/= 0.U, cfg.nvvw =/= 0.U)
    def lookup(fn: Int => Int) =
      MuxCase(fn(0).U, sel.zip(sel.size until 0 by -1).map {
        case (s, i) => s -> fn(i).U
      })
    val mask = lookup(i => (1 << i) - 1)
    val n = mask.getWidth
    val _epb = Cat(epb_base >> n.U, epb_base(n-1, 0) & ~mask)
    (_epb, lookup(i => i))
  } else (epb_base, cfg_reg_lstride)

  val cfg_maxvl = Cat(epb, 0.U((bLanes + bBanks + bSlices).W))
  val cfg_vl = min(cfg_reg_maxvl, io.rocc.cmd.bits.rs1)(bMLVLen-1, 0)

  val cfg_busy = if (confprec) RegInit(false.B) else false.B
  if (confprec) {
    val state = Reg(UInt())
    val s1 :: s2 :: Nil = Enum(2)

    val _epb = Reg(UInt())
    val vregs = Mux(state === s1, cfg_reg.nvvd, cfg_reg.nvvw)
    val vspan = _epb * vregs

    when (fire_vsetcfg) {
      cfg_busy := true.B
      state := s1
      _epb := epb
    }
    when (cfg_busy) {
      switch (state) {
        is (s1) {
          state := s2
          cfg_reg_base.w := vspan
        }
        is (s2) {
          cfg_busy := false.B
          cfg_reg_base.h := cfg_reg_base.w + Ceil(vspan, 1)
        }
      }
    }
  }
  io.cfg.valid := !cfg_busy

  when (fire_vsetcfg) {
    cfg_reg := cfg
    cfg_reg_maxvl := cfg_maxvl
    cfg_reg_vl := 0.U
    cfg_reg_lstride := cfg_lstride
    cfg_reg_unpred := (cfg.nvp === 0.U)
    cfg_reg_nvv_dw := cfg_nvv_dw
    cfg_reg_nvv_dwh := cfg_nvv_dwh
    val vru_switch_on = io.rocc.cmd.bits.rs1(63).asBool
    printf("H: VSETCFG[nlanes=%d][nvvd=%d][nvvw=%d][nvvh=%d][nvp=%d][lstride=%d][epb_nvv=%d][epb_nvp=%d][maxvl=%d][vru_enable=%d]\n",
      nLanes.U, cfg.nvvd, cfg.nvvw, cfg.nvvh, cfg.nvp, cfg_lstride, epb_nvv, epb_nvp, cfg_maxvl, vru_switch_on)
    vru_enable := vru_switch_on
  }

  val ignore_dup_vsetvl = Bool(p(HwachaVSETVLCompress)) && (decode_vsetvl && (cfg_reg_vl === cfg_vl))

  when (fire_vsetvl && !ignore_dup_vsetvl) {
    cfg_reg_vl := cfg_vl
    printf("H: VSETVL[maxvl=%d][vl=%d]\n",
      cfg_reg_maxvl, cfg_vl)
  } .elsewhen (fire_vsetvl && ignore_dup_vsetvl) {
    printf("H: IGNORED REPEAT VSETVL[maxvl=%d][vl=%d]\n",
      cfg_reg_maxvl, cfg_vl)
  }

  // Hookup ready port of RoCC cmd queue
  //COLIN FIXME: we use the exception flag to set a sticky bit that causes to always be ready after exceptions
  io.rocc.cmd.ready := fire(io.rocc.cmd.valid)
  cmdq.io.enq.cmd.valid := fire(mask_vxu_cmd_ready, enq_cmd, !ignore_dup_vsetvl)
  cmdq.io.enq.imm.valid := fire(mask_vxu_imm_ready, enq_imm, !ignore_dup_vsetvl)
  cmdq.io.enq.rd.valid := fire(mask_vxu_rd_ready, enq_rd, !ignore_dup_vsetvl)
  cmdq.io.enq.cnt.valid := fire(mask_vxu_cnt_ready, enq_cnt, !ignore_dup_vsetvl)
  cmdq.io.enq.status.valid := fire(mask_vxu_status_ready, enq_status, !ignore_dup_vsetvl)
  respq.io.enq.valid := fire(mask_resp_ready, enq_resp)

  vrucmdq.io.enq.cmd.valid := fire(mask_vru_cmd_ready, vru_enq_cmd, !ignore_dup_vsetvl, vru_enable)
  vrucmdq.io.enq.imm.valid := fire(mask_vru_imm_ready, vru_enq_imm, !ignore_dup_vsetvl, vru_enable)
  vrucmdq.io.enq.rd.valid := fire(mask_vru_rd_ready, vru_enq_rd, !ignore_dup_vsetvl, vru_enable)
  vrucmdq.io.enq.cnt.valid := fire(mask_vru_cnt_ready, vru_enq_cnt, !ignore_dup_vsetvl, vru_enable)
  vrucmdq.io.enq.status.valid := fire(mask_vru_status_ready, vru_enq_status, !ignore_dup_vsetvl, vru_enable)

  // cmdq dpath
  val cmd_out = ctrl.sel_cmd
  val imm_out =
    MuxLookup(ctrl.sel_imm, 0.U, Array(
      RIMM_VLEN -> cfg_vl,
      RIMM_RS1  -> io.rocc.cmd.bits.rs1,
      RIMM_ADDR -> (io.rocc.cmd.bits.rs1.zext + rocc_split_imm12.asSInt).asUInt
    ))
  val rd_out = Mux(ctrl.rd_type === VRT_S, rocc_srd, rocc_rd)
  cmdq.io.enq.cmd.bits := cmd_out
  cmdq.io.enq.imm.bits := imm_out
  cmdq.io.enq.rd.bits := rd_out
  cmdq.io.enq.status.bits := io.rocc.cmd.bits.status

  vrucmdq.io.enq.cmd.bits := cmd_out
  vrucmdq.io.enq.imm.bits := imm_out
  vrucmdq.io.enq.rd.bits := rd_out
  vrucmdq.io.enq.status.bits := io.rocc.cmd.bits.status


  // respq dpath
  respq.io.enq.bits.data :=
    MuxLookup(ctrl.sel_resp, 0.U, Array(
      RESP_NVL -> cfg_vl,
      RESP_CFG -> cfg_reg.asUInt,
      RESP_VL  -> cfg_reg_vl
    ))
  respq.io.enq.bits.rd := io.rocc.cmd.bits.inst.rd

  // hookup output ports
  io.cmdqs.vu.cmd <> cmdq.io.deq.cmd
  io.cmdqs.vu.imm <> cmdq.io.deq.imm
  io.cmdqs.vu.rd <> cmdq.io.deq.rd
  io.cmdqs.vu.status <> cmdq.io.deq.status

  io.rocc.resp <> respq.io.deq

  io.cmdqs.vru.cmd <> vrucmdq.io.deq.cmd
  io.cmdqs.vru.imm <> vrucmdq.io.deq.imm
  io.cmdqs.vru.rd <> vrucmdq.io.deq.rd
  io.cmdqs.vru.status <> vrucmdq.io.deq.status

  io.counters.req <> cmdq.io.counters
  io.counters.resp := respq.io.count

 // COLIN FIXME: update keepcfg
  keepcfg :=
    cmdq.io.deq.cmd.valid ||
    io.vf_active || io.pending.mseq ||
    cfg_busy

  // Busy signal for fencing
  val busy =
    cmdq.io.deq.cmd.valid ||
    io.vf_active || io.pending.mseq || io.pending.mrt

  io.rocc.busy := busy

  // Setup interrupt
  io.rocc.interrupt := false.B


  //COLIN FIXME: do we need to do something on the rocc.s field that hold used to do
  /*
  val reg_hold = RegInit(false.B)
  when (rocc_valid && decl_hold && construct_ready(null)) { reg_hold := true.B }
  when (reg_hold && !io.rocc.s) { reg_hold := false.B }

  io.xcpt.rocc.exception := io.rocc.exception
  io.xcpt.rocc.evac := rocc_valid && decl_evac && construct_ready(null)
  io.xcpt.rocc.evac_addr := io.rocc.cmd.bits.rs1
  io.xcpt.rocc.hold := reg_hold
  io.xcpt.rocc.kill := rocc_valid && decl_kill && construct_ready(null)
  */

}

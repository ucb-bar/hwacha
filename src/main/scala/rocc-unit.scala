package hwacha

import Chisel._
import cde.Parameters
import Commands._

class HwachaConfigIO(implicit p: Parameters) extends HwachaBundle()(p) with LaneParameters {
  val morelax = Bool(OUTPUT)
  val unpred = Bool(OUTPUT)
  val lstrip = UInt(OUTPUT, bfLStrip)
  val lstride = UInt(OUTPUT, bLStride)
  val pstride = UInt(OUTPUT, bPredAddr)
  val vstride = new Bundle {
    val d = UInt(OUTPUT, bRFAddr)
    val w = UInt(OUTPUT, bRFAddr)
    val h = UInt(OUTPUT, bRFAddr)
  }
  val vbase = new HwachaConfigBase().asOutput
  val vident = new HwachaConfigIdent().asOutput
}
class HwachaConfigBase(implicit p: Parameters) extends HwachaBundle()(p) with LaneParameters {
  val w = UInt(width = bRFAddr)
  val h = UInt(width = bRFAddr)
}
class HwachaConfigIdent(implicit p: Parameters) extends HwachaBundle()(p) {
  val d = UInt(width = bfVRegs)
  val dw = UInt(width = bfVRegs)
}

class DecodeConfig(implicit p: Parameters) extends HwachaBundle()(p) {
  val nvvh = UInt(width = bfVRegs)
  val nvvw = UInt(width = bfVRegs)
  val nvp = UInt(width = bfPRegs)
  val nvvd = UInt(width = bfVRegs)
}
class CMDQIO(implicit p: Parameters) extends HwachaBundle()(p) {
  val cmd = Decoupled(Bits(width = CMD_X.getWidth))
  val imm = Decoupled(Bits(width = regLen))
  val rd  = Decoupled(Bits(width = bSDest))
  val cnt = Decoupled(Bits(width = bMLVLen))
}

class CMDQ(resetSignal: Bool = null)(implicit p: Parameters) extends HwachaModule(_reset = resetSignal)(p) {
  val io = new Bundle {
    val enq = new CMDQIO().flip
    val deq = new CMDQIO()
  }

  io.deq.cmd <> Queue(io.enq.cmd, confvcmdq.ncmd)
  io.deq.imm <> Queue(io.enq.imm, confvcmdq.nimm)
  io.deq.rd <> Queue(io.enq.rd, confvcmdq.nrd)
  io.deq.cnt <> Queue(io.enq.cnt, confvcmdq.ncnt)
}

object HwachaDecodeTable extends HwachaDecodeConstants {
  import HwachaInstructions._
  val default: List[BitPat] =
                // * means special case decode code below     checkvl?             
                //     inst_val                               |                         save
                //     |  priv                                | vrd?      resp?         | restore
                //     |  |  vmcd_val                         | | imm?    |             | |
                //     |  |  |  cmd          rtype  imm       | | | vcnt? | resptype    | | kill
                //     |  |  |  |            |      |         | | | |     | |           | | |
                  List(N, N, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    N,RESP_X,     N,N,N)
  val table: Array[(BitPat, List[BitPat])] = Array(
    // General instructions
    VSETCFG    -> List(Y, N, Y, CMD_VSETCFG, VRT_X, IMM_VLEN, N,N,Y,N,    N,RESP_X,     N,N,N), //* set maxvl register
    VSETVL     -> List(Y, N, Y, CMD_VSETVL,  VRT_X, IMM_VLEN, N,N,Y,N,    Y,RESP_NVL,   N,N,N), //* set vl register
    VGETCFG    -> List(Y, N, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    Y,RESP_CFG,   N,N,N),
    VGETVL     -> List(Y, N, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    Y,RESP_VL,    N,N,N),
    VF         -> List(Y, N, Y, CMD_VF,      VRT_X, IMM_ADDR, Y,N,Y,N,    N,RESP_X,     N,N,N),
    VFT        -> List(Y, N, Y, CMD_VFT,     VRT_X, IMM_ADDR, Y,N,Y,N,    N,RESP_X,     N,N,N),
    VMCA       -> List(Y, N, Y, CMD_VMCA,    VRT_A, IMM_RS1,  N,Y,Y,N,    N,RESP_X,     N,N,N),
    VMCS       -> List(Y, N, Y, CMD_VMCS,    VRT_S, IMM_RS1,  N,Y,Y,N,    N,RESP_X,     N,N,N),
    // Exception and save/restore instructions
    VXCPTCAUSE -> List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    Y,RESP_CAUSE, N,N,N),
    VXCPTAUX   -> List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    Y,RESP_AUX,   N,N,N),
    VXCPTSAVE  -> List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    N,RESP_X,     Y,N,N),
    VXCPTRESTORE->List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    N,RESP_X,     N,Y,N),
    VXCPTKILL  -> List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    N,RESP_X,     N,N,Y)
  )
}

class RoCCUnit(implicit p: Parameters) extends HwachaModule()(p) with LaneParameters with MinMax{
  import HwachaDecodeTable._

  val io = new Bundle {
    val rocc = new rocket.RoCCInterface

    val vf_active = Bool(INPUT)
    val pending = new Bundle {
      val mseq = Bool(INPUT)
      val mrt = Bool(INPUT)
    }

    val cfg = new HwachaConfigIO

    val cmdqs = new Bundle {
      val vu = new CMDQIO
      val vru = new CMDQIO
    }
  }

  private def _reg[U <: Data](cond: Boolean, x: U)(y: U = x) =
    if (cond) Reg(outType=None, next=None, init=Some(x), clock=None) else y

  // Configuration defaults
  val cfg_init = new DecodeConfig
  cfg_init.nvvd := UInt(256)
  cfg_init.nvvw := UInt(0)
  cfg_init.nvvh := UInt(0)
  cfg_init.nvp := UInt(16)
  private val cfg_init_nvvdw = cfg_init.nvvd + cfg_init.nvvw
  private val cfg_init_nvv = cfg_init_nvvdw + cfg_init.nvvh

  // Configuration state
  val cfg_reg = Reg(init=cfg_init)
  val cfg_reg_maxvl = Reg(init=UInt(8, bMLVLen))
  val cfg_reg_vl = Reg(init=UInt(0, bMLVLen))
  val cfg_reg_lstride = Reg(init=UInt(0, bLStride))
  val cfg_reg_unpred = Reg(init=Bool(false))
  val cfg_reg_nvvdw = Reg(init=cfg_init_nvvdw)
  val cfg_reg_vbase = _reg(confprec, new HwachaConfigBase().fromBits(Bits(0)))()
  val cfg_reg_vstride = _reg(!confprec, cfg_init_nvv)(cfg_reg.nvvd)

  io.cfg.morelax := Bool(false)
  io.cfg.unpred := cfg_reg_unpred
  io.cfg.lstrip := UInt(nStrip) << io.cfg.lstride
  io.cfg.lstride := cfg_reg_lstride
  io.cfg.pstride := cfg_reg.nvp
  io.cfg.vstride.d := cfg_reg_vstride
  io.cfg.vstride.w := cfg_reg.nvvw
  io.cfg.vstride.h := cfg_reg.nvvh
  io.cfg.vbase := cfg_reg_vbase
  io.cfg.vident.d := cfg_reg.nvvd
  io.cfg.vident.dw := cfg_reg_nvvdw

  // Decode
  val rocc_inst = io.rocc.cmd.bits.inst.toBits
  val rocc_imm12 = rocc_inst(31, 20)
  val rocc_split_imm12 = Cat(rocc_inst(31, 25), rocc_inst(11, 7))
  val rocc_rd = rocc_inst(11, 7)
  val rocc_srd = Cat(rocc_inst(22, 20), rocc_rd)

  val logic = rocket.DecodeLogic(rocc_inst, HwachaDecodeTable.default, HwachaDecodeTable.table)
  val cs = logic.map {
    case b if b.inputs.head.getClass == classOf[Bool] => b.toBool
    case u => u
  }

  val flush_kill = this.reset 
  val cmdq = Module(new CMDQ(resetSignal = flush_kill))

  // TODO: probably want to change the length of queues in here
  val vrucmdq = Module(new CMDQ(resetSignal = flush_kill))
  val respq = Module(new Queue(io.rocc.resp.bits, 2))
  val vru_enable = Reg(init=Bool(false))

  val (inst_val: Bool) :: (inst_priv: Bool) :: (enq_cmd_ : Bool) :: sel_cmd :: rd_type :: sel_imm :: cs0 = cs
  val (check_vl: Bool) :: (enq_rd_ : Bool) :: (enq_imm_ : Bool) :: (enq_vcnt_ : Bool) :: cs1 = cs0
  val (enq_resp_ : Bool) :: sel_resp :: (decode_save: Bool) :: (decode_rest: Bool) :: (decode_kill: Bool) :: Nil = cs1

  val stall_hold = Reg(init=Bool(false))
  val stall_vsetcfg = Bool()
  val stall = stall_hold || stall_vsetcfg

  val decode_vsetcfg = enq_cmd_ && (sel_cmd === CMD_VSETCFG)
  val decode_vsetvl = enq_cmd_ && (sel_cmd === CMD_VSETVL)

  val keepcfg = Bool()
  val mask_vsetcfg = !decode_vsetcfg || !keepcfg

  val mask_vl = !check_vl || (cfg_reg_vl =/= UInt(0))
  val enq_cmd = mask_vl && enq_cmd_
  val enq_imm = mask_vl && enq_imm_
  val enq_rd = mask_vl && enq_rd_
  val enq_cnt = Bool(false)
  val enq_resp = mask_vl && enq_resp_

  val vru_insts_wanted = !(sel_cmd === CMD_VMCS)
  val vru_enq_cmd = enq_cmd && vru_insts_wanted
  val vru_enq_imm = enq_imm && vru_insts_wanted
  val vru_enq_rd = enq_rd && vru_insts_wanted
  val vru_enq_cnt = Bool(false)

  val mask_vxu_cmd_ready = !enq_cmd || cmdq.io.enq.cmd.ready
  val mask_vxu_imm_ready = !enq_imm || cmdq.io.enq.imm.ready
  val mask_vxu_rd_ready = !enq_rd || cmdq.io.enq.rd.ready
  val mask_vxu_cnt_ready = !enq_cnt || cmdq.io.enq.cnt.ready
  val mask_resp_ready = !enq_resp || respq.io.enq.ready

  val mask_vru_cmd_ready = !vru_enq_cmd || vrucmdq.io.enq.cmd.ready || !vru_enable
  val mask_vru_imm_ready = !vru_enq_imm || vrucmdq.io.enq.imm.ready || !vru_enable
  val mask_vru_rd_ready = !vru_enq_rd || vrucmdq.io.enq.rd.ready || !vru_enable
  val mask_vru_cnt_ready = !vru_enq_cnt || vrucmdq.io.enq.cnt.ready || !vru_enable

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      !stall, mask_vsetcfg,
      io.rocc.cmd.valid,
      mask_vxu_cmd_ready, 
      mask_vxu_imm_ready, 
      mask_vxu_rd_ready, 
      mask_vxu_cnt_ready, 
      mask_resp_ready,
      mask_vru_cmd_ready, 
      mask_vru_imm_ready, 
      mask_vru_rd_ready, 
      mask_vru_cnt_ready
    )
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }
  val _fire = fire(null)
  val fire_vsetcfg = _fire && decode_vsetcfg
  val fire_vsetvl = _fire && decode_vsetvl

  // Logic to handle vector length calculation
  val cfg = new DecodeConfig().fromBits(rocc_imm12 | io.rocc.cmd.bits.rs1)
  val cfg_nvvdw = cfg.nvvd + cfg.nvvw
  val cfg_nvv = cfg.nvvh + (
    if (confprec) (cfg.nvvd << UInt(2)) + (cfg.nvvw << UInt(1))
    else cfg_nvvdw)

  // vector length lookup
  val lookup_tbl_nvv = (0 to (nVRegs << bPack)).toArray map { n =>
    (UInt(n), UInt(if (n < 2) (nSRAM) else math.min((nSRAM << bPack) / n, maxVLen),
      width = log2Down(nSRAM)+bPack+1)) }
  val lookup_tbl_nvp = (0 to nPRegs).toArray map { n =>
    (UInt(n), UInt(if (n < 2) (nPred) else (nPred / n), width = log2Down(nPred)+1)) }

  // epb: elements per bank
  val epb_nvv = Lookup(cfg_nvv, lookup_tbl_nvv.last._2, lookup_tbl_nvv)
  val epb_nvp = Lookup(cfg.nvp, lookup_tbl_nvp.last._2, lookup_tbl_nvp) << UInt(bPack)
  val epb_base = min(epb_nvv, epb_nvp)
  val (epb, cfg_lstride) = if (confprec) {
    val sel = Seq(cfg.nvvh =/= UInt(0), cfg.nvvw =/= UInt(0))
    def lookup(fn: Int => Int) =
      MuxCase(UInt(fn(0)), sel.zip(sel.size until 0 by -1).map {
        case (s, i) => s -> UInt(fn(i))
      })
    val mask = lookup(i => (1 << i) - 1)
    val n = mask.getWidth
    val _epb = Cat(epb_base >> UInt(n), epb_base(n-1, 0) & ~mask)
    (_epb, lookup(i => i))
  } else (epb_base, cfg_reg_lstride)

  val cfg_maxvl = Cat(epb, UInt(0, bLanes + bBanks + bSlices))
  val cfg_vl = min(cfg_reg_maxvl, io.rocc.cmd.bits.rs1)(bMLVLen-1, 0)

  stall_vsetcfg := Bool(false)
  if (confprec) {
    val busy = Reg(init = Bool(false))
    val state = Reg(UInt())
    val s1 :: s2 :: Nil = Enum(UInt(), 2)

    val _epb = Reg(UInt())
    val vregs = Mux(state === s1, cfg_reg.nvvd, cfg_reg.nvvw)
    val vspan = _epb * vregs

    when (fire_vsetcfg) {
      busy := Bool(true)
      state := s1
      _epb := epb
    }
    when (busy) {
      stall_vsetcfg := decode_vsetcfg
      switch (state) {
        is (s1) {
          state := s2
          cfg_reg_vbase.w := vspan
        }
        is (s2) {
          busy := Bool(false)
          cfg_reg_vbase.h := cfg_reg_vbase.w + Ceil(vspan, 1)
        }
      }
    }
  }

  when (fire_vsetcfg) {
    cfg_reg := cfg
    cfg_reg_maxvl := cfg_maxvl
    cfg_reg_vl := UInt(0)
    cfg_reg_lstride := cfg_lstride
    cfg_reg_unpred := (cfg.nvp === UInt(0))
    cfg_reg_nvvdw := cfg_nvvdw
    if (!confprec) cfg_reg_vstride := cfg_nvv
    val vru_switch_on = Bool(io.rocc.cmd.bits.rs1(63))
    printf("H: VSETCFG[nlanes=%d][nvvd=%d][nvvw=%d][nvvh=%d][nvp=%d][lstride=%d][epb_nvv=%d][epb_nvp=%d][maxvl=%d][vru_enable=%d]\n",
      UInt(nLanes), cfg.nvvd, cfg.nvvw, cfg.nvvh, cfg.nvp, cfg_lstride, epb_nvv, epb_nvp, cfg_maxvl, vru_switch_on)
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
  respq.io.enq.valid := fire(mask_resp_ready, enq_resp)

  vrucmdq.io.enq.cmd.valid := fire(mask_vru_cmd_ready, vru_enq_cmd, !ignore_dup_vsetvl, vru_enable)
  vrucmdq.io.enq.imm.valid := fire(mask_vru_imm_ready, vru_enq_imm, !ignore_dup_vsetvl, vru_enable)
  vrucmdq.io.enq.rd.valid := fire(mask_vru_rd_ready, vru_enq_rd, !ignore_dup_vsetvl, vru_enable)
  vrucmdq.io.enq.cnt.valid := fire(mask_vru_cnt_ready, vru_enq_cnt, !ignore_dup_vsetvl, vru_enable)

  // cmdq dpath
  val cmd_out = sel_cmd
  val imm_out = 
    MuxLookup(sel_imm, Bits(0), Array(
      IMM_VLEN -> cfg_vl,
      IMM_RS1  -> io.rocc.cmd.bits.rs1,
      IMM_ADDR -> (io.rocc.cmd.bits.rs1 + rocc_split_imm12.toSInt).toUInt
    ))
  val rd_out = Mux(rd_type === VRT_S, rocc_srd, rocc_rd)
  cmdq.io.enq.cmd.bits := cmd_out
  cmdq.io.enq.imm.bits := imm_out
  cmdq.io.enq.rd.bits := rd_out

  vrucmdq.io.enq.cmd.bits := cmd_out
  vrucmdq.io.enq.imm.bits := imm_out
  vrucmdq.io.enq.rd.bits := rd_out


  // respq dpath
  respq.io.enq.bits.data :=
    MuxLookup(sel_resp, Bits(0), Array(
      RESP_NVL -> cfg_vl,
      RESP_CFG -> cfg_reg.toBits,
      RESP_VL  -> cfg_reg_vl
    ))
  respq.io.enq.bits.rd := io.rocc.cmd.bits.inst.rd

  // hookup output ports
  io.cmdqs.vu.cmd <> cmdq.io.deq.cmd
  io.cmdqs.vu.imm <> cmdq.io.deq.imm
  io.cmdqs.vu.rd <> cmdq.io.deq.rd

  io.rocc.resp <> respq.io.deq

  io.cmdqs.vru.cmd <> vrucmdq.io.deq.cmd
  io.cmdqs.vru.imm <> vrucmdq.io.deq.imm
  io.cmdqs.vru.rd <> vrucmdq.io.deq.rd

 // COLIN FIXME: update keepcfg
  keepcfg :=
    cmdq.io.deq.cmd.valid ||
    io.vf_active || io.pending.mseq

  // Busy signal for fencing
  val busy =
    cmdq.io.deq.cmd.valid ||
    io.vf_active || io.pending.mseq || io.pending.mrt

  io.rocc.busy := busy

  // Setup interrupt
  io.rocc.interrupt := Bool(false)


  //COLIN FIXME: do we need to do something on the rocc.s field that hold used to do
  /*
  val reg_hold = Reg(init=Bool(false))
  when (rocc_valid && decl_hold && construct_ready(null)) { reg_hold := Bool(true) }
  when (reg_hold && !io.rocc.s) { reg_hold := Bool(false) }

  io.xcpt.rocc.exception := io.rocc.exception
  io.xcpt.rocc.evac := rocc_valid && decl_evac && construct_ready(null)
  io.xcpt.rocc.evac_addr := io.rocc.cmd.bits.rs1
  io.xcpt.rocc.hold := reg_hold
  io.xcpt.rocc.kill := rocc_valid && decl_kill && construct_ready(null)
  */

}

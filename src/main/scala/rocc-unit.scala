package hwacha

import Chisel._
import Commands._

class HwachaConfigIO extends HwachaBundle with LaneParameters {
  val vstride = UInt(OUTPUT, bRFAddr)
}

class CMDQIO extends HwachaBundle {
  val cmd = Decoupled(Bits(width = CMD_X.getWidth))
  val imm = Decoupled(Bits(width = regLen))
  val rd  = Decoupled(Bits(width = bSDest))
  val cnt = Decoupled(Bits(width = bVLen))
}

class CMDQ(resetSignal: Bool = null) extends HwachaModule(_reset = resetSignal) {
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
                // * means special case decode code below     checkvl?             
                //     inst_val                               |                         save
                //     |  priv                                | vrd?      resp?         | restore
                //     |  |  vmcd_val                         | | imm?    |             | |
                //     |  |  |  cmd          rtype  imm       | | | vcnt? | resptype    | | kill
                //     |  |  |  |            |      |         | | | |     | |           | | |
  val default =   List(N, N, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    N,RESP_X,     N,N,N)
  val table = Array(
    // General instructions
    VSETCFG    -> List(Y, N, Y, CMD_VSETCFG, VRT_X, IMM_VLEN, N,N,Y,N,    N,RESP_X,     N,N,N), //* set maxvl register
    VSETVL     -> List(Y, N, Y, CMD_VSETVL,  VRT_X, IMM_VLEN, N,N,Y,N,    Y,RESP_NVL,   N,N,N), //* set vl register
    VGETCFG    -> List(Y, N, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    Y,RESP_CFG,   N,N,N),
    VGETVL     -> List(Y, N, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    Y,RESP_VL,    N,N,N),
    VF         -> List(Y, N, Y, CMD_VF,      VRT_X, IMM_ADDR, Y,N,Y,N,    N,RESP_X,     N,N,N),
    VFT        -> List(Y, N, Y, CMD_VFT,     VRT_X, IMM_ADDR, Y,N,Y,N,    N,RESP_X,     N,N,N),
    VMSA       -> List(Y, N, Y, CMD_VMSA,    VRT_A, IMM_RS1,  N,Y,Y,N,    N,RESP_X,     N,N,N),
    VMSS       -> List(Y, N, Y, CMD_VMSS,    VRT_S, IMM_RS1,  N,Y,Y,N,    N,RESP_X,     N,N,N),
    // Exception and save/restore instructions
    VXCPTCAUSE -> List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    Y,RESP_CAUSE, N,N,N),
    VXCPTAUX   -> List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    Y,RESP_AUX,   N,N,N),
    VXCPTSAVE  -> List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    N,RESP_X,     Y,N,N),
    VXCPTRESTORE->List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    N,RESP_X,     N,Y,N),
    VXCPTKILL  -> List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    N,RESP_X,     N,N,Y)
  )
}

class RoCCUnit extends HwachaModule with LaneParameters {
  import HwachaDecodeTable._

  val io = new Bundle {
    val rocc = new rocket.RoCCInterface

    val vf_active = Bool(INPUT)
    val pending_seq = Bool(INPUT)
    val pending_memop = Bool(INPUT)

    val cfg = new HwachaConfigIO

    val cmdq = new CMDQIO
  }

  // Cofiguration state
  val cfg_maxvl = Reg(init=UInt(8, bVLen))
  val cfg_vl = Reg(init=UInt(0, bVLen))
  val cfg_vregs = Reg(init=UInt(256, bVRegs))
  val cfg_pregs = Reg(init=UInt(1, bPRegs))
  io.cfg.vstride := cfg_vregs

  // Decode
  val raw_inst = io.rocc.cmd.bits.inst.toBits
  val inst_i_imm = raw_inst(31, 20)
  val logic = rocket.DecodeLogic(raw_inst, HwachaDecodeTable.default, HwachaDecodeTable.table)
  val cs = logic.map {
    case b if b.inputs.head.getClass == classOf[Bool] => b.toBool
    case u => u
  }

  val flush_kill = this.reset 
  val cmdq = Module(new CMDQ(resetSignal = flush_kill))

  val respq = Module(new Queue(io.rocc.resp.bits, 2))

  val (inst_val: Bool) :: (inst_priv: Bool) :: (enq_cmd_ : Bool) :: sel_cmd :: rd_type :: sel_imm :: cs0 = cs
  val (check_vl: Bool) :: (enq_rd_ : Bool) :: (enq_imm_ : Bool) :: (enq_vcnt_ : Bool) :: cs1 = cs0
  val (enq_resp_ : Bool) :: sel_resp :: (decode_save: Bool) :: (decode_rest: Bool) :: (decode_kill: Bool) :: Nil = cs1

  val stall_hold = Reg(init=Bool(false))
  val stall = stall_hold

  val decode_vcfg = enq_cmd_ && (sel_cmd === CMD_VSETCFG)
  val decode_vsetvl = enq_cmd_ && (sel_cmd === CMD_VSETVL)

  val keepcfg = Bool()
  val mask_vcfg = !decode_vcfg || !keepcfg

  val mask_vl = !check_vl || cfg_vl != UInt(0)
  val enq_cmd = mask_vl && enq_cmd_
  val enq_imm = mask_vl && enq_imm_
  val enq_rd = mask_vl && enq_rd_
  val enq_cnt = Bool(false)
  val enq_resp = mask_vl && enq_resp_

  val mask_cmd_ready = !enq_cmd || cmdq.io.enq.cmd.ready
  val mask_imm_ready = !enq_imm || cmdq.io.enq.imm.ready
  val mask_rd_ready = !enq_rd || cmdq.io.enq.rd.ready
  val mask_cnt_ready = !enq_cnt || cmdq.io.enq.cnt.ready
  val mask_resp_ready = !enq_resp || respq.io.enq.ready

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(
      !stall, mask_vcfg,
      io.rocc.cmd.valid,
      mask_cmd_ready, mask_imm_ready, mask_rd_ready, mask_cnt_ready, mask_resp_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  // Logic to handle vector length calculation
  val nvpr = (io.rocc.cmd.bits.rs1(bVRegs-2, 0) + inst_i_imm(bVRegs-2, 0)) + UInt(1,bVRegs)
  val nppr = (io.rocc.cmd.bits.rs1(bVRegs+bPRegs-3, bVRegs-1) + inst_i_imm(11, bVRegs-1)) + UInt(1,bPRegs)

  // vector length lookup
  val rom_allocation_units = (0 to nVRegs).toArray.map(n => (UInt(n),
    UInt(if (n < 2) (nSRAM) else (nSRAM / n), width = log2Down(nSRAM)+1)
  ))

  val elems_per_bank = Lookup(nvpr, rom_allocation_units.last._2, rom_allocation_units)
  val new_maxvl = elems_per_bank * UInt(nBanks) * UInt(nSlices)
  val new_vl = Mux(io.rocc.cmd.bits.rs1 < cfg_maxvl, io.rocc.cmd.bits.rs1, cfg_maxvl)(bVLen-1, 0)

  when (fire(null, decode_vcfg)) {
    cfg_maxvl := new_maxvl
    cfg_vl := UInt(0)
    cfg_vregs := nvpr
    cfg_pregs := nppr
  }
  when (fire(null, decode_vsetvl)) {
    cfg_vl := new_vl
  }

  // Hookup ready port of RoCC cmd queue
  //COLIN FIXME: we use the exception flag to set a sticky bit that causes to always be ready after exceptions
  io.rocc.cmd.ready := fire(io.rocc.cmd.valid)
  cmdq.io.enq.cmd.valid := fire(mask_cmd_ready, enq_cmd)
  cmdq.io.enq.imm.valid := fire(mask_imm_ready, enq_imm)
  cmdq.io.enq.rd.valid := fire(mask_rd_ready, enq_rd)
  cmdq.io.enq.cnt.valid := fire(mask_cnt_ready, enq_cnt)
  respq.io.enq.valid := fire(mask_resp_ready, enq_resp)

  // cmdq/cmd dpath
  cmdq.io.enq.cmd.bits := sel_cmd

  // cmdq/imm dpath
  val imm_vlen = new VCFG().fromBits(Cat(nppr, nvpr, new_vl))
  val vf_immediate = Cat(raw_inst(31,25),raw_inst(11,7)).toSInt
  val imm_addr = (io.rocc.cmd.bits.rs1 + vf_immediate).toUInt

  cmdq.io.enq.imm.bits :=
    MuxLookup(sel_imm, Bits(0), Array(
    IMM_VLEN -> imm_vlen.toBits(),
      IMM_RS1  -> io.rocc.cmd.bits.rs1,
      IMM_ADDR -> imm_addr
    ))

  // cmdq/rd dpath
  val rd = Mux(rd_type === VRT_S, Cat(raw_inst(22,20), raw_inst(11,7)), raw_inst(11,7))

  cmdq.io.enq.rd.bits := rd

  // respq dpath
  respq.io.enq.bits.data :=
    MuxLookup(sel_resp, Bits(0), Array(
      RESP_NVL -> new_vl,
      RESP_CFG -> Cat(cfg_pregs, cfg_vregs),
      RESP_VL  -> cfg_vl
    ))

  respq.io.enq.bits.rd := io.rocc.cmd.bits.inst.rd

  // hookup output ports
  io.cmdq.cmd <> cmdq.io.deq.cmd
  io.cmdq.imm <> cmdq.io.deq.imm
  io.cmdq.rd <> cmdq.io.deq.rd
  io.rocc.resp <> respq.io.deq

  // COLIN FIXME: update keepcfg
  keepcfg :=
    cmdq.io.deq.cmd.valid ||
    io.vf_active || io.pending_seq

  // Busy signal for fencing
  val busy =
    cmdq.io.deq.cmd.valid ||
    io.vf_active || io.pending_seq || io.pending_memop

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

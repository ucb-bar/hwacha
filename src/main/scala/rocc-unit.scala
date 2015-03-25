package hwacha

import Chisel._
import Node._
import Constants._

class RESPQIO extends Bundle
{
  val value = Decoupled(Bits(width = SZ_ADDR))
}

class RESPQ(resetSignal: Bool = null) extends HwachaModule(_reset = resetSignal)
{
  val io = new Bundle {
    val enq = new RESPQIO().flip
    val deq = new RESPQIO()
  }

  io.deq.value <> Queue(io.enq.value, confvcmdq.ncmd)
}

class CMDQIO extends Bundle
{
  val cmd = Decoupled(Bits (width = 8))
  val imm = Decoupled(Bits(width = SZ_ADDR))
  val rd  = Decoupled(Bits(width = SZ_REGCNT))
  val cnt = Decoupled(new HwachaCnt)
}

class CMDQ(resetSignal: Bool = null) extends HwachaModule(_reset = resetSignal)
{
  val io = new Bundle {
    val enq = new CMDQIO().flip
    val deq = new CMDQIO()
  }

  io.deq.cmd <> Queue(io.enq.cmd, confvcmdq.ncmd)
  io.deq.imm <> Queue(io.enq.imm, confvcmdq.nimm)
  io.deq.rd  <> Queue(io.enq.rd,  confvcmdq.nrd)
  io.deq.cnt <> Queue(io.enq.cnt, confvcmdq.ncnt)
}

object HwachaDecodeTable extends HwachaDecodeConstants
{
  import Commands._
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
    VMSA       -> List(Y, N, Y, CMD_VMSA,    VRT_A, IMM_RS1,  Y,Y,Y,N,    N,RESP_X,     N,N,N),
    VMSS       -> List(Y, N, Y, CMD_VMSS,    VRT_S, IMM_RS1,  Y,Y,Y,N,    N,RESP_X,     N,N,N),
    // Exception and save/restore instructions
    VXCPTCAUSE -> List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    Y,RESP_CAUSE, N,N,N),
    VXCPTAUX   -> List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    Y,RESP_AUX,   N,N,N),
    VXCPTSAVE  -> List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    N,RESP_X,     Y,N,N),
    VXCPTRESTORE->List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    N,RESP_X,     N,Y,N),
    VXCPTKILL  -> List(Y, Y, N, CMD_X,       VRT_X, IMM_X,    N,N,N,N,    N,RESP_X,     N,N,Y)
  )
}

class RoCC extends HwachaModule 
{
  import Commands._
  import HwachaDecodeTable._

  val io = new Bundle {
    val rocc = new rocket.RoCCInterface

    val vf_active = Bool(INPUT)
    val pending_seq = Bool(INPUT)
    val pending_memop = Bool(INPUT)

    val respq = new RESPQIO().flip

    val cmdq = new CMDQIO
  }

  //tie rocc resp to respq
  io.respq.value.ready := Bool(true)
  io.rocc.resp.bits.data := io.respq.value.bits

  //tie down unused rocc ports for now
  io.rocc.dmem.acquire.valid := Bool(false)
  io.rocc.dmem.grant.ready := Bool(true)
  io.rocc.dmem.finish.valid := Bool(false)
  io.rocc.dptw.req.valid := Bool(false)
  io.rocc.pptw.req.valid := Bool(false)

  io.rocc.mem.req.valid := Bool(false)

  // Cofiguration state
  val cfg_maxvl = Reg(init=UInt(32, log2Up(nreg_total)+1))
  val cfg_vl    = Reg(init=UInt(0, log2Up(nreg_total)+1))
  val cfg_regs  = Reg(init=Cat(UInt(1, 4), UInt(32, 8)))
  val cfg_vregs = cfg_regs(7,0)
  val cfg_pregs = cfg_regs(11,8)
  val cfg_vstride = Reg(init = Bits(31,SZ_REGLEN))
  val cfg_precision = Reg(init = PREC_DOUBLE)

  // Decode
  val raw_inst = io.rocc.cmd.bits.inst.toBits
  val inst_i_imm = raw_inst(31, 20)
  val logic = rocket.DecodeLogic(raw_inst, HwachaDecodeTable.default, HwachaDecodeTable.table)
  val cs = logic.map {
    case b if b.inputs.head.getClass == classOf[Bool] => b.toBool
    case u => u
  }

  val resp_q = Module( new Queue(io.rocc.resp.bits, 2) )
  resp_q.io.deq <> io.rocc.resp

  val (inst_val: Bool) :: (inst_priv: Bool) :: (emit_cmd: Bool) :: sel_cmd :: rd_type :: sel_imm :: cs0 = cs
  val (check_vl: Bool) :: (emit_rd: Bool) :: (emit_imm: Bool) :: (emit_vcnt: Bool) :: cs1 = cs0
  val (emit_response: Bool) :: sel_resp :: (decl_save: Bool) :: (decl_restore: Bool) :: (decl_kill: Bool) :: Nil = cs1

  val stall_hold = Reg(init=Bool(false))
  val stall = stall_hold


  val decode_vcfg   = emit_cmd && (sel_cmd === CMD_VSETCFG)
  val decode_vsetvl = emit_cmd && (sel_cmd === CMD_VSETVL)
  val decode_vf     = emit_cmd && (sel_cmd === CMD_VF || sel_cmd === CMD_VFT)
  val keepcfg = Bool()
  val mask_vcfg = !decode_vcfg || !keepcfg

  val cmd_valid = inst_val && io.rocc.cmd.valid && (!check_vl || cfg_vl != UInt(0)) && mask_vcfg


  def construct_ready(exclude: Bool): Bool = {
    val all_readies = Array(
      !stall, resp_ready,
      cmd_ready, imm_ready, rd_ready, vcnt_ready)
    all_readies.filter(_ != exclude).reduce(_&&_)
  }

  val reg_prec = Reg(init = PREC_DOUBLE)
  val next_prec = Bits(width = SZ_PREC)

  val prec = (io.rocc.cmd.bits.rs1(13,12)).zext.toUInt
  if (confprec) {
    next_prec := MuxLookup(prec, PREC_DOUBLE, Array(
      UInt(0,2) -> PREC_DOUBLE,
      UInt(1,2) -> PREC_SINGLE,
      UInt(2,2) -> PREC_HALF
    ))
  } else {
    next_prec := PREC_DOUBLE
  }

  // Logic to handle vector length calculation
  val nvpr = (io.rocc.cmd.bits.rs1( 7,0) + inst_i_imm( 7,0)).zext.toUInt
  val nppr = (io.rocc.cmd.bits.rs1(11,8) + inst_i_imm(11,8)).zext.toUInt

  val packing = MuxLookup(next_prec, UInt(0), Array(
    PREC_DOUBLE -> UInt(0),
    PREC_SINGLE -> UInt(1),
    PREC_HALF   -> UInt(2)
  ))

  // vector length lookup
  val regs_used = (Mux(nvpr === UInt(0), UInt(0), nvpr - UInt(1)) << packing)
  val vlen_width = log2Up(nreg_per_bank + 1)
  val rom_allocation_units = (0 to 256).toArray.map(n => (UInt(n),
    UInt(if (n < 2) (nreg_per_bank) else (nreg_per_bank / n), width = vlen_width)
  ))

  val ut_per_bank = Lookup(regs_used, rom_allocation_units.last._2, rom_allocation_units) << packing
  val new_maxvl = ut_per_bank << UInt(3) // microthreads
  val xf_split = (new_maxvl >> UInt(3)) * (nvpr - UInt(1))

  val new_vl = Mux(io.rocc.cmd.bits.rs1 < cfg_maxvl, io.rocc.cmd.bits.rs1, cfg_maxvl)

  val imm_vlen = Cat(UInt(0,18), next_prec, UInt(8,4), SInt(-1,8), nppr(3,0), nvpr(7,0), new_vl(SZ_VLEN-1,0))
  when (cmd_valid && emit_cmd && construct_ready(null)) {
    switch (sel_cmd) {
      is (CMD_VSETCFG) {
        cfg_maxvl := new_maxvl
        cfg_vl := UInt(0)
        cfg_regs := Cat(nppr(3,0),nvpr(7,0))
        reg_prec := next_prec
      }
      is (CMD_VSETVL) {
        cfg_vl := new_vl
      }
    }
  }

  // Calculate the vf address
  val vf_immediate = Cat(raw_inst(31,25),raw_inst(11,7)).toSInt
  val imm_addr = (io.rocc.cmd.bits.rs1 + vf_immediate).toUInt
  debug(vf_immediate)
  debug(imm_addr)

  // Hookup ready port of RoCC cmd queue
  //COLIN FIXME: we use the exception flag to set a sticky bit that causes to always be ready after exceptions
  io.rocc.cmd.ready := mask_vcfg && construct_ready(null)

  /////////////////////////////
  //Command Queue and Counters
  /////////////////////////////
  val flush_kill = this.reset 
  val cmdq = Module(new CMDQ(resetSignal = flush_kill))
  val cmdq_enq = new CMDQIO().flip

  // counters
  val cmdqcnt = new {
    val cmd = Module(new QCounter(confvcmdq.ncmd, confvcmdq.ncmd, resetSignal = flush_kill))
    val imm = Module(new QCounter(confvcmdq.nimm, confvcmdq.nimm, resetSignal = flush_kill))
    val rd  = Module(new QCounter(confvcmdq.nrd, confvcmdq.nrd, resetSignal = flush_kill))
  }

  //COLIN FIXME: mask_vcfg means we have a valid cmd if we are not updating the config or we don't need to keep the cfg because we are in the middle of vf block
  //COLIN FIXME: does vl need to be set to run scalar instructions?
  val resp_ready  = !emit_response || resp_q.io.enq.ready
  val cmdq_user_ready = cmdqcnt.cmd.io.watermark 
  val cmd_ready  = !emit_cmd  || Mux(io.rocc.s, cmdq_enq.cmd.ready, cmdq_user_ready)
  val immq_user_ready = cmdqcnt.imm.io.watermark 
  val imm_ready = !emit_imm || Mux(io.rocc.s, cmdq_enq.imm.ready, immq_user_ready)
  val rdq_user_ready = cmdqcnt.rd.io.watermark 
  val rd_ready = !emit_rd || Mux(io.rocc.s, cmdq_enq.rd.ready, rdq_user_ready)
  val vcnt_ready  = !emit_vcnt  || cmdq_enq.cnt.ready

  // Hookup cmdq.cmd
  cmdq_enq.cmd.bits := sel_cmd
  cmdq_enq.cmd.valid := cmd_valid && emit_cmd && construct_ready(cmd_ready)
  cmdq.io.enq.cmd <> MaskStall(cmdq_enq.cmd, Bool(false))

  io.cmdq.cmd <> cmdq.io.deq.cmd
  
  // Hookup cmdq.imm
  debug(sel_imm)
  debug(imm_vlen)
  cmdq_enq.imm.bits := MuxLookup(sel_imm, imm_vlen, Array(
    IMM_VLEN -> imm_vlen,
    IMM_RS1  -> io.rocc.cmd.bits.rs1,
    IMM_ADDR -> imm_addr
  ))
  //val cmdq_enq.imm.bits = cmdq_enq_imm_bits
  cmdq_enq.imm.valid := cmd_valid && emit_imm && construct_ready(imm_ready)
  cmdq.io.enq.imm <> MaskStall(cmdq_enq.imm, Bool(false))

  io.cmdq.imm <> cmdq.io.deq.imm

  // Hookup cmdq.rd
  val rd = Mux(rd_type===VRT_S, Cat(raw_inst(22,20),raw_inst(11,7)), raw_inst(11,7))
  cmdq_enq.rd.bits := rd
  cmdq_enq.rd.valid := cmd_valid && emit_rd && construct_ready(rd_ready)
  cmdq.io.enq.rd <> MaskStall(cmdq_enq.rd, Bool(false))

  io.cmdq.rd <> cmdq.io.deq.rd


  ////////////////////////
  //Command Queue Counters
  ////////////////////////
  cmdqcnt.cmd.io.dec := cmdq.io.enq.cmd.ready && cmdq_enq.cmd.valid
  cmdqcnt.cmd.io.inc := io.cmdq.cmd.ready && cmdq.io.deq.cmd.valid
  cmdqcnt.imm.io.dec := cmdq.io.enq.imm.ready && cmdq_enq.imm.valid
  cmdqcnt.imm.io.inc := io.cmdq.imm.ready && cmdq.io.deq.imm.valid

  cmdqcnt.cmd.io.qcnt := UInt(confvcmdq.ncmd - nbanks)
  cmdqcnt.imm.io.qcnt := UInt(confvcmdq.nimm - nbanks)

  //COLIN FIXME: update keepcfg
  keepcfg :=
    cmdq.io.deq.cmd.valid ||
    io.vf_active || io.pending_seq

  val busy =
    cmdq.io.deq.cmd.valid ||
    io.vf_active || io.pending_memop

  // Hookup resp queue
  resp_q.io.enq.valid := cmd_valid && emit_response && construct_ready(resp_ready)
  resp_q.io.enq.bits.data := MuxLookup(sel_resp, new_vl, Array(
    RESP_NVL   -> new_vl,
    RESP_CFG   -> cfg_regs,
    RESP_VL    -> cfg_vl
  ))
  resp_q.io.enq.bits.rd := io.rocc.cmd.bits.inst.rd

  // Busy signal for fencing
  io.rocc.busy := busy || cmd_valid

  // Setup interrupt
  io.rocc.interrupt := Bool(false)


  //COLIN FIXME: do we need to do something on the rocc.s field that hold used to do
  /*
  val reg_hold = Reg(init=Bool(false))
  when (cmd_valid && decl_hold && construct_ready(null)) { reg_hold := Bool(true) }
  when (reg_hold && !io.rocc.s) { reg_hold := Bool(false) }

  io.xcpt.rocc.exception := io.rocc.exception
  io.xcpt.rocc.evac := cmd_valid && decl_evac && construct_ready(null)
  io.xcpt.rocc.evac_addr := io.rocc.cmd.bits.rs1
  io.xcpt.rocc.hold := reg_hold
  io.xcpt.rocc.kill := cmd_valid && decl_kill && construct_ready(null)
  */

}

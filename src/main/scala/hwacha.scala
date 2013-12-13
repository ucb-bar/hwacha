package hwacha

import Chisel._
import uncore._

case class HwachaConfiguration(icache: rocket.ICacheConfig, nbanks: Int, nreg_per_bank: Int, ndtlb: Int, nptlb: Int)
{
  val nreg_total = nbanks * nreg_per_bank
  val vru = true
  val fma = true
  val confprec = true

  // rocket pipeline latencies
  val dfma_stages = 4
  val sfma_stages = 3

  // pipeline latencies
  val int_stages = 2
  val imul_stages = 4
  val fma_stages = 3
  val fconv_stages = 3

  val shift_buf_read = 3
  val shift_buf_write = fma_stages + 4

  val vcmdq = new {
    val ncmd = 19
    val nimm1 = 19
    val nimm2 = 17
    val ncnt = 8
  }

  val nvvaq = 16
  val nvpaq = 16
  val nvpfvaq = 16
  val nvpfpaq = 16
  val nvldq = 128
  val nvsdq = 16
  val nvpasdq = 31
  val nvsreq = 31
  val nvlreq = nvldq
}

trait HwachaDecodeConstants
{
  val Y = Bool(true)
  val N = Bool(false)
  val X = Bits("b?", 1)

  val VRT_X = Bits("b?", 1)
  val VRT_S = Bits(0, 1)
  val VRT_F = Bits(1, 1)

  val VR_X   = Bits("b?", 1)
  val VR_RS1 = Bits(0, 1)
  val VR_RD  = Bits(1, 1)

  val VIMM_X    = Bits("b???",3)
  val VIMM_VLEN = Bits(0,3)
  val VIMM_RS1  = Bits(1,3)
  val VIMM_RS2  = Bits(2,3)
  val VIMM_ADDR = Bits(3,3)
  val VIMM_PREC = Bits(4,3)
}

object HwachaDecodeTable extends HwachaDecodeConstants
{
  import HwachaInstructions._
  import Commands._
                //                                                               vcmd?
                //                                                               | vimm1?
                //   val                                                         | | vimm2?
                //   |  vcmd         regtype reg1   reg2    imm1      imm2       | | | response?
                //   |  |            |       |      |       |         |          | | | |
  val default = List(N, CMD_X,       VRT_X,  VR_X,  VR_X,   VIMM_X,   VIMM_X,    N,N,N,N)
  val table = Array( 
    // Configurable precision instructions
    VSETPREC->  List(Y, CMD_VSETPREC,VRT_X,  VR_X,  VR_X,   VIMM_PREC,VIMM_X,    Y,Y,N,N),
    // General instructions
    VSETCFG ->  List(Y, CMD_VVCFGIVL,VRT_X,  VR_X,  VR_X,   VIMM_VLEN,VIMM_X,    Y,Y,N,N),
    VSETVL  ->  List(Y, CMD_VSETVL,  VRT_X,  VR_X,  VR_X,   VIMM_VLEN,VIMM_X,    Y,Y,N,Y),
    VF      ->  List(Y, CMD_VF,      VRT_X,  VR_X,  VR_X,   VIMM_ADDR,VIMM_X,    Y,Y,N,N),
    VMVV    ->  List(Y, CMD_VMVV,    VRT_S,  VR_RD, VR_RS1, VIMM_X,   VIMM_X,    Y,N,N,N),
    VMSV    ->  List(Y, CMD_VMSV,    VRT_S,  VR_RD, VR_RS1, VIMM_RS1, VIMM_X,    Y,Y,N,N),
    // Memory load/stores (x-registers)
    VLD     ->  List(Y, CMD_VLD,     VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VLW     ->  List(Y, CMD_VLW,     VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VLWU    ->  List(Y, CMD_VLWU,    VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VLH     ->  List(Y, CMD_VLH,     VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VLHU    ->  List(Y, CMD_VLHU,    VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VLB     ->  List(Y, CMD_VLB,     VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VLBU    ->  List(Y, CMD_VLBU,    VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VSD     ->  List(Y, CMD_VSD,     VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VSW     ->  List(Y, CMD_VSW,     VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VSH     ->  List(Y, CMD_VSH,     VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VSB     ->  List(Y, CMD_VSB,     VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    // Memory load/stores (fp-registers)
    VFLD     ->  List(Y, CMD_VFLD,   VRT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VFLW     ->  List(Y, CMD_VFLW,   VRT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VFSD     ->  List(Y, CMD_VFSD,   VRT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    VFSW     ->  List(Y, CMD_VFSW,   VRT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,N,N),
    // Memory strided load/stores (x-registers)
    VLSTD   ->  List(Y, CMD_VLSTD,   VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VLSTW   ->  List(Y, CMD_VLSTW,   VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VLSTWU  ->  List(Y, CMD_VLSTWU,  VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VLSTH   ->  List(Y, CMD_VLSTH,   VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VLSTHU  ->  List(Y, CMD_VLSTHU,  VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VLSTB   ->  List(Y, CMD_VLSTB,   VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VLSTBU  ->  List(Y, CMD_VLSTBU,  VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VSSTD   ->  List(Y, CMD_VSSTD,   VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VSSTW   ->  List(Y, CMD_VSSTW,   VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VSSTH   ->  List(Y, CMD_VSSTH,   VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VSSTB   ->  List(Y, CMD_VSSTB,   VRT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    // Memory strided load/stores (fp-registers)
    VFLSTD   ->  List(Y, CMD_VFLSTD, VRT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VFLSTW   ->  List(Y, CMD_VFLSTW, VRT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VFSSTD   ->  List(Y, CMD_VFSSTD, VRT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N),
    VFSSTW   ->  List(Y, CMD_VFSSTW, VRT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,N)
  )
}

class Hwacha(hc: HwachaConfiguration, rc: rocket.RocketConfiguration) extends rocket.RoCC(rc) {
  import HwachaDecodeTable._
  import Commands._
  
  implicit val conf = hc

  val icache = Module(new rocket.Frontend()(hc.icache, rc.tl))
  val dtlb = Module(new rocket.TLB(hc.ndtlb))
  val ptlb = Module(new rocket.TLB(hc.nptlb))
  val vu = Module(new vu)
  
  // Decode
  val raw_inst = io.cmd.bits.inst.toBits
  val inst_i_imm = raw_inst(31, 20)
  val logic = rocket.DecodeLogic(raw_inst, HwachaDecodeTable.default, HwachaDecodeTable.table)
  val cs = logic.map {
    case b if b.inputs.head.getClass == classOf[Bool] => b.toBool
    case u => u 
  }

  val resp_q = Module( new Queue(io.resp.bits, 2) )
  resp_q.io.deq <> io.resp

  val (inst_val: Bool) :: sel_vcmd :: sel_vrtype :: sel_vr1 :: sel_vr2 :: sel_vimm1 :: sel_vimm2 :: (emit_vcmd: Bool) :: (emit_vimm1: Bool) :: (emit_vimm2: Bool) :: (emit_response: Bool) :: Nil = cs

  val cmd_valid = inst_val && io.cmd.valid
  val resp_ready  = !emit_response || resp_q.io.enq.ready
  val vcmd_ready  = !emit_vcmd  || vu.io.vcmdq_user_ready
  val vimm1_ready = !emit_vimm1 || vu.io.vimm1q_user_ready
  val vimm2_ready = !emit_vimm2 || vu.io.vimm2q_user_ready

  def construct_ready(exclude: Bool): Bool = {
    val all_readies = Array(resp_ready, vcmd_ready, vimm1_ready, vimm2_ready)
    all_readies.filter(_ != exclude).reduce(_&&_)
  }

  // Connect supporting Hwacha memory modules to external ports
  io.imem <> icache.io.mem
  io.iptw <> icache.io.cpu.ptw
  io.dptw <> dtlb.io.ptw
  io.pptw <> ptlb.io.ptw

  // Connect VU to I$
  icache.io.cpu <> vu.io.imem

  // Connect VU to D$
  io.mem.req.bits.data := RegEnable(vu.io.dmem_req.bits.data, vu.io.dmem_req.valid && isWrite(vu.io.dmem_req.bits.cmd))
  io.mem.req <> vu.io.dmem_req
  vu.io.dmem_resp := io.mem.resp

  // Connect VU to DTLB and PTLB
  vu.io.vtlb <> dtlb.io
  vu.io.vpftlb <> ptlb.io

  // Busy signal for fencing TODO: CONNECT
  io.busy := vu.io.busy

  // TODO: SETUP PREFETCH QUEUES
  // TODO: SETUP INTERRUPT
  io.interrupt := Bool(false)

  // Logic to handle vector length calculation
  val cfg_maxvl = Reg(init=UInt(32, log2Up(hc.nreg_total)+1))

  val nxpr = (io.cmd.bits.rs1( 5,0) + inst_i_imm( 5,0)).zext.toUInt
  val nfpr = (io.cmd.bits.rs1(11,6) + inst_i_imm(11,6)).zext.toUInt

  //val new_maxvl = Mux(nxpr+nfpr < UInt(2), UInt(hc.nreg_per_bank), UInt(hc.nreg_per_bank) / (nxpr-UInt(1) + nfpr)) << UInt(3)
  // ROM implementation of above function
  /*val def_nut_per_bank = UInt(hc.nreg_per_bank/(64-1))
  val rom_nut_per_bank = Vec.tabulate(64){n => UInt(
    if(n<2) (hc.nreg_per_bank) else (hc.nreg_per_bank/(n-1))
  )}
  val new_maxvl = Mux((nxpr+nfpr)>=UInt(64), def_nut_per_bank, rom_nut_per_bank((nxpr+nfpr)(5,0))) << UInt(3)
  */
  // Alternative ROM implementation that actually works
  val rom_nut_per_bank = (0 to 64).toArray.map(n => (UInt(n),
    UInt(if(n<2) (hc.nreg_per_bank) else (hc.nreg_per_bank/(n-1)), width=log2Up(hc.nreg_per_bank+1))
  ))
  val new_maxvl = Lookup(nxpr+nfpr, rom_nut_per_bank.last._2, rom_nut_per_bank) << UInt(3)

  val new_vl = Mux(io.cmd.bits.rs1 < cfg_maxvl, io.cmd.bits.rs1, cfg_maxvl)
  val new_vl_m1 = Mux(sel_vcmd===CMD_VVCFGIVL, UInt(0), new_vl - UInt(1)) // translate into form for vcmdq

  val vimm_vlen = Cat(UInt(0,29), UInt(8,4), SInt(-1,8), nfpr(5,0), nxpr(5,0), new_vl_m1(10,0))
  when(cmd_valid && sel_vcmd === CMD_VVCFGIVL && construct_ready(null)) { cfg_maxvl := new_maxvl }

  val prec = (inst_i_imm(11,5)).toUInt
  val vimm_prec = UInt(width = 64)
  if (conf.confprec) {
    vimm_prec := Cat(UInt(0,59), prec(6,0))
  } else {
    vimm_prec := UInt(64, 64)
  }

  // Calculate the vf address
  val vf_immediate = Cat(raw_inst(31,25),raw_inst(11,7)).toSInt
  val vimm_addr = io.cmd.bits.rs1 + vf_immediate

  // Hookup ready port of cmd queue
  io.cmd.ready := construct_ready(null)

  // Hookup vcmdq.cmd
  val vr1 = Mux(sel_vr1===VR_RS1, raw_inst(19,15), raw_inst(11,7))
  val vr2 = Mux(sel_vr2===VR_RS1, raw_inst(19,15), raw_inst(11,7))
  val construct_vcmd = Cat(sel_vcmd, sel_vrtype, vr1, sel_vrtype, vr2)

  vu.io.vcmdq.cmd.valid := cmd_valid && emit_vcmd && construct_ready(vcmd_ready) 
  vu.io.vcmdq.cmd.bits := construct_vcmd
  
  // Hookup vcmdq.imm1
  vu.io.vcmdq.imm1.valid := cmd_valid && emit_vimm1 && construct_ready(vimm1_ready) 
  vu.io.vcmdq.imm1.bits := MuxLookup(sel_vimm1, vimm_vlen, Array(
    VIMM_VLEN -> vimm_vlen,
    VIMM_RS1  -> io.cmd.bits.rs1,
    VIMM_RS2  -> io.cmd.bits.rs2,
    VIMM_ADDR -> vimm_addr,
    VIMM_PREC -> vimm_prec
  ))

  // Hookup vcmdq.imm2
  vu.io.vcmdq.imm2.valid := cmd_valid && emit_vimm2 && construct_ready(vimm2_ready) 
  vu.io.vcmdq.imm2.bits := MuxLookup(sel_vimm2, vimm_vlen, Array(
    VIMM_VLEN -> vimm_vlen,
    VIMM_RS1  -> io.cmd.bits.rs1,
    VIMM_RS2  -> io.cmd.bits.rs2,
    VIMM_ADDR -> vimm_addr,
    VIMM_PREC -> UInt(0, 64)
  ))
  
  // Hookup resp queue
  resp_q.io.enq.valid := cmd_valid && emit_response && construct_ready(resp_ready)
  resp_q.io.enq.bits.data := new_vl
  resp_q.io.enq.bits.rd   := io.cmd.bits.inst.rd

  // TODO: hook this stuff up properly
  vu.io.vcmdq.cnt.valid := Bool(false)
  vu.io.vpfcmdq.cmd.valid := Bool(false)
  vu.io.vpfcmdq.imm1.valid := Bool(false)
  vu.io.vpfcmdq.imm2.valid := Bool(false)
  vu.io.vpfcmdq.cnt.valid := Bool(false)
  vu.io.xcpt.exception := Bool(false)
  vu.io.xcpt.evac := Bool(false)
  vu.io.xcpt.hold := Bool(false)
  vu.io.xcpt.kill := Bool(false)

  // Connect FMA units
  io.cp_dfma <> vu.io.cp_dfma
  io.cp_sfma <> vu.io.cp_sfma
}

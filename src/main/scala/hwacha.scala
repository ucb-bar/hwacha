package hwacha

import Chisel._
import rocket._
import uncore._

case class HwachaConfiguration(icache: ICacheConfig, nbanks: Int, nreg_per_bank: Int, ndtlb: Int, nptlb: Int)
{
  val nreg_total = nbanks * nreg_per_bank
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

  val VIMM_X    = Bits("b??",2)
  val VIMM_VLEN = Bits(0,2)
  val VIMM_RS1  = Bits(1,2)
  val VIMM_RS2  = Bits(2,2)
  val VIMM_ADDR = Bits(3,2)
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
    VSETCFG ->  List(Y, CMD_VVCFGIVL,VRT_X,  VR_X,  VR_X,   VIMM_VLEN,VIMM_X,    Y,Y,N,N),
    VSETVL  ->  List(Y, CMD_VSETVL,  VRT_X,  VR_X,  VR_X,   VIMM_VLEN,VIMM_X,    Y,Y,N,Y)
  )
}

class Hwacha(hc: HwachaConfiguration, rc: RocketConfiguration) extends RoCC(rc) {
  import HwachaDecodeTable._
  import Commands._
  
  val icache = Module(new Frontend()(hc.icache, rc.tl))
  val dtlb = Module(new TLB(hc.ndtlb))
  val ptlb = Module(new TLB(hc.nptlb))
  val vu = Module(new vu())
  
  // Decode
  val raw_inst = io.cmd.bits.inst.toBits
  val inst_i_imm = raw_inst(31, 20)
  val logic = DecodeLogic(raw_inst, HwachaDecodeTable.default, HwachaDecodeTable.table)
  val cs = logic.map {
    case b if b.inputs.head.getClass == classOf[Bool] => b.toBool
    case u => u 
  }

  val resp_q = Module( new Queue(io.resp.bits, 2) )
  resp_q.io.deq <> io.resp

  val (inst_val: Bool) :: sel_vcmd :: sel_vrtype :: sel_vr1 :: sel_vr2 :: sel_vimm1 :: sel_vimm2 :: (emit_vcmd: Bool) :: (emit_vimm1: Bool) :: (emit_vimm2: Bool) :: (emit_response: Bool) :: Nil = cs

  val cmd_valid = inst_val && io.cmd.valid
  val resp_ready  = !emit_response || resp_q.io.enq.ready
  val vcmd_ready  = Bool(true)//!emit_vcmd  || vu.io.vcmdq_user_ready
  val vimm1_ready = Bool(true)//!emit_vimm1 || vu.io.vximm1q_user_ready
  val vimm2_ready = Bool(true)// !emit_vimm2 || vu.io.vximm2q_user_ready

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
  icache.io.cpu.req.valid := vu.io.imem_req.valid
  icache.io.cpu.req.bits.pc := vu.io.imem_req.bits
  icache.io.cpu.req.bits.mispredict := Bool(false)
  icache.io.cpu.req.bits.taken := Bool(false)
  //icache.io.cpu.req.bits.current_pc

  vu.io.imem_resp <> icache.io.cpu.resp

  // Connect VU to D$
  io.mem.req <> vu.io.dmem_req
  vu.io.dmem_resp := io.mem.resp

  // Connect VU to DTLB and PTLB
  vu.io.vtlb <> dtlb.io
  vu.io.vpftlb <> ptlb.io

  // Busy signal for fencing TODO: CONNECT
  io.busy := Bool(false)//!vu.io.vfence_ready

  // TODO: SETUP PREFETCH QUEUES
  // TODO: SETUP INTERRUPT
  io.interrupt := Bool(false)

  // Logic to handle vector length calculation
  val cfg_maxvl = Reg(init=UInt(0, log2Up(hc.nreg_total)+1))

  val nxpr = (io.cmd.bits.rs1( 5,0) + inst_i_imm( 5,0)).zext.toUInt
  val nfpr = (io.cmd.bits.rs1(11,6) + inst_i_imm(11,6)).zext.toUInt
  val new_maxvl = Mux(nxpr+nfpr < UInt(2), UInt(hc.nreg_per_bank), UInt(hc.nreg_per_bank) / (nxpr-UInt(1) + nfpr)) << UInt(3)
  val new_vl = Mux(io.cmd.bits.rs1 < cfg_maxvl, io.cmd.bits.rs1, cfg_maxvl)
  val new_vl_m1 = Mux(sel_vcmd===CMD_VVCFGIVL, UInt(0), new_vl - UInt(1)) // translate into form for vcmdq

  val vimm_vlen = Cat(UInt(0,29), UInt(8,4), SInt(-1,8), nfpr(5,0), nxpr(5,0), new_vl_m1(10,0))
  when(cmd_valid && sel_vcmd === CMD_VVCFGIVL && construct_ready(null)) { cfg_maxvl := new_maxvl }

  // Calculate the vf address
  val vf_immediate = Cat(raw_inst(31,25),raw_inst(11,7)).toSInt
  val vimm_addr = io.cmd.bits.rs1 + vf_immediate

  // Hookup ready port of cmd queue
  io.cmd.ready := construct_ready(null)

  // Hookup vcmdq
  val vr1 = Mux(sel_vr1===VR_RS1, raw_inst(19,15), raw_inst(11,7))
  val vr2 = Mux(sel_vr2===VR_RS1, raw_inst(19,15), raw_inst(11,7))
  val construct_vcmd = Cat(sel_vcmd, sel_vrtype, vr1, sel_vrtype, vr2)

  vu.io.vcmdq.valid := cmd_valid && emit_vcmd && construct_ready(vcmd_ready) 
  vu.io.vcmdq.bits := construct_vcmd
  
  // Hookup vximm1q
  vu.io.vximm1q.valid := cmd_valid && emit_vimm1 && construct_ready(vimm1_ready) 
  vu.io.vximm1q.bits := MuxLookup(sel_vimm1, vimm_vlen, Array(
    VIMM_VLEN -> vimm_vlen,
    VIMM_RS1  -> io.cmd.bits.rs1,
    VIMM_RS2  -> io.cmd.bits.rs2,
    VIMM_ADDR -> vimm_addr
  ))

  // Hookup vximm2q
  vu.io.vximm2q.valid := cmd_valid && emit_vimm2 && construct_ready(vimm2_ready) 
  vu.io.vximm2q.bits := MuxLookup(sel_vimm2, vimm_vlen, Array(
    VIMM_VLEN -> vimm_vlen,
    VIMM_RS1  -> io.cmd.bits.rs1,
    VIMM_RS2  -> io.cmd.bits.rs2,
    VIMM_ADDR -> vimm_addr
  ))
  
  // Hookup resp queue
  resp_q.io.enq.valid := cmd_valid && emit_response && construct_ready(resp_ready)
  resp_q.io.enq.bits.data := new_vl
  resp_q.io.enq.bits.rd   := io.cmd.bits.inst.rd
  
}

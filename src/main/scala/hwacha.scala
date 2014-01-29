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
  val nvsreq = 128
  val nvlreq = nvldq
}

trait HwachaDecodeConstants
{
  val Y = Bool(true)
  val N = Bool(false)
  val X = Bits("b?", 1)

  val VCT_X = Bits("b?", 1)
  val VCT_S = Bits(0, 2)
  val VCT_F = Bits(1, 2)
  val VCT_A = Bits(2, 2)

  val VR_X   = Bits("b?", 1)
  val VR_RS1 = Bits(0, 1)
  val VR_RD  = Bits(1, 1)

  val VIMM_X    = Bits("b???",3)
  val VIMM_VLEN = Bits(0,3)
  val VIMM_RS1  = Bits(1,3)
  val VIMM_RS2  = Bits(2,3)
  val VIMM_ADDR = Bits(3,3)
  val VIMM_PREC = Bits(4,3)

  val RESP_X     = Bits("b??",2)
  val RESP_NVL   = Bits(0,2)
  val RESP_CAUSE = Bits(1,2)
  val RESP_AUX   = Bits(2,2)

  val SZ_PREC = 2
  val PREC_DOUBLE = Bits("b00", SZ_PREC)
  val PREC_SINGLE = Bits("b01", SZ_PREC)
  val PREC_HALF   = Bits("b10", SZ_PREC)
}

object HwachaDecodeTable extends HwachaDecodeConstants
{
  import HwachaInstructions._
  import Commands._
                // * means special case decode code below                          checkvl?
                //                                                                 | vcmd?
                //                                                                 | | vimm1?
                //     val                                                         | | | vimm2?  resp?
                //     |  vcmd         vctype  reg1   reg2    imm1      imm2       | | | | vcnt? | resp_type
                //     |  |            |       |      |       |         |          | | | | |     | |
  val default =   List(N, CMD_X,       VCT_X,  VR_X,  VR_X,   VIMM_X,   VIMM_X,    N,N,N,N,N,    N,RESP_X    )
  val table = Array( 
    // Configurable precision instructions
    VSETPREC   -> List(Y, CMD_VSETPREC,VCT_X,  VR_X,  VR_X,   VIMM_PREC,VIMM_X,    N,Y,Y,N,N,    N,RESP_X    ),
    // General instructions
    VSETCFG    -> List(Y, CMD_VVCFGIVL,VCT_X,  VR_X,  VR_X,   VIMM_VLEN,VIMM_X,    N,Y,Y,N,N,    N,RESP_X    ), //* set maxvl register
    VSETVL     -> List(Y, CMD_VSETVL,  VCT_X,  VR_X,  VR_X,   VIMM_VLEN,VIMM_X,    N,Y,Y,N,N,    Y,RESP_NVL  ), //* set vl register
    VF         -> List(Y, CMD_VF,      VCT_X,  VR_X,  VR_X,   VIMM_ADDR,VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VMVV       -> List(Y, CMD_VMVV,    VCT_S,  VR_RD, VR_RS1, VIMM_X,   VIMM_X,    Y,Y,N,N,N,    N,RESP_X    ),
    VMSV       -> List(Y, CMD_VMSV,    VCT_S,  VR_RD, VR_RS1, VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    // Memory load/stores (x-registers)
    VLD        -> List(Y, CMD_VLD,     VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VLW        -> List(Y, CMD_VLW,     VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VLWU       -> List(Y, CMD_VLWU,    VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VLH        -> List(Y, CMD_VLH,     VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VLHU       -> List(Y, CMD_VLHU,    VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VLB        -> List(Y, CMD_VLB,     VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VLBU       -> List(Y, CMD_VLBU,    VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VSD        -> List(Y, CMD_VSD,     VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VSW        -> List(Y, CMD_VSW,     VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VSH        -> List(Y, CMD_VSH,     VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VSB        -> List(Y, CMD_VSB,     VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    // Memory load/stores (fp-registers)
    VFLD       -> List(Y, CMD_VFLD,    VCT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VFLW       -> List(Y, CMD_VFLW,    VCT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VFSD       -> List(Y, CMD_VFSD,    VCT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    VFSW       -> List(Y, CMD_VFSW,    VCT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_X,    Y,Y,Y,N,N,    N,RESP_X    ),
    // Memory strided load/stores (x-registers)
    VLSTD      -> List(Y, CMD_VLSTD,   VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VLSTW      -> List(Y, CMD_VLSTW,   VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VLSTWU     -> List(Y, CMD_VLSTWU,  VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VLSTH      -> List(Y, CMD_VLSTH,   VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VLSTHU     -> List(Y, CMD_VLSTHU,  VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VLSTB      -> List(Y, CMD_VLSTB,   VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VLSTBU     -> List(Y, CMD_VLSTBU,  VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VSSTD      -> List(Y, CMD_VSSTD,   VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VSSTW      -> List(Y, CMD_VSSTW,   VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VSSTH      -> List(Y, CMD_VSSTH,   VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VSSTB      -> List(Y, CMD_VSSTB,   VCT_S,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    // Memory strided load/stores (fp-registers)
    VFLSTD     -> List(Y, CMD_VFLSTD,  VCT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VFLSTW     -> List(Y, CMD_VFLSTW,  VCT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VFSSTD     -> List(Y, CMD_VFSSTD,  VCT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    VFSSTW     -> List(Y, CMD_VFSSTW,  VCT_F,  VR_RD, VR_RD,  VIMM_RS1, VIMM_RS2,  Y,Y,Y,Y,N,    N,RESP_X    ),
    // Exception and save/restore instructions
    VXCPTCAUSE -> List(Y, CMD_X,       VCT_X,  VR_X,  VR_X,   VIMM_X,   VIMM_X,    N,N,N,N,N,    Y,RESP_CAUSE),
    VXCPTAUX   -> List(Y, CMD_X,       VCT_X,  VR_X,  VR_X,   VIMM_X,   VIMM_X,    N,N,N,N,N,    Y,RESP_AUX  ),
    VXCPTSAVE  -> List(N, CMD_X,       VCT_X,  VR_X,  VR_X,   VIMM_X,   VIMM_X,    N,N,N,N,N,    N,RESP_X    ),
    VXCPTRESTORE->List(N, CMD_X,       VCT_X,  VR_X,  VR_X,   VIMM_X,   VIMM_X,    N,N,N,N,N,    N,RESP_X    ),
    VXCPTKILL  -> List(N, CMD_X,       VCT_X,  VR_X,  VR_X,   VIMM_X,   VIMM_X,    N,N,N,N,N,    N,RESP_X    ),
    VXCPTEVAC  -> List(N, CMD_X,       VCT_X,  VR_X,  VR_X,   VIMM_X,   VIMM_X,    N,N,N,N,N,    N,RESP_X    ),
    VXCPTHOLD  -> List(N, CMD_X,       VCT_X,  VR_X,  VR_X,   VIMM_X,   VIMM_X,    N,N,N,N,N,    N,RESP_X    ),
    VENQCMD    -> List(Y, CMD_X,       VCT_A,  VR_X,  VR_X,   VIMM_X,   VIMM_X,    N,Y,N,N,N,    N,RESP_X    ),
    VENQIMM1   -> List(Y, CMD_X,       VCT_X,  VR_X,  VR_X,   VIMM_RS1, VIMM_X,    N,N,Y,N,N,    N,RESP_X    ),
    VENQIMM2   -> List(Y, CMD_X,       VCT_X,  VR_X,  VR_X,   VIMM_X,   VIMM_RS1,  N,N,N,Y,N,    N,RESP_X    ),
    VENQCNT    -> List(Y, CMD_X,       VCT_X,  VR_X,  VR_X,   VIMM_X,   VIMM_X,    N,N,N,N,Y,    N,RESP_X    )
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
 
  // Cofiguration state
  val cfg_maxvl = Reg(init=UInt(32, log2Up(hc.nreg_total)+1))
  val cfg_vl    = Reg(init=UInt(0, log2Up(hc.nreg_total)+1))
  
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

  val (inst_val: Bool) :: sel_vcmd :: sel_vctype :: sel_vr1 :: sel_vr2 :: sel_vimm1 :: sel_vimm2 :: (check_vl: Bool) :: (emit_vcmd: Bool) :: (emit_vimm1: Bool) :: (emit_vimm2: Bool) :: (emit_cnt: Bool) :: (emit_response: Bool) :: sel_resp :: Nil = cs

  val cmd_valid = inst_val && io.cmd.valid && (!check_vl || cfg_vl != UInt(0))
  val resp_ready  = !emit_response || resp_q.io.enq.ready
  val vcmd_ready  = !emit_vcmd  || vu.io.vcmdq_user_ready
  val vimm1_ready = !emit_vimm1 || vu.io.vimm1q_user_ready
  val vimm2_ready = !emit_vimm2 || vu.io.vimm2q_user_ready
  val vcnt_ready  = !emit_cnt || vu.io.vcmdq.cnt.ready

  def construct_ready(exclude: Bool): Bool = {
    val all_readies = Array(resp_ready, vcmd_ready, vimm1_ready, vimm2_ready, vcnt_ready)
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

  val reg_prec = Reg(init = PREC_DOUBLE)
  val next_prec = Bits(width = SZ_PREC)

  val prec = (inst_i_imm(11,5)).toUInt
  val vimm_prec = UInt(width = 64)
  if (conf.confprec) {
    vimm_prec := Cat(UInt(0,59), prec(6,0))
    next_prec := MuxLookup(vimm_prec, PREC_DOUBLE, Array(
      UInt(64) -> PREC_DOUBLE,
      UInt(32) -> PREC_SINGLE,
      UInt(16) -> PREC_HALF
    ))
  } else {
    vimm_prec := UInt(64, 64)
    next_prec := PREC_DOUBLE
  }

  // Logic to handle vector length calculation
  val nxpr = (io.cmd.bits.rs1( 5,0) + inst_i_imm( 5,0)).zext.toUInt
  val nfpr = (io.cmd.bits.rs1(11,6) + inst_i_imm(11,6)).zext.toUInt

  val reg_nxpr = Reg(init = UInt(0, 6))
  val reg_nfpr = Reg(init = UInt(0, 6))

  val packing = MuxLookup(reg_prec, UInt(0), Array(
    PREC_DOUBLE -> UInt(0),
    PREC_SINGLE -> UInt(1),
    PREC_HALF   -> UInt(2)
  ))

  // vector length lookup
  val regs_used = (Mux(nxpr === UInt(0), UInt(0), nxpr - UInt(1)) << packing) + nfpr
  val vlen_width = log2Up(hc.nreg_per_bank + 1)
  val rom_allocation_units = (0 to 164).toArray.map(n => (UInt(n),
    UInt(if (n < 2) (hc.nreg_per_bank) else (hc.nreg_per_bank / n), width = vlen_width)
  ))

  val ut_per_bank = Lookup(regs_used, rom_allocation_units.last._2, rom_allocation_units) << packing
  val new_maxvl = ut_per_bank << UInt(3) // microthreads
  val xf_split = (new_maxvl >> UInt(3)) * (nxpr - UInt(1))

  val new_vl = Mux(io.cmd.bits.rs1 < cfg_maxvl, io.cmd.bits.rs1, cfg_maxvl)
  val new_vl_m1 = Mux(sel_vcmd === CMD_VVCFGIVL, UInt(0), new_vl - UInt(1)) // translate into form for vcmdq

  val vimm_vlen = Cat(UInt(0,19), xf_split, UInt(8,4), SInt(-1,8), nfpr(5,0), nxpr(5,0), new_vl_m1(10,0))
  when (cmd_valid && construct_ready(null)) {
    switch (sel_vcmd) {
      is (CMD_VVCFGIVL) {
        printf("setting maxvl to %d (was %d)\n", new_maxvl, cfg_maxvl)
        cfg_maxvl := new_maxvl
        cfg_vl := UInt(0)
        reg_nxpr := nxpr
        reg_nfpr := nfpr
      }
      is (CMD_VSETVL) {
        cfg_vl := new_vl
      }
    }
  }

  // set precision
  when (cmd_valid && sel_vcmd === CMD_VSETPREC && construct_ready(null)) {
    reg_prec := next_prec
    printf("setting prec to %d\n", next_prec)
  }


  // Calculate the vf address
  val vf_immediate = Cat(raw_inst(31,25),raw_inst(11,7)).toSInt
  val vimm_addr = io.cmd.bits.rs1 + vf_immediate

  // Hookup ready port of cmd queue
  io.cmd.ready := construct_ready(null)

  // Hookup vcmdq.cmd
  val vr1 = Mux(sel_vr1===VR_RS1, raw_inst(19,15), raw_inst(11,7))
  val vr2 = Mux(sel_vr2===VR_RS1, raw_inst(19,15), raw_inst(11,7))
  val sel_vrtype = Mux(sel_vctype===VCT_F, UInt(1,1), UInt(0,1))
  val construct_vcmd = Cat(sel_vcmd, sel_vrtype, vr1, sel_vrtype, vr2)

  vu.io.vcmdq.cmd.valid := cmd_valid && emit_vcmd && construct_ready(vcmd_ready) 
  vu.io.vcmdq.cmd.bits := Mux(sel_vctype===VCT_A, io.cmd.bits.rs1, construct_vcmd)
  
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

  // Hookup vcmdq.cnt
  vu.io.vcmdq.cnt.valid := cmd_valid && emit_cnt && construct_ready(vcnt_ready)
  vu.io.vcmdq.cnt.bits := io.cmd.bits.rs1
  
  // Hookup resp queue
  resp_q.io.enq.valid := cmd_valid && emit_response && construct_ready(resp_ready)
  resp_q.io.enq.bits.data := MuxLookup(sel_resp, new_vl, Array(
    RESP_NVL   -> new_vl,
    RESP_CAUSE -> vu.io.irq_cause,
    RESP_AUX   -> vu.io.irq_aux
  ))
  resp_q.io.enq.bits.rd   := io.cmd.bits.inst.rd

  // TODO: hook this stuff up properly
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

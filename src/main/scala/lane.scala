package hwacha

import Chisel._
import Node._
import Constants._
import scala.collection.mutable.ArrayBuffer

class LaneUopIO extends Bundle
{
  val read = Valid(new BankUopRead)
  val write = Valid(new BankUopWrite)
  val viu = Valid(new BankUopVIU)
  val vau0 = Valid(new LfuncUopVAU0)
  val vau1 = Valid(new LfuncUopVAU1)
  val vau2 = Valid(new LfuncUopVAU2)
  val vgu = Valid(new LfuncUopVGU)
  val vlu = Valid(new LfuncUopVLU)
  val vsu = Valid(new LfuncUopVSU)
}

class VMUIO extends Bundle 
{
  val vaq_val   = Bool(OUTPUT)
  val vaq_check = new io_vxu_mem_check().asOutput
  val vaq_mem = new io_vxu_mem_cmd().asOutput
  val vaq_imm = Bits(OUTPUT, SZ_DATA)
  val vaq_utmemop = Bool(OUTPUT)
  val vaq_rf = Bits(OUTPUT, SZ_DATA)

  val vldq_rdy  = Bool(OUTPUT)
  val vldq_bits = Bits(INPUT, SZ_DATA)

  val vsdq_val  = Bool(OUTPUT)
  val vsdq_mem = new io_vxu_mem_cmd().asOutput
  val vsdq_bits = Bits(OUTPUT, SZ_DATA)
}

class io_lane_to_hazard extends Bundle
{
  val rlast = Bool()
  val wlast = Bool()
  val wlast_mask = Bool()
  val pvfb_tag = Bits(width=SZ_PVFB_TAG)
}

class ioLaneToPVFB extends Bundle
{
  val mask = Valid(Bits(width=WIDTH_PVFB) ) 
}

class ioLaneToIssue extends Bundle
{
  val mask = Valid(Bits(width=WIDTH_PVFB * NUM_PVFB) )
  val pvfb_tag = Bits(OUTPUT, SZ_PVFB_TAG)
}

class Lane(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val cp_dfma = new rocket.ioFMA(65).flip
    val cp_sfma = new rocket.ioFMA(33).flip

    val issue_to_lane = new io_vxu_issue_to_lane().asInput
    val uop = new LaneUopIO().flip
    val lane_to_hazard = new io_lane_to_hazard().asOutput
    val laneToIssue = new ioLaneToIssue()
    val vmu = new VMUIO()

    val prec = Bits(INPUT, SZ_PREC)
  }

  val conn = new ArrayBuffer[BankUopIO]
  val rblen = new ArrayBuffer[UInt]
  val rdata = new ArrayBuffer[UInt]
  val ropl0 = new ArrayBuffer[UInt]
  val ropl1 = new ArrayBuffer[UInt]

  //forward declaring imul, fma, and conv units
  val imul = Module(new LaneMul)
  val fma  = Module(new LaneFMA)
  val conv = Module(new LaneConv)

  for (i <- 0 until SZ_BANK) 
  {
    val bank = Module(new Bank)
    bank.io.active := io.issue_to_lane.bactive(i)
    bank.io.prec := io.prec
    bank.io.uop.in <> (if (i == 0) io.uop else conn.last)
    
    conn += bank.io.uop.out
    rblen += bank.io.rw.rblen
    rdata += bank.io.rw.rdata
    ropl0 += bank.io.rw.ropl0
    ropl1 += bank.io.rw.ropl1

    bank.io.rw.wbl0 := imul.io.out
    bank.io.rw.wbl1 := fma.io.out
    bank.io.rw.wbl2 := conv.io.out
    bank.io.rw.wbl3 := io.vmu.vldq_bits
  }

  io.lane_to_hazard.rlast := conn.last.read.valid && conn.last.read.bits.last
  io.lane_to_hazard.wlast := conn.last.write.valid && conn.last.write.bits.last
  io.lane_to_hazard.wlast_mask := Bool(false)

  val xbar = Module(new LaneXbar)
  xbar.io.rblen <> rblen
  xbar.io.rdata <> rdata
  xbar.io.ropl0 <> ropl0
  xbar.io.ropl1 <> ropl1
  val rbl = xbar.io.rbl

  val lfu = Module(new LaneLFU)

  lfu.io.uop <> io.uop

  io.vmu.vaq_val := lfu.io.vaq_val
  io.vmu.vaq_check <> lfu.io.vaq_check
  io.vmu.vaq_mem <> lfu.io.vaq_mem
  io.vmu.vaq_imm := lfu.io.vaq_imm
  io.vmu.vaq_utmemop := lfu.io.vaq_utmemop

  io.vmu.vldq_rdy := lfu.io.vldq_rdy
  io.vmu.vsdq_val := lfu.io.vsdq_val
  io.vmu.vsdq_mem <> lfu.io.vsdq_mem

  //integer multiply
  imul.io.valid := lfu.io.vau0_val
  imul.io.fn := lfu.io.vau0_fn
  imul.io.in0 := rbl(0)
  imul.io.in1 := rbl(1)

  //fma
  fma.io.valid := lfu.io.vau1_val
  fma.io.fn := lfu.io.vau1_fn
  fma.io.in0 := rbl(2)
  fma.io.in1 := rbl(3)
  fma.io.in2 := rbl(4)

  io.cp_dfma <> fma.io.cp_dfma
  io.cp_sfma <> fma.io.cp_sfma

  //conv
  conv.io.valid := lfu.io.vau2_val
  conv.io.fn := lfu.io.vau2_fn
  conv.io.in := rbl(5)

  io.vmu.vaq_rf := rbl(6)(SZ_ADDR-1,0)
  io.vmu.vsdq_bits := rbl(7)
}

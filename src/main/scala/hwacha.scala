package hwacha

import Chisel._
import uncore._
import Constants._
import rocket.NTLBEntries

case object HwachaNBanks extends Field[Int]
case object HwachaNRegPerBank extends Field[Int]
case object HwachaNDTLB extends Field[Int]
case object HwachaNPTLB extends Field[Int]
case object HwachaCacheBlockOffsetBits extends Field[Int]
case object HwachaScalarDataBits extends Field[Int]
case object HwachaNVectorLoadMetaBufferEntries extends Field[Int]

abstract class HwachaModule(clock: Clock = null, _reset: Bool = null) extends Module(clock, _reset) with UsesHwachaParameters
abstract class HwachaBundle extends Bundle with UsesHwachaParameters

abstract trait UsesHwachaParameters extends UsesParameters {
  val nbanks = params(HwachaNBanks)
  val nreg_per_bank = params(HwachaNRegPerBank)
  val ndtlb = params(HwachaNDTLB)  
  val nptlb = params(HwachaNPTLB)  

  val nreg_total = nbanks * nreg_per_bank
  val confvru = true
  val confprec = false

  // pipeline latencies
  val int_stages = 2
  val imul_stages = 3
  val fma_stages = 3
  val fconv_stages = 3

  val second_fma_pipe = true
  val second_fconv_pipe = false

  val delay_seq_exp = 2

  val ptr_incr_max =
    nbanks-1 +
    List(int_stages+2, imul_stages+3, fma_stages+4, fconv_stages+2).reduce(scala.math.max(_,_)) +
    delay_seq_exp +
    2 // buffer
  val ptr_incr_sz = log2Up(ptr_incr_max)

  val shift_buf_read = 3
  val shift_buf_write =
    List(imul_stages+3, fma_stages+4, fconv_stages+2).reduce(scala.math.max(_,_)) + 1


  val confvcmdq = new {
    val ncmd = 19
    val nimm = 19
    val nrd = 19
    val ncnt = nbanks
  }

  require(confvcmdq.ncmd >= nbanks)
  require(confvcmdq.nimm >= nbanks)
  require(confvcmdq.nrd >= nbanks)

  val nbrq = 2
  val nbwq = 2

  val confvmu = new {
    val ncmdq = 2
    val naddrq = 2

    val nvvaq = 16
    val nvpaq = 16
    val nvsdq = 16
    val nvldq = 16
    val nvlmb = 16

    val nvvapfq = 8
    val nvpapfq = 8

    val sz_tag = log2Up(nvlmb)
    val sz_addr = math.max(params(PAddrBits), params(VAddrBits))
    val sz_data = 64
  }

  val nvsreq = 128
  val nvlreq = 128
  val nvsdq = nbrq * nbanks

  // D$ tag requirement for hwacha
  require(params(rocket.CoreDCacheReqTagBits) >= confvmu.sz_tag)

}

class Hwacha extends rocket.RoCC with UsesHwachaParameters
{
  import HwachaDecodeTable._
  import Commands._

  val icache = Module(new rocket.Frontend, {case CacheName => "HwI"})
  //val dtlb = Module(new rocket.TLB, {case NTLBEntries => ndtlb})
  //val ptlb = Module(new rocket.TLB, {case NTLBEntries => nptlb})

  val rocc = Module(new RoCC)
  val scalar = Module(new Scalar)

  rocc.io.rocc <> io
  rocc.io.pending_memop := scalar.io.pending_memop
  rocc.io.vf_active := scalar.io.vf_active
  rocc.io <> scalar.io

  // Connect Scalar to I$
  icache.io.cpu <> scalar.io.imem

  // Connect supporting Hwacha memory modules to external ports
  io.imem <> icache.io.mem
  io.iptw <> icache.io.ptw
  //io.dptw <> dtlb.io.ptw

  //fake vmu
  scalar.io.vmu.loadData.valid := Bool(false)
  scalar.io.vmu.storeAck := Bool(false)

  /*

  // Connect VU to D$
  io.mem <> vu.io.dmem
  */

  // Connect VU to DTLB
  //vu.io.vtlb <> dtlb.io

}

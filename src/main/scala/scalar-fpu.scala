package hwacha 

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.tile.FPConstants._
import freechips.rocketchip.tile.{FPResult, FPUCtrlSigs, HasFPUParameters}
import freechips.rocketchip.util._

class ScalarFPU(implicit p: Parameters) extends HwachaModule()(p) with HasFPUParameters {
  val io = new Bundle {
    val req = Decoupled(new freechips.rocketchip.tile.FPInput()).flip
    val resp = Decoupled(new FPResult())
  }
  //buffer for simple back-pressure model
  val resp_reg = Reg(Bits())
  val resp_reg_val = Reg(init=Bool(false))

  io.req.ready := !resp_reg_val

  val ex_ctrl = Wire(new FPUCtrlSigs)
  ex_ctrl <> io.req.bits
  val wb_ctrl = RegEnable(ex_ctrl, io.req.valid)
  val wb_reg_valid = Reg(next=io.req.valid, init=Bool(false))

  val req = new freechips.rocketchip.tile.FPInput
  req := io.req.bits
  req.in2 := Mux(io.req.bits.swap23, io.req.bits.in3, io.req.bits.in2)
  req.in3 := Mux(io.req.bits.swap23, io.req.bits.in2, io.req.bits.in3)

  val sfma = Module(new freechips.rocketchip.tile.FPUFMAPipe(p(HwachaStagesSFMA), freechips.rocketchip.tile.FType.S))
  sfma.suggestName("sfmaInst")
  sfma.io.in.valid := io.req.valid && io.req.bits.fma &&
                      (io.req.bits.typeTagIn === S)
  sfma.io.in.bits := req

  val dfma = Module(new freechips.rocketchip.tile.FPUFMAPipe(p(HwachaStagesDFMA), freechips.rocketchip.tile.FType.D))
  dfma.suggestName("dfmaInst")
  dfma.io.in.valid := io.req.valid && io.req.bits.fma &&
                      (io.req.bits.typeTagOut === D)
  dfma.io.in.bits := req

  val fpiu = Module(new freechips.rocketchip.tile.FPToInt)
  fpiu.suggestName("fpiuInst")
  fpiu.io.in.valid := io.req.valid && (io.req.bits.toint || io.req.bits.div || io.req.bits.sqrt || (io.req.bits.fastpipe && io.req.bits.wflags))
  fpiu.io.in.bits := req

  val ifpu = Module(new freechips.rocketchip.tile.IntToFP(3))
  ifpu.suggestName("ifpuInst")
  ifpu.io.in.valid := io.req.valid && io.req.bits.fromint
  ifpu.io.in.bits := req

  //ifpu.io.in.bits.in1 := io.dpath.fromint_data

  val fpmu = Module(new freechips.rocketchip.tile.FPToFP(2))
  fpmu.suggestName("fpmuInst")
  fpmu.io.in.valid := io.req.valid && io.req.bits.fastpipe
  fpmu.io.in.bits := req
  fpmu.io.lt := fpiu.io.out.bits.lt

  // No writeback arbitration since ScalarUnit can't put backpressure on us
  // writeback arbitration
  case class Pipe(p: Module, lat: Int, cond: (FPUCtrlSigs) => Bool, res: FPResult)
  val pipes = List(
    Pipe(fpmu, fpmu.latency, (c: FPUCtrlSigs) => c.fastpipe, fpmu.io.out.bits),
    Pipe(ifpu, ifpu.latency, (c: FPUCtrlSigs) => c.fromint, ifpu.io.out.bits),
    Pipe(sfma, sfma.latency, (c: FPUCtrlSigs) => c.fma && (c.typeTagOut === S), sfma.io.out.bits),
    Pipe(dfma, dfma.latency, (c: FPUCtrlSigs) => c.fma && (c.typeTagOut === D), dfma.io.out.bits)
  )
  def latencyMask(c: FPUCtrlSigs, offset: Int) = {
    require(pipes.forall(_.lat >= offset))
    pipes.map(p => Mux(p.cond(c), UInt(1 << p.lat-offset), UInt(0))).reduce(_|_)
  }
  def pipeid(c: FPUCtrlSigs) = pipes.zipWithIndex.map(p => Mux(p._1.cond(c), UInt(p._2), UInt(0))).reduce(_|_)
  val maxLatency = pipes.map(_.lat).max
  val wbLatencyMask = latencyMask(wb_ctrl, 2)

  class WBInfo extends Bundle {
    val single = Bool()
    val pipeid = UInt(width = log2Ceil(pipes.size))
  }

  val wen = Reg(init=Bits(0, maxLatency-1))
  val wbInfo = Reg(Vec(maxLatency-1, new WBInfo))
  val wb_wen = wb_reg_valid && (wb_ctrl.fma || wb_ctrl.fastpipe || wb_ctrl.fromint)
  val write_port_busy = RegEnable(wb_wen && (wbLatencyMask & latencyMask(ex_ctrl, 1)).orR || (wen & latencyMask(ex_ctrl, 0)).orR, io.req.valid)
  val wb_winfo = pipeid(wb_ctrl)

  for (i <- 0 until maxLatency-2) {
    when (wen(i+1)) { wbInfo(i) := wbInfo(i+1) }
  }
  wen := wen >> UInt(1)
  when (wb_wen) {
    wen := wen >> UInt(1) | wbLatencyMask
    for (i <- 0 until maxLatency-1) {
      when (!write_port_busy && wbLatencyMask(i)) {
        wbInfo(i).single := wb_ctrl.typeTagOut === S
        wbInfo(i).pipeid := pipeid(wb_ctrl)
      }
    }
  }

  val wsrc = wbInfo(0).pipeid
  val wdata = (pipes.map(_.res.data): Seq[UInt])(wsrc)
  val wexc = (pipes.map(_.res.exc): Seq[UInt])(wsrc)
  val resp_data = Mux(!fpiu.io.out.valid, wdata, fpiu.io.out.bits.toint) 
  io.resp.bits.data := resp_data
  when (wen(0) || fpiu.io.out.valid ) { 
    when(!io.resp.ready){
      resp_reg := resp_data
      resp_reg_val := Bool(true)
    }
  }
  when(io.resp.ready && resp_reg_val){
    io.resp.bits.data := resp_reg
    resp_reg_val := Bool(false)
  }
  io.resp.valid := wen(0) || fpiu.io.out.valid || resp_reg_val
}

package hwacha 

import Chisel._
import cde.Parameters
import rocket.FPConstants._

class ScalarFPU(implicit p: Parameters) extends HwachaModule()(p) {
  val io = new Bundle {
    val req = Decoupled(new rocket.FPInput()).flip
    val resp = Decoupled(new rocket.FPResult())
  }
  //buffer for simple back-pressure model
  val resp_reg = Reg(Bits())
  val resp_reg_val = Reg(init=Bool(false))

  io.req.ready := !resp_reg_val

  val wb_ctrl = RegEnable(io.req.bits, io.req.valid)
  val wb_reg_valid = Reg(next=io.req.valid, init=Bool(false))

  val req = new rocket.FPInput
  req := io.req.bits
  req.in2 := Mux(io.req.bits.swap23, io.req.bits.in3, io.req.bits.in2)
  req.in3 := Mux(io.req.bits.swap23, io.req.bits.in2, io.req.bits.in3)

  val sfma = Module(new rocket.FPUFMAPipe(p(rocket.SFMALatency), 23, 9))
  sfma.io.in.valid := io.req.valid && io.req.bits.fma &&
                      io.req.bits.single
  sfma.io.in.bits := req

  val dfma = Module(new rocket.FPUFMAPipe(p(rocket.DFMALatency), 52, 12))
  dfma.io.in.valid := io.req.valid && io.req.bits.fma &&
                      !io.req.bits.single
  dfma.io.in.bits := req

  val fpiu = Module(new rocket.FPToInt)
  fpiu.io.in.valid := io.req.valid && (io.req.bits.toint || io.req.bits.cmd === FCMD_MINMAX)
  fpiu.io.in.bits := req

  val ifpu = Module(new rocket.IntToFP(3))
  ifpu.io.in.valid := io.req.valid && io.req.bits.fromint
  ifpu.io.in.bits := req

  //ifpu.io.in.bits.in1 := io.dpath.fromint_data

  val fpmu = Module(new rocket.FPToFP(2))
  fpmu.io.in.valid := io.req.valid && io.req.bits.fastpipe
  fpmu.io.in.bits := req
  fpmu.io.lt := fpiu.io.out.bits.lt

  // No writeback arbitration since ScalarUnit can't put backpressure on us
  // writeback arbitration
  case class Pipe(p: Module, lat: Int, cond: (rocket.FPInput) => Bool, wdata: Bits, wexc: Bits)
  val pipes = List(
  Pipe(fpmu, fpmu.latency, (c: rocket.FPInput) => c.fastpipe, fpmu.io.out.bits.data, fpmu.io.out.bits.exc),
  Pipe(ifpu, ifpu.latency, (c: rocket.FPInput) => c.fromint, ifpu.io.out.bits.data, ifpu.io.out.bits.exc),
  Pipe(sfma, sfma.latency, (c: rocket.FPInput) => c.fma && (c.single), Cat(SInt(-1, 32), sfma.io.out.bits.data), sfma.io.out.bits.exc),
  Pipe(dfma, dfma.latency, (c: rocket.FPInput) => c.fma && !(c.single), dfma.io.out.bits.data, dfma.io.out.bits.exc))
  def latencyMask(c: rocket.FPInput, offset: Int) = {
    require(pipes.forall(_.lat >= offset))
    pipes.map(p => Mux(p.cond(c), UInt(1 << p.lat-offset), UInt(0))).reduce(_|_)
  }
  def pipeid(c: rocket.FPInput) = pipes.zipWithIndex.map(p => Mux(p._1.cond(c), UInt(p._2), UInt(0))).reduce(_|_)
  val maxLatency = pipes.map(_.lat).max
  val wbLatencyMask = latencyMask(wb_ctrl, 2)

  val wen = Reg(init=Bits(0, maxLatency-1))
  val winfo = Vec.fill(maxLatency-1){Reg(Bits())}
  val wb_wen = wb_reg_valid && (wb_ctrl.fma || wb_ctrl.fastpipe || wb_ctrl.fromint)
  val write_port_busy = RegEnable(wb_wen && (wbLatencyMask & latencyMask(io.req.bits, 1)).orR || (wen & latencyMask(io.req.bits, 0)).orR, io.req.valid)
  val wb_winfo = pipeid(wb_ctrl)

  for (i <- 0 until maxLatency-2) {
    when (wen(i+1)) { winfo(i) := winfo(i+1) }
  }
  wen := wen >> UInt(1)
  when (wb_wen) {
    wen := wen >> UInt(1) | wbLatencyMask
    for (i <- 0 until maxLatency-1) {
      when (!write_port_busy && wbLatencyMask(i)) {
        winfo(i) := wb_winfo
      }
    }
  }

  val wsrc = winfo(0)
  val wdata = Vec(pipes.map(_.wdata))(wsrc)
  val wexc = Vec(pipes.map(_.wexc))(wsrc)
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

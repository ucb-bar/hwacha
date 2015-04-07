package hwacha 

import Chisel._
import Node._
import Constants._
import rocket.FPConstants._

object ScalarFPUDecode
{ 
  val H = PREC_HALF
  val S = PREC_SINGLE
  val D = PREC_DOUBLE

  val FX       = Cat(FCMD_X,      X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X)
  val FCVT_S_W = Cat(FCMD_CVT_FI, N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,Y)
  val FCVT_S_WU= Cat(FCMD_CVT_FI, N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,Y)
  val FCVT_S_L = Cat(FCMD_CVT_FI, N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,Y)
  val FCVT_S_LU= Cat(FCMD_CVT_FI, N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,Y)
  val FCVT_D_W = Cat(FCMD_CVT_FI, N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,Y)
  val FCVT_D_WU= Cat(FCMD_CVT_FI, N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,Y)
  val FCVT_D_L = Cat(FCMD_CVT_FI, N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,Y)
  val FCVT_D_LU= Cat(FCMD_CVT_FI, N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,Y)
  val FCLASS_S = Cat(FCMD_MV_XF,  N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,N)
  val FCLASS_D = Cat(FCMD_MV_XF,  N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,N)
  val FCVT_W_S = Cat(FCMD_CVT_IF, N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,Y)
  val FCVT_WU_S= Cat(FCMD_CVT_IF, N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,Y)
  val FCVT_L_S = Cat(FCMD_CVT_IF, N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,Y)
  val FCVT_LU_S= Cat(FCMD_CVT_IF, N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,Y)
  val FCVT_W_D = Cat(FCMD_CVT_IF, N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,Y)
  val FCVT_WU_D= Cat(FCMD_CVT_IF, N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,Y)
  val FCVT_L_D = Cat(FCMD_CVT_IF, N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,Y)
  val FCVT_LU_D= Cat(FCMD_CVT_IF, N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,Y)
  val FCVT_S_D = Cat(FCMD_CVT_FF, N,Y,Y,N,N,N,X,Y,N,N,Y,N,N,N,Y,Y)
  val FCVT_D_S = Cat(FCMD_CVT_FF, N,Y,Y,N,N,N,X,N,N,N,Y,N,N,N,Y,Y)
  val FEQ_S    = Cat(FCMD_CMP,    N,N,Y,Y,N,N,N,Y,N,Y,N,N,N,N,N,Y)
  val FLT_S    = Cat(FCMD_CMP,    N,N,Y,Y,N,N,N,Y,N,Y,N,N,N,N,N,Y)
  val FLE_S    = Cat(FCMD_CMP,    N,N,Y,Y,N,N,N,Y,N,Y,N,N,N,N,N,Y)
  val FEQ_D    = Cat(FCMD_CMP,    N,N,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,Y)
  val FLT_D    = Cat(FCMD_CMP,    N,N,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,Y)
  val FLE_D    = Cat(FCMD_CMP,    N,N,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,Y)
  val FSGNJ_S  = Cat(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,N)
  val FSGNJN_S = Cat(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,N)
  val FSGNJX_S = Cat(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,N)
  val FSGNJ_D  = Cat(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N)
  val FSGNJN_D = Cat(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N)
  val FSGNJX_D = Cat(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N)
  val FMIN_S   = Cat(FCMD_MINMAX, N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,Y)
  val FMAX_S   = Cat(FCMD_MINMAX, N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,Y)
  val FMIN_D   = Cat(FCMD_MINMAX, N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y)
  val FMAX_D   = Cat(FCMD_MINMAX, N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y)
  val FADD_S   = Cat(FCMD_ADD,    N,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y,Y)
  val FSUB_S   = Cat(FCMD_SUB,    N,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y,Y)
  val FMUL_S   = Cat(FCMD_MUL,    N,Y,Y,Y,N,N,N,Y,N,N,N,Y,N,N,Y,Y)
  val FADD_D   = Cat(FCMD_ADD,    N,Y,Y,Y,N,N,Y,N,N,N,N,Y,N,N,Y,Y)
  val FSUB_D   = Cat(FCMD_SUB,    N,Y,Y,Y,N,N,Y,N,N,N,N,Y,N,N,Y,Y)
  val FMUL_D   = Cat(FCMD_MUL,    N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,Y,Y)
  val FMADD_S  = Cat(FCMD_MADD,   N,Y,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,Y,Y)
  val FMSUB_S  = Cat(FCMD_MSUB,   N,Y,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,Y,Y)
  val FNMADD_S = Cat(FCMD_NMADD,  N,Y,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,Y,Y)
  val FNMSUB_S = Cat(FCMD_NMSUB,  N,Y,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,Y,Y)
  val FMADD_D  = Cat(FCMD_MADD,   N,Y,Y,Y,Y,N,N,N,N,N,N,Y,N,N,Y,Y)
  val FMSUB_D  = Cat(FCMD_MSUB,   N,Y,Y,Y,Y,N,N,N,N,N,N,Y,N,N,Y,Y)
  val FNMADD_D = Cat(FCMD_NMADD,  N,Y,Y,Y,Y,N,N,N,N,N,N,Y,N,N,Y,Y)
  val FNMSUB_D = Cat(FCMD_NMSUB,  N,Y,Y,Y,Y,N,N,N,N,N,N,Y,N,N,Y,Y)
  val FDIV_S   = Cat(FCMD_DIV,    N,Y,Y,Y,N,N,N,Y,N,N,N,N,Y,N,Y,Y)
  val FSQRT_S  = Cat(FCMD_SQRT,   N,Y,Y,N,N,Y,X,Y,N,N,N,N,N,Y,Y,Y)
  val FDIV_D   = Cat(FCMD_DIV,    N,Y,Y,Y,N,N,N,N,N,N,N,N,Y,N,Y,Y)
  val FSQRT_D  = Cat(FCMD_SQRT,   N,Y,Y,N,N,Y,X,N,N,N,N,N,N,Y,Y,Y)
}

class ScalarFPU extends HwachaModule
{
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

  val sfma = Module(new rocket.FPUFMAPipe(params(rocket.SFMALatency), 23, 9))
  sfma.io.in.valid := io.req.valid && io.req.bits.fma &&
                      io.req.bits.single
  sfma.io.in.bits := req

  val dfma = Module(new rocket.FPUFMAPipe(params(rocket.DFMALatency), 52, 12))
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

package hwacha 

import Chisel._
import cde.Parameters
import rocket.FPConstants._
import HardFloatHelper._

object ScalarFPUDecode {
  val FX: List[BitPat]=
                 List(FCMD_X,      X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X)
  val FCVT_S_W = List(FCMD_CVT_FI, N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,Y)
  val FCVT_S_WU= List(FCMD_CVT_FI, N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,Y)
  val FCVT_S_L = List(FCMD_CVT_FI, N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,Y)
  val FCVT_S_LU= List(FCMD_CVT_FI, N,Y,N,N,N,X,X,Y,Y,N,N,N,N,N,Y,Y)
  val FCVT_D_W = List(FCMD_CVT_FI, N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,Y)
  val FCVT_D_WU= List(FCMD_CVT_FI, N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,Y)
  val FCVT_D_L = List(FCMD_CVT_FI, N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,Y)
  val FCVT_D_LU= List(FCMD_CVT_FI, N,Y,N,N,N,X,X,N,Y,N,N,N,N,N,Y,Y)
  val FCLASS_S = List(FCMD_MV_XF,  N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,N)
  val FCLASS_D = List(FCMD_MV_XF,  N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,N)
  val FCVT_W_S = List(FCMD_CVT_IF, N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,Y)
  val FCVT_WU_S= List(FCMD_CVT_IF, N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,Y)
  val FCVT_L_S = List(FCMD_CVT_IF, N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,Y)
  val FCVT_LU_S= List(FCMD_CVT_IF, N,N,Y,N,N,N,X,Y,N,Y,N,N,N,N,Y,Y)
  val FCVT_W_D = List(FCMD_CVT_IF, N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,Y)
  val FCVT_WU_D= List(FCMD_CVT_IF, N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,Y)
  val FCVT_L_D = List(FCMD_CVT_IF, N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,Y)
  val FCVT_LU_D= List(FCMD_CVT_IF, N,N,Y,N,N,N,X,N,N,Y,N,N,N,N,Y,Y)
  val FCVT_S_S = List(FCMD_CVT_FF, N,N,Y,N,N,N,X,Y,N,N,Y,N,N,N,Y,Y) // special op
  val FCVT_S_D = List(FCMD_CVT_FF, N,Y,Y,N,N,N,X,Y,N,N,Y,N,N,N,Y,Y)
  val FCVT_D_S = List(FCMD_CVT_FF, N,Y,Y,N,N,N,X,N,N,N,Y,N,N,N,Y,Y)
  val FEQ_S    = List(FCMD_CMP,    N,N,Y,Y,N,N,N,Y,N,Y,N,N,N,N,N,Y)
  val FLT_S    = List(FCMD_CMP,    N,N,Y,Y,N,N,N,Y,N,Y,N,N,N,N,N,Y)
  val FLE_S    = List(FCMD_CMP,    N,N,Y,Y,N,N,N,Y,N,Y,N,N,N,N,N,Y)
  val FEQ_D    = List(FCMD_CMP,    N,N,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,Y)
  val FLT_D    = List(FCMD_CMP,    N,N,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,Y)
  val FLE_D    = List(FCMD_CMP,    N,N,Y,Y,N,N,N,N,N,Y,N,N,N,N,N,Y)
  val FSGNJ_S  = List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,N)
  val FSGNJN_S = List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,N)
  val FSGNJX_S = List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,N)
  val FSGNJ_D  = List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N)
  val FSGNJN_D = List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N)
  val FSGNJX_D = List(FCMD_SGNJ,   N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,N)
  val FMIN_S   = List(FCMD_MINMAX, N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,Y)
  val FMAX_S   = List(FCMD_MINMAX, N,Y,Y,Y,N,N,N,Y,N,N,Y,N,N,N,N,Y)
  val FMIN_D   = List(FCMD_MINMAX, N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y)
  val FMAX_D   = List(FCMD_MINMAX, N,Y,Y,Y,N,N,N,N,N,N,Y,N,N,N,N,Y)
  val FADD_S   = List(FCMD_ADD,    N,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y,Y)
  val FSUB_S   = List(FCMD_SUB,    N,Y,Y,Y,N,N,Y,Y,N,N,N,Y,N,N,Y,Y)
  val FMUL_S   = List(FCMD_MUL,    N,Y,Y,Y,N,N,N,Y,N,N,N,Y,N,N,Y,Y)
  val FADD_D   = List(FCMD_ADD,    N,Y,Y,Y,N,N,Y,N,N,N,N,Y,N,N,Y,Y)
  val FSUB_D   = List(FCMD_SUB,    N,Y,Y,Y,N,N,Y,N,N,N,N,Y,N,N,Y,Y)
  val FMUL_D   = List(FCMD_MUL,    N,Y,Y,Y,N,N,N,N,N,N,N,Y,N,N,Y,Y)
  val FMADD_S  = List(FCMD_MADD,   N,Y,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,Y,Y)
  val FMSUB_S  = List(FCMD_MSUB,   N,Y,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,Y,Y)
  val FNMADD_S = List(FCMD_NMADD,  N,Y,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,Y,Y)
  val FNMSUB_S = List(FCMD_NMSUB,  N,Y,Y,Y,Y,N,N,Y,N,N,N,Y,N,N,Y,Y)
  val FMADD_D  = List(FCMD_MADD,   N,Y,Y,Y,Y,N,N,N,N,N,N,Y,N,N,Y,Y)
  val FMSUB_D  = List(FCMD_MSUB,   N,Y,Y,Y,Y,N,N,N,N,N,N,Y,N,N,Y,Y)
  val FNMADD_D = List(FCMD_NMADD,  N,Y,Y,Y,Y,N,N,N,N,N,N,Y,N,N,Y,Y)
  val FNMSUB_D = List(FCMD_NMSUB,  N,Y,Y,Y,Y,N,N,N,N,N,N,Y,N,N,Y,Y)
  val FDIV_S   = List(FCMD_DIV,    N,Y,Y,Y,N,N,N,Y,N,N,N,N,Y,N,Y,Y)
  val FSQRT_S  = List(FCMD_SQRT,   N,Y,Y,N,N,Y,X,Y,N,N,N,N,N,Y,Y,Y)
  val FDIV_D   = List(FCMD_DIV,    N,Y,Y,Y,N,N,N,N,N,N,N,N,Y,N,Y,Y)
  val FSQRT_D  = List(FCMD_SQRT,   N,Y,Y,N,N,Y,X,N,N,N,N,N,N,Y,Y,Y)
}

class HwachaFPInput(implicit p: Parameters) extends rocket.FPInput {
  val bSRegs = log2Up(p(HwachaNScalarRegs))
  val in_fmt = UInt(width = 2)
  val tag = UInt(width = bSRegs)
  override def cloneType = new HwachaFPInput()(p).asInstanceOf[this.type]
}

class HwachaFPResult(implicit p: Parameters) extends rocket.FPResult {
  val bSRegs = log2Up(p(HwachaNScalarRegs))
  val tag = UInt(width = bSRegs)
  override def cloneType = new HwachaFPResult()(p).asInstanceOf[this.type]
}

class ScalarFPUInterface(implicit p: Parameters) extends HwachaModule()(p) with Packing {
  val io = new Bundle {
    val hwacha = new Bundle {
      val req = Decoupled(new HwachaFPInput).flip
      val resp = Decoupled(new HwachaFPResult)
    }
    val rocc = new Bundle {
      val req = Decoupled(new rocket.FPInput)
      val resp = Decoupled(new rocket.FPResult).flip
    }
  }

  val pending_fpu = Reg(init=Bool(false))
  val pending_fpu_req = Reg(new HwachaFPInput)
  val pending_fpu_typ = Reg(Bits(width=2))

  val reqq = Module(new Queue(new HwachaFPInput, 2))
  val respq = Module(new Queue(new rocket.FPResult, 2))

  reqq.io.enq <> io.hwacha.req

  private val hreq = reqq.io.deq.bits

  val enq_rocc = !(hreq.cmd === FCMD_CVT_FF && !hreq.wen && !hreq.toint && !hreq.fromint)
  val mask_rocc_req_ready = !enq_rocc || io.rocc.req.ready
  val mask_respq_enq_ready = enq_rocc || respq.io.enq.ready

  def fire(exclude: Bool, include: Bool*) = {
    val rvs = Seq(!pending_fpu,
      reqq.io.deq.valid, mask_rocc_req_ready, mask_respq_enq_ready)
    (rvs.filter(_ ne exclude) ++ include).reduce(_ && _)
  }

  reqq.io.deq.ready := fire(reqq.io.deq.valid)
  io.rocc.req.valid := fire(mask_rocc_req_ready, enq_rocc)

  when (fire(null)) {
    pending_fpu := Bool(true)
    pending_fpu_req := hreq
    pending_fpu_typ := Mux(hreq.fromint, hreq.in_fmt, hreq.typ)
  }

  val h2s =
    List(hreq.in1, hreq.in2, hreq.in3) map { case in =>
      val h2s = Module(new hardfloat.RecFNToRecFN(5, 11, 8, 24))
      h2s.io.in := recode_hp(in)
      h2s.io.roundingMode := hreq.rm
      // XXX: use h2s.io.exceptionFlags
      h2s.io.out
    }

  io.rocc.req.bits <> hreq

  val rec_s_in1 = Cat(SInt(-1,32), recode_sp(hreq.in1))
  io.rocc.req.bits.in1 :=
    Mux(hreq.fromint, hreq.in1,
      Mux(hreq.in_fmt === UInt(0), rec_s_in1,
        Mux(hreq.in_fmt === UInt(1), recode_dp(hreq.in1),
          h2s(0))))
  io.rocc.req.bits.in2 :=
    Mux(hreq.in_fmt === UInt(0), Cat(SInt(-1,32), recode_sp(hreq.in2)),
      Mux(hreq.in_fmt === UInt(1), recode_dp(hreq.in2),
        h2s(1)))
  io.rocc.req.bits.in3 :=
    Mux(hreq.in_fmt === UInt(0), Cat(SInt(-1,32), recode_sp(hreq.in3)),
      Mux(hreq.in_fmt === UInt(1), recode_dp(hreq.in3),
        h2s(2)))

  respq.io.enq.valid := io.rocc.resp.valid || fire(mask_respq_enq_ready, !enq_rocc)
  respq.io.enq.bits := io.rocc.resp.bits
  when (fire(null, !enq_rocc)) {
    respq.io.enq.bits.data := Mux(hreq.in_fmt === UInt(0), rec_s_in1, h2s(0))
  }

  respq.io.deq.ready := io.hwacha.resp.ready
  io.hwacha.resp.valid := respq.io.deq.valid
  io.rocc.resp.ready := respq.io.enq.ready

  when (respq.io.deq.fire()) {
    pending_fpu := Bool(false)
  }

  private val rresp = respq.io.deq.bits
  private val hresp = io.hwacha.resp.bits

  val s2h = Module(new hardfloat.RecFNToRecFN(8, 24, 5, 11))
  s2h.io.in := rresp.data
  s2h.io.roundingMode := pending_fpu_req.rm
  // XXX: use s2h.io.exceptionFlags

  val unrec_h = ieee_hp(s2h.io.out)
  val unrec_s = ieee_sp(rresp.data)
  val unrec_d = ieee_dp(rresp.data)
  val unrec_fpu_resp =
    Mux(pending_fpu_typ === UInt(0), expand_float_s(unrec_s),
      Mux(pending_fpu_typ === UInt(1), unrec_d,
        expand_float_h(unrec_h)))

  hresp.tag := pending_fpu_req.tag
  hresp.data :=
    Mux(pending_fpu_req.toint, rresp.data(63, 0), unrec_fpu_resp)
}

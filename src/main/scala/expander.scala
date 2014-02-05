package hwacha

import Chisel._
import Node._
import Constants._

class io_vxu_expand_to_hazard extends Bundle
{
  val ren = Bool()
  val wen = Bool()
  val wen_mask = Bool()
}

class io_expand_to_xcpt_handler extends Bundle
{
  val empty = Bool(OUTPUT)
}

class Expander(implicit conf: HwachaConfiguration) extends Module 
{
  val io = new Bundle {
    val seq_to_expand = new io_vxu_seq_to_expand().asInput
    val expand_to_hazard = new io_vxu_expand_to_hazard().asOutput

    val seq = new io_vxu_seq_fu().asInput
    val seq_fn = new io_vxu_seq_fn().asInput
    val seq_regid_imm = new io_vxu_seq_regid_imm().asInput

    val laneuop = new LaneUopIO

    val expand_to_xcpt = new io_expand_to_xcpt_handler()
  }

  class BuildExpander[T<:Data](gen: T, n: Int)
  {
    val valid = Vec.fill(n){Reg(init=Bool(false))}
    val bits = Vec.fill(n){Reg(gen)}

    (0 until n).reverse.foreach(i => ({
      val step_en = if (i==n-1) Bool(false) else valid(i+1)
      valid(i) := step_en
      if (i==n-1) {
        bits(i) := bits(i).fromBits(Bits(0))
      } else {
        when (step_en) {
          bits(i) := bits(i+1)
        }
      }
    }))

    def ondeck = Pipe(valid(0), bits(0), 0)
  }

  def rblen(b: Bits) = new BankUopRead().rblen.fromBits(b)

  val rexp = new BuildExpander(new BankUopRead, conf.shift_buf_read)
  val wexp = new BuildExpander(new BankUopWrite, conf.shift_buf_write)
  val viuexp = new BuildExpander(new BankUopVIU, conf.shift_buf_read)
  val vau0exp = new BuildExpander(new LfuncUopVAU0, conf.shift_buf_read)
  val vau1exp = new BuildExpander(new LfuncUopVAU1, conf.shift_buf_read)
  val vau2exp = new BuildExpander(new LfuncUopVAU2, conf.shift_buf_read)
  val vguexp = new BuildExpander(new LfuncUopVGU, conf.shift_buf_read)
  val vluexp = new BuildExpander(new LfuncUopVLU, conf.shift_buf_read)
  val vsuexp = new BuildExpander(new LfuncUopVSU, conf.shift_buf_read)

  when (io.seq.viu) 
  {
    val rports2 = io.seq_fn.viu(RG_VIU_T) === Cat(ML,MR)

    rexp.valid(0) := Bool(true)
    rexp.bits(0).last := io.seq_to_expand.last
    rexp.bits(0).cnt := io.seq_regid_imm.cnt
    rexp.bits(0).addr := io.seq_regid_imm.vs
    rexp.bits(0).oplen := Bits("b01")
    rexp.bits(0).rblen := rblen(Bits(0))

    when (rports2) {
      rexp.valid(1) := Bool(true)
      rexp.bits(1).last := io.seq_to_expand.last
      rexp.bits(1).cnt := io.seq_regid_imm.cnt
      rexp.bits(1).addr := io.seq_regid_imm.vt
      rexp.bits(1).oplen := Bits("b00")
      rexp.bits(1).rblen := rblen(Bits(0))
    }

    when (io.seq_fn.viu(RG_VIU_T) === Cat(M0,MR)) {
      rexp.bits(0).addr := io.seq_regid_imm.vt
    }

    val viu_wptr = Mux(rports2, Bits(conf.int_stages+2), Bits(conf.int_stages+1))

    wexp.valid(viu_wptr) := Bool(true)
    wexp.bits(viu_wptr).last := io.seq_to_expand.last
    wexp.bits(viu_wptr).cnt := io.seq_regid_imm.cnt
    wexp.bits(viu_wptr).addr := io.seq_regid_imm.vd
    wexp.bits(viu_wptr).sel := Bits(4)

    val viu_rest = io.seq_fn.viu(SZ_VIU_DW + SZ_VIU_FP + SZ_VIU_OP - 1, 0)
    val viu_t1 = io.seq_fn.viu(RG_VIU_T1)
    val viu_t0 = io.seq_fn.viu(RG_VIU_T0)

    when (rports2) {
      viuexp.valid(2) := Bool(true)
      viuexp.bits(2).cnt := io.seq_regid_imm.cnt
      when (io.seq_regid_imm.vs_zero) {
        when (io.seq_regid_imm.vt_zero) { viuexp.bits(2).fn := Cat(M0, M0, viu_rest) }
        .otherwise { viuexp.bits(2).fn := Cat(M0, viu_t1, viu_rest) }
      }
      .otherwise {
        when (io.seq_regid_imm.vt_zero) { viuexp.bits(2).fn := Cat(viu_t0, M0, viu_rest) }
        .otherwise { viuexp.bits(2).fn := io.seq_fn.viu }
      }
    }
    .otherwise {
      viuexp.valid(1) := Bool(true)
      viuexp.bits(1).cnt := io.seq_regid_imm.cnt
      viuexp.bits(1).fn := io.seq_fn.viu
      viuexp.bits(1).utidx := io.seq_regid_imm.utidx
      viuexp.bits(1).imm := io.seq_regid_imm.imm

      when (io.seq_regid_imm.vs_zero) { viuexp.bits(1).fn := Cat(M0, viu_t1, viu_rest) }
    }
  }

  when (io.seq.vau0) 
  {
    rexp.valid(0) := Bool(true)
    rexp.bits(0).last := io.seq_to_expand.last
    rexp.bits(0).cnt := io.seq_regid_imm.cnt
    rexp.bits(0).addr := io.seq_regid_imm.vs
    rexp.bits(0).oplen := Bits("b01")
    rexp.bits(0).rblen := rblen(Bits(0))

    rexp.valid(1) := Bool(true)
    rexp.bits(1).last := io.seq_to_expand.last
    rexp.bits(1).cnt := io.seq_regid_imm.cnt
    rexp.bits(1).addr := io.seq_regid_imm.vt
    rexp.bits(1).oplen := Bits("b00")
    rexp.bits(1).rblen := rblen(Bits("b0000_0011"))

    when (io.seq_regid_imm.vs_zero) { rexp.bits(1).rblen(0) := Bool(false) }
    when (io.seq_regid_imm.vt_zero) { rexp.bits(1).rblen(1) := Bool(false) }

    val vau0_wptr = Bits(conf.imul_stages+2)

    wexp.valid(vau0_wptr) := Bool(true)
    wexp.bits(vau0_wptr).last := io.seq_to_expand.last
    wexp.bits(vau0_wptr).cnt := io.seq_regid_imm.cnt
    wexp.bits(vau0_wptr).addr := io.seq_regid_imm.vd
    wexp.bits(vau0_wptr).sel := Bits(0)

    vau0exp.valid(1) := Bool(true)
    vau0exp.bits(1).cnt := io.seq_regid_imm.cnt
    vau0exp.bits(1).fn := io.seq_fn.vau0
  }

  when (io.seq.vau1)
  {
    val rports3 = FN_VAU1_FMA(io.seq_fn.vau1)

    when (rports3) {
      rexp.valid(0) := Bool(true)
      rexp.bits(0).last := io.seq_to_expand.last
      rexp.bits(0).cnt := io.seq_regid_imm.tcnt // turbo
      rexp.bits(0).addr := io.seq_regid_imm.vs
      rexp.bits(0).oplen := Bits("b10")
      rexp.bits(0).rblen := rblen(Bits(0))

      rexp.valid(1) := Bool(true)
      rexp.bits(1).last := io.seq_to_expand.last
      rexp.bits(1).cnt := io.seq_regid_imm.tcnt
      rexp.bits(1).addr := io.seq_regid_imm.vt
      rexp.bits(1).oplen := Bits("b01")
      rexp.bits(1).rblen := rblen(Bits(0))
      
      rexp.valid(2) := Bool(true)
      rexp.bits(2).last := io.seq_to_expand.last
      rexp.bits(2).cnt := io.seq_regid_imm.tcnt
      rexp.bits(2).addr := io.seq_regid_imm.vr
      rexp.bits(2).oplen := Bits("b00")
      rexp.bits(2).rblen := rblen(Bits("b0001_1100"))

      when (io.seq_regid_imm.vs_zero) { rexp.bits(2).rblen(2) := Bool(false) }
      when (io.seq_regid_imm.vt_zero) { rexp.bits(2).rblen(3) := Bool(false) }
      when (io.seq_regid_imm.vr_zero) { rexp.bits(2).rblen(4) := Bool(false) }
    }
    .otherwise {
      rexp.valid(0) := Bool(true)
      rexp.bits(0).last := io.seq_to_expand.last
      rexp.bits(0).cnt := io.seq_regid_imm.tcnt
      rexp.bits(0).addr := io.seq_regid_imm.vs
      rexp.bits(0).oplen := Bits("b10")
      rexp.bits(0).rblen := rblen(Bits(0))
      
      rexp.valid(1) := Bool(true)
      rexp.bits(1).last := io.seq_to_expand.last
      rexp.bits(1).cnt := io.seq_regid_imm.tcnt
      rexp.bits(1).addr := io.seq_regid_imm.vt
      rexp.bits(1).oplen := Bits("b00")
      rexp.bits(1).rblen := rblen(Bits("b0001_0100"))

      when (io.seq_regid_imm.vs_zero) { rexp.bits(1).rblen(2) := Bool(false) }
      when (io.seq_regid_imm.vt_zero) { rexp.bits(1).rblen(4) := Bool(false) }
    }

    val vau1_wptr = Mux(rports3, Bits(conf.fma_stages+3), Bits(conf.fma_stages+2))

    wexp.valid(vau1_wptr) := Bool(true)
    wexp.bits(vau1_wptr).last := io.seq_to_expand.last
    wexp.bits(vau1_wptr).cnt := io.seq_regid_imm.tcnt
    wexp.bits(vau1_wptr).addr := io.seq_regid_imm.vd
    wexp.bits(vau1_wptr).sel := Bits(1)

    when (rports3) {
      vau1exp.valid(2) := Bool(true)
      vau1exp.bits(2).cnt := io.seq_regid_imm.cnt
      vau1exp.bits(2).fn := io.seq_fn.vau1
    }
    .otherwise {
      vau1exp.valid(1) := Bool(true)
      vau1exp.bits(1).cnt := io.seq_regid_imm.cnt
      vau1exp.bits(1).fn := io.seq_fn.vau1
    }
  }

  when (io.seq.vau2)
  {
    rexp.valid(0) := Bool(true)
    rexp.bits(0).last := io.seq_to_expand.last
    rexp.bits(0).cnt := io.seq_regid_imm.cnt
    rexp.bits(0).addr := io.seq_regid_imm.vs
    rexp.bits(0).oplen := Bits("b00")
    rexp.bits(0).rblen := rblen(Bits("b0010_0000"))

    when (io.seq_regid_imm.vs_zero) { rexp.bits(0).rblen(5) := Bool(false) }

    val vau2_wptr = Bits(conf.fconv_stages+1)

    wexp.valid(vau2_wptr) := Bool(true)
    wexp.bits(vau2_wptr).last := io.seq_to_expand.last
    wexp.bits(vau2_wptr).cnt := io.seq_regid_imm.cnt
    wexp.bits(vau2_wptr).addr := io.seq_regid_imm.vd
    wexp.bits(vau2_wptr).sel := Bits(2)

    vau2exp.valid(0) := Bool(true)
    vau2exp.bits(0).cnt := io.seq_regid_imm.cnt
    vau2exp.bits(0).fn := io.seq_fn.vau2
  }

  when (io.seq.vaq)
  {
    rexp.valid(0) := Bool(true)
    rexp.bits(0).last := io.seq_to_expand.last
    rexp.bits(0).cnt := io.seq_regid_imm.cnt

    when (io.seq_regid_imm.utmemop) {
      rexp.bits(0).addr := io.seq_regid_imm.vs
      rexp.bits(0).oplen := Bits("b00")
      rexp.bits(0).rblen := rblen(Bits("b0100_0000"))

      when (io.seq_regid_imm.vs_zero) { rexp.bits(0).rblen(6) := Bool(false) }
    }
    .otherwise {
      rexp.bits(0).rblen := rblen(Bits(0))
    }

    vguexp.valid(0) := Bool(true)
    vguexp.bits(0).cnt := io.seq_regid_imm.cnt
    vguexp.bits(0).mem := io.seq_regid_imm.mem
    vguexp.bits(0).imm := io.seq_regid_imm.imm
    vguexp.bits(0).imm2 := io.seq_regid_imm.imm2
    vguexp.bits(0).utmemop := io.seq_regid_imm.utmemop
  }

  when (io.seq.vldq)
  {
    wexp.valid(0) := Bool(true)
    wexp.bits(0).last := io.seq_to_expand.last
    wexp.bits(0).cnt := io.seq_regid_imm.tcnt
    wexp.bits(0).addr := io.seq_regid_imm.vd
    wexp.bits(0).sel := Bits(3)

    vluexp.valid(0) := Bool(true)
    vluexp.bits(0).cnt := io.seq_regid_imm.cnt
  }

  when (io.seq.vsdq)
  {
    rexp.valid(0) := Bool(true)
    rexp.bits(0).last := io.seq_to_expand.last
    rexp.bits(0).cnt := io.seq_regid_imm.tcnt
    rexp.bits(0).addr := io.seq_regid_imm.vt
    rexp.bits(0).oplen := Bits("b00")
    rexp.bits(0).rblen := rblen(Bits("b1000_0000"))

    when (io.seq_regid_imm.vt_zero) { rexp.bits(0).rblen(7) := Bool(false) }

    vsuexp.valid(0) := Bool(true)
    vsuexp.bits(0).cnt := io.seq_regid_imm.cnt
    vsuexp.bits(0).mem := io.seq_regid_imm.mem
  }

  io.expand_to_hazard.ren := rexp.valid(0)
  io.expand_to_hazard.wen := wexp.valid(0)
  io.expand_to_hazard.wen_mask := wexp.valid(0)

  io.laneuop.read <> rexp.ondeck
  io.laneuop.write <> wexp.ondeck
  io.laneuop.viu <> viuexp.ondeck
  io.laneuop.vau0 <> vau0exp.ondeck
  io.laneuop.vau1 <> vau1exp.ondeck
  io.laneuop.vau2 <> vau2exp.ondeck
  io.laneuop.vgu <> vguexp.ondeck
  io.laneuop.vlu <> vluexp.ondeck
  io.laneuop.vsu <> vsuexp.ondeck

  io.expand_to_xcpt.empty :=
    !(List(rexp, wexp, viuexp, vau0exp, vau1exp, vau2exp, vguexp, vluexp, vsuexp).map(e => e.valid.toBits().orR()).reduce(_||_))
}

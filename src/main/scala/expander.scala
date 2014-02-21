package hwacha

import Chisel._
import Node._
import Constants._

class io_vxu_expand_to_hazard extends Bundle
{
  val ren = Bool()
  val wen = Bool()
}

class io_expand_to_xcpt_handler extends Bundle
{
  val empty = Bool(OUTPUT)
}

class Expander(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val expand_to_hazard = new io_vxu_expand_to_hazard().asOutput

    val seqop = new SequencerOpIO().flip
    val laneop = new LaneOpIO

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

  def rblen(b: Bits) = new ReadBankOp().rblen.fromBits(b)

  val rexp = new BuildExpander(new ReadBankOp, conf.shift_buf_read)
  val wexp = new BuildExpander(new WriteBankOp, conf.shift_buf_write)
  val viuexp = new BuildExpander(new VIUOp, conf.shift_buf_read)
  val vau0exp = new BuildExpander(new VAU0Op, conf.shift_buf_read)
  val vau1exp = new BuildExpander(new VAU1Op, conf.shift_buf_read)
  val vau2exp = new BuildExpander(new VAU2Op, conf.shift_buf_read)
  val vguexp = new BuildExpander(new VGUOp, conf.shift_buf_read)
  val vluexp = new BuildExpander(new VLUOp, conf.shift_buf_read)
  val vsuexp = new BuildExpander(new VSUOp, conf.shift_buf_read)

  when (io.seqop.valid) {

    when (io.seqop.bits.active.viu) {
      rexp.valid(0) := Bool(true)
      rexp.bits(0).last := io.seqop.bits.last
      rexp.bits(0).cnt := io.seqop.bits.cnt
      rexp.bits(0).addr := io.seqop.bits.reg.vs.id
      rexp.bits(0).oplen := Bits("b01")
      rexp.bits(0).rblen := rblen(Bits(0))

      when (io.seqop.bits.fn.viu.rtype()) {
        rexp.valid(1) := Bool(true)
        rexp.bits(1).last := io.seqop.bits.last
        rexp.bits(1).cnt := io.seqop.bits.cnt
        rexp.bits(1).addr := io.seqop.bits.reg.vt.id
        rexp.bits(1).oplen := Bits("b00")
        rexp.bits(1).rblen := rblen(Bits(0))
      }

      when (io.seqop.bits.fn.viu.itype()) {
        rexp.bits(0).addr := io.seqop.bits.reg.vt.id
      }

      val viu_wptr =
        Mux(io.seqop.bits.fn.viu.rtype(), Bits(conf.int_stages+2), Bits(conf.int_stages+1))

      wexp.valid(viu_wptr) := Bool(true)
      wexp.bits(viu_wptr).last := io.seqop.bits.last
      wexp.bits(viu_wptr).cnt := io.seqop.bits.cnt
      wexp.bits(viu_wptr).addr := io.seqop.bits.reg.vd.id
      wexp.bits(viu_wptr).sel := Bits(4)

      when (io.seqop.bits.fn.viu.rtype()) {
        viuexp.valid(2) := Bool(true)
        viuexp.bits(2).cnt := io.seqop.bits.cnt
        viuexp.bits(2).fn := io.seqop.bits.fn.viu

        when (io.seqop.bits.reg.vs.zero) { viuexp.bits(2).fn.t0 := M0 }
        when (io.seqop.bits.reg.vt.zero) { viuexp.bits(2).fn.t1 := M0 }
      }
      .otherwise {
        viuexp.valid(1) := Bool(true)
        viuexp.bits(1).cnt := io.seqop.bits.cnt
        viuexp.bits(1).fn := io.seqop.bits.fn.viu
        viuexp.bits(1).utidx := io.seqop.bits.utidx
        viuexp.bits(1).imm := io.seqop.bits.imm.imm

        when (io.seqop.bits.reg.vs.zero) { viuexp.bits(1).fn.t0 := M0 }
      }
    }

    when (io.seqop.bits.active.vau0) {
      rexp.valid(0) := Bool(true)
      rexp.bits(0).last := io.seqop.bits.last
      rexp.bits(0).cnt := io.seqop.bits.cnt
      rexp.bits(0).addr := io.seqop.bits.reg.vs.id
      rexp.bits(0).oplen := Bits("b01")
      rexp.bits(0).rblen := rblen(Bits(0))

      rexp.valid(1) := Bool(true)
      rexp.bits(1).last := io.seqop.bits.last
      rexp.bits(1).cnt := io.seqop.bits.cnt
      rexp.bits(1).addr := io.seqop.bits.reg.vt.id
      rexp.bits(1).oplen := Bits("b00")
      rexp.bits(1).rblen := rblen(Bits("b0000_0011"))

      when (io.seqop.bits.reg.vs.zero) { rexp.bits(1).rblen(0) := Bool(false) }
      when (io.seqop.bits.reg.vt.zero) { rexp.bits(1).rblen(1) := Bool(false) }

      val vau0_wptr = Bits(conf.imul_stages+2)

      wexp.valid(vau0_wptr) := Bool(true)
      wexp.bits(vau0_wptr).last := io.seqop.bits.last
      wexp.bits(vau0_wptr).cnt := io.seqop.bits.cnt
      wexp.bits(vau0_wptr).addr := io.seqop.bits.reg.vd.id
      wexp.bits(vau0_wptr).sel := Bits(0)

      vau0exp.valid(1) := Bool(true)
      vau0exp.bits(1).cnt := io.seqop.bits.cnt
      vau0exp.bits(1).fn := io.seqop.bits.fn.vau0
    }

    when (io.seqop.bits.active.vau1) {
      when (io.seqop.bits.fn.vau1.fma()) {
        rexp.valid(0) := Bool(true)
        rexp.bits(0).last := io.seqop.bits.last
        rexp.bits(0).cnt := io.seqop.bits.cnt
        rexp.bits(0).addr := io.seqop.bits.reg.vs.id
        rexp.bits(0).oplen := Bits("b10")
        rexp.bits(0).rblen := rblen(Bits(0))

        rexp.valid(1) := Bool(true)
        rexp.bits(1).last := io.seqop.bits.last
        rexp.bits(1).cnt := io.seqop.bits.cnt
        rexp.bits(1).addr := io.seqop.bits.reg.vt.id
        rexp.bits(1).oplen := Bits("b01")
        rexp.bits(1).rblen := rblen(Bits(0))

        rexp.valid(2) := Bool(true)
        rexp.bits(2).last := io.seqop.bits.last
        rexp.bits(2).cnt := io.seqop.bits.cnt
        rexp.bits(2).addr := io.seqop.bits.reg.vr.id
        rexp.bits(2).oplen := Bits("b00")
        rexp.bits(2).rblen := rblen(Bits("b0001_1100"))

        when (io.seqop.bits.reg.vs.zero) { rexp.bits(2).rblen(2) := Bool(false) }
        when (io.seqop.bits.reg.vt.zero) { rexp.bits(2).rblen(3) := Bool(false) }
        when (io.seqop.bits.reg.vr.zero) { rexp.bits(2).rblen(4) := Bool(false) }
      }
      .otherwise {
        rexp.valid(0) := Bool(true)
        rexp.bits(0).last := io.seqop.bits.last
        rexp.bits(0).cnt := io.seqop.bits.cnt
        rexp.bits(0).addr := io.seqop.bits.reg.vs.id
        rexp.bits(0).oplen := Bits("b10")
        rexp.bits(0).rblen := rblen(Bits(0))

        rexp.valid(1) := Bool(true)
        rexp.bits(1).last := io.seqop.bits.last
        rexp.bits(1).cnt := io.seqop.bits.cnt
        rexp.bits(1).addr := io.seqop.bits.reg.vt.id
        rexp.bits(1).oplen := Bits("b00")
        rexp.bits(1).rblen := rblen(Bits("b0001_0100"))

        when (io.seqop.bits.reg.vs.zero) { rexp.bits(1).rblen(2) := Bool(false) }
        when (io.seqop.bits.reg.vt.zero) { rexp.bits(1).rblen(4) := Bool(false) }
      }

      val vau1_wptr =
        Mux(io.seqop.bits.fn.vau1.fma(), Bits(conf.fma_stages+3), Bits(conf.fma_stages+2))

      wexp.valid(vau1_wptr) := Bool(true)
      wexp.bits(vau1_wptr).last := io.seqop.bits.last
      wexp.bits(vau1_wptr).cnt := io.seqop.bits.cnt
      wexp.bits(vau1_wptr).addr := io.seqop.bits.reg.vd.id
      wexp.bits(vau1_wptr).sel := Bits(1)

      when (io.seqop.bits.fn.vau1.fma()) {
        vau1exp.valid(2) := Bool(true)
        vau1exp.bits(2).cnt := io.seqop.bits.cnt
        vau1exp.bits(2).fn := io.seqop.bits.fn.vau1
      }
      .otherwise {
        vau1exp.valid(1) := Bool(true)
        vau1exp.bits(1).cnt := io.seqop.bits.cnt
        vau1exp.bits(1).fn := io.seqop.bits.fn.vau1
      }
    }

    when (io.seqop.bits.active.vau2) {
      rexp.valid(0) := Bool(true)
      rexp.bits(0).last := io.seqop.bits.last
      rexp.bits(0).cnt := io.seqop.bits.cnt
      rexp.bits(0).addr := io.seqop.bits.reg.vs.id
      rexp.bits(0).oplen := Bits("b00")
      rexp.bits(0).rblen := rblen(Bits("b0010_0000"))

      when (io.seqop.bits.reg.vs.zero) { rexp.bits(0).rblen(5) := Bool(false) }

      val vau2_wptr = Bits(conf.fconv_stages+1)

      wexp.valid(vau2_wptr) := Bool(true)
      wexp.bits(vau2_wptr).last := io.seqop.bits.last
      wexp.bits(vau2_wptr).cnt := io.seqop.bits.cnt
      wexp.bits(vau2_wptr).addr := io.seqop.bits.reg.vd.id
      wexp.bits(vau2_wptr).sel := Bits(2)

      vau2exp.valid(0) := Bool(true)
      vau2exp.bits(0).cnt := io.seqop.bits.cnt
      vau2exp.bits(0).fn := io.seqop.bits.fn.vau2
    }

    when (io.seqop.bits.active.vgu) {
      rexp.valid(0) := Bool(true)
      rexp.bits(0).last := io.seqop.bits.last
      rexp.bits(0).cnt := io.seqop.bits.cnt

      when (io.seqop.bits.fn.vmu.utmemop()) {
        rexp.bits(0).addr := io.seqop.bits.reg.vs.id
        rexp.bits(0).oplen := Bits("b00")
        rexp.bits(0).rblen := rblen(Bits("b0100_0000"))

        when (io.seqop.bits.reg.vs.zero) { rexp.bits(0).rblen(6) := Bool(false) }
      }
      .otherwise {
        rexp.bits(0).rblen := rblen(Bits(0))
      }

      vguexp.valid(0) := Bool(true)
      vguexp.bits(0).cnt := io.seqop.bits.cnt
      vguexp.bits(0).fn := io.seqop.bits.fn.vmu
      vguexp.bits(0).base := io.seqop.bits.imm.imm
      vguexp.bits(0).stride := io.seqop.bits.imm.stride
    }

    when (io.seqop.bits.active.vlu) {
      wexp.valid(1) := Bool(true)
      wexp.bits(1).last := io.seqop.bits.last
      wexp.bits(1).cnt := io.seqop.bits.cnt
      wexp.bits(1).addr := io.seqop.bits.reg.vd.id
      wexp.bits(1).sel := Bits(3)

      vluexp.valid(0) := Bool(true)
      vluexp.bits(0).cnt := io.seqop.bits.cnt
      vluexp.bits(0).fn := io.seqop.bits.fn.vmu
    }

    when (io.seqop.bits.active.vsu) {
      rexp.valid(0) := Bool(true)
      rexp.bits(0).last := io.seqop.bits.last
      rexp.bits(0).cnt := io.seqop.bits.cnt
      rexp.bits(0).addr := io.seqop.bits.reg.vt.id
      rexp.bits(0).oplen := Bits("b00")
      rexp.bits(0).rblen := rblen(Bits("b1000_0000"))

      when (io.seqop.bits.reg.vt.zero) { rexp.bits(0).rblen(7) := Bool(false) }

      vsuexp.valid(0) := Bool(true)
      vsuexp.bits(0).cnt := io.seqop.bits.cnt
      vsuexp.bits(0).fn := io.seqop.bits.fn.vmu
    }

  }

  io.expand_to_hazard.ren := rexp.valid(0)
  io.expand_to_hazard.wen := wexp.valid(0)

  io.laneop.read <> rexp.ondeck
  io.laneop.write <> wexp.ondeck
  io.laneop.viu <> viuexp.ondeck
  io.laneop.vau0 <> vau0exp.ondeck
  io.laneop.vau1 <> vau1exp.ondeck
  io.laneop.vau2 <> vau2exp.ondeck
  io.laneop.vgu <> vguexp.ondeck
  io.laneop.vlu <> vluexp.ondeck
  io.laneop.vsu <> vsuexp.ondeck

  io.expand_to_xcpt.empty :=
    !(List(rexp, wexp, viuexp, vau0exp, vau1exp, vau2exp, vguexp, vluexp, vsuexp).map(e => e.valid.toBits().orR()).reduce(_||_))
}

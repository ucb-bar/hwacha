package hwacha

import Chisel._
import Node._

class LaneCtrl extends HwachaModule with LaneParameters
{
  val io = new Bundle {
    val op = new LaneOpIO().flip
    val uop = new MicroOpIO
  }

  class Systolic[T <: LaneOp](in: ValidIO[T])
  {
    val in_overflow = in.bits.strip > UInt(nSlices)
    val in_next_valid = in.valid && in_overflow
    val in_pred = Vec((0 until nSlices).map(UInt(_) < in.bits.strip)).toBits
    val in_popcnt = Mux(in_overflow, UInt(nSlices), in.bits.strip)

    val out = Valid(in.bits.clone).asDirectionless
    out.valid := Reg(next=in_next_valid)
    out.bits := RegEnable(in.bits, in_next_valid)
    out.bits.strip := RegEnable(in.bits.strip - in_popcnt, in_next_valid)

    when (reset) {
      out.valid := Bool(false)
    }
  }

  var sram_rf_read = io.op.sram.read
  var sram_rf_write = io.op.sram.write
  var opl = io.op.opl
  var xbar = io.op.xbar
  var viu = io.op.viu
  var vsu = io.op.vsu

  for (i <- 0 until nbanks) {
    val sys_sram_rf_read = new Systolic(sram_rf_read)
    val sys_sram_rf_write = new Systolic(sram_rf_write)
    val sys_opl = new Systolic(opl)
    val sys_xbar = new Systolic(xbar)
    val sys_vsu = new Systolic(vsu)
    val sys_viu = new Systolic(viu)

    io.uop.bank(i).sram.read.valid := sram_rf_read.valid
    io.uop.bank(i).sram.read.bits.addr := sram_rf_read.bits.addr
    io.uop.bank(i).sram.read.bits.pred := sys_sram_rf_read.in_pred

    io.uop.bank(i).sram.write.valid := sram_rf_write.valid
    io.uop.bank(i).sram.write.bits.addr := sram_rf_write.bits.addr
    io.uop.bank(i).sram.write.bits.selg := sram_rf_write.bits.selg
    io.uop.bank(i).sram.write.bits.wsel := sram_rf_write.bits.wsel
    io.uop.bank(i).sram.write.bits.pred := sys_sram_rf_write.in_pred

    io.uop.bank(i).opl.valid := opl.valid
    io.uop.bank(i).opl.bits.global.latch := opl.bits.global.latch
    io.uop.bank(i).opl.bits.global.selff := opl.bits.global.selff
    io.uop.bank(i).opl.bits.local.latch := opl.bits.local.latch
    io.uop.bank(i).opl.bits.local.selff := opl.bits.local.selff
    io.uop.bank(i).opl.bits.pred := sys_opl.in_pred

    io.uop.bank(i).xbar.valid := xbar.valid
    io.uop.bank(i).xbar.bits.en := xbar.bits.en
    io.uop.bank(i).xbar.bits.pred := sys_xbar.in_pred

    io.uop.bank(i).viu.valid := viu.valid
    io.uop.bank(i).viu.bits.fn := viu.bits.fn
    io.uop.bank(i).viu.bits.eidx := viu.bits.eidx
    io.uop.bank(i).viu.bits.pred := sys_viu.in_pred

    io.uop.bank(i).vsu.valid := vsu.valid
    io.uop.bank(i).vsu.bits.selff := vsu.bits.selff
    io.uop.bank(i).vsu.bits.pred := sys_vsu.in_pred

    sram_rf_read = sys_sram_rf_read.out
    sram_rf_write = sys_sram_rf_write.out
    opl = sys_opl.out
    xbar = sys_xbar.out
    viu = sys_viu.out
    vsu = sys_vsu.out

    for (j <- 0 until nFFRPorts) {
      io.uop.bank(i).ff.read(j).valid := Bool(false)
    }

    io.uop.bank(i).ff.write.valid := Bool(false)
  }

  class Shared[T <: LaneOp](in: ValidIO[T])
  {
    val reg_valid = Reg(Bool())
    val reg_bits = Reg(in.bits.clone)

    val strip = Mux(in.valid, in.bits.strip, reg_bits.strip)
    val overflow = strip > UInt(nSlices)
    val in_next_valid = overflow
    val valid = in.valid || reg_valid
    val bits = Mux(in.valid, in.bits, reg_bits)
    val pred = Vec((0 until nSlices).map(UInt(_) < strip)).toBits
    val popcnt = Mux(overflow, UInt(nSlices), strip)

    reg_valid := in_next_valid
    when (in.valid && overflow) {
      reg_bits := in.bits
    }
    when (in_next_valid) {
      reg_bits.strip := strip - popcnt
    }

    when (reset) {
      reg_valid := Bool(false)
    }
  }

  val vqu = new Shared(io.op.vqu)
  val vgu = new Shared(io.op.vgu)
  val vimu = new Shared(io.op.vimu)
  val vfmu0 = new Shared(io.op.vfmu0)
  val vfmu1 = new Shared(io.op.vfmu1)
  val vfcu = new Shared(io.op.vfcu)
  val vfvu = new Shared(io.op.vfvu)

  io.uop.vqu.valid := vqu.valid
  io.uop.vqu.bits.fn := vqu.bits.fn
  io.uop.vqu.bits.pred := vqu.pred

  io.uop.vgu.valid := vgu.valid
  io.uop.vgu.bits.fn := vgu.bits.fn
  io.uop.vgu.bits.pred := vgu.pred

  io.uop.vimu.valid := vimu.valid
  io.uop.vimu.bits.fn := vimu.bits.fn
  io.uop.vimu.bits.pred := vimu.pred

  io.uop.vfmu0.valid := vfmu0.valid
  io.uop.vfmu0.bits.fn := vfmu0.bits.fn
  io.uop.vfmu0.bits.pred := vfmu0.pred

  io.uop.vfmu1.valid := vfmu1.valid
  io.uop.vfmu1.bits.fn := vfmu1.bits.fn
  io.uop.vfmu1.bits.pred := vfmu1.pred

  io.uop.vfcu.valid := vfcu.valid
  io.uop.vfcu.bits.fn := vfcu.bits.fn
  io.uop.vfcu.bits.pred := vfcu.pred

  io.uop.vfvu.valid := vfvu.valid
  io.uop.vfvu.bits.fn := vfvu.bits.fn
  io.uop.vfvu.bits.pred := vfvu.pred
}

package hwacha

import Chisel._
import freechips.rocketchip.config._

class LaneCtrl(implicit p: Parameters) extends VXUModule()(p) {
  val io = new Bundle {
    val op = new LaneOpIO().flip
    val uop = new MicroOpIO
  }

  class Systolic[T <: LaneOp](in: ValidIO[T], multirate: Boolean) {
    val in_overflow = in.bits.strip > UInt(nSlices)
    val in_next_valid = in.valid && in_overflow
    val in_pred = Vec(
      for (i <- (0 until (if (multirate) nPack else 1)); j <- (0 until nSlices))
        yield UInt((i * nStrip) + j) < in.bits.strip).asUInt
    val in_popcnt = Mux(in_overflow, UInt(nSlices), in.bits.strip(bSlices, 0))

    if (confprec && !multirate)
      assert(!in.valid || (in.bits.strip <= UInt(nStrip)),
        "check strip count for single-rate systolic laneop: " +
        in.bits.getClass.getName)

    val out = Wire(Valid(in.bits.cloneType))
    out.valid := Reg(next=in_next_valid, init=Bool(false))
    out.bits := RegEnable(in.bits, in_next_valid)
    out.bits.strip := RegEnable(in.bits.strip - in_popcnt, in_next_valid)
  }

  def gen_systolic[T <: LaneOp, S <: MicroOp]
    (lop: ValidIO[T], uop: ValidIO[S], mr: Boolean = true) = {
      val sys = new Systolic(lop, mr)
      uop <> lop
      uop.bits.pred := sys.in_pred
      sys.out
  }

  def gen_vec_systolic[T <: LaneOp, S <: MicroOp]
    (lops: Seq[ValidIO[T]], uops: Seq[ValidIO[S]], mr: Boolean = true) = {
      Vec((lops zip uops) map { case (lop, uop) => gen_systolic(lop, uop, mr) })
  }

  io.uop.bank.foldLeft(io.op.sram.read)((lop, bio) => gen_systolic(lop, bio.sram.read))
  io.uop.bank.foldLeft(io.op.sram.write)((lop, bio) => gen_systolic(lop, bio.sram.write))
  io.uop.bank.foldLeft(io.op.pred.gread)((lop, bio) => gen_systolic(lop, bio.pred.gread))
  io.uop.bank.foldLeft(io.op.pred.pread)((lop, bio) => gen_systolic(lop, bio.pred.pread, false))
  io.uop.bank.foldLeft(io.op.pred.read)((lops, bio) => gen_vec_systolic(lops, bio.pred.read))
  io.uop.bank.foldLeft(io.op.pred.write)((lop, bio) => gen_systolic(lop, bio.pred.write))
  io.uop.bank.foldLeft(io.op.opl.global)((lops, bio) => gen_vec_systolic(lops, bio.opl.global))
  io.uop.bank.foldLeft(io.op.opl.local)((lops, bio) => gen_vec_systolic(lops, bio.opl.local))
  io.uop.bank.foldLeft(io.op.pdl.global)((lops, bio) => gen_vec_systolic(lops, bio.pdl.global))
  io.uop.bank.foldLeft(io.op.pdl.local)((lops, bio) => gen_vec_systolic(lops, bio.pdl.local))
  io.uop.bank.foldLeft(io.op.sreg.local)((lops, bio) => gen_vec_systolic(lops, bio.sreg))
  io.uop.bank.foldLeft(io.op.xbar)((lops, bio) => gen_vec_systolic(lops, bio.xbar))
  io.uop.bank.foldLeft(io.op.pxbar)((lops, bio) => gen_vec_systolic(lops, bio.pxbar))
  io.uop.bank.foldLeft(io.op.viu)((lop, bio) => gen_systolic(lop, bio.viu))
  io.uop.bank.foldLeft(io.op.vipu)((lop, bio) => gen_systolic(lop, bio.vipu))
  io.uop.bank.foldLeft(io.op.vpu)((lop, bio) => gen_systolic(lop, bio.vpu, false))
  io.uop.bank.foldLeft(io.op.vsu)((lop, bio) => gen_systolic(lop, bio.vsu, false))

  class Shared[T <: LaneOp](in: ValidIO[T], multirate: Boolean = false) {
    val reg_valid = Reg(Bool())
    val reg_bits = Reg(in.bits.cloneType)

    val strip = Mux(in.valid, in.bits.strip, reg_bits.strip)
    val overflow = strip > UInt(nSlices)
    val in_next_valid = overflow
    val valid = in.valid || reg_valid
    val bits = Mux(in.valid, in.bits, reg_bits)
    val pred = Vec(
      for (i <- (0 until (if (multirate) nPack else 1)); j <- (0 until nSlices))
        yield UInt((i * nStrip) + j) < strip).asUInt
    val popcnt = Mux(overflow, UInt(nSlices), strip(bSlices, 0))

    if (confprec && !multirate)
      assert(!in.valid || (in.bits.strip <= UInt(nStrip)),
        "check strip count for single-rate shared laneop: " +
        in.bits.getClass.getName)

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

  val sreg = (0 until nGOPL).map { i => new Shared(io.op.sreg.global(i), true) }
  val vqu = new Shared(io.op.vqu)
  val vgu = new Shared(io.op.vgu)
  val vimu = new Shared(io.op.vimu)
  val vfmu = (0 until nVFMU) map { i => new Shared(io.op.vfmu(i), true) }
  val vfcu = new Shared(io.op.vfcu)
  val vfvu = new Shared(io.op.vfvu, true)

  (io.uop.sreg zip sreg) foreach { case (u, s) =>
    u.valid := s.valid
    u.bits.operand := s.bits.operand
    u.bits.rate := s.bits.rate
    u.bits.pred := s.pred
  }

  def connect_vfu[T <: LaneOp, S <: MicroOp]
    (uop: ValidIO[S], s: Shared[T], fn: (S, Shared[T])=>Unit) = {
      uop.valid := s.valid
      uop.bits.pred := s.pred
      fn(uop.bits, s)
  }
  connect_vfu(io.uop.vqu, vqu, (u: VQUMicroOp, s: Shared[VQULaneOp]) => u <> s.bits)
  connect_vfu(io.uop.vgu, vgu, (u: VGUMicroOp, s: Shared[VGULaneOp]) => u <> s.bits)
  connect_vfu(io.uop.vimu, vimu, (u: VIMUMicroOp, s: Shared[VIMULaneOp]) => u <> s.bits)
  (io.uop.vfmu zip vfmu) foreach { case (uop, shared) =>
    connect_vfu(uop, shared, (u: VFMUMicroOp, s: Shared[VFMULaneOp]) => u <> s.bits)
  }
  connect_vfu(io.uop.vfcu, vfcu, (u: VFCUMicroOp, s: Shared[VFCULaneOp]) => u <> s.bits)
  connect_vfu(io.uop.vfvu, vfvu, (u: VFVUMicroOp, s: Shared[VFVULaneOp]) => u <> s.bits)
}

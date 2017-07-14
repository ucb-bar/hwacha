package hwacha

import Chisel._
import freechips.rocketchip.config._
import DataGating._

class IMulOperand(implicit p: Parameters) extends VXUBundle()(p) {
  val fn = new VIMUFn
  val in0 = Bits(width = SZ_D)
  val in1 = Bits(width = SZ_D)
}

class IMulResult extends Bundle {
  val out = Bits(width = SZ_D)
}

class IMulSlice(implicit p: Parameters) extends VXUModule()(p) {
  val io = new Bundle {
    val req = Valid(new IMulOperand).flip
    val resp = Valid(new IMulResult)
  }

  val fn = io.req.bits.fn.dgate(io.req.valid)
  val in0 = dgate(io.req.valid, io.req.bits.in0)
  val in1 = dgate(io.req.valid, io.req.bits.in1)

  val sxl64 = fn.is(DW64, IM_MH, IM_MHSU)
  val sxr64 = fn.is(DW64, IM_MH)
  val zxl32 = fn.is(DW32, IM_MHU)
  val zxr32 = fn.is(DW32, IM_MHU, IM_MHSU)
  val sxl32 = fn.is(DW32, IM_MH, IM_MHSU)
  val sxr32 = fn.is(DW32, IM_MH)

  val lhs = Cat(
    in0(63) & sxl64,
    Fill(32, ~zxl32)&in0(63,32) | Fill(32, sxl32&in0(31)),
    in0(31,0)) //TODO: 65 bits
  val rhs = Cat(
    in1(63) & sxr64,
    Fill(32, ~zxr32)&in1(63,32) | Fill(32, sxr32&in1(31)),
    in1(31,0)) //TODO: 65 bits

  val mul_result = lhs.asSInt * rhs.asSInt //TODO:130 bits

  val result = Wire(new IMulResult)
  result.out := MuxCase(
    Bits(0), Array(
      fn.is(DW64, IM_M)    -> mul_result(63,0),
      fn.is(DW64, IM_MH)   -> mul_result(127,64),
      fn.is(DW64, IM_MHU)  -> mul_result(127,64),
      fn.is(DW64, IM_MHSU) -> mul_result(127,64),
      fn.is(DW32, IM_M)    -> Cat(Fill(32, mul_result(31)), mul_result(31,0)),
      fn.is(DW32, IM_MH)   -> Cat(Fill(32, mul_result(63)), mul_result(63,32)),
      fn.is(DW32, IM_MHU)  -> Cat(Fill(32, mul_result(63)), mul_result(63,32)),
      fn.is(DW32, IM_MHSU) -> Cat(Fill(32, mul_result(63)), mul_result(63,32))
    ))

  io.resp := Pipe(io.req.valid, result, stagesIMul)
}

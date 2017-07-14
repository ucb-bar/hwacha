package hwacha

import Chisel._
import freechips.rocketchip.config._

class PLUOperand(implicit p: Parameters) extends VXUBundle()(p) {
  val fn = new VIPUFn
  val in0 = Bool()
  val in1 = Bool()
  val in2 = Bool()
}

class PLUResult(implicit p: Parameters) extends VXUBundle()(p) {
  val out = Bool()
}

class PLUSlice(implicit p: Parameters) extends VXUModule()(p) {
  val io = new Bundle {
    val req = Valid(new PLUOperand).flip
    val resp = Valid(new PLUResult)
  }

  val op = io.req.bits.fn.op
  val s2 = Mux(io.req.bits.in2, op(7,4), op(3,0))
  val s1 = Mux(io.req.bits.in1, s2(3,2), s2(1,0))
  val s0 = Mux(io.req.bits.in0, s1(1), s1(0))

  val result = Wire(new PLUResult)
  result.out := s0

  //io.resp := Pipe(io.req.valid, result, stagesPLU)
  //TODO COLIN FIXME: Bug in chisel Pipe falsely? generating chisel3 compat error
  io.resp.valid := io.req.valid
  io.resp.bits := result
}

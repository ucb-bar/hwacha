package hwacha

import Chisel._
import cde.Parameters

class VRU(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val toicache = new FrontendIO // checked, matches vxu, icache has .flip
    val fromscalar_active = Bool(INPUT) 
    val fromscalar_req = Valid(new FrontendReq).flip
    val cmdqio = new CMDQIO().flip 
  }

  val delay = 4
  val vru_req = ShiftRegister(io.fromscalar_req,delay)
  io.toicache.req := vru_req
  io.toicache.active := ShiftRegister(io.fromscalar_active, delay)
  io.toicache.resp.ready := Bool(true)

  // addr regfile
  val arf = Mem(UInt(width = 64), 32)

  printf("CMD: %b\n", io.cmdqio.cmd.bits)
  printf("IMM: %b\n", io.cmdqio.imm.bits)
  printf("RD: %b\n", io.cmdqio.rd.bits)
    io.cmdqio.cmd.ready := Bool(true)
    io.cmdqio.imm.ready := Bool(true)
    io.cmdqio.rd.ready := Bool(true)

/*  when (io.cmdqio.cmd.valid && io.cmdqio.imm.valid && io.cmdqio.rd.valid) {
    printf("CMD: %b\n", io.cmdqio.cmd.bits)
    printf("IMM: %b\n", io.cmdqio.imm.bits)
    printf("RD: %b\n", io.cmdqio.rd.bits)

    // BAD
    io.cmdqio.cmd.ready := Bool(true)
    io.cmdqio.imm.ready := Bool(true)
    io.cmdqio.rd.ready := Bool(true)
  } .otherwise {

    // BAD
    io.cmdqio.cmd.ready := Bool(false)
    io.cmdqio.imm.ready := Bool(false)
    io.cmdqio.rd.ready := Bool(false)
  }
*/
}

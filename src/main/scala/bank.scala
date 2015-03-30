package hwacha

import Chisel._
import Node._
import Constants._
import Packing._
import scala.collection.mutable.ArrayBuffer

class BankOpIO extends Bundle with HwachaLaneParameters
{
  val sram = new Bundle {
    val read = Valid(new SRAMRFReadOp)
    val write = Valid(new SRAMRFWriteOp)
  }
  val ff = new Bundle {
    val read = Vec.fill(nFFRPorts){Valid(new FFRFReadOp)}
    val write = Valid(new FFRFWriteOp)
  }
  val opl = Valid(new OPLOp)
  val brq = Valid(new BRQOp)
  val viu = Valid(new VIUOp)
}

class BRQIO extends DecoupledIO(new BRQEntry)
class BWQIO extends DecoupledIO(new BWQEntry)

class BankRWIO extends Bundle with HwachaLaneParameters
{
  val rdata = Vec.fill(nOPL){new BankReadEntry().asOutput}
  val wdata = Vec.fill(nWSel){new BankWriteEntry().asInput}

  val brq = new BRQIO
  val bwq = new Bundle {
    val mem = new BWQIO().flip
    val fu = new BWQIO().flip
  }
}

class Bank extends HwachaModule with HwachaLaneParameters
{
  val io = new Bundle {
    val op = new BankOpIO().flip
    val ack = Valid(new VIUAck)
    val rw = new BankRWIO
  }

  val rf = Module(new BankRegfile)

  rf.io.op <> io.op
  rf.io.global <> io.rw

  val outs = new ArrayBuffer[ValidIO[BankALUResult]]
  for (i <- 0 until nSlices) {
    val alu = Module(new BankALUSlice)

    alu.io.req.valid := io.op.viu.valid && io.op.viu.bits.pred(i)
    alu.io.req.bits.fn := io.op.viu.bits.fn
    alu.io.req.bits.eidx := io.op.viu.bits.eidx

    alu.io.req.bits.in0 := unpack_slice(rf.io.local.rdata(0).d, i)
    alu.io.req.bits.in1 := unpack_slice(rf.io.local.rdata(1).d, i)

    outs += alu.io.resp
  }
  rf.io.local.wdata.d := repack_slice(outs.map(_.bits.out))

  io.ack.valid := outs.map(_.valid).reduce(_|_)
  io.ack.bits.pred := Vec(outs.map(_.valid)).toBits
}

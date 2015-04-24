package hwacha

import Chisel._
import scala.collection.mutable.ArrayBuffer

class BankOpIO extends VXUBundle {
  val sram = new Bundle {
    val read = Valid(new SRAMRFReadMicroOp)
    val write = Valid(new SRAMRFWriteMicroOp)
  }
  val ff = new Bundle {
    val read = Vec.fill(nFFRPorts){Valid(new FFRFReadMicroOp)}
    val write = Valid(new FFRFWriteMicroOp)
  }
  val opl = new Bundle {
    val global = Vec.fill(nGOPL){Valid(new OPLMicroOp)}
    val local = Vec.fill(nLOPL){Valid(new OPLMicroOp)}
  }
  val sreg = Vec.fill(nLOPL){Valid(new SRegMicroOp)}
  val xbar = Vec.fill(nGOPL){Valid(new XBarMicroOp)}
  val viu = Valid(new VIUMicroOp)
  val vsu = Valid(new VSUMicroOp)
}

class BRQIO extends DecoupledIO(new BRQEntry)
class BWQIO extends DecoupledIO(new BWQEntry)

class BankRWIO extends VXUBundle {
  val rdata = Vec.fill(nGOPL){new BankReadEntry().asOutput}
  val wdata = Vec.fill(nWSel){new BankWriteEntry().asInput}

  val brq = new BRQIO
  val bwq = new Bundle {
    val mem = new BWQIO().flip
    val fu = new BWQIO().flip
  }
}

class Bank(id: Int) extends VXUModule with Packing {
  val io = new Bundle {
    val op = new BankOpIO().flip
    val ack = Valid(new VIUAck)
    val rw = new BankRWIO
  }

  val rf = Module(new BankRegfile)

  rf.io.op <> io.op
  rf.io.global <> io.rw

  val outs = new ArrayBuffer[ValidIO[ALUResult]]
  for (i <- 0 until nSlices) {
    val alu = Module(new ALUSlice(id*nSlices+i))

    alu.io.req.valid := io.op.viu.valid && io.op.viu.bits.pred(i)
    alu.io.req.bits.fn := io.op.viu.bits.fn
    alu.io.req.bits.eidx := io.op.viu.bits.eidx

    alu.io.req.bits.in0 :=
      Mux(io.op.sreg(0).valid, io.op.sreg(0).bits.operand,
                               unpack_slice(rf.io.local.rdata(0).d, i))
    alu.io.req.bits.in1 :=
      Mux(io.op.sreg(1).valid, io.op.sreg(1).bits.operand,
                               unpack_slice(rf.io.local.rdata(1).d, i))

    outs += alu.io.resp
  }
  rf.io.local.wdata.d := repack_slice(outs.map(_.bits.out))

  io.ack.valid := outs.map(_.valid).reduce(_|_)
  io.ack.bits.pred := Vec(outs.map(_.valid)).toBits
}

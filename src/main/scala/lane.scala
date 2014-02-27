package hwacha

import Chisel._
import Node._
import Constants._
import scala.collection.mutable.ArrayBuffer

class LaneOpIO extends Bundle
{
  val read = Valid(new ReadBankOp)
  val write = Valid(new WriteBankOp)
  val viu = Valid(new VIUOp)
  val vau0 = Valid(new VAU0Op)
  val vau1t = Valid(new VAU1Op)
  val vau1f = Valid(new VAU1Op)
  val vau2 = Valid(new VAU2Op)
  val vgu = Valid(new VGUOp)
  val vlu = Valid(new VLUOp)
  val vsu = Valid(new VSUOp)
}

class LaneFUOpIO extends Bundle
{
  val vau0 = Valid(new VAU0Op)
  val vau1t = Valid(new VAU1Op)
  val vau1f = Valid(new VAU1Op)
  val vau2 = Valid(new VAU2Op)
  val vgu = Valid(new VGUOp)
  val vlu = Valid(new VLUOp)
  val vsu = Valid(new VSUOp)
}

class LaneMemOpIO extends Bundle
{
  val vgu = Valid(new VGUOp)
  val vlu = Valid(new VLUOp)
  val vsu = Valid(new VSUOp)
}

class LaneMemDataIO extends Bundle
{
  val paddr = Bits(OUTPUT, SZ_DATA)
  val ldata = Bits(INPUT, SZ_DATA)
  val sdata = Bits(OUTPUT, SZ_DATA)
}

class Lane(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val cfg = new HwachaConfigIO().flip
    val op = new LaneOpIO().flip
    val vmu = new VMUIO
    val lret = new MRTLoadRetireIO
  }

  val conn = new ArrayBuffer[BankOpIO]
  val rblen = new ArrayBuffer[Bits]
  val ropl0 = new ArrayBuffer[Bits]
  val ropl1 = new ArrayBuffer[Bits]
  val ropl2 = new ArrayBuffer[Bits]

  val lfu = Module(new LaneLFU)
  val imul = Module(new LaneMul)
  val fma0 = Module(new LaneFMA)
  val fma1 = Module(new LaneFMA)
  val conv = Module(new LaneConv)
  val mem = Module(new LaneMem)

  for (i <- 0 until SZ_BANK) {
    val bank = Module(new Bank)

    bank.io.active := io.cfg.bactive(i)
    bank.io.op.in <> (if (i == 0) io.op else conn.last)
    
    conn += bank.io.op.out
    rblen += bank.io.rw.rblen
    ropl0 += bank.io.rw.ropl0
    ropl1 += bank.io.rw.ropl1
    ropl2 += bank.io.rw.ropl2

    bank.io.rw.wbl0 := imul.io.out
    bank.io.rw.wbl1 := fma0.io.out
    bank.io.rw.wbl2 := fma1.io.out
    bank.io.rw.wbl3 := conv.io.out
    bank.io.rw.wbl4 := mem.io.data.ldata
  }

  // For each bank, match bank n's rbl enable bit with bank n's corresponding ropl and mask if disabled.
  // For each ropl, reduce all banks' version of that ropl with a bitwise-OR.
  val rbl = List(ropl1, ropl0, ropl2, ropl1, ropl0, ropl2, ropl1, ropl0, ropl0, ropl0, ropl0).zipWithIndex.map(
    rblgroup => rblen.zip(rblgroup._1).map(b => Fill(SZ_DATA, b._1(rblgroup._2)) & b._2).reduce(_|_))

  lfu.io.op <> io.op

  imul.io.valid := lfu.io.vau0.valid
  imul.io.fn := lfu.io.vau0.bits.fn
  imul.io.in0 := rbl(0)
  imul.io.in1 := rbl(1)

  fma0.io.valid := lfu.io.vau1t.valid
  fma0.io.fn := lfu.io.vau1t.bits.fn
  fma0.io.in0 := rbl(2)
  fma0.io.in1 := rbl(3)
  fma0.io.in2 := rbl(4)

  fma1.io.valid := lfu.io.vau1f.valid
  fma1.io.fn := lfu.io.vau1f.bits.fn
  fma1.io.in0 := rbl(5)
  fma1.io.in1 := rbl(6)
  fma1.io.in2 := rbl(7)

  conv.io.valid := lfu.io.vau2.valid
  conv.io.fn := lfu.io.vau2.bits.fn
  conv.io.in := rbl(8)

  mem.io.op <> lfu.io.mem
  mem.io.data.paddr := rbl(9)(SZ_ADDR-1,0)
  mem.io.data.sdata := rbl(10)

  io.vmu <> mem.io.vmu
  io.lret <> mem.io.lret
}

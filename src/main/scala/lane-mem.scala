package hwacha

import Chisel._
import Node._
import Constants._
import Compaction._
import uncore.constants.AddressConstants._
import uncore.constants.MemoryOpConstants._

class LaneMem(implicit conf: HwachaConfiguration) extends Module
{
  val io = new Bundle {
    val op = new LaneMemOpIO().flip
    val data = new LaneMemDataIO().flip
    val vmu = new vmunit.VMUIO
  }

  // VGU
  // Timing Diagram
  // | Seq | Exp/SRAM-Addr-Setup | SRAM-Clock-Q | XBar/Addr-Gen/VAQ-Setup |
  //                                              ^ io.op.vgu starts here
  io.vmu.addr.q.valid := io.op.vgu.valid
  io.vmu.addr.q.bits := io.op.vgu.bits.base + io.data.paddr
}

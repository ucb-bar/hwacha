package hwacha

import Chisel._
import Node._
import Constants._

class XBarMux8IO extends Bundle 
{
  val rblen = Vec.fill(8){UInt(INPUT, SZ_BRPORT)}
  val rdata = Vec.fill(8){UInt(INPUT, SZ_DATA)}
  val rbl   = UInt(OUTPUT, SZ_DATA)
}

class vuVXU_Banked8_Lane_Xbar_Mux8(port: Int) extends Module
{
  val io = new XBarMux8IO()
  io.rbl := Fill(SZ_DATA, io.rblen(0)(port)) & io.rdata(0) |
            Fill(SZ_DATA, io.rblen(1)(port)) & io.rdata(1) |
            Fill(SZ_DATA, io.rblen(2)(port)) & io.rdata(2) |
            Fill(SZ_DATA, io.rblen(3)(port)) & io.rdata(3) |
            Fill(SZ_DATA, io.rblen(4)(port)) & io.rdata(4) |
            Fill(SZ_DATA, io.rblen(5)(port)) & io.rdata(5) |
            Fill(SZ_DATA, io.rblen(6)(port)) & io.rdata(6) |
            Fill(SZ_DATA, io.rblen(7)(port)) & io.rdata(7)
  
}

object vuVXU_Banked8_Lane_Xbar_Mux8
{
  def apply(rblen: Vec[UInt], port: Int, rdata: Vec[UInt]): UInt = 
    {
      val mux8 = Module(new vuVXU_Banked8_Lane_Xbar_Mux8(port))
      mux8.io.rblen <> rblen
      mux8.io.rdata <> rdata
      mux8.io.rbl
    }
}

class XbarIO extends Bundle 
{
  val rblen = Vec.fill(8){UInt(INPUT, SZ_BRPORT)}
  val rdata = Vec.fill(8){UInt(INPUT, SZ_DATA)}
  val ropl0 = Vec.fill(8){UInt(INPUT, SZ_DATA)}
  val ropl1 = Vec.fill(8){UInt(INPUT, SZ_DATA)}

  val rbl = Vec.fill(8){UInt(OUTPUT, SZ_DATA)}
}

class vuVXU_Banked8_Lane_Xbar extends Module
{
  val io = new XbarIO()
  io.rbl(0) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 0, io.ropl0)
  io.rbl(1) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 1, io.rdata)
  io.rbl(2) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 2, io.ropl1)
  io.rbl(3) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 3, io.ropl0)
  io.rbl(4) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 4, io.rdata)
  io.rbl(5) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 5, io.rdata)
  io.rbl(6) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 6, io.rdata)
  io.rbl(7) := vuVXU_Banked8_Lane_Xbar_Mux8(io.rblen, 7, io.rdata)

}

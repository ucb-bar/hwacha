package hwacha

import Chisel._
import Node._
import Constants._

class RegfileIO extends Bundle
{
  val ren    = Bool(INPUT)
  val raddr  = Bits(INPUT, SZ_BREGLEN)
  val roplen = Bits(INPUT, SZ_BOPL)

  val wen   = Bool(INPUT)
  val waddr = Bits(INPUT, SZ_BREGLEN)
  val wsel  = Bits(INPUT, SZ_BWPORT)

  val rdata = Bits(OUTPUT, SZ_DATA)
  val ropl0 = Bits(OUTPUT, SZ_DATA)
  val ropl1 = Bits(OUTPUT, SZ_DATA)

  val wbl0 = Bits(INPUT, SZ_DATA)
  val wbl1 = Bits(INPUT, SZ_DATA)
  val wbl2 = Bits(INPUT, SZ_DATA)
  val wbl3 = Bits(INPUT, SZ_DATA)

  val viu_rdata = Bits(OUTPUT, SZ_DATA)
  val viu_ropl  = Bits(OUTPUT, SZ_DATA)
  val viu_wdata = Bits(INPUT, SZ_DATA)
}

class vuVXU_Banked8_Bank_Regfile extends Module
{
  val io = new RegfileIO()

  val wdata = MuxLookup(
    io.wsel, Bits(0, SZ_DATA), Array(
      Bits(0, SZ_BWPORT) -> io.wbl0,
      Bits(1, SZ_BWPORT) -> io.wbl1,
      Bits(2, SZ_BWPORT) -> io.wbl2,
      Bits(3, SZ_BWPORT) -> io.wbl3,
      Bits(4, SZ_BWPORT) -> io.viu_wdata
    ))

  val rfile = Mem(Bits(width = SZ_DATA), 256, seqRead = true)
  val raddr = Reg(Bits())
  when (io.wen) { rfile(io.waddr) := wdata }
  when (io.ren) { raddr := io.raddr }
  val rdata_rf = Mux(RegUpdate(io.ren), rfile(raddr), Bits(0)) 
  io.rdata := rdata_rf

  val ropl0Reg = Reg(Bits(width = SZ_DATA))
  val ropl1Reg = Reg(Bits(width = SZ_DATA))
  when (io.roplen(0)) { ropl0Reg := rdata_rf }
  when (io.roplen(1)) { ropl1Reg := rdata_rf }

  io.ropl0 := ropl0Reg
  io.ropl1 := ropl1Reg

  io.viu_rdata := rdata_rf
  io.viu_ropl := io.ropl0
}
